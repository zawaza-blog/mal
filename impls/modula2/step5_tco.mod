MODULE step5_tco;

FROM StdChans IMPORT StdInChan, StdOutChan;
FROM TextIO IMPORT WriteString, WriteLn, ReadString, SkipLine;
FROM WholeIO IMPORT WriteCard;
FROM IOChan IMPORT ReadResult, ChanId;
IMPORT IOResult;

FROM Types IMPORT MalVal, MalType, MalList, FreeMalVal, NewNil, NewList, NewVector, NewNativeFn,
                   NewFunction, ListAppend, ListLength, ListGet, IsNil, IsSpecialForm;
FROM SYSTEM IMPORT ADDRESS, CAST;
FROM Reader IMPORT ReadStr, HasError, WriteErrorTo, ClearError, SetError, SetErrorString;
FROM Printer IMPORT PrStr;
FROM Env IMPORT Env, NewEnv, NewEnvOuter, EnvSetArray, EnvSetString, EnvGetArray, EnvGetString, EnvFindArray, GetEnvStats, GetCacheStats;
IMPORT Core;
FROM Strings IMPORT Assign, Concat, Equal;
FROM DynString IMPORT String;
IMPORT DynString;

VAR
  stdin, stdout: ChanId;
  replEnv: Env;

(* READ - Read a line from input and parse it *)
PROCEDURE READ(VAR line: ARRAY OF CHAR): MalVal;
BEGIN
  RETURN ReadStr(line);
END READ;

(* EVAL - Evaluate the AST with TCO *)
PROCEDURE EVAL(astParam: MalVal; envParam: Env): MalVal;
VAR
  result: MalVal;
  evaledList: MalVal;
  curr: MalList;
  evaledItem: MalVal;
  fn: MalVal;
  args: MalVal;
  i, len: CARDINAL;
  found: BOOLEAN;
  errMsg: String;
  newEnv: Env;
  bindKey: MalVal;
  ast: MalVal;
  env: Env;
BEGIN
  (* Initialize with parameters *)
  ast := astParam;
  env := envParam;

  (* TCO loop *)
  LOOP

  (* Handle symbols *)
  IF ast^.type = MAL_SYMBOL THEN
    result := EnvGetString(env, ast^.strVal, found);
    IF found THEN
      RETURN result;
    ELSE
      ClearError();
      errMsg := DynString.Concat3("'", ast^.strVal, "' not found");
      SetErrorString(errMsg);
      RETURN NewNil();
    END;
  END;

  (* Handle vectors - evaluate each element *)
  IF ast^.type = MAL_VECTOR THEN
    result := NewVector();
    curr := ast^.listVal;
    WHILE curr # NIL DO
      evaledItem := EVAL(curr^.val, env);
      IF HasError() THEN
        RETURN result;
      END;
      ListAppend(result, evaledItem);
      curr := curr^.next;
    END;
    RETURN result;
  END;

  (* Handle non-lists: return as-is *)
  IF ast^.type # MAL_LIST THEN
    RETURN ast;
  END;

  (* Empty list evaluates to itself *)
  IF ListLength(ast) = 0 THEN
    RETURN ast;
  END;

  (* Check for special forms *)
  curr := ast^.listVal;
  IF (curr # NIL) AND (curr^.val^.type = MAL_SYMBOL) THEN

    (* def! special form: (def! key value) *)
    IF IsSpecialForm(curr^.val, "def!") THEN
      IF ListLength(ast) # 3 THEN
        ClearError();
        SetError("def! requires exactly 2 arguments");
        RETURN NewNil();
      END;

      (* Get the key (must be a symbol) *)
      bindKey := ListGet(ast, 1);
      IF bindKey^.type # MAL_SYMBOL THEN
        ClearError();
        SetError("def! first argument must be a symbol");
        RETURN NewNil();
      END;

      (* Evaluate the value *)
      evaledItem := EVAL(ListGet(ast, 2), env);
      IF HasError() THEN
        RETURN evaledItem;
      END;

      (* Set in environment and return the value *)
      EnvSetString(env, bindKey^.strVal, evaledItem);
      RETURN evaledItem;

    ELSIF IsSpecialForm(curr^.val, "let*") THEN
    (* let* special form: (let* (k1 v1 k2 v2 ...) form) - TCO *)
      IF ListLength(ast) # 3 THEN
        ClearError();
        SetError("let* requires exactly 2 arguments");
        RETURN NewNil();
      END;

      (* Get bindings list *)
      bindKey := ListGet(ast, 1);
      IF (bindKey^.type # MAL_LIST) AND (bindKey^.type # MAL_VECTOR) THEN
        ClearError();
        SetError("let* first argument must be a list or vector");
        RETURN NewNil();
      END;

      (* Create new environment with current as outer *)
      newEnv := NewEnvOuter(env);

      (* Process bindings *)
      curr := bindKey^.listVal;
      WHILE curr # NIL DO
        (* Get key *)
        IF curr^.val^.type # MAL_SYMBOL THEN
          ClearError();
          SetError("let* binding key must be a symbol");
          RETURN NewNil();
        END;

        (* Get value (next element) *)
        IF curr^.next = NIL THEN
          ClearError();
          SetError("let* bindings must have even number of elements");
          RETURN NewNil();
        END;

        (* Evaluate value in new environment *)
        evaledItem := EVAL(curr^.next^.val, newEnv);
        IF HasError() THEN
          RETURN evaledItem;
        END;

        (* Set binding *)
        EnvSetString(newEnv, curr^.val^.strVal, evaledItem);

        (* Move to next pair *)
        curr := curr^.next^.next;
      END;

      (* TCO: Evaluate body in new environment *)
      ast := ListGet(ast, 2);
      env := newEnv;
      (* Continue loop *)

    ELSIF IsSpecialForm(curr^.val, "if") THEN
    (* if special form: (if test then else?) - TCO *)
      len := ListLength(ast);
      IF (len < 3) OR (len > 4) THEN
        ClearError();
        SetError("if requires 2 or 3 arguments");
        RETURN NewNil();
      END;

      (* Evaluate test *)
      evaledItem := EVAL(ListGet(ast, 1), env);
      IF HasError() THEN
        RETURN evaledItem;
      END;

      (* Check if test is true (only nil and false are falsy) *)
      IF (evaledItem^.type = MAL_NIL) OR (evaledItem^.type = MAL_FALSE) THEN
        (* Test is false - TCO: evaluate else branch if it exists *)
        IF len = 4 THEN
          ast := ListGet(ast, 3);
          (* Continue loop *)
        ELSE
          RETURN NewNil();
        END;
      ELSE
        (* Test is true - TCO: evaluate then branch *)
        ast := ListGet(ast, 2);
        (* Continue loop *)
      END;

    ELSIF IsSpecialForm(curr^.val, "do") THEN
    (* do special form: (do expr1 expr2 ... exprN) - TCO *)
      len := ListLength(ast);
      IF len < 2 THEN
        RETURN NewNil();
      END;

      (* Evaluate all expressions except the last *)
      FOR i := 1 TO len - 2 DO
        evaledItem := EVAL(ListGet(ast, i), env);
        IF HasError() THEN
          RETURN evaledItem;
        END;
      END;

      (* TCO: Evaluate last expression *)
      ast := ListGet(ast, len - 1);
      (* Continue loop *)

    ELSIF IsSpecialForm(curr^.val, "fn*") THEN
    (* fn* special form: (fn* (params...) body) *)
      IF ListLength(ast) # 3 THEN
        ClearError();
        SetError("fn* requires exactly 2 arguments");
        RETURN NewNil();
      END;

      (* Get parameters list *)
      bindKey := ListGet(ast, 1);
      IF (bindKey^.type # MAL_LIST) AND (bindKey^.type # MAL_VECTOR) THEN
        ClearError();
        SetError("fn* first argument must be a list or vector");
        RETURN NewNil();
      END;

      (* Get body *)
      evaledItem := ListGet(ast, 2);

      (* Create closure - capture current environment *)
      RETURN NewFunction(bindKey, evaledItem, CAST(ADDRESS, env), FALSE);

    ELSE
      (* Not a special form - evaluate as function application *)
      (* Evaluate list elements *)
      evaledList := NewList();
      curr := ast^.listVal;
      WHILE curr # NIL DO
        evaledItem := EVAL(curr^.val, env);
        IF HasError() THEN
          RETURN evaledList;
        END;
        ListAppend(evaledList, evaledItem);
        curr := curr^.next;
      END;

      (* First element should be a function *)
      fn := ListGet(evaledList, 0);

      (* Collect arguments (rest of the list) *)
      args := NewList();
      len := ListLength(evaledList);
      FOR i := 1 TO len - 1 DO
        ListAppend(args, ListGet(evaledList, i));
      END;

      (* Handle native functions *)
      IF fn^.type = MAL_NATIVE_FN THEN
        RETURN fn^.nativeFn(args);
      ELSIF fn^.type = MAL_FUNCTION THEN
        (* Handle user-defined functions - TCO *)
        (* Create new environment with closure as outer *)
        newEnv := NewEnvOuter(CAST(Env, fn^.closure));

        (* Bind parameters to arguments *)
        IF NOT Core.BindFunctionParams(fn^.params, args, newEnv) THEN
          RETURN NewNil();
        END;

        (* TCO: Evaluate body in new environment *)
        ast := fn^.body;
        env := newEnv;
        (* Continue loop *)
      ELSE
        (* Not a function *)
        ClearError();
        SetError("Cannot invoke non-function");
        RETURN NewNil();
      END;
    END;
  END; (* End special forms check *)

  END; (* TCO loop *)
END EVAL;

(* PRINT - Print the result *)
PROCEDURE PRINT(exp: MalVal): String;
BEGIN
  RETURN PrStr(exp, TRUE);
END PRINT;

(* REP - Read-Eval-Print *)
PROCEDURE rep(VAR line: ARRAY OF CHAR): String;
VAR
  ast, result: MalVal;
  output: String;
BEGIN
  ast := READ(line);
  result := EVAL(ast, replEnv);
  output := PRINT(result);
  (* Free MalVal objects - only free ast if different from result *)
  IF result # ast THEN
    FreeMalVal(ast);
  END;
  FreeMalVal(result);
  RETURN output;
END rep;

(* Read input from stdin *)
PROCEDURE ReadInput(VAR line: ARRAY OF CHAR): BOOLEAN;
VAR
  res: IOResult.ReadResults;
BEGIN
  WriteString(stdout, "user> ");
  ReadString(stdin, line);
  res := ReadResult(stdin);

  (* endOfInput means EOF, return FALSE to exit *)
  IF res = IOResult.endOfInput THEN
    RETURN FALSE;
  END;

  (* Skip the rest of the line (newline character) *)
  SkipLine(stdin);
  RETURN TRUE;
END ReadInput;

(* Initialize REPL environment *)
PROCEDURE InitReplEnv();
BEGIN
  (* Use Core.InitReplEnv to create environment with all core functions *)
  replEnv := Core.InitReplEnv(0);
END InitReplEnv;

(* Main loop *)
VAR
  line: ARRAY [0..1023] OF CHAR;
  output: String;
  (* Debug modula2 alloc internal linked lists.
  creates, gets, getSteps, sets: CARDINAL;
  cacheHits, cacheMisses: CARDINAL;
  *)

BEGIN
  stdin := StdInChan();
  stdout := StdOutChan();

  InitReplEnv();

  WHILE ReadInput(line) DO
    output := rep(line);
    IF HasError() THEN
      WriteString(stdout, "Error: ");
      WriteErrorTo(stdout);
      WriteLn(stdout);
      ClearError();
    ELSE
      DynString.WriteToChannel(output, stdout);
      WriteLn(stdout);
      DynString.Dispose(output);
    END;
  END;

  WriteLn(stdout);

  (* Debug modula2 alloc internal linked lists.
  GetEnvStats(creates, gets, getSteps, sets);
  GetCacheStats(cacheHits, cacheMisses);

  WriteString(stdout, "=== Environment Profiling Stats ===");
  WriteLn(stdout);
  WriteString(stdout, "Environments created: ");
  WriteCard(stdout, creates, 0);
  WriteLn(stdout);
  WriteString(stdout, "EnvGetArray calls: ");
  WriteCard(stdout, gets, 0);
  WriteLn(stdout);
  WriteString(stdout, "EnvGetArray steps (comparisons): ");
  WriteCard(stdout, getSteps, 0);
  WriteLn(stdout);
  WriteString(stdout, "EnvSetArray calls: ");
  WriteCard(stdout, sets, 0);
  WriteLn(stdout);
  IF gets > 0 THEN
    WriteString(stdout, "Avg steps per EnvGetArray: ");
    WriteCard(stdout, getSteps DIV gets, 0);
    WriteLn(stdout);
  END;

  WriteLn(stdout);
  WriteString(stdout, "=== Cache Statistics ===");
  WriteLn(stdout);
  WriteString(stdout, "Cache hits: ");
  WriteCard(stdout, cacheHits, 0);
  WriteLn(stdout);
  WriteString(stdout, "Cache misses: ");
  WriteCard(stdout, cacheMisses, 0);
  WriteLn(stdout);
  IF (cacheHits + cacheMisses) > 0 THEN
    WriteString(stdout, "Cache hit rate: ");
    WriteCard(stdout, (cacheHits * 100) DIV (cacheHits + cacheMisses), 0);
    WriteString(stdout, "%");
    WriteLn(stdout);
  END;
  *)

END step5_tco.
