MODULE step9_try;

FROM StdChans IMPORT StdInChan, StdOutChan;
FROM TextIO IMPORT WriteString, WriteLn, ReadString, SkipLine;
FROM IOChan IMPORT ReadResult, ChanId;
IMPORT IOResult;

FROM Types IMPORT MalVal, MalType, MalList, FreeMalVal, NewNil, NewList, NewVector, NewNativeFn,
                   NewFunction, NewString, NewStringCopy, NewSymbol, NewHashMap, ListAppend, ListLength, ListGet, IsNil,
                   HashMapKeys, HashMapGetString, HashMapPutString, IsSpecialForm;
FROM SYSTEM IMPORT ADDRESS, CAST;
FROM Reader IMPORT ReadStr, HasError, GetError, WriteErrorTo, ClearError, SetError, SetErrorString,
                   HasException, GetException, ClearException, SetException;
FROM Printer IMPORT PrStr;
FROM Env IMPORT Env, NewEnv, NewEnvOuter, EnvSetArray, EnvSetString, EnvGetArray, EnvGetString, EnvFindArray;
FROM DynString IMPORT String;
IMPORT DynString;
IMPORT Core;
FROM Strings IMPORT Assign, Concat, Equal;
IMPORT Strings;
IMPORT Args;

TYPE
  (* Result type for special form handlers to support TCO *)
  EvalAction = (EVAL_RETURN, EVAL_TCO);
  EvalResult = RECORD
    action: EvalAction;
    value: MalVal;    (* Used when action = EVAL_RETURN *)
    newAst: MalVal;   (* Used when action = EVAL_TCO *)
    newEnv: Env;      (* Used when action = EVAL_TCO *)
  END;

VAR
  stdin, stdout: ChanId;
  replEnv: Env;


(* READ - Read a line from input and parse it *)
PROCEDURE READ(VAR line: ARRAY OF CHAR): MalVal;
BEGIN
  RETURN ReadStr(line);
END READ;

(* IsMacroCall - Check if ast is a macro call *)
PROCEDURE IsMacroCall(ast: MalVal; env: Env): BOOLEAN;
VAR
  first: MalVal;
  found: BOOLEAN;
  val: MalVal;
BEGIN
  (* Must be a non-empty list *)
  IF (ast^.type # MAL_LIST) OR (ListLength(ast) = 0) THEN
    RETURN FALSE;
  END;

  (* First element must be a symbol *)
  first := ListGet(ast, 0);
  IF first^.type # MAL_SYMBOL THEN
    RETURN FALSE;
  END;

  (* Look up the symbol *)
  val := EnvGetString(env, first^.strVal, found);
  IF NOT found THEN
    RETURN FALSE;
  END;

  (* Check if it's a macro *)
  RETURN (val^.type = MAL_FUNCTION) AND val^.isMacro;
END IsMacroCall;

(* MacroExpand - Expand macros in ast *)
PROCEDURE MacroExpand(ast: MalVal; env: Env): MalVal;
VAR
  macroFn, first: MalVal;
  args: MalVal;
  newEnv: Env;
  i: CARDINAL;
  curr: MalList;
  evaledItem: MalVal;
  found: BOOLEAN;
BEGIN
  (* Keep expanding while we have a macro call *)
  WHILE IsMacroCall(ast, env) DO
    (* Get the macro function *)
    first := ListGet(ast, 0);
    macroFn := EnvGetString(env, first^.strVal, found);

    (* Collect arguments (don't evaluate them - macros get unevaluated args) *)
    args := NewList();
    FOR i := 1 TO ListLength(ast) - 1 DO
      ListAppend(args, ListGet(ast, i));
    END;

    (* Create new environment for macro expansion *)
    newEnv := NewEnvOuter(CAST(Env, macroFn^.closure));

    (* Bind parameters to arguments *)
    IF NOT Core.BindFunctionParams(macroFn^.params, args, newEnv) THEN
      RETURN NewNil();
    END;

    (* Evaluate macro body to get expansion *)
    ast := EVAL(macroFn^.body, newEnv);
    IF HasError() THEN
      RETURN ast;
    END;
  END;

  RETURN ast;
END MacroExpand;

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
  tempStr: String;
  newEnv: Env;
  bindKey: MalVal;
  ast: MalVal;
  env: Env;
  keys: MalVal;
  keyVal, val: MalVal;
  debugEvalLiteral: ARRAY [0..15] OF CHAR;
  debugEvalStr: String;
BEGIN
  (* Initialize with parameters *)
  ast := astParam;
  env := envParam;

  (* Initialize DEBUG-EVAL string *)
  Assign("DEBUG-EVAL", debugEvalLiteral);
  debugEvalStr := DynString.Create(debugEvalLiteral);

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

  (* Handle hash-maps - evaluate values *)
  IF ast^.type = MAL_HASHMAP THEN
    result := NewHashMap();
    keys := HashMapKeys(ast);
    curr := keys^.listVal;
    WHILE curr # NIL DO
      keyVal := curr^.val;
      (* Get value for this key *)
      val := HashMapGetString(ast, keyVal^.strVal);
      (* Evaluate value *)
      evaledItem := EVAL(val, env);
      IF HasError() THEN
        RETURN result;
      END;
      (* Put evaluated value in result *)
      HashMapPutString(result, keyVal^.strVal, evaledItem);
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

  (* Macro expansion - expand macros before evaluation *)
  ast := MacroExpand(ast, env);
  IF HasError() THEN
    RETURN ast;
  END;

  (* After macro expansion, if it's not a list, loop back to re-evaluate *)
  (* This handles cases where a macro expands to a symbol or other non-list *)
  IF ast^.type # MAL_LIST THEN
    (* Continue from top of loop - will handle symbols, vectors, etc. *)
  ELSE
  (* Only process as list if ast is still a list after macro expansion *)

  (* DEBUG-EVAL support: print form being evaluated after macro expansion *)
  result := EnvGetString(env, debugEvalStr, found);
  IF found AND (result^.type = MAL_TRUE) THEN
    WriteString(stdout, "EVAL: ");
    tempStr := PrStr(ast, TRUE);
    DynString.WriteToChannel(tempStr, stdout);
    WriteLn(stdout);
  END;

  (* Empty list after macro expansion *)
  IF ListLength(ast) = 0 THEN
    RETURN ast;
  END;

  (* Check for special forms *)
  curr := ast^.listVal;
  IF (curr # NIL) AND IsSpecialForm(curr^.val, "def!") THEN
    (* def! special form: (def! key value) *)
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

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "let*") THEN
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

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "if") THEN
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

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "do") THEN
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

  ELSIF (curr # NIL) AND (curr^.val^.type = MAL_SYMBOL) AND DynString.EqualArray(curr^.val^.strVal, "eval") THEN
    (* eval special form: (eval expr) *)
      IF ListLength(ast) # 2 THEN
        ClearError();
        SetError("eval requires exactly 1 argument");
        RETURN NewNil();
      END;

      (* Evaluate the argument *)
      evaledItem := EVAL(ListGet(ast, 1), env);
      IF HasError() THEN
        RETURN evaledItem;
      END;

      (* Then evaluate the result in the REPL environment *)
      ast := evaledItem;
      env := replEnv;
      (* Continue loop *)

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "quote") THEN
    (* quote special form: (quote form) - return form unevaluated *)
      IF ListLength(ast) # 2 THEN
        ClearError();
        SetError("quote requires exactly 1 argument");
        RETURN NewNil();
      END;
      RETURN ListGet(ast, 1);

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "quasiquote") THEN
    (* quasiquote special form: (quasiquote form) - TCO *)
      IF ListLength(ast) # 2 THEN
        ClearError();
        SetError("quasiquote requires exactly 1 argument");
        RETURN NewNil();
      END;
      (* Transform quasiquote to quote/unquote forms and continue *)
      ast := Core.Quasiquote(ListGet(ast, 1));
      (* Continue loop to evaluate the transformed AST *)

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "macroexpand") THEN
    (* macroexpand special form: (macroexpand form) *)
      IF ListLength(ast) # 2 THEN
        ClearError();
        SetError("macroexpand requires exactly 1 argument");
        RETURN NewNil();
      END;
      (* Return the macro-expanded form without evaluating it *)
      RETURN MacroExpand(ListGet(ast, 1), env);

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "fn*") THEN
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

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "defmacro!") THEN
    (* defmacro! special form: (defmacro! name (params...) body) *)
      IF ListLength(ast) # 3 THEN
        ClearError();
        SetError("defmacro! requires exactly 2 arguments");
        RETURN NewNil();
      END;

      (* Get the key (must be a symbol) *)
      bindKey := ListGet(ast, 1);
      IF bindKey^.type # MAL_SYMBOL THEN
        ClearError();
        SetError("defmacro! first argument must be a symbol");
        RETURN NewNil();
      END;

      (* Evaluate the value (should be a function) *)
      evaledItem := EVAL(ListGet(ast, 2), env);
      IF HasError() THEN
        RETURN evaledItem;
      END;

      (* Check if it's a function *)
      IF evaledItem^.type # MAL_FUNCTION THEN
        ClearError();
        SetError("defmacro! requires a function as second argument");
        RETURN NewNil();
      END;

      (* Mark the function as a macro *)
      evaledItem^.isMacro := TRUE;

      (* Set in environment and return the macro *)
      EnvSetString(env, bindKey^.strVal, evaledItem);
      RETURN evaledItem;

  ELSIF (curr # NIL) AND IsSpecialForm(curr^.val, "try*") THEN
    (* try* special form: (try* A (catch* B C)) *)
      len := ListLength(ast);
      IF (len < 2) OR (len > 3) THEN
        ClearError();
        SetError("try* requires 1 or 2 arguments");
        RETURN NewNil();
      END;

      (* Evaluate the try expression *)
      evaledItem := EVAL(ListGet(ast, 1), env);

      (* Check if an exception was thrown or an error occurred *)
      IF HasException() OR HasError() THEN
        (* Get the exception value *)
        IF HasException() THEN
          result := GetException();
          ClearException();
        ELSE
          (* Convert error to exception *)
          errMsg := GetError();
          result := NewStringCopy(errMsg);
          ClearError();
        END;

        (* If there's a catch block, evaluate it *)
        IF len = 3 THEN
          bindKey := ListGet(ast, 2);

          (* catch* block must be a list starting with catch* symbol *)
          IF (bindKey^.type # MAL_LIST) OR (ListLength(bindKey) # 3) THEN
            ClearError();
            SetError("catch* block must be (catch* symbol expr)");
            RETURN NewNil();
          END;

          (* First element must be the catch* symbol *)
          evaledItem := ListGet(bindKey, 0);
          IF (evaledItem^.type # MAL_SYMBOL) OR NOT DynString.EqualArray(evaledItem^.strVal, "catch*") THEN
            ClearError();
            SetError("catch block must start with catch*");
            RETURN NewNil();
          END;

          (* Second element must be a symbol (the exception binding) *)
          evaledItem := ListGet(bindKey, 1);
          IF evaledItem^.type # MAL_SYMBOL THEN
            ClearError();
            SetError("catch* binding must be a symbol");
            RETURN NewNil();
          END;

          (* Create new environment with exception bound to the symbol *)
          newEnv := NewEnvOuter(env);
          EnvSetString(newEnv, evaledItem^.strVal, result);

          (* Evaluate the catch expression *)
          RETURN EVAL(ListGet(bindKey, 2), newEnv);
        ELSE
          (* No catch block, re-throw the exception *)
          SetException(result);
          RETURN NewNil();
        END;
      ELSE
        (* No exception, return the result *)
        RETURN evaledItem;
      END;

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
  END; (* End special forms check *)

  END; (* End ELSE for macro expansion check *)

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
  exceptVal: MalVal;
  exceptStr, errorMsg, output: String;
BEGIN
  ast := READ(line);
  result := EVAL(ast, replEnv);

  (* Check for exceptions *)
  IF HasException() THEN
    exceptVal := GetException();
    ClearException();
    (* Convert exception to string with "Exception thrown: " prefix *)
    exceptStr := PrStr(exceptVal, TRUE);
    errorMsg := DynString.CreateFromArray("Exception thrown: ");
    DynString.AppendString(errorMsg, exceptStr);
    (* Set error to signal exception in main loop *)
    SetErrorString(errorMsg);
    (* Free MalVal objects before returning *)
    IF result # ast THEN
      FreeMalVal(ast);
    END;
    FreeMalVal(result);
    RETURN DynString.CreateFromArray("");
  END;

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
PROCEDURE InitReplEnv(argc: CARDINAL);
BEGIN
  (* Use Core.InitReplEnv to create environment with all core functions *)
  (* load-file is now defined automatically in Core.InitReplEnv *)
  replEnv := Core.InitReplEnv(argc);
END InitReplEnv;

(* Main loop *)
VAR
  line: ARRAY [0..1023] OF CHAR;
  output: String;
  argc: CARDINAL;
  filename: ARRAY [0..1023] OF CHAR;
  loadCmd: ARRAY [0..2047] OF CHAR;

BEGIN
  stdin := StdInChan();
  stdout := StdOutChan();

  (* Set the evaluator for Core to use in swap! *)
  Core.SetEvaluator(EVAL);

  argc := Args.Narg();
  InitReplEnv(argc);

  (* Define standard macros *)
  Assign('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list (quote if) (first xs) (if (> (count xs) 1) (nth xs 1) nil) (cons (quote cond) (rest (rest xs)))))))', loadCmd);
  output := rep(loadCmd);
  IF HasError() THEN
    WriteString(stdout, "Error defining cond macro: ");
    WriteErrorTo(stdout);
    WriteLn(stdout);
  END;
  DynString.Dispose(output);

  (* If a filename was provided, load it and exit *)
  IF argc > 1 THEN
    IF Args.GetArg(filename, 1) THEN
      (* Build: (load-file "filename") *)
      Assign('(load-file "', loadCmd);
      Concat(loadCmd, filename, loadCmd);
      Concat(loadCmd, '")', loadCmd);

      output := rep(loadCmd);
      IF HasError() THEN
        WriteString(stdout, "Error: ");
        WriteErrorTo(stdout);
        WriteLn(stdout);
      END;
      DynString.Dispose(output);
    END;

    (* Exit after loading file *)
    RETURN;
  END;

  (* Otherwise, run REPL *)
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
END step9_try.
