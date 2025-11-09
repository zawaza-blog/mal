MODULE step2_eval;

FROM StdChans IMPORT StdInChan, StdOutChan;
FROM TextIO IMPORT WriteString, WriteLn, ReadString, SkipLine;
FROM IOChan IMPORT ReadResult, ChanId;
IMPORT IOResult;

FROM Types IMPORT MalVal, MalType, MalList, FreeMalVal, NewNil, NewList, NewVector, NewHashMap, NewNativeFn,
                   ListAppend, ListLength, ListGet, IsNil, HashMapKeys, HashMapGetString, HashMapPutString;
FROM Reader IMPORT ReadStr, HasError, WriteErrorTo, ClearError, SetError, SetErrorString;
FROM Printer IMPORT PrStr;
FROM Env IMPORT Env, NewEnv, EnvSetArray, EnvSetString, EnvGetArray, EnvGetString, EnvFindArray;
IMPORT Core;
FROM Strings IMPORT Assign, Concat;
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

(* EVAL - Evaluate the AST *)
PROCEDURE EVAL(ast: MalVal; env: Env): MalVal;
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
BEGIN
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

  (* Handle hashmaps - evaluate each value *)
  IF ast^.type = MAL_HASHMAP THEN
    result := NewHashMap();
    (* Get all keys from the hashmap *)
    evaledList := HashMapKeys(ast);
    curr := evaledList^.listVal;
    WHILE curr # NIL DO
      (* Get the original value for this key *)
      evaledItem := HashMapGetString(ast, curr^.val^.strVal);
      (* Evaluate the value *)
      evaledItem := EVAL(evaledItem, env);
      IF HasError() THEN
        RETURN result;
      END;
      (* Put the key and evaluated value into result hashmap *)
      HashMapPutString(result, curr^.val^.strVal, evaledItem);
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
  IF fn^.type # MAL_NATIVE_FN THEN
    ClearError();
    SetError("Cannot invoke non-function");
    RETURN NewNil();
  END;

  (* Collect arguments (rest of the list) *)
  args := NewList();
  len := ListLength(evaledList);
  FOR i := 1 TO len - 1 DO
    ListAppend(args, ListGet(evaledList, i));
  END;

  (* Call the function with arguments *)
  RETURN fn^.nativeFn(args);
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
END step2_eval.
