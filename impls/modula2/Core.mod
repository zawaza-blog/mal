IMPLEMENTATION MODULE Core;

(* MAL Core Functions - Implementation Module *)

FROM Types IMPORT MalVal, MalType, MalList, MalListNode, NewInteger, NewTrue, NewFalse, NewNil, NewList,
                   ListGet, ListLength, ListAppend, IsNil, IsList, NewAtom, IsAtom,
                   AtomDeref, AtomReset, NewString, NewStringCopy, NewNativeFn, NewVector, NewSymbol, NewSymbolCopy, NewKeyword, NewKeywordCopy, NewHashMap,
                   HashMapKeys, HashMapVals, HashMapCopy, Alloc, NewFunction,
                   HashMapPutString, HashMapGetString, HashMapContainsString, HashMapDeleteString;
FROM Strings IMPORT Assign, Length, Concat;
IMPORT Strings;
FROM DynString IMPORT String;
IMPORT DynString;
FROM Reader IMPORT ReadStr, SetError, HasError, ClearError,
                   HasException, GetException, ClearException, SetException;
FROM Printer IMPORT PrStr;
FROM Env IMPORT Env, NewEnv, NewEnvOuter, NewReplEnv, EnvSetArray, EnvSetString, EnvGetArray, PreInternSymbols;
FROM SYSTEM IMPORT ADDRESS, CAST, TSIZE;
IMPORT SYSTEM;
FROM InOut IMPORT WriteString, WriteLn;
FROM TextIO IMPORT ReadString, SkipLine, WriteChar;
FROM StdChans IMPORT StdInChan, StdOutChan;
FROM StreamFile IMPORT Open, Close;
IMPORT StreamFile;
FROM ChanConsts IMPORT OpenResults;
FROM IOChan IMPORT ChanId;
FROM RawIO IMPORT Read;
FROM IOResult IMPORT ReadResult, ReadResults;
FROM IOConsts IMPORT endOfInput;
FROM SysClock IMPORT DateTime, GetClock;
IMPORT Args;

TYPE
  CompareOp = (CMP_LT, CMP_LE, CMP_GT, CMP_GE);

(* Global evaluator - set by SetEvaluator before InitReplEnv *)
VAR
  globalEval: EvalProc;
  globalReplEnv: Env;  (* REPL environment for eval function *)

(* Add - sum all arguments *)
PROCEDURE Add(args: MalVal): MalVal;
VAR
  result: INTEGER;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  result := 0;
  len := ListLength(args);
  FOR i := 0 TO len - 1 DO
    arg := ListGet(args, i);
    result := result + arg^.intVal;
  END;
  RETURN NewInteger(result);
END Add;

(* Subtract - first arg minus rest *)
PROCEDURE Sub(args: MalVal): MalVal;
VAR
  result: INTEGER;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  len := ListLength(args);
  IF len = 0 THEN
    RETURN NewInteger(0);
  END;

  arg := ListGet(args, 0);
  result := arg^.intVal;

  IF len = 1 THEN
    RETURN NewInteger(-result);
  END;

  FOR i := 1 TO len - 1 DO
    arg := ListGet(args, i);
    result := result - arg^.intVal;
  END;
  RETURN NewInteger(result);
END Sub;

(* Multiply - product of all arguments *)
PROCEDURE Mul(args: MalVal): MalVal;
VAR
  result: INTEGER;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  result := 1;
  len := ListLength(args);
  FOR i := 0 TO len - 1 DO
    arg := ListGet(args, i);
    result := result * arg^.intVal;
  END;
  RETURN NewInteger(result);
END Mul;

(* Divide - first arg divided by rest *)
PROCEDURE Div(args: MalVal): MalVal;
VAR
  result: INTEGER;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  len := ListLength(args);
  IF len = 0 THEN
    RETURN NewInteger(0);
  END;

  arg := ListGet(args, 0);
  result := arg^.intVal;

  FOR i := 1 TO len - 1 DO
    arg := ListGet(args, i);
    result := result DIV arg^.intVal;
  END;
  RETURN NewInteger(result);
END Div;

(* Helper to compare two MalVals for equality *)
PROCEDURE MalEqual(a, b: MalVal): BOOLEAN;
VAR
  currA, currB: MalList;
  keyA, valA, valB: MalVal;
BEGIN
  (* Lists and vectors can be compared with each other *)
  IF ((a^.type = MAL_LIST) OR (a^.type = MAL_VECTOR)) AND
     ((b^.type = MAL_LIST) OR (b^.type = MAL_VECTOR)) THEN
    (* Compare sequential collections element by element *)
    IF ListLength(a) # ListLength(b) THEN
      RETURN FALSE;
    END;
    currA := a^.listVal;
    currB := b^.listVal;
    WHILE currA # NIL DO
      IF NOT MalEqual(currA^.val, currB^.val) THEN
        RETURN FALSE;
      END;
      currA := currA^.next;
      currB := currB^.next;
    END;
    RETURN TRUE;
  END;

  (* Compare hash-maps *)
  IF (a^.type = MAL_HASHMAP) AND (b^.type = MAL_HASHMAP) THEN
    (* Hash-maps must have same number of key-value pairs *)
    IF a^.treeSize # b^.treeSize THEN
      RETURN FALSE;
    END;

    (* Check that all keys in A exist in B with same values *)
    (* Get all keys from A *)
    keyA := HashMapKeys(a);
    currA := keyA^.listVal;
    WHILE currA # NIL DO
      (* Get value for this key in A *)
      valA := HashMapGetString(a, currA^.val^.strVal);

      (* Get value for this key in B *)
      IF NOT HashMapContainsString(b, currA^.val^.strVal) THEN
        RETURN FALSE;
      END;
      valB := HashMapGetString(b, currA^.val^.strVal);

      (* Compare values *)
      IF NOT MalEqual(valA, valB) THEN
        RETURN FALSE;
      END;

      currA := currA^.next;
    END;

    RETURN TRUE;
  END;

  (* Check if types match *)
  IF a^.type # b^.type THEN
    RETURN FALSE;
  END;

  CASE a^.type OF
    MAL_NIL, MAL_TRUE, MAL_FALSE:
      RETURN TRUE;
  | MAL_INTEGER:
      RETURN a^.intVal = b^.intVal;
  | MAL_SYMBOL, MAL_STRING, MAL_KEYWORD:
      (* Compare strings *)
      RETURN DynString.Equal(a^.strVal, b^.strVal);
  ELSE
    RETURN FALSE;
  END;
END MalEqual;

(* Equal - compare two values *)
PROCEDURE Equal(args: MalVal): MalVal;
VAR
  a, b: MalVal;
BEGIN
  IF ListLength(args) < 2 THEN
    RETURN NewFalse();
  END;
  a := ListGet(args, 0);
  b := ListGet(args, 1);
  IF MalEqual(a, b) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END Equal;

(* LessThan - compare two integers *)
PROCEDURE LessThan(args: MalVal): MalVal;
BEGIN
  RETURN IntComparison(args, CMP_LT);
END LessThan;

(* LessThanOrEqual - compare two integers *)
PROCEDURE LessThanOrEqual(args: MalVal): MalVal;
BEGIN
  RETURN IntComparison(args, CMP_LE);
END LessThanOrEqual;

(* GreaterThan - compare two integers *)
PROCEDURE GreaterThan(args: MalVal): MalVal;
BEGIN
  RETURN IntComparison(args, CMP_GT);
END GreaterThan;

(* GreaterThanOrEqual - compare two integers *)
PROCEDURE GreaterThanOrEqual(args: MalVal): MalVal;
BEGIN
  RETURN IntComparison(args, CMP_GE);
END GreaterThanOrEqual;

(* NotFn - logical NOT operation *)
PROCEDURE NotFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  arg := ListGet(args, 0);

  (* nil and false are falsey, everything else is truthy *)
  IF IsNil(arg) OR (arg^.type = MAL_FALSE) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END NotFn;

(* MakeList - create a list from arguments *)
PROCEDURE MakeList(args: MalVal): MalVal;
BEGIN
  (* args is already a list, just return it *)
  RETURN args;
END MakeList;

PROCEDURE MakeVector(args: MalVal): MalVal;
VAR
  result: MalVal;
  curr: MalList;
BEGIN
  (* Create new vector and copy all argument elements *)
  result := NewVector();
  curr := args^.listVal;
  WHILE curr # NIL DO
    ListAppend(result, curr^.val);
    curr := curr^.next;
  END;
  RETURN result;
END MakeVector;

(* NormalizeHashMap - remove duplicate keys, keeping last value *)
PROCEDURE NormalizeHashMap(hmap: MalVal): MalVal;
VAR
  result, key, val, tempKey: MalVal;
  curr, tempCurr: MalList;
  found: BOOLEAN;
BEGIN
  result := NewHashMap();
  curr := hmap^.listVal;

  WHILE curr # NIL DO
    key := curr^.val;
    curr := curr^.next;
    IF curr # NIL THEN
      val := curr^.val;

      (* Check if key already exists and update it *)
      found := FALSE;
      tempCurr := result^.listVal;
      WHILE tempCurr # NIL DO
        tempKey := tempCurr^.val;
        tempCurr := tempCurr^.next;
        IF tempCurr # NIL THEN
          IF (tempKey^.type = key^.type) AND DynString.Equal(tempKey^.strVal, key^.strVal) THEN
            (* Update existing value *)
            tempCurr^.val := val;
            found := TRUE;
          END;
          tempCurr := tempCurr^.next;
        END;
      END;

      (* If key not found, append new key-value pair *)
      IF NOT found THEN
        ListAppend(result, key);
        ListAppend(result, val);
      END;

      curr := curr^.next;
    END;
  END;

  RETURN result;
END NormalizeHashMap;

PROCEDURE MakeHashMap(args: MalVal): MalVal;
VAR
  hmap, keyVal: MalVal;
  curr: MalList;
BEGIN
  (* Create hash-map from arguments (key-value pairs) *)
  hmap := NewHashMap();
  curr := args^.listVal;

  WHILE curr # NIL DO
    (* Get key *)
    IF (curr^.val^.type = MAL_STRING) OR (curr^.val^.type = MAL_KEYWORD) THEN
      (* Save key, then move to value *)
      keyVal := curr^.val;
      curr := curr^.next;

      IF curr # NIL THEN
        (* Insert key-value pair into RB tree *)
        HashMapPutString(hmap, keyVal^.strVal, curr^.val);
        curr := curr^.next;
      END;
    ELSE
      (* Skip non-string keys *)
      curr := curr^.next;
      IF curr # NIL THEN
        curr := curr^.next;
      END;
    END;
  END;

  RETURN hmap;
END MakeHashMap;

(* GetFn - get value from hash-map by key *)
PROCEDURE GetFn(args: MalVal): MalVal;
VAR
  hmap, key: MalVal;
BEGIN
  hmap := ListGet(args, 0);

  (* Handle nil hash-map *)
  IF IsNil(hmap) THEN
    RETURN NewNil();
  END;

  key := ListGet(args, 1);

  (* Convert key to string *)
  IF (key^.type = MAL_STRING) OR (key^.type = MAL_KEYWORD) THEN
    RETURN HashMapGetString(hmap, key^.strVal);
  END;

  RETURN NewNil();
END GetFn;

(* AssocFn - associate key-value pairs in hash-map *)
PROCEDURE AssocFn(args: MalVal): MalVal;
VAR
  hmap, result, key, val: MalVal;
  argCount, i: CARDINAL;
BEGIN
  hmap := ListGet(args, 0);
  argCount := ListLength(args);

  (* Create new hash-map by copying the old one *)
  result := HashMapCopy(hmap);

  (* Add/update key-value pairs from arguments *)
  i := 1;
  WHILE i < argCount DO
    key := ListGet(args, i);
    val := ListGet(args, i + 1);

    IF (key^.type = MAL_STRING) OR (key^.type = MAL_KEYWORD) THEN
      HashMapPutString(result, key^.strVal, val);
    END;

    INC(i, 2);
  END;

  RETURN result;
END AssocFn;

(* ContainsFn - check if hash-map contains a key *)
PROCEDURE ContainsFn(args: MalVal): MalVal;
VAR
  hmap, key: MalVal;
BEGIN
  hmap := ListGet(args, 0);
  key := ListGet(args, 1);

  IF (key^.type = MAL_STRING) OR (key^.type = MAL_KEYWORD) THEN
    IF HashMapContainsString(hmap, key^.strVal) THEN
      RETURN NewTrue();
    END;
  END;

  RETURN NewFalse();
END ContainsFn;

(* DissocFn - dissociate keys from hash-map *)
PROCEDURE DissocFn(args: MalVal): MalVal;
VAR
  hmap, result, checkKey: MalVal;
  argCount, i: CARDINAL;
BEGIN
  hmap := ListGet(args, 0);
  argCount := ListLength(args);

  (* Create new hash-map by copying the old one *)
  result := HashMapCopy(hmap);

  (* Remove keys specified in arguments *)
  FOR i := 1 TO argCount - 1 DO
    checkKey := ListGet(args, i);
    IF (checkKey^.type = MAL_STRING) OR (checkKey^.type = MAL_KEYWORD) THEN
      HashMapDeleteString(result, checkKey^.strVal);
    END;
  END;

  RETURN result;
END DissocFn;

(* KeysFn - return list of all keys in hash-map *)
PROCEDURE KeysFn(args: MalVal): MalVal;
VAR
  hmap: MalVal;
BEGIN
  hmap := ListGet(args, 0);
  RETURN HashMapKeys(hmap);
END KeysFn;

(* ValsFn - return list of all values in hash-map *)
PROCEDURE ValsFn(args: MalVal): MalVal;
VAR
  hmap: MalVal;
BEGIN
  hmap := ListGet(args, 0);
  RETURN HashMapVals(hmap);
END ValsFn;

(* IsListFn - check if argument is a list *)
PROCEDURE IsListFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  IF ListLength(args) = 0 THEN
    RETURN NewFalse();
  END;
  arg := ListGet(args, 0);
  IF IsList(arg) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END IsListFn;

(* IsEmptyFn - check if list is empty *)
PROCEDURE IsEmptyFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  IF ListLength(args) = 0 THEN
    RETURN NewTrue();
  END;
  arg := ListGet(args, 0);
  IF IsNil(arg) OR (ListLength(arg) = 0) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END IsEmptyFn;

(* CountFn - return number of elements in a list *)
PROCEDURE CountFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  IF ListLength(args) = 0 THEN
    RETURN NewInteger(0);
  END;
  arg := ListGet(args, 0);
  IF IsNil(arg) THEN
    RETURN NewInteger(0);
  ELSE
    RETURN NewInteger(ListLength(arg));
  END;
END CountFn;

(* ConsFn - prepend element to list *)
PROCEDURE ConsFn(args: MalVal): MalVal;
VAR
  result: MalVal;
  elem, lst: MalVal;
  curr: MalList;
BEGIN
  elem := ListGet(args, 0);
  lst := ListGet(args, 1);

  (* Create new list *)
  result := NewList();

  (* Add the element first *)
  ListAppend(result, elem);

  (* Add all elements from the input list *)
  IF NOT IsNil(lst) THEN
    curr := lst^.listVal;
    WHILE curr # NIL DO
      ListAppend(result, curr^.val);
      curr := curr^.next;
    END;
  END;

  RETURN result;
END ConsFn;

(* ConcatFn - concatenate multiple lists *)
PROCEDURE ConcatFn(args: MalVal): MalVal;
VAR
  result: MalVal;
  curr, innerCurr: MalList;
  lst: MalVal;
BEGIN
  result := NewList();

  (* Iterate through each argument list *)
  curr := args^.listVal;
  WHILE curr # NIL DO
    lst := curr^.val;

    (* Append all elements from this list *)
    IF NOT IsNil(lst) THEN
      innerCurr := lst^.listVal;
      WHILE innerCurr # NIL DO
        ListAppend(result, innerCurr^.val);
        innerCurr := innerCurr^.next;
      END;
    END;

    curr := curr^.next;
  END;

  RETURN result;
END ConcatFn;

(* VecFn - convert a sequence (list or vector) to a vector *)
PROCEDURE VecFn(args: MalVal): MalVal;
VAR
  result: MalVal;
  seq: MalVal;
  curr: MalList;
BEGIN
  seq := ListGet(args, 0);

  (* If already a vector, return it as-is *)
  IF seq^.type = MAL_VECTOR THEN
    RETURN seq;
  END;

  (* Create new vector and copy all elements *)
  result := NewVector();

  IF NOT IsNil(seq) THEN
    curr := seq^.listVal;
    WHILE curr # NIL DO
      ListAppend(result, curr^.val);
      curr := curr^.next;
    END;
  END;

  RETURN result;
END VecFn;

(* NthFn - get element at index *)
PROCEDURE NthFn(args: MalVal): MalVal;
VAR
  seq, idxVal: MalVal;
  idx: INTEGER;
  i: INTEGER;
  curr: MalList;
BEGIN
  seq := ListGet(args, 0);
  idxVal := ListGet(args, 1);
  idx := idxVal^.intVal;

  (* Check if sequence is list or vector *)
  IF (seq^.type # MAL_LIST) AND (seq^.type # MAL_VECTOR) THEN
    SetError("nth: first argument must be a list or vector");
    RETURN NewNil();
  END;

  (* Check bounds *)
  IF idx < 0 THEN
    SetError("nth: index out of range");
    RETURN NewNil();
  END;

  (* Traverse to the nth element *)
  curr := seq^.listVal;
  i := 0;
  WHILE (curr # NIL) AND (i < idx) DO
    curr := curr^.next;
    INC(i);
  END;

  (* Check if we found it *)
  IF curr = NIL THEN
    SetError("nth: index out of range");
    RETURN NewNil();
  END;

  RETURN curr^.val;
END NthFn;

(* FirstFn - get first element *)
PROCEDURE FirstFn(args: MalVal): MalVal;
VAR
  seq: MalVal;
BEGIN
  seq := ListGet(args, 0);

  (* nil returns nil *)
  IF IsNil(seq) THEN
    RETURN NewNil();
  END;

  (* Check if sequence is list or vector *)
  IF (seq^.type # MAL_LIST) AND (seq^.type # MAL_VECTOR) THEN
    SetError("first: argument must be a list or vector");
    RETURN NewNil();
  END;

  (* Return first element or nil if empty *)
  IF seq^.listVal = NIL THEN
    RETURN NewNil();
  ELSE
    RETURN seq^.listVal^.val;
  END;
END FirstFn;

(* RestFn - get tail of sequence *)
PROCEDURE RestFn(args: MalVal): MalVal;
VAR
  seq: MalVal;
  result: MalVal;
  curr: MalList;
BEGIN
  seq := ListGet(args, 0);

  (* nil returns empty list *)
  IF IsNil(seq) THEN
    RETURN NewList();
  END;

  (* Check if sequence is list or vector *)
  IF (seq^.type # MAL_LIST) AND (seq^.type # MAL_VECTOR) THEN
    SetError("rest: argument must be a list or vector");
    RETURN NewNil();
  END;

  (* Create new list with all but first element *)
  result := NewList();

  IF seq^.listVal # NIL THEN
    curr := seq^.listVal^.next;  (* Skip first *)
    WHILE curr # NIL DO
      ListAppend(result, curr^.val);
      curr := curr^.next;
    END;
  END;

  RETURN result;
END RestFn;

(* Quasiquote - Transform quasiquoted form *)
PROCEDURE Quasiquote(ast: MalVal): MalVal;
VAR
  result, newList, vecWrapper: MalVal;
  elt, first: MalVal;
  symName: ARRAY [0..31] OF CHAR;
  i: INTEGER;
  isVector: BOOLEAN;
BEGIN
  (* Remember if input is a vector *)
  isVector := (ast^.type = MAL_VECTOR);

  (* Non-list/vector: quote symbols, return self-evaluating types as-is *)
  IF (ast^.type # MAL_LIST) AND (ast^.type # MAL_VECTOR) THEN
    (* Self-evaluating types: numbers, strings, keywords, nil, true, false *)
    IF (ast^.type = MAL_INTEGER) OR (ast^.type = MAL_STRING) OR
       (ast^.type = MAL_KEYWORD) OR (ast^.type = MAL_NIL) OR
       (ast^.type = MAL_TRUE) OR (ast^.type = MAL_FALSE) THEN
      RETURN ast;
    END;
    (* Symbols need to be quoted *)
    result := NewList();
    Assign("quote", symName);
    ListAppend(result, NewSymbol(symName));
    ListAppend(result, ast);
    RETURN result;
  END;

  (* Empty list/vector: wrap empty vectors with (vec ()) for consistency *)
  IF ListLength(ast) = 0 THEN
    IF isVector THEN
      (* Transform empty vector [] to (vec ()) *)
      vecWrapper := NewList();
      Assign("vec", symName);
      ListAppend(vecWrapper, NewSymbol(symName));
      ListAppend(vecWrapper, NewList());  (* Empty list as argument *)
      RETURN vecWrapper;
    ELSE
      (* Empty list stays as-is *)
      RETURN ast;
    END;
  END;

  (* Check first element *)
  first := ListGet(ast, 0);

  (* (unquote x) => x - only for lists, not vectors *)
  IF (ast^.type = MAL_LIST) AND (first^.type = MAL_SYMBOL) AND DynString.EqualArray(first^.strVal, "unquote") THEN
    RETURN ListGet(ast, 1);
  END;

  (* Build result from right to left *)
  (* Start with empty list and process elements in reverse *)
  result := NewList();

  (* Iterate from last to first *)
  i := ListLength(ast) - 1;
  WHILE i >= 0 DO
    elt := ListGet(ast, i);

    (* Check if element is (splice-unquote ...) *)
    IF (elt^.type = MAL_LIST) AND (ListLength(elt) > 0) THEN
      first := ListGet(elt, 0);
      IF (first^.type = MAL_SYMBOL) AND DynString.EqualArray(first^.strVal, "splice-unquote") THEN
        (* (splice-unquote x) => (concat x result) *)
        newList := NewList();
        Assign("concat", symName);
        ListAppend(newList, NewSymbol(symName));
        ListAppend(newList, ListGet(elt, 1));
        ListAppend(newList, result);
        result := newList;
      ELSE
        (* Regular element: (cons (quasiquote elt) result) *)
        newList := NewList();
        Assign("cons", symName);
        ListAppend(newList, NewSymbol(symName));
        ListAppend(newList, Quasiquote(elt));
        ListAppend(newList, result);
        result := newList;
      END;
    ELSE
      (* Regular element: (cons (quasiquote elt) result) *)
      newList := NewList();
      Assign("cons", symName);
      ListAppend(newList, NewSymbol(symName));
      ListAppend(newList, Quasiquote(elt));
      ListAppend(newList, result);
      result := newList;
    END;

    DEC(i);
  END;

  (* If input was a vector, wrap result with (vec ...) *)
  IF isVector THEN
    vecWrapper := NewList();
    Assign("vec", symName);
    ListAppend(vecWrapper, NewSymbol(symName));
    ListAppend(vecWrapper, result);
    RETURN vecWrapper;
  END;

  RETURN result;
END Quasiquote;

(* Atom operations *)

PROCEDURE AtomFn(args: MalVal): MalVal;
BEGIN
  RETURN NewAtom(ListGet(args, 0));
END AtomFn;

PROCEDURE IsAtomFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  arg := ListGet(args, 0);
  IF IsAtom(arg) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END IsAtomFn;

PROCEDURE IsMacroFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  arg := ListGet(args, 0);
  IF (arg^.type = MAL_FUNCTION) AND arg^.isMacro THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END IsMacroFn;

(* Exception operations *)

PROCEDURE ThrowFn(args: MalVal): MalVal;
BEGIN
  (* Throw the first argument as an exception *)
  SetException(ListGet(args, 0));
  RETURN NewNil();
END ThrowFn;

(* Type predicates *)

(* Helper for simple type predicates *)
PROCEDURE TypePredicate(args: MalVal; expectedType: MalType): MalVal;
VAR
  arg: MalVal;
BEGIN
  arg := ListGet(args, 0);
  IF arg^.type = expectedType THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END TypePredicate;

(* Helper for integer comparisons *)
PROCEDURE IntComparison(args: MalVal; op: CompareOp): MalVal;
VAR
  a, b: MalVal;
  result: BOOLEAN;
BEGIN
  a := ListGet(args, 0);
  b := ListGet(args, 1);

  CASE op OF
    CMP_LT: result := a^.intVal < b^.intVal;
  | CMP_LE: result := a^.intVal <= b^.intVal;
  | CMP_GT: result := a^.intVal > b^.intVal;
  | CMP_GE: result := a^.intVal >= b^.intVal;
  END;

  IF result THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END IntComparison;

PROCEDURE NilPredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_NIL);
END NilPredFn;

PROCEDURE TruePredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_TRUE);
END TruePredFn;

PROCEDURE FalsePredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_FALSE);
END FalsePredFn;

PROCEDURE SymbolPredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_SYMBOL);
END SymbolPredFn;

PROCEDURE KeywordPredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_KEYWORD);
END KeywordPredFn;

PROCEDURE SequentialPredFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  arg := ListGet(args, 0);
  IF (arg^.type = MAL_LIST) OR (arg^.type = MAL_VECTOR) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END SequentialPredFn;

PROCEDURE VectorPredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_VECTOR);
END VectorPredFn;

PROCEDURE MapPredFn(args: MalVal): MalVal;
BEGIN
  RETURN TypePredicate(args, MAL_HASHMAP);
END MapPredFn;

(* Symbol and keyword constructors *)

PROCEDURE SymbolFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
  trimmed: String;
BEGIN
  arg := ListGet(args, 0);
  IF arg^.type # MAL_STRING THEN
    SetError("symbol requires a string argument");
    RETURN NewNil();
  END;

  (* Remove quotes from string and create symbol *)
  trimmed := DynString.TrimQuotes(arg^.strVal);
  RETURN NewSymbolCopy(trimmed);
END SymbolFn;

PROCEDURE KeywordFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
  trimmed, colon, keywordStr: String;
  colonStr: ARRAY [0..1] OF CHAR;
BEGIN
  arg := ListGet(args, 0);

  (* If already a keyword, return it *)
  IF arg^.type = MAL_KEYWORD THEN
    RETURN arg;
  END;

  IF arg^.type # MAL_STRING THEN
    SetError("keyword requires a string argument");
    RETURN NewNil();
  END;

  (* Remove quotes from string and prepend : *)
  trimmed := DynString.TrimQuotes(arg^.strVal);
  Assign(":", colonStr);
  colon := DynString.Create(colonStr);
  keywordStr := DynString.Concat(colon, trimmed);
  RETURN NewKeywordCopy(keywordStr);
END KeywordFn;

(* Functional programming *)

(* Bind function parameters to arguments with variadic support *)
PROCEDURE BindFunctionParams(params: MalVal; args: MalVal; env: Env): BOOLEAN;
VAR
  curr: MalList;
  i: CARDINAL;
  restArgs: MalVal;
BEGIN
  (* Iterate through parameters *)
  curr := params^.listVal;
  i := 0;

  WHILE curr # NIL DO
    (* Check that parameter is a symbol *)
    IF curr^.val^.type # MAL_SYMBOL THEN
      SetError("Function parameter must be a symbol");
      RETURN FALSE;
    END;

    (* Check if this is the & symbol for variadic args *)
    IF DynString.EqualArray(curr^.val^.strVal, "&") THEN
      (* Next parameter gets all remaining arguments as a list *)
      curr := curr^.next;
      IF curr = NIL THEN
        SetError("& must be followed by a parameter name");
        RETURN FALSE;
      END;

      (* Verify the variadic param is a symbol *)
      IF curr^.val^.type # MAL_SYMBOL THEN
        SetError("Function parameter must be a symbol");
        RETURN FALSE;
      END;

      (* Create a list from remaining arguments *)
      restArgs := NewList();
      WHILE i < ListLength(args) DO
        ListAppend(restArgs, ListGet(args, i));
        INC(i);
      END;

      EnvSetString(env, curr^.val^.strVal, restArgs);
      curr := curr^.next;

      (* Should be no more parameters after variadic *)
      IF curr # NIL THEN
        SetError("No parameters allowed after & parameter");
        RETURN FALSE;
      END;
    ELSE
      (* Regular parameter *)
      IF i >= ListLength(args) THEN
        SetError("Too few arguments");
        RETURN FALSE;
      END;

      EnvSetString(env, curr^.val^.strVal, ListGet(args, i));
      curr := curr^.next;
      INC(i);
    END;
  END;

  (* Check if we used all arguments (unless we had variadic params) *)
  IF i # ListLength(args) THEN
    SetError("Too many arguments");
    RETURN FALSE;
  END;

  RETURN TRUE;
END BindFunctionParams;

PROCEDURE ApplyFn(args: MalVal): MalVal;
VAR
  fn: MalVal;
  fnArgs: MalVal;
  lastArg: MalVal;
  curr: MalList;
  i, argCount: CARDINAL;
  newEnv: Env;
BEGIN
  argCount := ListLength(args);
  IF argCount < 2 THEN
    SetError("apply requires at least 2 arguments");
    RETURN NewNil();
  END;

  fn := ListGet(args, 0);

  (* Build the argument list *)
  fnArgs := NewList();

  (* Add all arguments except the last *)
  FOR i := 1 TO argCount - 2 DO
    ListAppend(fnArgs, ListGet(args, i));
  END;

  (* Last argument should be a list or vector *)
  lastArg := ListGet(args, argCount - 1);
  IF (lastArg^.type # MAL_LIST) AND (lastArg^.type # MAL_VECTOR) THEN
    SetError("apply last argument must be a list or vector");
    RETURN NewNil();
  END;

  (* Append all elements from the last argument *)
  curr := lastArg^.listVal;
  WHILE curr # NIL DO
    ListAppend(fnArgs, curr^.val);
    curr := curr^.next;
  END;

  (* Call the function *)
  IF fn^.type = MAL_NATIVE_FN THEN
    RETURN fn^.nativeFn(fnArgs);
  ELSIF fn^.type = MAL_FUNCTION THEN
    (* For user functions, we need to call EVAL with the function body *)
    (* This requires the evaluator to be set *)
    IF globalEval = NIL THEN
      SetError("apply: evaluator not set");
      RETURN NewNil();
    END;

    (* Create environment and bind parameters *)
    newEnv := NewEnvOuter(CAST(Env, fn^.closure));

    IF NOT BindFunctionParams(fn^.params, fnArgs, newEnv) THEN
      RETURN NewNil();
    END;

    RETURN globalEval(fn^.body, newEnv);
  ELSE
    SetError("apply: first argument must be a function");
    RETURN NewNil();
  END;
END ApplyFn;

PROCEDURE MapFn(args: MalVal): MalVal;
VAR
  fn: MalVal;
  seq: MalVal;
  result: MalVal;
  curr: MalList;
  fnArgs: MalVal;
  mapped: MalVal;
  newEnv: Env;
BEGIN
  fn := ListGet(args, 0);
  seq := ListGet(args, 1);

  IF (seq^.type # MAL_LIST) AND (seq^.type # MAL_VECTOR) THEN
    SetError("map: second argument must be a list or vector");
    RETURN NewNil();
  END;

  result := NewList();
  curr := seq^.listVal;

  WHILE curr # NIL DO
    (* Create argument list with single element *)
    fnArgs := NewList();
    ListAppend(fnArgs, curr^.val);

    (* Call the function *)
    IF fn^.type = MAL_NATIVE_FN THEN
      mapped := fn^.nativeFn(fnArgs);
    ELSIF fn^.type = MAL_FUNCTION THEN
      IF globalEval = NIL THEN
        SetError("map: evaluator not set");
        RETURN NewNil();
      END;

      (* Create environment and bind parameter *)
      newEnv := NewEnvOuter(CAST(Env, fn^.closure));

      IF NOT BindFunctionParams(fn^.params, fnArgs, newEnv) THEN
        RETURN NewNil();
      END;

      mapped := globalEval(fn^.body, newEnv);
    ELSE
      SetError("map: first argument must be a function");
      RETURN NewNil();
    END;

    IF HasError() OR HasException() THEN
      RETURN result;
    END;

    ListAppend(result, mapped);
    curr := curr^.next;
  END;

  RETURN result;
END MapFn;

PROCEDURE DerefFn(args: MalVal): MalVal;
BEGIN
  RETURN AtomDeref(ListGet(args, 0));
END DerefFn;

PROCEDURE ResetFn(args: MalVal): MalVal;
VAR
  atom, val: MalVal;
BEGIN
  atom := ListGet(args, 0);
  val := ListGet(args, 1);
  RETURN AtomReset(atom, val);
END ResetFn;

(* swap! - Atomically swap the value of an atom using a function *)
PROCEDURE SwapFn(args: MalVal): MalVal;
VAR
  atom, fn, fnArgs, oldVal, newVal: MalVal;
  len, i: CARDINAL;
  newEnv: Env;
BEGIN
  atom := ListGet(args, 0);
  fn := ListGet(args, 1);
  oldVal := AtomDeref(atom);

  (* Build argument list for function: old value + any additional args *)
  fnArgs := NewList();
  ListAppend(fnArgs, oldVal);
  len := ListLength(args);
  IF len > 2 THEN
    FOR i := 2 TO len - 1 DO
      ListAppend(fnArgs, ListGet(args, i));
    END;
  END;

  (* Handle native functions *)
  IF fn^.type = MAL_NATIVE_FN THEN
    newVal := fn^.nativeFn(fnArgs);
  ELSIF fn^.type = MAL_FUNCTION THEN
    (* For user-defined functions, manually apply by binding params and evaluating body *)
    (* Create new environment with closure as outer *)
    newEnv := NewEnvOuter(CAST(Env, fn^.closure));

    IF NOT BindFunctionParams(fn^.params, fnArgs, newEnv) THEN
      RETURN NewNil();
    END;

    (* Use the global evaluator to evaluate the function body in the new environment *)
    newVal := globalEval(fn^.body, newEnv);
  ELSE
    SetError("swap! requires a function as second argument");
    RETURN NewNil();
  END;

  RETURN AtomReset(atom, newVal);
END SwapFn;

(* String/IO operations *)

PROCEDURE ReadStringFn(args: MalVal): MalVal;
VAR
  strVal: MalVal;
  trimmedStr: String;
  dataPtr: DynString.CharArrayPtr;
  str: ARRAY [0..8191] OF CHAR;
  i, maxLen: CARDINAL;
BEGIN
  strVal := ListGet(args, 0);
  (* Strip surrounding quotes if present *)
  trimmedStr := DynString.TrimQuotes(strVal^.strVal);

  (* Copy string to local array *)
  dataPtr := DynString.GetDataPtr(trimmedStr);
  maxLen := DynString.Length(trimmedStr);
  IF maxLen > 8191 THEN
    maxLen := 8191;
  END;
  IF maxLen > 0 THEN
    FOR i := 0 TO maxLen - 1 DO
      str[i] := dataPtr^[i];
    END;
  END;
  str[maxLen] := 0C;  (* Null terminate *)

  RETURN ReadStr(str);
END ReadStringFn;

PROCEDURE SlurpFn(args: MalVal): MalVal;
VAR
  content: ARRAY [0..8191] OF CHAR;
  ch: CHAR;
  cid: ChanId;
  openRes: OpenResults;
  readRes: ReadResults;
  pos, i, maxLen: CARDINAL;
  filenameVal: MalVal;
  trimmedFilename: String;
  filenamePtr: DynString.CharArrayPtr;
  filename: ARRAY [0..255] OF CHAR;
BEGIN
  (* Get filename from arguments *)
  filenameVal := ListGet(args, 0);
  IF filenameVal^.type # MAL_STRING THEN
    SetError("slurp requires a string filename");
    RETURN NewNil();
  END;

  (* Strip surrounding quotes if present *)
  trimmedFilename := DynString.TrimQuotes(filenameVal^.strVal);

  (* Copy filename to local array *)
  filenamePtr := DynString.GetDataPtr(trimmedFilename);
  maxLen := DynString.Length(trimmedFilename);
  IF maxLen > 255 THEN
    maxLen := 255;
  END;
  IF maxLen > 0 THEN
    FOR i := 0 TO maxLen - 1 DO
      filename[i] := filenamePtr^[i];
    END;
  END;
  filename[maxLen] := 0C;  (* Null terminate *)

  (* Open file for reading *)
  Open(cid, filename, StreamFile.read + StreamFile.old + StreamFile.raw, openRes);
  IF openRes # opened THEN
    SetError("Cannot open file");
    RETURN NewNil();
  END;

  (* Read file contents *)
  pos := 0;
  LOOP
    IF pos >= HIGH(content) - 2 THEN
      (* Buffer full *)
      FOR i := pos TO 1 BY -1 DO
        content[i] := content[i-1];
      END;
      content[0] := '"';
      content[pos+1] := '"';
      content[pos+2] := CHR(0);
      Close(cid);
      RETURN NewString(content);
    END;

    Read(cid, ch);
    readRes := ReadResult(cid);

    IF readRes = endOfInput THEN
      (* End of file - wrap in quotes *)
      content[pos] := CHR(0);
      FOR i := pos TO 1 BY -1 DO
        content[i] := content[i-1];
      END;
      content[0] := '"';
      content[pos+1] := '"';
      content[pos+2] := CHR(0);
      Close(cid);
      RETURN NewString(content);
    END;

    content[pos] := ch;
    INC(pos);
  END;
END SlurpFn;

(* Evaluation *)

PROCEDURE EvalFn(args: MalVal): MalVal;
VAR
  form: MalVal;
BEGIN
  (* Get the form to evaluate *)
  form := ListGet(args, 0);

  (* Evaluate in the REPL environment *)
  RETURN globalEval(form, globalReplEnv);
END EvalFn;

(* Printing operations *)

PROCEDURE PrStrFn(args: MalVal): MalVal;
VAR
  resultStr, tempStr: String;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  resultStr := DynString.CreateFromArray("");
  len := ListLength(args);
  IF len > 0 THEN
    FOR i := 0 TO len - 1 DO
      arg := ListGet(args, i);
      tempStr := PrStr(arg, TRUE);
      IF i > 0 THEN
        DynString.AppendChar(resultStr, ' ');
      END;
      DynString.AppendString(resultStr, tempStr);
    END;
  END;

  (* Wrap result in quotes for MAL string *)
  DynString.AddQuotesInPlace(resultStr);

  RETURN NewStringCopy(resultStr);
END PrStrFn;

PROCEDURE StrFn(args: MalVal): MalVal;
VAR
  resultStr, tempStr: String;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  resultStr := DynString.CreateFromArray("");
  len := ListLength(args);
  IF len > 0 THEN
    FOR i := 0 TO len - 1 DO
      arg := ListGet(args, i);
      tempStr := PrStr(arg, FALSE);
      DynString.AppendString(resultStr, tempStr);
    END;
  END;

  (* Wrap result in quotes for MAL string *)
  DynString.AddQuotesInPlace(resultStr);

  RETURN NewStringCopy(resultStr);
END StrFn;

PROCEDURE PrnFn(args: MalVal): MalVal;
VAR
  tempStr: String;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  len := ListLength(args);
  IF len > 0 THEN
    FOR i := 0 TO len - 1 DO
      arg := ListGet(args, i);
      tempStr := PrStr(arg, TRUE);
      IF i > 0 THEN
        WriteString(" ");
      END;
      DynString.WriteToChannel(tempStr, StdOutChan());
    END;
  END;
  WriteLn;
  RETURN NewNil();
END PrnFn;

PROCEDURE PrintlnFn(args: MalVal): MalVal;
VAR
  tempStr: String;
  i, len: CARDINAL;
  arg: MalVal;
BEGIN
  len := ListLength(args);
  IF len > 0 THEN
    FOR i := 0 TO len - 1 DO
      arg := ListGet(args, i);
      tempStr := PrStr(arg, FALSE);
      IF i > 0 THEN
        WriteString(" ");
      END;
      DynString.WriteToChannel(tempStr, StdOutChan());
    END;
  END;
  WriteLn;
  RETURN NewNil();
END PrintlnFn;

(* time-ms - Get current time in milliseconds *)
PROCEDURE TimeMsFn(args: MalVal): MalVal;
VAR
  dt: DateTime;
  ms: INTEGER;
BEGIN
  GetClock(dt);
  (* Convert to milliseconds: seconds * 1000 + fractions / 1000 *)
  (* Since fractions are in microseconds (maxSecondParts = 1000000), *)
  (* divide by 1000 to get milliseconds *)
  ms := (dt.second * 1000) + (dt.fractions DIV 1000);
  (* Add minutes, hours, days for a monotonic value *)
  ms := ms + (dt.minute * 60 * 1000);
  ms := ms + (dt.hour * 60 * 60 * 1000);
  (* For simplicity, we'll just use time within the current day *)
  (* This is sufficient for MAL tests *)
  RETURN NewInteger(ms);
END TimeMsFn;

(* string? - Check if value is a string *)
PROCEDURE StringPredFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  IF ListLength(args) # 1 THEN
    SetError("string? requires exactly 1 argument");
    RETURN NewNil();
  END;
  arg := ListGet(args, 0);
  IF arg^.type = MAL_STRING THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END StringPredFn;

(* number? - Check if value is a number *)
PROCEDURE NumberPredFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  IF ListLength(args) # 1 THEN
    SetError("number? requires exactly 1 argument");
    RETURN NewNil();
  END;
  arg := ListGet(args, 0);
  IF arg^.type = MAL_INTEGER THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END NumberPredFn;

(* fn? - Check if value is a function (but not a macro) *)
PROCEDURE FnPredFn(args: MalVal): MalVal;
VAR
  arg: MalVal;
BEGIN
  IF ListLength(args) # 1 THEN
    SetError("fn? requires exactly 1 argument");
    RETURN NewNil();
  END;
  arg := ListGet(args, 0);
  IF (arg^.type = MAL_NATIVE_FN) OR
     ((arg^.type = MAL_FUNCTION) AND NOT arg^.isMacro) THEN
    RETURN NewTrue();
  ELSE
    RETURN NewFalse();
  END;
END FnPredFn;

(* seq - Convert to sequence (list or vector to list, string to list of characters, nil to nil) *)
PROCEDURE SeqFn(args: MalVal): MalVal;
VAR
  arg, result: MalVal;
  i, len: CARDINAL;
  ch: CHAR;
  charStr: ARRAY [0..1] OF CHAR;
  srcPtr: DynString.CharArrayPtr;
BEGIN
  IF ListLength(args) # 1 THEN
    SetError("seq requires exactly 1 argument");
    RETURN NewNil();
  END;
  arg := ListGet(args, 0);

  (* nil returns nil *)
  IF arg^.type = MAL_NIL THEN
    RETURN NewNil();
  END;

  (* Empty list/vector/string returns nil *)
  IF ((arg^.type = MAL_LIST) OR (arg^.type = MAL_VECTOR)) AND (ListLength(arg) = 0) THEN
    RETURN NewNil();
  END;

  IF (arg^.type = MAL_STRING) THEN
    srcPtr := DynString.GetDataPtr(arg^.strVal);
    len := DynString.Length(arg^.strVal);
    (* Skip quotes if present *)
    IF (len >= 2) AND (srcPtr^[0] = '"') THEN
      IF len = 2 THEN
        RETURN NewNil(); (* Empty string *)
      END;
      result := NewList();
      FOR i := 1 TO len - 2 DO
        charStr[0] := srcPtr^[i];
        charStr[1] := CHR(0);
        ListAppend(result, NewString(charStr));
      END;
    ELSE
      IF len = 0 THEN
        RETURN NewNil();
      END;
      result := NewList();
      FOR i := 0 TO len - 1 DO
        charStr[0] := srcPtr^[i];
        charStr[1] := CHR(0);
        ListAppend(result, NewString(charStr));
      END;
    END;
    RETURN result;
  END;

  (* List returns as-is *)
  IF arg^.type = MAL_LIST THEN
    RETURN arg;
  END;

  (* Vector converts to list *)
  IF arg^.type = MAL_VECTOR THEN
    result := NewList();
    len := ListLength(arg);
    FOR i := 0 TO len - 1 DO
      ListAppend(result, ListGet(arg, i));
    END;
    RETURN result;
  END;

  SetError("seq: argument must be a list, vector, string, or nil");
  RETURN NewNil();
END SeqFn;

(* conj - Add elements to a collection *)
PROCEDURE ConjFn(args: MalVal): MalVal;
VAR
  coll, result: MalVal;
  i, len: CARDINAL;
  curr, newNode: MalList;
BEGIN
  IF ListLength(args) < 1 THEN
    SetError("conj requires at least 1 argument");
    RETURN NewNil();
  END;

  coll := ListGet(args, 0);

  (* List: prepend elements *)
  IF coll^.type = MAL_LIST THEN
    result := NewList();
    result^.listVal := coll^.listVal; (* Copy reference to original list *)

    (* Prepend new elements in forward order *)
    len := ListLength(args);
    FOR i := 1 TO len - 1 DO
      (* Create new node at head *)
      newNode := CAST(MalList, Alloc(TSIZE(MalListNode)));
      newNode^.val := ListGet(args, i);
      newNode^.next := result^.listVal;
      result^.listVal := newNode;
    END;
    RETURN result;
  END;

  (* Vector: append elements *)
  IF coll^.type = MAL_VECTOR THEN
    result := NewVector();
    (* Copy original elements *)
    len := ListLength(coll);
    IF len > 0 THEN
      FOR i := 0 TO len - 1 DO
        ListAppend(result, ListGet(coll, i));
      END;
    END;
    (* Append new elements *)
    len := ListLength(args);
    IF len > 1 THEN
      FOR i := 1 TO len - 1 DO
        ListAppend(result, ListGet(args, i));
      END;
    END;
    RETURN result;
  END;

  SetError("conj: first argument must be a list or vector");
  RETURN NewNil();
END ConjFn;

(* readline - Read a line from stdin with a prompt *)
PROCEDURE ReadlineFn(args: MalVal): MalVal;
VAR
  prompt: MalVal;
  line: ARRAY [0..1023] OF CHAR;
  res: ReadResults;
  stdin: ChanId;
  srcPtr: DynString.CharArrayPtr;
  i, len: CARDINAL;
BEGIN
  IF ListLength(args) # 1 THEN
    SetError("readline requires exactly 1 argument");
    RETURN NewNil();
  END;

  prompt := ListGet(args, 0);
  IF prompt^.type # MAL_STRING THEN
    SetError("readline: argument must be a string");
    RETURN NewNil();
  END;

  stdin := StdInChan();

  (* Print prompt (without quotes) *)
  srcPtr := DynString.GetDataPtr(prompt^.strVal);
  len := DynString.Length(prompt^.strVal);
  IF (len >= 2) AND (srcPtr^[0] = '"') THEN
    (* Skip quotes *)
    FOR i := 1 TO len - 2 DO
      WriteChar(StdOutChan(), srcPtr^[i]);
    END;
  ELSE
    DynString.WriteToChannel(prompt^.strVal, StdOutChan());
  END;

  (* Read line *)
  ReadString(stdin, line);
  res := ReadResult(stdin);

  IF res = endOfInput THEN
    RETURN NewNil();
  END;

  SkipLine(stdin);

  (* Wrap input in quotes to create a proper MAL string *)
  (* The input is raw user data, so we add quotes to make it a valid MAL string *)
  RETURN NewStringCopy(DynString.AddQuotes(DynString.Create(line)));
END ReadlineFn;

(* meta - Get metadata from a value *)
PROCEDURE MetaFn(args: MalVal): MalVal;
VAR
  obj: MalVal;
BEGIN
  IF ListLength(args) # 1 THEN
    SetError("meta requires exactly 1 argument");
    RETURN NewNil();
  END;
  obj := ListGet(args, 0);
  IF obj^.meta = NIL THEN
    RETURN NewNil();
  ELSE
    RETURN obj^.meta;
  END;
END MetaFn;

(* with-meta - Attach metadata to a value *)
PROCEDURE WithMetaFn(args: MalVal): MalVal;
VAR
  obj, metadata, result: MalVal;
  i, len: CARDINAL;
BEGIN
  IF ListLength(args) # 2 THEN
    SetError("with-meta requires exactly 2 arguments");
    RETURN NewNil();
  END;
  obj := ListGet(args, 0);
  metadata := ListGet(args, 1);

  (* Create a copy of the object with new metadata *)
  CASE obj^.type OF
    MAL_LIST:
      result := NewList();
      len := ListLength(obj);
      IF len > 0 THEN
        FOR i := 0 TO len - 1 DO
          ListAppend(result, ListGet(obj, i));
        END;
      END;
  | MAL_VECTOR:
      result := NewVector();
      len := ListLength(obj);
      IF len > 0 THEN
        FOR i := 0 TO len - 1 DO
          ListAppend(result, ListGet(obj, i));
        END;
      END;
  | MAL_HASHMAP:
      result := HashMapCopy(obj);
  | MAL_FUNCTION:
      result := NewFunction(obj^.params, obj^.body, obj^.closure, obj^.isMacro);
  | MAL_NATIVE_FN:
      result := NewNativeFn(obj^.nativeFn);
  ELSE
    (* For other immutable types, use the original object *)
    result := obj;
  END;

  (* Attach metadata *)
  result^.meta := metadata;
  RETURN result;
END WithMetaFn;

(* SetEvaluator - Store the EVAL procedure for use by swap! *)
PROCEDURE SetEvaluator(evalProc: EvalProc);
BEGIN
  globalEval := evalProc;
END SetEvaluator;

(* DefineLoadFile - Define load-file function in MAL *)
PROCEDURE DefineLoadFile(env: Env);
VAR
  malCode: ARRAY [0..511] OF CHAR;
  temp: ARRAY [0..127] OF CHAR;
  ast, result: MalVal;
BEGIN
  (* Build the MAL definition string character by character to handle quotes *)
  (* (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)"))))) *)

  Assign("(def! load-file (fn* (f) (eval (read-string (str ", malCode);

  (* Add: "(do " - a string containing quote-paren-do-space-quote *)
  temp[0] := '"';
  temp[1] := '(';
  temp[2] := 'd';
  temp[3] := 'o';
  temp[4] := ' ';
  temp[5] := '"';
  temp[6] := CHR(0);
  Concat(malCode, temp, malCode);

  (* Add: (slurp f) *)
  Concat(malCode, " (slurp f) ", malCode);

  (* Add: "\nnil)" - string containing quote-backslash-n-n-i-l-paren-quote *)
  temp[0] := '"';
  temp[1] := '\';
  temp[2] := 'n';
  temp[3] := 'n';
  temp[4] := 'i';
  temp[5] := 'l';
  temp[6] := ')';
  temp[7] := '"';
  temp[8] := CHR(0);
  Concat(malCode, temp, malCode);

  (* Close with ))))) *)
  Concat(malCode, ")))))", malCode);

  (* Parse and evaluate the definition *)
  ast := ReadStr(malCode);
  IF NOT HasError() THEN
    result := globalEval(ast, env);
  END;
  (* Ignore any errors - load-file is optional *)
  ClearError();
END DefineLoadFile;

(* InitReplEnv - Initialize the REPL environment with all core functions *)
(* Register arithmetic operations *)
PROCEDURE RegisterArithmeticFns(env: Env);
BEGIN
  EnvSetArray(env, "+", NewNativeFn(Add));
  EnvSetArray(env, "-", NewNativeFn(Sub));
  EnvSetArray(env, "*", NewNativeFn(Mul));
  EnvSetArray(env, "/", NewNativeFn(Div));
END RegisterArithmeticFns;

(* Register comparison operations *)
PROCEDURE RegisterComparisonFns(env: Env);
BEGIN
  EnvSetArray(env, "=", NewNativeFn(Equal));
  EnvSetArray(env, "<", NewNativeFn(LessThan));
  EnvSetArray(env, "<=", NewNativeFn(LessThanOrEqual));
  EnvSetArray(env, ">", NewNativeFn(GreaterThan));
  EnvSetArray(env, ">=", NewNativeFn(GreaterThanOrEqual));
  EnvSetArray(env, "not", NewNativeFn(NotFn));
END RegisterComparisonFns;

(* Register list and collection operations *)
PROCEDURE RegisterCollectionFns(env: Env);
BEGIN
  EnvSetArray(env, "list", NewNativeFn(MakeList));
  EnvSetArray(env, "list?", NewNativeFn(IsListFn));
  EnvSetArray(env, "empty?", NewNativeFn(IsEmptyFn));
  EnvSetArray(env, "count", NewNativeFn(CountFn));
  EnvSetArray(env, "cons", NewNativeFn(ConsFn));
  EnvSetArray(env, "concat", NewNativeFn(ConcatFn));
  EnvSetArray(env, "vec", NewNativeFn(VecFn));
  EnvSetArray(env, "vector", NewNativeFn(MakeVector));
  EnvSetArray(env, "nth", NewNativeFn(NthFn));
  EnvSetArray(env, "first", NewNativeFn(FirstFn));
  EnvSetArray(env, "rest", NewNativeFn(RestFn));
  EnvSetArray(env, "seq", NewNativeFn(SeqFn));
  EnvSetArray(env, "conj", NewNativeFn(ConjFn));
END RegisterCollectionFns;

(* Register hash-map operations *)
PROCEDURE RegisterHashMapFns(env: Env);
BEGIN
  EnvSetArray(env, "hash-map", NewNativeFn(MakeHashMap));
  EnvSetArray(env, "get", NewNativeFn(GetFn));
  EnvSetArray(env, "assoc", NewNativeFn(AssocFn));
  EnvSetArray(env, "contains?", NewNativeFn(ContainsFn));
  EnvSetArray(env, "dissoc", NewNativeFn(DissocFn));
  EnvSetArray(env, "keys", NewNativeFn(KeysFn));
  EnvSetArray(env, "vals", NewNativeFn(ValsFn));
END RegisterHashMapFns;

(* Register atom operations *)
PROCEDURE RegisterAtomFns(env: Env);
BEGIN
  EnvSetArray(env, "atom", NewNativeFn(AtomFn));
  EnvSetArray(env, "atom?", NewNativeFn(IsAtomFn));
  EnvSetArray(env, "deref", NewNativeFn(DerefFn));
  EnvSetArray(env, "reset!", NewNativeFn(ResetFn));
  EnvSetArray(env, "swap!", NewNativeFn(SwapFn));
END RegisterAtomFns;

(* Register type predicates *)
PROCEDURE RegisterTypePredicateFns(env: Env);
BEGIN
  EnvSetArray(env, "nil?", NewNativeFn(NilPredFn));
  EnvSetArray(env, "true?", NewNativeFn(TruePredFn));
  EnvSetArray(env, "false?", NewNativeFn(FalsePredFn));
  EnvSetArray(env, "symbol?", NewNativeFn(SymbolPredFn));
  EnvSetArray(env, "keyword?", NewNativeFn(KeywordPredFn));
  EnvSetArray(env, "sequential?", NewNativeFn(SequentialPredFn));
  EnvSetArray(env, "vector?", NewNativeFn(VectorPredFn));
  EnvSetArray(env, "map?", NewNativeFn(MapPredFn));
  EnvSetArray(env, "string?", NewNativeFn(StringPredFn));
  EnvSetArray(env, "number?", NewNativeFn(NumberPredFn));
  EnvSetArray(env, "fn?", NewNativeFn(FnPredFn));
END RegisterTypePredicateFns;

(* Register string/IO/printing operations *)
PROCEDURE RegisterIOFns(env: Env);
BEGIN
  EnvSetArray(env, "read-string", NewNativeFn(ReadStringFn));
  EnvSetArray(env, "slurp", NewNativeFn(SlurpFn));
  EnvSetArray(env, "pr-str", NewNativeFn(PrStrFn));
  EnvSetArray(env, "str", NewNativeFn(StrFn));
  EnvSetArray(env, "prn", NewNativeFn(PrnFn));
  EnvSetArray(env, "println", NewNativeFn(PrintlnFn));
  EnvSetArray(env, "readline", NewNativeFn(ReadlineFn));
  EnvSetArray(env, "time-ms", NewNativeFn(TimeMsFn));
END RegisterIOFns;

(* Register functional programming and meta operations *)
PROCEDURE RegisterMetaFns(env: Env);
BEGIN
  EnvSetArray(env, "symbol", NewNativeFn(SymbolFn));
  EnvSetArray(env, "keyword", NewNativeFn(KeywordFn));
  EnvSetArray(env, "apply", NewNativeFn(ApplyFn));
  EnvSetArray(env, "map", NewNativeFn(MapFn));
  EnvSetArray(env, "macro?", NewNativeFn(IsMacroFn));
  EnvSetArray(env, "throw", NewNativeFn(ThrowFn));
  EnvSetArray(env, "eval", NewNativeFn(EvalFn));
  EnvSetArray(env, "meta", NewNativeFn(MetaFn));
  EnvSetArray(env, "with-meta", NewNativeFn(WithMetaFn));
END RegisterMetaFns;

PROCEDURE InitReplEnv(argc: CARDINAL): Env;
VAR
  replEnv: Env;
  argv: MalVal;
  arg: ARRAY [0..1023] OF CHAR;
  i: CARDINAL;
BEGIN
  (* Pre-intern common symbols for fast lookup *)
  PreInternSymbols();

  (* Create REPL environment with hash table for optimal performance *)
  replEnv := NewReplEnv();

  (* Register all core functions by category *)
  RegisterArithmeticFns(replEnv);
  RegisterComparisonFns(replEnv);
  RegisterCollectionFns(replEnv);
  RegisterHashMapFns(replEnv);
  RegisterAtomFns(replEnv);
  RegisterTypePredicateFns(replEnv);
  RegisterIOFns(replEnv);
  RegisterMetaFns(replEnv);

  (* Store REPL environment for eval function to use *)
  globalReplEnv := replEnv;

  (* Define load-file in MAL: (def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)"))))) *)
  (* This must be done after eval is registered *)
  IF globalEval # NIL THEN
    DefineLoadFile(replEnv);
  END;

  (* Initialize *ARGV* with command line arguments *)
  argv := NewList();
  IF argc > 2 THEN
    FOR i := 2 TO argc - 1 DO
      IF Args.GetArg(arg, i) THEN
        ListAppend(argv, NewString(arg));
      END;
    END;
  END;
  EnvSetArray(replEnv, "*ARGV*", argv);

  RETURN replEnv;
END InitReplEnv;

END Core.
