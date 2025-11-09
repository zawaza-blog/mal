IMPLEMENTATION MODULE Env;

(* MAL Environment - Implementation Module *)
(* Optimized with symbol interning, hash table for REPL, and symbol cache *)

FROM Types IMPORT MalVal, NewNil, Alloc;
FROM Strings IMPORT Assign, Equal, Length;
FROM SYSTEM IMPORT ADDRESS, CAST, TSIZE, ADR;
IMPORT DynString;

CONST
  HASH_SIZE = 32;          (* Size of hash table for REPL environment *)
  SYMBOL_TABLE_SIZE = 128; (* Size of symbol intern table *)
  CACHE_SIZE = 32;         (* Number of cached frequently-used symbols *)

TYPE
  (* Type aliases for casting *)
  PtrToString = POINTER TO ARRAY [0..255] OF CHAR;
  PtrToHashTable = POINTER TO ARRAY [0..31] OF Binding;

  (* Cache entry for frequently accessed symbols *)
  CacheEntry = RECORD
    sym: Symbol;
    env: Env;
    val: MalVal;
    valid: BOOLEAN;
  END;

  (* Intern cache entry - caches string->Symbol mapping *)
  InternCacheEntry = RECORD
    str: DynString.String;
    sym: Symbol;
    valid: BOOLEAN;
  END;

VAR
  (* Symbol interning table - global hash table of all symbols *)
  symbolTable: ARRAY [0..SYMBOL_TABLE_SIZE-1] OF Symbol;

  (* Cache for frequently accessed symbols *)
  cache: ARRAY [0..CACHE_SIZE-1] OF CacheEntry;
  cacheIndex: CARDINAL;  (* Round-robin index for cache replacement *)

  (* Intern cache - caches recent string->Symbol lookups *)
  internCache: ARRAY [0..CACHE_SIZE-1] OF InternCacheEntry;
  internCacheIndex: CARDINAL;

  (* Profiling counters *)
  envCreateCount: CARDINAL;
  envGetCallCount: CARDINAL;
  envGetStepCount: CARDINAL;
  envSetCallCount: CARDINAL;
  cacheHits: CARDINAL;
  cacheMisses: CARDINAL;

(* Simple hash function for strings *)
PROCEDURE HashString(key: ARRAY OF CHAR): CARDINAL;
VAR
  hash: CARDINAL;
  i: CARDINAL;
BEGIN
  hash := 0;
  i := 0;
  WHILE (i < HIGH(key) + 1) AND (key[i] # 0C) DO
    hash := (hash * 31 + ORD(key[i])) MOD 65521;  (* Use prime modulus *)
    INC(i);
  END;
  RETURN hash;
END HashString;

(* Intern a symbol - return existing or create new *)
PROCEDURE InternSymbol(key: ARRAY OF CHAR): Symbol;
VAR
  hash, index: CARDINAL;
  current: Symbol;
  newSym: Symbol;
  len: CARDINAL;
  i: CARDINAL;
BEGIN
  (* Check intern cache first - this avoids string comparisons for common symbols *)
  FOR i := 0 TO CACHE_SIZE - 1 DO
    IF internCache[i].valid AND DynString.EqualArray(internCache[i].str, key) THEN
      (* Found in intern cache - return it immediately *)
      RETURN internCache[i].sym;
    END;
  END;

  (* Compute hash *)
  hash := HashString(key);
  index := hash MOD SYMBOL_TABLE_SIZE;

  (* Search for existing symbol in intern table *)
  current := symbolTable[index];
  WHILE current # NIL DO
    IF DynString.EqualArray(current^.str, key) THEN
      (* Found existing interned symbol - add to intern cache and return *)
      internCache[internCacheIndex].str := current^.str;
      internCache[internCacheIndex].sym := current;
      internCache[internCacheIndex].valid := TRUE;
      internCacheIndex := (internCacheIndex + 1) MOD CACHE_SIZE;
      RETURN current;
    END;
    current := current^.next;
  END;

  (* Not found - create new interned symbol *)
  newSym := CAST(Symbol, Alloc(TSIZE(SymbolRec)));
  newSym^.str := DynString.Create(key);
  newSym^.hash := hash;
  newSym^.next := symbolTable[index];
  symbolTable[index] := newSym;

  (* Add to intern cache *)
  internCache[internCacheIndex].str := newSym^.str;
  internCache[internCacheIndex].sym := newSym;
  internCache[internCacheIndex].valid := TRUE;
  internCacheIndex := (internCacheIndex + 1) MOD CACHE_SIZE;

  RETURN newSym;
END InternSymbol;

(* Check cache for symbol lookup *)
PROCEDURE CheckCache(env: Env; sym: Symbol; VAR val: MalVal): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO CACHE_SIZE - 1 DO
    IF cache[i].valid AND (cache[i].sym = sym) AND (cache[i].env = env) THEN
      val := cache[i].val;
      INC(cacheHits);
      RETURN TRUE;
    END;
  END;
  INC(cacheMisses);
  RETURN FALSE;
END CheckCache;

(* Update cache with new binding *)
PROCEDURE UpdateCache(env: Env; sym: Symbol; val: MalVal);
BEGIN
  cache[cacheIndex].sym := sym;
  cache[cacheIndex].env := env;
  cache[cacheIndex].val := val;
  cache[cacheIndex].valid := TRUE;
  cacheIndex := (cacheIndex + 1) MOD CACHE_SIZE;
END UpdateCache;

(* Invalidate cache entries for an environment *)
PROCEDURE InvalidateCache(env: Env);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO CACHE_SIZE - 1 DO
    IF cache[i].valid AND (cache[i].env = env) THEN
      cache[i].valid := FALSE;
    END;
  END;
END InvalidateCache;

(* Create a new environment *)
PROCEDURE NewEnv(): Env;
VAR
  env: Env;
BEGIN
  env := CAST(Env, Alloc(TSIZE(EnvRec)));
  env^.bindings := NIL;
  env^.hashTable := NIL;  (* No hash table for local envs *)
  env^.outer := NIL;
  INC(envCreateCount);
  RETURN env;
END NewEnv;

(* Create a new environment with outer scope *)
PROCEDURE NewEnvOuter(outerEnv: Env): Env;
VAR
  env: Env;
BEGIN
  env := CAST(Env, Alloc(TSIZE(EnvRec)));
  env^.bindings := NIL;
  env^.hashTable := NIL;  (* No hash table for local envs *)
  env^.outer := outerEnv;
  INC(envCreateCount);
  RETURN env;
END NewEnvOuter;

(* Create REPL environment with hash table *)
PROCEDURE NewReplEnv(): Env;
VAR
  env: Env;
  i: CARDINAL;
BEGIN
  env := CAST(Env, Alloc(TSIZE(EnvRec)));
  env^.bindings := NIL;
  env^.hashTable := Alloc(HASH_SIZE * TSIZE(Binding));  (* ARRAY [0..31] OF Binding *)

  (* Initialize hash table buckets *)
  FOR i := 0 TO HASH_SIZE - 1 DO
    env^.hashTable^[i] := NIL;
  END;

  env^.outer := NIL;
  INC(envCreateCount);
  RETURN env;
END NewReplEnv;

(* Set a symbol in the environment using String - primary implementation *)
PROCEDURE EnvSetString(env: Env; key: String; val: MalVal);
VAR
  keyStr: ARRAY [0..255] OF CHAR;
  srcPtr: DynString.CharArrayPtr;
  i, maxLen: CARDINAL;
  sym: Symbol;
  current: Binding;
  newBinding: Binding;
  hashIndex: CARDINAL;
BEGIN
  IF key = NIL THEN
    (* Cannot set with NIL key - this is a programming error *)
    RETURN;
  END;

  INC(envSetCallCount);

  (* Convert String to array for InternSymbol *)
  srcPtr := DynString.GetDataPtr(key);
  maxLen := DynString.Length(key);
  IF maxLen > 255 THEN
    maxLen := 255;
  END;
  IF maxLen > 0 THEN
    FOR i := 0 TO maxLen - 1 DO
      keyStr[i] := srcPtr^[i];
    END;
  END;
  keyStr[maxLen] := 0C;  (* Null terminate *)

  (* Intern the symbol *)
  sym := InternSymbol(keyStr);

  (* Invalidate any cached entries for this environment *)
  InvalidateCache(env);

  (* If this is a REPL environment with hash table, use it *)
  IF env^.hashTable # NIL THEN
    hashIndex := sym^.hash MOD HASH_SIZE;

    (* Check if symbol already exists in this hash bucket *)
    current := env^.hashTable^[hashIndex];
    WHILE current # NIL DO
      INC(envGetStepCount);  (* Count comparison *)
      IF current^.sym = sym THEN  (* Pointer comparison - O(1)! *)
        (* Update existing binding *)
        current^.val := val;
        RETURN;
      END;
      current := current^.next;
    END;

    (* Not found - create new binding at head of hash bucket *)
    newBinding := CAST(Binding, Alloc(TSIZE(BindingRec)));
    newBinding^.sym := sym;
    newBinding^.val := val;
    newBinding^.next := env^.hashTable^[hashIndex];
    env^.hashTable^[hashIndex] := newBinding;
    RETURN;
  END;

  (* For local environments, use linked list *)
  (* Check if symbol already exists *)
  current := env^.bindings;
  WHILE current # NIL DO
    INC(envGetStepCount);  (* Count comparison *)
    IF current^.sym = sym THEN  (* Pointer comparison - O(1)! *)
      (* Update existing binding *)
      current^.val := val;
      RETURN;
    END;
    current := current^.next;
  END;

  (* Not found - create new binding *)
  newBinding := CAST(Binding, Alloc(TSIZE(BindingRec)));
  newBinding^.sym := sym;
  newBinding^.val := val;
  newBinding^.next := env^.bindings;
  env^.bindings := newBinding;
END EnvSetString;

(* Set a symbol in the environment - wrapper for array-based API *)
PROCEDURE EnvSetArray(env: Env; key: ARRAY OF CHAR; val: MalVal);
VAR
  keyStr: String;
BEGIN
  keyStr := DynString.Create(key);
  EnvSetString(env, keyStr, val);
  DynString.Dispose(keyStr);  (* Free temporary string *)
END EnvSetArray;

(* Get a symbol from the environment using String - primary implementation *)
PROCEDURE EnvGetString(env: Env; key: String; VAR found: BOOLEAN): MalVal;
VAR
  keyStr: ARRAY [0..255] OF CHAR;
  srcPtr: DynString.CharArrayPtr;
  i, maxLen: CARDINAL;
  sym: Symbol;
  current: Binding;
  hashIndex: CARDINAL;
  result: MalVal;
BEGIN
  IF key = NIL THEN
    (* Cannot get with NIL key - this is a programming error *)
    found := FALSE;
    RETURN NewNil();
  END;

  INC(envGetCallCount);

  (* Convert String to array for InternSymbol *)
  srcPtr := DynString.GetDataPtr(key);
  maxLen := DynString.Length(key);
  IF maxLen > 255 THEN
    maxLen := 255;
  END;
  IF maxLen > 0 THEN
    FOR i := 0 TO maxLen - 1 DO
      keyStr[i] := srcPtr^[i];
    END;
  END;
  keyStr[maxLen] := 0C;  (* Null terminate *)

  (* Intern the symbol for fast comparison *)
  sym := InternSymbol(keyStr);

  (* Check cache first *)
  IF CheckCache(env, sym, result) THEN
    found := TRUE;
    RETURN result;
  END;

  (* If this is a REPL environment with hash table, use it *)
  IF env^.hashTable # NIL THEN
    hashIndex := sym^.hash MOD HASH_SIZE;

    (* Search in hash bucket *)
    current := env^.hashTable^[hashIndex];
    WHILE current # NIL DO
      INC(envGetStepCount);  (* Count comparison *)
      IF current^.sym = sym THEN  (* Pointer comparison - O(1)! *)
        found := TRUE;
        UpdateCache(env, sym, current^.val);
        RETURN current^.val;
      END;
      current := current^.next;
    END;

    (* Not found in REPL env - REPL has no outer *)
    found := FALSE;
    RETURN NewNil();
  END;

  (* For local environments, search linked list *)
  current := env^.bindings;
  WHILE current # NIL DO
    INC(envGetStepCount);  (* Count comparison *)
    IF current^.sym = sym THEN  (* Pointer comparison - O(1)! *)
      found := TRUE;
      UpdateCache(env, sym, current^.val);
      RETURN current^.val;
    END;
    current := current^.next;
  END;

  (* Not found in current environment - check outer *)
  IF env^.outer # NIL THEN
    INC(envGetStepCount);  (* Count step to outer environment *)
    RETURN EnvGetString(env^.outer, key, found);
  END;

  (* Not found anywhere *)
  found := FALSE;
  RETURN NewNil();
END EnvGetString;

(* Get a symbol from the environment - wrapper for array-based API *)
PROCEDURE EnvGetArray(env: Env; key: ARRAY OF CHAR; VAR found: BOOLEAN): MalVal;
VAR
  keyStr: String;
  result: MalVal;
BEGIN
  keyStr := DynString.Create(key);
  result := EnvGetString(env, keyStr, found);
  DynString.Dispose(keyStr);  (* Free temporary string *)
  RETURN result;
END EnvGetArray;

(* Check if symbol exists - deprecated, use EnvGetArray with found flag *)
PROCEDURE EnvFindArray(env: Env; key: ARRAY OF CHAR): BOOLEAN;
VAR
  found: BOOLEAN;
  dummy: MalVal;
BEGIN
  dummy := EnvGetArray(env, key, found);
  RETURN found;
END EnvFindArray;

(* Get profiling statistics *)
PROCEDURE GetEnvStats(VAR creates, gets, getSteps, sets: CARDINAL);
BEGIN
  creates := envCreateCount;
  gets := envGetCallCount;
  getSteps := envGetStepCount;
  sets := envSetCallCount;
END GetEnvStats;

(* Get cache statistics *)
PROCEDURE GetCacheStats(VAR hits, misses: CARDINAL);
BEGIN
  hits := cacheHits;
  misses := cacheMisses;
END GetCacheStats;

(* Pre-intern common REPL symbols *)
PROCEDURE PreInternSymbols();
VAR
  sym: Symbol;
BEGIN
  (* Pre-intern common arithmetic operators *)
  sym := InternSymbol("+");
  sym := InternSymbol("-");
  sym := InternSymbol("*");
  sym := InternSymbol("/");

  (* Pre-intern comparison operators *)
  sym := InternSymbol("=");
  sym := InternSymbol("<");
  sym := InternSymbol("<=");
  sym := InternSymbol(">");
  sym := InternSymbol(">=");

  (* Pre-intern common functions *)
  sym := InternSymbol("list");
  sym := InternSymbol("list?");
  sym := InternSymbol("empty?");
  sym := InternSymbol("count");
  sym := InternSymbol("prn");
  sym := InternSymbol("println");

  (* Pre-intern special forms *)
  sym := InternSymbol("def!");
  sym := InternSymbol("let*");
  sym := InternSymbol("if");
  sym := InternSymbol("do");
  sym := InternSymbol("fn*");

  (* Pre-intern common variable names used in tests *)
  sym := InternSymbol("n");
  sym := InternSymbol("acc");
  sym := InternSymbol("sum2");
END PreInternSymbols;

(* Reset profiling statistics *)
PROCEDURE ResetEnvStats();
VAR
  i: CARDINAL;
BEGIN
  envCreateCount := 0;
  envGetCallCount := 0;
  envGetStepCount := 0;
  envSetCallCount := 0;
  cacheHits := 0;
  cacheMisses := 0;

  (* Clear cache *)
  FOR i := 0 TO CACHE_SIZE - 1 DO
    cache[i].sym := NIL;
    cache[i].env := NIL;
    cache[i].val := NIL;
    cache[i].valid := FALSE;
  END;
  cacheIndex := 0;

  (* Clear intern cache *)
  FOR i := 0 TO CACHE_SIZE - 1 DO
    internCache[i].str := NIL;
    internCache[i].sym := NIL;
    internCache[i].valid := FALSE;
  END;
  internCacheIndex := 0;
END ResetEnvStats;

BEGIN
  (* Initialize profiling counters *)
  envCreateCount := 0;
  envGetCallCount := 0;
  envGetStepCount := 0;
  envSetCallCount := 0;
  cacheHits := 0;
  cacheMisses := 0;
  cacheIndex := 0;
  internCacheIndex := 0;

  (* Initialize symbol table *)
  FOR envCreateCount := 0 TO SYMBOL_TABLE_SIZE - 1 DO
    symbolTable[envCreateCount] := NIL;
  END;
  envCreateCount := 0;  (* Reset after using as loop variable *)

  (* Initialize cache *)
  FOR envSetCallCount := 0 TO CACHE_SIZE - 1 DO
    cache[envSetCallCount].sym := NIL;
    cache[envSetCallCount].env := NIL;
    cache[envSetCallCount].val := NIL;
    cache[envSetCallCount].valid := FALSE;
  END;
  envSetCallCount := 0;  (* Reset after using as loop variable *)

  (* Initialize intern cache *)
  FOR envGetCallCount := 0 TO CACHE_SIZE - 1 DO
    internCache[envGetCallCount].str := NIL;
    internCache[envGetCallCount].sym := NIL;
    internCache[envGetCallCount].valid := FALSE;
  END;
  envGetCallCount := 0;  (* Reset after using as loop variable *)

END Env.
