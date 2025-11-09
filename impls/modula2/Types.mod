IMPLEMENTATION MODULE Types;

(* MAL Types - Implementation Module *)

FROM Strings IMPORT Assign, Length;
FROM SYSTEM IMPORT TSIZE, ADR, CAST, ADDRESS;
FROM libc IMPORT malloc, free;
FROM DynString IMPORT String;
IMPORT DynString;

(* Fast allocation using libc malloc to avoid RTentity overhead *)
PROCEDURE Alloc(size: CARDINAL): ADDRESS;
BEGIN
  RETURN malloc(size);
END Alloc;

(* Constructors *)

PROCEDURE NewNil(): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_NIL;
  val^.meta := NIL;
  RETURN val;
END NewNil;

PROCEDURE NewTrue(): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_TRUE;
  val^.meta := NIL;
  RETURN val;
END NewTrue;

PROCEDURE NewFalse(): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_FALSE;
  val^.meta := NIL;
  RETURN val;
END NewFalse;

PROCEDURE NewInteger(value: INTEGER): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_INTEGER;
  val^.intVal := value;
  val^.meta := NIL;
  RETURN val;
END NewInteger;

PROCEDURE NewSymbol(VAR str: ARRAY OF CHAR): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_SYMBOL;
  val^.strVal := DynString.Create(str);
  val^.meta := NIL;
  RETURN val;
END NewSymbol;

PROCEDURE NewString(VAR str: ARRAY OF CHAR): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_STRING;
  val^.strVal := DynString.Create(str);
  val^.meta := NIL;
  RETURN val;
END NewString;

PROCEDURE NewKeyword(VAR str: ARRAY OF CHAR): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_KEYWORD;
  val^.strVal := DynString.Create(str);
  val^.meta := NIL;
  RETURN val;
END NewKeyword;

(* Create a new string MalVal by copying an existing String *)
(* Create a new symbol MalVal by copying an existing String *)
PROCEDURE NewSymbolCopy(str: String): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_SYMBOL;
  val^.strVal := DynString.Copy(str);
  val^.meta := NIL;
  RETURN val;
END NewSymbolCopy;

PROCEDURE NewStringCopy(str: String): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_STRING;
  val^.strVal := DynString.Copy(str);
  val^.meta := NIL;
  RETURN val;
END NewStringCopy;

(* Create a new keyword MalVal by copying an existing String *)
PROCEDURE NewKeywordCopy(str: String): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_KEYWORD;
  val^.strVal := DynString.Copy(str);
  val^.meta := NIL;
  RETURN val;
END NewKeywordCopy;

PROCEDURE NewList(): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_LIST;
  val^.listVal := NIL;
  val^.listTail := NIL;  (* Initialize tail pointer *)
  val^.meta := NIL;
  RETURN val;
END NewList;

PROCEDURE NewVector(): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_VECTOR;
  val^.listVal := NIL;
  val^.listTail := NIL;  (* Initialize tail pointer *)
  val^.meta := NIL;
  RETURN val;
END NewVector;

PROCEDURE NewHashMap(): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_HASHMAP;
  val^.treeRoot := NIL;
  val^.treeSize := 0;
  val^.meta := NIL;
  RETURN val;
END NewHashMap;

PROCEDURE NewNativeFn(fn: NativeFn): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_NATIVE_FN;
  val^.nativeFn := fn;
  val^.meta := NIL;
  RETURN val;
END NewNativeFn;

PROCEDURE NewFunction(params: MalVal; body: MalVal; env: ADDRESS; isMacro: BOOLEAN): MalVal;
VAR
  val: MalVal;
BEGIN
  val := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  val^.type := MAL_FUNCTION;
  val^.params := params;
  val^.body := body;
  val^.closure := env;
  val^.isMacro := isMacro;
  val^.meta := NIL;
  RETURN val;
END NewFunction;

PROCEDURE NewAtom(val: MalVal): MalVal;
VAR
  atom: MalVal;
BEGIN
  atom := CAST(MalVal, Alloc(TSIZE(MalValRec)));
  atom^.type := MAL_ATOM;
  atom^.atomVal := val;
  atom^.meta := NIL;
  RETURN atom;
END NewAtom;

(* Atom operations *)

PROCEDURE AtomDeref(atom: MalVal): MalVal;
BEGIN
  RETURN atom^.atomVal;
END AtomDeref;

PROCEDURE AtomReset(atom: MalVal; val: MalVal): MalVal;
BEGIN
  atom^.atomVal := val;
  RETURN val;
END AtomReset;

(* List operations *)

PROCEDURE ListAppend(list: MalVal; val: MalVal);
VAR
  node: MalList;
BEGIN
  node := CAST(MalList, Alloc(TSIZE(MalListNode)));
  node^.val := val;
  node^.next := NIL;

  IF list^.listVal = NIL THEN
    (* Empty list - set both head and tail *)
    list^.listVal := node;
    list^.listTail := node;
  ELSE
    (* Non-empty list - append to tail in O(1) *)
    list^.listTail^.next := node;
    list^.listTail := node;
  END;
END ListAppend;

PROCEDURE ListLength(list: MalVal): CARDINAL;
VAR
  count: CARDINAL;
  curr: MalList;
BEGIN
  count := 0;
  curr := list^.listVal;
  WHILE curr # NIL DO
    INC(count);
    curr := curr^.next;
  END;
  RETURN count;
END ListLength;

PROCEDURE ListGet(list: MalVal; index: CARDINAL): MalVal;
VAR
  i: CARDINAL;
  curr: MalList;
BEGIN
  i := 0;
  curr := list^.listVal;
  WHILE (curr # NIL) AND (i < index) DO
    INC(i);
    curr := curr^.next;
  END;
  IF curr # NIL THEN
    RETURN curr^.val;
  ELSE
    RETURN NIL;
  END;
END ListGet;

(* Type checking *)

PROCEDURE IsNil(val: MalVal): BOOLEAN;
BEGIN
  RETURN (val = NIL) OR (val^.type = MAL_NIL);
END IsNil;

PROCEDURE IsList(val: MalVal): BOOLEAN;
BEGIN
  RETURN (val # NIL) AND (val^.type = MAL_LIST);
END IsList;

PROCEDURE IsVector(val: MalVal): BOOLEAN;
BEGIN
  RETURN (val # NIL) AND (val^.type = MAL_VECTOR);
END IsVector;

PROCEDURE IsSeq(val: MalVal): BOOLEAN;
BEGIN
  RETURN IsList(val) OR IsVector(val);
END IsSeq;

PROCEDURE IsAtom(val: MalVal): BOOLEAN;
BEGIN
  RETURN val^.type = MAL_ATOM;
END IsAtom;

PROCEDURE IsSpecialForm(val: MalVal; formName: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF val = NIL THEN
    RETURN FALSE;
  END;
  IF val^.type # MAL_SYMBOL THEN
    RETURN FALSE;
  END;
  RETURN DynString.EqualArray(val^.strVal, formName);
END IsSpecialForm;

(* Memory management *)

PROCEDURE FreeMalVal(val: MalVal);
VAR
  curr, next: MalList;
BEGIN
  (* NOTE: Memory management disabled - too risky without proper GC *)
  (* MalVal objects may be referenced in multiple places: *)
  (*   - Environment bindings (def!, let*, function closures) *)
  (*   - Data structure sharing (same list used in multiple places) *)
  (*   - Macro expansions *)
  (* Freeing here causes use-after-free bugs. *)
  (* The DynString leaks are fixed separately. *)
  RETURN;
END FreeMalVal;

(* ===== Red-Black Tree Implementation ===== *)

(* Create a new RB tree node from String *)
PROCEDURE NewRBNodeString(key: String; value: MalVal): RBNode;
VAR
  node: RBNode;
BEGIN
  node := CAST(RBNode, Alloc(TSIZE(RBNodeRec)));
  node^.color := RED;  (* New nodes are always red *)
  node^.key := DynString.Copy(key);
  node^.value := value;
  node^.left := NIL;
  node^.right := NIL;
  node^.parent := NIL;
  RETURN node;
END NewRBNodeString;

(* Left rotation *)
PROCEDURE RotateLeft(VAR root: RBNode; x: RBNode);
VAR
  y: RBNode;
BEGIN
  y := x^.right;
  x^.right := y^.left;

  IF y^.left # NIL THEN
    y^.left^.parent := x;
  END;

  y^.parent := x^.parent;

  IF x^.parent = NIL THEN
    root := y;
  ELSIF x = x^.parent^.left THEN
    x^.parent^.left := y;
  ELSE
    x^.parent^.right := y;
  END;

  y^.left := x;
  x^.parent := y;
END RotateLeft;

(* Right rotation *)
PROCEDURE RotateRight(VAR root: RBNode; y: RBNode);
VAR
  x: RBNode;
BEGIN
  x := y^.left;
  y^.left := x^.right;

  IF x^.right # NIL THEN
    x^.right^.parent := y;
  END;

  x^.parent := y^.parent;

  IF y^.parent = NIL THEN
    root := x;
  ELSIF y = y^.parent^.right THEN
    y^.parent^.right := x;
  ELSE
    y^.parent^.left := x;
  END;

  x^.right := y;
  y^.parent := x;
END RotateRight;

(* Fix RB tree after insertion *)
PROCEDURE RBInsertFixup(VAR root: RBNode; z: RBNode);
VAR
  y: RBNode;
BEGIN
  WHILE (z^.parent # NIL) AND (z^.parent^.color = RED) DO
    IF z^.parent = z^.parent^.parent^.left THEN
      y := z^.parent^.parent^.right;
      IF (y # NIL) AND (y^.color = RED) THEN
        z^.parent^.color := BLACK;
        y^.color := BLACK;
        z^.parent^.parent^.color := RED;
        z := z^.parent^.parent;
      ELSE
        IF z = z^.parent^.right THEN
          z := z^.parent;
          RotateLeft(root, z);
        END;
        z^.parent^.color := BLACK;
        z^.parent^.parent^.color := RED;
        RotateRight(root, z^.parent^.parent);
      END;
    ELSE
      y := z^.parent^.parent^.left;
      IF (y # NIL) AND (y^.color = RED) THEN
        z^.parent^.color := BLACK;
        y^.color := BLACK;
        z^.parent^.parent^.color := RED;
        z := z^.parent^.parent;
      ELSE
        IF z = z^.parent^.left THEN
          z := z^.parent;
          RotateRight(root, z);
        END;
        z^.parent^.color := BLACK;
        z^.parent^.parent^.color := RED;
        RotateLeft(root, z^.parent^.parent);
      END;
    END;
  END;
  root^.color := BLACK;
END RBInsertFixup;

(* Insert a key-value pair into RB tree *)
(* Insert a key-value pair into RB tree using String key *)
PROCEDURE RBInsertString(VAR root: RBNode; key: String; value: MalVal);
VAR
  z, y, x: RBNode;
  cmp: INTEGER;
BEGIN
  z := NewRBNodeString(key, value);

  y := NIL;
  x := root;

  WHILE x # NIL DO
    y := x;
    cmp := DynString.Compare(z^.key, x^.key);
    IF cmp < 0 THEN
      x := x^.left;
    ELSIF cmp > 0 THEN
      x := x^.right;
    ELSE
      (* Key exists, update value *)
      x^.value := value;
      RETURN;
    END;
  END;

  z^.parent := y;

  IF y = NIL THEN
    root := z;
  ELSIF DynString.Compare(z^.key, y^.key) < 0 THEN
    y^.left := z;
  ELSE
    y^.right := z;
  END;

  IF z^.parent = NIL THEN
    z^.color := BLACK;
    RETURN;
  END;

  IF z^.parent^.parent = NIL THEN
    RETURN;
  END;

  RBInsertFixup(root, z);
END RBInsertString;

(* Search for a key in RB tree using String *)
PROCEDURE RBSearchString(root: RBNode; key: String): MalVal;
VAR
  x: RBNode;
  cmp: INTEGER;
BEGIN
  x := root;
  WHILE x # NIL DO
    cmp := DynString.Compare(key, x^.key);
    IF cmp = 0 THEN
      RETURN x^.value;
    ELSIF cmp < 0 THEN
      x := x^.left;
    ELSE
      x := x^.right;
    END;
  END;
  RETURN NewNil();
END RBSearchString;

(* Check if key exists in RB tree using String *)
PROCEDURE RBContainsString(root: RBNode; key: String): BOOLEAN;
VAR
  x: RBNode;
  cmp: INTEGER;
BEGIN
  x := root;
  WHILE x # NIL DO
    cmp := DynString.Compare(key, x^.key);
    IF cmp = 0 THEN
      RETURN TRUE;
    ELSIF cmp < 0 THEN
      x := x^.left;
    ELSE
      x := x^.right;
    END;
  END;
  RETURN FALSE;
END RBContainsString;

(* Count nodes in RB tree *)
PROCEDURE RBSize(root: RBNode): CARDINAL;
BEGIN
  IF root = NIL THEN
    RETURN 0;
  ELSE
    RETURN 1 + RBSize(root^.left) + RBSize(root^.right);
  END;
END RBSize;

(* Simplified delete using String - recreate tree without the key *)
PROCEDURE RBDeleteHelperString(root: RBNode; key: String; VAR newRoot: RBNode);
VAR
  cmp: INTEGER;
BEGIN
  IF root = NIL THEN
    RETURN;
  END;

  cmp := DynString.Compare(key, root^.key);
  IF cmp # 0 THEN
    (* Not the node to delete, copy it *)
    RBInsertString(newRoot, root^.key, root^.value);
  END;

  (* Process children *)
  RBDeleteHelperString(root^.left, key, newRoot);
  RBDeleteHelperString(root^.right, key, newRoot);
END RBDeleteHelperString;

PROCEDURE RBDeleteString(VAR root: RBNode; key: String);
VAR
  newRoot: RBNode;
BEGIN
  newRoot := NIL;
  RBDeleteHelperString(root, key, newRoot);
  root := newRoot;
END RBDeleteString;

(* Hash-map operations *)

PROCEDURE HashMapPutString(hmap: MalVal; key: String; value: MalVal);
BEGIN
  RBInsertString(hmap^.treeRoot, key, value);
  hmap^.treeSize := RBSize(hmap^.treeRoot);
END HashMapPutString;

PROCEDURE HashMapGetString(hmap: MalVal; key: String): MalVal;
BEGIN
  RETURN RBSearchString(hmap^.treeRoot, key);
END HashMapGetString;

PROCEDURE HashMapContainsString(hmap: MalVal; key: String): BOOLEAN;
BEGIN
  RETURN RBContainsString(hmap^.treeRoot, key);
END HashMapContainsString;

PROCEDURE HashMapDeleteString(hmap: MalVal; key: String);
BEGIN
  RBDeleteString(hmap^.treeRoot, key);
  hmap^.treeSize := RBSize(hmap^.treeRoot);
END HashMapDeleteString;

(* In-order traversal to collect keys *)
PROCEDURE CollectKeys(node: RBNode; result: MalVal);
VAR
  keyVal: MalVal;
BEGIN
  IF node = NIL THEN
    RETURN;
  END;

  CollectKeys(node^.left, result);

  (* Create MalVal for key *)
  IF DynString.GetChar(node^.key, 0) = ':' THEN
    keyVal := NewKeywordCopy(node^.key);
  ELSE
    keyVal := NewStringCopy(node^.key);
  END;
  ListAppend(result, keyVal);

  CollectKeys(node^.right, result);
END CollectKeys;

PROCEDURE HashMapKeys(hmap: MalVal): MalVal;
VAR
  result: MalVal;
BEGIN
  result := NewList();
  CollectKeys(hmap^.treeRoot, result);
  RETURN result;
END HashMapKeys;

(* In-order traversal to collect values *)
PROCEDURE CollectVals(node: RBNode; result: MalVal);
BEGIN
  IF node = NIL THEN
    RETURN;
  END;

  CollectVals(node^.left, result);
  ListAppend(result, node^.value);
  CollectVals(node^.right, result);
END CollectVals;

PROCEDURE HashMapVals(hmap: MalVal): MalVal;
VAR
  result: MalVal;
BEGIN
  result := NewList();
  CollectVals(hmap^.treeRoot, result);
  RETURN result;
END HashMapVals;

(* Copy all nodes to new hash-map *)
PROCEDURE CopyTree(node: RBNode; newHmap: MalVal);
BEGIN
  IF node = NIL THEN
    RETURN;
  END;

  CopyTree(node^.left, newHmap);
  HashMapPutString(newHmap, node^.key, node^.value);
  CopyTree(node^.right, newHmap);
END CopyTree;

PROCEDURE HashMapCopy(hmap: MalVal): MalVal;
VAR
  result: MalVal;
BEGIN
  result := NewHashMap();
  CopyTree(hmap^.treeRoot, result);
  RETURN result;
END HashMapCopy;

END Types.
