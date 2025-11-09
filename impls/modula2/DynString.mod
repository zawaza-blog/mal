IMPLEMENTATION MODULE DynString;

(* Dynamic String Module - Implementation *)

FROM SYSTEM IMPORT ADDRESS, CAST, TSIZE;
IMPORT Strings;
FROM libc IMPORT malloc, free, realloc;
FROM IOChan IMPORT ChanId;
FROM IOResult IMPORT ReadResult, ReadResults;
IMPORT TextIO;

TYPE
  CharPtr = POINTER TO ARRAY [0..1000000] OF CHAR;

(* Constructor - create a new string from an array *)
PROCEDURE Create(VAR src: ARRAY OF CHAR): String;
VAR
  str: String;
  len: CARDINAL;
  srcPtr, destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  len := Strings.Length(src);

  (* Allocate String record *)
  str := CAST(String, malloc(TSIZE(StringRec)));
  str^.length := len;

  (* Allocate data buffer *)
  str^.data := malloc(len + 1);

  (* Copy string content *)
  destPtr := CAST(CharPtr, str^.data);
  FOR i := 0 TO len DO
    destPtr^[i] := src[i];
  END;

  RETURN str;
END Create;

PROCEDURE CreateFromArray(src: ARRAY OF CHAR): String;
VAR
  str: String;
  len: CARDINAL;
  srcPtr, destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  len := Strings.Length(src);

  (* Allocate String record *)
  str := CAST(String, malloc(TSIZE(StringRec)));
  IF str = NIL THEN
    RETURN NIL;
  END;
  str^.length := len;

  (* Allocate data buffer *)
  str^.data := malloc(len + 1);
  IF str^.data = NIL THEN
    free(CAST(ADDRESS, str));
    RETURN NIL;
  END;

  (* Copy string content *)
  destPtr := CAST(CharPtr, str^.data);
  FOR i := 0 TO len DO
    destPtr^[i] := src[i];
  END;

  RETURN str;
END CreateFromArray;

(* Destructor - free a string *)
PROCEDURE Dispose(VAR str: String);
BEGIN
  IF str # NIL THEN
    IF str^.data # NIL THEN
      free(str^.data);
      str^.data := NIL;
    END;
    free(CAST(ADDRESS, str));
    str := NIL;
  END;
END Dispose;

(* Concatenate two strings into a new string *)
PROCEDURE Concat(str1, str2: String): String;
VAR
  result: String;
BEGIN
  IF (str1 = NIL) OR (str2 = NIL) THEN
    RETURN NIL;
  END;

  result := Copy(str1);
  AppendString(result, str2);
  RETURN result;
END Concat;

(* Concatenate a String with an ARRAY OF CHAR *)
PROCEDURE ConcatArray(str: String; arr: ARRAY OF CHAR): String;
VAR
  result: String;
BEGIN
  IF str = NIL THEN
    RETURN CreateFromArray(arr);
  END;

  result := Copy(str);
  AppendArray(result, arr);
  RETURN result;
END ConcatArray;

(* Concatenate an ARRAY OF CHAR with a String *)
PROCEDURE ArrayConcat(arr: ARRAY OF CHAR; str: String): String;
VAR
  result: String;
BEGIN
  IF str = NIL THEN
    RETURN CreateFromArray(arr);
  END;

  result := CreateFromArray(arr);
  AppendString(result, str);
  RETURN result;
END ArrayConcat;

(* Concatenate ARRAY + String + ARRAY (common pattern for error messages) *)
PROCEDURE Concat3(arr1: ARRAY OF CHAR; str: String; arr2: ARRAY OF CHAR): String;
VAR
  result: String;
BEGIN
  result := CreateFromArray(arr1);
  IF str # NIL THEN
    AppendString(result, str);
  END;
  AppendArray(result, arr2);
  RETURN result;
END Concat3;

(* Create a copy of a string *)
PROCEDURE Copy(str: String): String;
VAR
  result: String;
  srcPtr, destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  IF str = NIL THEN
    RETURN NIL;
  END;

  (* Allocate result String record *)
  result := CAST(String, malloc(TSIZE(StringRec)));
  result^.length := str^.length;

  (* Allocate data buffer *)
  result^.data := malloc(str^.length + 1);

  (* Copy data *)
  srcPtr := CAST(CharPtr, str^.data);
  destPtr := CAST(CharPtr, result^.data);
  FOR i := 0 TO str^.length DO
    destPtr^[i] := srcPtr^[i];
  END;

  RETURN result;
END Copy;

(* Update string from a fixed array *)
PROCEDURE AssignFromArray(VAR src: ARRAY OF CHAR; str: String);
VAR
  len: CARDINAL;
  destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  IF str = NIL THEN
    RETURN;
  END;

  len := Strings.Length(src);

  (* Free old data if length changed *)
  IF str^.data # NIL THEN
    free(str^.data);
  END;

  (* Allocate new data buffer *)
  str^.length := len;
  str^.data := malloc(len + 1);

  (* Copy content *)
  destPtr := CAST(CharPtr, str^.data);
  FOR i := 0 TO len DO
    destPtr^[i] := src[i];
  END;
END AssignFromArray;

(* Get string length *)
PROCEDURE Length(str: String): CARDINAL;
BEGIN
  IF str = NIL THEN
    RETURN 0;
  END;
  RETURN str^.length;
END Length;

(* Get data pointer *)
PROCEDURE GetData(str: String): ADDRESS;
BEGIN
  IF str = NIL THEN
    RETURN NIL;
  END;
  RETURN str^.data;
END GetData;

(* Get direct pointer to internal data for zero-copy operations *)
PROCEDURE GetDataPtr(str: String): CharArrayPtr;
BEGIN
  (* str should never be NIL if used correctly *)
  RETURN CAST(CharArrayPtr, str^.data);
END GetDataPtr;

(* Compare two strings lexicographically *)
PROCEDURE Compare(str1, str2: String): INTEGER;
VAR
  ptr1, ptr2: CharPtr;
  i, minLen: CARDINAL;
BEGIN
  IF (str1 = NIL) AND (str2 = NIL) THEN
    RETURN 0;
  ELSIF str1 = NIL THEN
    RETURN -1;
  ELSIF str2 = NIL THEN
    RETURN 1;
  END;

  ptr1 := CAST(CharPtr, str1^.data);
  ptr2 := CAST(CharPtr, str2^.data);

  minLen := str1^.length;
  IF str2^.length < minLen THEN
    minLen := str2^.length;
  END;

  (* Only compare characters if both strings have content *)
  IF minLen > 0 THEN
    FOR i := 0 TO minLen - 1 DO
      IF ptr1^[i] < ptr2^[i] THEN
        RETURN -1;
      ELSIF ptr1^[i] > ptr2^[i] THEN
        RETURN 1;
      END;
    END;
  END;

  (* All characters matched, compare lengths *)
  IF str1^.length < str2^.length THEN
    RETURN -1;
  ELSIF str1^.length > str2^.length THEN
    RETURN 1;
  ELSE
    RETURN 0;
  END;
END Compare;

(* Check if two strings are equal *)
PROCEDURE Equal(str1, str2: String): BOOLEAN;
BEGIN
  RETURN Compare(str1, str2) = 0;
END Equal;

(* Get character at index *)
PROCEDURE GetChar(str: String; index: CARDINAL): CHAR;
VAR
  ptr: CharPtr;
BEGIN
  IF (str = NIL) OR (index >= str^.length) THEN
    RETURN 0C;
  END;

  ptr := CAST(CharPtr, str^.data);
  RETURN ptr^[index];
END GetChar;

(* Compare String with array - for comparing with literals *)
PROCEDURE EqualArray(str: String; arr: ARRAY OF CHAR): BOOLEAN;
VAR
  arrLen: CARDINAL;
  i: CARDINAL;
  ptr: CharPtr;
BEGIN
  (* Check for NIL string *)
  IF str = NIL THEN
    RETURN FALSE;
  END;

  (* Get array length *)
  arrLen := Strings.Length(arr);

  (* Compare lengths first - quick reject *)
  IF str^.length # arrLen THEN
    RETURN FALSE;
  END;

  (* Compare character by character *)
  IF arrLen > 0 THEN
    ptr := CAST(CharPtr, str^.data);
    FOR i := 0 TO arrLen - 1 DO
      IF ptr^[i] # arr[i] THEN
        RETURN FALSE;
      END;
    END;
  END;

  RETURN TRUE;
END EqualArray;

(* Remove surrounding quotes from a string *)
PROCEDURE TrimQuotes(str: String): String;
VAR
  srcPtr, destPtr: CharPtr;
  newStr: String;
  i, newLen: CARDINAL;
BEGIN
  (* Check for NIL or empty string *)
  IF (str = NIL) OR (str^.length < 2) THEN
    RETURN str;
  END;

  srcPtr := CAST(CharPtr, str^.data);

  (* Check if surrounded by quotes *)
  IF (srcPtr^[0] = '"') AND (srcPtr^[str^.length - 1] = '"') THEN
    (* Create new string without quotes *)
    newLen := str^.length - 2;
    newStr := CAST(String, malloc(TSIZE(StringRec)));
    newStr^.length := newLen;
    newStr^.data := malloc(newLen + 1);

    destPtr := CAST(CharPtr, newStr^.data);
    (* Only copy if there's content between quotes *)
    IF newLen > 0 THEN
      FOR i := 0 TO newLen - 1 DO
        destPtr^[i] := srcPtr^[i + 1];
      END;
    END;
    destPtr^[newLen] := 0C;

    RETURN newStr;
  ELSE
    (* No quotes - return copy *)
    RETURN Copy(str);
  END;
END TrimQuotes;

(* Add quotes around a string *)
PROCEDURE AddQuotes(str: String): String;
VAR
  result: String;
BEGIN
  IF str = NIL THEN
    RETURN NIL;
  END;

  result := Copy(str);
  AddQuotesInPlace(result);
  RETURN result;
END AddQuotes;

(* Read entire channel contents into a String *)
PROCEDURE ReadFromChannel(cid: ChanId): String;
CONST
  INITIAL_SIZE = 256;
  GROW_FACTOR = 2;
VAR
  buffer: ADDRESS;
  bufferPtr: CharPtr;
  size, pos: CARDINAL;
  ch: CHAR;
  res: ReadResults;
  result: String;
  tempBuffer: ADDRESS;
  tempPtr: CharPtr;
  i: CARDINAL;
BEGIN
  (* Allocate initial buffer *)
  size := INITIAL_SIZE;
  buffer := malloc(size);
  bufferPtr := CAST(CharPtr, buffer);
  pos := 0;

  LOOP
    (* Read one character *)
    TextIO.ReadChar(cid, ch);
    res := ReadResult(cid);

    (* Check if end of input *)
    IF res = endOfInput THEN
      EXIT;
    END;

    (* Skip if not a valid read *)
    IF (res # allRight) AND (res # endOfLine) THEN
      (* Exit on error to avoid infinite loop *)
      EXIT;
    END;

    (* Check if we need to grow the buffer *)
    IF pos >= size - 1 THEN
      (* Allocate larger buffer *)
      tempBuffer := malloc(size * GROW_FACTOR);
      tempPtr := CAST(CharPtr, tempBuffer);

      (* Copy existing data *)
      IF pos > 0 THEN
        FOR i := 0 TO pos - 1 DO
          tempPtr^[i] := bufferPtr^[i];
        END;
      END;

      (* Free old buffer and use new one *)
      free(buffer);
      buffer := tempBuffer;
      bufferPtr := tempPtr;
      size := size * GROW_FACTOR;
    END;

    (* Store character *)
    bufferPtr^[pos] := ch;
    INC(pos);
  END;

  (* Null-terminate *)
  bufferPtr^[pos] := 0C;

  (* Create String from buffer *)
  result := CAST(String, malloc(TSIZE(StringRec)));
  result^.length := pos;
  result^.data := buffer;

  RETURN result;
END ReadFromChannel;

(* Write string to channel *)
PROCEDURE WriteToChannel(str: String; cid: ChanId);
VAR
  ptr: CharPtr;
  i: CARDINAL;
BEGIN
  IF (str = NIL) OR (str^.data = NIL) THEN
    RETURN;
  END;

  (* Only write if string has content *)
  IF str^.length > 0 THEN
    ptr := CAST(CharPtr, str^.data);
    FOR i := 0 TO str^.length - 1 DO
      TextIO.WriteChar(cid, ptr^[i]);
    END;
  END;
END WriteToChannel;

(* In-place append String to another String using realloc *)
PROCEDURE AppendString(VAR str: String; other: String);
VAR
  oldLen, newLen: CARDINAL;
  srcPtr, destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  IF str = NIL THEN
    str := Copy(other);
    RETURN;
  END;

  IF other = NIL THEN
    RETURN;
  END;

  oldLen := str^.length;
  newLen := oldLen + other^.length;

  (* Resize the data buffer *)
  str^.data := realloc(str^.data, newLen + 1);
  str^.length := newLen;

  (* Copy other string to end *)
  IF other^.length > 0 THEN
    srcPtr := CAST(CharPtr, other^.data);
    destPtr := CAST(CharPtr, str^.data);
    FOR i := 0 TO other^.length - 1 DO
      destPtr^[oldLen + i] := srcPtr^[i];
    END;
  END;

  (* Null terminate *)
  destPtr := CAST(CharPtr, str^.data);
  destPtr^[newLen] := 0C;
END AppendString;

(* In-place append ARRAY OF CHAR to String using realloc *)
PROCEDURE AppendArray(VAR str: String; arr: ARRAY OF CHAR);
VAR
  oldLen, arrLen, newLen: CARDINAL;
  destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  arrLen := Strings.Length(arr);

  IF str = NIL THEN
    str := CreateFromArray(arr);
    RETURN;
  END;

  IF arrLen = 0 THEN
    RETURN;
  END;

  oldLen := str^.length;
  newLen := oldLen + arrLen;

  (* Resize the data buffer *)
  str^.data := realloc(str^.data, newLen + 1);
  str^.length := newLen;

  (* Copy array to end *)
  destPtr := CAST(CharPtr, str^.data);
  FOR i := 0 TO arrLen - 1 DO
    destPtr^[oldLen + i] := arr[i];
  END;

  (* Null terminate *)
  destPtr^[newLen] := 0C;
END AppendArray;

(* In-place append single character using realloc *)
PROCEDURE AppendChar(VAR str: String; ch: CHAR);
VAR
  oldLen, newLen: CARDINAL;
  destPtr: CharPtr;
BEGIN
  IF str = NIL THEN
    (* Create new string with single character *)
    str := CAST(String, malloc(TSIZE(StringRec)));
    str^.length := 1;
    str^.data := malloc(2);
    destPtr := CAST(CharPtr, str^.data);
    destPtr^[0] := ch;
    destPtr^[1] := 0C;
    RETURN;
  END;

  oldLen := str^.length;
  newLen := oldLen + 1;

  (* Resize the data buffer *)
  str^.data := realloc(str^.data, newLen + 1);
  str^.length := newLen;

  (* Add character *)
  destPtr := CAST(CharPtr, str^.data);
  destPtr^[oldLen] := ch;
  destPtr^[newLen] := 0C;
END AppendChar;

(* Escape special characters in a String *)
PROCEDURE EscapeString(str: String): String;
VAR
  result: String;
  srcPtr: CharPtr;
  i, len: CARDINAL;
  ch: CHAR;
BEGIN
  IF str = NIL THEN
    RETURN CreateFromArray("");
  END;

  result := CreateFromArray("");
  len := str^.length;
  srcPtr := CAST(CharPtr, str^.data);

  (* Only process if string has content *)
  IF len > 0 THEN
    FOR i := 0 TO len - 1 DO
      ch := srcPtr^[i];
      IF ch = '\' THEN
        AppendChar(result, '\');
        AppendChar(result, '\');
      ELSIF ch = '"' THEN
        AppendChar(result, '\');
        AppendChar(result, '"');
      ELSIF ch = 12C THEN  (* newline *)
        AppendChar(result, '\');
        AppendChar(result, 'n');
      ELSE
        AppendChar(result, ch);
      END;
    END;
  END;

  RETURN result;
END EscapeString;

(* In-place add quotes using realloc *)
PROCEDURE AddQuotesInPlace(VAR str: String);
VAR
  oldLen, newLen: CARDINAL;
  srcPtr, destPtr: CharPtr;
  i: CARDINAL;
BEGIN
  IF str = NIL THEN
    RETURN;
  END;

  oldLen := str^.length;
  newLen := oldLen + 2;

  (* Resize the data buffer *)
  str^.data := realloc(str^.data, newLen + 1);
  str^.length := newLen;

  destPtr := CAST(CharPtr, str^.data);

  (* Shift existing content right by 1 *)
  FOR i := oldLen TO 1 BY -1 DO
    destPtr^[i] := destPtr^[i - 1];
  END;

  (* Add quotes *)
  destPtr^[0] := '"';
  destPtr^[newLen - 1] := '"';
  destPtr^[newLen] := 0C;
END AddQuotesInPlace;

END DynString.
