IMPLEMENTATION MODULE Reader;

(* MAL Reader - Implementation Module *)

FROM Types IMPORT MalVal, MalType, NewNil, NewTrue, NewFalse, NewInteger,
                   NewSymbol, NewString, NewKeyword, NewList, NewVector,
                   NewHashMap, ListAppend, HashMapPutString;
FROM Strings IMPORT Assign, Length, Concat;
FROM WholeStr IMPORT StrToInt, ConvResults;
FROM DynString IMPORT String;
IMPORT DynString;
IMPORT IOChan;
FROM SYSTEM IMPORT ADR, CAST;

CONST
  MAX_TOKENS = 4096;

TYPE
  Token = ARRAY [0..255] OF CHAR;
  TokenArray = ARRAY [0..MAX_TOKENS-1] OF Token;

VAR
  tokens: TokenArray;
  tokenCount: CARDINAL;
  position: CARDINAL;
  errorMsg: String;
  hasErr: BOOLEAN;
  initialized: BOOLEAN;
  exceptionVal: MalVal;
  hasExc: BOOLEAN;

(* Module initialization *)
PROCEDURE Initialize();
BEGIN
  IF NOT initialized THEN
    tokenCount := 0;
    position := 0;
    hasErr := FALSE;
    (* errorMsg is already initialized in module init block *)
    hasExc := FALSE;
    exceptionVal := NIL;
    initialized := TRUE;
  END;
END Initialize;

(* Error handling *)

PROCEDURE SetError(msg: ARRAY OF CHAR);
BEGIN
  IF errorMsg # NIL THEN
    DynString.Dispose(errorMsg);
  END;
  errorMsg := DynString.Create(msg);
  hasErr := TRUE;
END SetError;

PROCEDURE SetErrorString(msg: String);
BEGIN
  IF errorMsg # NIL THEN
    DynString.Dispose(errorMsg);
  END;
  errorMsg := DynString.Copy(msg);
  hasErr := TRUE;
END SetErrorString;

PROCEDURE HasError(): BOOLEAN;
BEGIN
  RETURN hasErr;
END HasError;

PROCEDURE GetError(): String;
BEGIN
  IF errorMsg # NIL THEN
    RETURN DynString.Copy(errorMsg);
  ELSE
    RETURN DynString.CreateFromArray("");
  END;
END GetError;

PROCEDURE GetErrorPtr(): DynString.CharArrayPtr;
BEGIN
  (* errorMsg should always be valid, never NIL *)
  RETURN DynString.GetDataPtr(errorMsg);
END GetErrorPtr;

PROCEDURE WriteErrorTo(cid: IOChan.ChanId);
BEGIN
  (* Write error message to channel without any copying *)
  DynString.WriteToChannel(errorMsg, cid);
END WriteErrorTo;

PROCEDURE ClearError();
BEGIN
  hasErr := FALSE;
  (* errorMsg should never be NIL *)
  DynString.Dispose(errorMsg);
  errorMsg := DynString.CreateFromArray("");
END ClearError;

(* Exception handling procedures *)
PROCEDURE HasException(): BOOLEAN;
BEGIN
  RETURN hasExc;
END HasException;

PROCEDURE GetException(): MalVal;
BEGIN
  RETURN exceptionVal;
END GetException;

PROCEDURE ClearException();
BEGIN
  hasExc := FALSE;
  exceptionVal := NIL;
END ClearException;

PROCEDURE SetException(val: MalVal);
BEGIN
  hasExc := TRUE;
  exceptionVal := val;
END SetException;

(* Tokenizer helpers *)

PROCEDURE IsWhitespace(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch = ' ') OR (ch = 11C) OR (ch = 12C) OR (ch = 15C);
END IsWhitespace;

PROCEDURE IsDigit(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch >= '0') AND (ch <= '9');
END IsDigit;

PROCEDURE IsSymbolChar(ch: CHAR): BOOLEAN;
BEGIN
  RETURN ((ch >= 'a') AND (ch <= 'z')) OR
         ((ch >= 'A') AND (ch <= 'Z')) OR
         ((ch >= '0') AND (ch <= '9')) OR
         (ch = '+') OR (ch = '-') OR (ch = '*') OR (ch = '/') OR
         (ch = '_') OR (ch = '!') OR (ch = '?') OR (ch = '<') OR
         (ch = '>') OR (ch = '=') OR (ch = '.') OR (ch = '&') OR
         (ch = ':');
END IsSymbolChar;

(* Tokenize input string *)

PROCEDURE Tokenize(VAR str: ARRAY OF CHAR);
VAR
  i, j, len: CARDINAL;
  ch: CHAR;
  inComment: BOOLEAN;
BEGIN
  tokenCount := 0;
  i := 0;
  len := Length(str);
  inComment := FALSE;

  WHILE (i < len) AND (tokenCount < MAX_TOKENS) DO
    ch := str[i];

    (* Handle comments *)
    IF ch = ';' THEN
      inComment := TRUE;
    END;

    IF (ch = 12C) OR (ch = 15C) THEN
      inComment := FALSE;
    END;

    IF inComment THEN
      INC(i);
    ELSIF IsWhitespace(ch) OR (ch = ',') THEN
      INC(i);
    ELSIF ch = '~' THEN
      (* Check for ~@ *)
      tokens[tokenCount][0] := ch;
      INC(i);
      IF (i < len) AND (str[i] = '@') THEN
        tokens[tokenCount][1] := '@';
        tokens[tokenCount][2] := 0C;
        INC(i);
      ELSE
        tokens[tokenCount][1] := 0C;
      END;
      INC(tokenCount);
    ELSIF (ch = '(') OR (ch = ')') OR (ch = '[') OR (ch = ']') OR
          (ch = '{') OR (ch = '}') OR (ch = 47C) OR (ch = '`') OR
          (ch = '^') OR (ch = '@') THEN
      (* Single character tokens *)
      tokens[tokenCount][0] := ch;
      tokens[tokenCount][1] := 0C;
      INC(tokenCount);
      INC(i);
    ELSIF ch = '"' THEN
      (* String token *)
      j := 0;
      tokens[tokenCount][j] := ch;
      INC(j);
      INC(i);
      WHILE (i < len) AND (str[i] # '"') DO
        IF str[i] = '\' THEN
          INC(i);
          IF i < len THEN
            IF str[i] = 'n' THEN
              tokens[tokenCount][j] := 12C;
            ELSIF str[i] = '\' THEN
              tokens[tokenCount][j] := '\';
            ELSIF str[i] = '"' THEN
              tokens[tokenCount][j] := '"';
            ELSE
              tokens[tokenCount][j] := str[i];
            END;
            INC(j);
            INC(i);
          END;
        ELSE
          tokens[tokenCount][j] := str[i];
          INC(j);
          INC(i);
        END;
      END;
      IF i < len THEN
        tokens[tokenCount][j] := '"';
        INC(j);
        INC(i);
        tokens[tokenCount][j] := 0C;
        INC(tokenCount);
      ELSE
        (* Unterminated string *)
        SetError("unexpected EOF (unbalanced)");
        RETURN;
      END;
    ELSIF ch = ':' THEN
      (* Keyword *)
      j := 0;
      tokens[tokenCount][j] := ch;
      INC(j);
      INC(i);
      WHILE (i < len) AND IsSymbolChar(str[i]) DO
        tokens[tokenCount][j] := str[i];
        INC(j);
        INC(i);
      END;
      tokens[tokenCount][j] := 0C;
      INC(tokenCount);
    ELSE
      (* Symbol or number *)
      j := 0;
      WHILE (i < len) AND IsSymbolChar(str[i]) DO
        tokens[tokenCount][j] := str[i];
        INC(j);
        INC(i);
      END;
      tokens[tokenCount][j] := 0C;
      INC(tokenCount);
    END;
  END;
END Tokenize;

(* Parser helpers *)

PROCEDURE Peek(): Token;
BEGIN
  IF position < tokenCount THEN
    RETURN tokens[position];
  ELSE
    RETURN "";
  END;
END Peek;

PROCEDURE Next(): Token;
VAR
  tok: Token;
BEGIN
  tok := Peek();
  IF position < tokenCount THEN
    INC(position);
  END;
  RETURN tok;
END Next;

(* Read an atom (number, symbol, string, etc.) *)
PROCEDURE ReadAtom(): MalVal;
VAR
  tok: Token;
  val: INTEGER;
  res: ConvResults;
BEGIN
  tok := Next();

  (* Check for special values *)
  IF tok[0] = 0C THEN
    SetError("unexpected EOF");
    RETURN NewNil();
  ELSIF (tok[0] = 'n') AND (tok[1] = 'i') AND (tok[2] = 'l') AND (tok[3] = 0C) THEN
    RETURN NewNil();
  ELSIF (tok[0] = 't') AND (tok[1] = 'r') AND (tok[2] = 'u') AND (tok[3] = 'e') AND (tok[4] = 0C) THEN
    RETURN NewTrue();
  ELSIF (tok[0] = 'f') AND (tok[1] = 'a') AND (tok[2] = 'l') AND (tok[3] = 's') AND (tok[4] = 'e') AND (tok[5] = 0C) THEN
    RETURN NewFalse();
  ELSIF tok[0] = '"' THEN
    (* String - remove quotes *)
    RETURN NewString(tok);
  ELSIF tok[0] = ':' THEN
    (* Keyword *)
    RETURN NewKeyword(tok);
  ELSIF IsDigit(tok[0]) OR ((tok[0] = '-') AND IsDigit(tok[1])) THEN
    (* Number *)
    StrToInt(tok, val, res);
    IF res = strAllRight THEN
      RETURN NewInteger(val);
    ELSE
      RETURN NewSymbol(tok);
    END;
  ELSE
    (* Symbol *)
    RETURN NewSymbol(tok);
  END;
END ReadAtom;

(* Read a form (list, vector, hashmap, or atom) - declared here, defined later *)
(* Helper to wrap a form with a symbol (for reader macros) *)
PROCEDURE WrapWithSymbol(symbolName: ARRAY OF CHAR; form: MalVal): MalVal;
VAR
  list: MalVal;
BEGIN
  list := NewList();
  ListAppend(list, NewSymbol(symbolName));
  ListAppend(list, form);
  RETURN list;
END WrapWithSymbol;

(* Read a list starting from '(' *)
PROCEDURE ReadList(): MalVal;
VAR
  list: MalVal;
  tok: Token;
BEGIN
  list := NewList();
  tok := Next(); (* consume '(' *)

  LOOP
    tok := Peek();
    IF tok[0] = 0C THEN
      SetError("unexpected EOF");
      RETURN list;
    ELSIF tok[0] = ')' THEN
      tok := Next(); (* consume ')' *)
      EXIT;
    ELSE
      ListAppend(list, ReadFormImpl());
      IF hasErr THEN
        RETURN list;
      END;
    END;
  END;

  RETURN list;
END ReadList;

(* Read a vector starting from '[' *)
PROCEDURE ReadVector(): MalVal;
VAR
  vec: MalVal;
  tok: Token;
BEGIN
  vec := NewVector();
  tok := Next(); (* consume '[' *)

  LOOP
    tok := Peek();
    IF tok[0] = 0C THEN
      SetError("unexpected EOF");
      RETURN vec;
    ELSIF tok[0] = ']' THEN
      tok := Next(); (* consume ']' *)
      EXIT;
    ELSE
      ListAppend(vec, ReadFormImpl());
      IF hasErr THEN
        RETURN vec;
      END;
    END;
  END;

  RETURN vec;
END ReadVector;

(* Read a hashmap starting from '{' *)
PROCEDURE ReadHashMap(): MalVal;
VAR
  hmap: MalVal;
  tok: Token;
  key, val: MalVal;
BEGIN
  hmap := NewHashMap();
  tok := Next(); (* consume '{' *)

  LOOP
    tok := Peek();
    IF tok[0] = 0C THEN
      SetError("unexpected EOF (unbalanced)");
      RETURN hmap;
    ELSIF tok[0] = '}' THEN
      tok := Next(); (* consume '}' *)
      EXIT;
    ELSE
      (* Read key *)
      key := ReadFormImpl();
      IF hasErr THEN
        RETURN hmap;
      END;

      (* Check if there's a value *)
      tok := Peek();
      IF (tok[0] = '}') OR (tok[0] = 0C) THEN
        SetError("hash-map requires even number of elements");
        RETURN hmap;
      END;

      (* Read value *)
      val := ReadFormImpl();
      IF hasErr THEN
        RETURN hmap;
      END;

      (* Insert key-value pair *)
      IF (key^.type = MAL_STRING) OR (key^.type = MAL_KEYWORD) THEN
        HashMapPutString(hmap, key^.strVal, val);
      ELSE
        SetError("hash-map keys must be strings or keywords");
        RETURN hmap;
      END;
    END;
  END;

  RETURN hmap;
END ReadHashMap;

PROCEDURE ReadFormImpl(): MalVal;
VAR
  tok: Token;
  form, form2, list: MalVal;
  symbol: ARRAY [0..20] OF CHAR;
BEGIN
  tok := Peek();

  (* Handle reader macros *)
  IF (tok[0] = 47C) AND (tok[1] = 0C) THEN
    (* ' -> (quote ...) *)
    tok := Next();
    form := ReadFormImpl();
    IF hasErr THEN RETURN NewList(); END;
    RETURN WrapWithSymbol("quote", form);

  ELSIF (tok[0] = '`') AND (tok[1] = 0C) THEN
    (* ` -> (quasiquote ...) *)
    tok := Next();
    form := ReadFormImpl();
    IF hasErr THEN RETURN NewList(); END;
    RETURN WrapWithSymbol("quasiquote", form);

  ELSIF (tok[0] = '~') AND (tok[1] = '@') THEN
    (* ~@ -> (splice-unquote ...) *)
    tok := Next();
    form := ReadFormImpl();
    IF hasErr THEN RETURN NewList(); END;
    RETURN WrapWithSymbol("splice-unquote", form);

  ELSIF (tok[0] = '~') AND (tok[1] = 0C) THEN
    (* ~ -> (unquote ...) *)
    tok := Next();
    form := ReadFormImpl();
    IF hasErr THEN RETURN NewList(); END;
    RETURN WrapWithSymbol("unquote", form);

  ELSIF (tok[0] = '@') AND (tok[1] = 0C) THEN
    (* @ -> (deref ...) *)
    tok := Next();
    form := ReadFormImpl();
    IF hasErr THEN RETURN NewList(); END;
    RETURN WrapWithSymbol("deref", form);

  ELSIF (tok[0] = '^') AND (tok[1] = 0C) THEN
    (* ^ -> (with-meta obj metadata) - special case: reads 2 forms in reverse order *)
    tok := Next();
    list := NewList();
    Assign("with-meta", symbol);
    ListAppend(list, NewSymbol(symbol));
    (* Read metadata first *)
    form := ReadFormImpl();
    IF hasErr THEN RETURN list; END;
    (* Read the object second *)
    form2 := ReadFormImpl();
    IF hasErr THEN RETURN list; END;
    (* Append in reverse order: object first, then metadata *)
    ListAppend(list, form2);
    ListAppend(list, form);
    RETURN list;
  END;

  (* Handle containers *)
  IF tok[0] = '(' THEN
    RETURN ReadList();
  ELSIF tok[0] = '[' THEN
    RETURN ReadVector();
  ELSIF tok[0] = '{' THEN
    RETURN ReadHashMap();
  ELSE
    RETURN ReadAtom();
  END;
END ReadFormImpl;

(* Main entry point *)
PROCEDURE ReadStr(VAR str: ARRAY OF CHAR): MalVal;
BEGIN
  Initialize();
  ClearError();
  Tokenize(str);

  (* Check if tokenization failed *)
  IF hasErr THEN
    RETURN NewNil();
  END;

  position := 0;

  IF tokenCount = 0 THEN
    RETURN NewNil();
  END;

  RETURN ReadFormImpl();
END ReadStr;

(* Module initialization block *)
BEGIN
  tokenCount := 0;
  position := 0;
  hasErr := FALSE;
  errorMsg := DynString.CreateFromArray("");
  hasExc := FALSE;
  exceptionVal := NIL;
  initialized := FALSE;
END Reader.
