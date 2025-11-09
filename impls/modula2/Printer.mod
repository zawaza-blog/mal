IMPLEMENTATION MODULE Printer;

(* MAL Printer - Implementation Module *)

FROM Types IMPORT MalVal, MalType, MalList, IsNil, IsList, IsVector, HashMapKeys, HashMapGetString, NewKeyword;
FROM Strings IMPORT Assign, Concat, Length;
FROM WholeStr IMPORT IntToStr;
FROM DynString IMPORT String;
IMPORT DynString;
FROM IOChan IMPORT ChanId;

(* Print a MAL value to a DynString.String *)
PROCEDURE PrStr(val: MalVal; printReadably: BOOLEAN): String;
VAR
  result, temp: String;
  curr: MalList;
  first: BOOLEAN;
  keys: MalVal;
  valVal: MalVal;
  intStr: ARRAY [0..31] OF CHAR;
  i: CARDINAL;
BEGIN
  IF IsNil(val) THEN
    RETURN DynString.CreateFromArray("nil");
  END;

  CASE val^.type OF
    MAL_NIL:
      RETURN DynString.CreateFromArray("nil");

  | MAL_TRUE:
      RETURN DynString.CreateFromArray("true");

  | MAL_FALSE:
      RETURN DynString.CreateFromArray("false");

  | MAL_INTEGER:
      IntToStr(val^.intVal, intStr);
      (* Remove leading '+' sign if present *)
      IF (intStr[0] = '+') THEN
        (* Shift string left to remove '+' *)
        i := 0;
        WHILE intStr[i+1] # 0C DO
          intStr[i] := intStr[i+1];
          INC(i);
        END;
        intStr[i] := 0C;
      END;
      RETURN DynString.CreateFromArray(intStr);

  | MAL_SYMBOL:
      RETURN DynString.Copy(val^.strVal);

  | MAL_STRING:
      IF printReadably THEN
        (* Trim quotes, escape, add quotes back *)
        temp := DynString.TrimQuotes(val^.strVal);
        result := DynString.EscapeString(temp);
        (* DynString.Dispose(temp);  -- Free trimmed string *)
        temp := DynString.CreateFromArray('"');
        DynString.AppendString(temp, result);
        (* DynString.Dispose(result);  -- Free escaped string *)
        DynString.AppendChar(temp, '"');
        RETURN temp;
      ELSE
        (* Just return without quotes *)
        RETURN DynString.TrimQuotes(val^.strVal);
      END;

  | MAL_KEYWORD:
      RETURN DynString.Copy(val^.strVal);

  | MAL_LIST:
      result := DynString.CreateFromArray("(");
      curr := val^.listVal;
      first := TRUE;
      WHILE curr # NIL DO
        IF NOT first THEN
          DynString.AppendChar(result, ' ');
        END;
        first := FALSE;
        temp := PrStr(curr^.val, printReadably);
        DynString.AppendString(result, temp);
        (* DynString.Dispose(temp);  -- Free temp string *)
        curr := curr^.next;
      END;
      DynString.AppendChar(result, ')');
      RETURN result;

  | MAL_VECTOR:
      result := DynString.CreateFromArray("[");
      curr := val^.listVal;
      first := TRUE;
      WHILE curr # NIL DO
        IF NOT first THEN
          DynString.AppendChar(result, ' ');
        END;
        first := FALSE;
        temp := PrStr(curr^.val, printReadably);
        DynString.AppendString(result, temp);
        (* DynString.Dispose(temp);  -- Free temp string *)
        curr := curr^.next;
      END;
      DynString.AppendChar(result, ']');
      RETURN result;

  | MAL_HASHMAP:
      result := DynString.CreateFromArray("{");
      keys := HashMapKeys(val);
      curr := keys^.listVal;
      first := TRUE;
      WHILE curr # NIL DO
        IF NOT first THEN
          DynString.AppendChar(result, ' ');
        END;
        first := FALSE;
        (* Print key *)
        temp := PrStr(curr^.val, printReadably);
        DynString.AppendString(result, temp);
        (* DynString.Dispose(temp);  -- Free temp string *)
        DynString.AppendChar(result, ' ');
        (* Print value *)
        valVal := HashMapGetString(val, curr^.val^.strVal);
        temp := PrStr(valVal, printReadably);
        DynString.AppendString(result, temp);
        (* DynString.Dispose(temp);  -- Free temp string *)
        curr := curr^.next;
      END;
      DynString.AppendChar(result, '}');
      RETURN result;

  | MAL_NATIVE_FN:
      RETURN DynString.CreateFromArray("#<function>");

  | MAL_FUNCTION:
      RETURN DynString.CreateFromArray("#<function>");

  | MAL_ATOM:
      result := DynString.CreateFromArray("(atom ");
      temp := PrStr(val^.atomVal, printReadably);
      DynString.AppendString(result, temp);
      (* DynString.Dispose(temp);  -- Free temp string *)
      DynString.AppendChar(result, ')');
      RETURN result;

  ELSE
    RETURN DynString.CreateFromArray("<unknown>");
  END;
END PrStr;

(* Print a MAL value directly to a channel - zero-copy *)
PROCEDURE PrStrToChannel(val: MalVal; printReadably: BOOLEAN; cid: ChanId);
VAR
  str: String;
BEGIN
  str := PrStr(val, printReadably);
  DynString.WriteToChannel(str, cid);
END PrStrToChannel;

END Printer.
