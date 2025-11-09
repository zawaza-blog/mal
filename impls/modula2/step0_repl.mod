MODULE step0_repl;

FROM StdChans IMPORT StdInChan, StdOutChan;
FROM TextIO IMPORT WriteString, WriteLn, ReadString, SkipLine;
FROM IOChan IMPORT ReadResult, ChanId;
IMPORT IOResult;

VAR
  stdin, stdout: ChanId;

(* READ - Read a line from input *)
PROCEDURE READ(VAR line: ARRAY OF CHAR): BOOLEAN;
VAR
  result: IOResult.ReadResults;
BEGIN
  WriteString(stdout, "user> ");
  ReadString(stdin, line);
  result := ReadResult(stdin);

  (* Check for EOF or error *)
  IF result = IOResult.endOfInput THEN
    RETURN FALSE;
  END;

  (* Check if we got valid input *)
  IF (result # IOResult.allRight) AND (line[0] = 0C) THEN
    RETURN FALSE;
  END;

  (* Skip to end of line *)
  SkipLine(stdin);
  RETURN TRUE;
END READ;

(* EVAL - Evaluate the input (for step0, just return as-is) *)
PROCEDURE EVAL(VAR ast: ARRAY OF CHAR);
BEGIN
  (* For step0, EVAL does nothing - just passes through *)
END EVAL;

(* PRINT - Print the result *)
PROCEDURE PRINT(VAR exp: ARRAY OF CHAR);
BEGIN
  WriteString(stdout, exp);
  WriteLn(stdout);
END PRINT;

(* REP - Read-Eval-Print *)
PROCEDURE rep(): BOOLEAN;
VAR
  line: ARRAY [0..1023] OF CHAR;
BEGIN
  IF NOT READ(line) THEN
    RETURN FALSE;
  END;
  EVAL(line);
  PRINT(line);
  RETURN TRUE;
END rep;

(* Main loop *)
BEGIN
  stdin := StdInChan();
  stdout := StdOutChan();

  WHILE rep() DO
    (* Continue processing *)
  END;

  WriteLn(stdout);
END step0_repl.
