unit HLVM.Static.Lexer.Miscs;

interface

{ EXPORTS: }
  function HLM_GetPairingBracket(const AChar: Char): Char;
  function HLM_GetTypeString(const AType: Integer): String;
  function HLM_EncloseString(const AString: String): String;

implementation

uses
  DScript.Lexer;

function HLM_GetPairingBracket(const AChar: Char): Char;
begin
  Result := ' ';
  case AChar of
    '(': Result := ')';
    ')': Result := '(';
    '{': Result := '}';
    '}': Result := '{';
    '[': Result := ']';
    ']': Result := '[';
  end;
end;

function HLM_GetTypeString(const AType: Integer): String;
begin
  case AType of
    State_None        : Result := 'None';
    State_Token       : Result := 'Identifier';
    State_Punctuation : Result := 'Operator';
    State_Spacing     : Result := 'Whitespace';
    State_BraceOpened : Result := 'Opening bracket';
    State_BraceClosed : Result := 'Closing bracket';
    State_String      : Result := 'String';
    State_Comment     : Result := 'Comment';
    State_Separation  : Result := 'Separation';
    State_KeyWord     : Result := 'Keyword Identifier';
  else
    Result := 'Unknown';
  end;
end;

function HLM_EncloseString(const AString: String): String;
var ALen, APos: Integer;
begin
  ALen := Length(AString);
  APos := 1;
  Result := '';
  while (APos <= ALen) do begin
    case AString[APos] of
      '''', '\': begin
        Result := Result + '\' + AString[APos];
      end;
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #9:  Result := Result + '\t';
      #8:  Result := Result + '\b';
      #11: Result := Result + '\v';
      #14: Result := Result + '\f';
      #7:  Result := Result + '\a';
      #0:  Result := Result + '\0';
    else
      Result := Result + AString[APos];
    end;
    APos := APos + 1;
  end;
  Result := '''' + Result + '''';
end;

end.
