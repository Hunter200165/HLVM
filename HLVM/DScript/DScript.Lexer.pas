unit DScript.Lexer;

interface

uses 
  System.SysUtils,
  Winapi.Windows,
  System.Generics.Collections;

type
  { Internal type for storing IDs }
  THId = Integer;
  THIds = array of THId;
  THPosition = record
  private
    FLineNumber: Integer;
    FRowNumber: Integer;
    FScopeName: String;
  public 
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property RowNumber: Integer read FRowNumber write FRowNumber;
    property ScopeName: String read FScopeName write FScopeName;
  end;
  THLexeme = class(TObject)
  private
    FContent: String;
    FState: Integer;
    FID: Integer;
    FPosition: THPosition;
    FData_IDs: THIds;
    FAdvancedSwitch: Boolean;
    FConstant: Boolean;
    FAsync: Boolean;
    FObjective: Boolean;
  public
    property Content: String read FContent write FContent;
    property State: Integer read FState write FState;
    property ID: Integer read FID write FID;
    property Position: THPosition read FPosition write FPosition;

    property Data_IDs: THIds read FData_IDs write FData_IDs;
    procedure Data_IDs_Add(const AnID: THId);

    property AdvancedSwitch: Boolean read FAdvancedSwitch write FAdvancedSwitch;
    property Constant: Boolean read FConstant write FConstant;

    property Async: Boolean read FAsync write FAsync;
    property Objective: Boolean read FObjective write FObjective;
  end;
  THLexemes = TObjectList<THLexeme>;

type 
  THBracket = class(THLexeme);
  THBrackets = TObjectList<THBracket>;

const 
  State_None        = 0;
  State_Token       = 1;
  State_Punctuation = 2;
  State_Spacing     = 3;
  State_BraceOpened = 4;
  State_BraceClosed = 5;
  State_String      = 6;
  State_Comment     = 7;
  State_Separation  = 8;

  State_KeyWord     = 20;
  State_Operator    = 21;
  
type EParsingException = class(Exception);
type 
  THLexer = class(TObject)
  private
    FSourceText: String;
    FLastID: Integer;
    FLexemes: THLexemes;
    FLastPosition: THPosition;
    FScopeName: String;
  public 
    property SourceText: String read FSourceText write FSourceText;
    property LastID: Integer read FLastID write FLastID;
    property LastPosition: THPosition read FLastPosition write FLastPosition;

    property ScopeName: String read FScopeName write FScopeName;

    property Lexemes: THLexemes read FLexemes write FLexemes;

    function Push(const ALexeme: THLexeme): THLexeme; inline;
    function PushNewLexeme(const Content: String = ''; const State: Integer = State_None): THLexeme; inline;
    function GetLastLexeme: THLexeme; inline;
    
    function GetPairingBracket(const ABracketChar: Char): Char; inline;
    function GetCharState(const Chr: Char): Integer; inline;

    function IsNewlineAt(const APos: Integer): Boolean; inline;
    function Sub(const Str: String; const AFrom, ATo: Integer): String;
    function CountNewlinesInString(const Str: String; var LastRow: Integer): Integer;

    procedure ParseForLexemes;

    function ParseString(const PositionToStart: Integer): Integer;
    function ParseMultilineString(const PositionToStart: Integer; var EqualsLength: Integer): Integer;
    function ParseComment(const PositionToStart: Integer): Integer;
    function ParseMultilineComment(const PositionToStart: Integer): Integer;

    function FormatAt(const Pos: THPosition): String;
  end;

const
  HPunctuationChars = ['%', '$', '&', '|', '!', '@', '*', '+', '-', '=', '~', '^', '?', '/', '#', '.', ':', '>', '<'];
  HSeparationChars  = [',', ';'];
  HSpacingChars     = [' ', #13, #10, #9];
  HBraceOpening     = ['(', '{', '['];
  HBraceClosing     = [')', '}', ']'];

const
  IsWindows = True;
  IsLinux   = True and not IsWindows;
  IsMAC     = True and not IsLinux and not IsWindows;

implementation

{ THLexer }

function THLexer.CountNewlinesInString(const Str: String; var LastRow: Integer): Integer;
var i: Integer;
begin
  Result := 0;
  LastRow := 1;
  for i := 1 to Str.Length do begin 
    if (IsWindows or IsLinux) and (Str[i] = #13) then begin
      Result := Result + 1;
      LastRow := 0;
    end
    else if IsMAC and (Str[i] = #10) then begin
      Result := Result + 1;
      LastRow := 0;
    end;
    if not (IsWindows and (Str[i] = #10)) then
      LastRow := LastRow + 1;
  end;
end;

function THLexer.FormatAt(const Pos: THPosition): String;
begin
  Result := Format('at ("%s", %d, %d)', [Pos.FScopeName, Pos.FLineNumber, Pos.FRowNumber]);
end;

function THLexer.GetCharState(const Chr: Char): Integer;
begin
  if IsCharAlphaNumeric(Chr) or (Chr = '_') then
    Result := State_Token
  else if CharInSet(Chr, HPunctuationChars) then 
    Result := State_Punctuation
  else if CharInSet(Chr, HSeparationChars) then
    Result := State_Separation
  else if CharInSet(Chr, HSpacingChars) then
    Result := State_Spacing
  else if CharInSet(Chr, HBraceOpening) then
    Result := State_BraceOpened
  else if CharInSet(Chr, HBraceClosing) then
    Result := State_BraceClosed
  else 
    raise EParsingException.Create('Cannot resolve char state: ' + Chr);
end;

function THLexer.GetLastLexeme: THLexeme;
begin
  Result := Lexemes[Lexemes.Count - 1];
end;

function THLexer.GetPairingBracket(const ABracketChar: Char): Char;
begin
  if (ABracketChar = '(') then
    Result := ')'
  else if (ABracketChar = ')') then
    Result := '('
  else if (ABracketChar = '{') then
    Result := '}'
  else if (ABracketChar = '}') then
    Result := '{'
  else if (ABracketChar = '[') then
    Result := ']'
  else if (ABracketChar = ']') then
    Result := '['
  else 
    raise EParsingException.Create('Char is not a bracket: ' + ABracketChar);
end;

function THLexer.IsNewlineAt(const APos: Integer): Boolean;
begin
  Result := False;
//  if IsWindows and ((SourceText.Length - APos + 1) < 1) then Exit;
  if ((SourceText.Length - APos + 1) < 1) then Exit;
  if IsLinux then
    Result := SourceText[APos] = #13
  else if IsMAC then
    Result := SourceText[APos] = #10
  else if IsWindows then
    { On Windows, noone can hear your scream... }
    Result := (SourceText[APos] = #13);
end;

function THLexer.ParseComment(const PositionToStart: Integer): Integer;
var Len, Pos: Integer;
begin
  Result := -1;
  Len := SourceText.Length;
  if (Len - PositionToStart + 1) < 2 then Exit;
  if not ((SourceText[PositionToStart] = '-') and (SourceText[PositionToStart + 1] = '-')) then
    Exit;    
  Pos := PositionToStart + 2;
  while (Pos <= Len) do begin
    if IsNewlineAt(Pos) then
      Break;
    Pos := Pos + 1;
  end;
  Result := Pos;
  if (Pos > Len) then
    Result := Result - 1;
end;

procedure THLexer.ParseForLexemes;
var Pos, Len, Status, LastState, CurrentRow, CurrentLine: Integer;
    CurrentChar: Char;
    Brackets: THBrackets;
    Bracket: THBracket;
//    LastLexeme: THLexeme;
    Position: THPosition;
    TempPos, Equals: Integer;
    TempStr: String;
begin
  Pos := 1;
  Len := Self.SourceText.Length;
  Self.Lexemes.Clear;

  LastID := 0;

//  LastLexeme := THLexeme.Create;
//  LastLexeme.State := State_None;
//  LastLexeme.Content := '';

  LastState := State_None;
  CurrentRow := 1;
  CurrentLine := 1;

  Position.LineNumber := CurrentLine;
  Position.RowNumber := CurrentRow;
  Position.ScopeName := ScopeName;

  Brackets := THBrackets.Create(True);
  try
    while (Pos <= Len) do begin
      CurrentChar := Self.SourceText[Pos];

      Self.LastPosition := Position;

      TempPos := ParseMultilineString(Pos, Equals);
      if TempPos > 0 then begin
        TempStr := Sub(SourceText, Pos, TempPos);
        LastState := State_String;

        CurrentLine := CurrentLine + CountNewlinesInString(TempStr, Equals);
        if not (CurrentLine = Position.LineNumber) then
          CurrentRow := Equals;

        PushNewLexeme(TempStr, LastState).Position := Position;

        Position.LineNumber := CurrentLine;
        Position.RowNumber := CurrentRow + 1;
        
        Pos := TempPos + 1;

        Continue;
      end;
      if CharInSet(CurrentChar, ['''', '"']) then begin 
        TempPos := ParseString(Pos);
        if TempPos > 0 then begin 
          TempStr := Sub(SourceText, Pos, TempPos);
          LastState := State_String;
          CurrentLine := CurrentLine + CountNewlinesInString(TempStr, Equals);
          if not (CurrentLine = Position.LineNumber) then
            CurrentRow := Equals;

          PushNewLexeme(TempStr, LastState).Position := Position;

          Position.LineNumber := CurrentLine;
          Position.RowNumber := CurrentRow + 1;

          Pos := TempPos + 1;
          Continue;
        end;
      end;
      TempPos := ParseMultilineComment(Pos);
      if TempPos > 0 then begin 
        TempStr := Sub(SourceText, Pos, TempPos);
        LastState := State_Comment;

        CurrentLine := CurrentLine + CountNewlinesInString(TempStr, Equals);
        if not (CurrentLine = Position.LineNumber) then
          CurrentRow := Equals;

        Position.LineNumber := CurrentLine;
        Position.RowNumber := CurrentRow + 1;

        PushNewLexeme(TempStr, LastState);
        
        Pos := TempPos + 1;

        Continue;
      end;
      TempPos := ParseComment(Pos);
      if TempPos > 0 then begin 
        TempStr := Sub(SourceText, Pos, TempPos);
        LastState := State_Comment;
        CurrentLine := CurrentLine + CountNewlinesInString(TempStr, Equals);
        if not (CurrentLine = Position.LineNumber) then
          CurrentRow := Equals;
        Position.LineNumber := CurrentLine;
        Position.RowNumber := CurrentRow + 1;

        PushNewLexeme(TempStr, LastState);

        Pos := TempPos + 1;
        Continue;
      end;

      Status := GetCharState(CurrentChar);

      if Status = State_BraceOpened then begin 
        { It will be State_BraceOpened }
        PushNewLexeme(CurrentChar, State_BraceOpened).Position := Position;
        Bracket := THBracket.Create;
      
        LastID := LastID + 1;
        Bracket.Content := CurrentChar;
        Bracket.ID := LastID;
        GetLastLexeme.ID := LastID;
      
        Brackets.Add(Bracket);
      end
      else if Status = State_BraceClosed then begin 
        if not (GetPairingBracket(Brackets[Brackets.Count - 1].Content[1]) = CurrentChar) then
          raise EParsingException.Create('Bracket mismatching: Expected "' + GetPairingBracket(Brackets[Brackets.Count - 1].Content[1]) +
            '" to close "' + Brackets[Brackets.Count - 1].Content + '" ' + FormatAt(Brackets[Brackets.Count - 1].Position) + 
            ', but got "' + CurrentChar + '" ' + FormatAt(Position));
        PushNewLexeme(CurrentChar, State_BraceClosed).Position := Position;
        GetLastLexeme.ID := Brackets[Brackets.Count - 1].ID;
        Brackets.Delete(Brackets.Count - 1);
      end
      else if Status = State_Separation then begin
        PushNewLexeme(CurrentChar, Status).Position := Position;
      end
      else if (Status = LastState) then begin
        GetLastLexeme.Content := GetLastLexeme.Content + CurrentChar;
      end
      else begin
        PushNewLexeme(CurrentChar, Status).Position := Position;
      end;

      if (Status = State_Spacing) and IsNewlineAt(Pos) then begin
        CurrentLine := CurrentLine + 1;
        Position.LineNumber := CurrentLine;
        CurrentRow := 0;
      end;

      LastState := Status;
      
      Pos := Pos + 1;
      CurrentRow := CurrentRow + 1;
      Position.RowNumber := CurrentRow;
    end;
  finally
    Brackets.Free;
  end;
end;

function THLexer.ParseMultilineComment(const PositionToStart: Integer): Integer;
var _: Integer;
begin
  Result := -1;
  if (SourceText.Length - PositionToStart + 1) < 2 then Exit;
  if not ((SourceText[PositionToStart] = '-') and (SourceText[PositionToStart + 1] = '-')) then Exit;
  try
    Result := ParseMultilineString(PositionToStart + 2, _);
  except
    on E: EParsingException do begin 
      raise EParsingException.Create('Multiline comment was not close ' + FormatAt(Self.LastPosition));
    end;
  end;
end;

function THLexer.ParseMultilineString(const PositionToStart: Integer; var EqualsLength: Integer): Integer;
var Pos, Len, EqualsCount, TempEqualsCount, LastPos: Integer;
begin
  Result := -1;
  if not (SourceText[PositionToStart] = '[') then Exit;
  Pos := PositionToStart + 1;
  Len := Self.SourceText.Length;
  EqualsCount := 0;
  while (Pos <= Len) do begin 
    if SourceText[Pos] = '=' then
      EqualsCount := EqualsCount + 1
    else if SourceText[Pos] = '[' then
      Break
    else 
      Exit;
    Pos := Pos + 1;
  end;
  if (Pos > Len) then Exit;
  while (Pos <= Len) do begin 
    if SourceText[Pos] = ']' then begin 
      Pos := Pos + 1;
      LastPos := Pos;
      TempEqualsCount := 0;
      while (Pos <= Len) do begin 
        if SourceText[Pos] = '=' then
          TempEqualsCount := TempEqualsCount + 1
        else 
          Break;
        Pos := Pos + 1;
      end;
      if (Pos > Len) then raise EParsingException.Create('Multiline string was not closed ' + FormatAt(LastPosition));
      if (TempEqualsCount = EqualsCount) and (SourceText[Pos] = ']') then begin
        EqualsLength := EqualsCount;
        Result := Pos;
        Exit;
      end
      else begin
        Pos := LastPos;
      end;
    end
    else begin
      Pos := Pos + 1;
    end;
  end;
  raise EParsingException.Create('Multiline string was not closed at ' + FormatAt(LastPosition));  
end;

function THLexer.ParseString(const PositionToStart: Integer): Integer;
var Pos, Len: Integer;
    CharInStart: Char;
begin
  Result := -1;
  Pos := PositionToStart;
  Len := SourceText.Length;
  if CharInSet(SourceText[Pos], ['''', '"']) then begin
    CharInStart := SourceText[Pos];
    Pos := Pos + 1;
    while (Pos <= Len) do begin
      if (SourceText[Pos] = '\') then
        Pos := Pos + 2
      else if (SourceText[Pos] = CharInStart) then
        Break
      else
        Pos := Pos + 1;
    end;
    if (Pos > Len) then
      raise EParsingException.Create('Singline string was not closed ' + FormatAt(LastPosition));
    Result := Pos;
  end;
end;

function THLexer.Push(const ALexeme: THLexeme): THLexeme;
begin
  Result := ALexeme;
  { No reason to add useless lexemes }
  if not (Result.State = State_None) then
    Lexemes.Add(ALexeme);
end;

function THLexer.PushNewLexeme(const Content: String; const State: Integer): THLexeme;
begin
  Result := THLexeme.Create;
  Result.Content := Content;
  Result.State := State;
  Self.Push(Result);
end;

function THLexer.Sub(const Str: String; const AFrom, ATo: Integer): String;
begin
  Result := Copy(Str, AFrom, ATo - AFrom + 1);
end;

{ THLexeme }

procedure THLexeme.Data_IDs_Add(const AnID: THId);
var Len: Integer;
begin
  Len := Length(Data_IDs);
  SetLength(FData_IDs, Len + 1);
  Data_IDs[Len] := AnID;
end;

end.


