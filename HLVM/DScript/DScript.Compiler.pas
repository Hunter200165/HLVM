unit DScript.Compiler;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Generics.Collections,
  HLVM.Types.Utilities,
  HLVM.Static.Command,
  HLVM.Types.Ordinal,
  HLVM.Static.Lexer.Miscs,
  DScript.Lexer;

type
  ECompilationException = class(Exception);

type
  THCompiler = class(TObject)
  private
    FLexemes: THLexemes;
    FOutputStream: TStream;
    FLabelIndex: Integer;
    FLexerObject: THLexer;
    FTotalWrittenCommand: Integer;
    FLastWrittenCommand: HCommand;
    FFormatSettings: TFormatSettings;
    function GetHighestLexeme: Integer;
  public
    property FormatSettings: TFormatSettings read FFormatSettings write FFormatSettings;

    property Lexemes: THLexemes read FLexemes write FLexemes;
    property OutputStream: TStream read FOutputStream write FOutputStream;

    property HighestLexeme: Integer read GetHighestLexeme;
    property LabelIndex: Integer read FLabelIndex write FLabelIndex;

    property LexerObject: THLexer read FLexerObject write FLexerObject;

    property TotalWrittenCommand: Integer read FTotalWrittenCommand write FTotalWrittenCommand;
    property LastWrittenCommand: HCommand read FLastWrittenCommand write FLastWrittenCommand;

    procedure WriteCompiledCommand(const Cmd: HCommand; const Pos: THPosition);
    procedure WriteInteger(const AValue: HInteger);
    procedure WriteByte(const AValue: Byte);
    procedure WriteString(const Str: String);
    procedure WritePosition(const APos: THPosition);
    procedure WriteFileHeader(const APos: THPosition);
    procedure WriteIntegerValue(const AValue: HSInteger);
    procedure WriteRealValue(const AValue: HSFloat);

    procedure AssignNormalisedLexemes(const Lxms: THLexemes);

    function ExpectStateUp_Raw(PosToStart: Integer; Args: Array of Const): Integer;

    function ScanForToken_Up(PosToStart: Integer): Integer;
    function ScanForToken_Down(PosToStart: Integer; const OperatorsPriority: Integer = -1; const UpTo: Integer = -1): Integer;

    function AnalyseTokensFor(const AContent: String; const AFrom, ATo: Integer): Integer; overload;
    function AnalyseTokensFor(const AContents: Array of String; const AFrom, ATo: Integer; var AChosenString: String): Integer; overload;

    function GetStatementClosure(const AFrom, ATo: Integer; var AChosenString: String): Integer;

    function IsOpeningClosureWord(const AWord: String): Boolean;

    function ResolveScopeClosure(const PosToStart: Integer): Integer;

    function IsTokenConstant(const AToken: THLexeme): Boolean;
    function IsTokenNumberic(const AToken: THLexeme): Boolean;
    
    function IsTokenReserved(const ATokenContent: String): Boolean;
    
    function GetMidOperatorPriority(const Op: String): Integer;

    function IsPrefixOperator(const AContext: String): Integer;
    function IsPostfixOperator(const AContext: String): Integer;

    function GetPairingBracketPosition(const PosToStart, BracketID: Integer): Integer;
    function GetPairingBracketPositionInversed(const PosToStart, BracketID: Integer): Integer;

    procedure CompileExpression(PosToStart, UpTo: Integer; Complex: Boolean);
    procedure CompileComplexExpressions(PosToStart, UpTo: Integer; const AComplex: Boolean);
    procedure CompileSimplifiedExpression(PosToStart, UpTo: Integer; Complex: Boolean; ANewIndexed: Boolean = False);
    procedure CompilePrefixOperators(PosToStart, UpTo: Integer; Complex: Boolean);
    procedure CompilePostfixOperators(PosToStart, UpTo: Integer; Complex: Boolean);

    procedure CompileNonConstantSimplifiedExpression(PosToStart, UpTo: Integer; Complex: Boolean; ANewIndexed: Boolean = False);
    
    procedure CompileTableExpression(PosToStart, UpTo: Integer);

    function CompileIfStatement(PosToStart: Integer): Integer;
    function CompileWhileStatement(PosToStart: Integer): Integer;
    function CompileForIteratorStatement(PosToStart: Integer): Integer;
    function CompileFunctionDeclaration(PosToStart: Integer): Integer;
    function CompileAnonymousFunctionDeclaration(PosToStart: Integer; const SkipName: Boolean = False): Integer;
    function CompileReturnStatement(PosToStart: Integer): Integer;
    function CompileTryStatement(PosToStart: Integer): Integer;

    procedure CompileOperator(const AOperator: THLexeme);
    procedure CompilePrefixOperator(const AOperator: THLexeme);
    procedure CompilePostfixOperator(const AOperator: THLexeme);
    procedure CompilePostfixOperatorAfter(const AOperator: THLexeme; const Complex: Boolean);
    procedure CompileMiddleOperator(AOperator: THLexeme);

    procedure CompilePush(const AValue: THLexeme; const Complex: Boolean = False);

    function CompileStatement(PosToStart: Integer): Integer;
    function CompileBlockOfStatements(PosToStart: Integer): Integer;
    procedure CompileChunk;

    function CompileAssigning(PosToStart: Integer): Integer;

    function GetRawStringContent(const ALanguageStr: String; const Pos: Integer): String;

    function GetTypeString(const AType: Integer): String;
    function IsValidIdentifier(const AIdentifier: THLexeme): Boolean;

    procedure FixNumbers;
    procedure FixLexemes;
    function TokenIsNumeric(const AToken: String): Boolean;

    function FormatAtPosition(const APos: THPosition): String;
    procedure RaiseCompilationException(const Msg: String; const APos: Integer);

    procedure Compile(const AText: String);
    
    constructor Create;
    destructor Destroy; override;
  end;

const
  PrefixOperators: array of String = ['!', 'not', 'bnot', '-', '~', '$', '#', 'async', 'await', 'const', 'native'];
  PostfixOperators: array of String = ['!', '?'];

  OpeningWords: array of String = [ 'begin', 'try', 'class' ];

  EndOfStatement: array of String = [ ';', 'end', 'finally', 'else', 'except' ];

  ReservedWords: array of String = [ 'if', 'then', 'else', 'do', 'begin', 'end', 'while', 'step', 'for', 'with', 'to', 'downto',
                                     'async', 'await', 'is', 'as', 'div', 'updiv', 'mod', 'and', 'band', 'or', 'bor', 'xor', 'bxor',
                                     'not', 'bnot', 'shl', 'shr', 'repeat', 'until', 'whilst' ];

const
  MidOperators: Array of String = [
    'in',

    'is',
    'is_not',
    'as',

    'and',
    '&&',

    'or',
    '||',

    'xor',

    'band',
    '&',

    'bor',
    '|',

    'bxor',
    '~^',

    'shl',
    'shr',
    '<<',
    '>>',

    '**',
    '^',

    '*',

    '%',
    'mod',

    '//',
    'div',

    '/^',
    'updiv',

    '/',
    '+',
    '-',

    '>=',
    '<=',
    '>',
    '<',
    
    '=',
    '!=',
    '~='
  ];

implementation

{ THCompiler }

(*
  Expressions are valued;
  Statements are actionised;

  Statements are:
    1) Assignement;
    2) Scopes:
      2.1: functions;
      2.2: if .. else blocks;
      2.3: for blocks;
      2.4: while blocks;
      2.5: repeat until/whilst blocks;
      2.6: classes;
      2.7: try .. except/finally .. end; blocks
*)

function THCompiler.AnalyseTokensFor(const AContent: String; const AFrom, ATo: Integer): Integer;
var i: Integer;
begin
  Result := -1;

  i := AFrom;
  while i <= ATo do begin
    if (Lexemes[i].Content = AContent) then begin
      Result := i;
      Break;
    end;
    if (Lexemes[i].State = State_BraceOpened) then begin
      i := GetPairingBracketPosition(i + 1, Lexemes[i].ID);
      if i > ATo then RaiseCompilationException('Brackets are violating general expression purpose', i);
    end
    else if IsOpeningClosureWord(Lexemes[i].Content) then begin
      i := ResolveScopeClosure(i);
      if i > ATo then RaiseCompilationException('Scope is violating general expression purpose', i);
    end
    else
      i := i + 1;
  end;
end;

function THCompiler.AnalyseTokensFor(const AContents: array of String; const AFrom, ATo: Integer; var AChosenString: String): Integer;
var i, k, Len: Integer;
begin
  Result := -1;
  Len := Length(AContents) - 1;
//  for i := AFrom to ATo do begin
  i := AFrom;
  while i <= ATo do begin
    for k := 0 to Len do begin
      if (Lexemes[i].Content = AContents[k]) then begin
        Result := i;
        AChosenString := AContents[k];
        Exit;
      end;
    end;
    if (Lexemes[i].State = State_BraceOpened) then begin
      i := GetPairingBracketPosition(i + 1, Lexemes[i].ID);
      if i > ATo then RaiseCompilationException('Brackets are violating general expression purpose', i);
    end
    else if IsOpeningClosureWord(Lexemes[i].Content) then begin
      i := ResolveScopeClosure(i);
      if i > ATo then RaiseCompilationException('Scope is violating general expression purpose', i);
      i := i + 1;
    end
    else if (Lexemes[i].Content = 'function') then begin
      { Parse function here }
      i := i + 1;
      while (i <= ATo) and not (Lexemes[i].Content = 'begin') do begin
        i := i + 1;
      end;
      if i > ATo then
        RaiseCompilationException('Anonymous function declaration is violating general expression purpose', i);
    end
    else
      i := i + 1;
  end;
//  end;
end;

procedure THCompiler.AssignNormalisedLexemes(const Lxms: THLexemes);
var i: Integer;
begin
  Self.Lexemes.Clear;
  for i := 0 to Lxms.Count - 1 do begin
    if not (Lxms[i].State in [State_None, State_Spacing, State_Comment]) then Self.Lexemes.Add(Lxms[i]);
  end;
end;

procedure THCompiler.Compile(const AText: String);
begin
  if (OutputStream = nil) then
    raise ECompilationException.Create('Output stream is not assigned!');
  if AText.IsEmpty then Exit;
end;

function THCompiler.CompileAnonymousFunctionDeclaration(PosToStart: Integer; const SkipName: Boolean): Integer;

type
  TTempVar = record
  public
    Name: String;
    Variable: Boolean;
  end;
  TTempVars = TList<TTempVar>;

var ArgPos, ArgEndPos, Pos, i: Integer;
    Vars: TTempVars;
    TempVar: TTempVar;
    SingleStatement: Boolean;
begin
  if not (Lexemes[PosToStart].Content = 'function') then
    RaiseCompilationException('Internal compiler error: function definition is not starting with `function` keyword', PosToStart);

  if SkipName then
    ArgPos := AnalyseTokensFor('(', PosToStart + 1, GetHighestLexeme)
  else
    ArgPos := PosToStart + 1;

  if ArgPos < 0 then
    RaiseCompilationException('Expected arguments declaration for function', PosToStart);
  if not (Lexemes[ArgPos].Content = '(') then
    RaiseCompilationException('Expected `(`, but given `' + Lexemes[ArgPos].Content + '`', ArgPos);

  ArgEndPos := GetPairingBracketPosition(ArgPos + 1, Lexemes[ArgPos].ID);
  if ArgEndPos < 0 then
    RaiseCompilationException('Unable to resolve arguments declaration', ArgPos);

  Vars := TTempVars.Create;
  try

    Pos := ArgPos + 1;
    while Pos < ArgEndPos do begin

      if Lexemes[Pos].Content = 'var' then begin

        { Huh? Pointers? C-Likety? NO! }
        { DScript is the first scripting language that can pass parameters by referencing them! }

        if not (Lexemes[Pos + 1].State = State_Token) then
          RaiseCompilationException('Expected variable identifier, but got ' + GetTypeString(Lexemes[Pos].State), Pos);
        if not IsValidIdentifier(Lexemes[Pos + 1]) then
          RaiseCompilationException('Invalid variable identifier', Pos);

        TempVar.Name := Lexemes[Pos + 1].Content;
        TempVar.Variable := True;

        Pos := Pos + 1;

      end
      else if Lexemes[Pos].Content = '...' then begin

        if not ((Pos + 1) = ArgEndPos) then
          RaiseCompilationException('VarArgs can only be the last argument of function', Pos);

        TempVar.Name := Lexemes[Pos].Content;
        TempVar.Variable := False;

      end
      else begin

        if not (Lexemes[Pos].State = State_Token) then
          RaiseCompilationException('Expected variable identifier, but got ' + GetTypeString(Lexemes[Pos].State), Pos);
        if not IsValidIdentifier(Lexemes[Pos]) then
          RaiseCompilationException('Invalid variable identifier', Pos);

        TempVar.Name := Lexemes[Pos].Content;
        TempVar.Variable := False;

      end;

      Pos := Pos + 1;
      if Pos < ArgEndPos then begin
        if not (Lexemes[Pos].Content = ',') then
          RaiseCompilationException('Expected `,` to separate arguments', Pos);
        Pos := Pos + 1;
      end;

      Vars.Add(TempVar);

      if TempVar.Name = '...' then
        Break;

    end;

    SingleStatement := False;

    Pos := ArgEndPos + 1;
    if SkipName then begin
      { It is not anonymous }
      if (Lexemes[Pos].Content = '=>') then begin
        SingleStatement := True;
      end
      else if not (Lexemes[Pos].Content = ';') then
        RaiseCompilationException('Expected `;` to end named function arguments definition', PosToStart);
      Pos := Pos + 1;
    end
    else if (Lexemes[Pos].Content = '=>') then begin
      { Anonymous function }
      SingleStatement := True;
      Pos := Pos + 1;
    end;

    WriteCompiledCommand(Command_CreateFunction, Lexemes[PosToStart].Position);
    WriteCompiledCommand(Command_OpenScope, Lexemes[PosToStart].Position);

      for i := 0 to Vars.Count - 1 do begin
        if not (Vars[i].Name = '...') then begin
          if not Vars[i].Variable then begin
            WriteCompiledCommand(Command_PushLocal, Lexemes[PosToStart].Position);
            WriteString(Vars[i].Name);
            WriteCompiledCommand(Command_Assign, Lexemes[PosToStart].Position)
          end
          else begin
            WriteCompiledCommand(Command_AssignLocalReference, Lexemes[PosToStart].Position);
            WriteString(Vars[i].Name);
          end;
        end
        else begin
          WriteCompiledCommand(Command_ConsumeArguments, Lexemes[PosToStart].Position);
          Break;
        end;
      end;
      WriteCompiledCommand(Command_ClearStack, Lexemes[PosToStart].Position);

      if SingleStatement and (Lexemes[Pos].Content = 'begin') then
        RaiseCompilationException('Unexpected `begin` after `=>`', Pos)
      else if not SingleStatement and not (Lexemes[Pos].Content = 'begin') then
        RaiseCompilationException('Expected `begin` after function declaration', Pos);

      Result := CompileBlockOfStatements(Pos);

    WriteCompiledCommand(Command_CloseScope, Lexemes[Result].Position);

  finally
    Vars.Free;
  end;
end;

function THCompiler.CompileAssigning(PosToStart: Integer): Integer;
var ALocal, AAssign: Boolean;
    Pos, Temp, LastPos, i, UpTo: Integer;
    TempStr: String;
    Vars: THCVariables;
    CurrentLxm: THLexeme;
begin
  { Assign can end with `;` or `end` instructions }
  //Temp := AnalyseTokensFor([';', 'end', 'else'], PosToStart, GetHighestLexeme, TempStr);
  Temp := GetStatementClosure(PosToStart, GetHighestLexeme, TempStr);
  if (Temp < 0) then Temp := GetHighestLexeme + 1;

  { Up to where we will scan? }
  UpTo := Temp - 1;

  Result := UpTo + 1;

  Pos := PosToStart;
  AAssign := False;
  
  { If it is local? }
  ALocal := Lexemes[PosToStart].Content = 'local';
  if ALocal then Pos := Pos + 1;

  LastPos := Pos;
  Vars.Size := 0;
  while (Pos <= UpTo) do begin
    CurrentLxm := Lexemes[Pos];
    if (CurrentLxm.Content = ',') then begin
      { Expect new variable here }
      if (Pos - LastPos) < 1 then RaiseCompilationException('Expected variable name', Pos);
      { Protect locals. }
      if ALocal and ((Pos - LastPos) > 1) then RaiseCompilationException('Local variables can be only single identifier', LastPos);
      if ALocal and not IsValidIdentifier(Lexemes[LastPos]) then RaiseCompilationException('Invalid local identifier', LastPos);
      { Add new variable }
      Vars.Add(THCVariable.Create(LastPos, Pos - 1));
      LastPos := Pos + 1;
    end
    else if (CurrentLxm.Content = ':=') then begin 
      { Assign with values; }
      AAssign := True;
      Break;
    end;
    Pos := Pos + 1;
  end;

  if ALocal and ((Pos - LastPos) > 1) then
    RaiseCompilationException('Local variables can be only single identifier', LastPos);
  if (Pos - LastPos) < 1 then RaiseCompilationException('Expected variable name', LastPos);
  Vars.Add(THCVariable.Create(LastPos, Pos - 1));

  WriteCompiledCommand(Command_MoveStackPointer, Lexemes[PosToStart].Position);
  if AAssign then begin 
    { Compile difficult expression. }
    CompileComplexExpressions(Pos + 1, UpTo, False);
  end;
  
  for i := 0 to Vars.Size - 1 do begin
    if ALocal then begin 
      WriteCompiledCommand(Command_PushLocal, Lexemes[Vars[i].StartPosition].Position);
      { Write single identifier }
      WriteString(Lexemes[Vars[i].StartPosition].Content);
    end
    else begin
      { Complex = !Constant }
      CompileNonConstantSimplifiedExpression(Vars[i].StartPosition, Vars[i].FinishPosition, False, True);

    end;
    { New index will take value itself. }
    if not (LastWrittenCommand = Command_NewIndex) then
      WriteCompiledCommand(Command_Assign, Lexemes[Vars[i].StartPosition].Position);
  end;

  WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[UpTo].Position);
end;

function THCompiler.CompileBlockOfStatements(PosToStart: Integer): Integer;
var Pos: Integer;
begin
  { We are starting with `begin` }
  { Otherwise we try to compile single statement }
  if Lexemes[PosToStart].Content = 'begin' then begin
    { Real block }
    { Result is end of it }
    Result := ResolveScopeClosure(PosToStart);
    Pos := PosToStart + 1;
    while (Pos < Result) do
      Pos := CompileStatement(Pos) + 1;
    if (Pos > Result) then
      { It cannot be truth in way all is done normally. }
      RaiseCompilationException('Internal compiler error: Statements block is violated by compiling single statement', Pos);
  end
  else begin
    { Single statement }
    Result := CompileStatement(PosToStart);
  end;
end;

procedure THCompiler.CompileChunk;
var Pos, Highest: Integer;
begin
  { We are not expecting `begin` there }
  Highest := GetHighestLexeme + 1;
  Pos := 0;
  while (Pos < Highest) do
    Pos := CompileStatement(Pos) + 1;
  if (Pos > Highest) then
    RaiseCompilationException('Chunk bounds error', Highest - 1);
end;

procedure THCompiler.CompileComplexExpressions(PosToStart, UpTo: Integer; const AComplex: Boolean);
var Pos, Temp, LastPos: Integer;
    CurrentLxm: THLexeme;
begin
  if (PosToStart > UpTo) then Exit;
  Pos := UpTo;
  LastPos := UpTo;
  while (Pos >= PosToStart) do begin
    CurrentLxm := Lexemes[Pos];
    case CurrentLxm.State of
      State_BraceClosed: begin
        Temp := GetPairingBracketPositionInversed(Pos - 1, CurrentLxm.ID);
        if (Temp < 0) or (Temp < PosToStart) then
          RaiseCompilationException('Brackets are not in valid position', Pos);
        Pos := Temp - 1;
      end;
      State_Separation: begin
        if not (CurrentLxm.Content = ',') then
          RaiseCompilationException('Expected ";" to separate expressions', Pos);
        if (LastPos <= Pos) then
          RaiseCompilationException('Empty expressions are not allowed', Pos);
        CompileExpression(Pos + 1, LastPos, AComplex or not (LastPos = UpTo));
        Pos := Pos - 1;
        LastPos := Pos;
      end;
    else
      Pos := Pos - 1;
    end;
  end;
  Pos := PosToStart;
  if (LastPos < Pos) then
    RaiseCompilationException('Empty expressions are not allowed', Pos);
  CompileExpression(Pos, LastPos, AComplex or not (LastPos = UpTo));
end;

procedure THCompiler.CompileExpression(PosToStart, UpTo: Integer; Complex: Boolean);
var CurrentLxm: THLexeme;
    Pos, LastPriority, Temp, TempPos {, Count } {, LastPos } {, TempNext} {, Len} {, LastState}: Integer;
begin
  { It is expression compiler, that can compile small expressions in recursive descending order }
  { For example: (self.GetCounter() + 23) * 2 : is complex expression, that is splitted in smaller ones }
  if PosToStart > UpTo then
    Exit;

  Pos := PosToStart;
  while Pos < UpTo do begin

    if Lexemes[Pos].State = State_BraceOpened then begin
      Pos := GetPairingBracketPosition(Pos + 1, Lexemes[Pos].ID);
      if Pos >= UpTo then
        Break;
    end;

    if Lexemes[Pos].Content = 'when' then begin
      // Ternary operator

      Temp := Pos + 1;
      LastPriority := 1;
      while (Temp < UpTo) and (LastPriority > 0) do begin 
        if Lexemes[Temp].Content = 'when' then
          LastPriority := LastPriority + 1
        else if Lexemes[Temp].Content = 'otherwise' then
          LastPriority := LastPriority - 1;

        if LastPriority = 0 then
          Break;
          
        if Lexemes[Temp].State = State_BraceOpened then
          Temp := GetPairingBracketPosition(Temp + 1, Lexemes[Temp].ID);
        Temp := Temp + 1;
      end;
      if LastPriority > 0 then
        RaiseCompilationException('`when` keyword pends to be closed with `otherwise`', Pos);

        
      { CONDITION }
      
      { REST IF TRUE }
      LastPriority := Pos - 1;
      while LastPriority > PosToStart do begin 
        if (Lexemes[LastPriority].Content = 'when') or (Lexemes[LastPriority].Content = 'otherwise') then begin 
          LastPriority := PosToStart;
          Break;
        end;
        if GetMidOperatorPriority(Lexemes[LastPriority].Content) >= 0 then begin
          { Stopping here }
          Break;
        end;
        if Lexemes[LastPriority].State = State_BraceClosed then
          LastPriority := GetPairingBracketPositionInversed(LastPriority - 1, Lexemes[LastPriority].ID);
        LastPriority := LastPriority - 1;
      end;

      if LastPriority <= PosToStart then begin    
        { Condition }
        CompileExpression(Pos + 1, Temp - 1, True);
        { Rest }
        CompileExpression(PosToStart, Pos - 1, True);
      end
      else begin
        { Part of expression } 
        CompileExpression(PosToStart, LastPriority - 1, True);
        CompileMiddleOperator(Lexemes[LastPriority]);
        { Condition }
        CompileExpression(Pos + 1, Temp - 1, True);
        { Rest }
        CompileExpression(LastPriority + 1, Pos - 1, True);
      end;
      { REST IF FALSE }
      TempPos := Temp + 1;
      while TempPos < UpTo do begin 
        if (Lexemes[TempPos].Content = 'when') or (Lexemes[TempPos].Content = 'otherwise') then begin 
          TempPos := UpTo;
          Break;
        end;
        if GetMidOperatorPriority(Lexemes[TempPos].Content) >= 0 then
          Break;
        if Lexemes[TempPos].State = State_BraceOpened then
          TempPos := GetPairingBracketPosition(TempPos, Lexemes[TempPos].ID);
        TempPos := TempPos + 1;
      end;
      
      if TempPos >= UpTo then      
        CompileExpression(Temp + 1, UpTo, True)
      else 
        CompileExpression(Temp + 1, TempPos - 1, True);
      WriteCompiledCommand(Command_Ternary, Lexemes[Pos].Position);

      if LastPriority > PosToStart then
        CompileOperator(Lexemes[LastPriority]);
      if TempPos < UpTo then begin 
        CompileMiddleOperator(Lexemes[TempPos]);
        CompileExpression(TempPos + 1, UpTo, True);
        CompileOperator(Lexemes[TempPos]);
      end;

      Exit;
    end;
    Pos := Pos + 1;
  end;

  Pos := PosToStart;
  LastPriority := -1;
//  Len := Lexemes.Count;
  Temp := -1;
  if UpTo > PosToStart then
    Temp := ScanForToken_Down(Pos, LastPriority, UpTo);
  if (Temp >= 0) and (Temp < UpTo) then begin
    { This is complicated expression }
//    Complex := True;
    CompileExpression(Pos, Temp, True);
    Pos := Temp + 1;
    while (Pos <= UpTo - 1) do begin
      { Expect an operator }
      CurrentLxm := Lexemes[Pos];
      LastPriority := GetMidOperatorPriority(CurrentLxm.Content);
      if LastPriority < 0 then
        RaiseCompilationException('Expected <Operator> but got <' + GetTypeString(CurrentLxm.State) + '>', Pos);
      { It will compile middle stage of some operators, for example logical `and`/`or` }
      CompileMiddleOperator(CurrentLxm);
      Temp := ScanForToken_Down(Pos + 1, LastPriority, UpTo);
      CompileExpression(Pos + 1, Temp, True);
      CompileOperator(CurrentLxm);
      Pos := Temp + 1;
    end;
  end
  else begin
    { Actual compilation }
    CompilePrefixOperators(PosToStart, UpTo, Complex);
  end;
end;

function THCompiler.CompileForIteratorStatement(PosToStart: Integer): Integer;
var {Pos, }DoPos, ToPos, EqPos, WithPos, Step, BreakPos, ContPos: Integer;
    TempStr, Variable{, StepStr}: String;
begin
  { It is iterative loop }

  if not (Lexemes[PosToStart].Content = 'for') then
    RaiseCompilationException('Internal compiler error: `for` loop does not start with `for` keyword', PosToStart);

  DoPos := AnalyseTokensFor('do', PosToStart, GetHighestLexeme);
  if (DoPos < 0) then
    RaiseCompilationException('`for` loop must be followed by `do` keyword', PosToStart);

  WithPos := AnalyseTokensFor('with', PosToStart, DoPos - 1);
  EqPos := AnalyseTokensFor(':=', PosToStart, DoPos);
  if (EqPos < 0) then
    RaiseCompilationException('Iterative `for` loop must have an assignment operator', PosToStart);

  ToPos := AnalyseTokensFor(['to', 'downto'], EqPos + 1, DoPos - 1, TempStr);
  if (ToPos < 0) then
    RaiseCompilationException('Iterative `for` loop must have `to`/`downto` keyword', PosToStart);

  if (WithPos > 0) and (WithPos <= ToPos) then 
    RaiseCompilationException('`with` can be placed only after high-bound expression', WithPos);
  if ((EqPos - 1) - (PosToStart + 1)) > 0 then
    RaiseCompilationException('Iterative `for` loop variable can be only single identifier', PosToStart + 1);

  Variable := Lexemes[PosToStart + 1].Content;
  if not IsValidIdentifier(Lexemes[PosToStart + 1]) then
    RaiseCompilationException('Invalid identifier in iterative `for` loop', PosToStart + 1);

  if (WithPos > 0) then begin 
  
    if not (Lexemes[WithPos + 1].Content = 'step') then
      RaiseCompilationException('Expected `step` keyword after `with`', WithPos + 1);
      
//    if not (Lexemes[WithPos + 2].Content = 'of') then
//      RaiseCompilationException('Expected `of` keyword after `step`', WithPos + 2);

    if not (((DoPos - 1) - (WithPos + 2)) = 0) then
      RaiseCompilationException('Step must be one and only integer number', WithPos + 2);

    if not TokenIsNumeric(Lexemes[WithPos + 2].Content) then
      RaiseCompilationException('Step must be integer number', WithPos + 3);

    if not TryStrToInt(Lexemes[WithPos + 2].Content, Step) then
      RaiseCompilationException('Could not convert step value; It is probably compiler''s fault', WithPos + 3);

    if (Step = 0) then 
      RaiseCompilationException('Step must be non-zero', WithPos + 3);
      
      
    { Now we have a step }
  end
  else begin
    Step := 1;
  end;

    
  LabelIndex := LabelIndex + 1;
  BreakPos := LabelIndex;
  LabelIndex := LabelIndex + 1;
  ContPos := LabelIndex;

  WriteCompiledCommand(Command_OpenScope, Lexemes[DoPos].Position);

    WriteCompiledCommand(Command_SetBreak, Lexemes[DoPos].Position);
    WriteInteger(BreakPos);
    WriteCompiledCommand(Command_SetContinue, Lexemes[DoPos].Position);
    WriteInteger(ContPos);

    WriteCompiledCommand(Command_ForSetType, Lexemes[DoPos].Position);
    if TempStr = 'to' then
      WriteInteger(1)
    else
      WriteInteger(0);

    WriteCompiledCommand(Command_ForSetStep, Lexemes[DoPos].Position);
    WriteInteger(Step);

    WriteCompiledCommand(Command_MoveStackPointer, Lexemes[DoPos].Position);
      CompileExpression(EqPos + 1, ToPos - 1, True);
      WriteCompiledCommand(Command_ForSetStart, Lexemes[DoPos].Position);
    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[DoPos].Position);

    WriteCompiledCommand(Command_MoveStackPointer, Lexemes[DoPos].Position);
      if (WithPos > 0) then
        CompileExpression(ToPos + 1, WithPos - 1, True)
      else
        CompileExpression(ToPos + 1, DoPos - 1, True);
      WriteCompiledCommand(Command_ForSetFinish, Lexemes[DoPos].Position);
    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[DoPos].Position);

    WriteCompiledCommand(Command_ForSetVar, Lexemes[DoPos].Position);
    WriteString(Variable);

    WriteCompiledCommand(Command_LABEL, Lexemes[DoPos].Position);
    WriteInteger(ContPos);

    { There loop begins }
//    WriteCompiledCommand(Command_OpenScope, Lexemes[Result].Position);

    WriteCompiledCommand(Command_ForCheck, Lexemes[DoPos].Position);

    Result := CompileBlockOfStatements(DoPos + 1);

//    WriteCompiledCommand(Command_LABEL, Lexemes[Result].Position);
//    WriteInteger(BreakPos);

//    WriteCompiledCommand(Command_CloseScope, Lexemes[Result].Position);

    WriteCompiledCommand(Command_JMP, Lexemes[Result].Position);
    WriteInteger(ContPos);

    WriteCompiledCommand(Command_LABEL, Lexemes[Result].Position);
    WriteInteger(BreakPos);

    (*WriteCompiledCommand(Command_CloseScope, Lexemes[Result].Position);*)

//    WriteCompiledCommand(Command_MoveStackPointer, Lexemes[DoPos].Position);
//    CompileExpression(EqPos + 1, ToPos - 1, True);
//    WriteCompiledCommand(Command_RestrictType, Lexemes[DoPos].Position);
//    WriteInteger(HLVM_Type_Integer);
//    WriteCompiledCommand(Command_PushLocal, Lexemes[DoPos].Position);
//    WriteString(Variable);
//    WriteCompiledCommand(Command_Assign, Lexemes[DoPos].Position);
//    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[DoPos].Position);
//
//    { Mark as loop }
//    WriteCompiledCommand(Command_PushVar, Lexemes[DoPos].Position);
//    WriteString(Variable);
//    WriteCompiledCommand(Command_MarkAsLoopVar, Lexemes[DoPos].Position);
//
//    { Compile High-Bound expression and use it... }
//    WriteCompiledCommand(Command_MoveStackPointer, Lexemes[DoPos].Position);
//    if WithPos > 0 then
//      CompileExpression(ToPos + 1, WithPos - 1, True)
//    else
//      CompileExpression(ToPos + 1, DoPos - 1, True);
//    WriteCompiledCommand(Command_RestrictType, Lexemes[DoPos].Position);
//    WriteInteger(HLVM_Type_Integer);
//
//    WriteCompiledCommand(Command_MoveStackPointer, Lexemes[DoPos].Position);
//
//    WriteCompiledCommand(Command_LABEL, Lexemes[DoPos].Position);
//    WriteInteger(ContPos);
//
//    { TO:     LBE <= HBE }   { * HBE < LBE == !(HBE >= LBE) }
//    { DOWNTO: LBE >= HBE }   { * HBE > LBE }
//    WriteCompiledCommand(Command_Duplicate, Lexemes[DoPos].Position);
//    WriteCompiledCommand(Command_PushVar, Lexemes[DoPos].Position);
//    WriteString(Variable);
//    if TempStr = 'to' then begin
//      WriteCompiledCommand(Command_GreaterEqual, Lexemes[DoPos].Position);
//      WriteCompiledCommand(Command_LgNOT, Lexemes[DoPos].Position);
//    end
//    else begin
//      WriteCompiledCommand(Command_GreaterThan, Lexemes[DoPos].Position);
//    end;
//    { If initial condition is violated here, loop must be broken }
//    WriteCompiledCommand(Command_JC, Lexemes[DoPos].Position);
//    WriteInteger(BreakPos);
//
//    Result := CompileBlockOfStatements(DoPos + 1);
//
//    WriteCompiledCommand(Command_PushVar, Lexemes[Result].Position);
//    WriteString(Variable);
//    WriteCompiledCommand(Command_PushInteger, Lexemes[Result].Position);
//    WriteIntegerValue(Step);
//    WriteCompiledCommand(Command_ArADD, Lexemes[Result].Position);
//    WriteCompiledCommand(Command_PushVar, Lexemes[Result].Position);
//    WriteString(Variable);
//    { Assign in strong way! }
//    WriteCompiledCommand(Command_StrongAssign, Lexemes[Result].Position);
//
//    WriteCompiledCommand(Command_JMP, Lexemes[Result].Position);
//    WriteInteger(ContPos);
//
//    WriteCompiledCommand(Command_LABEL, Lexemes[Result].Position);
//    WriteInteger(BreakPos);
//
//    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[Result].Position);
//    WriteCompiledCommand(Command_ClearStack, Lexemes[Result].Position);
//    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[Result].Position);

  WriteCompiledCommand(Command_CloseScope, Lexemes[Result].Position);

end;

function THCompiler.CompileFunctionDeclaration(PosToStart: Integer): Integer;
var NamePosStart, NamePosFinish: Integer;
begin
  if not (Lexemes[PosToStart].Content = 'function') then
    RaiseCompilationException('Internal compiler error: function statement is not started with `function` keyword', PosToStart);

  NamePosStart := PosToStart + 1;
  NamePosFinish := AnalyseTokensFor('(', NamePosStart, GetHighestLexeme);

  if NamePosFinish < 0 then
    RaiseCompilationException('Expected function arguments', PosToStart);
  if not ((NamePosFinish - NamePosStart) > 0) then
    RaiseCompilationException('Expected function name', PosToStart);

  NamePosFinish := NamePosFinish - 1;

  Result := CompileAnonymousFunctionDeclaration(PosToStart, True);
  CompileSimplifiedExpression(NamePosStart, NamePosFinish, True);
  WriteCompiledCommand(Command_Assign, Lexemes[Result].Position);
end;

function THCompiler.CompileIfStatement(PosToStart: Integer): Integer;
var Pos, ThenPos, IfLabel, ExitLabel: Integer;
begin
  if not (Lexemes[PosToStart].Content = 'if') then
    RaiseCompilationException('Internal compiler error: `if` statement expected', PosToStart);
  Pos := PosToStart + 1;
  ThenPos := AnalyseTokensFor('then', Pos, GetHighestLexeme);
  if (ThenPos < 0) then
    RaiseCompilationException('Expected `then` after `if` statement', PosToStart);
  LabelIndex := LabelIndex + 1;
  IfLabel := LabelIndex;
  { Expression of boolean }
  CompileExpression(Pos, ThenPos - 1, True);
  WriteCompiledCommand(Command_CheckStack, Lexemes[ThenPos].Position);
  WriteCompiledCommand(Command_JNC, Lexemes[ThenPos].Position);
  WriteInteger(IfLabel);
  WriteCompiledCommand(Command_OpenScope, Lexemes[ThenPos].Position);
  Pos := CompileBlockOfStatements(ThenPos + 1);
  if Lexemes[Pos].Content = 'end' then
    Pos := Pos + 1;
  if (Lexemes[Pos].Content = 'else') then begin
    { ELSE IS THERE! }
    LabelIndex := LabelIndex + 1;
    ExitLabel := LabelIndex;
    WriteCompiledCommand(Command_CloseScope, Lexemes[Pos].Position);
    WriteCompiledCommand(Command_JMP, Lexemes[Pos].Position);
    WriteInteger(ExitLabel);
    WriteCompiledCommand(Command_LABEL, Lexemes[Pos].Position);
    WriteInteger(IfLabel);
    WriteCompiledCommand(Command_OpenScope, Lexemes[Pos].Position);

    Pos := CompileBlockOfStatements(Pos + 1);
    WriteCompiledCommand(Command_CloseScope, Lexemes[Pos].Position);

    WriteCompiledCommand(Command_LABEL, Lexemes[Pos].Position);
    WriteInteger(ExitLabel);
    Result := Pos;
  end
  else begin
    WriteCompiledCommand(Command_CloseScope, Lexemes[Pos].Position);
    WriteCompiledCommand(Command_LABEL, Lexemes[Pos].Position);
    WriteInteger(IfLabel);
    Result := Pos;
  end;
end;

procedure THCompiler.CompileMiddleOperator(AOperator: THLexeme);
begin
  { Compiles middle stage of logical AND/OR }
  if (AOperator.Content = 'and') {or (AOperator.Content = '&&')} then begin
    LabelIndex := LabelIndex + 1;
    {
      A AND B ->

      (Push A;)
      CheckStack;
      JNC 1;
      (Push B;)
      (LgAnd;)
      Label 1;
    }
    WriteCompiledCommand(Command_CheckStackInvisible, AOperator.Position);
    { Memorize, where we stood }
    AOperator.Data_IDs_Add(LabelIndex);
    WriteCompiledCommand(Command_JNC, AOperator.Position);
    WriteInteger(LabelIndex);
  end
  else if (AOperator.Content = 'or') {or (AOperator.Content = '||')} then begin
    LabelIndex := LabelIndex + 1;
    WriteCompiledCommand(Command_CheckStackInvisible, AOperator.Position);
    AOperator.Data_IDs_Add(LabelIndex);
    WriteCompiledCommand(Command_JC, AOperator.Position);
    WriteInteger(LabelIndex);
  end;
end;

procedure THCompiler.CompileNonConstantSimplifiedExpression(PosToStart, UpTo: Integer; Complex: Boolean; ANewIndexed: Boolean);
var Pos, Temp {, LastPos}: Integer;
    CurrentLxm: THLexeme;
begin
  { It differs from compiling simplified expressions - constant expressions and functions calling is not allowed! }
  { Complex == !Constant }

  (* Compile Simplified expressions, like A.B[C](D){1, 2, 3} in recursive order *)
  (* Descending order. *)
  if PosToStart > UpTo then Exit;
  Pos := UpTo;
  while (Pos >= PosToStart) do begin
    CurrentLxm := Lexemes[Pos];
    case CurrentLxm.State of
      State_BraceClosed: begin
        { Get end of bracketed expression }
        Temp := GetPairingBracketPositionInversed(Pos - 1, CurrentLxm.ID);
        if not Complex and (Pos = UpTo) and not (CurrentLxm.Content = ']') then
          { Prevent calling function } 
          RaiseCompilationException('Expected variable identifier, but got constant expression', Pos);
        if PosToStart > Temp then
          RaiseCompilationException('Brackets general purpose violation', Pos);
        if Temp = PosToStart then begin
          { Not indexing or function call }
          if CurrentLxm.Content = ']' then
            RaiseCompilationException('Expected value for indexing', Temp);
          if CurrentLxm.Content = ')' then begin
            CompileExpression(Temp + 1, Pos - 1, True);
            CompileSimplifiedExpression(Pos + 1, UpTo, True);
          end
          else if CurrentLxm.Content = '}' then begin
            CompileTableExpression(Temp + 1, Pos - 1);
          end;
          Exit;
        end
        else begin
          if not (Lexemes[Temp - 1].State in [State_Token, State_String, State_BraceClosed]) then
            RaiseCompilationException('Invalid function call statement - expected <Token>, <String> or <Closing Bracket>', Temp - 1);
          if CurrentLxm.Content = ']' then begin
            { Indexing }
            { Compile first what we must index }
            if (Pos - Temp - 1) <= 0 then
              RaiseCompilationException('Expected value for indexing', Temp);
            CompileSimplifiedExpression(PosToStart, Temp - 1, True);
            CompileExpression(Temp + 1, Pos - 1, True);
            if ANewIndexed then begin
              if Pos = UpTo then begin
                WriteCompiledCommand(Command_NewIndex, CurrentLxm.Position);
              end
              else begin
                WriteCompiledCommand(Command_Index, CurrentLxm.Position);
                CompileSimplifiedExpression(Pos + 1, UpTo, True, True);
              end;
            end
            else begin
              WriteCompiledCommand(Command_Index, CurrentLxm.Position);
              CompileSimplifiedExpression(Pos + 1, UpTo, True);
            end;
            Exit;
          end
          else if CurrentLxm.Content = ')' then begin
            { Calling function }
            { Arguments are first! }
            WriteCompiledCommand(Command_MoveStackPointer, CurrentLxm.Position);
            CompileComplexExpressions(Temp + 1, Pos - 1, True);
            { Then compile function we must call }
            CompileSimplifiedExpression(PosToStart, Temp - 1, True);
            { Only simple-calls }
            WriteCompiledCommand(Command_SimpleCall, CurrentLxm.Position);
            WriteCompiledCommand(Command_RestoreStackPointer, CurrentLxm.Position);
            { Finally compile everything other }
            if (Pos < UpTo) then
              { There `Complexity` is useless - no way to make function call. }
              CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
            Exit;
          end
          else if CurrentLxm.Content = '}' then begin 
            { Calling function with table as parameter }
            { Argument is first in the queue }
            WriteCompiledCommand(Command_MoveStackPointer, CurrentLxm.Position);
            CompileTableExpression(Temp + 1, Pos - 1);
            CompileSimplifiedExpression(PosToStart, Temp - 1, True);
            { Only simple-calls }
            WriteCompiledCommand(Command_SimpleCall, CurrentLxm.Position);
            WriteCompiledCommand(Command_RestoreStackPointer, CurrentLxm.Position);
            if (Pos < UpTo) then
              CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
            Exit;
          end
          else 
            RaiseCompilationException('Unknown type of brackets: ' + CurrentLxm.Content, Pos);
        end;
      end;
      State_String: begin
        if (Pos > PosToStart) and (Lexemes[Pos - 1].State in [State_Token, State_String, State_BraceClosed]) then begin 
          { Function call }
          if not Complex and (Pos = UpTo) then
            RaiseCompilationException('Expected variable identifier, but got constant expression', Pos);
          { Argument }
          { To call function, we need to move stack pointer }
          WriteCompiledCommand(Command_MoveStackPointer, Lexemes[Pos].Position);
          WriteCompiledCommand(Command_PushString, Lexemes[Pos].Position);
          WriteString(GetRawStringContent(CurrentLxm.Content, Pos));
          CompileSimplifiedExpression(PosToStart, Pos - 1, True);
          { Always simple-calls }
          WriteCompiledCommand(Command_SimpleCall, Lexemes[Pos].Position);
          WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[Pos].Position);
          if (Pos < UpTo) then
            CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
          Exit;
        end
        else begin
          if Pos > PosToStart then
            RaiseCompilationException('Invalid function call', Pos);
          { Simple string }
          if not Complex and (Pos = UpTo) then
            RaiseCompilationException('Expected variable identifier, but got constant expression', Pos);
          CompileSimplifiedExpression(PosToStart, Pos - 1, True, False);
          WriteCompiledCommand(Command_PushString, Lexemes[Pos].Position);
          WriteString(GetRawStringContent(CurrentLxm.Content, Pos));
          CompileNonConstantSimplifiedExpression(Pos + 1, UpTo, Complex, ANewIndexed);
          Exit;
        end;
      end;
    end;
    Pos := Pos - 1;
  end;
  { That piece is without any function call }
  { As those strings represent strings like `A.B.C`, it should be inversed! }

  Temp := UpTo;
  while Temp >= PosToStart do begin
    if Lexemes[Temp].State = State_Separation then
      Break;
    Temp := Temp - 1;
  end;

  if Temp > PosToStart then
    UpTo := Temp;

  Pos := PosToStart;
  while (Pos <= UpTo) do begin 
    CurrentLxm := Lexemes[Pos];
    case CurrentLxm.State of
      State_Punctuation: begin
        (*
            A     .     B     .     C    (UpTo = Pos + 3)
          Pos-1  Pos  Pos+1 Pos+2 Pos+3
        *)
        if not ((CurrentLxm.Content = '.') or (CurrentLxm.Content = ':')) then
          RaiseCompilationException('Expression has invalid punctuation symbol: ' + CurrentLxm.Content, Pos);
        if Pos >= UpTo then
          RaiseCompilationException('Indexing point is at the end-of-expression', Pos);
        if not (Lexemes[Pos + 1].State = State_Token) then
          RaiseCompilationException('Expected <Identifier> after indexing point', Pos);
        WriteCompiledCommand(Command_PushString, CurrentLxm.Position);
        WriteString(Lexemes[Pos + 1].Content);
        if ANewIndexed and ((UpTo - Pos) = 1) then begin
          WriteCompiledCommand(Command_NewIndex, CurrentLxm.Position);
          ANewIndexed := False;
        end
        else
          WriteCompiledCommand(Command_Index, CurrentLxm.Position);
        { Mistake, it shoud be shifted to +1, as it will be incremented in the end of the loop }
//        Pos := Pos + 2;
        Pos := Pos + 1;
        Complex := True;
      end;
      State_Token: begin
        if not Complex and IsTokenConstant(CurrentLxm) then
          RaiseCompilationException('Expected variable identifier, but got constant expression', Pos);
        { It is always complex. Always. Do not try to think about non-complex! }
        CompilePush(CurrentLxm, {Complex or not (Pos = UpTo)} True);
        Complex := True;
      end;
      State_Separation: begin
        Break;
      end
    else 
      RaiseCompilationException('Unexpected state in expression: ' + GetTypeString(CurrentLxm.State), Pos);
    end;
    Pos := Pos + 1;
  end;
end;

procedure THCompiler.CompileOperator(const AOperator: THLexeme);
var Content: String;
begin
  Content := AOperator.Content;
  if (Content = '+') then begin
    WriteCompiledCommand(Command_ArADD, AOperator.Position);
  end
  else if (Content = '-') then begin
    WriteCompiledCommand(Command_ArSUB, AOperator.Position);
  end
  else if (Content = '*') then begin
    WriteCompiledCommand(Command_ArMUL, AOperator.Position);
  end
  else if (Content = '/') then begin
    WriteCompiledCommand(Command_ArDIV, AOperator.Position);
  end
  else if (Content = 'and') then begin
    WriteCompiledCommand(Command_LgAND, AOperator.Position);
    WriteCompiledCommand(Command_LABEL, AOperator.Position);
    WriteInteger(AOperator.Data_IDs[0]);
  end
  else if (Content = 'or') then begin
    WriteCompiledCommand(Command_LgOR, AOperator.Position);
    WriteCompiledCommand(Command_LABEL, AOperator.Position);
    WriteInteger(AOperator.Data_IDs[0]);
  end
  else if (Content = 'xor') then begin
    WriteCompiledCommand(Command_LgXOR, AOperator.Position);
  end
  else if (Content = 'band') then begin
    WriteCompiledCommand(Command_BtAND, AOperator.Position);
  end
  else if (Content = 'bor') then begin
    WriteCompiledCommand(Command_BtOR, AOperator.Position);
  end
  else if (Content = 'bxor') then begin
    WriteCompiledCommand(Command_BtXOR, AOperator.Position);
  end
  else if (Content = '=') then begin
    WriteCompiledCommand(Command_Equals, AOperator.Position);
  end
  else if (Content = '!=') or (Content = '<>') then begin
    WriteCompiledCommand(Command_Equals, AOperator.Position);
    WriteCompiledCommand(Command_LgNOT, AOperator.Position);
  end
  else if (Content = '>') or (Content = '<=') then begin
    WriteCompiledCommand(Command_GreaterThan, AOperator.Position);
    if not (Content = '>') then
      WriteCompiledCommand(Command_LgNOT, AOperator.Position);
  end
  else if (Content = '>=') or (Content = '<') then begin
    WriteCompiledCommand(Command_GreaterEqual, AOperator.Position);
    if not (Content = '>=') then
      WriteCompiledCommand(Command_LgNOT, AOperator.Position);
  end
  else if (Content = 'in') then begin
    WriteCompiledCommand(Command_IsIn, AOperator.Position);
  end
  else if (Content = '&&') then begin
    WriteCompiledCommand(Command_CBoolAnd, AOperator.Position);
  end
  else if (Content = '||') then begin
    WriteCompiledCommand(Command_CBoolOr, AOperator.Position);
  end
  else if (Content = '&') then begin
    WriteCompiledCommand(Command_CBitAnd, AOperator.Position);
  end
  else if (Content = '|') then begin
    WriteCompiledCommand(Command_CBitOr, AOperator.Position);
  end
  else if (Content = '~^') then begin
    WriteCompiledCommand(Command_CBitXor, AOperator.Position);
  end
  else if (Content = '>>') then begin
    WriteCompiledCommand(Command_CBitShr, AOperator.Position);
  end
  else if (Content = '<<') then begin
    WriteCompiledCommand(Command_CBitShl, AOperator.Position);
  end
  else if (Content = 'shl') then begin
    WriteCompiledCommand(Command_BitShiftLeft, AOperator.Position);
  end
  else if (Content = 'shr') then begin
    WriteCompiledCommand(Command_BitShiftRight, AOperator.Position);
  end
  else begin
//    raise ECompilationException.Create('Usage of non-implemented operator: ' + Content);
    raise ECompilationException.Create('Usage of non-implemented operator: "' + Content + '", ' + FormatAtPosition(AOperator.Position) );
  end;
end;

procedure THCompiler.CompilePostfixOperator(const AOperator: THLexeme);
var Content: String;
begin
  { BEFORE STATEMENT HAD BEEN COMPILED! }
  Content := AOperator.Content;
  if (Content = '!') then begin
    { Call function without parameters }
    { Therefore we need to move stack pointer to top before expression was compiled. }
    WriteCompiledCommand(Command_MoveStackPointer, AOperator.Position);
  end;
end;

procedure THCompiler.CompilePostfixOperatorAfter(const AOperator: THLexeme; const Complex: Boolean);
var Content: String;
begin
  Content := AOperator.Content;
  if (Content = '!') then begin
    { Call function without parameters }
    { Handle async statements. }
    if AOperator.Async then
      WriteCompiledCommand(Command_CallAsync, AOperator.Position)
    else if not Complex then
      WriteCompiledCommand(Command_Call, AOperator.Position)
    else
      WriteCompiledCommand(Command_SimpleCall, AOperator.Position);
    { Finally restore stack pointer. }
    WriteCompiledCommand(Command_RestoreStackPointer, AOperator.Position);
  end
  else if (Content = '?') then begin
    { Equals to nil? }
    { if (A?) then : means : if (A != nil) then }
    WriteCompiledCommand(Command_PushNil, AOperator.Position);
    WriteCompiledCommand(Command_Equals, AOperator.Position);
    WriteCompiledCommand(Command_LgNOT, AOperator.Position);
  end;
end;

procedure THCompiler.CompilePostfixOperators(PosToStart, UpTo: Integer; Complex: Boolean);
var Pos: Integer;
begin
  Pos := UpTo;
  if PosToStart > UpTo then RaiseCompilationException('Postfix operator should have initial expression', Pos);

  if IsPostfixOperator(Lexemes[Pos].Content) >= 0 then begin

    CompilePostfixOperator(Lexemes[Pos]);
    CompilePostfixOperators(PosToStart, Pos - 1, True);
    CompilePostfixOperatorAfter(Lexemes[Pos], Complex);

  end
  else
    CompileSimplifiedExpression(PosToStart, Pos, Complex);

end;

procedure THCompiler.CompilePrefixOperator(const AOperator: THLexeme);
var AContent: String;
begin
  AContent := AOperator.Content;
  if (AContent = '!') or (AContent = 'not') then begin 
    WriteCompiledCommand(Command_LgNOT, AOperator.Position);
  end
  else if (AContent = '~') or (AContent = 'bnot') then begin 
    WriteCompiledCommand(Command_BtNOT, AOperator.Position);
  end
  else if (AContent = '#') then begin 
    WriteCompiledCommand(Command_GetLength, AOperator.Position);
  end
  else if (AContent = '$') then begin 
    WriteCompiledCommand(Command_DoInterpolation, AOperator.Position);
  end
  else if (AContent = '-') then begin
    WriteCompiledCommand(Command_UnaryMinus, AOperator.Position);
  end
  else if (AContent = 'await') then begin
    WriteCompiledCommand(Command_MoveStackPointer, AOperator.Position);
    WriteCompiledCommand(Command_PushString, AOperator.Position);
    WriteString('Await');
    WriteCompiledCommand(Command_Index, AOperator.Position);
    WriteCompiledCommand(Command_SimpleCall, AOperator.Position);
    WriteCompiledCommand(Command_RestoreStackPointer, AOperator.Position);
  end
  else if (AContent = 'native') then begin
    WriteCompiledCommand(Command_MarkAsNative, AOperator.Position);
  end
  else if (AContent = 'const') then begin
    WriteCompiledCommand(Command_DoMonumentalPointer, AOperator.Position);
  end
  else
    raise ECompilationException.Create('Usage of non-implemented prefix operator: ' + AContent + ', ' + FormatAtPosition(AOperator.Position));
end;

procedure THCompiler.CompilePrefixOperators(PosToStart, UpTo: Integer; Complex: Boolean);
var Pos: Integer;
begin
  Pos := PosToStart;
  if PosToStart > UpTo then RaiseCompilationException('Prefix operator should be followed with expression', Pos);

  { Handle async keyword }
  if Lexemes[Pos].Content = 'async' then begin

    if not (Lexemes[UpTo].Content = '!') and (not (Lexemes[UpTo].State in [State_BraceClosed, State_String]) or (Lexemes[UpTo].Content = ']')) then
      RaiseCompilationException('Async requires function call in expression', UpTo);

    { Mark as Async }
    Lexemes[UpTo].Async := True;
//    CompileSimplifiedExpression(Pos + 1, UpTo, True);
    CompilePostfixOperators(Pos + 1, UpTo, True);

    Exit;
  end;


  if IsPrefixOperator(Lexemes[Pos].Content) >= 0 then begin
    CompilePrefixOperators(Pos + 1, UpTo, True);
    CompilePrefixOperator(Lexemes[Pos]);
  end
  else
//    CompileSimplifiedExpression(PosToStart, UpTo, Complex);
    CompilePostfixOperators(PosToStart, UpTo, Complex);
end;

procedure THCompiler.CompilePush(const AValue: THLexeme; const Complex: Boolean = False);
var AString: String;
    AFloat: HSFloat;
    ANumber: HSInteger;
begin
  AString := AValue.Content;
  if AString = 'nil' then
    WriteCompiledCommand(Command_PushNil, AValue.Position)
  else if (AString = '...') then begin
    { Arguments }
    if (Complex) then
      WriteCompiledCommand(Command_PushShortArguments, AValue.Position)
    else
      WriteCompiledCommand(Command_PushLongArguments, AValue.Position);
  end
  else if (AString = 'true') or (AString = 'True') then
    WriteCompiledCommand(Command_PushTrue, AValue.Position)
  else if (AString = 'false') or (AString = 'False') then
    WriteCompiledCommand(Command_PushFalse, AValue.Position)
  else begin
    if Pos('.', AString) > 0 then begin
      { It is probably a float number }
      // AString := AString.Replace('.', ',');
      // AString := AString.Replace('.', ThousandSeparator)
      if not TryStrToFloat(AString, AFloat, FormatSettings) then
        raise ECompilationException.Create('Malformed real number; ' + FormatAtPosition(AValue.Position));
      WriteCompiledCommand(Command_PushReal, AValue.Position);
      WriteRealValue(AFloat);
    end
    else if TokenIsNumeric(AString) then begin
      if not TryStrToInt64(AString, ANumber) then
        raise ECompilationException.Create('Malformed integer number; ' + FormatAtPosition(AValue.Position));
      WriteCompiledCommand(Command_PushInteger, AValue.Position);
      WriteIntegerValue(ANumber);
    end
    else begin
      WriteCompiledCommand(Command_PushVar, AValue.Position);
      WriteString(AValue.Content);
    end;
  end;
end;

function THCompiler.CompileReturnStatement(PosToStart: Integer): Integer;
var EndPos: Integer;
    TempStr: String;
begin
  if not (Lexemes[PosToStart].Content = 'return') then
    RaiseCompilationException('Internal compiler error: Return statement does not start with `return` keyword', PosToStart);
  //EndPos := AnalyseTokensFor([';', 'end', 'else'], PosToStart + 1, GetHighestLexeme, TempStr);
  EndPos := GetStatementClosure(PosToStart + 1, GetHighestLexeme, TempStr);
  if EndPos < 0 then
    EndPos := GetHighestLexeme + 1;

  WriteCompiledCommand(Command_ClearStack, Lexemes[PosToStart].Position);
  CompileComplexExpressions(PosToStart + 1, EndPos - 1, False);
  WriteCompiledCommand(Command_Return, Lexemes[EndPos].Position);
  Result := EndPos;
end;

procedure THCompiler.CompileSimplifiedExpression(PosToStart, UpTo: Integer; Complex: Boolean; ANewIndexed: Boolean);
var Pos, Temp {, LastPos}: Integer;
    CurrentLxm: THLexeme;
begin
  (* Compile Simplified expressions, like A.B[C](D){1, 2, 3} in recursive order *)
  { Descending order. }
  if PosToStart > UpTo then Exit;
  Pos := UpTo;
//  LastPos := Pos;
  while (Pos >= PosToStart) do begin
    CurrentLxm := Lexemes[Pos];

    case CurrentLxm.State of
      State_BraceClosed: begin
        { Get end of bracketed expression }
        Temp := GetPairingBracketPositionInversed(Pos - 1, CurrentLxm.ID);
        if PosToStart > Temp then
          RaiseCompilationException('Brackets general purpose violation', Pos);
        if Temp = PosToStart then begin
          { Not indexing or function call }
//          if CurrentLxm.Content = ']' then
//            RaiseCompilationException('Expected value for indexing', Temp);
          if CurrentLxm.Content = ')' then begin
            CompileExpression(Temp + 1, Pos - 1, True);
            CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
          end
          else if (CurrentLxm.Content = '}') or (CurrentLxm.Content = ']') then begin
            CompileTableExpression(Temp + 1, Pos - 1);
            CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
          end;
          Exit;
        end
        else begin
          if not (Lexemes[Temp - 1].State in [State_Token, State_String, State_BraceClosed]) then
            RaiseCompilationException('Invalid function call statement - expected <Token>, <String> or <Closing Bracket>', Temp - 1);
          if CurrentLxm.Content = ']' then begin
            { Indexing }
            { Compile first what we must index }
            if (Pos - Temp - 1) <= 0 then
              RaiseCompilationException('Expected value for indexing', Temp);
            CompileSimplifiedExpression(PosToStart, Temp - 1, True);
            CompileExpression(Temp + 1, Pos - 1, True);
            if ANewIndexed then begin
              if Pos = UpTo then begin
                WriteCompiledCommand(Command_NewIndex, CurrentLxm.Position);
              end
              else begin
                WriteCompiledCommand(Command_Index, CurrentLxm.Position);
                CompileSimplifiedExpression(Pos + 1, UpTo, Complex, True);
              end;
            end
            else begin
              WriteCompiledCommand(Command_Index, CurrentLxm.Position);
              CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
            end;
            Exit;
          end
          else if CurrentLxm.Content = ')' then begin
            { Calling function }
            { Arguments are first! }
            WriteCompiledCommand(Command_MoveStackPointer, CurrentLxm.Position);
            CompileComplexExpressions(Temp + 1, Pos - 1, True);
            { Then compile function we must call }
            CompileSimplifiedExpression(PosToStart, Temp - 1, True);
            if CurrentLxm.Objective then WriteCompiledCommand(Command_Duplicate, CurrentLxm.Position);
            if not CurrentLxm.Async then begin
              if not Complex and (Pos = UpTo) then
                { Strong call }
                WriteCompiledCommand(Command_Call, CurrentLxm.Position)
              else
                WriteCompiledCommand(Command_SimpleCall, CurrentLxm.Position);
            end
            else
              WriteCompiledCommand(Command_CallAsync, CurrentLxm.Position);
            WriteCompiledCommand(Command_RestoreStackPointer, CurrentLxm.Position);
            { Finally compile everything other }
            if (Pos < UpTo) then
              { There `Complexity` is useless - no way to make function call. }
              CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
            Exit;
          end
          else if CurrentLxm.Content = '}' then begin 
            { Calling function with table as parameter }
            { Argument is first in the queue }
            WriteCompiledCommand(Command_MoveStackPointer, CurrentLxm.Position);
            CompileTableExpression(Temp + 1, Pos - 1);
            CompileSimplifiedExpression(PosToStart, Temp - 1, True);
            if CurrentLxm.Objective then WriteCompiledCommand(Command_Duplicate, CurrentLxm.Position);
            if not CurrentLxm.Async then begin
              if not Complex and (Pos = UpTo) then
                WriteCompiledCommand(Command_Call, CurrentLxm.Position)
              else
                WriteCompiledCommand(Command_SimpleCall, CurrentLxm.Position);
            end
            else
              WriteCompiledCommand(Command_CallAsync, CurrentLxm.Position);
            WriteCompiledCommand(Command_RestoreStackPointer, CurrentLxm.Position);
            if (Pos < UpTo) then
              CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
            Exit;
          end
          else 
            RaiseCompilationException('Unknown type of brackets: ' + CurrentLxm.Content, Pos);
        end;
      end;

      State_String: begin
        if (Pos > PosToStart) and (Lexemes[Pos - 1].State in [State_Token, State_String, State_BraceClosed]) then begin
          { Function call }
          { Argument }
          { To call function, we need to move stack pointer }
          WriteCompiledCommand(Command_MoveStackPointer, Lexemes[Pos].Position);
          WriteCompiledCommand(Command_PushString, Lexemes[Pos].Position);
          WriteString(GetRawStringContent(CurrentLxm.Content, Pos));
          CompileSimplifiedExpression(PosToStart, Pos - 1, False);
          if CurrentLxm.Objective then WriteCompiledCommand(Command_Duplicate, CurrentLxm.Position);
          if not CurrentLxm.Async then begin
            if not Complex and (Pos = UpTo) then
              WriteCompiledCommand(Command_Call, Lexemes[Pos].Position)
            else
              WriteCompiledCommand(Command_SimpleCall, Lexemes[Pos].Position);
          end
          else
            WriteCompiledCommand(Command_CallAsync, CurrentLxm.Position);
          WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[Pos].Position);
          if (Pos < UpTo) then
            CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
          Exit;
        end
        else begin
          if Pos > PosToStart then
            RaiseCompilationException('Invalid function call', Pos);
          { Simple string }
          WriteCompiledCommand(Command_PushString, Lexemes[Pos].Position);
          WriteString(GetRawStringContent(CurrentLxm.Content, Pos));
          if (Pos < UpTo) then
            CompileSimplifiedExpression(Pos + 1, UpTo, Complex);
          Exit;
        end;
      end;

      State_Token: begin
        if CurrentLxm.Content = 'end' then begin
          { Must be `function() begin end` }
          while (Pos >= PosToStart) and not (Lexemes[Pos].Content = 'function') do begin
            Pos := Pos - 1;
          end;
          if not (Pos = PosToStart) then
            RaiseCompilationException('Malformed anonymous function declaration', PosToStart);

          Temp := CompileAnonymousFunctionDeclaration(Pos, False);
          if Temp > UpTo then
            RaiseCompilationException('Malformed anonymous function declaration', Temp);

          Exit;
        end;
      end;
    end;
    Pos := Pos - 1;
  end;

  { That piece is without any function call }
  Pos := PosToStart;
  while (Pos <= UpTo) do begin
    CurrentLxm := Lexemes[Pos];
    case CurrentLxm.State of
      State_Punctuation: begin
        if not ((CurrentLxm.Content = '.') or (CurrentLxm.Content = ':')) then
          RaiseCompilationException('Expression has invalid punctuation symbol: ' + CurrentLxm.Content, Pos);
        if Pos >= UpTo then
          RaiseCompilationException('Indexing point is at the end-of-expression', Pos);
        if not (Lexemes[Pos + 1].State = State_Token) then
          RaiseCompilationException('Expected <Identifier> after indexing point', Pos);
        if CurrentLxm.Objective then
          WriteCompiledCommand(Command_Duplicate, CurrentLxm.Position);
        WriteCompiledCommand(Command_PushString, CurrentLxm.Position);
        WriteString(Lexemes[Pos + 1].Content);
        if ANewIndexed and ((UpTo - Pos) = 1) then begin
          WriteCompiledCommand(Command_NewIndex, CurrentLxm.Position);
          ANewIndexed := False;
        end
        else begin
          WriteCompiledCommand(Command_Index, CurrentLxm.Position);
        end;
        { Should be reduced to that }
        Pos := Pos + 1;
        Complex := True;
      end;
      State_Token: begin
        CompilePush(CurrentLxm, Complex or not (Pos = UpTo));
        Complex := True;
      end;
      State_Separation: begin
        Break;
      end
    else 
      RaiseCompilationException('Unexpected state in expression: ' + GetTypeString(CurrentLxm.State), Pos);
    end;
    Pos := Pos + 1;
  end;
end;

function THCompiler.CompileStatement(PosToStart: Integer): Integer;
var Pos, UpTo: Integer;
    CurrentLxm: THLexeme;
    TempStr: String;
begin
  UpTo := GetHighestLexeme;
  Pos := PosToStart;

  CurrentLxm := Lexemes[Pos];
  if (CurrentLxm.Content = 'local') then begin
    { Local assignement or local function definition }
    if (Pos >= UpTo) then
      RaiseCompilationException('`local` modifier cannot be used alone', Pos);
    if (Lexemes[Pos + 1].Content = 'function') then begin
      { Function }
      Result := Pos;
    end
    else begin
      { Assignment }
      Result := CompileAssigning(Pos);
    end;
  end
  else if (CurrentLxm.Content = 'if') then begin
    Result := CompileIfStatement(Pos);
  end
  else if (CurrentLxm.Content = 'while') then begin
    Result := CompileWhileStatement(Pos);
  end
  else if (CurrentLxm.Content = 'for') then begin
    Result := CompileForIteratorStatement(Pos);
  end
  else if (CurrentLxm.Content = 'repeat') then begin
    Result := Pos;
  end
  else if (CurrentLxm.Content = 'function') then begin
    Result := CompileFunctionDeclaration(Pos);
  end
  else if (CurrentLxm.Content = 'return') then begin
    Result := CompileReturnStatement(Pos);
  end
  else if (CurrentLxm.Content = 'begin') then begin
    WriteCompiledCommand(Command_OpenScope, CurrentLxm.Position);
    Result := CompileBlockOfStatements(Pos);
    if (Result <= UpTo) and (Lexemes[Result + 1].Content = '.') then begin
      { `end.` - we should halt execution }
      if ((Result + 1) < UpTo) then
        RaiseCompilationException('`end` followed with `.` must be final instruction of the program', Result);
      { `.` position }
      Result := Result + 1;
    end;
    WriteCompiledCommand(Command_CloseScope, Lexemes[Result].Position);
  end
  else if (CurrentLxm.Content = 'break') then begin
    Result := Pos + 1;
    WriteCompiledCommand(Command_BreakLoop, CurrentLxm.Position);
  end
  else if (CurrentLxm.Content = 'continue') then begin
    Result := Pos + 1;
    WriteCompiledCommand(Command_ContinueLoop, CurrentLxm.Position);
  end
  else begin
    //UpTo := AnalyseTokensFor([';', 'end', 'else'], Pos, UpTo, TempStr);
    UpTo := GetStatementClosure(Pos, UpTo, TempStr);
    if UpTo < 0 then UpTo := GetHighestLexeme + 1;
    if (UpTo - Pos) <= 0 then begin
      { Empty statement }
      Result := UpTo;
      Exit;
    end;

    if (AnalyseTokensFor(':=', Pos, UpTo - 1) < 0) then begin
      WriteCompiledCommand(Command_MoveStackPointer, Lexemes[Pos].Position);
      CompileExpression(Pos, UpTo - 1, False);
      WriteCompiledCommand(Command_ClearStack, Lexemes[Pos].Position);
      WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[Pos].Position);
      Result := UpTo;
    end
    else begin
      Result := CompileAssigning(Pos);
    end;
  end;
end;

procedure THCompiler.CompileTableExpression(PosToStart, UpTo: Integer);
var Pos, Counter, Temp, TempAnother: Integer;
    CurrentLxm: THLexeme;
    TempStr: String;
begin
  { Method for table compilation }
  Counter := 1;
  { Table is compiled on stack. }
  {
    1. Move Stack pointer.
    2. Throw Value, Key;
    3. Repeat step 2 as many times, as you want;
    4. CreateTable;
    5. Restore Stack Pointer.
  }

  WriteCompiledCommand(Command_MoveStackPointer, Lexemes[PosToStart].Position);

  if (AnalyseTokensFor([',', ';'], PosToStart, UpTo, TempStr) >= 0) or (Lexemes[PosToStart].Content = '[') or (Lexemes[PosToStart + 1].Content = ':=') then begin
    Pos := PosToStart;
    while (Pos <= UpTo) do begin

      CurrentLxm := Lexemes[Pos];

      if ((UpTo - Pos) = 0) and (CurrentLxm.State = State_Separation) then
        { This is last `,` or `;` and DScript supports it as syntax }
        Break;
      
      if (CurrentLxm.Content = '[') then begin
        { Indexing using indexer }
        Temp := GetPairingBracketPosition(Pos + 1, CurrentLxm.ID);

        if (Temp < 0) or (Temp < PosToStart) or (Temp > UpTo) then
          RaiseCompilationException('Bracket is violating table bounds', Pos);
        if ((Temp - 1) - (Pos + 1)) < 0 then
          RaiseCompilationException('Expected Identifier', Pos + 1);
        if (Temp + 2 > UpTo) then RaiseCompilationException('Indexer needs value to assign', Temp);

        if not ((Lexemes[Temp + 1].Content = ':') or (Lexemes[Temp + 1].Content = ':=')) then
          RaiseCompilationException('Expected ":" or ":=", but got "' + Lexemes[Temp + 1].Content + '" instead', Temp + 1);
        TempAnother := AnalyseTokensFor([',', ';'], Temp + 2, UpTo, TempStr);

        if (TempAnother < 0) then TempAnother := UpTo + 1;
        if (TempAnother - (Temp + 2)) < 1 then RaiseCompilationException('Expected expression', Temp + 2);

        { We always expect there only one returned value. }
        (* Value expression *)
        CompileExpression(Temp + 2, TempAnother - 1, True);
        (* Index expression *)
        CompileExpression(Pos + 1, Temp - 1, True);

        Pos := TempAnother + 1;
      end
      else if (Pos + 2 <= UpTo) and (Lexemes[Pos + 1].Content = ':=') and (CurrentLxm.State = State_Token) then begin
        { Assigning using token. }
        Temp := AnalyseTokensFor([',', ';'], Pos + 2, UpTo, TempStr);
        if (Temp < 0) then Temp := UpTo + 1;
        if (Temp - (Pos + 2)) < 1 then
          RaiseCompilationException('Expected expression', Pos + 2);

        { Value expression }
        CompileExpression(Pos + 2, Temp - 1, True);
        { Index expression }
        WriteCompiledCommand(Command_PushString, CurrentLxm.Position);
        WriteString(CurrentLxm.Content);

        Pos := Temp + 1;
      end
      else begin
        { Simple enumerable interface }
        Temp := AnalyseTokensFor([',', ';'], Pos, UpTo, TempStr);
        if (Temp < 0) then Temp := UpTo + 1;
        if (Temp - Pos) < 1 then
          RaiseCompilationException('Expected expression', Pos);

        { Value expression }
        CompileExpression(Pos, Temp - 1, True);
        { Index expression }
        WriteCompiledCommand(Command_PushInteger, CurrentLxm.Position);
        WriteIntegerValue(Counter);

        { Counting from 1. Now you can get furious. But you will not be as furious as Furious Hunter200165, the creator of DScript! }
        Counter := Counter + 1;

        Pos := Temp + 1;
      end;
    end;

    WriteCompiledCommand(Command_CreateTable, Lexemes[UpTo].Position);
    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[UpTo].Position);
  end
  else begin
    { Run-Time Table }
    CompileExpression(PosToStart, UpTo, False);
    WriteCompiledCommand(Command_CreateRunTimeTable, Lexemes[UpTo].Position);
    WriteCompiledCommand(Command_RestoreStackPointer, Lexemes[UpTo].Position);
  end;
end;

function THCompiler.CompileTryStatement(PosToStart: Integer): Integer;
var Pos, PosEnd, Current, Ident, Position: Integer;
    {Count, }MarkFirst, MarkExit: Integer;
    Chosen: String;
begin
  if not (Lexemes[PosToStart].Content = 'try') then
    RaiseCompilationException('Internal compiler error: `try` statement is not started with `try` keyword', PosToStart);

  Pos := AnalyseTokensFor(['except', 'finally'], PosToStart + 1, GetHighestLexeme, Chosen);
  if Pos < 0 then
    RaiseCompilationException('`Try` statement should be followed with `except` or `finally`', PosToStart);

  LabelIndex := LabelIndex + 1;
  MarkFirst := LabelIndex;
  if Chosen = 'except' then begin
    WriteCompiledCommand(Command_SetExcept, Lexemes[Pos].Position);
    WriteInteger(MarkFirst);
  end
  else begin
    WriteCompiledCommand(Command_SetFinally, Lexemes[Pos].Position);
    WriteInteger(MarkFirst);
  end;

  PosEnd := AnalyseTokensFor('end', PosToStart + 1, GetHighestLexeme);
  if PosEnd < 0 then
    RaiseCompilationException('`Try` statement is malformed: `End` is not found', PosToStart);

  { Compile block of single statements }
  WriteCompiledCommand(Command_OpenScope, Lexemes[PosToStart].Position);
  Position := PosToStart + 1;
  while (Position < Pos) do
    Position := CompileStatement(Position) + 1;
  if (Position > Pos) then
    RaiseCompilationException('Single statement violates `Try` instructions block!', Position);
  WriteCompiledCommand(Command_CloseScope, Lexemes[Pos].Position);

  if Chosen = 'except' then begin
    LabelIndex := LabelIndex + 1;
    MarkExit := LabelIndex;

    Current := Pos + 1;
    if (Current >= PosEnd) or not (Lexemes[Current].Content = 'on') then
      RaiseCompilationException('`Try-Except` statement must have `on` keyword after `except`', Pos);

    Current := Current + 1;
    Ident := Current;
    if (Ident >= PosEnd) or not (IsValidIdentifier(Lexemes[Ident])) then
      RaiseCompilationException('`Try-Except` statement must have valid identifier after `except on`', Pos);

    Current := Current + 1;
    if (Current >= PosEnd) or not (Lexemes[Current].Content = 'do') then
      RaiseCompilationException('`Try-Except` statement must have `do` keyword after exception identifier', Pos);

    { Closing try section to avoid handling other exceptions! }
    { 1st case: All is done normally. We need to exit and forget about try-except }
    WriteCompiledCommand(Command_CloseTry, Lexemes[Pos].Position);
    WriteCompiledCommand(Command_JMP, Lexemes[Pos].Position);
    WriteInteger(MarkExit);
    { 2nd case: Exception had occured }
    WriteCompiledCommand(Command_LABEL, Lexemes[Pos].Position);
    WriteInteger(MarkFirst);
    WriteCompiledCommand(Command_CloseTry, Lexemes[Pos].Position);

    { Except section. }
    WriteCompiledCommand(Command_OpenScope, Lexemes[Pos].Position);
    Position := Current + 1;
    while (Position < PosEnd) do
      Position := CompileStatement(Position) + 1;
    if (Position > PosEnd) then
      RaiseCompilationException('Single expression violates `Try-Except` instructions block!', Position);
    WriteCompiledCommand(Command_CloseScope, Lexemes[PosEnd].Position);

    { Exit mark for normal execution }
    WriteCompiledCommand(Command_LABEL, Lexemes[PosEnd].Position);
    WriteInteger(MarkExit);
  end
  else begin

  end;

  Result := PosEnd;
end;

function THCompiler.CompileWhileStatement(PosToStart: Integer): Integer;
var Pos, BreakPos, ContPos: Integer;
begin
  if not (Lexemes[PosToStart].Content = 'while') then
    RaiseCompilationException('Internal compiler error: `while` statement is not started with `while` keyword', PosToStart);

  Pos := AnalyseTokensFor('do', PosToStart + 1, GetHighestLexeme);
  if (Pos < 0) then
    RaiseCompilationException('While statement should be followed with `do`', PosToStart);

  LabelIndex := LabelIndex + 1;
  ContPos := LabelIndex;
  LabelIndex := LabelIndex + 1;
  BreakPos := LabelIndex;

  { Open Scope }

  WriteCompiledCommand(Command_OpenScope, Lexemes[PosToStart].Position);
  WriteCompiledCommand(Command_SetBreak, Lexemes[PosToStart].Position);
  WriteInteger(BreakPos);
  WriteCompiledCommand(Command_SetContinue, Lexemes[PosToStart].Position);
  WriteInteger(ContPos);
  { Writing continue... }
  WriteCompiledCommand(Command_LABEL, Lexemes[PosToStart].Position);
  WriteInteger(ContPos);

  { Compile boolean expression }
  CompileExpression(PosToStart + 1, Pos - 1, True);
  WriteCompiledCommand(Command_CheckStack, Lexemes[Pos].Position);
  WriteCompiledCommand(Command_JNC, Lexemes[Pos].Position);
  WriteInteger(BreakPos);

  Result := CompileBlockOfStatements(Pos + 1);

  WriteCompiledCommand(Command_JMP, Lexemes[Result].Position);
  WriteInteger(ContPos);
  WriteCompiledCommand(Command_LABEL, Lexemes[Result].Position);
  WriteInteger(BreakPos);

  WriteCompiledCommand(Command_CloseScope, Lexemes[Result].Position);
end;

constructor THCompiler.Create;
begin
  LexerObject := THLexer.Create;
  Lexemes := THLexemes.Create(True);
  LexerObject.Lexemes := Lexemes;

  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ThousandSeparator := ',';
end;

destructor THCompiler.Destroy;
begin
  LexerObject.Free;
  Lexemes.Free;
  inherited;
end;

function THCompiler.ExpectStateUp_Raw(PosToStart: Integer; Args: array of Const): Integer;
var Lxm: THLexeme;
    i: Integer;
begin
  Result := -1;
  if (PosToStart >= Lexemes.Count - 1) then Exit;
  Lxm := Lexemes[PosToStart + 1];
  for i := 0 to Length(Args) - 1 do begin
    if Args[i].VInteger = Lxm.State then begin
      Result := Args[i].VInteger;
      Break;
    end;
  end;
end;

procedure THCompiler.FixLexemes;
var Pos, i: Integer;
    NewLexemes: THLexemes;
begin
  Pos := 0;
  NewLexemes := THLexemes.Create( True {Lexemes.OwnsObjects} );
  while (Pos < Lexemes.Count) do begin
//    if Lexemes[Pos].State in [State_Spacing, State_Comment] then
//      Lexemes.Delete(Pos)
//    else
//      Pos := Pos + 1;
    if not (Lexemes[Pos].State in [ State_Spacing, State_Comment ]) then
      NewLexemes.Add(Lexemes[Pos])
    else
      Lexemes[Pos].Free;
    Pos := Pos + 1;
  end;
  Lexemes.OwnsObjects := False;
  Lexemes.Free;
  Lexemes := NewLexemes;

  FixNumbers;
  for i := 0 to Lexemes.Count - 1 do begin
    if (Lexemes[i].Content = '...') then
      Lexemes[i].State := State_Token;
  end;
  Pos := 0;
  while (Pos < Lexemes.Count - 1) do begin
    {#! Fix that }
    if (Lexemes[Pos].Content = 'set') and (Lexemes[Pos + 1].Content = '[') then begin
      i := GetPairingBracketPosition(Pos + 2, Lexemes[Pos + 1].ID);
      if i < 0 then
        RaiseCompilationException('Cannot get pairing bracket', Pos);
      Lexemes[Pos + 1].Content := '{';
      Lexemes[i].Content := '}';
      Lexemes.Delete(Pos);
    end
    else begin
      Pos := Pos + 1;
    end;
  end;

//  Pos := 1;
  (*while (Pos < Lexemes.Count - 3) do begin
    { Colon syntax is deprecated in KadJIT v1.6 }
    if (Lexemes[Pos].Content = ':') then begin
//      Writeln('I see colon.');
      if Lexemes[Pos - 1].Content = ']' then begin
        i := GetPairingBracketPositionInversed(Pos - 2, Lexemes[Pos - 1].ID);
        if i < 0 then
          RaiseCompilationException('Cannot get pairing bracket', Pos - 1);
        if (i = 0) or (Lexemes[i - 1].State in [State_None, State_Punctuation, State_Separation]) then begin
          Pos := Pos + 1;
          Continue;
        end;
      end;
      if IsValidIdentifier(Lexemes[Pos + 1]) then begin
        if (Lexemes[Pos + 2].Content = '(') or (Lexemes[Pos + 2].Content = '{') then begin
          i := GetPairingBracketPosition(Pos + 3, Lexemes[Pos + 2].ID);
          if I < 0 then
            RaiseCompilationException('Cannot get pairing bracket', Pos + 3);
//          Lexemes[i].Objective := True;
          Lexemes[Pos].Content := '.';
          Lexemes[Pos].Objective := True;
        end
        else if (Lexemes[Pos + 2].State = State_String) then begin
//          Lexemes[Pos + 2].Objective := True;
          Lexemes[Pos].Content := '.';
          Lexemes[Pos].Objective := True;
        end;
      end;
    end;
    Pos := Pos + 1;
  end;*)
end;

procedure THCompiler.FixNumbers;
var Pos, Len, i: Integer;
    TempOut: Int64;
begin
  { This will tranform all floating point numbers to their normal state }
  Pos := 0;
  Len := Lexemes.Count;
  while (Pos < Len - 2) do begin
    if TokenIsNumeric(Lexemes[Pos].Content) and (Lexemes[Pos + 1].Content = '.') and TokenIsNumeric(Lexemes[Pos + 2].Content)  then
    begin
      Lexemes[Pos].Content := Lexemes[Pos].Content + Lexemes[Pos + 1].Content + Lexemes[Pos + 2].Content;
      { Delete `.` and number for floating point }
      Lexemes.Delete(Pos + 2);
      Lexemes.Delete(Pos + 1);
    end;
    Pos := Pos + 1;
    Len := Lexemes.Count;
  end;
  { Dealing with hexadecimal numbers }
  for i := 0 to Lexemes.Count - 1 do begin
    if System.Pos('0x', Lexemes[i].Content) = 1 then
      if not TryStrToInt64(Lexemes[i].Content, TempOut) then
        RaiseCompilationException('Malformed hexadecimal number', i)
      else
        Lexemes[i].Content := TempOut.ToString;
  end;
end;

function THCompiler.FormatAtPosition(const APos: THPosition): String;
begin
  Result := Format('at ("%s", %d, %d)', [APos.ScopeName, APos.LineNumber, APos.RowNumber]);
end;

function THCompiler.GetHighestLexeme: Integer;
begin
  Result := Lexemes.Count - 1;
end;

function THCompiler.GetMidOperatorPriority(const Op: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Length(MidOperators) - 1 do begin
    if (MidOperators[i] = Op) then begin
      { Index is priority. }
      { Lower index - higher priority }
      Result := i;
      Break;
    end;
  end;
end;

function THCompiler.GetPairingBracketPosition(const PosToStart, BracketID: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := PosToStart to Lexemes.Count - 1 do begin
    if (Lexemes[i].State in [State_BraceClosed, State_BraceOpened]) and (Lexemes[i].ID = BracketID) then begin
      Result := i;
      Break;
    end;
  end;
end;

function THCompiler.GetPairingBracketPositionInversed(const PosToStart, BracketID: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := PosToStart downto 0 do begin
    if (Lexemes[i].State in [State_BraceOpened, State_BraceClosed]) and (Lexemes[i].ID = BracketID) then begin
      Result := i;
      Break;
    end;
  end;
end;

function THCompiler.GetRawStringContent(const ALanguageStr: String; const Pos: Integer): String;
var AString: String;
    i, Len: Integer;
begin
  if (ALanguageStr[1] = '''') or (ALanguageStr[1] = '"') then begin 
    AString := Copy(ALanguageStr, 2, ALanguageStr.Length - 2);
    Result := '';
    i := 1;
    Len := AString.Length;
    while (i <= Len) do begin 
      case AString[i] of
        '\': begin 
          { Escape sequence should be followed }
          if i >= Len then RaiseCompilationException('Escape sequence was not closed!', Pos);
          case AString[i + 1] of
            '\', '''', '"': Result := Result + AString[i + 1];
            'n': Result := Result + #10;
            't': Result := Result + #9;
            'r': Result := Result + #13;
            'a': Result := Result + #7;
            'b': Result := Result + #8;
            'v': Result := Result + #11;
            'f': Result := Result + #14;
            '0': Result := Result + #0;
          else 
            RaiseCompilationException('Invalid escape character: ' + AString[i + 1], Pos);
          end;
          i := i + 2;
        end;
      else 
        Result := Result + AString[i];
      end;
      i := i + 1;
    end;
  end
  else begin
    i := 2;
    while (ALanguageStr.Length >= i) and not (ALanguageStr[i] = '[') do 
      i := i + 1;
    AString := Copy(ALanguageStr, i + 1, ALanguageStr.Length - 2 * i);
    Result := AString;
  end;
end;

function THCompiler.GetStatementClosure(const AFrom, ATo: Integer; var AChosenString: String): Integer;
begin
  Result := AnalyseTokensFor(EndOfStatement, AFrom, ATo, AChosenString);
end;

function THCompiler.GetTypeString(const AType: Integer): String;
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

function THCompiler.IsOpeningClosureWord(const AWord: String): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to Length(OpeningWords) - 1 do
    if OpeningWords[i] = AWord then begin
      Result := True;
      Break;
    end;
end;

function THCompiler.IsPostfixOperator(const AContext: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Length(PostfixOperators) do begin
    if PostfixOperators[i] = AContext then begin
      Result := i;
      Break;
    end;
  end;
end;

function THCompiler.IsPrefixOperator(const AContext: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Length(PrefixOperators) - 1 do begin
    if (PrefixOperators[i] = AContext) then begin
      Result := i;
      Break;
    end;
  end;
end;

function THCompiler.IsTokenConstant(const AToken: THLexeme): Boolean;
begin
  Result := (AToken.State = State_String) or (AToken.State = State_Token) and 
    (AToken.Content = 'nil') or 
    (AToken.Content = 'true') or (AToken.Content = 'True') or 
    (AToken.Content = 'false') or (AToken.Content = 'False') or 
    (IsTokenNumberic(AToken));
end;

function THCompiler.IsTokenNumberic(const AToken: THLexeme): Boolean;
var i, Len: Integer;
begin
  Result := AToken.State = State_Token;
  if not Result then Exit;
  Len := AToken.Content.Length;
  for i := 1 to Len do begin
    Result := Result and CharInSet(AToken.Content[i], ['0' .. '9', '.']);
    if not Result then Break;
  end;
end;

function THCompiler.IsTokenReserved(const ATokenContent: String): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to Length(ReservedWords) - 1 do begin 
    Result := ReservedWords[i] = ATokenContent;
    if Result then
      Break;
  end;
end;

function THCompiler.IsValidIdentifier(const AIdentifier: THLexeme): Boolean;
begin
  { IsTokenConstant checks for numeric tokens. }
  Result := (AIdentifier.State = State_Token) and not IsTokenConstant(AIdentifier) and not IsTokenReserved(AIdentifier.Content);
end;

procedure THCompiler.RaiseCompilationException(const Msg: String; const APos: Integer);
begin
  raise ECompilationException.Create('[FATAL]: Compilation failed : ' + Msg  + '; ' + FormatAtPosition(Lexemes[APos].Position));
end;

function THCompiler.ResolveScopeClosure(const PosToStart: Integer): Integer;
var Count, Pos: Integer;
begin
  { Finds begin-end pair to make scope closure }
  Pos := PosToStart;
  Count := 0;
  repeat
    if IsOpeningClosureWord(Lexemes[Pos].Content) then
      Count := Count + 1
    else if (Lexemes[Pos].Content = 'end') then
      Count := Count - 1;
    if Count < 0 then
      RaiseCompilationException('Scope closure violation - `end` closes nothing', Pos);
    if (Count > 0) then
    Pos := Pos + 1;
  until (Count = 0);
  Result := Pos;
end;

function THCompiler.ScanForToken_Down(PosToStart: Integer; const OperatorsPriority: Integer = -1; const UpTo: Integer = -1): Integer;
var Pos, Len: Integer;
    LastState, Temp: Integer;
    Lxm: THLexeme;
begin
//  Result := -1;
  Pos := PosToStart;
  Len := Lexemes.Count;
  LastState := State_None;
  while (Pos < Len) do begin

//    if (UpTo >= 0) and (Pos > UpTo) and (LastState in [State_Token, State_String, State_BraceClosed]) then begin
//      Pos := Pos - 1;
//      Break;
//    end;

    if (UpTo > 0) and (Pos > UpTo) then begin
      Pos := UpTo;
      Break;
    end;

    Lxm := Lexemes[Pos];

    if Lxm.Content = 'function' then begin
      Temp := Pos;
      Pos := AnalyseTokensFor('begin', Pos + 1, UpTo);
      if Pos < 0 then RaiseCompilationException('Function must be followed by `begin` keyword', Temp);

      Lxm := Lexemes[Pos];
      LastState := State_Token;
    end;

    if Lxm.Content = 'begin' then begin
      Temp := Pos;
      Pos := AnalyseTokensFor('end', Pos + 1, UpTo);
      if Pos < 0 then RaiseCompilationException('Cannot resolve scope closure', Temp);

      Pos := Pos + 1;
      LastState := State_Token;
      Continue;
    end;

    if Lxm.Content = 'end' then begin
      Pos := Pos - 1;
      Break;
    end;

    if (LastState = State_None) and (IsPrefixOperator(Lxm.Content) >= 0) then begin
      { If we have prefix operators! }
      Pos := Pos + 1;
      while (Pos <= Len - 1) and (IsPrefixOperator(Lexemes[Pos].Content) >= 0) do begin
        Pos := Pos + 1;
      end;
      if Pos >= Len then
        RaiseCompilationException('Prefix operator must be followed with <Identifier>', Pos - 1);
      Lxm := Lexemes[Pos];
    end;

    if (LastState in [State_Token, State_BraceClosed, State_String]) and (IsPostfixOperator(Lxm.Content) >= 0) then begin
      if (Pos = UpTo) then begin
        Break;
      end
      else if (Pos > UpTo) then begin
        RaiseCompilationException('Internal compiler error: Postfix operator violated general expression bounds', Pos);
      end;

      Pos := Pos + 1;
      while (Pos < UpTo) and (IsPostfixOperator(Lexemes[Pos].Content) >= 0) do begin
        Pos := Pos + 1;
      end;

      if (Pos = UpTo) then begin
        Break;
      end
      else if (Pos > UpTo) then begin
        RaiseCompilationException('Internal compiler error: Postfix operator violated general expression bounds', Pos);
      end;

      if not (Lexemes[Pos].State in [State_Punctuation, State_BraceClosed, State_BraceOpened, State_String, State_Separation]) and
         not (GetMidOperatorPriority(Lexemes[Pos].Content) >= 0) then
        RaiseCompilationException('Postfix operator must enclose the part of expression', Pos);
      Lxm := Lexemes[Pos];
    end;
    
    if not (Lxm.State in [State_Token, State_BraceOpened, State_String]) and (LastState = State_None) then
      { We should usually expect Identifier. }
      RaiseCompilationException('Expected <Identifier> but got "' + Lxm.Content + '"', Pos)
    else if (LastState = State_None) then begin
      if not (Lxm.State in [State_BraceOpened]) then begin
        LastState := Lxm.State;
        Pos := Pos + 1;
        Continue;
      end;
    end;

    if (Lxm.State in [State_Token, State_Punctuation]) then begin
      Temp := GetMidOperatorPriority(Lxm.Content);
//      if not (Temp >= 0) then RaiseCompilationException('Unexpected "' + Lxm.Content + '"', Pos);
      if (Temp >= 0) then begin
        if (OperatorsPriority < 0) or (OperatorsPriority < Temp) then begin
          { Lower value - higher the actual priority of operator }
          { And we are falling back to the nearest operator }
          Pos := Pos - 1;
          LastState := Lexemes[Pos].State;
          Break;
        end;
        LastState := State_Operator;
        Pos := Pos + 1;
        Continue;
      end
//      else begin
//        Pos := Pos + 1;
//        Continue;
//      end;
    end;

    case Lxm.State of
      State_Token: begin   
        if not (LastState in [State_Punctuation, State_BraceOpened, State_Operator]) then
          RaiseCompilationException('Unexpected <Identifier>', Pos);
        LastState := State_Token;
        Pos := Pos + 1;
        Continue;
      end;
      State_String: begin
        { Clear value }
        if not (LastState in [State_Token, State_String, State_BraceClosed, State_BraceOpened, State_Separation, State_Operator]) then
          RaiseCompilationException('Unexpected <String>', Pos);
        LastState := State_String;
        Pos := Pos + 1;
        Continue;
      end;
      State_Punctuation: begin
        if not (LastState in [State_Token, State_String, State_BraceClosed]) then
          RaiseCompilationException('Expected <Identifier> but got <' + GetTypeString(LastState) + '>', Pos);
        if (Lxm.Content = '.') or (Lxm.Content = ':') then begin
          { Methods and fields. }
          LastState := State_Punctuation;
          Pos := Pos + 1;
          Continue;
        end;
      end;
      State_BraceOpened: begin
        Temp := Pos;
        Pos := Pos + 1;
        while (Pos < Len) and not (Lexemes[Pos].ID = Lxm.ID) do
          Pos := Pos + 1;
        if (Pos >= Len) then 
          RaiseCompilationException('Expected "' + HLM_GetPairingBracket(Lxm.Content[1]) + '" to close "' + Lxm.Content + '"', Temp);
        { Skipping the last brace }
        Pos := Pos + 1;
        LastState := State_BraceClosed;
        Continue;
      end;
      State_BraceClosed: begin 
        { We have no reason, why not to leave }
        Pos := Pos - 1;
        LastState := Lexemes[Pos].State;
        Break;
//        Pos := Pos + 1;
//        Continue;
      end;
      State_Separation: begin
        { The decremented position is the end of expression }
        Pos := Pos - 1;
        Break;
      end;
    end;
  end;
  if Pos >= Len then
    { Up to the end }
    Pos := Len - 1;
  if Pos < PosToStart then RaiseCompilationException('Malformed expression', PosToStart);
  if not (LastState in [State_Token, State_BraceClosed, State_String]) and not (IsPostfixOperator(Lexemes[Pos].Content) >= 0) then
    RaiseCompilationException('Unexpected <' + GetTypeString(LastState) + '>', Pos);
  Result := Pos;
end;

function THCompiler.ScanForToken_Up(PosToStart: Integer): Integer;
begin
  Result := -1;
end;

function THCompiler.TokenIsNumeric(const AToken: String): Boolean;
var i: Integer;
begin
  Result := True;
  for i := 1 to Length(AToken) do begin
    Result := Result and CharInSet(AToken[i], ['0' .. '9']);
    if not Result then
      Break;
  end;
end;

procedure THCompiler.WriteByte(const AValue: Byte);
begin
  OutputStream.WriteBuffer(AValue, SizeOf(Byte));
end;

procedure THCompiler.WriteCompiledCommand(const Cmd: HCommand; const Pos: THPosition);
begin
  WritePosition(Pos);
  { Only 4/8 (32/64) bytes for one command! }
  OutputStream.WriteBuffer(Cmd, SizeOf(HCommand));
  TotalWrittenCommand := TotalWrittenCommand + 1;
  LastWrittenCommand := Cmd;
end;

procedure THCompiler.WriteFileHeader(const APos: THPosition);
begin
  WriteString(APos.ScopeName);
end;

procedure THCompiler.WriteInteger(const AValue: HInteger);
begin
  OutputStream.WriteBuffer(AValue, SizeOf(HInteger));
end;

procedure THCompiler.WriteIntegerValue(const AValue: HSInteger);
begin
  OutputStream.WriteBuffer(AValue, SizeOf(HSInteger));
end;

procedure THCompiler.WritePosition(const APos: THPosition);
begin
  if TotalWrittenCommand = 0 then
//    WriteString(APos.ScopeName);
    WriteFileHeader(APos);
  WriteInteger(APos.LineNumber);
  WriteInteger(APos.RowNumber);
end;

procedure THCompiler.WriteRealValue(const AValue: HSFloat);
begin
  OutputStream.WriteBuffer(AValue, SizeOf(HSFloat));
end;

procedure THCompiler.WriteString(const Str: String);
var Len: Integer;
    Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(Str);
  Len := Length(Bytes);
  OutputStream.WriteBuffer(Len, 4);
  OutputStream.WriteBuffer(Bytes[0], Len);
//  OutputStream.WriteBuffer(Str[1], Len);
end;

end.
