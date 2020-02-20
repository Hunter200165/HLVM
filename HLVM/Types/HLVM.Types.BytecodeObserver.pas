unit HLVM.Types.BytecodeObserver;

interface

uses
  HLVM.Static.Command,
  HLVM.Types.AbstractReader,
  HLVM.Types.Ordinal,
//  DScript.Lexer,
  HLVM.Static.Utilities,
  System.SysUtils,
  System.IOUtils,
  System.StrUtils,
  HCmdAPI.CmdIO,
  HCmdAPI.ColorConsole;

type
  THByteCodeObserver_Callback = procedure(const CurrentState: Integer; const Result: String) of Object;

  THByteCodeObserver = class(THAbstractReader)
  const
    TabInc = 4;
  private

    FCallback: THByteCodeObserver_Callback;
    FScopeName: String;
    FTabSize: Integer;
  public
    property Callback: THByteCodeObserver_Callback read FCallback write FCallback;
    property ScopeName: String read FScopeName write FScopeName;

    property TabSize: Integer read FTabSize write FTabSize;

    procedure StartObserving;
  end;

  THByteCodeObserverDefault = class(THByteCodeObserver)
  private
    FAssemblerOutFile: String;
  public
    procedure DefaultCallback(const CurrentState: Integer; const Result: String);
    property AssemblerOutFile: String read FAssemblerOutFile write FAssemblerOutFile;

    procedure PrintPretty(AText: String);

    constructor Create;
  end;

const
  Observer_State_Command = 0;
//  Observer_State_Name = 1;
//  Observer_State_Label = 2;
  Observer_State_Position = 3;
//  Observer_State_Next = 10;

implementation

uses
  System.Types;

{ THByteCodeObserver }

procedure THByteCodeObserver.StartObserving;
var ALen, ACommand: Int64;
    APos: THPosition;
    Resulting: String;
begin
  ALen := InputStream.Size;

  { Reading header }
  ScopeName := ReadString;

  TabSize := 0;

  while (InputStream.Position < ALen) do begin
    APos := ReadPosition;
    Resulting := Format('At ("%s" : %d)', [APos.ScopeName, APos.LineNumber]);
    Callback(Observer_State_Position, Resulting);

    Resulting := '';
    ACommand := ReadCommand;
    case ACommand of
      Command_PushVar: begin
        Resulting := DupeString(' ', TabSize) + 'PushVar ' + (ReadString);
      end;
      Command_ClearStack: begin
        Resulting := DupeString(' ', TabSize) + 'ClrStck';
      end;
      Command_MoveStackPointer: begin
        Resulting := DupeString(' ', TabSize) + 'MovStckPtr';
        TabSize := TabSize + TabInc;
      end;
      Command_RestoreStackPointer: begin
        TabSize := TabSize - TabInc;
        if TabSize < 0 then TabSize := 0;
        Resulting := DupeString(' ', TabSize) + 'RtrStckPtr';
      end;
      Command_PushString: begin
        Resulting := DupeString(' ', TabSize) + 'PushStr ' + HLM_EncloseString(ReadString);
      end;
      Command_Index: begin
        Resulting := DupeString(' ', TabSize) + 'Index';
      end;
      Command_ArADD: Resulting := DupeString(' ', TabSize) + 'Add';
      Command_ArSUB: Resulting := DupeString(' ', TabSize) + 'Sub';
      Command_ArMUL: Resulting := DupeString(' ', TabSize) + 'Mul';
      Command_ArDIV: Resulting := DupeString(' ', TabSize) + 'Div';
      Command_LgAND: Resulting := DupeString(' ', TabSize) + 'And';
      Command_BtAND: Resulting := DupeString(' ', TabSize) + 'Band';
      Command_LgOR:  Resulting := DupeString(' ', TabSize) + 'Or';
      Command_BtOR:  Resulting := DupeString(' ', TabSize) + 'Bor';
      Command_LgXOR: Resulting := DupeString(' ', TabSize) + 'Xor';
      Command_BtXOR: Resulting := DupeString(' ', TabSize) + 'Bxor';
      Command_LgNOT: Resulting := DupeString(' ', TabSize) + 'Not';
      Command_BtNOT: Resulting := DupeString(' ', TabSize) + 'Bnot';
      Command_LABEL: Resulting := DupeString(' ', TabSize) + 'Label :' + IntToStr(ReadInteger) + ':';
      Command_JMP:   Resulting := DupeString(' ', TabSize) + 'Jmp ' + IntToStr(ReadInteger);

      Command_PushInteger:  Resulting := DupeString(' ', TabSize) + 'PushInt ' + IntToStr(ReadIntegerValue);
      Command_PushNil:      Resulting := DupeString(' ', TabSize) + 'PushNil';
      Command_PushTrue:     Resulting := DupeString(' ', TabSize) + 'PushTrue';
      Command_PushFalse:    Resulting := DupeString(' ', TabSize) + 'PushFalse';
      Command_PushReal:     Resulting := DupeString(' ', TabSize) + 'PushReal ' + FloatToStr(ReadRealValue);
      Command_Call:         Resulting := DupeString(' ', TabSize) + 'Call';
      Command_SimpleCall:   Resulting := DupeString(' ', TabSize) + 'SimpleCall';

      Command_JC:   Resulting := DupeString(' ', TabSize) + 'JC ' + IntToStr(ReadInteger);
      Command_JNC:  Resulting := DupeString(' ', TabSize) + 'JNC ' + IntToStr(ReadInteger);

      Command_CheckStack: Resulting := DupeString(' ', TabSize) + 'ChckStck';

      Command_GetLength:    Resulting := DupeString(' ', TabSize) + 'GetLength';
      Command_Equals:       Resulting := DupeString(' ', TabSize) + 'IsEqual';
      Command_GreaterThan:  Resulting := DupeString(' ', TabSize) + 'IsGreater';
      Command_GreaterEqual: Resulting := DupeString(' ', TabSize) + 'IsGreaterEqual';

      Command_PushLongArguments:  Resulting := DupeString(' ', TabSize) + 'PushArgs';
      Command_PushShortArguments: Resulting := DupeString(' ', TabSize) + 'PushSingleArg';
      Command_UnaryMinus:         Resulting := DupeString(' ', TabSize) + 'Negate';

      Command_CreateTable:        Resulting := DupeString(' ', TabSize) + 'CreateTable';
      Command_CreateRunTimeTable: Resulting := DupeString(' ', TabSize) + 'CreateRunTimeTable';

      Command_PushLocal:  Resulting := DupeString(' ', TabSize) + 'PushLocal ' + (ReadString);
      Command_Assign:     Resulting := DupeString(' ', TabSize) + 'Assign';

      Command_CallAsync:  Resulting := DupeString(' ', TabSize) + 'CallAsync';

      Command_BreakLoop:    Resulting := DupeString(' ', TabSize) + 'BreakLoop';
      Command_ContinueLoop: Resulting := DupeString(' ', TabSize) + 'ContinueLoop';

      Command_SetBreak:    Resulting := DupeString(' ', TabSize) + 'SetBreak ' + IntToStr(ReadInteger);
      Command_SetContinue: Resulting := DupeString(' ', TabSize) + 'SetContinue ' + IntToStr(ReadInteger);

      Command_DoInterpolation: Resulting := DupeString(' ', TabSize) + 'DoInterpolation';

      Command_MarkAsLoopVar:   Resulting := DupeString(' ', TabSize) + 'MarkAsLoopVar';
      Command_StrongAssign:    Resulting := DupeString(' ', TabSize) + 'StrongAssign';

      Command_CheckStackInvisible: Resulting := DupeString(' ', TabSize) + 'ChckStckInvisible';

      Command_RestrictType: Resulting := DupeString(' ', TabSize) + 'RestrictType ' + IntToStr(ReadInteger);

      Command_Duplicate:    Resulting := DupeString(' ', TabSize) + 'DuplicateTop';

      Command_CreateFunction: Resulting := DupeString(' ', TabSize) + 'CreateFunction';
      Command_AssignPointer:  Resulting := DupeString(' ', TabSize) + 'AssignPointer';
      Command_ConsumeArguments: Resulting := DupeString(' ', TabSize) + 'ConsumeArguments';

      Command_Return: Resulting := DupeString(' ', TabSize) + 'Return';

      Command_IsIn: Resulting := DupeString(' ', TabSize) + 'IsIn';

      Command_OpenScope: begin
        Resulting := DupeString(' ', TabSize) + 'OpenScope';
        TabSize := TabSize + TabInc;
      end;
      Command_CloseScope: begin
        TabSize := TabSize - TabInc;
        if TabSize < 0 then TabSize := 0;
        Resulting := DupeString(' ', TabSize) + 'CloseScope';
      end;

      Command_NewIndex: Resulting := DupeString(' ', TabSize) + 'NewIndex';
      Command_ForSetStart: Resulting := DupeString(' ', TabSize) + 'ForSetStart';
      Command_ForSetFinish: Resulting := DupeString(' ', TabSize) + 'ForSetFinish';
      Command_ForSetType: Resulting := DupeString(' ', TabSize) + 'ForSetType ' + IntToStr(ReadInteger);
      Command_ForCheck: Resulting := DupeString(' ', TabSize) + 'ForCheck';
      Command_ForSetVar: Resulting := DupeString(' ', TabSize) + 'ForSetVar ' + ReadString;
      Command_ForSetStep: Resulting := DupeString(' ', TabSize) + 'ForSetStep ' + IntToStr(ReadInteger);

      Command_AssignLocalReference: Resulting := DupeString(' ', TabSize) + 'AssignLocalReference ' + ReadString;

      Command_Ternary: Resulting := DupeString(' ', TabSize) + 'TernarySelect';
    else
      raise Exception.Create('Unknown operation code: ' + IntToStr(ACommand));
    end;
    Resulting := Resulting + ';';
    Callback(Observer_State_Command, Resulting);
  end;
end;

{ THByteCodeObserverDefault }

constructor THByteCodeObserverDefault.Create;
begin
  Self.Callback := DefaultCallback;

  AssemblerOutFile := 'DScriptAssembler.dasm';
  TFile.WriteAllText(AssemblerOutFile, '');
end;

procedure THByteCodeObserverDefault.DefaultCallback(const CurrentState: Integer; const Result: String);
begin
  case CurrentState of
//    Observer_State_Position: begin
//      Writeln('--[[ ' + Result + ' ]]');
//    end;
    Observer_State_Command: begin
//      Writeln(Result);
      PrintPretty(Result);
      if not (AssemblerOutFile = '') then
        TFile.AppendAllText(AssemblerOutFile, Result + #13#10, TEncoding.UTF8);
    end;
  end;
end;

procedure THByteCodeObserverDefault.PrintPretty(AText: String);
const
  AColSpecial = '0111';
  AColDefault = '1101';
var Arr: TArray<String>;
    Count, i: Integer;
begin
  AText := Copy(AText, 1, Length(AText) - 1);
  Arr := Trim(AText).Split([' ']);
  Count := Length(AText) - Length(TrimLeft(AText));
//  if
//    (LowerCase(Arr[0]) = 'movstckptr') or
//    (LowerCase(Arr[0]) = 'rtrstckptr') then begin
//    CmdIO.SetColour_RGBH(AColSpecial);
//    CmdIO.Write(Arr[0]);
//    CmdIO.SetDefaultColour;
//  end
//  else begin
//    CmdIO.Write(DupeString(' ', Count));
    for i := 1 to (Count div TabInc) do begin
      WriteColour('║', '0001');
      Write(DupeString(' ', TabInc - 1));
    end;

    if (LowerCase(Arr[0]) = 'openscope') or (LowerCase(Arr[0]) = 'movstckptr') then
//      Arr[0] := '╔' + Arr[0]
      WriteColour('╔', '0001')
    else if (LowerCase(Arr[0]) = 'closescope') or (LowerCase(Arr[0]) = 'rtrstckptr') then
//      Arr[0] := '╚' + Arr[0];
      WriteColour('╚', '0001');

    CmdIO.SetColour_RGBH(AColDefault);
    CmdIO.Write(Arr[0]);
    CmdIO.SetDefaultColour;
//  end;
  if Length(Arr) > 1 then begin
    CmdIO.Write(' ');
    if (LowerCase(Arr[1]) = 'true') or (LowerCase(Arr[1]) = 'false')  then begin
      WriteColour(Arr[1], '1001');
    end
    else if (CharInSet(Arr[1][1], ['0' .. '9'])) then begin
      // Number
      WriteColour(Arr[1], '1011');
    end
    else if (Arr[1][1] = '''') then begin
      WriteColour(Arr[1], '0111');
    end
    else if (Arr[1][1] = ':') then begin
      WriteColour(Arr[1], '0101');
    end
    else
      WriteColour(Arr[1], '1001');
  end;
  WriteLnColour(';', '1110');
end;

end.
