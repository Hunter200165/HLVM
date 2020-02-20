unit HLVM.Debug.Format;

interface

uses
  System.SysUtils,
  System.StrUtils,
  HLVM.Static.Command,
  HLVM.JIT.Context.CommandExpansion,
  HLVM.Debug.BytecodeUtilities,
  HLVM.JIT.Context.Linker,
  HLVM.JIT.Context.Storage.Commons;

function FormatResolveCodes(const S: String): String;
function BytecodeTextToString(const Bytecodes: THLVMBytecodeSentence): String;

implementation

const
  SpacesAdd = 2;

function FormatResolveCodes(const S: String): String;
var Posi, PosS, Counter, TempCommand: Integer;
    Service, Temp: String;
begin
  Result := S;
  Service := '[ ';
  Posi := Pos('$', S);
  Counter := 0;
  while Posi > 0 do begin
    PosS := Pos(' ', Result, Posi + 1);
    if PosS <= 0 then
      PosS := Length(Result) + 1;
    { 3; 1; 3 - 1 => 2 - 1 => 1 }
    Temp := Copy(Result, Posi + 1, PosS - Posi - 1);
    if TryStrToInt(Temp, TempCommand) then begin
      Counter := Counter + 1;
      Service := Service + Counter.ToString + ': ' + CommandToString(TempCommand) + '; ';
    end;
    Posi := Pos('$', Result, PosS + 1);
  end;
  Service := Service + ']';
  Result := Result + ' ' + Service;
end;

function BytecodeWordToString(const ByteWord: HLVM_LinkerWord): String;
begin
  {Result := Format('\c[1001][%d; *%d]\r \c[1101]%s\r (cont : s => \c[0111]"%s"\r; i => \c[1011]%d\r; f => \c[1011]%f\r)', [
    ByteWord.RegisterIndex,
    ByteWord.PointerRegisterIndex,
    CommandToString(ByteWord.CommandID),
    ByteWord.StringStorage,
    ByteWord.IntegerStorage,
    ByteWord.FloatStorage
  ]);}
  Result := Format('\c[1001][%d; *%d]\r \c[1101]%s\r', [
    ByteWord.RegisterIndex,
    ByteWord.PointerRegisterIndex,
    CommandToString(ByteWord.CommandID)
  ]);

  case ByteWord.CommandID of
    Command_PushVar: begin
      case ByteWord.AdditionalData and 1 of
        1: Result := Result + ' \c[1001]FORCE\r';
      end;
      case ByteWord.AdditionalData and not 1 of
        ScoperFlag_Var_PushKnownLocal:
          Result := Result + ' Local \c[1110]&\r' + ByteWord.AdditionalPlusData.ToString + ' \c[1110]<\c[0111]''' + ByteWord.StringStorage + '''\c[1110]>\r';
        ScoperFlag_Var_PushKnownGlobal:
          Result := Result + ' Global \c[1110]&\r' + ByteWord.AdditionalPlusData.ToString + ' \c[1110]<\c[0111]''' + ByteWord.StringStorage + '''\c[1110]>\r';
        ScoperFlag_Var_PushRawGlobal:
          Result := Result + ' Global? \c[1110]<\c[0111]''' + ByteWord.StringStorage + '''\c[1110]>\r ';
        ScoperFlag_Var_PushHighest:
          Result := Result + ' Highest \c[1110]&\r' + ByteWord.AdditionalPlusData.ToString + ' \c[1110]<\c[0111]''' + ByteWord.StringStorage + '''\c[1110]>\r';
      else
        Result := Result + ' \c[1001]FAULT=>INCORRECT_DATA[' + ByteWord.AdditionalData.ToString + ']\r'
      end;
    end;
    Command_JMP, Command_JC, Command_JNC:
      begin
        Result := Result + ' \c[1110]&\r' + ByteWord.IntegerStorage.ToString;
      end;
    Command_BreakLoop, Command_ContinueLoop:
      begin
        Result := Result + ' \c[1110]&\r' + ByteWord.AdditionalPlusData.ToString;
      end;
  else
    if not (ByteWord.IntegerStorage = 0) then
      Result := Result + ' \c[1110]::\r int \c[1110]=>\r \c[1011]' + ByteWord.IntegerStorage.ToString + '\r';
    if not (ByteWord.FloatStorage = 0) then
      Result := Result + ' \c[1110]::\r float \c[1110]=>\r \c[1011]' + TCommons.FloatToString(ByteWord.FloatStorage) + '\r';
    if not (ByteWord.StringStorage = '') then
      Result := Result + ' \c[1110]::\r string \c[1110]=>\r \c[0111]''' + ByteWord.StringStorage + '''\r';
    if not (ByteWord.AdditionalData = 0) then
      Result := Result + ' \c[1110]:!\r data \c[1110]=>\r \c[1011]' + ByteWord.AdditionalData.ToString + '\r';
    if not (ByteWord.AdditionalPlusData = 0) then
      Result := Result + ' \c[1110]:!\r data+ \c[1110]=>\r \c[1011]' + ByteWord.AdditionalPlusData.ToString + '\r';
  end;
end;

function BytecodeRecWordToString(const Bytecodes: THLVMBytecodeSentence; const Pos: Integer; const Spaces: Integer = 0; const NeedLabel: Boolean = False): String;
var i: Integer;
    LWord: HLVM_LinkerWord;
begin
  LWord := Bytecodes.InternalStorage[Pos];
  Result := DupeString(' ', Spaces) + BytecodeWordToString(LWord);
  if NeedLabel and (LWord.LabeledIndex >= 0) then
    Result := Result + ' #' + LWord.LabeledIndex.ToString;
  if LWord.AbstractOperands.Count > 0 then begin
    Result := Result + ' ->' + #13#10;
    for i := 0 to LWord.AbstractOperands.Count - 1 do begin
      if not (i = 0) then
        Result := Result + #13#10;
      Result := Result + BytecodeRecWordToString(Bytecodes, LWord.AbstractOperands[i], Spaces + SpacesAdd);
    end;
  end
  else
    Result := Result + ';';
end;

function BytecodeTextToString(const Bytecodes: THLVMBytecodeSentence): String;
var i: Integer;
begin
  Result := '';
  for i := 0 to Bytecodes.ActualStorage.Count - 1 do begin
    if not (i = 0) then
      Result := Result + #13#10;
    Result := Result + BytecodeRecWordToString(Bytecodes, Bytecodes.ActualStorage[i], 0, True);
  end;
end;

end.
