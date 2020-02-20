unit HLVM.Debug.BytecodeUtilities;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  HLVM.Static.Command,
  { Types in HLVM }
  HLVM.Types.Containers,
  HLVM.Types.Reader,
  { AoTing (useless in many things) }
  HLVM.Types.AoT;

function CommandToString(const Command: Integer): String;
function GetBytecode(const Script: String): THLVM_AoTCompiledBytecode;
function GetBytecodeWithoutAoT(const Script: String): THLVMRawChunk;
function GetSentenceString(const Script: String): String;
function GetSentenceStringNew(const Script: String): String;
//function ReadTextFromFile(const Path: String): String;

function ReadData(const Path: String): String;

implementation

uses
  { Debug libraries }
  HLVM.Debug.Format,
  { Compiling }
  DScript.Compiler,
  { Extended commands }
  HLVM.JIT.Context.CommandExpansion,
  { Linking }
  HLVM.JIT.Context.Linker.New,
  HLVM.JIT.Context.Linker;

function CommandToString(const Command: Integer): String;
begin
  case Command of
    Command_PushVar: Result := 'PushVar';
    Command_ClearStack: Result := 'ClearStack';
    Command_MoveStackPointer: Result := 'MoveStackPointer';
    Command_RestoreStackPointer: Result := 'RestoreStackPointer';
    Command_PushString: Result := 'PushString';
    Command_PushInteger: Result := 'PushInteger';
    Command_PushReal: Result := 'PushFloat';
    Command_Index: Result := 'Index';
    Command_PushNil: Result := 'PushNil';
    Command_PushTrue: Result := 'PushTrue';
    Command_PushFalse: Result := 'PushFalse';

    Command_ArADD: Result := 'Add';
    Command_ArSUB: Result := 'Sub';
    Command_ArMUL: Result := 'Mul';
    Command_ArDIV: Result := 'Div';

    Command_Assign: Result := 'Assign';
    Command_AssignFromMemory: Result := 'AssignFromMemory';
    Command_NewIndex: Result := 'NewIndex';

    Command_LgNOT: Result := 'Not';
    Command_BtNOT: Result := 'BNot';

    Command_UnaryMinus: Result := 'Negate';

    Command_ForSetType: Result := 'ForSetType';
    Command_ForSetStep: Result := 'ForSetStep';
    Command_ForSetStart: Result := 'ForSetStart';
    Command_ForSetFinish: Result := 'ForSetFinish';

    Command_SetBreak: Result := 'SetBreak';
    Command_SetContinue: Result := 'SetContinue';

    Command_JMP: Result := 'JMP';
    Command_JC: Result := 'JC';
    Command_JNC: Result := 'JNC';
    Command_ForSetVar: Result := 'ForSetVar';
    Command_ForCheck: Result := 'ForCheck';

    Command_OpenScope: Result := 'OpenScope';
    Command_CloseScope: Result := 'CloseScope';

    Command_CheckStack: Result := 'CheckStack';
    Command_Call: Result := 'Call';
    Command_SimpleCall: Result := 'SimpleCall';

    Command_BtAND: Result := 'BAnd';
    Command_BtOR: Result := 'BOr';
    Command_BtXOR: Result := 'BXor';

    Command_LgAND: Result := 'And';
    Command_LgOR: Result := 'Or';
    Command_LgXOR: Result := 'Xor';

    Command_Equals: Result := 'Equals';
    Command_GreaterThan: Result := 'GreaterThan';
    Command_GreaterEqual: Result := 'GreaterEqual';

    Command_Internal_PushFunction: Result := 'PushFunction';

    JIT_Command_PushHighestVariable: Result := 'PushHighestVar';
    JIT_Command_DisposeHighestVariable: Result := 'DisposeHighestVar';

    Command_Return: Result := 'Return';
    Command_CreateRunTimeTable: Result := 'CreateRuntimeTable';
    Command_CreateTable: Result := 'CreateTable';

    Command_NoOperation: Result := 'NoOp';
    Command_BreakLoop: Result := 'Break';
    Command_ContinueLoop: Result := 'Continue';

    Command_AssignLocalReference: Result := 'AssignLocalReference';

    JIT_Command_Heading_SetLocalHeapSize: Result := '(HEADER) $SetLocalHeapSize';
    JIT_Command_Heading_SetGlobalHeapSize: Result := '(HEADER) $SetGlobalHeapSize';
    JIT_Command_Heading_CacheGlobal: Result := '(HEADER) $CacheGlobal';
    JIT_Command_Heading_CacheHighest: Result := '(HEADER) $CacheHighest';
  else
    Result := '<unknown operand [' + Command.ToString + ']>';
  end;
end;

function GetBytecode(const Script: String): THLVM_AoTCompiledBytecode;
var OutStream: TMemoryStream;
    Compiler: THCompiler;
    Reader: THLVM_BytecodeReader;
    RawChunk: THLVMRawChunk;
    AoT: THLVM_BytecodeAoTCompiler;
begin
  OutStream := TMemoryStream.Create;
  Compiler := THCompiler.Create;
  Compiler.OutputStream := OutStream;
  Reader := THLVM_BytecodeReader.Create;
  Reader.InputStream := OutStream;
  AoT := THLVM_BytecodeAoTCompiler.Create;
  RawChunk := nil;
  try
    Compiler.LexerObject.SourceText := Script;
    Compiler.LexerObject.ScopeName := '<Delphi_Internal>';
    Compiler.LexerObject.ParseForLexemes;
    Compiler.FixLexemes;
    Compiler.CompileChunk;

    { Stream contains bytecode. }
    OutStream.Position := 0;
    RawChunk := Reader.RetrieveBytecode;

    {with AoT.AoTCompile(RawChunk) do begin
      Functions.Clear;
      Functions.Free;
      Result := CompiledBytecode;
    end;}
    Result := AoT.AoTCompile(RawChunk);
  finally
    try OutStream.Free; except on E: Exception do end;
    try Compiler.Free; except on E: Exception do end;
    try Reader.Free; except on E: Exception do end;
    try AoT.Free; except on E: Exception do end;
    try RawChunk.Free; except on E: Exception do end;
  end;
end;

function GetBytecodeWithoutAoT(const Script: String): THLVMRawChunk;
var OutStream: TMemoryStream;
    Compiler: THCompiler;
    Reader: THLVM_BytecodeReader;
    RawChunk: THLVMRawChunk;
    // AoT: THLVM_BytecodeAoTCompiler;
begin
  OutStream := TMemoryStream.Create;
  Compiler := THCompiler.Create;
  Compiler.OutputStream := OutStream;
  Reader := THLVM_BytecodeReader.Create;
  Reader.InputStream := OutStream;
//  AoT := THLVM_BytecodeAoTCompiler.Create;
  RawChunk := nil;
  try
    Compiler.LexerObject.SourceText := Script;
    Compiler.LexerObject.ScopeName := '<Delphi_Internal>';
    Compiler.LexerObject.ParseForLexemes;
    Compiler.FixLexemes;
    Compiler.CompileChunk;

    { Stream contains bytecode. }
    OutStream.Position := 0;
    RawChunk := Reader.RetrieveBytecode;

    {with AoT.AoTCompile(RawChunk) do begin
      Functions.Clear;
      Functions.Free;
      Result := CompiledBytecode;
    end;}
    // Result := AoT.AoTCompile(RawChunk);
    Result := RawChunk;
  finally
    try OutStream.Free; except on E: Exception do end;
    try Compiler.Free; except on E: Exception do end;
    try Reader.Free; except on E: Exception do end;
//    try AoT.Free; except on E: Exception do end;
    //try RawChunk.Free; except on E: Exception do end;
  end;
end;

function GetFullBytecode(const Script: String): THLVM_AoTCompiledBytecode;
var OutStream: TMemoryStream;
    Compiler: THCompiler;
    Reader: THLVM_BytecodeReader;
    RawChunk: THLVMRawChunk;
    AoT: THLVM_BytecodeAoTCompiler;
begin
  OutStream := TMemoryStream.Create;
  Compiler := THCompiler.Create;
  Compiler.OutputStream := OutStream;
  Reader := THLVM_BytecodeReader.Create;
  Reader.InputStream := OutStream;
  AoT := THLVM_BytecodeAoTCompiler.Create;
  RawChunk := nil;
  try
    Compiler.LexerObject.SourceText := Script;
    Compiler.LexerObject.ScopeName := '<Delphi_Internal>';
    Compiler.LexerObject.ParseForLexemes;
    Compiler.FixLexemes;
    Compiler.CompileChunk;

    { Stream contains bytecode. }
    OutStream.Position := 0;
    RawChunk := Reader.RetrieveBytecode;

    Result := AoT.AoTCompile(RawChunk);
  finally
    try OutStream.Free; except on E: Exception do end;
    try Compiler.Free; except on E: Exception do end;
    try Reader.Free; except on E: Exception do end;
    try AoT.Free; except on E: Exception do end;
    try RawChunk.Free; except on E: Exception do end;
  end;
end;

function GetSentenceString(const Script: String): String;
var Bytecode: THLVM_AoTCompiledBytecode;
    Linked: THLVMBytecodeText;
    Linker: THLVMLinker;
    i: Integer;
begin
  Bytecode := GetBytecode(Script);
  Linker := THLVMLinker.Create;
  try
    try
      Linked := Linker.LinkFullBytecode(Bytecode);
    except
      on E: ELinkerException do begin
        E.Message := FormatResolveCodes(E.Message);
        raise;
      end;
    end;

    // Result := BytecodeTextToString(Linker.Linked);

    Result := '';
    for i := 0 to Length(Linked.Functions) - 1 do begin
      Result := Result + 'func #' + i.ToString + #13#10 + BytecodeTextToString(Linked.Functions[i]) + #13#10#13#10;
    end;

    Result := Result + BytecodeTextToString(Linked.MainChunk);

//    Bytecode.OwnsObjects := True;
//    Bytecode.Free;
//    Bytecode.CompiledBytecode.OwnsObjects := True;
//    Bytecode.CompiledBytecode.Free;
//    Bytecode.Functions.Clear;
//    Bytecode.Functions.Free;

    Bytecode.TerminateAll;
  finally
    Linker.Free;
  end;
end;

function GetSentenceStringNew(const Script: String): String;
var Bytecode: THLVMRawChunk;
    Linked: THLVMBytecodeText;
//    Linker: HLVM.JIT.Context.Linker.New.THLVMLinker;
    i: Integer;
begin
  Bytecode := GetBytecodeWithoutAoT(Script);
//  Linker := THLVMLinker.Create;
//  try
    try
      //Linked := Linker.LinkFullBytecode(Bytecode);
      Linked := HLVM.JIT.Context.Linker.New.THLVMLinker.LinkCompatible(Bytecode);
    except
      on E: ELinkerException do begin
        E.Message := FormatResolveCodes(E.Message);
        raise;
      end;
    end;

    // Result := BytecodeTextToString(Linker.Linked);

    Result := '';
    for i := 0 to Length(Linked.Functions) - 1 do begin
      Result := Result + 'func #' + i.ToString + #13#10 + BytecodeTextToString(Linked.Functions[i]) + #13#10#13#10;
    end;

    Result := Result + BytecodeTextToString(Linked.MainChunk);

//    Bytecode.OwnsObjects := True;
//    Bytecode.Free;
//    Bytecode.CompiledBytecode.OwnsObjects := True;
//    Bytecode.CompiledBytecode.Free;
//    Bytecode.Functions.Clear;
//    Bytecode.Functions.Free;


    try Bytecode.Free; except on E: Exception do end;
//  finally
//    Linker.Free;
//  end;
end;

//function ReadTextFromFile(const Path: String): String;
//begin
//  Result := TFile.ReadAllText(Path, TEncoding.UTF8);
//end;

function ReadData(const Path: String): String;
var Bytes: TBytes;
    S: TFileStream;
begin
  S := TFileStream.Create(Path, fmOpenRead);
  try
    if S.Size > 512 * 1024 * 1024 then
      raise EReadError.Create('File is too big to read (>512MB)');
    SetLength(Bytes, S.Size);
    S.ReadBuffer(Bytes[0], S.Size);
    Result := TEncoding.UTF8.GetString(Bytes);
  finally
    S.Free;
  end;
end;

end.
