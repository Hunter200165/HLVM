unit HLVM.Debug.Runnable;

interface

uses
  System.Diagnostics,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Winapi.Windows,
  { Old modules }
  HLVM.Runtime.Machine,
  HLVM.Runtime.Types,
  HLVM.Memory.Manager,
  HLVM.Types.AoT,
  HLVM.Types.Table,
  HLVM.Types.Containers,
  { New modules }
  DScript.Compiler,
  HLVM.Types.BytecodeObserver,
  HLVM.JIT.Context.Linker,
  HLVM.JIT.Context.Linker.New,
  HLVM.JIT.Execution.Provider,
  HLVM.JIT.Standards,
  HLVM.JIT.Types,
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.Storage.Table,
  HLVM.Debug.BytecodeUtilities,
  HLVM.Debug.Format,
  { Libs }
  HCmdAPI.CmdFormatting,
  HCmdAPI.CmdIO;

const
  UseDebugModuleRun = True;

type
  THLVMDebugEntryModule = class
  public type
    { Test types }
    TTestRec = record A, B, C, D: Int64; end;
  public
    { Test functions }
    class procedure BlankFunction(const A: Pointer; const [Ref] B: TTestRec); static;
  public
    { Test functions }
    class procedure TestFunctionCall; static;
    class procedure TestAdd; static;
    class procedure TestForTable; static;
    class procedure Tests; static;
    class procedure TestPointers; static;
    class procedure TestTable; static;
    class procedure TestNewTable; static;
    class procedure TestSync; static;
    { Working functions }
    class procedure DoObservation(const AText: String);
    class function KadJIITRun(const Inlined: Boolean = False): Boolean;
    class function JITRun: Boolean;
    class function JITRunNew: Boolean;
    class procedure REPL;
  public
    { Main procedure }
    class procedure Run;
  end;

implementation

{$I ./Version/include_version.txt}

{ THLVMDebugEntryModule }

class procedure THLVMDebugEntryModule.BlankFunction(const A: Pointer; const [Ref] B: TTestRec);
begin
  { Nothing is here! }
end;

class procedure THLVMDebugEntryModule.DoObservation(const AText: String);
var Compiler: THCompiler;
    OutStream: TMemoryStream;
    Observer: THByteCodeObserverDefault;
begin
//  if States.Count < 1 then States.Add(TState.Create(Self));
//  if States[0].SubStates.Count < 1 then
//    States[0].SubStates.Add(TSubState.Create(States[0]))
//  else
//    States[0].SubStates[0].PerformReset;
  Compiler := THCompiler.Create;
  OutStream := TMemoryStream.Create;
  Observer := THByteCodeObserverDefault.Create;
  try
    try
      OutStream.Clear;
      OutStream.Position := 0;
      Compiler.OutputStream := OutStream;
      Compiler.LexerObject.SourceText := AText;
      Compiler.LexerObject.ScopeName := 'StdIn';
      Compiler.LexerObject.ParseForLexemes;
      Compiler.FixLexemes;
      Compiler.CompileChunk;
      Observer.InputStream := OutStream;
      OutStream.Position := 0;
      Observer.StartObserving;
//    ExecuteStreamBytecode(OutStream);
    except
      on E: Exception do begin
        Writeln('Compilation error: ' + E.Message);
        raise;
      end;
    end;
  finally
    try Compiler.Free; except on E: Exception do end;
    try OutStream.Free; except on E: Exception do end;
    try Observer.Free; except on E: Exception do end;
  end;
end;

class function THLVMDebugEntryModule.JITRun: Boolean;
var Exec: TExecutionSubstate;
    Glob: TGlobalEnvironment;
    Linker: HLVM.JIT.Context.Linker.THLVMLinker;
    Script: String;
    Watch: TStopwatch;
    ToLink: THLVM_AoTCompiledBytecode;
    Linked: THLVMBytecodeText;
begin
  //Writeln('Started...');
  // Script := 'for i := 1 to 100 do print(''Hello world!'', 34);';
  Write('jit> ');
  ReadLn(Script);
  if Script = 'exit!' then begin
    Result := True;
    Exit;
  end
  else
    Result := False;

  if Script.StartsWith('ext!', True) then
    Script := ReadData(Trim(Copy(Script, 5)));

  Exec := TExecutionSubstate.Create;
  try
    try
      Glob.Nullify;
      Linker := HLVM.JIT.Context.Linker.THLVMLinker.Create;
      try
        InsertStandards(@Glob);

        WriteLn('Getting bytecode...');
        ToLink := GetBytecode(Script);
        Exec.Nullify;
        Exec.GlobalEnvironment := @Glob;
        WriteLn('Linking...');
        Linked := Linker.LinkFullBytecode(ToLink);
        ToLink.TerminateAll;

        Exec.ExecutionStorage := TJITCompiler.JITCompileBytecode(Linked.MainChunk);
        // Exec.Link;
        //Writeln('Execution...');
        Watch := TStopwatch.StartNew;
        Writeln('Executing...');
        Exec.Execute;
        Watch.Stop;
        Writeln('JIT time : ', Watch.ElapsedMilliseconds);
      finally
        try Glob.FreeAllContents; except on E: Exception do end;
        // try Linker.BytecodeArray.OwnsObjects := True; Linker.BytecodeArray.Free; except on E: Exception do end;
        Linker.Free;
      end;
    finally
      Exec.Free;
    end;
  except on E: Exception do
    begin
      E.Message := FormatResolveCodes(E.Message);
      raise;
    end;
  end;
end;

class function THLVMDebugEntryModule.JITRunNew: Boolean;
var Exec: TExecutionSubstate;
    Glob: TGlobalEnvironment;
    Linker: THLVMLinker;
    Script: String;
    Watch: TStopwatch;
    ToLink: THLVMRawChunk;
    Linked: THLVMBytecodeText;
begin
  //Writeln('Started...');
  // Script := 'for i := 1 to 100 do print(''Hello world!'', 34);';
  Write('new-jit> ');
  ReadLn(Script);
  if Script = 'exit!' then begin
    Result := True;
    Exit;
  end
  else
    Result := False;

  if Script.StartsWith('ext!', True) then
    Script := ReadData(Trim(Copy(Script, 5)));

  Exec := TExecutionSubstate.Create;
  try
    try
      Glob.Nullify;
      // Linker := HLVM.JIT.Context.Linker.THLVMLinker.Create;
      try
        InsertStandards(@Glob);

//        WriteLn('Getting bytecode...');
//        ToLink := GetBytecode(Script);
        ToLink := GetBytecodeWithoutAoT(Script);
        Exec.Nullify;
        Exec.GlobalEnvironment := @Glob;
        // Linked := Linker.LinkFullBytecode(ToLink);
//        WriteLn('Linking...');
        Linked := THLVMLinker.LinkCompatible(ToLink);
//        ToLink.TerminateAll;
        ToLink.OwnsObjects := True;
        ToLink.Free;

//        WriteLn('JIT compiling...');
        Exec.ExecutionStorage := TJITCompiler.JITCompileBytecode(Linked.MainChunk);
        // Exec.Link;
        //Writeln('Execution...');
        Watch := TStopwatch.StartNew;
//        WriteLn('Executing...');
        Exec.Execute;
        Watch.Stop;
        Writeln('JIT time : ', Watch.ElapsedMilliseconds);
      finally
        try Glob.FreeAllContents; except on E: Exception do end;
        // try Linker.BytecodeArray.OwnsObjects := True; Linker.BytecodeArray.Free; except on E: Exception do end;
        // Linker.Free;
      end;
    finally
      Exec.Free;
    end;
  except on E: Exception do
    begin
      E.Message := FormatResolveCodes(E.Message);
      raise;
    end;
  end;
end;

class function THLVMDebugEntryModule.KadJIITRun(const Inlined: Boolean): Boolean;
var Exec: TKadJIITSubState;
    Glob: TGlobalEnvironment;
    Linker: HLVM.JIT.Context.Linker.THLVMLinker;
    Script: String;
    Watch: TStopwatch;
    ToLink: THLVM_AoTCompiledBytecode;
    Linked: THLVMBytecodeText;
begin
  //Writeln('Started...');
  // Script := 'for i := 1 to 100 do print(''Hello world!'', 34);';
  Write('kadjiit> ');
  ReadLn(Script);
  if Script = 'exit!' then begin
    Result := True;
    Exit;
  end
  else
    Result := False;
  Exec := TKadJIITSubState.Create;
  try
    try
      Glob.Nullify;
      Linker := HLVM.JIT.Context.Linker.THLVMLinker.Create;
      try
        InsertStandards(@Glob);

        ToLink := GetBytecode(Script);
        Exec.Nullify;
        Exec.GlobalEnv := @Glob;
//        Linker.LinkBytecode;

        Linked := Linker.LinkFullBytecode(ToLink);
        ToLink.CompiledBytecode.OwnsObjects := True;
        ToLink.CompiledBytecode.Free;
        ToLink.Functions.Clear;
        ToLink.Functions.Free;

        Exec.ExecutionContainer := TKadJIITCompiler.JITCompileBytecode(Linked.MainChunk, True);
        // Exec.Link;
        //Writeln('Execution...');
        Watch := TStopwatch.StartNew;
        if Inlined then
          Exec.ExecuteInline
        else
          Exec.ExecuteFunctional;
        Watch.Stop;
        Writeln('KadJIIT time : ', Watch.ElapsedMilliseconds);
      finally
        try Glob.FreeAllContents; except on E: Exception do end;
//        try Linker.BytecodeArray.OwnsObjects := True; Linker.BytecodeArray.Free; except on E: Exception do end;
        Linker.Free;
      end;
    finally
      Exec.Free;
    end;
  except on E: Exception do
    begin
      E.Message := FormatResolveCodes(E.Message);
      raise;
    end;
  end;
end;

class procedure THLVMDebugEntryModule.REPL;
const N = 1000000;
var Machine: TMachine;
    Script, Temp: String;
    Timer: TStopwatch;
    Timing: Boolean;
begin
  Machine := TMachine.Create;
  Machine.RegisterBasics;
  Machine.RegisterMetas;

  WriteLn('Version of HLVM: ', Version);
  Machine.ExecuteTextChunk('print("Welcome to the DScript over HLVM interpreter.");');

  try
    //if not SetConsoleCP(CP_UTF8) then
      //WriteLn('Cannot change codepage to UTF8!');
//    SetTextCodePage(Input, CP_UTF8);
//    SetTextCodePage(Output, CP_UTF8);

    while True do begin
      try
        Timing := False;
        Write('> ');

        Readln(Script);
        if LowerCase(Script) = 'exit!' then Break;

        if Script.StartsWith('multln!', True) then begin
          Script := Copy(Script, 8);
          Temp := '';

          repeat
            Script := Script + Temp;
            Write('>> ');
            ReadLn(Temp);
          until (Temp = 'end!');
        end;


        if Script.StartsWith('ext!', True) then begin
          Script := ReadData(Trim(Copy(Script, 5)));
        end;

        if Script.StartsWith('observe!', True) then begin
          DoObservation(Copy(Script, 9));
          Continue;
        end;
        if Script.StartsWith('sent!', True) then begin
          try
            CmdFormatter.IO.DefaultColourForeground := 15;
            CmdFormatter.ProcessString(GetSentenceString(Copy(Script, 6)));
            Writeln;
          finally
            CmdFormatter.IO.DefaultColourForeground := 7;
            CmdIO.SetDefaultColour;
          end;
          Continue;
        end;
        if Script.StartsWith('sent-new!', True) then begin
          try
            CmdFormatter.IO.DefaultColourForeground := 15;
            CmdFormatter.ProcessString(GetSentenceStringNew(Copy(Script, 10)));
            Writeln;
          finally
            CmdFormatter.IO.DefaultColourForeground := 7;
            CmdIO.SetDefaultColour;
          end;
          Continue;
        end;
        if Script.StartsWith('jit!', True) then begin
          while not JITRun do;
          Continue;
        end;
        if Script.StartsWith('jit-new!', True) then begin
          while not JITRunNew do ;
          Continue;
        end;
        if Script.StartsWith('kadjiit!', True) then begin
          while not KadJIITRun do ;
          Continue;
        end;
        if Script.StartsWith('inline!', True) then begin
          while not KadJIITRun(True) do ;
          Continue;
        end;
        if Script.StartsWith('{$instructions +}') then begin
          Machine.AShowInstructions := True;
          Continue;
        end
        else if Script.StartsWith('{$instructions -}') then begin
          Machine.AShowInstructions := False;
          Continue;
        end;

        if Script.StartsWith('multln!') then begin
          Script := '';
          Temp := '';
          repeat
            Write('#> ');
            Readln(Temp);
            case Trim(Lowercase(Temp)) = 'end!' of
              False: Script := Script + Temp + #13#10;
              True: Break;
            end;
          until False;
        end;
        if LowerCase(Copy(Script, 1, 5)) = 'time!' then begin
          Timing := True;
          Script := Copy(Script, 6);
          Timer := TStopwatch.StartNew;
        end;
        Machine.ExecuteTextChunk(Script);
        if Timing then begin
          Timer.Stop;
          Writeln('Time is ', Timer.ElapsedMilliseconds, 'ms');
        end;
      except
        on E: Exception do begin
          Writeln(E.Message);
        end;
      end;
    end;
  finally
    Machine.Free;
  end;
end;

class procedure THLVMDebugEntryModule.Run;
begin
  { Do there whetever you want to call or make! }
//  TestFunctionCall;
  REPL;
end;

class procedure THLVMDebugEntryModule.TestAdd;
const N = 10000000;
var A: TMemoryManager;
    i: Integer;
    Watch: TStopwatch;
    V: TVariableShell;
begin
  Watch := TStopwatch.StartNew;
  A := A.Create;
  for i := 1 to N do begin
    //ID := PRunTimeVariable_Integer(A.AllocateNewInteger(0).Content).ID;
    //A.DisposeInteger(ID);
    New(PHInteger(V.Content));
    Dispose(V.Content);
  end;
  Watch.Stop;
  Writeln(Watch.ElapsedMilliseconds);
end;

class procedure THLVMDebugEntryModule.TestForTable;
const N = 1000000;
var Tabl: TTable;
    i: Integer;
    Watch: TStopwatch;
    Env: PEnvironment;
    AVar: TRunTimeVariable_Table;
begin
  Tabl := TTable.Create;
  AVar.Content := Tabl;
  AVar.RefCounter := 100;
  Watch := TStopwatch.StartNew;
  for i := 1 to N do begin
    Env := PEnvironment(TEnvironment.Create(PHTable(@AVar), nil));
    Env.DecrementRefCounter;
  end;
  Tabl.Free;
  Watch.Stop;
  Writeln('Tables are ', Watch.ElapsedMilliseconds);
end;

class procedure THLVMDebugEntryModule.TestFunctionCall;
const N = 100000000;
var Watch: TStopwatch;
    i: Integer;
    Rec: TTestRec;
begin
  Watch := TStopwatch.StartNew;
  for i := 1 to N do
    THLVMDebugEntryModule.BlankFunction(nil, Rec);
  Watch.Stop;
  Writeln('Elapsed on calls: ', Watch.ElapsedMilliseconds);
end;

class procedure THLVMDebugEntryModule.TestNewTable;
var Table: THLVMTable;
    Str, Str1: String;
    Ptr: PHLVM_Variable;
begin
  Table.Nullify;

  while True do begin
    Write('#! ');
    Readln(Str);
    if Str = 'exit' then
      Break;
    if Str = 'get' then begin
      Write('>> ');
      Readln(Str1);
      Ptr := Table.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(Str1));
      if Ptr = nil then
        Writeln('nil')
      else
        Writeln(Ptr.Stringy);
    end
    else if Str = 'set' then begin
      Write('>> ');
      Readln(Str1);
      Write('>> ');
      Readln(Str);
      with Table.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(Str1))^ do begin
        Typed := HLVM_Type_String;
        Stringy := Str;
      end;
    end
    else
      Writeln('Unknown command?');
  end;

  Table.FreeAllContents;
end;


class procedure THLVMDebugEntryModule.TestPointers;
var A: array of Integer;
begin
  SetLength(A, 1);
  Writeln(Integer(@A[0]));
  SetLength(A, 250000000);
  Writeln(Integer(@A[0]));
end;

class procedure THLVMDebugEntryModule.Tests;
var Stop: TStopwatch;
    i: Integer;
    A: array of Integer;
begin
  Stop := TStopwatch.StartNew;
  SetLength(A, 1);
  for i := 1 to 10000000 do
    A[0] := i;
  Stop.Stop;
  Writeln('Hmm? ', Stop.ElapsedMilliseconds);
end;

class procedure THLVMDebugEntryModule.TestSync;
begin

end;

class procedure THLVMDebugEntryModule.TestTable;
const N = 100000;
var Table: TFunctionalTable;
    Table1: TTablePrototype;
    Str: TRunTimeVariable_String;
    Val: TVariableShell;
    Stop: TStopwatch;
    i: Integer;
begin
  Table := TFunctionalTable.Create(0);
  Str := TRunTimeVariable_String.Create('Hello?');
  Val := TVariableShell.Create(PHString(@Str));
  if Table.GetItem(Val) = nil then
    Writeln('Praise the god')
  else
    raise Exception.Create('Demon is here.');
  Stop := TStopwatch.StartNew;
  for i := 1 to N do begin
    Table.AddOrSetItem(Val, Val);
    PHString(Val.Content).Content := 'Hello' + IntToStr(i);
  end;
  Stop.Stop;
  Writeln('My table write is ', Stop.ElapsedMilliseconds);
  Stop := TStopwatch.StartNew;
  for i := 1 to N * 10 do begin
    Table.GetItem(Val);
    PHString(Val.Content).Content := 'Hello' + IntToStr(i);
  end;
  Stop.Stop;
  Writeln('My table read is ', Stop.ElapsedMilliseconds);
  Table1 := TTablePrototype.Create();
  try
    Stop := TStopwatch.StartNew;
    for i := 1 to N do begin
      Table1.AddOrSetValue(Val, Val);
      PHString(Val.Content).Content := 'Hello' + IntToStr(i);
    end;
    Stop.Stop;
    Writeln('Table write is ', Stop.ElapsedMilliseconds);
    Stop := TStopwatch.StartNew;
    for i := 1 to N * 10 do begin
      Table1.ContainsKey(Val);
      PHString(Val.Content).Content := 'Hello' + IntToStr(i);
    end;
    Stop.Stop;
    Writeln('Table read is ', Stop.ElapsedMilliseconds);
  finally
    Table1.Free;
  end;
//  Writeln(PHString(Table.GetItem(Val).Content).Content);
end;

end.
