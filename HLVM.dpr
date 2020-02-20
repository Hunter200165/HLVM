program HLVM;

{$APPTYPE CONSOLE}

{$R *.res}

{$T +}

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.IOUtils,
  Winapi.Windows,
  uTExtendedX87 in 'HLVM\PartyLibraries\uTExtendedX87.pas',
  HLVM.RunTime.Types in 'HLVM\Runtime\HLVM.RunTime.Types.pas',
  HLVM.Memory.Manager in 'HLVM\Memory\HLVM.Memory.Manager.pas',
  HLVM.Memory.Space in 'HLVM\Memory\HLVM.Memory.Space.pas',
  HLVM.Memory.Storage in 'HLVM\Memory\HLVM.Memory.Storage.pas',
  HLVM.Types.Utilities in 'HLVM\Types\HLVM.Types.Utilities.pas',
  HLVM.Runtime.Hashes in 'HLVM\Runtime\HLVM.Runtime.Hashes.pas',
  HLVM.Runtime.Machine in 'HLVM\Runtime\HLVM.Runtime.Machine.pas',
  HLVM.Generics.Collections in 'HLVM\Generics\HLVM.Generics.Collections.pas',
  HLVM.Types.Stack in 'HLVM\Types\HLVM.Types.Stack.pas',
  HLVM.Static.Command in 'HLVM\Static\HLVM.Static.Command.pas',
  HLVM.Types.Ordinal in 'HLVM\Types\HLVM.Types.Ordinal.pas',
  HLVM.Types.AbstractReader in 'HLVM\Types\HLVM.Types.AbstractReader.pas',
  HLVM.Types.Reader in 'HLVM\Types\HLVM.Types.Reader.pas',
  HLVM.Types.Containers in 'HLVM\Types\HLVM.Types.Containers.pas',
  HLVM.Types.AoT in 'HLVM\Types\HLVM.Types.AoT.pas',
  DScript.Compiler in 'HLVM\DScript\DScript.Compiler.pas',
  DScript.Lexer in 'HLVM\DScript\DScript.Lexer.pas',
  HLVM.Static.Lexer.Miscs in 'HLVM\Static\HLVM.Static.Lexer.Miscs.pas',
  HLVM.Generics.HArray in 'HLVM\Generics\HLVM.Generics.HArray.pas',
  System.Diagnostics,
  System.StrUtils,
  System.Classes,
  System.Hash,
  HLVM.Types.BytecodeObserver in 'HLVM\Types\HLVM.Types.BytecodeObserver.pas',
  HLVM.Static.Utilities in 'HLVM\Static\HLVM.Static.Utilities.pas',
  HCmdAPI.CmdFormatting in 'HLVM\Static\Libs\HCmdAPI\HCmdAPI.CmdFormatting.pas',
  HCmdAPI.CmdIO in 'HLVM\Static\Libs\HCmdAPI\HCmdAPI.CmdIO.pas',
  HCmdAPI.CmdProgress in 'HLVM\Static\Libs\HCmdAPI\HCmdAPI.CmdProgress.pas',
  HCmdAPI.ColorConsole in 'HLVM\Static\Libs\HCmdAPI\HCmdAPI.ColorConsole.pas',
  HCmdAPI.Commons in 'HLVM\Static\Libs\HCmdAPI\HCmdAPI.Commons.pas',
  HCmdAPI.IO in 'HLVM\Static\Libs\HCmdAPI\HCmdAPI.IO.pas',
  HLVM.Types.Table in 'HLVM\Types\HLVM.Types.Table.pas',
  HLVM.Runtime.RuntimeMonster in 'HLVM\Runtime\HLVM.Runtime.RuntimeMonster.pas',
  System.Rtti,
  HLVM.JIT.Context in 'HLVM\Machine\HLVM.JIT.Context.pas',
  HLVM.JIT.Context.Linker in 'HLVM\Machine\HLVM.JIT.Context.Linker.pas',
  HLVM.JIT.Compiler in 'HLVM\Machine\HLVM.JIT.Compiler.pas',
  HLVM.JIT.Context.Storage in 'HLVM\Machine\HLVM.JIT.Context.Storage.pas',
  HLVM.JIT.Context.Storage.Table in 'HLVM\Machine\HLVM.JIT.Context.Storage.Table.pas',
  HLVM.Context.Hashing in 'HLVM\Machine\HLVM.Context.Hashing.pas',
  HLVM.JIT.Context.Storage.Commons in 'HLVM\Machine\HLVM.JIT.Context.Storage.Commons.pas',
  HLVM.Generics.HList in 'HLVM\Generics\HLVM.Generics.HList.pas',
  HLVM.Debug.BytecodeUtilities in 'HLVM\Machine\Debug\HLVM.Debug.BytecodeUtilities.pas',
  HLVM.Debug.Runnable in 'HLVM\Machine\Debug\HLVM.Debug.Runnable.pas',
  HLVM.Debug.Format in 'HLVM\Machine\Debug\HLVM.Debug.Format.pas',
  HLVM.JIT.Execution.Provider in 'HLVM\Machine\JIT\HLVM.JIT.Execution.Provider.pas',
  HLVM.JIT.Execution.Locals in 'HLVM\Machine\JIT\HLVM.JIT.Execution.Locals.pas',
  HLVM.JIT.Constants in 'HLVM\Machine\HLVM.JIT.Constants.pas',
  HLVM.JIT.Standards in 'HLVM\Machine\HLVM.JIT.Standards.pas',
  HLVM.JIT.Execution.RawLocals in 'HLVM\Machine\JIT\HLVM.JIT.Execution.RawLocals.pas',
  HLVM.JIT.Context.CommandExpansion in 'HLVM\Machine\HLVM.JIT.Context.CommandExpansion.pas',
  HLVM.JIT.Context.Scoper in 'HLVM\Machine\HLVM.JIT.Context.Scoper.pas',
  HLVM.JIT.Execution.FSVariables in 'HLVM\Machine\JIT\HLVM.JIT.Execution.FSVariables.pas',
  Velthuis.BigDecimals in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.BigDecimals.pas',
  Velthuis.BigIntegers in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.BigIntegers.pas',
  Velthuis.BigIntegers.Primes in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.BigIntegers.Primes.pas',
  Velthuis.BigRationals in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.BigRationals.pas',
  Velthuis.ExactFloatStrings in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.ExactFloatStrings.pas',
  Velthuis.FloatUtils in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.FloatUtils.pas',
  Velthuis.Loggers in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.Loggers.pas',
  Velthuis.Numerics in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.Numerics.pas',
  Velthuis.RandomNumbers in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.RandomNumbers.pas',
  Velthuis.Sizes in 'HLVM\PartyLibraries\VelthuisBigNumberLibrary\Velthuis.Sizes.pas',
  ExactFloatToStr_JH0 in 'HLVM\PartyLibraries\ExactFloatToStr_JH0.pas',
  HLVM.JIT.Tests.SpeedTest in 'HLVM\Machine\Tests\HLVM.JIT.Tests.SpeedTest.pas',
  HLVM.JIT.Types in 'HLVM\Machine\Types\HLVM.JIT.Types.pas',
  HLVM.JIT.Context.Linker.New in 'HLVM\Machine\HLVM.JIT.Context.Linker.New.pas';

  {$I ./Version/include_version.txt}

{procedure Test;
var Mem: TMemoryManager;
    Ass: TAssigner;
    AVar, ASec: TVariableShell;
begin
  AVar := Mem.AllocateNewInteger(0);
  ASec := Mem.AllocateNewInteger(1);
  Ass.MemoryManager := @Mem;
  Ass.AssignShells(AVar, ASec);
end;

procedure Test1;
var Tabl: TTable;
    Key, NewOne: TRunTimeVariable_String;
    Shell: TVariableShell;
    Shellp: ^TVariableShell;
//    Pair: TPair<TVariableShell, TVariableShell>;
begin
  Tabl := TTable.Create;
  Key := TRunTimeVariable_String.Create('Hello');
  NewOne := TRunTimeVariable_String.Create('Hwello?');
  Shell := TVariableShell.Create(PHString(@Key));
  Tabl.AddOrSetValue(Shell, Shell);
  Writeln(PHString(Tabl.Items[Shell].Content).Content);
//  Shellp := @(Tabl.ExtractPair(Shell).Value);
//  Pair := Tabl.ExtractPair(Shell);
//  Shellp := @(Pair.Value);
//  Shellp^ := TVariableShell.Create(PHString(@NewOne));
//  Tabl.TryGetValue(Shell, Shellp);
  Shellp := Tabl.PointedItems[Shell];
  Shellp.Content := PHString(@NewOne);

  Writeln(PHString(Tabl.Items[Shell].Content).Content);
  Tabl.Free;
end;   }

//procedure TestPerfHash;
//const N = 1000000;
//var i: Integer;
//    Str: String;
//    Watch: TStopwatch;
//begin
//  Str := 'Hello_world1231';
//  Watch := TStopwatch.StartNew;
//  for i := 1 to N do begin
//    THasher.HashString(Str);
//  end;
//  Watch.Stop;
//  Writeln('Hash is ', Watch.ElapsedMilliseconds);
//end;

procedure TestNewTable;
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

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    ReportMemoryLeaksOnShutdown := True;
//    Test;
//    Test1;
//    Test2;

//    RttiType := RttiContext.GetType(Obj.ClassType);
//    RttiMethod := RttiType.GetMethod('ClassName');
//    RttiMethod.Invoke(Obj.ClassType, []);

//    THLVMDebugEntryModule.Run;

//      REPL;
//      HLVM_JIT_TEST_MeasureSpeed;

//     JITRun;

    //TestAdd;
//
//    TRuntimeMonster.CallMethod(List, 'Test');

//    TestPerf;
//    TestPointers;
//    TestPerfHash;
//    TestForTable;
//    Tests;
//    TestTable;
//    CmdFormatter.ProcessString('\c[0011]Hello world!\r\nHey?');
//    CmdIO.SetDefaultColour;
//    TestNewTable;

    THLVMDebugEntryModule.Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
