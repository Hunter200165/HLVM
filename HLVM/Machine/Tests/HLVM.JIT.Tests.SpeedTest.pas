unit HLVM.JIT.Tests.SpeedTest;

interface

uses
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Execution.Provider,
  System.Diagnostics;

{$POINTERMATH ON}
  
procedure HLVM_JIT_TEST_MeasureSpeed;

implementation

type
  TSubState1 = class(TObject)
  public
    InstructionPointer: Pointer;
  end;

{ Empty procedure }
procedure SampleDelphiProc;
begin
end;

procedure SampleDelphiProcObject(const A: TSubState1);
begin

end;

procedure SampleDelphiProcObjectAction(const A: TSubState1);
begin
  // Inc(Integer(A.InstructionPointer), 1);
  A.InstructionPointer := Pointer(Integer(A.InstructionPointer) + 1);
end;

procedure SampleDelphiProcObjectAction1(const A: TSubState1);
var C, B: Integer;
begin
  B := 1;
  C := B + 13;
end;

{ KadJIT 1.0 (-WO Instruction assignment) }
function SampleKadJITProc(const A: TSubState1; const [Ref] Instr: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
end;

function SampleKadJITProcPractical(const SubState: TExecutionSubstate; const [Ref] Instr: TJITExecutionInstruction): PHLVM_Variable;
begin

  Result := nil;
end;

function SampleKadJITForCheck(const SubState: TExecutionSubstate; const [Ref] Instr: TJITExecutionInstruction): PHLVM_Variable;
type PForRecord = ^TForRecord;
var PCurrent, PFinish: PInt64;
    RFor: PForRecord;
begin
    Result := nil;
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { AdditionalPlusData is BreakPos to Jump [KadJIT update] }
      { ForType: 1 = Ascending; 0 = Descending } *)
  (*with SubState do begin
    with Fors[ForCount - 1] do begin
      // Inc(Current, Instr.IntContent);
      if Current > Finish then begin
        { Closing loop execution }
        Dec(ForCount);
        // CurrentInstruction := Breaks[BreakCount - 1].BreakPos;
        CurrentInstruction := Pointer(Instr.DataPlus);//Pointer(Instruction);
        { Returning }
        Exit;
      end;
//      with ForVar^ do begin
//        Typed := HLVM_Type_Integer;
//        SetInt(Current);
//      end;
    end;

    CurrentInstruction := Instr.NextInstruction;
  end;*)
  with SubState do begin 
    
    with Fors[ForCount - 1] do begin 
      PCurrent := @Current;
      PFinish := @Finish;
      Inc(PCurrent^, Instr.IntContent);
    end;

    CurrentInstruction := Instr.NextInstruction;
  end;
end;

procedure HLVM_JIT_TEST_MeasureSpeed;
const N = 100000000;
var i: Integer;
    Ptr: Pointer;
    PPtr: ^Pointer;
    Watch: TStopwatch;
    SubState: TSubState1;
    SS: TExecutionSubstate;
    Instr: TJITExecutionInstruction;
    ForVar: HLVM_Variable;
begin
  {$O -}
  SubState := TSubState1.Create;
  SS := TExecutionSubstate.Create;
  Instr.Nullify;

  Writeln('Iterations: ', N);
  Writeln;

  Writeln('Testing for loop...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do ;
  Watch.Stop;
  Writeln('for loop: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing while [incremental] loop...');
  Watch := TStopwatch.StartNew;
  i := 1;
  while i <= N do
    Inc(i);
  Watch.Stop;
  Writeln('while loop: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing pointer while loop...');
  Watch := TStopwatch.StartNew;
  Ptr := Pointer(N);
  while not (Ptr = nil) do
    Dec(NativeInt(Ptr), 1);
  Watch.Stop;
  Writeln('while loop: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing super pointed while loop...');
  Watch := TStopwatch.StartNew;
  Ptr := Pointer(N);
  PPtr := @Ptr;
  while not (PPtr^ = nil) do
    Dec(NativeInt(Ptr), 1);
  Watch.Stop;
  Writeln('while loop: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing object pointed while loop...');
  Watch := TStopwatch.StartNew;
  SubState.InstructionPointer := Pointer(N);
  while not (SubState.InstructionPointer = nil) do
    Dec(NativeInt(SubState.InstructionPointer), 1);
  Watch.Stop;
  Writeln('while loop: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing empty function...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do SampleDelphiProc;
  Watch.Stop;
  Writeln('empty function: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing object [parameter] function...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do SampleDelphiProcObject(SubState);
  Watch.Stop;
  Writeln('object function: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing object [parameter, action] function...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do SampleDelphiProcObjectAction(SubState);
  Watch.Stop;
  Writeln('object function: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing object [parameter, action] function...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do SampleDelphiProcObjectAction1(SubState);
  Watch.Stop;
  Writeln('object function: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing object, record [parameter] function...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do SampleKadJITProc(SubState, Instr);
  Watch.Stop;
  Writeln('object function: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing KadJIT substate...');
  Watch := TStopwatch.StartNew;
  Instr.GetContentFunction := SampleKadJITProcPractical;
  for i := 1 to N do
    Instr.GetContentFunction(SS, Instr);
  Watch.Stop;
  Writeln('substate: ', Watch.ElapsedMilliseconds);
  Writeln;

  SetLength(SS.Fors, 1);
  SS.Fors[0].Current := 1;
  SS.Fors[0].Finish := N;
  SS.Fors[0].ForVar := @ForVar;
  SS.ForCount := 1;
  Instr.IntContent := 1;
  Writeln('Testing for [KadJIT check] loop...');
  Watch := TStopwatch.StartNew;
  for i := 1 to N do 
    SampleKadJITForCheck(SS, Instr);
  Watch.Stop;
  Writeln('for loop: ', Watch.ElapsedMilliseconds);
  Writeln;

  Writeln('Testing done');
  SubState.Free;
  SS.Free;
end;

end.
