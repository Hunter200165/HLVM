unit HLVM.JIT.Standards;

interface

uses
  System.Hash,
  HLVM.JIT.Execution.Provider,
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.Storage.Commons,
  HLVM.Context.Hashing,
  HLVM.JIT.Context.Storage.Table;

procedure InsertStandards(const Env: PGlobalEnvironment);
procedure PrintStandard(const SubState: TExecutionSubstate; const ArgsPosition: PPHLVMVariable);

implementation

procedure PrintStandard(const SubState: TExecutionSubstate; const ArgsPosition: PPHLVMVariable);
var Pos: PPHLVMVariable;
begin
  Pos := ArgsPosition;
  Inc(Pos);
  while not (Pos^ = nil) do begin
    Write(Pos^^.ToString, #9);
    Inc(Pos);
  end;
  Writeln;
end;

procedure WriteStandard(const SubState: TExecutionSubstate; const ArgsPosition: PPHLVMVariable);
var Pos: PPHLVMVariable;
begin
  Pos := ArgsPosition;
  Inc(Pos);
  while not (Pos^ = nil) do begin
    Write(Pos^^.ToString);
    Inc(Pos);
  end;
end;

procedure WriteLnStandard(const SubState: TExecutionSubstate; const ArgsPosition: PPHLVMVariable);
var Pos: PPHLVMVariable;
begin
  Pos := ArgsPosition;
  Inc(Pos);
  while not (Pos^ = nil) do begin
    Write(Pos^^.ToString);
    Inc(Pos);
  end;
  Writeln;
end;

procedure Nothing(const SubState: TExecutionSubstate; const ArgsPosition: PPHLVMVariable);
begin
  { Almost... Nothing! }
end;

procedure RegisterFunction(const Env: PGlobalEnvironment; const Name: String; const Func: THLVMBuiltInFunction);
begin
  with Env.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(Name))^ do begin
    Typed := HLVM_Type_BuiltInFunction;
    SetPointer(@Func);
  end;
end;

procedure InsertStandards(const Env: PGlobalEnvironment);
begin
  RegisterFunction(Env, 'print', PrintStandard);
  RegisterFunction(Env, 'Write', WriteStandard);
  RegisterFunction(Env, 'WriteLn', WriteLnStandard);

  RegisterFunction(Env, 'Nothing', Nothing);
end;

end.
