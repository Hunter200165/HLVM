unit HLVM.JIT.Execution.Provider;

interface

{ Implements JIT compatible execution provider for HLVM bytecode! }

uses
  uTExtendedX87,
  System.SysUtils,
  System.Hash,
  HLVM.JIT.Context.Linker,
  // HLVM.JIT.Execution.Locals,
  HLVM.JIT.Execution.FSVariables,
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.Storage.Table,
  HLVM.JIT.Context.Storage.Commons,
  HLVM.JIT.Context.CommandExpansion,
  HLVM.JIT.Constants,
  HLVM.Context.Hashing,
  HLVM.Static.Command;

const
  HLVM_KadJIT_AllowDebugModeForLocals = True;

type
  { Environments }
  // TLocalEnvironment = array of TLocalVariablesRegister;
  { Global one }
  TGlobalEnvironment = THLVMTable;
  PGlobalEnvironment = ^TGlobalEnvironment;

type
  { Registers }
  TRegister = array of HLVM_Variable;
  TPointerRegister = array of PHLVM_Variable;
  { Pointers }
  PPHLVMVariable = ^PHLVM_Variable;

type
  { Forward definition }
  TExecutionSubstate = class;

  { Execution holders }
  TJITExecutionInstruction = record
  public type
    TJITExecutionFunction = function(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
    PInternalPointer = ^TJITExecutionInstruction;
    TOperands = array of PInternalPointer;
  public var
    { KadJIT DeLinking upgrade (v1.6) }
    RegisterPosition: HLVM_Integer {PHLVM_Variable};
    PointerRegisterPosition: HLVM_Integer {PPHLVMVariable};
  public var
    OPCode: Integer;

    StringContent: String;
    IntContent: HLVM_Integer;
    FloatContent: HLVM_Float;

    Data: HLVM_Integer;
    DataPlus: HLVM_Integer;
    DataPointer: Pointer;

    Operands: TOperands;

    NextInstruction: PInternalPointer;
  public var
    { Execution segment }
    GetContentFunction: TJITExecutionFunction;
  public
    procedure Nullify;
  end;
  TJITExecutionFunction = TJITExecutionInstruction.TJITExecutionFunction;
  PJITExecutionInstruction = TJITExecutionInstruction.PInternalPointer;

  TJITExecutionContainer = record
  public type
    TActualStorage = array of PJITExecutionInstruction;
    TInternalStorage = array of TJITExecutionInstruction;
  public var
    { KadJIT BeHeading upgrade (v1.6) }
    (*Header: THLVMHeader;*)

    InternalStorage: TInternalStorage;
    ActualStorage: TActualStorage;
  public
    procedure Nullify;
  end;

  { Main thing to make things work under JIT }

  (* Exceptions *)
  EJITCompilationException = class(EMachineException);
  EExecutionException = class(EMachineException);

  (* Functions definitions *)
  THLVMBuiltInFunction = procedure (const SubState: TExecutionSubstate; const ArgsPosition: PPHLVMVariable);
  PHLVMBuiltInFunction = ^THLVMBuiltInFunction;

  (* JIT compiler itself *)
  TJITCompiler = class(TObject)
  public
    { Some operators magic }
    class function JIT_EXEC_ADD(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    { Some pushes }
    class function JIT_EXEC_PushVar(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushVarForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    class function JIT_EXEC_PushKnownLocal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushKnownGlobal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushGlobal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushKnownLocalForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    //class function JIT_EXEC_PushKnownGlobalForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    //class function JIT_EXEC_PushGlobalForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    class function JIT_EXEC_PushInt(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushFloat(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushString(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushNil(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushFalse(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_PushTrue(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    { Some calls }
    class function JIT_EXEC_Call(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    { Some level work }
    class function JIT_EXEC_OpenScope(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_CloseScope(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    { Some JUMPS }
    class function JIT_EXEC_JC(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_JNC(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_JMP(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    (* Special instruction for JIT compiler in way to break the execution *)
    class function JIT_EXEC_JMP_NULL(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_JC_NULL(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_JNC_NULL(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    class function JIT_EXEC_BreakLoop(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_ContinueLoop(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    class function JIT_EXEC_CheckStack(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    class function JIT_EXEC_ForCheckAscending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_ForCheckDescending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_ForSetStartAscending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_ForSetStartDescending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_ForSetFinish(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_ForSetVar(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    
    class function JIT_EXEC_SetBreak(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_SetContinue(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    class function JIT_EXEC_Assign(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;

    { Instructions to work with new heading system }
    class function JIT_EXEC_Heading_SetLocalCacheSize(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_Heading_SetGlobalCacheSize(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
    class function JIT_EXEC_Heading_CacheGlobal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable; static;
  public
    class function JIT_STATIC_VarToBoolean(const AVar: PHLVM_Variable): Boolean;
  public
    class function JITCompileInstruction(const ABytecodeInstruction: HLVM_LinkerWord): TJITExecutionInstruction; static;
    class function JITCompileBytecode(const AInput: THLVMBytecodeSentence): TJITExecutionContainer; static;
    class procedure JITLinkBytecode(const AInput: THLVMBytecodeSentence; var Container: TJITExecutionContainer); static;
  end;

  { Where to store all the break stuff. }
  TBreakRecord = record
  public var
    BreakPos: PJITExecutionInstruction;
    ContinuePos: PJITExecutionInstruction;
    ScopeID: Integer;
  end;
  TBreakArray = array of TBreakRecord;

  { For loops }
  TForRecord = record
  public var
    Current: HLVM_Integer;
    Finish: HLVM_Integer;
    ForVar: PHLVM_Variable;
  end;
  TForArray = array of TForRecord;

  { Main execution state, holding the data for execution and JIT-compatible execution runner. }

  (*
    KadJIT update deprecates usage of named local variables and introduces its own system
  *)

  TExecutionSubstate = class(TObject)
  public var
    { Environmental needs }
    // LocalEnvironment: TLocalEnvironment;
    LocalScope: TLocalScope;
    GlobalScope: TGlobalScope;

    GlobalEnvironment: PGlobalEnvironment;

//    LocalHeapCount: Integer;
    LocalLevel: Integer;

//    LocalID: Integer;
  public var
    DataRegister: TRegister;
    PointerRegister: TPointerRegister;

    NullPointerStorage: HLVM_Variable;
  public var
    { CF }
    CarryFlag: Boolean;

    { For `for` loop! }
    { Does not need any special id matching }
    Fors: TForArray;
    ForCount: Integer;
    ForSize: Integer;
  public var
    { Execution data }
    FunctionState: TExecutionSubstate;

    ExecutionStorage: TJITExecutionContainer;
    CurrentInstruction: PJITExecutionInstruction;

    Linked: Boolean;
  public
    procedure Link;
    procedure OBSOLETE_ProcessHeader;
    procedure Execute;
  
    procedure Nullify;
  end;

{ KadJIT brother }
{$Define KadJIITInlines}
type
  TKadJIITSubState = class;
  { KadJIIT compiler instruction }
  TKadJIITInstruction = record
  public type
    PInternalPointer = ^TKadJIITInstruction;

    TComputeFunction = procedure (const Self: PInternalPointer; const SubState: TKadJIITSubState);
  public var
    OPCode: Integer;

    RegisterPosition: PHLVM_Variable;
    PointerRegisterPosition: PPHLVMVariable;

    StringContent: String;
    IntContent: HLVM_Integer;
    FloatContent: HLVM_Float;

    Data: HLVM_Integer;
    DataPlus: HLVM_Integer;
    DataPointer: Pointer;
  public var
    { Execution segment }
    Compute: TComputeFunction;
  public
    { Execution functions }
    class procedure EXEC_DoNothing(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}

    class procedure EXEC_PushInt(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}

    class procedure EXEC_JMP_NULL(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_JC_NULL(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_JNC_NULL(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_JMP(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_JC(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_JNC(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}

    class procedure EXEC_ForSetStartAsc(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_ForSetStartDes(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_ForCheckAsc(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_ForCheckDes(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_ForSetFinish(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_ForSetVar(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}

    class procedure EXEC_OpenScope(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
    class procedure EXEC_CloseScope(const Self: PInternalPointer; const SubState: TKadJIITSubState); static; {$IfDef KadJIITInlines} inline; {$EndIf}
  public
    procedure Nullify;
  end;
  PKadJIITInstruction = TKadJIITInstruction.PInternalPointer;
  PPKadJIITInstruction = ^PKadJIITInstruction;

  TKadJIITExecutionContainer = record
  public type
    TInstructions = array of TKadJIITInstruction;
  public var
    Instructions: TInstructions;

    Header: THLVMHeader;
  public
    procedure Nullify;
  end;

  TKadJIITCompiler = class(TObject)
  public type
    TLabel = record
    public var
      Index: Integer;
      LabelIndex: Integer;
    end;
    TLabelArray = array of TLabel;
  public
    class procedure JITCompileInstruction(const ABytecodeInstruction: HLVM_LinkerWord; var Result: TKadJIITInstruction); static;
    class function JITCompileBytecode(const AInput: THLVMBytecodeSentence; const DecrementLabels: Boolean = False): TKadJIITExecutionContainer; static;
    class procedure JITLinkBytecode(const AInput: THLVMBytecodeSentence; var Container: TKadJIITExecutionContainer; const Labels: TLabelArray; const DecrementLabels: Boolean = False); static;
  end;

  { KadJIIT SubState }
  TKadJIITSubState = class
  public var
    GlobalEnv: PGlobalEnvironment;

    GlobalScope: TGlobalScope;
    LocalScope: TLocalScope;

    LocalLevel: Integer;

    DataRegister: TRegister;
    PointerRegister: TPointerRegister;
  public var
    CarryFlag: Boolean;

    { For `for` loop! }
    { Does not need any special id matching }
    Fors: TForArray;

    { Deprecated, use ForIndex instead! } (* ForCount: Integer; *)
    ForIndex: Integer;
    ForSize: Integer;
  public var
    InstructionPtr: PPKadJIITInstruction;

    Linked: Boolean;
    ExecutionContainer: TKadJIITExecutionContainer;
  public
    procedure Link;
    procedure ProcessHeader;

    procedure ExecuteInline;
    procedure ExecuteFunctional;

    procedure Nullify;
  end;

implementation

{ TExecutionSubstate }

procedure TExecutionSubstate.Execute;
var APos: ^PJITExecutionInstruction;
begin
  // ProcessHeader;
  Link;
  try
    APos := @CurrentInstruction;
    APos^ := ExecutionStorage.ActualStorage[0];
    while not (APos^ = nil) do begin
      APos^^.GetContentFunction(Self, APos^^);
    end;
  finally
    ResetFPU;
    ClearFPUExceptions;
  end;
end;

procedure TExecutionSubstate.Link;
var i: Integer;
begin
  if Linked then
    Exit;
  { Link all variables position }

  (* KadJIT DeLinking upgrade (v1.6) *)
//  for i := 0 to Length(ExecutionStorage.InternalStorage) - 1 do begin
//    with ExecutionStorage.InternalStorage[i] do begin
//      RegisterPosition := @DataRegister[Integer(RegisterPosition)];
//      PointerRegisterPosition := @PointerRegister[Integer(PointerRegisterPosition)];
//    end;
//  end;

//  { Variables }
//  for i := 0 to Length(ExecutionStorage.InternalStorage) - 1 do begin
//    with ExecutionStorage.InternalStorage[i] do begin
//      case OPCode of
//        Command_PushVar: begin
//          if not (Data and ScoperFlag_Var_PushKnownLocal = 0) then
//            DataPlus := Int64(@LocalScope[DataPlus])
//          else if not (Data and ScoperFlag_Var_PushKnownGlobal = 0) then
//            DataPlus := Int64(@GlobalScope[DataPlus]);
//        end;
//        Command_ForSetVar: begin
//          DataPlus := Int64(@LocalScope[DataPlus]);
//        end;
//      end;
//    end;
//  end;
  Linked := True;
end;

procedure TExecutionSubstate.Nullify;
begin
  LocalLevel := 0;

  { Registers }
  SetLength(DataRegister, HLVM_REGISTER_SIZE);
  SetLength(PointerRegister, HLVM_POINTED_REGISTER_SIZE);
end;

procedure TExecutionSubstate.OBSOLETE_ProcessHeader;
var i, PosGlobals: Integer;
    HeaderEntry: THLVMHeaderEntry;
    Ptr: PHLVM_Variable;
begin
  (* KadJIT BeHeading upgrade (v1.6) *)
  raise EExecutionException.Create('KadJIT BeHeading upgrade does not allow linked bytecode to have header anymore!');

  (*PosGlobals := 0;
  for i := 0 to ExecutionStorage.Header.Count - 1 do begin
    HeaderEntry := ExecutionStorage.Header[i];
    case HeaderEntry.Code of
      JIT_Header_NULL: ;
      JIT_Header_LocalHeapSize: begin
        if HeaderEntry.IntStorage >= 0 then
          SetLength(LocalScope, HeaderEntry.IntStorage)
        else
          raise EExecutionException.Create('Invalid value in LocalHeapSize header!');
      end;
      JIT_Header_GlobalCacheSize: begin
        if HeaderEntry.IntStorage >= 0 then
          SetLength(GlobalScope, HeaderEntry.IntStorage)
        else
          raise EExecutionException.Create('Invalid value in GlobalCacheSize header!');
      end;
      JIT_Header_CacheGlobal: begin
        { Try to find this variable in heap }
        Ptr := GlobalEnvironment.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(HeaderEntry.StringStorage));
        if Ptr = nil then
          { Have not prevailed - insert new one. }
          Ptr := GlobalEnvironment.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(HeaderEntry.StringStorage));
        with GlobalScope[PosGlobals] do begin
          Name := HeaderEntry.StringStorage;
          Reference := Ptr;
        end;
      end;
    end;
  end;*)
end;

{ TJITExecutionContainer }

procedure TJITExecutionContainer.Nullify;
begin
  SetLength(InternalStorage, 0);
  SetLength(ActualStorage, 0);

  { KadJIT BeHeading upgrade (v1.6) }
  // Header.Size := 0;
end;

{ TJITExecutionInstruction }

procedure TJITExecutionInstruction.Nullify;
begin
  { Automatically done, yet for security reasons }
  StringContent := '';
  { Unmanaged memory }
  IntContent := 0;
//  {$IfNDef Win64}
  FloatContent := 0;
//  {$Else}
//  FillChar(FloatContent.AsBytes, 10, 0);
//  FloatContent := FloatContent.Create(0);
//  {$EndIf}
  Data := 0;
  DataPointer := nil;

  (* KadJIT DeLinking upgrade (v1.6) *)
  RegisterPosition := 0 {nil};
  PointerRegisterPosition := 0 {nil};

  { Arrays }
  SetLength(Operands, 0);
end;

{ TJITCompiler }

class function TJITCompiler.JITCompileBytecode(const AInput: THLVMBytecodeSentence): TJITExecutionContainer;
var i: Integer;
begin
  Result.Nullify;
  { Header is static }
//  Result.Header := AInput.Header;

  { Why Length + 1? }
  { Because JIT appends final JMP_NULL instruction in way to termainate all the execution }
  SetLength(Result.InternalStorage, AInput.InternalStorage.Count + 1);
  SetLength(Result.ActualStorage, AInput.ActualStorage.Count + 1);

  for i := 0 to AInput.InternalStorage.Count - 1 do 
    Result.InternalStorage[i] := TJITCompiler.JITCompileInstruction(AInput.InternalStorage[i]);

  with Result.InternalStorage[Length(Result.InternalStorage) - 1] do begin 
    Nullify;
    { No opcode }
    GetContentFunction := TJITCompiler.JIT_EXEC_JMP_NULL;
  end;

  TJITCompiler.JITLinkBytecode(AInput, Result);
end;

class function TJITCompiler.JITCompileInstruction(const ABytecodeInstruction: HLVM_LinkerWord): TJITExecutionInstruction;
var
  i: Integer;
begin
  Result.Nullify;
  Result.OPCode := ABytecodeInstruction.CommandID;
  with Result, ABytecodeInstruction do begin
    StringContent := StringStorage;
    IntContent := IntegerStorage;
    FloatContent := FloatStorage;
    Data := AdditionalData;
    DataPlus := AdditionalPlusData;
  end;

  { We are using actual pointers as storage for integer values. I love danger too! }
  { Without joke - it is optimised way to store pointed position, as compiler will make only ONE type coersion }

  (* KadJIT DeLinking upgrade (v1.6) *)
  Result.RegisterPosition := {Pointer(}ABytecodeInstruction.RegisterIndex{)};
  Result.PointerRegisterPosition := {Pointer(}ABytecodeInstruction.PointerRegisterIndex{)};

  case Result.OPCode of
    Command_ArADD: begin
      Result.GetContentFunction := TJITCompiler.JIT_EXEC_ADD;
    end;

    Command_OpenScope: Result.GetContentFunction := TJITCompiler.JIT_EXEC_OpenScope;
    Command_CloseScope: Result.GetContentFunction := TJITCompiler.JIT_EXEC_CloseScope;

    Command_PushInteger: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushInt;
    Command_PushReal: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushFloat;
    Command_PushString: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushString;
    Command_PushNil: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushNil;
    Command_PushFalse: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushFalse;
    Command_PushTrue: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushTrue;
    Command_PushVar: begin
      {case Result.Data and 1 of
        0: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushVar;
        1: Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushVarForce;
      end;}
      if not (Result.Data and ScoperFlag_Var_PushKnownLocal = 0) and not (Result.Data and 1 = 0) then
        Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushKnownLocalForce
      else if not (Result.Data and ScoperFlag_Var_PushKnownLocal = 0) then
        Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushKnownLocal
      else if not (Result.Data and ScoperFlag_Var_PushKnownGlobal = 0) then
        Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushKnownGlobal
      else if not (Result.Data and ScoperFlag_Var_PushRawGlobal = 0) then
        { Never will be forced, as unoptimal variant is used only for reading; Writing variable is optimised by KadJIT linker! }
        Result.GetContentFunction := TJITCompiler.JIT_EXEC_PushGlobal
      else
        raise EJITCompilationException.Create('Malformed PushVar instruction.');
    end;

    Command_Call: Result.GetContentFunction := TJITCompiler.JIT_EXEC_Call;

    Command_JMP, Command_JC, Command_JNC: { It is linked, yet not errorful for this step };

    Command_SetBreak: Result.GetContentFunction := TJITCompiler.JIT_EXEC_SetBreak;
    Command_SetContinue: Result.GetContentFunction := TJITCompiler.JIT_EXEC_SetContinue;

    Command_BreakLoop: Result.GetContentFunction := TJITCompiler.JIT_EXEC_BreakLoop;
    Command_ContinueLoop: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ContinueLoop;
    
    Command_ForSetStart: begin 
      case (Result.Data and 1) of
        0: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ForSetStartDescending; 
        1: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ForSetStartAscending;
      end;
    end;
    Command_ForCheck: begin 
      case (Result.Data and 1) of
        0: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ForCheckDescending;
        1: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ForCheckAscending;  
      end;
    end;
    
    Command_ForSetFinish: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ForSetFinish;
    Command_ForSetVar: Result.GetContentFunction := TJITCompiler.JIT_EXEC_ForSetVar;

    Command_Assign: Result.GetContentFunction := TJITCompiler.JIT_EXEC_Assign;

    Command_CheckStack: Result.GetContentFunction := TJITCompiler.JIT_EXEC_CheckStack;

    JIT_Command_Heading_SetLocalHeapSize: Result.GetContentFunction := TJITCompiler.JIT_EXEC_Heading_SetLocalCacheSize;
    JIT_Command_Heading_SetGlobalHeapSize: Result.GetContentFunction := TJITCompiler.JIT_EXEC_Heading_SetGlobalCacheSize;
    JIT_Command_Heading_CacheGlobal: Result.GetContentFunction := TJITCompiler.JIT_EXEC_Heading_CacheGlobal;
  else
    raise EJITCompilationException.Create('Cannot resolve command operational code: $' + IntToStr(Result.OPCode));
  end;

  SetLength(Result.Operands, ABytecodeInstruction.AbstractOperands.Count);
  for i := 0 to ABytecodeInstruction.AbstractOperands.Count - 1 do
    Result.Operands[i] := Pointer(ABytecodeInstruction.AbstractOperands[i]);
end;

class procedure TJITCompiler.JITLinkBytecode(const AInput: THLVMBytecodeSentence; var Container: TJITExecutionContainer);
var Labels: array of Integer;
    i, k, Pos, Content: Integer;
    Instr: PJITExecutionInstruction;
begin
  { Performs general linking in way to make it work! }
  SetLength(Labels, AInput.ActualStorage.Count);

  { Linking operands... }
  for i := 0 to Length(Container.InternalStorage) - 2 do begin
    { The last is undefined JMP_NULL }
    Instr := @Container.InternalStorage[i];
    for k := 0 to Length(Instr.Operands) - 1 do
      Instr.Operands[k] := @Container.InternalStorage[Integer(Instr.Operands[k])];
  end;

  { Linking execution order... }
  for i := 0 to AInput.ActualStorage.Count - 1 do begin
    Pos := AInput.ActualStorage[i];
    Labels[i] := AInput.InternalStorage[Pos].LabeledIndex;
    Container.ActualStorage[i] := @Container.InternalStorage[Pos];
  end;

  { Linking JMP_NULL... }
  Container.ActualStorage[Length(Container.ActualStorage) - 1] := @Container.InternalStorage[Length(Container.InternalStorage) - 1];

  { Linking Nexts... }
  for i := 0 to Length(Container.InternalStorage) - 1 do begin 
    Instr := @Container.InternalStorage[i];
    Instr.NextInstruction := Instr;
  end;
  for i := 0 to Length(Container.ActualStorage) - 2 do begin 
    { It is Length - 2 because the last instruction has no end-point to link }
    Container.ActualStorage[i].NextInstruction := Container.ActualStorage[i + 1];
  end;

  { Linking labels... }
  for i := 0 to Length(Container.InternalStorage) - 2 do begin 
    { The same - last is JMP_NULL }
    Instr := @Container.InternalStorage[i];
    case Instr.OPCode of
      Command_JMP, Command_JC, Command_JNC,
      Command_SetBreak, Command_SetContinue,
      Command_BreakLoop, Command_ContinueLoop: begin
        { We need to go deeper! }        
        Pos := -1;
        Content := Instr.IntContent;
        if Content < 0 then
          raise EJITCompilationException.Create('Linking label instruction got an exception: Not valid position to jump.');
        for k := 0 to Length(Labels) - 1 do 
          if Labels[k] >= Content then begin 
            Pos := k;
            Break;
          end;
        if Pos < 0 then begin 
          { This way we no label can be used to JMP. Therefore we jump to NULL }
          case Instr.OPCode of
            Command_JMP: Instr.GetContentFunction := TJITCompiler.JIT_EXEC_JMP_NULL;
            Command_JC: Instr.GetContentFunction := TJITCompiler.JIT_EXEC_JC_NULL;
            Command_JNC: Instr.GetContentFunction := TJITCompiler.JIT_EXEC_JNC_NULL; 
            Command_SetBreak, Command_SetContinue: Instr.IntContent := HLVM_Integer(nil);
            Command_BreakLoop, Command_ContinueLoop: Instr.IntContent := HLVM_Integer(nil);
          end;
        end
        else begin 
          Instr.IntContent := HLVM_Integer(Container.ActualStorage[k]);
          case Instr.OPCode of
            Command_JMP: Instr.GetContentFunction := TJITCompiler.JIT_EXEC_JMP;
            Command_JC: Instr.GetContentFunction := TJITCompiler.JIT_EXEC_JC;
            Command_JNC: Instr.GetContentFunction := TJITCompiler.JIT_EXEC_JNC; 
          end;
        end;
      end;

      Command_ForCheck: begin
        { Data+ is position to break }
        Pos := -1;
        Content := Instr.DataPlus;
        if Content < 0 then
          raise EJITCompilationException.Create('Linking Break instruction got an exception: Not valid position to jump.');
        for k := 0 to Length(Labels) - 1 do
          if Labels[k] >= Content then begin
            Pos := k;
            Break;
          end;
        if Pos < 0 then begin
          Instr.DataPlus := 0;
        end
        else
          Instr.DataPlus := Int64(Container.ActualStorage[k]);
      end;
    end;
  end;

  { Linking and checking scopes... }
  Content := 0;
  for i := 0 to Length(Container.ActualStorage) - 2 do begin
    { Last command ID is undefined!!! }
    { Because it is INTERNAL JIT OPERATION BY SPECIFICATION!!! } 
    case Container.ActualStorage[i].OPCode of
      Command_OpenScope: Content := Content + 1;
      Command_CloseScope: Content := Content - 1;
    end;
    if Content < 0 then
      raise EJITCompilationException.Create('Scopes closures are violated. `CloseScope` is not closing the relative `OpenScope`');
  end;
  if not (Content = 0) then
    raise EJITCompilationException.Create('Scopes are violated. Not all `OpenScope` are closed with `CloseScope`');
end;

class function TJITCompiler.JIT_EXEC_ADD(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var AFirst, ASecond: PHLVM_Variable;
    Operand: PJITExecutionInstruction;
    AT, ST: NativeInt;
    // Pos: PPHLVMVariable;
begin
//  Result := SubState.PointerRegister[Instruction];
//  Pos := Instruction.PointerRegisterPosition;
//  Result := Instruction.RegisterPosition;
  Result := @SubState.DataRegister[Instruction.RegisterPosition];

  { Second }
  Operand := Instruction.Operands[0];
  ASecond := Operand.GetContentFunction(SubState, Operand^);

  { First }
  Operand := Instruction.Operands[1];
  AFirst := Operand.GetContentFunction(SubState, Operand^);

  AT := AFirst.Typed;
  ST := ASecond.Typed;

  (*if (AT in [HLVM_Type_Integer, HLVM_Type_Float]) and (ST in [HLVM_Type_Integer, HLVM_Type_Float]) then begin
    { Arithmetics }
    if (AT = HLVM_Type_Integer) then
      if ST = HLVM_Type_Integer then begin
        Result.Typed := HLVM_Type_Integer;
        Result.SetInt(AFirst.GetInt + ASecond.GetInt);
      end
      else begin
        Result.Typed := HLVM_Type_Float;
        Result.SetFloat(AFirst.GetInt + ASecond.GetFloat);
      end
    else begin
      Result.Typed := HLVM_Type_Float;
      if ST = HLVM_Type_Integer then
        Result.SetFloat(AFirst.GetFloat + ASecond.GetInt)
      else
        Result.SetFloat(AFirst.GetFloat + ASecond.GetFloat);
    end;
  end;*)

  (* if (AT in [HLVM_Type_Integer, HLVM_Type_Float]) and (ST in [HLVM_Type_Integer, HLVM_Type_Float]) then begin
    { Arithmetics }
    AT := Byte(AT + (AT xor ST));
    if (AT = 1) then begin
      Result.Typed := HLVM_Type_Integer;
      Result.SetInt(AFirst.GetInt + ASecond.GetInt);
    end
    else if AT = 4 then begin
      Result.Typed := HLVM_Type_Float;
      Result.SetFloat(AFirst.GetInt + ASecond.GetFloat);
    end
    else if AT = 5 then begin
      Result.Typed := HLVM_Type_Float;
      Result.SetFloat(AFirst.GetFloat + ASecond.GetInt)
    end
    else if ST = 2 then begin
      Result.Typed := HLVM_Type_Float;
      Result.SetFloat(AFirst.GetFloat + ASecond.GetFloat);
    end;
  end;  *)

  case (AT + (AT xor ST)) of
    HLVM_Type_Integer: begin
      { Int + Int }
      Result.Typed := HLVM_Type_Integer;
      Result.SetInt(AFirst.GetInt + ASecond.GetInt);
    end;
    HLVM_Type_Integer + (HLVM_Type_Integer xor HLVM_Type_Float): begin
      { Int + Float }
      Result.Typed := HLVM_Type_Float;
      Result.SetFloat(AFirst.GetInt + ASecond.GetFloat);
    end;
    HLVM_Type_Float + (HLVM_Type_Integer xor HLVM_Type_Float): begin
      { Float + Int }
      Result.Typed := HLVM_Type_Float;
      Result.SetFloat(AFirst.GetFloat + ASecond.GetInt);
    end;
    HLVM_Type_Float: begin
      { Float + Float }
      Result.Typed := HLVM_Type_Float;
      Result.SetFloat(AFirst.GetFloat + ASecond.GetFloat);
    end;
  end;


  {case AFirst.Typed of
    HLVM_Type_Float: begin
      Result.Typed := HLVM_Type_Float;
      case ASecond.Typed of
        HLVM_Type_Float: begin
          Result.SetFloat(AFirst.GetFloat + ASecond.GetFloat);
        end;
        HLVM_Type_Integer: begin
          Result.SetFloat(AFirst.GetFloat + ASecond.GetInt);
        end;
      else
        raise EExecutionException.Create('Attempt to perform arithmetics on invalid values');
      end;
    end;
    HLVM_Type_Integer: begin
      case ASecond.Typed of
        HLVM_Type_Float: begin
          Result.Typed := HLVM_Type_Float;
          Result.SetFloat(AFirst.GetInt + ASecond.GetFloat);
        end;
        HLVM_Type_Integer: begin
          Result.Typed := HLVM_Type_Integer;
          Result.SetInt(AFirst.GetInt + ASecond.GetInt);
        end;
      else
        raise EExecutionException.Create('Attempt to perform arithmetics on invalid values');
      end;
    end;
    HLVM_Type_String: begin
      Result.Typed := HLVM_Type_String;
      Result.Stringy := AFirst.Stringy + ASecond.ToString;
    end
  else
    raise EExecutionException.Create('Attempt to perform arithmetics on invalid values');
  end;   }

//  Instruction.PointerRegisterPosition^ := Result;
  SubState.PointerRegister[Instruction.PointerRegisterPosition] := Result;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_Assign(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Oper: PJITExecutionInstruction;
    ActVar, ActVal: PHLVM_Variable;
begin
  Result := nil;

  Oper := Instruction.Operands[0];
  ActVar := Oper.GetContentFunction(SubState, Oper^);

  Oper := Instruction.Operands[1];
  ActVal := Oper.GetContentFunction(SubState, Oper^);
  
  ActVar^ := ActVal^;
  
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_BreakLoop(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;

  { In fact - it is useless }
  Dec(SubState.LocalLevel, Instruction.Data);
  Dec(SubState.ForCount, 1);

  SubState.CurrentInstruction := PJITExecutionInstruction(Instruction.IntContent);
end;

class function TJITCompiler.JIT_EXEC_Call(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var PosRes, ResultingPos: PPHLVMVariable;
    Oper: PJITExecutionInstruction;
    Func: PHLVM_Variable;
    i, Len: Integer;
    Operands: TJITExecutionInstruction.TOperands;
    Funct: THLVMBuiltInFunction;
begin
//  PosRes := Instruction.PointerRegisterPosition;
  PosRes := @SubState.PointerRegister[Instruction.PointerRegisterPosition];

  PosRes^ := nil;

  Operands := Instruction.Operands;

  Oper := Operands[0];
  Func := Oper.GetContentFunction(SubState, Oper^);

  Len := Length(Operands);
  for i := 1 to Len - 1 do begin
    Oper := Operands[i];
    Oper.GetContentFunction(SubState, Oper^);
  end;

  if Len > 1 then begin
    ResultingPos := @SubState.PointerRegister[Operands[Len - 1].PointerRegisterPosition];
    Inc(ResultingPos);
    { Separating with nil }
    ResultingPos^ := nil;
  end;

  case Func.Typed of
    HLVM_Type_Function: begin
      raise EExecutionException.Create('Not implemented call!');
    end;
    HLVM_Type_BuiltInFunction: begin
      // (THLVMBuiltInFunction(Func.GetPointer))(SubState, Succ(Func));
      // THLVMBuiltInFunction(Func.GetPointer^)(SubState, PosRes);
      Funct := THLVMBuiltInFunction(PHLVMBuiltInFunction(Func.GetPointer));
      Funct(SubState, PosRes);
    end;
  else 
    raise Exception.Create('Attempt to call invalid value!');
  end;

  Result := PosRes^;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_CheckStack(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PHLVM_Variable;
    Oper: PJITExecutionInstruction;
begin
  Result := nil;
  Oper := Instruction.Operands[0];
  Pos := Oper.GetContentFunction(SubState, Oper^);

  SubState.CarryFlag := not (Pos^.Typed = HLVM_Type_Nil) and not ((Pos^.Typed = HLVM_Type_Boolean) and not Boolean(Pos^.GetInt));

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_CloseScope(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  { KadJIT introduced fixes }
  Result := nil;
  SubState.LocalLevel := Instruction.IntContent;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_ContinueLoop(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;

  SubState.CurrentInstruction := PJITExecutioninstruction(Instruction.IntContent);
end;

class function TJITCompiler.JIT_EXEC_ForCheckAscending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { AdditionalPlusData is BreakPos to Jump [KadJIT update] }
      { ForType: 1 = Ascending; 0 = Descending } *)
  with SubState do begin
    with Fors[ForCount - 1] do begin 
      Inc(Current, Instruction.IntContent);
      if Current > Finish then begin 
        { Closing loop execution }
        Dec(ForCount);
        // CurrentInstruction := Breaks[BreakCount - 1].BreakPos;
        CurrentInstruction := Pointer(Instruction.DataPlus);//Pointer(Instruction);
        { Returning }
        Exit;
      end;
      with ForVar^ do begin
        Typed := HLVM_Type_Integer;
        SetInt(Current);
      end;
    end;

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_ForCheckDescending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { ForType: 1 = Ascending; 0 = Descending } *)
  with SubState do begin
    with Fors[ForCount - 1] do begin 
      Dec(Current, Instruction.IntContent);
      if Current < Finish then begin 
        { Closing loop execution }
        Dec(ForCount);
        // CurrentInstruction := Breaks[BreakCount - 1].BreakPos;
        CurrentInstruction := Pointer(Instruction.DataPlus);
        { Returning }
        Exit;
      end;
      with ForVar^ do begin 
        Typed := HLVM_Type_Integer;
        SetInt(Current);
      end;
    end;

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_ForSetFinish(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Oper: PJITExecutionInstruction;
    State: PHLVM_Variable;
begin
  Result := nil;

  Oper := Instruction.Operands[0];
  State := Oper.GetContentFunction(SubState, Oper^);

  if not (State.Typed = HLVM_Type_Integer) then
    raise EExecutionException.Create('For limit must be an integer.');

  with SubState do begin  
    Fors[ForCount - 1].Finish := State.GetInt;

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_ForSetStartAscending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Oper: PJITExecutionInstruction;
    Start: PHLVM_Variable;
begin
  Result := nil;
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { ForType: 1 = Ascending; 0 = Descending } *)
  Oper := Instruction.Operands[0];
  Start := Oper.GetContentFunction(SubState, Oper^);
  if not (Start.Typed = HLVM_Type_Integer) then
    raise EExecutionException.Create('For start must be an integer.');
  with SubState do begin
    Inc(ForCount);
    if ForCount > ForSize then begin
      Inc(ForSize, HLVM_FORS_EXPANSION_SIZE);
      SetLength(Fors, ForSize);
    end;
    with Fors[ForCount - 1] do 
      { Ascending }
      Current := Start.GetInt - Instruction.IntContent;

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_ForSetStartDescending(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Oper: PJITExecutionInstruction;
    Start: PHLVM_Variable;
begin
  Result := nil;
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { ForType: 1 = Ascending; 0 = Descending } *)
  Oper := Instruction.Operands[0];
  Start := Oper.GetContentFunction(SubState, Oper^);
  if not (Start.Typed = HLVM_Type_Integer) then
    raise EExecutionException.Create('For start must be an integer.');
  with SubState do begin
    Inc(ForCount);
    if ForCount > ForSize then begin
      Inc(ForSize, HLVM_FORS_EXPANSION_SIZE);
      SetLength(Fors, ForSize);
    end;
    with Fors[ForCount - 1] do 
      { Descending }
      Current := Start.GetInt + Instruction.IntContent;

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_ForSetVar(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var LPos: PLocalVariable;
begin
  Result := nil;
  with SubState do begin
//    Pos := LocalEnvironment[LocalLevel].PositionFor(HashString(Instruction.StringContent), Instruction.StringContent);
//    Fors[ForCount - 1].ForVar := @LocalEnvironment[LocalLevel].List[Pos].Value;

    { KadJIT introduced static local variables }

    (* KadJIT DeLinking upgrade (v1.6) *)
//    LPos := PLocalVariable(Instruction.DataPlus);
    LPos := @SubState.LocalScope[Instruction.DataPlus];
    Fors[ForCount - 1].ForVar := @LPos.Content;
    if HLVM_KadJIT_AllowDebugModeForLocals then
      LPos.Name := Instruction.StringContent;

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_Heading_CacheGlobal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Ptr: PHLVM_Variable;
begin
  Result := nil;

  (*
    { Try to find this variable in heap }
        Ptr := GlobalEnvironment.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(HeaderEntry.StringStorage));
        if Ptr = nil then
          { Have not prevailed - insert new one. }
          Ptr := GlobalEnvironment.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(HeaderEntry.StringStorage));
        with GlobalScope[PosGlobals] do begin
          Name := HeaderEntry.StringStorage;
          Reference := Ptr;
        end;
  *)
  { Try to find variable in Heap }
  Ptr := SubState.GlobalEnvironment.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(Instruction.StringContent));
  if Ptr = nil then
    { There is no global variable with this name }
    Ptr := SubState.GlobalEnvironment.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(Instruction.StringContent));
  with SubState.GlobalScope[Instruction.IntContent] do begin
    Name := Instruction.StringContent;
    Reference := Ptr;
  end;

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_Heading_SetGlobalCacheSize(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  { If fits - remain }
  if Length(SubState.GlobalScope) < Instruction.IntContent then
    SetLength(SubState.GlobalScope, Instruction.IntContent);

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_Heading_SetLocalCacheSize(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  { If fits - then all is OK }
  if Length(SubState.LocalScope) < Instruction.IntContent then
    SetLength(SubState.LocalScope, Instruction.IntContent);

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_JC(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  if SubState.CarryFlag then
    SubState.CurrentInstruction := PJITExecutionInstruction(Instruction.IntContent)
  else
    SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_JC_NULL(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  if SubState.CarryFlag then
    SubState.CurrentInstruction := nil
  else 
    SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_JMP(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  SubState.CurrentInstruction := PJITExecutionInstruction(Instruction.IntContent);
end;

class function TJITCompiler.JIT_EXEC_JMP_NULL(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  SubState.CurrentInstruction := nil;
end;

class function TJITCompiler.JIT_EXEC_JNC(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  if not SubState.CarryFlag then
    SubState.CurrentInstruction := PJITExecutionInstruction(Instruction.IntContent)
  else
    SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_JNC_NULL(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  if not SubState.CarryFlag then
    SubState.CurrentInstruction := nil
  else 
    SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_OpenScope(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  Inc(SubState.LocalLevel);

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_PushFalse(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];
  Pos^ := @SubState.DataRegister[Instruction.RegisterPosition];

  Result := Pos^;

  with Result^ do begin
    Typed := HLVM_Type_Boolean;
    SetInt(0);
  end;

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_PushFloat(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  with Instruction do begin
//    Result := RegisterPosition;
    Result := @SubState.DataRegister[Instruction.RegisterPosition];

    with Result^ do begin
      Typed := HLVM_Type_Float;
      SetFloat(Instruction.FloatContent);
    end;

    SubState.PointerRegister[PointerRegisterPosition] := Result;
    SubState.CurrentInstruction := NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_PushGlobal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
begin
  // raise EExecutionException.Create('Not supported');
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];

  Pos^ := SubState.GlobalEnvironment.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(Instruction.StringContent));
  if (Pos^ = nil) then begin
    SubState.NullPointerStorage := HLVM_NIL;
    Pos^ := @SubState.NullPointerStorage;
  end;

  Result := Pos^;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

//class function TJITCompiler.JIT_EXEC_PushGlobalForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
//begin
//
//end;

class function TJITCompiler.JIT_EXEC_PushInt(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
// var Pos: PPHLVMVariable;
begin
  // Pos := Instruction.PointerRegisterPosition;
  with Instruction do begin
//    Result := RegisterPosition;
    Result := @SubState.DataRegister[Instruction.RegisterPosition];

    with Result^ do begin
      Typed := HLVM_Type_Integer;
      SetInt(Instruction.IntContent);
    end;

    SubState.PointerRegister[PointerRegisterPosition] := Result;
    SubState.CurrentInstruction := NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_PushKnownGlobal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];

  (* KadJIT DeLinking upgrade (v1.6) *)
  // Pos^ := PGlobalVariable(Instruction.DataPlus).Reference;
  Pos^ := SubState.GlobalScope[Instruction.DataPlus].Reference;

  Result := Pos^;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

//class function TJITCompiler.JIT_EXEC_PushKnownGlobalForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
//begin
//
//end;

class function TJITCompiler.JIT_EXEC_PushKnownLocal(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];
  // Pos^ := @PLocalVariable(Instruction.DataPlus).Content;

  (* KadJIT DeLinking upgrade (v1.6) *)
  Pos^ := @SubState.LocalScope[Instruction.DataPlus].Content;

  Result := Pos^;
  SubState.CurrentInstruction := Instruction.NextInstruction;
//  PLocalVariable(Instruction.DataPlus)^;
end;

class function TJITCompiler.JIT_EXEC_PushKnownLocalForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
    LPos: PLocalVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];

  (* KadJIT DeLinking upgrade (v1.6) *)
//  LPos := PLocalVariable(Instruction.DataPlus);
  LPos := @SubState.PointerRegister[Instruction.DataPlus];

  if HLVM_KadJIT_AllowDebugModeForLocals then
    LPos.Name := Instruction.StringContent;
  Pos^ := @LPos.Content;

  Result := Pos^;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_PushNil(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];
//  Pos^ := Instruction.RegisterPosition;
  Pos^ := @SubState.DataRegister[Instruction.RegisterPosition];
  Result := Pos^;

  Result^ := HLVM_NIL;

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_PushString(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var AResult: PHLVM_Variable;
    Pos: PPHLVMVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];
//  AResult := Instruction.RegisterPosition;
  AResult := @SubState.DataRegister[Instruction.RegisterPosition];
  Pos^ := AResult;
  Result := AResult;

  AResult.Typed := HLVM_Type_String;
  // AResult.SetString(Instruction.StringContent);
  AResult.Stringy := Instruction.StringContent;
  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_PushTrue(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
var Pos: PPHLVMVariable;
begin
//  Pos := Instruction.PointerRegisterPosition;
  Pos := @SubState.PointerRegister[Instruction.PointerRegisterPosition];
//  Pos^ := Instruction.RegisterPosition;
  Pos^ := @SubState.DataRegister[Instruction.RegisterPosition];

  Result := Pos^;
  with Result^ do begin
    Typed := HLVM_Type_Boolean;
    SetInt(1);
  end;

  SubState.CurrentInstruction := Instruction.NextInstruction;
end;

class function TJITCompiler.JIT_EXEC_PushVar(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
(*var Pos: PPHLVMVariable;
    i, VarPos: Integer;
    Registers: TLocalEnvironment;
    Reg: ^TLocalVariablesRegister;
    Name: String;
    Hash: Cardinal;*)
begin
  (*Pos := Instruction.PointerRegisterPosition;
  Pos^ := nil;

  Name := Instruction.StringContent;
  Hash := Cardinal(HashString(Name));

  Registers := SubState.LocalEnvironment;

  for i := SubState.LocalLevel downto 0 do begin
    { Checking all variable states }
    Reg := @Registers[i];
    VarPos := Reg.PositionOf(Hash, Name);
    if (VarPos >= 0) then begin
      Pos^ := @Reg.List[VarPos].Value;
      Result := Pos^;

      SubState.CurrentInstruction := Instruction.NextInstruction;
      Exit;
    end;
  end;

  { Not found local reference }
  if not (Substate.GlobalEnvironment = nil) then begin
    Pos^ := Substate.GlobalEnvironment.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(Name));
  end;

  if Pos^ = nil then begin
    Pos^ := @SubState.NullPointerStorage;
    Pos^^ := HLVM_NIL;
  end;

  Result := Pos^;
  SubState.CurrentInstruction := Instruction.NextInstruction;  *)

  raise EExecutionException.Create('Raw PushVar is not supported anymore! It is error of linking and KadJIT compiler');
end;

class function TJITCompiler.JIT_EXEC_PushVarForce(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
(*var Pos: PPHLVMVariable;
    i, VarPos: Integer;
    Registers: TLocalEnvironment;
    Reg: ^TLocalVariablesRegister;
    Name: String;
    Hash: Cardinal;*)
begin
  (*Pos := Instruction.PointerRegisterPosition;
  Pos^ := nil;

  Name := Instruction.StringContent;
  Hash := Cardinal(HashString(Name));

  Registers := SubState.LocalEnvironment;

  for i := SubState.LocalLevel downto 0 do begin
    { Checking all variable states }
    Reg := @Registers[i];
    VarPos := Reg.PositionOf(Hash, Name);
    if (VarPos >= 0) then begin
      Pos^ := @Reg.List[VarPos].Value;
      Result := Pos^;
      Exit;
    end;
  end;

  { Not found local reference }
  Pos^ := Substate.GlobalEnvironment.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(Name));
  if Pos^ = nil then
    Pos^ := SubState.GlobalEnvironment.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(Name));

  Result := Pos^;
  SubState.CurrentInstruction := Instruction.NextInstruction; *)

  raise EExecutionException.Create('Raw PushVar (Force) is not supported anymore! It is general linking and KadJIT fault');
end;

class function TJITCompiler.JIT_EXEC_SetBreak(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  with SubState do begin
//    Inc(BreakCount);
//    if BreakCount > BreakSize then begin
//      Inc(BreakSize, HLVM_BREAKS_EXPANSION_SIZE);
//      SetLength(Breaks, BreakSize);
//    end;
//    with Breaks[BreakCount - 1] do begin
//      BreakPos := PJITExecutionInstruction(Instruction.IntContent);
//      ContinuePos := nil;
//      ScopeID := LocalID;
//    end;
//    BreakID := LocalID;

    { With KadJIT it is not required }
    { All is done by linker }

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_EXEC_SetContinue(const SubState: TExecutionSubstate; const [Ref] Instruction: TJITExecutionInstruction): PHLVM_Variable;
begin
  Result := nil;
  with SubState do begin
//    if BreakCount = 0 then
//      raise EExecutionException.Create('SetContinue has violated the general label behavior (It should go after SetBreak)');
//    Breaks[BreakCount - 1].ContinuePos := PJITExecutionInstruction(Instruction.IntContent);

    { With KadJIT it is not required }
    { All is done by linker }

    CurrentInstruction := Instruction.NextInstruction;
  end;
end;

class function TJITCompiler.JIT_STATIC_VarToBoolean(const AVar: PHLVM_Variable): Boolean;
begin
  Result := not (AVar.Typed = HLVM_Type_Nil) and not ((AVar.Typed = HLVM_Type_Boolean) and not AVar.GetBoolean);
end;

{ TKadJIITExecutionContainer }

procedure TKadJIITExecutionContainer.Nullify;
begin
  SetLength(Instructions, 0);
  Header.Nullify;
end;

{ TKadJIITInstruction }

class procedure TKadJIITInstruction.EXEC_CloseScope(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  SubState.LocalLevel := Self.IntContent;
end;

class procedure TKadJIITInstruction.EXEC_DoNothing(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  { Do really nothing }
end;

class procedure TKadJIITInstruction.EXEC_ForCheckAsc(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { AdditionalPlusData is BreakPos to Jump [KadJIT update] }
      { ForType: 1 = Ascending; 0 = Descending } *)
  with SubState do begin
    with Fors[ForIndex] do begin
      Inc(Current, Self.IntContent);
      if Current > Finish then begin
        { Closing loop execution }
        Dec(ForIndex);
        InstructionPtr^ := Pointer(Self.DataPlus);
        { Returning }
        Exit;
      end;
      with ForVar^ do begin
        Typed := HLVM_Type_Integer;
        SetInt(Current);
      end;
    end;
  end;
end;

class procedure TKadJIITInstruction.EXEC_ForCheckDes(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { AdditionalPlusData is BreakPos to Jump [KadJIT update] }
      { ForType: 1 = Ascending; 0 = Descending } *)
  with SubState do begin
    with Fors[ForIndex] do begin
      Dec(Current, Self.IntContent);
      if Current < Finish then begin
        { Closing loop execution }
        Dec(ForIndex);
        InstructionPtr^ := Pointer(Self.DataPlus);
        { Returning }
        Exit;
      end;
      with ForVar^ do begin
        Typed := HLVM_Type_Integer;
        SetInt(Current);
      end;
    end;
  end;
end;

class procedure TKadJIITInstruction.EXEC_ForSetFinish(const Self: PInternalPointer; const SubState: TKadJIITSubState);
var State: PHLVM_Variable;
begin
  State := Self.PointerRegisterPosition^;

  if not (State.Typed = HLVM_Type_Integer) then
    raise EExecutionException.Create('For limit must be an integer.');

  with SubState do begin
    Fors[ForIndex].Finish := State.GetInt;
  end;
end;

class procedure TKadJIITInstruction.EXEC_ForSetStartAsc(const Self: PInternalPointer; const SubState: TKadJIITSubState);
var Start: PHLVM_Variable;
begin
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { ForType: 1 = Ascending; 0 = Descending } *)
  Start := Self.PointerRegisterPosition^;
  if not (Start.Typed = HLVM_Type_Integer) then
    raise EExecutionException.Create('For start must be an integer.');
  with SubState do begin
    Inc(ForIndex);
    if ForIndex >= ForSize then begin
      Inc(ForSize, HLVM_FORS_EXPANSION_SIZE);
      SetLength(Fors, ForSize);
    end;
    with Fors[ForIndex] do
      { Ascending }
      Current := Start.GetInt - Self.IntContent;
  end;
end;

class procedure TKadJIITInstruction.EXEC_ForSetStartDes(const Self: PInternalPointer; const SubState: TKadJIITSubState);
var Start: PHLVM_Variable;
begin
  (* Documentation:
      { Integer storage is ForStep }
      { AdditionalData is ForType }
      { ForType: 1 = Ascending; 0 = Descending } *)
  Start := Self.PointerRegisterPosition^;
  if not (Start.Typed = HLVM_Type_Integer) then
    raise EExecutionException.Create('For start must be an integer.');
  with SubState do begin
    Inc(ForIndex);
    if ForIndex >= ForSize then begin
      Inc(ForSize, HLVM_FORS_EXPANSION_SIZE);
      SetLength(Fors, ForSize);
    end;
    with Fors[ForIndex] do
      { Descending }
      Current := Start.GetInt + Self.IntContent;
  end;
end;

class procedure TKadJIITInstruction.EXEC_ForSetVar(const Self: PInternalPointer; const SubState: TKadJIITSubState);
var LPos: PLocalVariable;
begin
  with SubState do begin
    { KadJIT introduced static local variables }
    LPos := PLocalVariable(Self.DataPlus);
    Fors[ForIndex].ForVar := @LPos.Content;
    if HLVM_KadJIT_AllowDebugModeForLocals then
      LPos.Name := Self.StringContent;
  end;
end;

class procedure TKadJIITInstruction.EXEC_JC(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  with SubState do if CarryFlag then
    InstructionPtr^ := Pointer(Self.IntContent);
end;

class procedure TKadJIITInstruction.EXEC_JC_NULL(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  with SubState do if CarryFlag then
    InstructionPtr^ := nil;
end;

class procedure TKadJIITInstruction.EXEC_JMP(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  SubState.InstructionPtr^ := Pointer(Self.IntContent);
end;

class procedure TKadJIITInstruction.EXEC_JMP_NULL(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  SubState.InstructionPtr^ := nil;
end;

class procedure TKadJIITInstruction.EXEC_JNC(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  with SubState do if not CarryFlag then
    InstructionPtr^ := Pointer(Self.IntContent);
end;

class procedure TKadJIITInstruction.EXEC_JNC_NULL(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  with SubState do if not CarryFlag then
    InstructionPtr^ := nil;
end;

class procedure TKadJIITInstruction.EXEC_OpenScope(const Self: PInternalPointer; const SubState: TKadJIITSubState);
begin
  SubState.LocalLevel := Self.IntContent;
end;

class procedure TKadJIITInstruction.EXEC_PushInt(const Self: PInternalPointer; const SubState: TKadJIITSubState);
var Pos: PPHLVMVariable;
begin
  Pos := Self.PointerRegisterPosition;
  Pos^ := Self.RegisterPosition;
  with Pos^^ do begin
    Typed := HLVM_Type_Integer;
    SetInt(Self.IntContent);
  end;
end;

procedure TKadJIITInstruction.Nullify;
begin
  OPCode := Command_NoOperation;

  RegisterPosition := nil;
  PointerRegisterPosition := nil;

  IntContent := 0;
  StringContent := '';
  FloatContent := 0;

  Data := 0;
  DataPlus := 0;
  DataPointer := nil;

  Compute := nil;
end;

{ TKadJIITCompiler }

class function TKadJIITCompiler.JITCompileBytecode(const AInput: THLVMBytecodeSentence; const DecrementLabels: Boolean = False): TKadJIITExecutionContainer;

  function CountCommands(const ACmd: HLVM_LinkerWord): Integer;
  var i: Integer;
  begin
    Result := 1;
    for i := 0 to ACmd.AbstractOperands.Count - 1 do
      Result := Result + CountCommands(AInput.InternalStorage[ACmd.AbstractOperands.Items[i]]);
  end;

var Step, LabelStep: Integer;
    Labels: TLabelArray;

  procedure CompileCommand(const ACmd: HLVM_LinkerWord; const Initial: Boolean = False);
  var i: Integer;
  begin
    for i := 0 to ACmd.AbstractOperands.Count - 1 do
      CompileCommand(AInput.InternalStorage[ACmd.AbstractOperands.Items[i]]);
    TKadJIITCompiler.JITCompileInstruction(ACmd, Result.Instructions[Step]);
    if Initial then begin
      with Labels[LabelStep] do begin
        Index := Step;
        LabelIndex := ACmd.LabeledIndex;
      end;
      Inc(LabelStep);
    end;
    Inc(Step);
  end;

var Count, i: Integer;
begin
  Result.Nullify;

  { KadJIT BeHeading upgrade (v1.6) }
//  Result.Header := AInput.Header;

  Count := 0;
  for i := 0 to AInput.ActualStorage.Count - 1 do
    Count := Count + CountCommands(AInput.InternalStorage[AInput.ActualStorage[i]]);
  { JMP_NULL! }
  Count := Count + 1;

  SetLength(Result.Instructions, Count);
  SetLength(Labels, Count - 1);
  Step := 0;
  LabelStep := 0;
  for i := 0 to AInput.ActualStorage.Count - 1 do
    CompileCommand(AInput.InternalStorage[AInput.ActualStorage[i]], True);

  with Result.Instructions[Count - 1] do begin
    Nullify;

    OPCode := JIT_Command_JMP_NULL;
    Compute := EXEC_JMP_NULL;
  end;

  TKadJIITCompiler.JITLinkBytecode(AInput, Result, Labels, DecrementLabels);
end;

class procedure TKadJIITCompiler.JITCompileInstruction(const ABytecodeInstruction: HLVM_LinkerWord; var Result: TKadJIITInstruction);
begin
  Result.Nullify;
  Result.OPCode := ABytecodeInstruction.CommandID;
  with Result, ABytecodeInstruction do begin
    StringContent := StringStorage;
    IntContent := IntegerStorage;
    FloatContent := FloatStorage;
    Data := AdditionalData;
    DataPlus := AdditionalPlusData;
  end;

  case Result.OPCode of
    Command_PushInteger:
      Result.Compute := Result.EXEC_PushInt;

    Command_JMP, Command_JC, Command_JNC:
      { Jumps are not processed here. };

    Command_ForSetStart:
      case Result.Data and 1 of
        0: begin
          Result.OPCode := JIT_Command_ForSetStartDes;
          Result.Compute := Result.EXEC_ForSetStartDes;
        end;
        1: begin
          Result.OPCode := JIT_Command_ForSetStartAsc;
          Result.Compute := Result.EXEC_ForSetStartAsc;
        end;
      end;
    Command_ForCheck:
      case Result.Data and 1 of
        0: begin
          Result.OPCode := JIT_Command_ForCheckDes;
          Result.Compute := Result.EXEC_ForCheckDes;
        end;
        1: begin
          Result.OPCode := JIT_Command_ForCheckAsc;
          Result.Compute := Result.EXEC_ForCheckAsc;
        end;
      end;
    Command_ForSetFinish:
      Result.Compute := Result.EXEC_ForSetFinish;
    Command_ForSetVar:
      Result.Compute := Result.EXEC_ForSetVar;

    Command_SetBreak, Command_SetContinue,
    Command_ForSetType, Command_ForSetStep:
      begin
        Result.OPCode := Command_NoOperation;
        Result.Compute := Result.EXEC_DoNothing;
      end;

    Command_OpenScope:
      Result.Compute := Result.EXEC_OpenScope;
    Command_CloseScope:
      Result.Compute := Result.EXEC_CloseScope;
  else
    raise EJITCompilationException.Create('KadJIIT failed to compile instruction: $' + IntToStr(Result.OPCode));
  end;
end;

class procedure TKadJIITCompiler.JITLinkBytecode(const AInput: THLVMBytecodeSentence; var Container: TKadJIITExecutionContainer; const Labels: TLabelArray; const DecrementLabels: Boolean = False);

  function ResolveLabel(const Pos: Integer): Integer;
  var i: Integer;
  begin
    Result := -1;
    for i := 0 to Length(Labels) - 1 do
      if Pos <= Labels[i].LabelIndex then begin
        Result := Labels[i].Index;
        Break;
      end;
  end;

var i, Content, Pos: Integer;
    Instr: PKadJIITInstruction;
begin
  for i := 0 to Length(Container.Instructions) - 2 do begin
    Instr := @Container.Instructions[i];
    case Instr.OPCode of
      Command_JMP, Command_JC, Command_JNC,
      Command_SetBreak, Command_SetContinue,
      Command_BreakLoop, Command_ContinueLoop: begin
        { We need to go deeper! }
        Content := Instr.IntContent;
        if Content < 0 then
          raise EJITCompilationException.Create('Linking label instruction got an exception: Not valid position to jump.');
        Pos := ResolveLabel(Content);
        if Pos < 0 then begin 
          { This way we no label can be used to JMP. Therefore we jump to NULL }
          case Instr.OPCode of
            Command_JMP: with Instr^ do begin
              OPCode := JIT_Command_JMP_NULL;
              Compute := EXEC_JMP_NULL;
            end;
            Command_JC: with Instr^ do begin
              OPCode := JIT_Command_JC_NULL;
              Compute := EXEC_JC_NULL;
            end;
            Command_JNC: with Instr^ do begin
              OPCode := JIT_Command_JNC_NULL;
              Compute := EXEC_JNC_NULL;
            end;
            Command_SetBreak, Command_SetContinue:
              Instr.IntContent := HLVM_Integer(nil);
            Command_BreakLoop, Command_ContinueLoop:
              Instr.IntContent := HLVM_Integer(nil);
          end;
        end
        else begin 
          // Instr.IntContent := HLVM_Integer(Container.ActualStorage[k]);
          Instr.IntContent := HLVM_Integer(@Container.Instructions[Pos]);
          if DecrementLabels then
            Dec(Instr.IntContent, SizeOf(TKadJIITInstruction));
          case Instr.OPCode of
            Command_JMP:
              Instr.Compute := Instr.EXEC_JMP;
            Command_JC:
              Instr.Compute := Instr.EXEC_JC;
            Command_JNC:
              Instr.Compute := Instr.EXEC_JNC;
          end;
        end;
        
      end;

      Command_ForCheck, JIT_Command_ForCheckAsc, JIT_Command_ForSetStartDes: begin
        { Data+ is position to break }
        Content := Instr.DataPlus;
        if Content < 0 then
          raise EJITCompilationException.Create('Linking Break instruction got an exception: Not valid position to jump.');
        Pos := ResolveLabel(Content);
        if Pos < 0 then begin
          Instr.DataPlus := 0;
        end
        else begin
          // Instr.DataPlus := Int64(Container.ActualStorage[k]);
          Instr.DataPlus := HLVM_Integer(@Container.Instructions[Pos]);
          if DecrementLabels then
            Dec(Instr.DataPlus, SizeOf(TKadJIITInstruction));
        end;
      end;
    end;
  end;

  { Linking and checking scopes... }
  Content := 0;
  for i := 0 to Length(Container.Instructions) - 2 do begin
    { Last command ID is undefined!!! }
    { Because it is INTERNAL JIT OPERATION BY SPECIFICATION!!! }
    case Container.Instructions[i].OPCode of
      Command_OpenScope: Content := Content + 1;
      Command_CloseScope: Content := Content - 1;
    end;
    if Content < 0 then
      raise EJITCompilationException.Create('Scopes closures are violated. `CloseScope` is not closing the relative `OpenScope`');
  end;
  if not (Content = 0) then
    raise EJITCompilationException.Create('Scopes are violated. Not all `OpenScope` are closed with `CloseScope`');
end;

{ TKadJIITSubState }

procedure TKadJIITSubState.ExecuteFunctional;
var Instr: PKadJIITInstruction;
begin
  ProcessHeader;
  Link;

  Instr := @ExecutionContainer.Instructions[0];
  InstructionPtr := @Instr;
  Dec(Instr);
  while not (Instr = nil) do begin
    Inc(Instr);
    Instr.Compute(Instr, Self);
  end;
end;

procedure TKadJIITSubState.ExecuteInline;
label Loop;
var Instr: PKadJIITInstruction;
    OPCode: NativeInt;
begin
  ProcessHeader;
  Link;

  Instr := @ExecutionContainer.Instructions[0];
  InstructionPtr := @Instr;

  Dec(Instr);
  Loop:
  if not (Instr = nil) then begin
    Inc(Instr);
    OPCode := Instr.OPCode;
    case OPCode of
      Command_PushInteger:
        begin
          TKadJIITInstruction.EXEC_PushInt(Instr, Self);
//          Continue;
          goto Loop;
        end;

      Command_JMP:
        begin
          TKadJIITInstruction.EXEC_JMP(Instr, Self);
//          Continue;
          goto Loop;
        end;
      Command_JC:
        begin
          TKadJIITInstruction.EXEC_JC(Instr, Self);
//          Continue;
          goto Loop;
        end;
      Command_JNC:
        begin
          TKadJIITInstruction.EXEC_JNC(Instr, Self);
//          Continue;
          goto Loop;
        end;
      JIT_Command_JMP_NULL:
        begin
          TKadJIITInstruction.EXEC_JMP_NULL(Instr, Self);
//          Continue;
          goto Loop;
        end;
      JIT_Command_JC_NULL:
        begin
          TKadJIITInstruction.EXEC_JC_NULL(Instr, Self);
//          Continue;
          goto Loop;
        end;
      JIT_Command_JNC_NULL:
        begin
          TKadJIITInstruction.EXEC_JNC_NULL(Instr, Self);
//          Continue;
          goto Loop;
        end;

      JIT_Command_ForSetStartAsc:
        begin
          TKadJIITInstruction.EXEC_ForSetStartAsc(Instr, Self);
//          Continue;
          goto Loop;
        end;
      JIT_Command_ForSetStartDes:
        begin
          TKadJIITInstruction.EXEC_ForSetStartDes(Instr, Self);
//          Continue;
          goto Loop;
        end;

      JIT_Command_ForCheckAsc:
        begin
          TKadJIITInstruction.EXEC_ForCheckAsc(Instr, Self);
//          Continue;
          goto Loop;
        end;
      JIT_Command_ForCheckDes:
        begin
          TKadJIITInstruction.EXEC_ForCheckDes(Instr, Self);
//          Continue;
          goto Loop;
        end;

      Command_ForSetFinish:
        begin
          TKadJIITInstruction.EXEC_ForSetFinish(Instr, Self);
//          Continue;
          goto Loop;
        end;
      Command_ForSetVar:
        begin
          TKadJIITInstruction.EXEC_ForSetVar(Instr, Self);
//          Continue;
          goto Loop;
        end;

      Command_NoOperation:
//        Continue;
        goto Loop;

      Command_SetBreak, Command_SetContinue,
      Command_ForSetType, Command_ForSetStep:
        begin
          // TKadJIITInstruction.EXEC_DoNothing(Instr, Self);
//          Continue;
          goto Loop;
        end;

      Command_OpenScope:
        begin
          TKadJIITInstruction.EXEC_OpenScope(Instr, Self);
//          Continue;
          goto Loop;
        end;
      Command_CloseScope:
        begin
          TKadJIITInstruction.EXEC_CloseScope(Instr, Self);
//          Continue;
          goto Loop;
        end;
    else
      raise EJITCompilationException.Create('KadJIIT [interpreter] failed to compile instruction: $' + IntToStr(Instr.OPCode));
    end;
  end;
end;

procedure TKadJIITSubState.Link;
var i: Integer;
begin
  if Linked then
    Exit;
  { Link all variables position }
  for i := 0 to Length(ExecutionContainer.Instructions) - 2 do begin
    with ExecutionContainer.Instructions[i] do begin
      RegisterPosition := @DataRegister[Integer(RegisterPosition)];
      PointerRegisterPosition := @PointerRegister[Integer(PointerRegisterPosition)];
    end;
  end;

  { Variables }
  for i := 0 to Length(ExecutionContainer.Instructions) - 2 do begin
    with ExecutionContainer.Instructions[i] do begin
      case OPCode of
        Command_PushVar: begin
          if not (Data and ScoperFlag_Var_PushKnownLocal = 0) then
            DataPlus := Int64(@LocalScope[DataPlus])
          else if not (Data and ScoperFlag_Var_PushKnownGlobal = 0) then
            DataPlus := Int64(@GlobalScope[DataPlus]);
        end;
        Command_ForSetVar: begin
          DataPlus := Int64(@LocalScope[DataPlus]);
        end;
      end;
    end;
  end;
  Linked := True;
end;

procedure TKadJIITSubState.Nullify;
begin
  LocalLevel := 0;

  { It is -1 }
  ForIndex := -1;

  { Registers }
  SetLength(DataRegister, HLVM_REGISTER_SIZE);
  SetLength(PointerRegister, HLVM_POINTED_REGISTER_SIZE);
end;

procedure TKadJIITSubState.ProcessHeader;
var i, PosGlobals: Integer;
    HeaderEntry: THLVMHeaderEntry;
    Ptr: PHLVM_Variable;
begin
  PosGlobals := 0;
  for i := 0 to ExecutionContainer.Header.Count - 1 do begin
    HeaderEntry := ExecutionContainer.Header[i];
    case HeaderEntry.Code of
      JIT_Header_NULL: ;
      JIT_Header_LocalHeapSize: begin
        if HeaderEntry.IntStorage >= 0 then
          SetLength(LocalScope, HeaderEntry.IntStorage)
        else
          raise EExecutionException.Create('Invalid value in LocalHeapSize header!');
      end;
      JIT_Header_GlobalCacheSize: begin
        if HeaderEntry.IntStorage >= 0 then
          SetLength(GlobalScope, HeaderEntry.IntStorage)
        else
          raise EExecutionException.Create('Invalid value in GlobalCacheSize header!');
      end;
      JIT_Header_CacheGlobal: begin
        { Try to find this variable in heap }
        Ptr := GlobalEnv.GetEntry(HLVM_Type_String, HLVM_Variable.NewString(HeaderEntry.StringStorage));
        if Ptr = nil then
          { Have not prevailed - insert new one. }
          Ptr := GlobalEnv.InsertEntry(HLVM_Type_String, HLVM_Variable.NewString(HeaderEntry.StringStorage));
        with GlobalScope[PosGlobals] do begin
          Name := HeaderEntry.StringStorage;
          Reference := Ptr;
        end;
      end;
    end;
  end;
end;

end.
