unit HLVM.Runtime.Machine;

interface

uses
  HLVM.Memory.Manager,
  HLVM.Runtime.Types,
  HLVM.Generics.Collections,
  HLVM.Types.Containers,
  HLVM.Static.Command,
  HLVM.Types.Stack,
  HLVM.Types.Utilities,
  HLVM.Types.Ordinal,
  HLVM.Memory.Space,
  HLVM.Types.AoT,
  HLVM.Types.Reader,
  HLVM.Runtime.Hashes,
  HLVM.Runtime.RuntimeMonster,
  DScript.Compiler,
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.Diagnostics,
  System.Hash,
  HCmdAPI.CmdIO,
  System.Threading;

type
  EExecutionException = class(Exception);

type
  TState = class;
  TSubState = class;

  TStates = TObjectList<TState>;
  TSubStates = TObjectList<TSubState>;

  { Machine main class! }
  TMachine = class(TObject)
  public
    Metatable_Number: TMetatable;
    Metatable_String: TMetatable;
    Metatable_Boolean: TMetatable;
    Metatable_Function: TMetatable;
    { Common table metatable }
    Metatable_Table: TMetatable;

    function GetMetatableFor(AType: HLVM_Types): TMetatable; inline;
  public
    MemoryManager: TMemoryManager;
    Assigner: TAssigner;
    States: TStates;
    CriticalSection: TCriticalSection;
    FunctionStackCounter: Integer;

    PrimaryEnvironment: TExpandedBaseEnvironment;

    AShowInstructions: Boolean;
  public
    function GetNextFunctionCounter: Integer;
  public
    FunctionsStorage: THLVM_AoT_FunctionContainers;
    AoTCompiler: THLVM_BytecodeAoTCompiler;
    procedure RegisterExternalFunctions(const Functions: THLVM_AoT_FunctionContainers);
    procedure PushBuiltInFunction(const Name: String; const Func: TBuiltInFunction);
    procedure RegisterBasics;
    procedure RegisterMetas;
  public
    procedure ExecuteRawChunk(const RawChunk: THLVMRawChunk);
    procedure ExecuteStreamBytecode(const Stream: TStream);
    procedure ExecuteTextChunk(const Text: String);

    procedure DestroyItems(const AArray: TTableArray);
  public
//    GlobalEnvTable: TRunTimeVariable_Table;
//    GlobalEnvironment: PEnvironment;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TState = class(TObject)
  public var
    Machine: TMachine;
    SubStates: TSubStates;
  public
    procedure CallAsync(const SubState: TSubState);
  public
    constructor Create(const Parent: TMachine);
    destructor Destroy; override;
  end;
  (* TSubState Executor Class *)
  TSubState = class(TObject)
  public var
    MemoryManager: PMemoryManager;
    Assigner: PAssigner;
    State: TState;
//    StateStack: TStack;
    StateStack: TFastManagedStack<TStackContainer>;
    PrimaryEnviroment: TExpandedBaseEnvironment;
//    Environment: PEnvironment;
    Environment: PNewEnvironment;
  public var 
    ValueAsNative: Boolean;
  public
    function GetMetatableOf(var Shell: TVariableShell): TMetatable; inline;
    function GetMetamethodOf(var Shell: TVariableShell; const MTMethod: Integer): PVariableShell; overload; inline;
    function GetMetamethodOf(var AFShell, ASShell: TVariableShell; const MTMethod: Integer): PVariableShell; overload; inline;
  public
    function RoughToString(const [ref] Shell: TVariableShell): String; // inline;

    procedure PerformCall(var Shell: TVariableShell; const LongCall: Boolean = False); overload; inline;
    procedure PerformCall(const LongCall: Boolean); overload; inline;
    function PerformSimpleCall(var Shell: TVariableShell): TVariableShell; inline;
    procedure ClearStack; inline;
    procedure ClearStackForcefully;
    procedure OpenScope; //inline;
    procedure CloseScope; //inline;
  public
    { Methods we need to execute! }
    procedure PushVar(const Name: String; const Hash: Integer); //inline;
    procedure PushLocal(const Name: String; const Hash: Integer); //inline;
    procedure PushInteger(const Content: HSInteger); //inline;
    procedure PushFloat(const Content: HSFloat); //inline;
    procedure PushString(const Content: String); //inline;
    procedure PushBoolean(const Content: Boolean); //inline;
    procedure PushNil; inline;

    procedure CheckStack(const Invisible: Boolean); //inline;

    procedure SetBreak(const Mark: Integer); //inline;
    procedure SetContinue(const Mark: Integer); //inline;
    function DoBreak: Integer; //inline;
    function DoContinue: Integer; //inline;

    procedure ForSetType(const Ascending: Boolean);// inline;
    procedure ForSetStep(const Step: Integer); //inline;
    procedure ForSetStart; //inline;
    procedure ForSetFinish; //inline;
    procedure ForSetVar(const Name: String);// inline;
    function CheckFor: Integer; //inline;

    procedure PerformAssign;
    procedure PerformAssignLocalReference(const Name: String);
    procedure PerformPushFunction(const ID: Integer);

    procedure PerformMarkAsNative;
    procedure PerformDoMonumentalPointer;
                                     
    procedure PerformAdd; //inline;

    procedure PerformIndex;
  public
    procedure Execute(const Bytecode: THLVMBytecodeChunk); //inline;
  public
    procedure PerformReset;
    procedure DestroyItemsArray(const AArray: TTableArray);

    procedure ThrowExecutionException(const AMsg: String; const AType: HLVM_Types);

    constructor Create(const Parent: TState);
    destructor Destroy; override;
  end;

implementation

{ TSubState }

function TSubState.CheckFor: Integer;
begin
  with Environment^ do begin
    if ForAscending then
      ForStart := ForStart + ForStep
    else
      ForStart := ForStart - ForStep;
    ForVar.Content := ForStart;
    PVariableShell(ForShell).Referenced := nil;
    PVariableShell(ForShell).Content := @ForVar;
    PVariableShell(ForShell).TypeOf := Type_Integer;
    if (ForAscending and (ForStart > ForFinish)) or (not ForAscending and (ForFinish > ForStart)) then
      Result := Environment.BreakPos
    else
      Result := -1;
  end;
end;

procedure TSubState.CheckStack(const Invisible: Boolean);
var Shell: PVariableShell;
begin
  if Invisible then
//    Shell := StateStack.MemoryStack.GetAsPointer(StateStack.MemoryStack.LastPos).GetShell
    Shell := StateStack.SP.GetShell
  else
    Shell := StateStack.PickAddress.GetShell;
  Environment.CarryFlag := not (Shell.TypeOf = Type_Nil) and not ((Shell.TypeOf = Type_Boolean) and not PHBoolean(Shell.Content).Content);
  if not Invisible then Assigner.DestroyShell(Shell^);
end;

procedure TSubState.ClearStack;
//var {i: Integer;}
//    Shell: PVariableShell;
begin
//  for i := 1 to StateStack.GetSafeCount do
    //Assigner.DestroyShell(StateStack.Pick.GetShell^);
  with StateStack do while (Integer(SP) >= Integer(LowBound)) do begin
//    Assigner.DecrementVariableReferenceCounter(StateStack.Pick.GetShell^);
//    Shell := StateStack.SP.GetShell;
//    Assigner.DecrementVariableReferenceCounter(Shell^);
    Assigner.DestroyShell(SP.GetShell^);
    Dec(SP);
  end;
end;

procedure TSubState.ClearStackForcefully;
//var i: Integer;
begin
//  for i := 1 to StateStack.MemoryStack.Count do
//    Assigner.DestroyShell(StateStack.Pick.GetShell^);
  while Integer(StateStack.SPs.SP) >= Integer(StateStack.SPs.Base) do
    StateStack.RestoreStackPointer;
  while Integer(StateStack.SP) >= Integer(StateStack.Base) do
    Assigner.DecrementVariableReferenceCounter(StateStack.Pick.GetShell^);
end;

procedure TSubState.CloseScope;
var Scope: PNewEnvironment;
begin
  Scope := PNewEnvironment(Environment.Parent);
//  Environment.Free;
//  Tabl := Environment.Table;
  Environment.DecrementRefCounter(1);
//  if Tabl.RefCounter <= 0 then
//    MemoryManager.DisposeTable(Tabl.ID);
  Environment := Scope;
  if not (Scope = nil) then
    Environment.SetupEnvironment;
end;

constructor TSubState.Create(const Parent: TState);
begin
  State := Parent;
  MemoryManager := @Parent.Machine.MemoryManager;
  Assigner := @Parent.Machine.Assigner;
  PrimaryEnviroment := TExpandedBaseEnvironment.Create(Parent.Machine.PrimaryEnvironment);
//  Environment := PEnvironment(TEnvironment.Create(Self.MemoryManager.AllocateRawTable(0), TEnvironment.SelfPointer(State.Machine.GlobalEnvironment)));
  Environment := PNewEnvironment(TNewEnvironment.Create(PrimaryEnviroment, nil, DestroyItemsArray));
  Environment.SetupEnvironment;
//  StateStack.CurrentStackPointer := -1;
  StateStack.Init;
end;

destructor TSubState.Destroy;
begin
  while not (Environment = nil) do
    CloseScope;
  PrimaryEnviroment.Free;
  inherited;
end;

procedure TSubState.DestroyItemsArray(const AArray: TTableArray);
var i: Integer;
begin
  // Writeln('Destroyed env;');
  for i := 0 to Length(AArray) - 1 do begin
    with AArray[i] do begin
      { If hash is empty }
      if (HashCode = -1) or not (Value.Referenced = nil) then Continue;
      try
        Assigner.DestroyShell(Value);
      except
        on E: Exception do
          Writeln(E.Message);
      end;
    end;
  end;
end;

function TSubState.DoBreak: Integer;
var Env: PNewEnvironment;
    Count: Integer;
    i: Integer;
begin
  Env := Environment;
  Count := 0;
  while not (Env = nil) and (Env.BreakPos <= 0) do begin
    Env := PNewEnvironment(Env.Parent);
    Count := Count + 1;
  end;
  if (Env = nil) then
    raise EExecutionException.Create('Break must be placed in loop!');
  for i := 1 to Count do
    CloseScope;
  Result := Environment.BreakPos;
end;

function TSubState.DoContinue: Integer;
var Env: PNewEnvironment;
    Count: Integer;
    i: Integer;
begin
  Env := Environment;
  Count := 0;
  while not (Env = nil) and (Env.ContinuePos <= 0) do begin
    Env := PNewEnvironment(Env.Parent);
    Count := Count + 1;
  end;
  if (Env = nil) then
    raise EExecutionException.Create('Continue must be placed in loop!');
  for i := 1 to Count do
    CloseScope;
  Result := Environment.ContinuePos;
end;

procedure TSubState.Execute(const Bytecode: THLVMBytecodeChunk);
var CommandLet: ^TUniversalContainer;
    {Pos, }Len, Temp, Opened, StackMoves: Integer;
    Executing: Boolean;
//    Watch: TStopwatch;
    CommandLets: array of TUniversalContainer;
    i, Overall, HB: Integer;
begin
  Len := Bytecode.Count;
//  Pos := 0;
  Executing := True;
  SetLength(CommandLets, Bytecode.Count);
  for i := 0 to Bytecode.Count - 1 do
    CommandLets[i] := Bytecode[i];
  Overall := 0;
  if Bytecode.Count > 0 then
    CommandLet := @CommandLets[0]
  else
    Exit;
  HB := Integer(@CommandLets[Len - 1]);

  StateStack.MoveStackPointer;
  Opened := 0;
  StackMoves := 1;
  
  while (Integer(CommandLet) <= HB) and Executing do begin
//    Watch := TStopwatch.StartNew;
//    for i := 1 to 10000000 do

//    Writeln(Watch.ElapsedMilliseconds, ' to read commands');
    Environment.CurrentPosition := @CommandLet.Position;

    case CommandLet.Command of
      Command_PushVar: PushVar((CommandLet).StringContent, (CommandLet).IntContent);
      Command_PushLocal: PushLocal((CommandLet).StringContent, (CommandLet).IntContent);

      Command_ClearStack: ClearStack;
      Command_MoveStackPointer: begin
        StateStack.MoveStackPointer;
        Inc(StackMoves);
      end;
      Command_RestoreStackPointer: begin
        {StateStack.ReturnStackPointer} StateStack.RestoreStackPointer;
        Dec(StackMoves);
      end;

      Command_PushString: PushString((CommandLet).StringContent);
      Command_PushInteger: PushInteger((CommandLet).IntContent);
      Command_PushReal: PushFloat((CommandLet).FloatContent);
      Command_PushNil: PushNil;
      Command_PushTrue, Command_PushFalse: PushBoolean(CommandLet.Command = Command_PushTrue);

      Command_ArADD: PerformAdd;

      Command_Call, Command_SimpleCall: PerformCall(CommandLet.Command = Command_Call);

      Command_JMP: begin
//        Pos := (CommandLet).IntContent;
        CommandLet := @CommandLets[CommandLet.IntContent];
//        Pos := Pos - 1;
        Continue;
      end;
      Command_JC: if Environment.CarryFlag then begin
//        Pos := (CommandLet).IntContent;
        CommandLet := @CommandLets[CommandLet.IntContent];
        Continue;
      end;
      Command_JNC: if not Environment.CarryFlag then begin
//        Pos := (CommandLet).IntContent;
        CommandLet := @CommandLets[CommandLet.IntContent];
        Continue;
      end;

      Command_OpenScope: begin
        OpenScope;
        Inc(Opened);
      end;
      Command_CloseScope: begin
        CloseScope;
        Dec(Opened);
      end;

      Command_ForSetType: ForSetType(not ((CommandLet).IntContent = 0));
      Command_ForSetStep: ForSetStep((CommandLet).IntContent);
      Command_ForSetStart: ForSetStart;
      Command_ForSetFinish: ForSetFinish;
      Command_ForSetVar: ForSetVar((CommandLet).StringContent);
      Command_ForCheck: begin
        Temp := CheckFor;
        if Temp >= 0 then begin
//          Pos := Temp;
          CommandLet := @CommandLets[Temp];
          Continue;
        end;
      end;

      Command_SetBreak: SetBreak((CommandLet).IntContent);
      Command_SetContinue: SetContinue((CommandLet).IntContent);

      Command_BreakLoop: begin
//        Pos := DoBreak;
        CommandLet := @CommandLets[DoBreak];
        Continue;
      end;
      Command_ContinueLoop: begin
//        Pos := DoContinue;
        CommandLet := @CommandLets[DoContinue];
        Continue;
      end;

      Command_CheckStack, Command_CheckStackInvisible: CheckStack(CommandLet.Command = Command_CheckStackInvisible);

      Command_Assign: PerformAssign;
      Command_AssignLocalReference: PerformAssignLocalReference(CommandLet.StringContent);

      Command_Internal_PushFunction: PerformPushFunction(CommandLet.IntContent);
      Command_Return: Break;

      Command_Index: begin
        PerformIndex;
      end;

      Command_NoOperation: ;

      Command_MarkAsNative: PerformMarkAsNative;
      Command_DoMonumentalPointer: PerformDoMonumentalPointer;
    else
      raise EExecutionException.Create('Unknown bytecode operation: ' + IntToStr(CommandLet.Command));
    end;

//    NormalisedEnvironment.CurrentPosition := CommandLet.Position;
    {$REGION OBSOLETE}
    {case CommandLet.Command of
      Command_PushVar:    TExecutor.PushValue(Self, (CommandLet as THLVMCommandLetContainer_String).Content);
      Command_PushLocal:  TExecutor.PushLocal(Self, (CommandLet as THLVMCommandLetContainer_String).Content);

      Command_ClearStack: TExecutor.ClearStack(Self);
      Command_MoveStackPointer: TExecutor.MoveStackPointer(Self);
      Command_RestoreStackPointer: TExecutor.RestoreStackPointer(Self);

      Command_PushString: TExecutor.PushString(Self, (CommandLet as THLVMCommandLetContainer_String).Content);
      Command_PushInteger: TExecutor.PushInteger(Self, (CommandLet as THLVMCommandLetContainer_Integer).Content);
      Command_PushReal: TExecutor.PushReal(Self, (CommandLet as THLVMCommandLetContainer_Float).Content);
      Command_PushNil: TExecutor.PushNil(Self);
      Command_PushTrue, Command_PushFalse: TExecutor.PushBoolean(Self, CommandLet.Command = Command_PushTrue);

      Command_ArADD: TExecutor.Perform_Add(Self);
      Command_ArSUB: TExecutor.Perform_Sub(Self);
      Command_ArMUL: TExecutor.Perform_Mul(Self);
      Command_ArDIV: TExecutor.Perform_Div(Self);

      Command_LgAND: TExecutor.Perform_AND(Self);
      Command_LgOR: TExecutor.Perform_OR(Self);
      Command_LgXOR: TExecutor.Perform_XOR(Self);
      Command_LgNOT: TExecutor.Perform_NOT(Self);

      Command_BtAND: TExecutor.Perform_BitwiseAND(Self);
      Command_BtOR:  TExecutor.Perform_BitwiseOR (Self);
      Command_BtXOR: TExecutor.Perform_BitwiseXOR(Self);
      Command_BtNOT: TExecutor.Perform_BitwiseNOT(Self);

      Command_Call, Command_SimpleCall: TExecutor.Call(Self, CommandLet.Command = Command_Call);
      Command_JMP: begin
        Pos := Integer((CommandLet as THLVMCommandLetContainer_Integer).Content);
        Continue;
      end;
      Command_JC: if NormalisedEnvironment.BooleanFlag then begin
        Pos := Integer((CommandLet as THLVMCommandLetContainer_Integer).Content);
        Continue;
      end;
      Command_JNC:  if not NormalisedEnvironment.BooleanFlag then begin
        Pos := Integer((CommandLet as THLVMCommandLetContainer_Integer).Content);
        Continue;
      end;

      Command_CheckStack: TExecutor.CheckStack(Self);
      Command_CheckStackInvisible: TExecutor.CheckStackInvisible(Self);

      Command_LABEL: raise EExecutorException.Create('Bytecode is not AOT compiled! Found [LABEL] instruction!');

      Command_Duplicate: TExecutor.Duplicate(Self);
      Command_Assign: TExecutor.PerformAssign(Self);
      Command_AssignPointer: TExecutor.PerformAssignPointed(Self);
      Command_AssignLocalReference: TExecutor.PerformAssignLocalReference(Self, (CommandLet as THLVMCommandLetContainer_String).Content);

      Command_ConsumeArguments: TExecutor.ConsumeArguments(Self);
      Command_PushLongArguments, Command_PushShortArguments: TExecutor.PushArguments(Self, CommandLet.Command = Command_PushLongArguments);

      Command_OpenScope: begin
        TExecutor.OpenScope(Self);
        NormalisedEnvironment.ChunkName := Bytecode.NameOfChunk;
      end;
      Command_CloseScope: TExecutor.CloseScope(Self);

      Command_SetBreak: NormalisedEnvironment.LabelBreak := Integer((CommandLet as THLVMCommandLetContainer_Integer).Content);
      Command_SetContinue: NormalisedEnvironment.LabelContinue := Integer((CommandLet as THLVMCommandLetContainer_Integer).Content);

      Command_BreakLoop: begin
        Pos := TExecutor.BreakLoop(Self);
        Continue;
      end;
      Command_ContinueLoop: begin
        Pos := TExecutor.ContinueLoop(Self);
        Continue;
      end;

      Command_Equals: TExecutor.CheckEqual(Self);
      Command_GreaterThan: TExecutor.CheckGreater(Self);
      Command_GreaterEqual: TExecutor.CheckGreaterEqual(Self);

      Command_CreateTable: TExecutor.CreateTable(Self);
      Command_CreateRunTimeTable: TExecutor.CreateRunTimeTable(Self);
      Command_Index: TExecutor.Index(Self);
      Command_NewIndex: TExecutor.NewIndex(Self);

      Command_ForSetStart: TExecutor.ForSetStart(Self);
      Command_ForSetFinish: TExecutor.ForSetFinish(Self);
      Command_ForSetType: TExecutor.ForSetType(Self, Boolean((CommandLet as THLVMCommandLetContainer_Integer).Content and 1));
      Command_ForSetVar: TExecutor.ForSetVar(Self, (CommandLet as THLVMCommandLetContainer_String).Content);
      Command_ForSetStep: TExecutor.ForSetStep(Self, (CommandLet as THLVMCommandLetContainer_Integer).Content);
      Command_ForCheck: begin
        if TExecutor.ForNext(Self) then begin
          Pos := TExecutor.BreakLoop(Self);
          Continue;
        end;
      end;

      Command_Internal_PushFunction: TExecutor.Internal_PushFunction(Self, Integer((CommandLet as THLVMCommandLetContainer_Integer).Content));

      Command_Return: Executing := False;
    else
      raise EExecutorException.Create('Unknown operation code to execute - Is this bytecode broken? [OPCODE: ' + IntToStr(CommandLet.Command) + ']');
    end;     }
    {$ENDREGION}
//    Pos := Pos + 1;
//    Inc(Pos);
    Inc(CommandLet);
    Inc(Overall);
  end;
  while Opened > 0 do begin
    try
      CloseScope;
    except
      on E: Exception do Writeln('FATAL during scope close: ', E.Message);
    end;
    Dec(Opened);
  end;
  while StackMoves > 0 do begin
    try
      StateStack.RestoreStackPointer;
    except
      on E: Exception do Writeln('FATAL during Stack restore: ', E.Message);
    end;
    Dec(StackMoves);
  end;
  if State.Machine.AShowInstructions then
    Writeln(Overall, ' instructions has been made');
end;

procedure TSubState.ForSetFinish;
var Shell: PVariableShell;
begin
  Shell := StateStack.Pick.GetShell;
  if not (Shell.TypeOf = Type_Integer) then
    raise EExecutionException.Create('For high bound must be integer!');
  Environment.ForFinish := PHInteger(Shell.Content).Content;
  Assigner.DestroyShell(Shell^);
end;

procedure TSubState.ForSetStart;
var Shell: PVariableShell;
begin
  Shell := StateStack.Pick.GetShell;
  if not (Shell.TypeOf = Type_Integer) then
    raise EExecutionException.Create('For low bound must be integer!');
  Environment.ForStart := PHInteger(Shell.Content).Content;
  with Environment^ do if ForAscending then
    ForStart := ForStart - ForStep
  else
    ForStart := ForStart + ForStep;
  Assigner.DestroyShell(Shell^);
end;

procedure TSubState.ForSetStep(const Step: Integer);
begin
  Environment.ForStep := Step;
end;

procedure TSubState.ForSetType(const Ascending: Boolean);
begin
  Environment.ForAscending := Ascending;
end;

procedure TSubState.ForSetVar(const Name: String);
var Key: TVariableShell;
    Str: TRunTimeVariable_String;
begin
  Str := TRunTimeVariable_String.Create(Name);
  Key := TVariableShell.Create(PHString(@Str));
  //Environment.ForShell := Environment.GetReferencedLocal(Key);
//  New(Environment.ForVar);
  Environment.ForShell := PVariableShell(Environment.Table.PushNew(Key));
//  Environment.Table.PushNew(Key);
  Environment.PreserveEnvironment;
//  Environment.Table.PushNew(Key);
//  Environment.ForShell := PVariableShell(Environment.Table.PointedItems[Key]);
end;

function TSubState.GetMetamethodOf(var AFShell, ASShell: TVariableShell; const MTMethod: Integer): PVariableShell;
begin
  Result := GetMetamethodOf(AFShell, MTMethod);
  if Result = nil then Result := GetMetamethodOf(ASShell, MTMethod);
end;

function TSubState.GetMetamethodOf(var Shell: TVariableShell; const MTMethod: Integer): PVariableShell;
var MT: TMetatable;
begin
  MT := GetMetatableOf(Shell);
  if not (MT = nil) then
    Result := MT.GetMetamethod(MTMethod)
  else
    Result := nil;
end;

function TSubState.GetMetatableOf(var Shell: TVariableShell): TMetatable;
begin
  if Shell.TypeOf = Type_Nil then begin
    Result := nil;
    Exit;
  end;
  if Shell.TypeOf = Type_Table then begin
    Result := TMetatable(PHTable(Shell.Content).Content.Metatable);
    if not (Result = nil) then Exit;
  end;
  Result := State.Machine.GetMetatableFor(Shell.TypeOf);
end;

procedure TSubState.OpenScope;
begin
  Environment.PreserveEnvironment;
  Environment := PNewEnvironment(TNewEnvironment.Create(PrimaryEnviroment, TNewEnvironment.PEnv(Environment), DestroyItemsArray));
  Environment.SetupEnvironment;
end;

procedure TSubState.PerformAdd;
var AFi, ASe: PVariableShell;
    SAFi, SASe: TVariableShell;
    SFi, SSe: ^TStackContainer;
    MtM: PVariableShell;
    Temp: TStackContainer;
begin
   SSe := StateStack.PickAddress;
  SFi := StateStack.PickAddress;
  AFi := SFi.GetShell;
  ASe := SSe.GetShell;
  SAFi := AFi^;
  SASe := ASe^;
  if IsNumbericType(AFi^) and IsNumbericType(ASe^) then begin
    if AFi.TypeOf = ASe.TypeOf then begin
      case AFi.TypeOf of
        Type_Integer: Temp := (TStackContainer.Create(Self.MemoryManager.AllocateNewInteger( ToInt(AFi^).Content + ToInt(ASe^).Content )));
        Type_Float: Temp := (TStackContainer.Create(Self.MemoryManager.AllocateNewFloat( ToFlt(AFi^).Content + ToFlt(ASe^).Content )));
      end;
    end
    else begin
      case AFi.TypeOf of
        Type_Integer: Temp := (TStackContainer.Create(Self.MemoryManager.AllocateNewFloat( ToInt(AFi^).Content + ToFlt(ASe^).Content )));
        Type_Float: Temp := (TStackContainer.Create(Self.MemoryManager.AllocateNewFloat( ToFlt(AFi^).Content + ToInt(ASe^).Content )));
      end;
    end;
    Inc(StateStack.SP);
    Assigner.DestroyShell(SAFi);
    Assigner.DestroyShell(SASe);
    SFi^ := Temp;
  end
  else begin
    MtM := GetMetamethodOf(AFi^, ASe^, Metatable_MethodAdd);
    if MtM = nil then begin
      Assigner.DestroyShell(AFi^);
      Assigner.DestroyShell(ASe^);
      //raise EExecutionException.Create('Attempt to perform arithmetics on ...');
      ThrowExecutionException('Attempt to perform arithmetics on ', AFi.TypeOf);
    end;
    { Restore Stack position }
    Inc(StateStack.SP, 2);
    StateStack.MoveStackPointer;
    try
      PerformCall(MtM^, False);
    finally
//      StateStack.ReturnStackPointer;
      StateStack.RestoreStackPointer;
    end;
  end;
end;

procedure TSubState.PerformAssign;
var AVar, AValue: PVariableShell;
    AA: TVariableShell;
begin
  AVar := StateStack.PickAddress^.GetShell;
  if not StateStack.IsEmpty then begin
    Assigner.DecrementVariableReferenceCounter(AVar^);
    AValue := StateStack.PickAddress^.GetShell;
    case AValue.TypeOf of
      Type_BuiltInField: begin 
        with PHBuiltInField(AValue.Content)^ do
          AA := TRuntimeMonster.GetFieldValue(Self.MemoryManager, PHBuiltInObject(ParentObject).Content, Content);
        AValue := @AA;
      end;
      Type_BuiltInProperty: begin
        with PHBuiltInProperty(AValue.Content)^ do 
          AA := TRuntimeMonster.GetPropertyValue(Self.MemoryManager, PHBuiltInObject(ParentObject).Content, Content);
        AValue := @AA;
      end;
    end;
    
    case AVar.TypeOf of
      Type_BuiltInField: begin   
        with PHBuiltInField(AVar.Content)^ do 
          TRuntimeMonster.SetFieldValue(PHBuiltInObject(ParentObject).Content, Content, AValue^);
        Assigner.DestroyShell(AValue^);
      end;
      Type_BuiltInProperty: begin 
        with PHBuiltInProperty(AVar.Content)^ do 
          TRuntimeMonster.SetPropertyValue(PHBuiltInObject(ParentObject).Content, Content, AValue^);
        Assigner.DestroyShell(AValue^);
      end;
    else 
      Assigner.AssignValues(AVar^, AValue^);
    end;

    Assigner.DestroyShell(AValue^);
//  Assigner.DestroyShell(AValue^);
  end
  else begin
    AA := TVariableShell.Create(Type_Nil, nil);
//    Assigner.AssignValues(AVar^, AA);

    case AVar.TypeOf of
      Type_BuiltInField: begin   
        with PHBuiltInField(AVar.Content)^ do 
          TRuntimeMonster.SetFieldValue(PHBuiltInObject(ParentObject).Content, Content, AA);
        Assigner.DestroyShell(AA);
      end;
      Type_BuiltInProperty: begin 
        with PHBuiltInProperty(AVar.Content)^ do 
          TRuntimeMonster.SetPropertyValue(PHBuiltInObject(ParentObject).Content, Content, AA);
        Assigner.DestroyShell(AA);
      end;
    else 
      Assigner.AssignValues(AVar^, AA);
    end;
  end;
end;

procedure TSubState.PerformAssignLocalReference(const Name: String);
var Shell: PVariableShell;
    Container: ^TStackContainer;
begin
//  PushLocal(Name, THasher.HashString(Name));
  Container := StateStack.PickAddress;
  Shell := Container.GetShell;
  if Container.IsReferenced then
    Assigner.AssignLinked(Shell, Environment.GetPrehashedLocal(Type_String, THasher.FixHash(THasher.HashString(Name)))^)
  else
    Assigner.AssignValues(Environment.GetPrehashedLocal(Type_String, THasher.FixHash(THasher.HashString(Name)))^, Shell^);
end;

procedure TSubState.PerformCall(const LongCall: Boolean);
var Shell: PVariableShell;
begin
  Shell := StateStack.Pick.GetShell;
  try
    PerformCall(Shell^, LongCall);
  finally
    Assigner.DestroyShell(Shell^);
//    ClearStack;
  end;
end;

procedure TSubState.PerformDoMonumentalPointer;
var OldShell: TVariableShell;
begin
  OldShell := StateStack.SP.GetShell^;
  StateStack.SP^ := TStackContainer.Create(OldShell);
end;

procedure TSubState.PerformIndex;
var IndexVar, IndexValue: ^TStackContainer;
    IVShell, ICShell: PVariableShell;
    SavedPointer: TVariableShell;
begin
  IndexValue := StateStack.PickAddress;
  IndexVar := StateStack.PickAddress;
  IVShell := IndexVar.GetShell;
  ICShell := IndexValue.GetShell;

  SavedPointer := ICShell^;

  if IVShell.TypeOf = Type_Table then begin
    with PHTable(IVShell.Content)^ do begin
      if Content.ContainsKey(ICShell^) then begin
        StateStack.Push(TStackContainer.Create(PVariableShell(Content.PointedItems[ICShell^])));
        Assigner.IncrementVariableReferenceCounter(StateStack.SP.GetShell^);
      end
      else begin
        StateStack.SP^ := TStackContainer.Create(GetNil);
      end;
    end;
    Assigner.DecrementVariableReferenceCounter(SavedPointer);
    Assigner.DecrementVariableReferenceCounter(IVShell^);
  end
  else if IVShell.TypeOf = Type_BuiltInObject then begin
    if ICShell.TypeOf = Type_String then begin
      StateStack.Push(TStackContainer.Create(TRuntimeMonster.GetClassPart(Self.MemoryManager, PHBuiltInObject(IVShell.Content), PHString(ICShell.Content).Content)));
    end
    else
      StateStack.Push(TStackContainer.Create(GetNil));
    Assigner.DecrementVariableReferenceCounter(SavedPointer);
    if (StateStack.SP^.GetShell^.TypeOf = Type_Nil) then
      Assigner.DecrementVariableReferenceCounter(IVShell^);
  end
  else begin
    if true then begin
      Assigner.DecrementVariableReferenceCounter(IVShell^);
      Assigner.DecrementVariableReferenceCounter(ICShell^);
      ThrowExecutionException('Attempt to index ', IVShell.TypeOf);
    end;
  end;
end;

procedure TSubState.PerformMarkAsNative;
begin
  StateStack.SP.IsConstant := True;
end;

procedure TSubState.PerformPushFunction(const ID: Integer);
var Val: TVariableShell;
begin
  Val := MemoryManager.AllocateNewFunction(State.Machine.FunctionsStorage[ID].CompiledBytecode, Environment);
//  with PHFunction(Val.Content)^ do begin
//    Content := State.Machine.FunctionsStorage[ID].CompiledBytecode;
//    LinkedEnvironment := Environment;
//    Environment.IncrementRefCounter(1);
//  end;
  StateStack.Push(TStackContainer.Create(Val));
end;

procedure TSubState.PerformReset;
begin
  while not (Environment{.Parent} = nil) do
    CloseScope;
//  Environment := PEnvironment(TEnvironment.Create(MemoryManager.AllocateRawTable(0), TEnvironment.SelfPointer(State.Machine.GlobalEnvironment)));
  Environment := PNewEnvironment(TNewEnvironment.Create(PrimaryEnviroment, nil, DestroyItemsArray));
//  Environment.PreserveEnvironment;
  Environment.SetupEnvironment;
  ClearStackForcefully;
//  StateStack.CurrentStackPointer := -1;
end;

procedure TSubState.PerformCall(var Shell: TVariableShell; const LongCall: Boolean);
var FuncOut: TFunctionOutput;
    i: Integer;
    PreservedEnv: PNewEnvironment;
    Mt: PVariableShell;
    SavedPointer: ^TStackContainer;
    Returned: TVariableShell;
begin
  case Shell.TypeOf of
    Type_BuiltInFunction: begin
      FuncOut := PHBuiltInFunction(Shell.Content).Content(Self);
      if Length(FuncOut) <= 0 then
        StateStack.Push(TStackContainer.Create(GetNil))
      else if LongCall then for i := 0 to Length(FuncOut) - 1 do
        StateStack.Push(TStackContainer.Create(FuncOut[i]))
      else begin
        StateStack.Push(TStackContainer.Create(FuncOut[0]));
        //for i := 1 to Length(FuncOut) - 1 do Assigner.DestroyShell(FuncOut[i]);
      end;
    end;
    Type_Function: begin
      Environment.PreserveEnvironment;
      PreservedEnv := Self.Environment;
      with PHFunction(Shell.Content)^ do begin
        Self.Environment := LinkedEnvironment;
        Environment.SetupEnvironment;
//        Inc(Self.Environment.RefCounter);
        Environment.IncrementRefCounter(1);
        try
          Execute(Content);
//          if StateStack.GetSafeCount = 0 then
//          if (StateStack.LowBound = StateStack.SP) then
          if StateStack.IsEmpty then
            StateStack.Push(TStackContainer.Create(GetNil))
          else if LongCall {and (StateStack.GetSafeCount > 1)} then
//            for i := 1 to StateStack.GetSafeCount - 1 do
//              Assigner.DestroyShell(StateStack.Pick.GetShell^);
            while (Integer(StateStack.SP) > Integer(StateStack.LowBound)) do
              Assigner.DestroyShell(StateStack.Pick.GetShell^);
        finally
          Environment.DecrementRefCounter(1);
          Environment := PreservedEnv;
          Environment.SetupEnvironment;
        end;
      end;
    end;
    Type_BuiltInMethod: begin
      SavedPointer := (StateStack.SP);
      with PHBuiltInMethod(Shell.Content)^ do
        Returned := TRuntimeMonster.InvokeMethod(Self, ParentObject.Content, Content);
      StateStack.SP := SavedPointer;
      ClearStack;
      StateStack.Push(TStackContainer.Create(Returned));
    end
  else
    Mt := GetMetamethodOf(Shell, Metatable_MethodCall);
    if Mt = nil then begin
      //raise EExecutionException.Create('Attempt to call non-function value!');
      ThrowExecutionException('Attempt to call ', Shell.TypeOf);
    end;
    PerformCall(Mt^, LongCall);
  end;
end;

function TSubState.PerformSimpleCall(var Shell: TVariableShell): TVariableShell;
begin
  StateStack.MoveStackPointer;
  try
    PerformCall(Shell, False);
    Result := StateStack.Pick.GetShell^;
  finally
//    StateStack.ReturnStackPointer;
    StateStack.RestoreStackPointer;
  end;
end;

procedure TSubState.PushBoolean(const Content: Boolean);
begin
//  StateStack.Push(TStackContainer.Create(MemoryManager.AllocateNewBoolean(Content)));
  with StateStack.NextSP^ do begin
    IsReferenced := False;
    VariableShell := MemoryManager.AllocateNewBoolean(Content);
  end;
//  StateStack.Push(TStackContainer.Create(TVariableShell.Create()));
  Environment.Name_VarialbleName := '?';
end;

procedure TSubState.PushFloat(const Content: HSFloat);
begin
  StateStack.Push(TStackContainer.Create(MemoryManager.AllocateNewFloat(Content)));
  Environment.Name_VarialbleName := '?';
end;

procedure TSubState.PushInteger(const Content: HSInteger);
begin
  StateStack.Push(TStackContainer.Create(MemoryManager.AllocateNewInteger(Content), ValueAsNative));
  Environment.Name_VarialbleName := '?';
end;

procedure TSubState.PushLocal(const Name: String; const Hash: Integer);
//var Str: TRunTimeVariable_String;
var Shell: PVariableShell;
begin
//  Str := TRunTimeVariable_String.Create(Name);
//  StateStack.Push(Environment.GetReferencedLocal(TVariableShell.Create(PHString(@Str))));
  Shell := Environment.GetPrehashedLocal(Type_String, Hash);
  Assigner.IncrementVariableReferenceCounter(Shell^);
  StateStack.Push(TStackContainer.Create(Shell, ValueAsNative));
  Environment.Name_VarialbleName := Name;
end;

procedure TSubState.PushNil;
begin
//  StateStack.Push(TVariableShell.Create(Type_Nil, nil));
  StateStack.Push(TStackContainer.Create(GetNil, ValueAsNative));
  Environment.Name_VarialbleName := '?';
end;

procedure TSubState.PushString(const Content: String);
begin
  StateStack.Push(TStackContainer.Create(MemoryManager.AllocateNewString(Content), ValueAsNative));
  Environment.Name_VarialbleName := '?';
end;

procedure TSubState.PushVar(const Name: String; const Hash: Integer);
var //Str: TRunTimeVariable_String;
    Shell: PVariableShell;
begin
//  Str := TRunTimeVariable_String.Create(Name);
//  StateStack.Push(Environment.GetReferencedValue(TVariableShell.Create(PHString(@Str))));
//  Writeln(Hash);
  Shell := Environment.GetPrehashedValue(Type_String, Hash);
//  while not (Shell.Referenced = nil) do
//    Shell := PVariableShell(Shell.Referenced);
  Assigner.IncrementVariableReferenceCounter(Shell^);
  StateStack.Push(TStackContainer.Create(Shell, ValueAsNative));

  Environment.Name_VarialbleName := Name; // It is initial point
end;

function TSubState.RoughToString(const [ref] Shell: TVariableShell): String;
begin
  case Shell.TypeOf of
    Type_Nil: Result := 'nil';
    Type_Integer: Result := IntToStr(ToInt(Shell).Content);
    Type_Float: Result := FloatToStr(ToFlt(Shell).Content);
    Type_String: Result := ToStr(Shell).Content;
    Type_Boolean: Result := BoolToStr(PHBoolean(Shell.Content).Content, True);
    Type_Table: Result := 'table';
    Type_Function: Result := 'function';
    Type_BuiltInFunction: Result := 'function (built-in)';
    Type_BuiltInObject: Result := 'object (built-in)';
    Type_BuiltInField: Result := 'field (built-in)';
    Type_BuiltInProperty: Result := 'property (built-in)';
    Type_BuiltInMethod: Result := 'method (built-in)';
  else
    Result := 'Unknown';
  end;
end;

procedure TSubState.SetBreak(const Mark: Integer);
begin
  Environment.BreakPos := Mark;
end;

procedure TSubState.SetContinue(const Mark: Integer);
begin
  Environment.ContinuePos := Mark;
end;

procedure TSubState.ThrowExecutionException(const AMsg: String; const AType: HLVM_Types);
begin
  raise EExecutionException.Create(AMsg + Environment.Name_VarialbleName + ' (a ' + TypeToString(AType) + ' value)');
end;

{ TState }

procedure TState.CallAsync(const SubState: TSubState);
var Task: TTask;
begin
  Task := TTask.Create(procedure()
    begin

      //Task.Free;
    end) as TTask;
  Task.Free;
end;

constructor TState.Create(const Parent: TMachine);
begin
  Machine := Parent;
  SubStates := TSubStates.Create();
end;

constructor TMachine.Create;
begin
  Assigner.MemoryManager := @MemoryManager;
//  GlobalEnvTable.Content := TTable.Create;
  CriticalSection := TCriticalSection.Create;

  AoTCompiler := THLVM_BytecodeAoTCompiler.Create;
  AoTCompiler.AoTNumerationFunction := Self.GetNextFunctionCounter;

//  GlobalEnvironment := PEnvironment(TEnvironment.Create(@GlobalEnvTable, nil));
  FunctionsStorage := THLVM_AoT_FunctionContainers.Create;

  States := TStates.Create;

  PrimaryEnvironment := TExpandedBaseEnvironment.Create(nil);
end;

destructor TMachine.Destroy;
var i: Integer;
begin
  States.Free;

//  GlobalEnvTable.Content.Free;
//  GlobalEnvironment.Free;
  if not (Metatable_String = nil) then Metatable_String.Free;

  FunctionsStorage.Free;
  for i := 0 to FunctionsStorage.Count - 1 do begin
    FunctionsStorage[i].CompiledBytecode.OwnsObjects := True;
    FunctionsStorage[i].CompiledBytecode.Free;
  end;
  CriticalSection.Free;
  AoTCompiler.Free;

  DestroyItems(PrimaryEnvironment.FItems^);

  PrimaryEnvironment.Free;
  inherited;
end;

procedure TMachine.DestroyItems(const AArray: TTableArray);
var i: Integer;
begin
  // Writeln('Destroyed env;');
  for i := 0 to Length(AArray) - 1 do begin
    with AArray[i] do begin
      { If hash is empty }
      if HashCode = -1 then Continue;
      try
        Assigner.DestroyShell(Value);
      except
        on E: Exception do
          Writeln(E.Message);
      end;
    end;
  end;
end;

procedure TMachine.RegisterBasics;
var Key: TRunTimeVariable_String;
    Table: PHTable;
begin

  PushBuiltInFunction('print',
  function(const SubState: TObject): TFunctionOutput
  var This: TSubState;
      Shell: TStackContainer;
  begin
    SetLength(Result, 0);
    This := SubState as TSubState;
    while (Integer(This.StateStack.SP) > Integer(This.StateStack.LowBound)) do begin
      Shell := This.StateStack.Pick;
      Write(This.RoughToString(Shell.GetShell^), #9);
      This.Assigner.DecrementVariableReferenceCounter(Shell.GetShell^);
    end;
    if not This.StateStack.IsEmpty then begin
      Shell := This.StateStack.Pick;
      Write(This.RoughToString(Shell.GetShell^));
      This.Assigner.DecrementVariableReferenceCounter(Shell.GetShell^);
    end;
    Writeln;
  end);

  PushBuiltInFunction('write',
  function (const SubState: TObject): TFunctionOutput
  var This: TSubState;
      Shell: TStackContainer;
  begin
    SetLength(Result, 0);
    This := SubState as TSubState;
    while (Integer(This.StateStack.SP) > Integer(This.StateStack.LowBound)) do begin
      Shell := This.StateStack.Pick;
      Write(This.RoughToString(Shell.GetShell^), #9);
      This.Assigner.DecrementVariableReferenceCounter(Shell.GetShell^);
    end;
    if not This.StateStack.IsEmpty then begin
      Shell := This.StateStack.Pick;
      Write(This.RoughToString(Shell.GetShell^));
      This.Assigner.DecrementVariableReferenceCounter(Shell.GetShell^);
    end;
  end);

  PushBuiltInFunction('Hash',
  function(const SubState: TObject): TFunctionOutput
  var This: TSubState;
      Shell: TStackContainer;
  begin
    SetLength(Result, 0);
    This := SubState as TSubState;
    if This.StateStack.IsEmpty then Exit;

    Shell := This.StateStack.Pick;
    SetLength(Result, 1);
    try
      try
        Result[0] := This.MemoryManager.AllocateNewInteger(Shell.GetShell.GetHashCode);
      finally
        This.Assigner.DestroyShell(Shell.GetShell^);
      end;
    except
      on E: Exception do
        Result[0] := This.MemoryManager.AllocateNewString(E.Message);
    end;
  end);

  PushBuiltInFunction('Length', 
  function(const SubState: TObject): TFunctionOutput
  var This: TSubState;
      Shell: PVariableShell;
  begin 
    SetLength(Result, 0);
    This := SubState as TSubState;
    if This.StateStack.IsEmpty then raise EExecutionException.Create('Expected string/array parameter.');

    Shell := This.StateStack.Pick.GetShell;
    if Shell.TypeOf = Type_String then begin 
      SetLength(Result, 1);
      try
        Result[0] := This.MemoryManager.AllocateNewInteger(Length(PHString(Shell^.Content).Content));
      finally 
        This.Assigner.DestroyShell(Shell^);
      end;
    end
    else 
      raise EExecutionException.Create('Unsupported type');
  end);

  PushBuiltInFunction('Concatenate',
  function(const SubState: TObject): TFunctionOutput
  var This: TSubState;
      Table: PHTable;
      Num: TRunTimeVariable_Integer;
      Shell: PVariableShell;
      i: Integer;
  begin
    SetLength(Result, 0);
    This := SubState as TSubState;
    if This.StateStack.IsEmpty then
      raise EExecutionException.Create('Expected table to concatenate.');
    Shell := This.StateStack.PickAddress.GetShell;
    i := 1;
    try
      if not (Shell.TypeOf = Type_Table) then
        raise EExecutionException.Create('Expected table to concatenate.');
      SetLength(Result, 1);
      Result[0] := MemoryManager.AllocateNewString('');
      Table := PHTable(Shell.Content);
      Num := TRunTimeVariable_Integer.Create(i);
      while Table.Content.ContainsKey(TVariableShell.Create(Type_Integer, @Num)) do begin
        i := i + 1;
        with PHString(Result[0].Content)^ do
          if Content = '' then
            Content := Content + This.RoughToString(Table.Content.Items[TVariableShell.Create(Type_Integer, @Num)])
          else
            Content := Content + #13#10 + This.RoughToString(Table.Content.Items[TVariableShell.Create(Type_Integer, @Num)]);
        Num.Content := i;
      end;

    finally
      This.Assigner.DestroyShell(Shell^);
    end;
  end);

  Key := TRunTimeVariable_String.Create('Class');
  PrimaryEnvironment.AddOrSetValue(TVariableShell.Create(Type_String, @Key), MemoryManager.AllocateNewTable(1));

  Table := PHTable(PrimaryEnvironment[TVariableShell.Create(Type_String, @Key)].Content);
  Key := TRunTimeVariable_String.Create('GetMethods');
  Table.Content.AddOrSetValue(TVariableShell.Create(Type_String, @Key), MemoryManager.AllocateNewBuiltInFunction(
    function(const SubState: TObject): TFunctionOutput
    var This: TSubState;
        Table: PHTable;
        Shell: PVariableShell;
        Temp: TRunTimeVariable_Integer;
        Methods: TStringArray;
        i: Integer;
    begin
      SetLength(Result, 0);
      This := SubState as TSubState;
      if This.StateStack.IsEmpty then
        raise EExecutionException.Create('Expected built-in object as argument.');
      Shell := This.StateStack.PickAddress.GetShell;
      try
        if not (Shell.TypeOf = Type_BuiltInObject) then
          raise EExecutionException.Create('Expected built-in object as argument.');
        Methods := TRuntimeMonster.GetMethods(PHBuiltInObject(Shell.Content).Content);
        SetLength(Result, 1);
        Result[0] := MemoryManager.AllocateNewTable(1);
        Table := PHTable(Result[0].Content);
        for i := 0 to Length(Methods) - 1 do begin
          Temp := TRunTimeVariable_Integer.Create(i + 1);
          Table.Content.AddOrSetValue(TVariableShell.Create(Type_Integer, @Temp), MemoryManager.AllocateNewString(Methods[i]));
        end;
      finally
        This.Assigner.DestroyShell(Shell^);
      end;
    end));

  Key := TRunTimeVariable_String.Create('TestObject');
  PrimaryEnvironment.AddOrSetValue(TVariableShell.Create(Type_String, @Key), MemoryManager.AllocateNewBuiltInObject(TObject.Create, @MemoryManager, True, 1));
  Key := TRunTimeVariable_String.Create('Console');
  PrimaryEnvironment.AddOrSetValue(TVariableShell.Create(Type_String, @Key), MemoryManager.AllocateNewBuiltInObject(CmdIO, @MemoryManager, False, 1));
end;

procedure TMachine.RegisterExternalFunctions(const Functions: THLVM_AoT_FunctionContainers);
var Func: THLVM_AoT_FunctionContainer;
    Mark: Integer;
begin
  for Func in Functions do begin
    Mark := Func.FunctionMark;
    if Mark >= FunctionsStorage.Count then FunctionsStorage.Count := Mark + 1;
    FunctionsStorage[Mark] := Func;
  end;
end;

procedure TMachine.RegisterMetas;
var Key: TVariableShell;
    Str: TRunTimeVariable_String;
    Func: TBuiltInFunction;
begin
  Metatable_String := TMetatable.Create;

  Str := TRunTimeVariable_String.Create(Metatable_MetaAdd_Name);
  Key.TypeOf := Type_String;
  Key.Links := 0;
  Key.Content := @Str;
  Key.Referenced := nil;

  Func :=
    function (const SubState: TObject): TFunctionOutput
    var This: TSubState;
        First, Second: PVariableShell;
    begin
      This := SubState as TSubState;
      Second := This.StateStack.PickAddress^.GetShell;
      First := This.StateStack.PickAddress^.GetShell;
      SetLength(Result, 1);

      Result[0] := This.MemoryManager.AllocateNewString(This.RoughToString(First^) + This.RoughToString(Second^));
      with This.Assigner^ do begin
        DestroyShell(First^);
        DestroyShell(Second^);
      end;
    end;

  Metatable_String.AddOrSetValue(Key, MemoryManager.AllocateNewBuiltInFunction(Func));
end;

function TMachine.GetNextFunctionCounter: Integer;
begin
  { Thread safe! }
  CriticalSection.Acquire;
  try
    Result := FunctionStackCounter;
    FunctionStackCounter := FunctionStackCounter + 1;
  finally
    CriticalSection.Release;
  end;
end;

procedure TMachine.PushBuiltInFunction(const Name: String; const Func: TBuiltInFunction);
var Str: TRunTimeVariable_String;
    Vara: TVariableShell;
begin
  Str := TRunTimeVariable_String.Create(Name);
  Vara := MemoryManager.AllocateNewBuiltInFunction(Func);
//  GlobalEnvironment.Table^.Content.AddOrSetValue(
//    TVariableShell.Create(PHString(@Str)),
//    Vara);
  PrimaryEnvironment.AddOrSetValue(TVariableShell.Create(PHString(@Str)), Vara);
end;

function TMachine.GetMetatableFor(AType: HLVM_Types): TMetatable;
begin
  case AType of
    Type_Nil: raise EExecutionException.Create('Nil type cannot have metatable');
    Type_Integer, Type_Float: Result := Metatable_Number;
    Type_String: Result := Metatable_String;
    Type_Boolean: Result := Metatable_Boolean;
    Type_Function, Type_BuiltInFunction: Result := Metatable_Function;
    Type_Table: Result := Metatable_Table;
  else
    Result := nil;
  end;
end;

procedure TMachine.ExecuteRawChunk(const RawChunk: THLVMRawChunk);
var CompiledBytecode: THLVM_AoTCompiledBytecode;
    Bytecode: THLVMBytecodeChunk;
begin
  CompiledBytecode := AoTCompiler.AoTCompile(RawChunk);
  RegisterExternalFunctions(CompiledBytecode.Functions);
  CompiledBytecode.Functions.Free;
  Bytecode := CompiledBytecode.CompiledBytecode;
  try
    States[0].SubStates[0].Execute(Bytecode);
  finally
    Bytecode.OwnsObjects := True;
    Bytecode.Free;
  end;
end;

procedure TMachine.ExecuteStreamBytecode(const Stream: TStream);
var RawChunk: THLVMRawChunk;
    Reader: THLVM_BytecodeReader;
begin
//  Writeln('Bytecode size: ', Stream.Size.ToString, ' bytes.');
  if Stream.Size <= 0 then Exit;
  Reader := THLVM_BytecodeReader.Create;
  try
    Reader.InputStream := Stream;
    Stream.Position := 0;
    RawChunk := Reader.RetrieveBytecode;
    //RawChunk.OwnsObjects := False;
    try
      ExecuteRawChunk(RawChunk);
    finally
      RawChunk.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TMachine.ExecuteTextChunk(const Text: String);
var Compiler: THCompiler;
    OutStream: TMemoryStream;
begin
  if States.Count < 1 then States.Add(TState.Create(Self));
  if States[0].SubStates.Count < 1 then
    States[0].SubStates.Add(TSubState.Create(States[0]))
  else
    States[0].SubStates[0].PerformReset;
  Compiler := THCompiler.Create;
  OutStream := TMemoryStream.Create;
  try
    OutStream.Clear;
    OutStream.Position := 0;
    Compiler.OutputStream := OutStream;
    Compiler.LexerObject.SourceText := Text;
    Compiler.LexerObject.ScopeName := 'StdIn';
    Writeln('Lexing...');
    Compiler.LexerObject.ParseForLexemes;
    WriteLn('Fixing...');
    Compiler.FixLexemes;
    WriteLn('Compiling...');
    Compiler.CompileChunk;
    WriteLn('Executing...');
    ExecuteStreamBytecode(OutStream);
  finally
    try Compiler.Free; except on E: Exception do end;
    try OutStream.Free; except on E: Exception do end;
  end;
end;


destructor TState.Destroy;
begin
  SubStates.Free;
  inherited;
end;

end.
