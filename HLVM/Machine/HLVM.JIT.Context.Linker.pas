unit HLVM.JIT.Context.Linker;

interface

{
  Main module that implements Linker,
  which links RawBytecode to Bytecode sentences
}

uses
  System.SysUtils,
  //uTExtendedX87,
  { Other usages: }
  HLVM.Static.Command,
  HLVM.Types.Ordinal,
  HLVM.Types.Containers,
  HLVM.Generics.HList,
  HLVM.JIT.Context.Storage.Commons,
  HLVM.JIT.Context.CommandExpansion,
  HLVM.JIT.Context.Storage,
  { Obsolete }
  HLVM.Types.AoT;

const
  HLVM_Linker_MaxStackSize = 256;

type
  HLVM_LinkerWord = record
  public type
    PHLVM_LinkerWord = ^HLVM_LinkerWord;
    TLinkerOperands = HList<PHLVM_LinkerWord>;
    TPositionedOperands = HList<Integer>;
  public var
    CommandID: Integer;
    { Storing operands }
    AbstractOperands: TPositionedOperands;
  public
    RegisterIndex: Integer;
    PointerRegisterIndex: Integer;
    UsesRegister: Boolean;
  public 
    StringStorage: String;
    IntegerStorage: Int64;
    FloatStorage: HLVM_Float;
    LabeledIndex: Integer;

    AdditionalData: Int64;
    AdditionalPlusData: Int64;

    DataPointer: Int64;
  public
    constructor Create(const ID: Integer);
  end;
  PHLVM_LinkerWord = HLVM_LinkerWord.PHLVM_LinkerWord;
  TLinkerOperands = HLVM_LinkerWord.TLinkerOperands;
  TPositionedOperands = HLVM_LinkerWord.TPositionedOperands;

type
  { Obsolete. KadJIT BeHeading upgrade (v1.6.15) }
  THLVMHeaderEntry = record
  public var
    Code: Integer;
    { Storage thing }
    IntStorage: Int64;
    AdditionalData: Int64;
    FloatStorage: Extended;
    StringStorage: String;
  public
    procedure Nullify;
  end;

type
  THLVMInternalLinkerStorage = HList<HLVM_LinkerWord>;
  THLVMHeader = HList<THLVMHeaderEntry>;

type
  THLVMBytecodeSentence = record
  public type 
    TLinkerInternalIntegerList = HList<Integer>;
  public var
    // Header: THLVMHeader;

    InternalStorage: THLVMInternalLinkerStorage;
    ActualStorage: TLinkerInternalIntegerList;
    // EntryPoint: PHLVM_LinkerWord;
  public
    procedure NullifySelf;
  end;
  PHLVMBytecodeSentence = ^THLVMBytecodeSentence;

type
  THLVMBytecodeSentenceArray = array of THLVMBytecodeSentence;

type
  THLVMBytecodeText = record
  public var
    { Record, containing all sentences of functions and actual chunk }
    { Main chunk, containing all code for main }
    MainChunk: THLVMBytecodeSentence;
    { Function storage }
    Functions: THLVMBytecodeSentenceArray;
  end;

type
  ELinkerException = class(Exception);

{
  Comments on Linker implementation [TODO LIST!]:
    Hunter200165: Add function support;
    Hunter200165: Add security check on bounds of CloseScope either on OpenScope;
}

type
  THLVMLinker = class(THLVMObject)
  public
    type
      TLinkerPassState = record
      public
        BytecodeArray: THLVMBytecodeChunk;
        Linked: THLVMBytecodeSentence;

        PositionRegister: Integer;
        PositionPointedRegister: Integer;
      end;
  public var
    // BytecodeArray: THLVMBytecodeChunk;
    // Linked: THLVMBytecodeSentence;

//    PositionRegister: Integer;
//    PositionPointedRegister: Integer;
  public
    { Overall methods of current class }
    function LinkFullBytecode(const AoTBytecode: THLVM_AoTCompiledBytecode): THLVMBytecodeText;
    function LinkBytecode(const BytecodeArray: THLVMBytecodeChunk): THLVMBytecodeSentence;

    procedure PrecompileFunction(var State: TLinkerPassState);

    function CompileOperand(
        var State: TLinkerPassState;
        var Pos: Integer;
        const AllowNonReturn: Boolean = False;
        const AllowMultipleReturn: Boolean = False;
        const AllowJumps: Boolean = False;
        const UnderAssign: Boolean = False
      ): Integer;
  public
    function ResolveScope(var State: TLinkerPassState; const APos: Integer): Integer;
  public
    function CompilePushes(var State: TLinkerPassState; const Position: Integer; const Force: Boolean = False): Integer;
    function CompileTwoWayOperators(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileOneWayOperators(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileTwoWayNoReturnOperators(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileOneWayNoReturnOperators(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileEmptyNoReturnOperators(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileReturnOperator(var State: TLinkerPassState; const Position: Integer): Integer;

    function CompileForCheck(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileForSetStart(var State: TLinkerPassState; const Position: Integer): Integer;
    function CompileCall(var State: TLinkerPassState; const Position: Integer): Integer;
  public
    procedure PerformVariableLink(var State: TLinkerPassState);
    procedure LinkBreakContinue(var State: TLinkerPassState);
  public
    { Class functions (Static build!) }
    class function OperandsCount(const Command: Integer): Integer; static;
    class function IsIgnorable(const Command: Integer; const AllowJumps: Boolean = False): Boolean; static;
  end;

implementation

{
  Methods of compilation should:
    Receive by reference position of operand they need to compile from (from bottom to top);
    Return Integer position of Register they have taken;
}

uses
  HLVM.JIT.Context.Scoper;

{ HLVM_LinkerWord }

constructor HLVM_LinkerWord.Create(const ID: Integer);
begin
  Self.CommandID := ID;
  AbstractOperands.Nullify;

  RegisterIndex := 0;
  PointerRegisterIndex := 0;

  StringStorage := '';
//  {$IfNDef Win64}
  FloatStorage := 0;
//  {$Else}
  //FillChar(FloatStorage.AsBytes, SizeOf(FloatStorage), 0);
  // FloatStorage.AsExtRec80.BuildUp(False, 0, 0);
//  ResetFPU;
//  ClearFPUExceptions;
//  FloatStorage := 0;
//  FloatStorage := FloatStorage.Create(0);
//  {$EndIf}
  IntegerStorage := 0;

  AdditionalData := 0;
  AdditionalPlusData := 0;
  DataPointer := 0;
end;

{ THLVMLinker }

function THLVMLinker.CompileEmptyNoReturnOperators(var State: TLinkerPassState; const Position: Integer): Integer;
var LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;
begin
  Result := Position - 1;
  Container := State.BytecodeArray[Position];

  LWord := HLVM_LinkerWord.Create(Container.Command);

  with LWord, Container do begin
    IntegerStorage := IntContent;
    FloatStorage := Extended(FloatContent);
    StringStorage := StringContent;
  end;
  State.Linked.InternalStorage.Add(LWord);
end;

function THLVMLinker.CompileForCheck(var State: TLinkerPassState; const Position: Integer): Integer;
var Pos: Integer;
    LWord: HLVM_LinkerWord;
begin
  { Interprets ForCheck to a JITable form! }
  Result := Position - 1;
  Pos := Result;
  while Pos >= 0 do begin
    if State.BytecodeArray[Pos].Command = Command_ForSetType then
      Break;
    Pos := Pos - 1;
  end;
  if Pos < 0 then
    raise ELinkerException.Create('For loop has invalid bounds.');
  if not (State.BytecodeArray[Pos + 1].Command = Command_ForSetStep) then
    raise ELinkerException.Create('Expected Command_ForSetStep after setting type of for loop.');
  LWord := HLVM_LinkerWord.Create(Command_ForCheck);
  { Integer storage is ForStep }
  LWord.IntegerStorage := State.BytecodeArray[Pos + 1].IntContent;
  { AdditionalData is ForType }
  { ForType: 1 = Ascending; 0 = Descending }
  LWord.AdditionalData := State.BytecodeArray[Pos].IntContent;

  State.Linked.InternalStorage.Add(LWord);
end;

function THLVMLinker.CompileForSetStart(var State: TLinkerPassState; const Position: Integer): Integer;
var Pos: Integer;
    LWord: HLVM_LinkerWord;
    RPos, PPos: Integer;
begin
  { Interprets ForSetStart to a JITable form! }
  Result := Position - 1;
  { Requires an operand }
  RPos := State.PositionRegister;
  PPos := State.PositionPointedRegister;
  Result := CompileOperand(State, Result);

  Pos := Result;
  while Pos >= 0 do begin
    if State.BytecodeArray[Pos].Command = Command_ForSetType then
      Break;
    Pos := Pos - 1;
  end;
  if Pos < 0 then
    raise ELinkerException.Create('For loop has invalid bounds.');
  if not (State.BytecodeArray[Pos + 1].Command = Command_ForSetStep) then
    raise ELinkerException.Create('Expected Command_ForSetStep after setting type of for loop.');
  LWord := HLVM_LinkerWord.Create(Command_ForSetStart);
  { Integer storage is ForStep }
  LWord.IntegerStorage := State.BytecodeArray[Pos + 1].IntContent;
  { AdditionalData is ForType }
  { ForType: 1 = Ascending; 0 = Descending }
  LWord.AdditionalData := State.BytecodeArray[Pos].IntContent;

  { Operands }
  LWord.AbstractOperands.Add(State.Linked.InternalStorage.LastIndex);

  State.PositionRegister := RPos;
  State.PositionPointedRegister := PPos;

  State.Linked.InternalStorage.Add(LWord);
end;

function THLVMLinker.CompileCall(var State: TLinkerPassState; const Position: Integer): Integer;
var Pos, Count: Integer;
    LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;
    RPos, PPos: Integer;
begin
  { Call is made before RtrStckPtr }
  Count := 1;
  Pos := Position - 1;
  while (Count > 0) and (Pos >= 0) do begin
    case State.BytecodeArray[Pos].Command of
      Command_RestoreStackPointer: begin
        Count := Count + 1;
      end;
      Command_MoveStackPointer: begin
        Count := Count - 1;
      end;
    end;
    if Count > 0 then
      Pos := Pos - 1;
  end;
  if Pos < 0 then
    raise ELinkerException.Create('RtrStckPtr has invalid bounds.');

  while IsIgnorable(State.BytecodeArray[Pos].Command, False) do
    Pos := Pos + 1;
  Pos := Pos - 1;

  RPos := State.PositionRegister;
  PPos := State.PositionPointedRegister;

  Container := State.BytecodeArray[Position];
  LWord := HLVM_LinkerWord.Create(Container.Command);
  Result := Position - 1;

  while Result > Pos do begin
    Result := CompileOperand(State, Result);
    LWord.AbstractOperands.Add(State.Linked.InternalStorage.LastIndex);
  end;

  LWord.UsesRegister := True;
  LWord.RegisterIndex := RPos;
  LWord.PointerRegisterIndex := PPos;

  State.Linked.InternalStorage.Add(LWord);

  State.PositionRegister := RPos + 1;
  State.PositionPointedRegister := PPos + 1;
end;

function THLVMLinker.CompileOneWayNoReturnOperators(var State: TLinkerPassState; const Position: Integer): Integer;
var LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;
    RPos, PPos, OPos: Integer;
begin
  RPos := State.PositionRegister;
  PPos := State.PositionPointedRegister;
  Container := State.BytecodeArray[Position];

  Result := Position - 1;
  Result := CompileOperand(State, Result);
  OPos := State.Linked.InternalStorage.LastIndex;

  LWord := HLVM_LinkerWord.Create(Container.Command);
  with LWord, Container do begin
    StringStorage := StringContent;
    IntegerStorage := IntContent;
    FloatStorage := FloatContent;
  end;
  LWord.AbstractOperands.Add(OPos);

  State.Linked.InternalStorage.Add(LWord);

  { Restore them all! }
  State.PositionRegister := RPos;
  State.PositionPointedRegister := PPos;
end;

function THLVMLinker.CompileOneWayOperators(var State: TLinkerPassState; const Position: Integer): Integer;
var LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;
    RPos, OPos, PPos: Integer;
begin
  RPos := State.PositionRegister;
  PPos := State.PositionPointedRegister;
  Container := State.BytecodeArray[Position];

  Result := Position - 1;
  Result := CompileOperand(State, Result);
  OPos := State.Linked.InternalStorage.LastIndex;

  LWord := HLVM_LinkerWord.Create(Container.Command);
  with LWord, Container do begin 
    StringStorage := StringContent;
    IntegerStorage := IntContent;
    FloatStorage := FloatContent;
  end;
  LWord.UsesRegister := True;
  LWord.RegisterIndex := RPos;
  LWord.PointerRegisterIndex := PPos;

  LWord.AbstractOperands.Add(OPos);

  State.PositionRegister := RPos + 1;
  State.PositionPointedRegister := PPos + 1;
  State.Linked.InternalStorage.Add(LWord);
end;

function THLVMLinker.CompileOperand(
  var State: TLinkerPassState;
  var Pos: Integer;
  const AllowNonReturn: Boolean = False;
  const AllowMultipleReturn: Boolean = False;
  const AllowJumps: Boolean = False;
  const UnderAssign: Boolean = False
): Integer;
var PosA: Integer;
    Container: TUniversalContainer;
begin
  { Result := -1; }
  PosA := Pos;
  Container := State.BytecodeArray[PosA];
  while IsIgnorable(Container.Command, AllowJumps) do begin
    PosA := PosA - 1;
    if PosA < 0 then begin
      Result := -5;
      Exit;
    end;
    Container := State.BytecodeArray[PosA];
  end;

  case Container.Command of
    Command_PushString, 
    Command_PushVar,
    Command_PushLocal,
    Command_PushInteger, 
    Command_PushReal,
    Command_PushNil,
    Command_PushTrue,
    Command_PushFalse:
      begin 
        Result := CompilePushes(State, PosA, (Container.Command = Command_PushVar) and UnderAssign);
      end;
    Command_ArADD, Command_ArSUB, Command_ArMUL, Command_ArDIV,
    Command_BtAND, Command_BtOR, Command_BtXOR,
    Command_LgAND, Command_LgOR, Command_LgXOR,
    Command_OperatorIs, Command_OperatorAs,
    Command_Equals, Command_GreaterThan, Command_GreaterEqual,
    Command_Index,
    Command_IsIn,
    Command_CBoolAnd, Command_CBoolOr,
    Command_CBitAnd, Command_CBitOr, Command_CBitXor, Command_CBitShl, Command_CBitShr,
    Command_BitShiftLeft, Command_BitShiftRight:
      begin 
        Result := CompileTwoWayOperators(State, PosA);
      end;
    Command_UnaryMinus, 
    Command_LgNOT, Command_BtNOT, Command_CBitNot, Command_CBoolNot,
    Command_CheckStack:
      begin 
        Result := CompileOneWayOperators(State, PosA);
      end;
    Command_Call:
      begin
        if not AllowMultipleReturn then
          raise ELinkerException.Create('Illegal full call in bytecode, while expected simple operator.');
        Result := CompileCall(State, PosA);
      end;
    Command_SimpleCall:
      begin
        Result := CompileCall(State, PosA);
      end;
    Command_Assign:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_Assign]; Expected value-operator!');
        Result := CompileTwoWayNoReturnOperators(State, PosA);
      end;
    Command_ForSetStart:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_ForSetStart]; Expected value-operator!');
        Result := CompileForSetStart(State, PosA);
      end;
    Command_ForSetFinish:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_ForSetFinish]; Expected value-operator!');
        Result := CompileOneWayNoReturnOperators(State, PosA);
      end;
    Command_ForCheck:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_ForCheck]; Expected value-operator!');
        Result := CompileForCheck(State, PosA);
      end;
    Command_OpenScope, Command_CloseScope, Command_ForSetVar, Command_SetContinue, Command_SetBreak:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_OpenScope, Command_CloseScope, Command_ForSetVar, Command_SetContinue, Command_SetBreak]; Expected value-operator!');
        Result := CompileEmptyNoReturnOperators(State, PosA);
      end;
    Command_JMP, Command_JC, Command_JNC:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_JMP, Command_JC, Command_JNC]; Expected value-operator!');
        Result := CompileEmptyNoReturnOperators(State, PosA);
      end;
    Command_BreakLoop, Command_ContinueLoop:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_BreakLoop, Command_ContinueLoop]; Expected value-operator!');
        Result := CompileEmptyNoReturnOperators(State, PosA);
      end;
    Command_Internal_PushFunction:
      begin
        Result := CompilePushes(State, PosA, False);
      end;
    Command_Return:
      begin
        if not AllowNonReturn then
          raise ELinkerException.Create('Unexpected non-returnable operation [Command_Return]; Expected value-operator!');
        Result := CompilePushes(State, PosA, False);
      end;
  else
    raise ELinkerException.Create('Unknown operation OPCode: $' + IntToStr(Container.Command));
  end;
end;

function THLVMLinker.CompilePushes(var State: TLinkerPassState; const Position: Integer; const Force: Boolean): Integer;
var LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;  
    RPos, PPos: Integer;
begin
  Result := Position - 1;

  Container := State.BytecodeArray[Position];
  LWord := HLVM_LinkerWord.Create(Container.Command);
  with LWord do begin
    StringStorage := Container.StringContent;
    IntegerStorage := Container.IntContent;
    FloatStorage := Container.FloatContent;

    AdditionalData := Int64(Force);
  end;

  if Container.Command = Command_Internal_PushFunction then begin
    { AoT makes +1 offset }
    LWord.IntegerStorage := LWord.IntegerStorage - 1;
    if LWord.IntegerStorage < 0 then
      raise ELinkerException.Create('PushFunction command seems to be invalid: Invalid index.');
  end;

  LWord.RegisterIndex := 0;
  if not (Container.Command = Command_PushVar) and not (Container.Command = Command_Internal_PushFunction) and not (Container.Command = Command_Return) then begin
    { PushVar does not use registers. It uses only pointers }
    { PushFunction does not use registers either }
    RPos := State.PositionRegister;
    State.PositionRegister := State.PositionRegister + 1;

    LWord.UsesRegister := True;
    LWord.RegisterIndex := RPos;
  end;
  PPos := State.PositionPointedRegister;
  State.PositionPointedRegister := State.PositionPointedRegister + 1;
  LWord.PointerRegisterIndex := PPos;

  State.Linked.InternalStorage.Add(LWord);
end;

function THLVMLinker.CompileReturnOperator(var State: TLinkerPassState; const Position: Integer): Integer;
var Container: TUniversalContainer;
begin
  Result := Position - 1;
end;

function THLVMLinker.CompileTwoWayNoReturnOperators(var State: TLinkerPassState; const Position: Integer): Integer;
var LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;
    RPos, PPos, FPos, SPos: Integer;
begin
  { For example - assign }
  Result := Position - 1;
  Container := State.BytecodeArray[Position];

  RPos := State.PositionRegister;
  PPos := State.PositionPointedRegister;

  Result := CompileOperand(State, Result, False, False, False, Container.Command = Command_Assign);
  FPos := State.Linked.InternalStorage.LastIndex;
  Result := CompileOperand(State, Result);
  SPos := State.Linked.InternalStorage.LastIndex;

  LWord := HLVM_LinkerWord.Create(Container.Command);
  with LWord, Container do begin
    StringStorage := StringContent;
    IntegerStorage := IntContent;
    FloatStorage := FloatContent;
  end;

  with LWord.AbstractOperands do begin
    Add(FPos);
    Add(SPos);
  end;

  State.PositionRegister := RPos;
  State.PositionPointedRegister := PPos;

  State.Linked.InternalStorage.Add(LWord);
end;

function THLVMLinker.CompileTwoWayOperators(var State: TLinkerPassState; const Position: Integer): Integer;
var LWord: HLVM_LinkerWord;
    Container: TUniversalContainer;
    RPos, PosF, PosS, PPos: Integer;
begin
  RPos := State.PositionRegister;
  PPos := State.PositionPointedRegister;

  Container := State.BytecodeArray[Position];
  Result := Position - 1;
  Result := CompileOperand(State, Result);
  PosF := State.Linked.InternalStorage.LastIndex;
  Result := CompileOperand(State, Result);
  PosS := State.Linked.InternalStorage.LastIndex;

  LWord := HLVM_LinkerWord.Create(Container.Command);

  LWord.AbstractOperands.Add(PosF);
  LWord.AbstractOperands.Add(PosS);
  
  LWord.UsesRegister := True;
  LWord.RegisterIndex := RPos;
  LWord.PointerRegisterIndex := PPos;
  with LWord, Container do begin 
    StringStorage := StringContent;
    IntegerStorage := IntContent;
    FloatStorage := FloatContent;
  end;
  
  { Restore position of register! }
  State.PositionRegister := RPos + 1;
  State.PositionPointedRegister := PPos + 1;
  State.Linked.InternalStorage.Add(LWord);
end;

class function THLVMLinker.IsIgnorable(const Command: Integer; const AllowJumps: Boolean = False): Boolean;
begin
  case Command of
    Command_MoveStackPointer, Command_RestoreStackPointer,
    Command_ClearStack, Command_CheckStackInvisible:
      begin
        Result := True;
      end;
    Command_JMP, Command_JC, Command_JNC:
      begin
        Result := not AllowJumps;
      end;
    Command_ForSetType, Command_ForSetStep:
      begin
        Result := True;
      end
  else
    Result := False;
  end;
end;

procedure THLVMLinker.LinkBreakContinue(var State: TLinkerPassState);
type
  TBreakContPos = record
  public
    PosBreak: Integer;
    PosContinue: Integer;
    ScopeID: Integer;
    Depth: Integer;
  end;
  TBreaks = HList<TBreakContPos>;

var i, Depth, BrDepth: Integer;
    LWord: PHLVM_LinkerWord;
    BreakList: TBreaks;
    BreakPos: TBreakContPos;
begin
  { Link `break`s and `continue`s }
  { Scoper does the job for linking all scopes and making their IDs }
  BreakList.Nullify;
  BrDepth := -1;
  Depth := 0;
  for i := 0 to State.Linked.ActualStorage.Count - 1 do begin
    LWord := @State.Linked.InternalStorage.Items[State.Linked.ActualStorage[i]];
    case LWord.CommandID of
      Command_OpenScope: begin
        Depth := LWord.IntegerStorage;
      end;
      Command_CloseScope: begin
        Depth := LWord.IntegerStorage;
        if (BrDepth >= 0) and (BrDepth < Depth) then begin
          BreakList.Remove;
          if BreakList.Count <= 0 then
            BrDepth := -1
          else
            BrDepth := BreakList.LastItem.Depth;
        end;
      end;

      Command_SetBreak: begin
        if (BrDepth = Depth) then
          raise ELinkerException.Create('Break can be declared only once!');
      
        BreakPos.PosBreak := LWord.IntegerStorage;
        BreakPos.Depth := Depth;
        { Not used }
        BreakPos.ScopeID := 0;

        BrDepth := Depth;
        BreakList.Add(BreakPos);
      end;
      Command_SetContinue: begin
        if not (BrDepth = Depth) then
          raise ELinkerException.Create('Continue has no explicit declared `Break` position, which violated general label purpose!');
        BreakList.Items[BreakList.LastIndex].PosContinue := LWord.IntegerStorage;
      end;

      Command_BreakLoop: begin
        if BreakList.Count <= 0 then
          raise ELinkerException.Create('Break has no loop to break!');

        LWord.CommandID := Command_BreakLoop;
        LWord.IntegerStorage := BreakList.LastItem.PosBreak;
        LWord.AdditionalData := Depth - BreakList.LastItem.Depth;

        if LWord.AdditionalData < 0 then
          raise ELinkerException.Create('Internal error in linking - scope closures for break to close is negative! It is critical flaw!');
      end;
      Command_ContinueLoop: begin
        if BreakList.Count <= 0 then
          raise ELinkerException.Create('Continue has no loop to continue!');

        LWord.CommandID := Command_ContinueLoop;
        LWord.IntegerStorage := BreakList.LastItem.PosContinue;
        LWord.AdditionalData := Depth - BreakList.LastItem.Depth;
        
        if LWord.AdditionalData < 0 then
          raise ELinkerException.Create('Internal error in linking - scope closures for continue to close is negative! It is critical flaw!');
      end;

      Command_ForCheck: begin
        if not (BrDepth = Depth) then
          raise ELinkerException.Create('ForCheck have no break label!');
        LWord.AdditionalPlusData := BreakList.LastItem.PosBreak;
      end;
    end;
  end;
  if not (Depth = 0) then
    raise ELinkerException.Create('Scopes mismatch in break linking.');
end;

function THLVMLinker.LinkBytecode(const BytecodeArray: THLVMBytecodeChunk): THLVMBytecodeSentence;
var Pos, APos, LPos: Integer;
    State: TLinkerPassState;
begin
  Result.NullifySelf;

  Result.InternalStorage.ResizeNullify(BytecodeArray.Count);
  Result.ActualStorage.ResizeNullify(BytecodeArray.Count);

  State.Linked.NullifySelf;
  State.Linked := Result;
  State.BytecodeArray := BytecodeArray;

  Pos := BytecodeArray.Count - 1;
  while (Pos >= 0) do begin
    State.PositionRegister := 0;
    State.PositionPointedRegister := 0;
    APos := CompileOperand(State, Pos, True, True, True);
    if APos < -3 then
      Break;
    LPos := State.Linked.InternalStorage.LastIndex;

    State.Linked.InternalStorage.Items[LPos].LabeledIndex := Pos;

    State.Linked.ActualStorage.Add(LPos);
    Pos := APos;
  end;
  State.Linked.ActualStorage.Reverse;

  PerformVariableLink(State);
  LinkBreakContinue(State);

  Result := State.Linked;
end;

function THLVMLinker.LinkFullBytecode(const AoTBytecode: THLVM_AoTCompiledBytecode): THLVMBytecodeText;
var i: Integer;
begin
  SetLength(Result.Functions, AoTBytecode.Functions.Count);
  Result.MainChunk := LinkBytecode(AoTBytecode.CompiledBytecode);
  for i := 0 to AoTBytecode.Functions.Count - 1 do begin
    Result.Functions[i] := LinkBytecode(AoTBytecode.Functions[i].CompiledBytecode);
    if not (AoTBytecode.Functions[i].FunctionMark = i + 1) then
      raise ELinkerException.Create('AoT compiler must have function offset of 1 (got ' + (AoTBytecode.Functions[i].FunctionMark - i).ToString + '). KadJIT compiler will align function storage on its own');
  end;
end;

class function THLVMLinker.OperandsCount(const Command: Integer): Integer;
begin
  case Command of
    Command_ArADD, Command_ArSUB, Command_ArMUL, Command_ArDIV,
    Command_BtAND, Command_BtOR, Command_BtXOR,
    Command_LgAND, Command_LgOR, Command_LgXOR,
    Command_OperatorIs, Command_OperatorAs,
    Command_Equals, Command_GreaterThan, Command_GreaterEqual,
    Command_Index,
    Command_IsIn,
    Command_CBoolAnd, Command_CBoolOr,
    Command_CBitAnd, Command_CBitOr, Command_CBitXor, Command_CBitShl, Command_CBitShr,
    Command_BitShiftLeft, Command_BitShiftRight:
      begin
        Result := 2;
      end;
    Command_BtNOT, Command_LgNOT,
    Command_CBitNot, Command_CBoolNot:
      begin
        Result := 1;
      end;
    Command_NewIndex:
      begin
        Result := 3;
      end;
    Command_Call, Command_SimpleCall,
    Command_CallAsync:
      begin
        Result := -1;
      end;
  else
    Result := 0;
  end;
end;

procedure THLVMLinker.PerformVariableLink(var State: TLinkerPassState);
var Scoper: TScoper;
begin
  { Advanced variable compilation in order to make JIT super fast! }
  { Only usable if JIT compiler and JIT-Compatible Execution Provider are in FS (Fast-Static) Mode }
  { Alpha 2.0 is JIT_Revolution Alpha, deprecates non-FS JIT mode }

  { KadJIT compiler introduced TScoper class for help with scoping }
  Scoper.DoScoping(State.Linked);
end;

procedure THLVMLinker.PrecompileFunction(var State: TLinkerPassState);
var Pos: Integer;
begin
  for Pos := 0 to State.BytecodeArray.Count - 1 do begin
    with State.BytecodeArray[Pos] do begin
      case Command of
        Command_Assign:
          begin
            { Linker does not understand direct memory assumptions }
            { Therefore we are linking it in direct way }
            Command := Command_AssignFromMemory;
          end;
        Command_ClearStack:
          begin
            Break;
          end;
      end;
    end;
  end;
end;

function THLVMLinker.ResolveScope(var State: TLinkerPassState; const APos: Integer): Integer;
var Counter: Integer;
begin
  Result := APos;
  Counter := 0;
  repeat
    case State.BytecodeArray[Result].Command of
      Command_CloseScope: Counter := Counter + 1;
      Command_OpenScope: Counter := Counter - 1;
    end;
    if Counter > 0 then
      Result := Result - 1;
  until (Counter <= 0) or (Result < 0);
end;

{ THLVMBytecodeSentence }

procedure THLVMBytecodeSentence.NullifySelf;
begin
  { So, we will nullify them all }
  InternalStorage.Nullify;
  ActualStorage.Nullify;

  (* KadJIT BeHeading upgrade (v1.6) *)
//  Header.Nullify;

//  InternalStorage.Size := 0;
//  Header.Size := 0;
  // EntryPoint := nil;
end;

{ THLVMHeaderEntry }

procedure THLVMHeaderEntry.Nullify;
begin
  Code := JIT_Header_NULL;
  { Clear }
  IntStorage := 0;
  AdditionalData := 0;
  FloatStorage := 0;
  StringStorage := '';
end;

end.
