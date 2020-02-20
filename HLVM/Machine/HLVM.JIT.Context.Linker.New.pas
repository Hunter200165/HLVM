unit HLVM.JIT.Context.Linker.New;

interface

(*
  This is expanded form of Linker, that combines Linker, Scoper and AoT-pseudocompiler,
  which makes it easier to maintain (not sure) and definetely helps by removing
  dead dependence in form of obsolete HLVM.Generics.Collections and old AoT
  linker.
*)

uses
  System.SysUtils,
  HLVM.Generics.HList,
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.Storage.Commons,
  HLVM.JIT.Context.Linker,
  HLVM.JIT.Context.CommandExpansion,
  HLVM.Static.Command,
  { Obsolete }
  HLVM.Types.Containers,
  HLVM.Types.Ordinal;

type
  HLVM_LinkerWord = record
  public type
    PHLVM_LinkerWord = ^HLVM_LinkerWord;
    TLinkerOperands = HList<HLVM_LinkerWord>;
    TPositionedOperands = HList<Integer>;
  public var
    CommandID: Integer;
    { Storing operands }
    AbstractOperands: TPositionedOperands;
    GeneralOperands: TLinkerOperands;
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

    { Internal Linker flag }
    Compiled: Boolean;
  public
    constructor Create(const ID: Integer);
  end;
  PHLVM_LinkerWord = HLVM_LinkerWord.PHLVM_LinkerWord;
  TLinkerOperands = HLVM_LinkerWord.TLinkerOperands;
  TPositionedOperands = HLVM_LinkerWord.TPositionedOperands;

type
  THLVMLinkerLinkedStructure = HList<HLVM_LinkerWord>;
  THLVMLinkerLinkedStructures = HList<THLVMLinkerLinkedStructure>;

{ Result in link operation }
type 
  THLVMLinkerResultingLinkedStructure = record 
  public var 
    MainBlock: THLVMLinkerLinkedStructure;
    Functions: THLVMLinkerLinkedStructures;
  end;

type
  THLVMLinkerBytecodeInstruction = record
  public var
    Command: HLVM_Command;
    Position: THPosition;

    LabeledPosition: Integer;
  public var
    IntegerStorage: HLVM_Integer;
    FloatStorage: HLVM_Float;
    StringStorage: String;
  public
    constructor Create(const Command: HLVM_Command; Position: THPosition; const IntStorage: HLVM_Integer = 0; const FloatStorage: HLVM_Float = 0; const StringStorage: String = ''); overload;
    constructor Create(const Container: TUniversalContainer); overload;
  end;
  THLVMLinkerBytecodeText = HList<THLVMLinkerBytecodeInstruction>;

type 
  THLVMLinkerBytecodePart = record
  public var 
    From, UpTo: Integer;
  end;
  THLVMLinkerBytecodeMap = HList<THLVMLinkerBytecodePart>;

type 
  THLVMLinkerVariableObject = record
  public var 
    Name: String;
    Local: Boolean;
    Index: Integer;
    { Globally reference counted? ~[KadJIT GRC Mechanism] }
    IsHighestVariable: Boolean;
    { Is this variable used in loop? }
    IsLoopVariable: Boolean;
    { Is this variable should be blocked to use as highest? }
    ForbidHighUsage: Boolean;
    { Link instruction with its source. }
    InstructionPointer: PHLVM_LinkerWord;
  public
    procedure Nullify;
  end;
  THLVMLinkerVariables = HList<THLVMLinkerVariableObject>;

type 
  { Just represents all the global variables in given scope. }
  THLVMLinkerGlobalVariable = record 
  public var 
    Name: String;
  end;
  THLVMLinkerGlobalVariables = HList<THLVMLinkerGlobalVariable>;

type
  THLVMLinkerHighestVariable = record
  public var
    Name: String;
    { If set to true, then tells that highest variable comes from local scope }
    { Otherwise tells that highest variable comes from up-highest varaibles }
    IsLocal: Boolean;
    { Index of Local Variable/Higher Variable }
    PreviousIndex: Integer;
    Index: Integer;
  public 
    constructor CreateLocal(const Name: String; const Index: Integer; const CurrentIndex: Integer);
    constructor CreateHighest(const Name: String; const Index: Integer; const CurrentIndex: Integer);
  end;
  THLVMLinkerHighestVariables = HList<THLVMLinkerHighestVariable>;
  
type
  THLVMLinkerVariableScope = record
  public type 
    PHLVMLinkerVariableScope = ^THLVMLinkerVariableScope;
  public var
    Variables: THLVMLinkerVariables;
    HighestVariables: PHLVMLinkerVariableScope;
    CachedHighestVariables: THLVMLinkerHighestVariables;
  public
    function FindLocalVariable(const Name: String): Integer;
    function FindHighestCachedVariable(const Name: String): Integer;
    function TryFindHighestVariable(const Name: String): Integer;
  public
    { New methods }
    function FindLocalVariableNew(const Name: String): Integer;
    function ResolveHighestVariableLinkNew(const VarName: String): Integer;
  end;
  PHLVMLinkerVariableScope = THLVMLinkerVariableScope.PHLVMLinkerVariableScope;

type
  THLVMLinkerHighestVariableCache = HList<HLVM_Integer>;

type
  THLVMLinkerFunctionContainer = record
  public var
    { Executeion text of instruction }
    Body: THLVMLinkerBytecodeText;
    { Unique identifier }
    Marker: Integer;
    { Map of the bytecode }
    Map: THLVMLinkerBytecodeMap;
    { Variables scope }
    VariableScope: THLVMLinkerVariableScope;
    { Resulting }
    Result: THLVMLinkerLinkedStructure;
    { Highest variables }
    HighestVariablesCache: THLVMLinkerHighestVariableCache;
  public var
    { Trigger, that shows that compiler had linked that section. }
    { If it is False and linkage was over, then something is really wrong }
    Compiled: Boolean;
  end;
  PHLVMLinkerFunctionContainer = ^THLVMLinkerFunctionContainer;
  THLVMLinkerFunctionsPool = HList<THLVMLinkerFunctionContainer>;

type
  { Count of variables that are currently in scopes. }
  { Must be set by Command_OpenScope and reset by Command_CloseScope }
  THLVMLinkerScopesSignals = HList<Integer>;

type 
  THLVMLinkerLoopRecord = record 
  public var 
    BreakPos: Integer;
    ContinuePos: Integer;
    Depth: Integer;
  end;
  THLVMLinkerLoopRecords = HList<THLVMLinkerLoopRecord>;
  
type
  THLVMLinkerAdvancedState = record
  public var
    { Registers }
    RegisterPosition: Integer;
    PointerRegisterPosition: Integer;
    { Position of compiled command }
    CompilationDepth: Integer;
    ScopeDepth: Integer;
    { If something was forced to compile, for example - left side of assign operator }
    PushVarForced: Boolean;
  public var
    Scopes: THLVMLinkerScopesSignals;
    HighestVaraiblesCache: THLVMLinkerHighestVariableCache;
  public 
    LoopInfo: THLVMLinkerLoopRecords;
  public var
    BytecodeText: THLVMLinkerBytecodeText;
    Map: THLVMLinkerBytecodeMap;
    VariableScope: THLVMLinkerVariableScope;
    GlobalVariables: THLVMLinkerGlobalVariables;
    Result: THLVMLinkerLinkedStructure;
  public
    constructor Create(const BytecodeText: THLVMLinkerBytecodeText; const Map: THLVMLinkerBytecodeMap; const Variables: THLVMLinkerVariableScope);
  end;

type
  THLVMLinkerPassState = record
  public var
    RawBytecode: THLVMRawChunk;
    Processed: THLVMLinkerBytecodeText;
    Map: THLVMLinkerBytecodeMap;
    FunctionsPool: THLVMLinkerFunctionsPool;
  public var 
    GlobalVariables: THLVMLinkerGlobalVariables;
    MaxGlobalVariables: Integer;
  public var 
    Header: THLVMHeader;
  public
    constructor Create(const RawBytecode: THLVMRawChunk);
  end;

type 
  THLVMLinkerAoTLabelRecord = record 
  public var 
    LabelId: Integer;
    LabelIndex: Integer;
  end;
  THLVMLinkerAoTLabels = HList<THLVMLinkerAoTLabelRecord>;
  
type
  ELinkerException = class(EHLVMCommonException);

type
  THLVMLinker = class (THLVMObject)
  public 
    { Abstraction level: Low }
    class procedure BuildBytecodeMap(var State: THLVMLinkerPassState);
  public
    { Abstraction level: Medium }
    class function ProcessRawBytecode(var State: THLVMLinkerPassState; const Pos, UpTo: Integer): THLVMLinkerBytecodeText;
    class function MapBytecode(const State: THLVMLinkerPassState; const BytecodeText: THLVMLinkerBytecodeText): THLVMLinkerBytecodeMap;
    class function GeneralLinkBytecode(State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState): THLVMLinkerLinkedStructure;
    class function LinkCommandLet(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Pos: Integer; var ResultPos: Integer): HLVM_LinkerWord;
    class function LinkPushVar(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Instruction: THLVMLinkerBytecodeInstruction; const InstrPointer: PHLVM_LinkerWord): HLVM_LinkerWord;
    class function LinkPushVarNew(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Instruction: THLVMLinkerBytecodeInstruction; const InstrPointer: PHLVM_LinkerWord): HLVM_LinkerWord;
    class procedure CloseAndFinaliseScopes(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const UntilScope: Integer; const DisposeLoopVariables: Boolean; const SoftClean: Boolean);
    class procedure AoTLink(var State: THLVMLinkerPassState);
    class procedure AoTLinkText(var Text: THLVMLinkerBytecodeText);
    { Link function procedure }
    class function LinkFunction(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const FuncPos: Integer): THLVMLinkerLinkedStructure;
    class function LinkForCheck(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Position: Integer): HLVM_LinkerWord;
    class function LinkForStart(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Position: Integer): HLVM_LinkerWord;
    { Linker itself }
    class function Link(const Bytecode: THLVMRawChunk): THLVMLinkerResultingLinkedStructure;
    class function LinkCompatible(const Bytecode: THLVMRawChunk): THLVMBytecodeText;
  public
    { Abstraction level: High }
    class function GetRelatedScopeClosure(const State: THLVMLinkerPassState; const Pos: Integer): Integer;
    class function GetInversedRelatedScopeClosure(const AdvState: THLVMLinkerAdvancedState; const Pos: Integer): Integer;
    class function GetRelatedStackClosure(const BytecodeText: THLVMLinkerBytecodeText; const Pos: Integer): Integer;
    class function GetCommandRegisterUsage(const Command: HCommand): Integer;
    class function GetOperandsCount(const Command: HCommand): Integer;
    class function GetCommandReturns(const Command: HCommand): Integer;
    class function IsCommandIgnorable(const Command: HCommand; const IgnoreJumps: Boolean): Boolean;
    class function GetIndefiniteCommandBounds(const BytecodeText: THLVMLinkerBytecodeText; const Pos: Integer): Integer;
    class function GetCommandBounds(const BytecodeText: THLVMLinkerBytecodeText; const Pos: Integer): Integer;
  public 
    { Translation to the older version of API }
    class function Translate(const LinkedStructure: THLVMLinkerResultingLinkedStructure): THLVMBytecodeText; overload;
    class function TranslateInContext(const LWord: HLVM_LinkerWord; var Storage: THLVMInternalLinkerStorage): Integer; 
    class function TranslateCountOfOps(const LWord: HLVM_LinkerWord): Integer;
  end;

implementation

{ HLVM_LinkerWord }

constructor HLVM_LinkerWord.Create(const ID: Integer);
begin
  Self.CommandID := ID;
  AbstractOperands.Nullify;
  GeneralOperands.Nullify;

  IntegerStorage := 0;
  FloatStorage := 0;
  StringStorage := '';

  DataPointer := 0;
  AdditionalData := 0;
  AdditionalPlusData := 0;

  RegisterIndex := 0;
  PointerRegisterIndex := 0;

  LabeledIndex := -1;

  Compiled := False;
end;

{ THLVMLinkerBytecodeInstruction }

constructor THLVMLinkerBytecodeInstruction.Create(const Command: HLVM_Command; Position: THPosition; const IntStorage: HLVM_Integer; const FloatStorage: HLVM_Float; const StringStorage: String);
begin
  Self.Command := Command;
  Self.Position := Position;
  Self.IntegerStorage := IntStorage;
  Self.FloatStorage := FloatStorage;
  Self.StringStorage := StringStorage;

  LabeledPosition := -1;
end;

constructor THLVMLinkerBytecodeInstruction.Create(const Container: TUniversalContainer);
begin
  Self.Create(Container.Command, Container.Position, Container.IntContent, Container.FloatContent, Container.StringContent);
end;

{ THLVMLinker }

class procedure THLVMLinker.AoTLink(var State: THLVMLinkerPassState);
var i: Integer;
begin
  { Links all Label positions in bytecode }
  AoTLinkText(State.Processed);
  for i := 0 to State.FunctionsPool.Count - 1 do
    AoTLinkText(State.FunctionsPool.Items[i].Body);
end;

class procedure THLVMLinker.AoTLinkText(var Text: THLVMLinkerBytecodeText);
var i, k, Actual, Pos: Integer;
    Labels: THLVMLinkerAoTLabels;
begin
  Labels.ResizeNullify(0);
  { AoT Linker should link all labels, breaks, continues and other stuff }
  { Collecting information about all labels }
  for i := 0 to Text.Count - 1 do begin
    if Text[i].LabeledPosition >= 0 then
      with Labels.AddReferenced^ do begin 
        LabelId := Text[i].LabeledPosition;
        LabelIndex := i;
        Text.Items[i].LabeledPosition := LabelIndex;
      end;
  end;
  { Link with collected information }
  for i := 0 to Text.Count - 1 do begin 
    case Text[i].Command of
      Command_JMP,
      Command_JC,
      Command_JNC,
      Command_SetBreak,
      Command_SetContinue: begin   
        Pos := -1;
        Actual := Text[i].IntegerStorage;
        for k := 0 to Labels.Count - 1 do
          if Labels[k].LabelId = Actual then begin 
            Pos := Labels[k].LabelIndex;
            Break;
          end;
        if Pos < 0 then
          raise ELinkerException.Create('Linker exception: AoT linking observed an invalid label instruction!');
        if i = Pos then
          raise ELinkerException.Create('Linker exception: AoT linking found security violation - label instruction is jumped over itself!');
        Text.Items[i].IntegerStorage := Pos;
      end;
    end;
  end;
end;

class procedure THLVMLinker.BuildBytecodeMap(var State: THLVMLinkerPassState);
var i: Integer;
begin
  State.Map := MapBytecode(State, State.Processed);
  for i := 0 to State.FunctionsPool.Count - 1 do begin 
    State.FunctionsPool.Items[i].Map := MapBytecode(State, State.FunctionsPool.Items[i].Body);
  end;
end;

/// <param name="SoftClean">
///   Parameter indicates, if variable scope should be finalised only by bytecode,
///  but not by linker itself (means it will not call SetCount on variable scope)
///  after all variables are disposed. Useful to use, when you are clearing some
///  scope materials before exiting this scope.
///  Also, when SoftClean is True, it will not delete any Scope
/// </param>
class procedure THLVMLinker.CloseAndFinaliseScopes(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const UntilScope: Integer; const DisposeLoopVariables: Boolean; const SoftClean: Boolean);
var ScopeID, i, PreviousCount, LastLoopVar, VarCount: Integer;
    VarPtr: ^THLVMLinkerVariableObject;
begin
  VarCount := AdvancedState.VariableScope.Variables.Count;
  for ScopeID := AdvancedState.Scopes.Count - 1 downto UntilScope do begin
    if ScopeID < -1 then
      raise ELinkerException.Create('Linker Internal Exception: CloseAndFinaliseScope received negative scope index (< -1)!');
    { Use ScopeID = -1 to signal close final (main/module) scope }
    if ScopeID = -1 then
      PreviousCount := 0
    else
      PreviousCount := AdvancedState.Scopes[ScopeID];

    { This method does not (!) finalise any loop scope. If you need it - write along with call to this method (but before the call) }

    LastLoopVar := PreviousCount - 1;
    for i := VarCount - 1 downto PreviousCount do begin
      VarPtr := @AdvancedState.VariableScope.Variables.Items[i];
      if not DisposeLoopVariables and VarPtr.IsLoopVariable then begin 
        { Problem begins here - if it is loop variable, it cannot be disposed, as machine relies on it }
        { Yet we know that normal code SHOULD provide local loop variable only in the beginning of the scope }
        { So, we must control it }
        LastLoopVar := i;
        Break;
      end;

      if VarPtr.IsHighestVariable then begin
        { It must be local highest }
        if not VarPtr.Local then
          Continue;

        if VarPtr.InstructionPointer = nil then
          raise ELinkerException.Create('Linker internal exception: Local variable does not provide pointer interface.');

        { As we now know that this variable is Highest, we need to allocate it in GRC }
        AdvancedState.VariableScope.Variables.Items[i].InstructionPointer^.CommandID := JIT_Command_PushHighestVariable;

        { It should be properly created and disposed }
        AdvancedState.Result.AddReferenced^ := HLVM_LinkerWord.Create(JIT_Command_DisposeHighestVariable);
        with AdvancedState.Result.LastItemPointer^ do begin
          { Index that needs to be disposed }
  //            IntegerStorage := AdvancedState.VariableScope.Variables[i].Index;
          IntegerStorage := i;
        end;  
      end;
    end;

    if not DisposeLoopVariables then begin
      for i := LastLoopVar downto PreviousCount do begin
        if not AdvancedState.VariableScope.Variables[i].IsLoopVariable then
          raise ELinkerException.Create('Linker Exception: Loop variable is being disposed by finalising scope, but this is forbidden!');
      end;
    end;

    if PreviousCount = LastLoopVar + 1 then begin 
      if not SoftClean and (AdvancedState.Scopes.Count > 0) then
        AdvancedState.Scopes.Remove;
    end
    else begin 
      { We must break that loop }
      if not (ScopeID = UntilScope) then
        raise ELinkerException.Create('Linker Exception: Loop scope linking and finalisation is out of its scope!');

      PreviousCount := LastLoopVar + 1;
      if not SoftClean then
        AdvancedState.VariableScope.Variables.SetCount(PreviousCount);
      VarCount := PreviousCount;

      Break;
    end;

    PreviousCount := LastLoopVar + 1;
    VarCount := PreviousCount;
    if not SoftClean then
      AdvancedState.VariableScope.Variables.SetCount(PreviousCount);
  end;
end;

class function THLVMLinker.GeneralLinkBytecode(State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState): THLVMLinkerLinkedStructure;
var i, k, ResultingPos, PrevCount: Integer;
    Bounds: ^THLVMLinkerBytecodePart;
    Word: HLVM_LinkerWord;
    HeaderSize: Integer;
    NewWithHeader: THLVMLinkerLinkedStructure;
begin
  Result.Nullify;
  Result.Size := AdvancedState.Map.Count;

  (* Deprecated. Now it is done once in Link! *)
//  { Processing things like `LABEL`, `Create_Function` and other things }
//  State.Processed := ProcessRawBytecode(State, 0, State.RawBytecode.Count - 1);
//  AoTLink(State);
//  BuildBytecodeMap(State);

  PrevCount := 0;
  for i := AdvancedState.Map.Count - 1 downto 0 do begin
    { Reset current register positions }
    AdvancedState.RegisterPosition := 0;
    AdvancedState.PointerRegisterPosition := 0;

    AdvancedState.CompilationDepth := 0;
    AdvancedState.ScopeDepth := 0;

    Bounds := @AdvancedState.Map.Items[i];
    Word := THLVMLinker.LinkCommandLet(State, AdvancedState, Bounds.From, ResultingPos);
    if not (ResultingPos = Bounds.UpTo) then
      raise ELinkerException.Create('Linker exception: ASM-let has been compiled, but it was out-of-bounds (' + IntToStr(ResultingPos) + ', but expected ' + IntToStr(Bounds.UpTo) + ')');
      
    { Add something else }
    if not (AdvancedState.Result.Count = PrevCount) then begin
      for k := PrevCount to AdvancedState.Result.Count - 1 do begin
        Result.Add(AdvancedState.Result[k]);
      end;
      PrevCount := AdvancedState.Result.Count;
    end;

    Result.Add(Word);
    if Word.CommandID = Command_PushVar then
      AdvancedState.VariableScope.Variables.LastItemPointer^.InstructionPointer := PHLVM_LinkerWord(Result.LastItemPointer);
  end;

  HeaderSize :=
    { GlobalCacheSize } 1 +
    { LocalCacheSize } 1 +
    { GlobalVariables } AdvancedState.GlobalVariables.Count;

  NewWithHeader.ResizeNullify(Result.Count + HeaderSize);

  { Setting global cache }
  NewWithHeader[0] := HLVM_LinkerWord.Create(JIT_Command_Heading_SetGlobalHeapSize);
  NewWithHeader.Items[0].IntegerStorage := AdvancedState.GlobalVariables.Count;

  { Setting local cache }
  NewWithHeader[1] := HLVM_LinkerWord.Create(JIT_Command_Heading_SetLocalHeapSize);
  NewWithHeader.Items[1].IntegerStorage := AdvancedState.VariableScope.Variables.Size;

  { Assigning global cache }
  for i := 0 to AdvancedState.GlobalVariables.Count - 1 do begin 
    NewWithHeader[2 + i] := HLVM_LinkerWord.Create(JIT_Command_Heading_CacheGlobal);
    NewWithHeader.Items[2 + i].StringStorage := AdvancedState.GlobalVariables[i].Name;
    NewWithHeader.Items[2 + i].IntegerStorage := i;
  end;

  { Coping all the result in place }
  for i := 0 to Result.Count - 1 do
    NewWithHeader[HeaderSize + i] := Result[i];

  Result := NewWithHeader;
end;

class function THLVMLinker.LinkForCheck(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Position: Integer): HLVM_LinkerWord;
var Pos: Integer;
begin
  Result := HLVM_LinkerWord.Create(Command_ForCheck);
  Result.LabeledIndex := AdvancedState.BytecodeText[Position].LabeledPosition;
  {#! Add position }

  Pos := Position - 1;
  while Pos >= 0 do begin 
    case AdvancedState.BytecodeText[Pos].Command of
      Command_ForSetType: Break;
      Command_OpenScope: begin   
        Pos := -1;
        Break;
      end;
      Command_CloseScope: begin 
        Pos := GetInversedRelatedScopeClosure(AdvancedState, Pos - 1);
      end;
    end;
    Pos := Pos - 1;
  end;

  if Pos < 0 then
    raise ELinkerException.Create('For-Loop has invalid bounds!');
  if not (AdvancedState.BytecodeText[Pos + 1].Command = Command_ForSetStep) then
    raise ELinkerException.Create('For-Loop must have a ForSetStep opcode after ForSetType.');
  
  Result.IntegerStorage := AdvancedState.BytecodeText[Pos + 1].IntegerStorage;
  { Type of loop: 1 = Asc, 0 = Decs }
  Result.AdditionalData := AdvancedState.BytecodeText[Pos].IntegerStorage;
end;

class function THLVMLinker.LinkForStart(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Position: Integer): HLVM_LinkerWord;
var Pos: Integer;
begin
  Result := HLVM_LinkerWord.Create(Command_ForSetStart);
  Result.LabeledIndex := AdvancedState.BytecodeText[Position].LabeledPosition;
  {#! Add position here }

  Pos := Position - 1;
  while Pos >= 0 do begin 
    case AdvancedState.BytecodeText[Pos].Command of
      Command_ForSetType: Break;
      Command_OpenScope: begin   
        Pos := -1;
        Break;
      end;
    end;
    Pos := Pos - 1;
  end;

  if Pos < 0 then
    raise ELinkerException.Create('For-Loop has invalid bounds!');
  if not (AdvancedState.BytecodeText[Pos + 1].Command = Command_ForSetStep) then
    raise ELinkerException.Create('For-Loop must have a ForSetStep opcode after ForSetType.');
  
  { Integer storage is ForStep }
  Result.IntegerStorage := AdvancedState.BytecodeText[Pos + 1].IntegerStorage;
  { Additional data is ForType }
  { 1 = Asc, 0 = Decs }
  Result.AdditionalData := AdvancedState.BytecodeText[Pos].IntegerStorage;
end;

class function THLVMLinker.LinkFunction(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const FuncPos: Integer): THLVMLinkerLinkedStructure;
var NewAdvState: THLVMLinkerAdvancedState;
    NewVars: THLVMLinkerVariableScope;
    Func: ^THLVMLinkerFunctionContainer;
begin
  if FuncPos >= State.FunctionsPool.Count then
    raise ELinkerException.Create('Linker internal exception: Position of linked function is out-of-bounds!');
  Func := @State.FunctionsPool.Items[FuncPos];
  NewVars.Variables.Nullify;
  { Chaining blocks }
  NewVars.HighestVariables := @AdvancedState.VariableScope;
  NewVars.CachedHighestVariables.Nullify;
  NewAdvState := THLVMLinkerAdvancedState.Create(Func.Body, Func.Map, NewVars);

  NewAdvState.Result := GeneralLinkBytecode(State, NewAdvState);
  Result := NewAdvState.Result;
end;

class function THLVMLinker.GetCommandBounds(const BytecodeText: THLVMLinkerBytecodeText; const Pos: Integer): Integer;
var Instr: ^THLVMLinkerBytecodeInstruction;
    Ops, i: Integer;
begin
  Result := Pos;
  Instr := @BytecodeText.Items[Pos];
  case Instr.Command of
    Command_Assign: 
      begin 
        {#! Accessing -1 element sometimes. }
        {#! Probably it is ProcessBytecode fault - first instruction is ignored. }
      
        Result := Result - 1;
        { It can be OpenScope in function declaration }
        if (BytecodeText[Result].Command = Command_MoveStackPointer) or (BytecodeText[Result].Command = Command_OpenScope) then begin
          Result := Result + 1;
          Exit;
        end;
        while (Result >= 0) and IsCommandIgnorable(BytecodeText[Result].Command, True) do
          Result := Result - 1;
        if Result < 0 then
          raise ELinkerException.Create('Linker exception: Cross-instructions boundary has been hit.');
        if not (GetCommandReturns(BytecodeText[Result].Command) = 1) then
          raise ELinkerException.Create('Linker exception: Expected single return command');
        { That is PushVar }
        Result := GetCommandBounds(BytecodeText, Result);
        if Result <= 0 then
          Exit;
        
        { This can be another Assign }
        for i := 1 to 2 do begin 
          Result := Result - 1;
          if (BytecodeText[Result].Command = Command_MoveStackPointer) or (BytecodeText[Result].Command = Command_OpenScope) then begin
            Result := Result + 1;
            Exit;
          end;
          while (Result >= 0) and IsCommandIgnorable(BytecodeText[Result].Command, True) do
            Result := Result - 1;
          if Result < 0 then
            raise ELinkerException.Create('Linker exception: Cross-instructions boundary has been hit.');
          if (i = 1) and (BytecodeText[Result].Command in [ Command_Assign, Command_AssignLocalReference ]) then begin 
            Result := GetCommandBounds(BytecodeText, Result);
          end
          else begin 
            Result := GetCommandBounds(BytecodeText, Result);
            Break;
          end;
        end;
      end;

    Command_AssignLocalReference: 
      begin 
//        Result := Result - 1;
//        while (Result >= 0) and IsCommandIgnorable(BytecodeText[Result].Command, True) do
//          Result := Result - 1;
        if (Result > 0) and (BytecodeText[Result - 1].Command in [ Command_Assign, Command_AssignLocalReference ]) then
          Result := GetCommandBounds(BytecodeText, Result - 1);
        Exit;
      end
  else 
    { General }
    Ops := GetOperandsCount(Instr.Command);
    if Ops >= 0 then begin 
      for i := 1 to Ops do begin 
        Result := Result - 1;
        while (Result >= 0) and IsCommandIgnorable(BytecodeText[Result].Command, True) do
          Result := Result - 1;
        if Result < 0 then
          raise ELinkerException.Create('Linker exception: Cross-instructions boundary has been hit.');
        if not (GetCommandReturns(BytecodeText[Result].Command) = 1) then
          raise ELinkerException.Create('Linker exception: Expected single-return command');
        Result := GetCommandBounds(BytecodeText, Result);
      end;
    end
    else begin
      { Operands count is various and not definite }
      Result := GetIndefiniteCommandBounds(BytecodeText, Pos);
    end;
  end;
end;

class function THLVMLinker.GetCommandRegisterUsage(const Command: HCommand): Integer;
begin
  Result := 0;
  case Command of
    Command_PushInteger,
    Command_PushReal,
    Command_PushTrue,
    Command_PushFalse,
    Command_PushString,
    Command_PushNil: 
      Result := 1;  
  end;
end;

class function THLVMLinker.GetCommandReturns(const Command: HCommand): Integer;
begin
  Result := 0;
  case Command of
    Command_PushVar, Command_PushString,
    Command_PushInteger, Command_PushReal,
    Command_PushNil, Command_PushTrue,
    Command_PushFalse, Command_PushLocal,
    Command_PushShortArguments, Command_ArADD,
    Command_ArSUB, Command_ArMUL, Command_ArDIV,
    Command_LgAND, Command_BtAND, Command_LgOR,
    Command_BtOr, Command_LgXOR, Command_BtXOR,
    Command_LgNOT, Command_BtNOT, Command_Ternary,
    Command_CheckStack, Command_SimpleCall,
    Command_DoInterpolation, Command_OperatorIs, Command_OperatorAs,
    Command_Equals, Command_GreaterThan, Command_GreaterEqual,
    Command_UnaryMinus, 
    Command_CreateTable, Command_CallAsync, 
    Command_Internal_PushFunction, Command_Index,
    Command_CreateRunTimeTable:
      Result := 1;
    Command_Call, Command_PushLongArguments: 
      Result := -1;
    { Zeroes }
    Command_OpenScope, Command_CloseScope,
    Command_MoveStackPointer, Command_RestoreStackPointer,
    Command_JMP, Command_JC, Command_JNC,
    Command_SetBreak, Command_SetContinue,
    Command_BreakLoop, Command_ContinueLoop,
    Command_ForSetStart, Command_ForSetFinish, 
    Command_ForSetType, Command_ForSetVar, 
    Command_ForSetStep, Command_Return,
    Command_NewIndex, Command_NoOperation,
    Command_AssignLocalReference:
      Result := 0;
  else 
    raise ELinkerException.Create('Unknown bytecode opcode to get return count: $' + IntToStr(Command));
  end;
end;

class function THLVMLinker.GetIndefiniteCommandBounds(const BytecodeText: THLVMLinkerBytecodeText; const Pos: Integer): Integer;
var i: Integer;
begin
  { Mistake? }
  // if GetCommandReturns(BytecodeText[Pos].Command) <= 0 then
  if GetOperandsCount(BytecodeText[Pos].Command) >= 0 then
    raise ELinkerException.Create('Linker internal exception: Call to GetIndefiniteCommandBounds(?) while command has definite amount of operands.');
  Result := GetRelatedStackClosure(BytecodeText, Pos - 1);
end;

class function THLVMLinker.GetInversedRelatedScopeClosure(const AdvState: THLVMLinkerAdvancedState; const Pos: Integer): Integer;
var i, Counter: Integer;
begin                
  i := Pos;
  Counter := 1;
  while i >= 0 do begin 
    case AdvState.BytecodeText[i].Command of
      Command_OpenScope: begin   
        Counter := Counter - 1;
        if Counter < 0 then
          raise ELinkerException.Create('Linker internal exception: Counter of scope somehow got negative value.');
        if Counter = 0 then begin 
          Result := i;
          Exit;
        end;
      end;
      Command_CloseScope: begin 
        Counter := Counter + 1;
      end;
    end;
    i := i - 1;
  end;
  raise ELinkerException.Create('Linker exception: Scopes placement violation.');
end;

class function THLVMLinker.GetOperandsCount(const Command: HCommand): Integer;
begin
  Result := 0;
  case Command of
    { Unary }
    Command_UnaryMinus,
    Command_BtNOT,
    Command_LgNOT,
    Command_ForSetStart,
    Command_ForSetFinish,
    Command_CheckStack:
      Result := 1;
    { Binary }
    Command_ArADD, Command_ArSUB,
    Command_ArMUL, Command_ArDIV,
    Command_LgAND, Command_BtAND,
    Command_LgOR, Command_BtOR,
    Command_LgXOR, Command_BtXOR,
    Command_Index, Command_OperatorIs,
    Command_OperatorAs, Command_Equals,
    Command_GreaterThan, Command_GreaterEqual,
    Command_Assign, Command_IsIn, 
    Command_BitShiftLeft, Command_BitShiftRight,
    Command_CBoolAnd, Command_CBoolOr,
    Command_CBitAnd, Command_CBitOr,
    Command_CBitXor, Command_CBitShl,
    Command_CBitShr:
      Result := 2;
    { Ternary }
    Command_Ternary, Command_NewIndex:
      Result := 3;
    { Multiple various }
    Command_Call, Command_SimpleCall, Command_Return,
    Command_CreateRunTimeTable, Command_CreateTable:
      Result := -1;
    { Zeroers }
    Command_PushInteger, Command_PushString,
    Command_PushReal, Command_PushNil,
    Command_PushTrue, Command_PushFalse,
    Command_PushVar, Command_PushLocal: 
      Result := 0;
    Command_OpenScope, Command_CloseScope, Command_Internal_PushFunction: 
      Result := 0;
    Command_MoveStackPointer, Command_RestoreStackPointer,
    Command_JMP, Command_JC, Command_JNC,
    Command_SetBreak, Command_BreakLoop,
    Command_SetContinue, Command_ContinueLoop,
    Command_ForCheck, Command_ForSetType,
    Command_ForSetVar, Command_ForSetStep,
    Command_NoOperation, Command_AssignLocalReference:
      Result := 0;
  else 
    raise ELinkerException.Create('Linker exception: Unknown bytecode operand: $' + IntToStr(Command));
  end;
end;

class function THLVMLinker.GetRelatedScopeClosure(const State: THLVMLinkerPassState; const Pos: Integer): Integer;
var i, Counter: Integer;
begin
  Counter := 1;
  Result := 0;
  for i := Pos to State.RawBytecode.Count - 1 do
    case State.RawBytecode[i].Command of
      Command_OpenScope:
        Counter := Counter + 1;
      Command_CloseScope: begin
        Counter := Counter - 1;
        if Counter < 0 then
          raise ELinkerException.Create('Asm of scopes invalidation: closure of inexisting scope.');
        if Counter = 0 then begin
          Result := i;
          Break;
        end;
      end;
    end;
  if Result <= 0 then
    raise ELinkerException.Create('Asm of scopes invalidation: scope was not closed.');
end;

class function THLVMLinker.GetRelatedStackClosure(const BytecodeText: THLVMLinkerBytecodeText; const Pos: Integer): Integer;
var i, Counter: Integer;
begin
  Counter := 1;
  Result := -1;
  for i := Pos downto 0 do begin 
    case BytecodeText[i].Command of
      Command_RestoreStackPointer: 
        Counter := Counter + 1;
      Command_MoveStackPointer: 
        begin 
          Counter := Counter - 1;
          if Counter <= 0 then begin 
            Result := i;
            Break;
          end;
        end;
    end;
  end;
  if Result < 0 then
    raise ELinkerException.Create('Linker exception: Stack closure has not been completed!');
end;

class function THLVMLinker.IsCommandIgnorable(const Command: HCommand; const IgnoreJumps: Boolean): Boolean;
begin
  Result := False;
  case Command of
    Command_ClearStack,
    Command_MoveStackPointer, 
    Command_RestoreStackPointer,
    Command_CheckStackInvisible:
      Result := True;
    Command_JMP,
    Command_JC,
    Command_JNC:
      Result := IgnoreJumps;
  end;
end;

class function THLVMLinker.Link(const Bytecode: THLVMRawChunk): THLVMLinkerResultingLinkedStructure;
var State: THLVMLinkerPassState;
    AdvState: THLVMLinkerAdvancedState;
    Variables: THLVMLinkerVariableScope;
    i: Integer;
begin
  { Creating state of Linker }
  State := THLVMLinkerPassState.Create(Bytecode);
  { Initialising it with bytecode }
  State.Processed := ProcessRawBytecode(State, 0, State.RawBytecode.Count - 1);
  AoTLink(State);
  BuildBytecodeMap(State);
  { Creating variables initial container }
  Variables.Variables.Nullify;
  Variables.HighestVariables := nil;
  Variables.CachedHighestVariables.Nullify;
  { Creating separate state for main body of bytecode }
  AdvState := THLVMLinkerAdvancedState.Create(State.Processed, State.Map, Variables);

  {#! Make OpenScope and CloseScope for main block }

  { Link main block }
  Result.MainBlock := GeneralLinkBytecode(State, AdvState);

  Result.Functions.ResizeNullify(State.FunctionsPool.Count);
  { Link all functions }
  for i := 0 to State.FunctionsPool.Count - 1 do
    Result.Functions.Items[i] := LinkFunction(State, AdvState, i);
end;

class function THLVMLinker.LinkCommandLet(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Pos: Integer; var ResultPos: Integer): HLVM_LinkerWord;
var i, Ops: Integer;
    PrevR, PrevPR, Bounds, Temp: Integer;
    Name: String;
    PreviousCount: Integer;
begin
  Ops := GetOperandsCount(AdvancedState.BytecodeText.Items[Pos].Command);

  Inc(AdvancedState.CompilationDepth);

  Result := HLVM_LinkerWord.Create(AdvancedState.BytecodeText.Items[Pos].Command);
//  Result.Create(AdvancedState.BytecodeText.Items[Pos].Command);
  Result.RegisterIndex := 0;
  Result.PointerRegisterIndex := 0;
  Result.LabeledIndex := AdvancedState.BytecodeText.Items[Pos].LabeledPosition;
  {#! Add position assign here }

  case AdvancedState.BytecodeText.Items[Pos].Command of
    Command_Assign: begin
      { Time to think. HMMMMM TIME! }
      { Assign does not return, as well as does not use any register space }

      PrevR := AdvancedState.RegisterPosition;
      PrevPR := AdvancedState.PointerRegisterPosition;

//      AdvancedState.RegisterPosition := PrevR;
//      AdvancedState.PointerRegisterPosition := PrevPR;

      { Totally wrong! }
      { Left side could be any - from PushVar to NewIndex }
      { No, it could not. }
      { Assign only works with PushVar }
      { For fields assign - NewIndex should be used }
      if Pos <= 0 then
        raise ELinkerException.Create('Linker exception: Assign instruction is placed wrongly, without operand to assign.');
      if not (AdvancedState.BytecodeText[Pos - 1].Command in [Command_PushVar, Command_PushLocal]) then
        raise ELinkerException.Create('Linker exception: Expected variable opcode after Command_Assign');

      if (Pos - 2) < 0 then
        raise ELinkerException.Create('Linker internal exception: Position of control instruction is out-of-bounds');

      Name := AdvancedState.BytecodeText[Pos - 1].StringStorage;

      // Result := HLVM_LinkerWord.Create(Command_Assign);
      Result.PointerRegisterIndex := PrevPR;
      Result.RegisterIndex := PrevR;
      
      { Possible misbehavior - PushVar in the beginning must be processed after, as it created illusion of existing variable. }
      { Changed now }
      
      Result.GeneralOperands.Size := 2;
      Result.GeneralOperands.SetCount(2);

      ResultPos := Pos - 2;
      if AdvancedState.BytecodeText[Pos - 2].Command in [ Command_Assign, Command_AssignLocalReference ] then begin
        { First assign is the last, means all enclosed are done before it }
//        Inc(AdvancedState.PointerRegisterPosition);

        AdvancedState.Result.AddReferenced^ := LinkCommandLet(State, AdvancedState, Pos - 2, ResultPos);

        Inc(AdvancedState.PointerRegisterPosition);
        PrevPR := AdvancedState.PointerRegisterPosition;
        PrevR := AdvancedState.RegisterPosition;

//        AdvancedState.PointerRegisterPosition := PrevPR;
//        AdvancedState.RegisterPosition := PrevR;
//
        ResultPos := ResultPos - 1;
      end
      else begin
        Inc(AdvancedState.PointerRegisterPosition);
        PrevPR := AdvancedState.PointerRegisterPosition;
      end;
//      else
//        ResultPos := ResultPos - 1;

      if AdvancedState.BytecodeText[ResultPos].Command in [ Command_MoveStackPointer, Command_OpenScope ] then begin
        { Empty assignment, assuming it is Command_PushNil }
        { No, it is not Command_PushNil, but the next level of PointerRegister }
        { Which pointer index we need to take? }
        Result.CommandID := Command_AssignFromMemory;
        Result.IntegerStorage := PrevPR;

        ResultPos := ResultPos + 1;

        Result.GeneralOperands.Size := 1;
        Result.GeneralOperands.SetCount(1);
      end
      else begin
        { If it is a normal instruction -> }
        while ResultPos >= 0 do begin
          if not IsCommandIgnorable(AdvancedState.BytecodeText[ResultPos].Command, False) then
            Break;
          ResultPos := ResultPos - 1;
        end;

        if GetCommandReturns(AdvancedState.BytecodeText[ResultPos].Command) = 0 then
          raise ELinkerException.Create('Linker exception: Assignment expected returnable opcode of operator');

        Result.GeneralOperands.Items[1] := LinkCommandLet(State, AdvancedState, ResultPos, ResultPos);
        Result.IntegerStorage := PrevPR;
      end;

      { All assignments should already be linked. }
      { Forced push var is internally processed }
      AdvancedState.PushVarForced := True;
//      Result.GeneralOperands.Items[0] := LinkPushVar(State, AdvancedState, AdvancedState.BytecodeText[Pos - 1], @Result.GeneralOperands.Items[0]);
      Result.GeneralOperands.Items[0] := LinkPushVarNew(State, AdvancedState, AdvancedState.BytecodeText[Pos - 1], @Result.GeneralOperands.Items[0]);
      { KadJIT uses [*0;0] position to allocate variable pointer, so multiple assignment from memory will work fine }
      Result.GeneralOperands.Items[0].PointerRegisterIndex := 0;
      Result.GeneralOperands.Items[0].RegisterIndex := 0;
      AdvancedState.PushVarForced := False;
      
      { For chaining command blocks }
//      Inc(AdvancedState.PointerRegisterPosition);

      AdvancedState.PointerRegisterPosition := PrevPR;
      AdvancedState.RegisterPosition := PrevR;

      Exit;
    end;

    Command_AssignLocalReference: begin 
      PrevR := AdvancedState.RegisterPosition;
      PrevPR := AdvancedState.PointerRegisterPosition;
    
      { Add variable }
      ResultPos := Pos;

      if (ResultPos > 0) and (AdvancedState.BytecodeText[ResultPos - 1].Command in [ Command_Assign, Command_AssignLocalReference ]) then begin 
        AdvancedState.Result.AddReferenced^ := LinkCommandLet(State, AdvancedState, ResultPos - 1, ResultPos);

        Inc(AdvancedState.PointerRegisterPosition);
        PrevPR := AdvancedState.PointerRegisterPosition;
        PrevR := AdvancedState.RegisterPosition;
      end
      else begin
        Inc(AdvancedState.PointerRegisterPosition);
        PrevPR := AdvancedState.PointerRegisterPosition;
      end;

      with AdvancedState.VariableScope.Variables.AddReferenced^ do begin 
        Name := AdvancedState.BytecodeText[Pos].StringStorage;
        IsLoopVariable := False;
        IsHighestVariable := False;
        ForbidHighUsage := False;
      end;

      Result.GeneralOperands.Size := 1;
      Result.GeneralOperands.SetCount(1);
      Result.GeneralOperands.Items[0] := HLVM_LinkerWord.Create(Command_PushVar);
      Result.GeneralOperands.Items[0].IntegerStorage := AdvancedState.VariableScope.Variables.Count - 1;
      Result.GeneralOperands.Items[0].StringStorage := AdvancedState.BytecodeText[Pos].StringStorage;
      { Add Forced modifier }
      Result.GeneralOperands.Items[0].AdditionalData := ScoperFlag_Var_PushKnownLocal or 1;

      { Assign pointer to instruction }
      AdvancedState.VariableScope.Variables.LastItemPointer^.InstructionPointer := @Result.GeneralOperands.Items[0];
    
      Exit;
//      raise ELinkerException.Create('Linker Exception: No support for reference variables currently');
    end;
    
    Command_PushVar, Command_PushLocal: begin
      if AdvancedState.CompilationDepth = 1 then begin
        { Useless command, but Linker should protect even it }

        { Why is it done sooooo badly if it is self-contained command? }
        { Reason is just so simple: pointer overwriting }
        { If PushVar is within any command, rewriting higher command will not affect actual PushVar pointer }
        { But if it is bare command, rewriting this command will make lots of Access Violations over linking }
        // Result := HLVM_LinkerWord.Create(JIT_Command_Container);
        Result.CommandID := JIT_Command_Container;
        Result.GeneralOperands.Nullify;
        Result.GeneralOperands.Size := 1;
        Result.GeneralOperands.SetCount(1);
//        Result.GeneralOperands.Items[0] := LinkPushVar(State, AdvancedState, AdvancedState.BytecodeText[Pos], @Result.GeneralOperands.Items[0]);
        Result.GeneralOperands.Items[0] := LinkPushVarNew(State, AdvancedState, AdvancedState.BytecodeText[Pos], @Result.GeneralOperands.Items[0]);
        Result.GeneralOperands.Items[0].RegisterIndex := AdvancedState.RegisterPosition;
        Result.GeneralOperands.Items[0].PointerRegisterIndex := AdvancedState.PointerRegisterPosition;
        //Inc(AdvancedState.PointerRegisterPosition);
      end
      else begin
//        Result := LinkPushVar(State, AdvancedState, AdvancedState.BytecodeText[Pos], nil);
        Result := LinkPushVarNew(State, AdvancedState, AdvancedState.BytecodeText[Pos], nil);
        Result.PointerRegisterIndex := AdvancedState.PointerRegisterPosition;
        Result.RegisterIndex := AdvancedState.RegisterPosition;
        //Inc(AdvancedState.PointerRegisterPosition);
      end;

      ResultPos := Pos;
      // ResultPos := Pos - 1;

      Exit;
    end;

    Command_OpenScope: begin
      // Result := HLVM_LinkerWord.Create(Command_OpenScope);
      Result.IntegerStorage := AdvancedState.Scopes.Count + 1;

      AdvancedState.Scopes.Add(AdvancedState.VariableScope.Variables.Count);

//      ResultPos := Pos - 1;
      ResultPos := Pos;

      Exit;
    end;

    Command_CloseScope: begin
//      if AdvancedState.Scopes.Count <= 0 then
//        raise ELinkerException.Create('Linker internal exception: Cannot properly determine scope closure, because there is not entrance point!');
//      PreviousCount := AdvancedState.Scopes.LastItem;

      if AdvancedState.LoopInfo.Count > 0 then begin 
        if AdvancedState.LoopInfo.LastItem.Depth = AdvancedState.Scopes.Count then
          AdvancedState.LoopInfo.Remove;
      end;

      // Result := HLVM_LinkerWord.Create(Command_CloseScope);
      Result.IntegerStorage := AdvancedState.Scopes.Count - 1;
      if Result.IntegerStorage >= 0 then
        PreviousCount := AdvancedState.Scopes[Result.IntegerStorage]
      else
        PreviousCount := 0;
      { Notes: KadJIT Finalisation v1 }
      Result.AdditionalData := PreviousCount;

//      for i := AdvancedState.VariableScope.Variables.Count - 1 downto PreviousCount do begin
//        if AdvancedState.VariableScope.Variables[i].IsHighestVariable then begin
//          { It is not local highest variable }
//          if not AdvancedState.VariableScope.Variables[i].Local then
//            Continue;
//
//          if AdvancedState.VariableScope.Variables[i].InstructionPointer = nil then
//            raise ELinkerException.Create('Linker internal exception: Local variable does not provide pointer interface.');
//
//          { As we now know that this variable is Highest, we need to allocate it in GRC }
//          AdvancedState.VariableScope.Variables.Items[i].InstructionPointer^.CommandID := JIT_Command_PushHighestVariable;
//
//          { It should be properly created and disposed }
//          AdvancedState.Result.AddReferenced^ := HLVM_LinkerWord.Create(JIT_Command_DisposeHighestVariable);
//          with AdvancedState.Result.LastItemPointer^ do begin
//            { Index that needs to be disposed }
////            IntegerStorage := AdvancedState.VariableScope.Variables[i].Index;
//            IntegerStorage := i;
//          end;
//        end;
//      end;
//
//      AdvancedState.Scopes.Remove;
//      AdvancedState.VariableScope.Variables.SetCount(PreviousCount);
//
////       Result := HLVM_LinkerWord.Create(Command_CloseScope);
//      Result.IntegerStorage := AdvancedState.Scopes.Count;

//      ResultPos := Pos - 1;

      Temp := AdvancedState.Result.Count;
//      Writeln('Now temp is ', Temp);

      CloseAndFinaliseScopes(State, AdvancedState, Result.IntegerStorage, True, False);
      ResultPos := Pos;

      if (Temp < AdvancedState.Result.Count) and (Result.LabeledIndex >= 0) then begin
//        Writeln('And it is now here');
        { If something is disposed, and the CloseScope acts as point of JMP }
        { Then we are redirecting JMP to the first instruction of disposal  }
        AdvancedState.Result.Items[Temp].LabeledIndex := Result.LabeledIndex;
        Result.LabeledIndex := -1;
      end;

      Exit;
    end;

    Command_ForCheck: begin 
      Result := LinkForCheck(State, AdvancedState, Pos);
    
      if AdvancedState.LoopInfo.Count <= 0 then
        raise ELinkerException.Create('Linker Exception: Cannot link ForCheck outside the loop scope!');

      Result.AdditionalPlusData := AdvancedState.LoopInfo.LastItem.BreakPos;
//      ResultPos := Pos - 1;

      ResultPos := Pos;
      Exit;
    end;

    Command_ForSetStart: begin 
      { ForSetStart has 1 operands }
      Result := LinkForStart(State, AdvancedState, Pos);

      Result.GeneralOperands.Size := 1;
      Result.GeneralOperands.SetCount(1);
      Result.GeneralOperands[0] := LinkCommandLet(State, AdvancedState, Pos - 1, ResultPos);
      
      Exit;
    end;
    { SetBreak }
    Command_SetBreak: begin 
      if AdvancedState.LoopInfo.Count > 0 then begin
        if AdvancedState.LoopInfo.LastItem.Depth = AdvancedState.Scopes.Count then
          raise ELinkerException.Create('Linker Exception: SetBreak cannot be set twice on the same scope!');
      end;

      Result.IntegerStorage := AdvancedState.BytecodeText[Pos].IntegerStorage;
      with AdvancedState.LoopInfo.AddReferenced^ do begin
        BreakPos := Result.IntegerStorage;
        ContinuePos := -1;
        Depth := AdvancedState.Scopes.Count;
      end;

      ResultPos := Pos;
      Exit;
    end;
    { SetContinue }
    Command_SetContinue: begin 
      if (AdvancedState.LoopInfo.Count <= 0) or (AdvancedState.LoopInfo.LastItem.ContinuePos >= 0) then
        raise ELinkerException.Create('Linker Exception: SetContinue is called outside the loop scope/before SetBreak/twice in the same scope!');

      Result.IntegerStorage := AdvancedState.BytecodeText[Pos].IntegerStorage;
      with AdvancedState.LoopInfo.LastItemPointer^ do 
        ContinuePos := Result.IntegerStorage;
        
      ResultPos := Pos;
      Exit;
    end;

    { PushFunction }
    Command_Internal_PushFunction: begin 
      { Index of function }
      i := AdvancedState.BytecodeText[Pos].IntegerStorage;
      WriteLn('Linking function #', i);
      if (i < 0) or (i >= State.FunctionsPool.Size) then
        raise ELinkerException.Create('Linker internal exception: Somehow function insertion had overflown the list.');
      State.FunctionsPool.Items[i].Result := Self.LinkFunction(State, AdvancedState, i);

      ResultPos := Pos;
      Exit;
    end;

    { ForSetVar }
    Command_ForSetVar: begin
      with AdvancedState.VariableScope.Variables.AddReferenced^ do begin
        Name := AdvancedState.BytecodeText[Pos].StringStorage;
        IsHighestVariable := False;
        IsLoopVariable := True;
        Local := True;
        InstructionPointer := nil;
        Index := -1;
      end;
      Result.IntegerStorage := AdvancedState.VariableScope.Variables.LastIndex;
      Result.StringStorage := AdvancedState.BytecodeText[Pos].StringStorage;

      ResultPos := Pos;
      Exit;
    end;

    { JMP }
    Command_JMP: begin
      { If there is CloseScope after, then it is loop construction }
      Result.IntegerStorage := AdvancedState.BytecodeText[Pos].IntegerStorage;
      if AdvancedState.BytecodeText.Count > Pos + 1 then
        if AdvancedState.BytecodeText[Pos + 1].Command = Command_CloseScope then begin
          { We need to do it, as loop continues }
          CloseAndFinaliseScopes(State, AdvancedState, AdvancedState.Scopes.Count - 1, False, False);
//          Exit;
        end;

      ResultPos := Pos;
      Exit;
    end;

    { Continue }
    Command_ContinueLoop: begin
      { Need to dispose all the variables until continuing }
      { Continue generates more expensive bytecode, than Break does }
      { Now there is no difference }
      if AdvancedState.LoopInfo.Count <= 0 then
        raise ELinkerException.Create('Linker Exception: Continue is found, but there is no loop information!');

      CloseAndFinaliseScopes(State, AdvancedState, AdvancedState.LoopInfo.LastItem.Depth - 1 {AdvancedState.Scopes.Count - 1}, False, True);

      { And write label position to continue from }
      Result.AdditionalPlusData := AdvancedState.LoopInfo.LastItem.ContinuePos;

      ResultPos := Pos;
      Exit;
    end;

    { Break }
    Command_BreakLoop: begin
      { The same as Continue Loop }
      { I decided to do it that compicated, but safe }
      if AdvancedState.LoopInfo.Count <= 0 then
        raise ELinkerException.Create('Linker Exception: Break is found, but there is no loop information!');

      CloseAndFinaliseScopes(State, AdvancedState, AdvancedState.LoopInfo.LastItem.Depth - 1, False, True);

      { And write label position to break to }
      Result.AdditionalPlusData := AdvancedState.LoopInfo.LastItem.BreakPos;

      ResultPos := Pos;
      Exit;
    end;
  end;

  { Register position is now computed differently and optimised for big linking }
//  Temp := GetCommandReturns(AdvancedState.BytecodeText[Pos].Command);
//  if (Temp = -1) or (Temp = 1) then begin
//    Result.PointerRegisterIndex := AdvancedState.PointerRegisterPosition;
//    Inc(AdvancedState.PointerRegisterPosition);
//  end;
//  if GetCommandRegisterUsage(AdvancedState.BytecodeText[Pos].Command) > 0 then begin 
//    Result.RegisterIndex := AdvancedState.RegisterPosition;
//    Inc(AdvancedState.RegisterPosition, 1);
//  end;
  Result.PointerRegisterIndex := AdvancedState.PointerRegisterPosition;
  Result.RegisterIndex := AdvancedState.RegisterPosition;

  PrevR := AdvancedState.RegisterPosition;
  PrevPR := AdvancedState.PointerRegisterPosition;
  if Ops >= 0 then begin 
    Result.GeneralOperands.Size := Ops;
    Result.GeneralOperands.SetCount(Ops);

    with AdvancedState.BytecodeText.Items[Pos] do begin
      Result.StringStorage := StringStorage;
      Result.IntegerStorage := IntegerStorage;
      Result.FloatStorage := FloatStorage;
    end;
    
    { If no more ops }
    ResultPos := Pos;
    for i := 1 to Ops do begin
      ResultPos := ResultPos - 1;
      while (ResultPos >= 0) and IsCommandIgnorable(AdvancedState.BytecodeText.Items[ResultPos].Command, True) do
        ResultPos := ResultPos - 1;
      if ResultPos < 0 then
        raise ELinkerException.Create('Linker internal exception: Cross-instruction boundary has been hit while linking operand');

      if not (GetCommandReturns(AdvancedState.BytecodeText.Items[ResultPos].Command) = 1) then
        raise ELinkerException.Create('Linker internal exception: Expected single return operation');
      Temp := GetCommandRegisterUsage(AdvancedState.BytecodeText.Items[ResultPos].Command);
        
      Result.GeneralOperands.Items[i - 1] := LinkCommandLet(State, AdvancedState, ResultPos, ResultPos);

      { It have to return somewhere }
      Inc(AdvancedState.PointerRegisterPosition);
      if Temp > 0 then
        Inc(AdvancedState.RegisterPosition);
    end;
  end
  else begin 
    with AdvancedState.BytecodeText.Items[Pos] do begin
      Result.IntegerStorage := IntegerStorage;
      Result.FloatStorage := FloatStorage;
      Result.StringStorage := StringStorage;
    end;
  
    ResultPos := Pos;
    ResultPos := ResultPos - 1;
    Bounds := GetIndefiniteCommandBounds(AdvancedState.BytecodeText, Pos);
    while ResultPos > Bounds do begin 
      while (ResultPos >= 0) and IsCommandIgnorable(AdvancedState.BytecodeText.Items[ResultPos].Command, True) do
        ResultPos := ResultPos - 1;
      if ResultPos < 0 then
        raise ELinkerException.Create('Linker internal exception: Cross-instruction boundary has been hit while linking long operand');
      if ResultPos <= Bounds then
        raise ELinkerException.Create('Linker exception: Long operand linking has been finished faultly');
        
      if not (GetCommandReturns(AdvancedState.BytecodeText.Items[ResultPos].Command) = 1) then
        raise ELinkerException.Create('Linker internal exception: Expected single return operation while linking long operand');
      Temp := GetCommandRegisterUsage(AdvancedState.BytecodeText.Items[ResultPos].Command);
        
      Result.GeneralOperands.AddReferenced^ := LinkCommandLet(State, AdvancedState, ResultPos, ResultPos);
      ResultPos := ResultPos - 1;

      { Every operand returns }
      Inc(AdvancedState.PointerRegisterPosition);
      if Temp > 0 then
        Inc(AdvancedState.RegisterPosition);
    end;

    if not (ResultPos = Bounds) then
      raise ELinkerException.Create('Linker exception: Long operand linking lead to linker fault');
  end;
  AdvancedState.RegisterPosition := PrevR;
  AdvancedState.PointerRegisterPosition := PrevPR;
end;

class function THLVMLinker.LinkCompatible(const Bytecode: THLVMRawChunk): THLVMBytecodeText;
var State: THLVMLinkerPassState;
    AdvState: THLVMLinkerAdvancedState;
    Variables: THLVMLinkerVariableScope;
    i: Integer;
    Resulting: THLVMLinkerResultingLinkedStructure;
begin

  { Creating state of Linker }
  State := THLVMLinkerPassState.Create(Bytecode);
  { Initialising it with bytecode }
//  WriteLn('Processing raw bytecode...');
  State.Processed := ProcessRawBytecode(State, 0, State.RawBytecode.Count - 1);
//  WriteLn('AoT linking...');
  AoTLink(State);
//  WriteLn('Building bytecode map...');
  BuildBytecodeMap(State);
  { Creating variables initial container }
  Variables.Variables.Nullify;
  Variables.HighestVariables := nil;
  Variables.CachedHighestVariables.Nullify;
  { Creating separate state for main body of bytecode }
  AdvState := THLVMLinkerAdvancedState.Create(State.Processed, State.Map, Variables);

  { Link main block }
  Resulting.Functions.ResizeNullify(State.FunctionsPool.Count);
  Resulting.Functions.SetCount(State.FunctionsPool.Count);

//  Writeln('Linking main block...');
  Resulting.MainBlock := GeneralLinkBytecode(State, AdvState);

//  Resulting.Functions.ResizeNullify(State.FunctionsPool.Count);
  { Link all functions }
//  for i := 0 to State.FunctionsPool.Count - 1 do
//    Resulting.Functions.Items[i] := LinkFunction(State, AdvState, i);
  for i := 0 to State.FunctionsPool.Count - 1 do
    Resulting.Functions.Items[i] := State.FunctionsPool.Items[i].Result;

  Result.MainChunk.NullifySelf;
  SetLength(Result.Functions, 0);

  { Old standard of execution must be compatible }
//  WriteLn('Translation...');
  Result := Translate(Resulting);

  (* Headers are deprecated *)
//  Result.MainChunk.Header.Nullify;

//  with Result.MainChunk.Header.AddReferenced^ do begin
//    Code := JIT_Header_LocalHeapSize;
//    IntStorage := AdvState.VariableScope.Variables.Size;
//  end;
//  with Result.MainChunk.Header.AddReferenced^ do begin
//    Code := JIT_Header_GlobalCacheSize;
//    IntStorage := State.GlobalVariables.Size;
//  end;
//
//  for i := 0 to State.GlobalVariables.Count - 1 do begin
//    with Result.MainChunk.Header.AddReferenced^ do begin
//      Code := JIT_Header_CacheGlobal;
//      StringStorage := State.GlobalVariables[i].Name;
//    end;
//  end;
end;

class function THLVMLinker.LinkPushVar(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Instruction: THLVMLinkerBytecodeInstruction; const InstrPointer: PHLVM_LinkerWord): HLVM_LinkerWord;
var i, Index: Integer;
    VarObject: THLVMLinkerVariableObject;
begin
  Result := HLVM_LinkerWord.Create(Command_PushVar);
  Result.LabeledIndex := Instruction.LabeledPosition;
  {#! Add position here }

  case Instruction.Command of
    Command_PushVar: begin 
      Index := -1;
      for i := 0 to AdvancedState.VariableScope.Variables.Count - 1 do begin 
        if AdvancedState.VariableScope.Variables[i].Name = Instruction.StringStorage then begin 
          { Found variable! }
          Index := i;
          if not AdvancedState.VariableScope.Variables[i].Local then
            raise ELinkerException.Create('Linker exception: Variable scope contains invalid local reference');
//          Result := HLVM_LinkerWord.Create(JIT_Command_PushKnownLocal);
          // Result := HLVM_LinkerWord.Create(Command_PushVar);
          Result.StringStorage := Instruction.StringStorage;

          Result.AdditionalData := ScoperFlag_Var_PushKnownLocal;
          if AdvancedState.PushVarForced then
            Result.AdditionalData := Result.AdditionalData or 1;

          Result.StringStorage := Instruction.StringStorage;
          Result.IntegerStorage := i;
        end;
      end;
      if Index >= 0 then
        Exit;
      Index := AdvancedState.VariableScope.TryFindHighestVariable(Instruction.StringStorage);
      if Index >= 0 then begin 
        // Result := HLVM_LinkerWord.Create(JIT_Command_PushHighestVariable);
        Result.CommandID := JIT_Command_PushHighestVariable;
        
        Result.StringStorage := Instruction.StringStorage;
        Result.IntegerStorage := Index;
        Exit;
      end;

      { Global variable }
      for i := 0 to State.GlobalVariables.Count - 1 do
        if State.GlobalVariables[i].Name = Instruction.StringStorage then begin 
          Index := i;
          Break;
        end;

      if Index >= 0 then begin 
        { There is a cached global variable }
        // Result := HLVM_LinkerWord.Create(JIT_Command_PushKnownGlobal);
        // Result := HLVM_LinkerWord.Create(Command_PushVar);
        Result.AdditionalData := ScoperFlag_Var_PushKnownGlobal;

        Result.StringStorage := Instruction.StringStorage;
        Result.IntegerStorage := Index;
      end
      else if AdvancedState.PushVarForced then begin
        { We can cache this variable, as it will never be undefined nil after that }
        with State.GlobalVariables.AddReferenced^ do begin
          { Add new one to the cache of linker and execution provider }
          Name := Instruction.StringStorage;
        end;
        // Result := HLVM_LinkerWord.Create(JIT_Command_PushKnownGlobal);
        // Result := HLVM_LinkerWord.Create(Command_PushVar);

        Result.StringStorage := Instruction.StringStorage;
        Result.AdditionalData := ScoperFlag_Var_PushKnownGlobal;
        Result.IntegerStorage := State.GlobalVariables.LastIndex;

        { Increase counter of global variables cache }
        { Deprecated and not used anymore }
//        Inc(State.MaxGlobalVariables);
      end
      else begin
        { Unfortunately, we could not do optimisations here }
//        Result := HLVM_LinkerWord.Create(JIT_Command_PushGlobal);
        // Result := HLVM_LinkerWord.Create(Command_PushVar);

        Result.AdditionalData := ScoperFlag_Var_PushRawGlobal;
        Result.StringStorage := Instruction.StringStorage;
      end;

      Exit;
    end;
    Command_PushLocal: begin
      VarObject.Nullify;
      VarObject.Name := Instruction.StringStorage;
      VarObject.Local := True;
      VarObject.Index := AdvancedState.VariableScope.Variables.Count;

      // Result := HLVM_LinkerWord.Create(JIT_Command_PushKnownLocal);
      // Result := HLVM_LinkerWord.Create(Command_PushVar);
      Result.AdditionalData := ScoperFlag_Var_PushKnownLocal;
      if AdvancedState.PushVarForced then
        Result.AdditionalData := Result.AdditionalData or 1;

      Result.IntegerStorage := VarObject.Index;
      Result.StringStorage := Instruction.StringStorage;

//      VarObject.InstructionPointer := @Result;
      VarObject.InstructionPointer := InstrPointer;

      AdvancedState.VariableScope.Variables.Add(VarObject);
      if not (VarObject.Index = AdvancedState.VariableScope.Variables.LastIndex) then
        raise ELinkerException.Create('Linker exception: Variable link critical fault: memory has been overwritten');

      Exit;
    end;
  else 
    raise ELinkerException.Create('Linker exception: Invalid operational code for variable push.');
  end;
  raise ELinkerException.Create('Linker exception: Unreacheable state of code is touched ~[HLVM.JIT.Context.Linker.New.pas:THLVMLinker.LinkPushVar():?]');
end;

class function THLVMLinker.LinkPushVarNew(const State: THLVMLinkerPassState; var AdvancedState: THLVMLinkerAdvancedState; const Instruction: THLVMLinkerBytecodeInstruction; const InstrPointer: PHLVM_LinkerWord): HLVM_LinkerWord;
var i, Index, LastCount: Integer;
begin
  Result := HLVM_LinkerWord.Create(Command_PushVar);

  case Instruction.Command of
    Command_PushVar:
      begin
        Index := AdvancedState.VariableScope.FindLocalVariableNew(Instruction.StringStorage);
        if Index >= 0 then begin
          { There is a local variable stored in the scope }
          Result.AdditionalData := ScoperFlag_Var_PushKnownLocal or Integer(AdvancedState.PushVarForced);
          Result.AdditionalPlusData := Index;
          Result.IntegerStorage := Index;
          Result.StringStorage := Instruction.StringStorage;

          Exit;
        end;

        { Highest variables }
        LastCount := AdvancedState.VariableScope.Variables.Count;
        Index := AdvancedState.VariableScope.ResolveHighestVariableLinkNew(Instruction.StringStorage);
        if Index >= 0 then begin
          { There is a highest variable }
          Result.AdditionalData := ScoperFlag_Var_PushHighest or Integer(AdvancedState.PushVarForced);
          Result.AdditionalPlusData := Index;
          Result.IntegerStorage := Index;
          Result.StringStorage := Instruction.StringStorage;

          if not (LastCount = AdvancedState.VariableScope.Variables.Count) then begin 
            { We bring highest variable, so we must assign it }
            AdvancedState.Result.Add(HLVM_LinkerWord.Create(JIT_Command_Heading_CacheHighest));
            with AdvancedState.Result.LastItemPointer^ do begin 
              IntegerStorage := AdvancedState.VariableScope.Variables.LastItemPointer.Index;
              AdditionalData := LastCount;
            end;
          end;

          Exit;
        end;

        { Global? }
        Index := -1;
        for i := 0 to AdvancedState.GlobalVariables.Count - 1 do
          if AdvancedState.GlobalVariables.Items[i].Name = Instruction.StringStorage then begin
            Index := i;
            Break;
          end;

        if Index >= 0 then begin
          { Known global variable to push }
          Result.AdditionalData := ScoperFlag_Var_PushKnownGlobal or Integer(AdvancedState.PushVarForced);
          Result.AdditionalPlusData := Index;
          Result.IntegerStorage := Index;
          Result.StringStorage := Instruction.StringStorage;
        end
        else begin
          { It is unknown global variable }
          if AdvancedState.PushVarForced then begin
            { But there we know it is not undefined nil anymore }
            with AdvancedState.GlobalVariables.AddReferenced^ do begin
              Name := Instruction.StringStorage;
            end;

            Result.AdditionalData := ScoperFlag_Var_PushKnownGlobal or Integer(AdvancedState.PushVarForced);
            Result.AdditionalPlusData := AdvancedState.GlobalVariables.LastIndex;
            Result.IntegerStorage := AdvancedState.GlobalVariables.LastIndex;
            Result.StringStorage := Instruction.StringStorage;
          end
          else begin
            { There we have no information about this global variable }
            Result.AdditionalData := ScoperFlag_Var_PushRawGlobal or Integer(AdvancedState.PushVarForced);
            Result.StringStorage := Instruction.StringStorage;
          end;
        end;

        Exit;
      end;

    Command_PushLocal:
      begin
        if AdvancedState.Scopes.Count > 0 then
          LastCount := AdvancedState.Scopes.LastItem
        else
          LastCount := -1;

        Index := AdvancedState.VariableScope.FindLocalVariableNew(Instruction.StringStorage);
        if Index < 0 then begin
          with AdvancedState.VariableScope.Variables.AddReferenced^ do begin
            Name := Instruction.StringStorage;
            Local := True;
            IsHighestVariable := False;
            InstructionPointer := InstrPointer;
          end;

          Result.AdditionalData := ScoperFlag_Var_PushKnownLocal or 1;
          Result.AdditionalPlusData := AdvancedState.VariableScope.Variables.LastIndex;
          Result.IntegerStorage := Result.AdditionalPlusData;
          Result.StringStorage := Instruction.StringStorage;
        end
        else if Index < LastCount then begin
          { Local variable is not from local scope, means it should be created here }
          Index := AdvancedState.VariableScope.Variables.Count;

          with AdvancedState.VariableScope.Variables.AddReferenced^ do begin
            Name := Instruction.StringStorage;
            Local := True;
            IsHighestVariable := False;
            InstructionPointer := InstrPointer;
          end;

          Result.AdditionalData := ScoperFlag_Var_PushKnownLocal or 1;
          Result.AdditionalPlusData := Index;
          Result.IntegerStorage := Index;
          Result.StringStorage := Instruction.StringStorage;
        end
        else begin
          { Variable with this name already here }
          { And we do not change the loop? field, as it can be loop variable }
          Result.AdditionalData := ScoperFlag_Var_PushKnownLocal or Integer(AdvancedState.PushVarForced);
          Result.AdditionalPlusData := Index;
          Result.IntegerStorage := Index;
          Result.StringStorage := Instruction.StringStorage;
        end;

        Exit;
      end;

  else
    raise ELinkerException.Create('Linker Exception: PushVar has invalid opcode to process.');
  end;
  raise ELinkerException.Create('Linker Exception: Linker had entered forbidden section.');
end;

class function THLVMLinker.MapBytecode(const State: THLVMLinkerPassState; const BytecodeText: THLVMLinkerBytecodeText): THLVMLinkerBytecodeMap;
var i, Ops, Last: Integer;
    Instr: ^THLVMLinkerBytecodeInstruction;
begin
  Result.Nullify;
  Last := -1;
  i := BytecodeText.Count - 1;
  while i >= 0 do begin
    Instr := @BytecodeText.Items[i];
    if IsCommandIgnorable(Instr.Command, False) then begin 
      i := i - 1;
      Continue;
    end;

    Last := i;
    i := GetCommandBounds(BytecodeText, Last);
    
    { Last is > than i }
    if {i - Last} Last - i >= 0 then
      with Result.AddReferenced^ do begin  
        From := Last;
        UpTo := i;
      end;
    
    i := i - 1;
  end;
  { Result is returned }
end;

class function THLVMLinker.ProcessRawBytecode(var State: THLVMLinkerPassState; const Pos, UpTo: Integer): THLVMLinkerBytecodeText;
var i, LastLabel, ScopePos, FunctionMark, Index: Integer;
    Func: PHLVMLinkerFunctionContainer;
begin
  { Method for processing `Label` and `CreateFunction` }
  Result.Nullify;

  if UpTo - Pos <= 0 then
    Exit;
    
  { Predicting size }
  Result.Size := UpTo - Pos + 1;

  LastLabel := -1;
  Index := 0;
  i := Pos;
//  for i := Pos to State.RawBytecode.Count - 1 do begin
  while i <= UpTo do begin
    case State.RawBytecode[i].Command of
      Command_LABEL: begin
        { On first link, we keep the relative code reference (RCR) }
        LastLabel := State.RawBytecode[i].IntContent;
      end;
      Command_CreateFunction: begin
        { HLVM_STD: CreateFunction should be followed by OpenScope     }
        { So, we are ignoring next, assuming it is standard form       }
        { Method will automatically raise an exception on invalid call }
        ScopePos := Self.GetRelatedScopeClosure(State, i + 2);
        { This is index of insertion }
        Func := PHLVMLinkerFunctionContainer(State.FunctionsPool.AddReferenced);
        Func.Body.Nullify;
        Func.Map.Nullify;
        Func.VariableScope.Variables.Nullify;
        Func.VariableScope.HighestVariables := nil;
        Func.VariableScope.CachedHighestVariables.Nullify;
        Func.Result.Nullify;
        
        FunctionMark := State.FunctionsPool.LastIndex;
        
        Func.Compiled := False;
        Func.Marker := FunctionMark;
        // { Not including OpenScope and CloseScope }
        { Now including }
//        Func.Body := Self.ProcessRawBytecode(State, i + 2, ScopePos - 1);
        Func.Body := Self.ProcessRawBytecode(State, i + 1, ScopePos);

        if Index > Result.Size then begin
          Result.Add(THLVMLinkerBytecodeInstruction.Create(
            { Creating it with FunctionMark as index of insertion of a function }
            Command_Internal_PushFunction, State.RawBytecode[i].Position, FunctionMark
          ));
          if not (Index = Result.LastIndex) then
            raise ELinkerException.Create('Linker internal exception: Insertion index mismatch!');
        end
        else
          Result[Index] := THLVMLinkerBytecodeInstruction.Create(
            Command_Internal_PushFunction, State.RawBytecode[i].Position, FunctionMark
          );

        if LastLabel >= 0 then begin
          Result.Items[Index].LabeledPosition := LastLabel;
          LastLabel := -1;
        end
        else
          Result.Items[Index].LabeledPosition := -1;
        Index := Index + 1;

        i := ScopePos;
      end;
    else
      { Main logic }
      if Index > Result.Size then begin 
        Result.Add(THLVMLinkerBytecodeInstruction.Create(State.RawBytecode[i]));
        if not (Index = Result.LastIndex) then
          raise ELinkerException.Create('Linker internal exception: Insertion index mismatch ~[HLVM.JIT.Context.Linker.?.pas:?]');
      end
      else 
        Result[Index] := THLVMLinkerBytecodeInstruction.Create(State.RawBytecode[i]);
      
      if LastLabel >= 0 then begin
        Result.Items[Index].LabeledPosition := LastLabel;
        LastLabel := -1;
      end
      else 
        Result.Items[Index].LabeledPosition := -1;
      Index := Index + 1;
    end;

    i := i + 1;
  end;

  if LastLabel >= 0 then begin
    { Label is set at the end of bytecode }
    Result.Add(THLVMLinkerBytecodeInstruction.Create(Command_NoOperation, Result.LastItem.Position, 0, 0, ''));
    with Result.LastItemPointer^ do begin
      LabeledPosition := LastLabel;
    end;

    LastLabel := -1;
  end;
end;

class function THLVMLinker.Translate(const LinkedStructure: THLVMLinkerResultingLinkedStructure): THLVMBytecodeText;
var i, k, Count: Integer;
    InternalStorage: ^THLVMInternalLinkerStorage;
    Linked: ^THLVMLinkerLinkedStructure;
begin
  Count := 0;
  for i := 0 to LinkedStructure.MainBlock.Count - 1 do
    Count := Count + TranslateCountOfOps(LinkedStructure.MainBlock[i]);

  {#! It now only compiles the main chunk without any functions! }
  Result.MainChunk.InternalStorage.Size := Count;
  Result.MainChunk.ActualStorage.Size := LinkedStructure.MainBlock.Count;

  InternalStorage := @Result.MainChunk.InternalStorage;
  
  for i := 0 to LinkedStructure.MainBlock.Count - 1 do
    Result.MainChunk.ActualStorage[i] := TranslateInContext(LinkedStructure.MainBlock[i], InternalStorage^);

  { Try to link a translate a function }
  SetLength(Result.Functions, LinkedStructure.Functions.Count);
  for i := 0 to LinkedStructure.Functions.Count - 1 do begin 
    Linked := @LinkedStructure.Functions.Items[i];
    Count := 0;
    for k := 0 to Linked.Count - 1 do 
      Count := Count + TranslateCountOfOps(Linked.Items[i]);

    Result.Functions[i].InternalStorage.Size := Count;
    Result.Functions[i].ActualStorage.Size := Linked.Count;

    InternalStorage := @Result.Functions[i].InternalStorage;

    for k := 0 to Linked.Count - 1 do
      Result.Functions[i].ActualStorage[k] := TranslateInContext(Linked.Items[k], InternalStorage^);
  end;
end;

class function THLVMLinker.TranslateCountOfOps(const LWord: HLVM_LinkerWord): Integer;
var i: Integer;
begin
  Result := 1;
  for i := 0 to LWord.GeneralOperands.Count - 1 do
    Result := Result + TranslateCountOfOps(LWord.GeneralOperands[i]);
end;

class function THLVMLinker.TranslateInContext(const LWord: HLVM_LinkerWord; var Storage: THLVMInternalLinkerStorage): Integer;
var i: Integer;
    OldWord: HLVM.JIT.Context.Linker.HLVM_LinkerWord;
begin
  Result := Storage.Count;

  OldWord.CommandID := LWord.CommandID;
  OldWord.RegisterIndex := LWord.RegisterIndex;
  OldWord.PointerRegisterIndex := LWord.PointerRegisterIndex;
  OldWord.UsesRegister := LWord.UsesRegister;
    
  OldWord.IntegerStorage := LWord.IntegerStorage;
  OldWord.FloatStorage := LWord.FloatStorage;
  OldWord.StringStorage := LWord.StringStorage;
  OldWord.LabeledIndex := LWord.LabeledIndex;

  OldWord.AdditionalData := LWord.AdditionalData;
  OldWord.AdditionalPlusData := LWord.AdditionalPlusData;
  OldWord.DataPointer := LWord.DataPointer;

  { Just reserve space! }
  Storage.Add(OldWord);

  OldWord.AbstractOperands.ResizeNullify(LWord.GeneralOperands.Count);
  for i := 0 to LWord.GeneralOperands.Count - 1 do begin  
    OldWord.AbstractOperands[i] := TranslateInContext(LWord.GeneralOperands[i], Storage);
  end;

  Storage.Items[Result] := OldWord;
end;

{ THLVMLinkerPassState }

constructor THLVMLinkerPassState.Create(const RawBytecode: THLVMRawChunk);
begin
  Self.RawBytecode := RawBytecode;
  Processed.Nullify;
  FunctionsPool.Nullify;

  MaxGlobalVariables := 0;
  Header.Nullify;
  GlobalVariables.Nullify;
end;

{ THLVMLinkerAdvancedState }

constructor THLVMLinkerAdvancedState.Create(const BytecodeText: THLVMLinkerBytecodeText; const Map: THLVMLinkerBytecodeMap; const Variables: THLVMLinkerVariableScope);
begin
  Self.BytecodeText := BytecodeText;
  Self.Map := Map;
  Self.VariableScope := Variables;

  Self.Scopes.Nullify;
  Self.Result.Nullify;
  GlobalVariables.Nullify;
  HighestVaraiblesCache.Nullify;

  RegisterPosition := 0;
  PointerRegisterPosition := 0;

  PushVarForced := False;

  LoopInfo.Nullify;
end;

{ THLVMLinkerVariableScope }

function THLVMLinkerVariableScope.FindHighestCachedVariable(const Name: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to CachedHighestVariables.Count - 1 do begin 
    if CachedHighestVariables[i].Name = Name then begin 
      Result := i;
      Break;
    end;
  end;
end;

function THLVMLinkerVariableScope.FindLocalVariable(const Name: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Variables.Count - 1 do begin
    if (Variables[i].Name = Name) then begin 
      Result := i;
      { It can be non-local }
      { But not with current version of linker }
      if not Variables[i].Local then
        raise ELinkerException.Create('Linker internal exception: Non local variable is in the local scope!');
      Break;
    end;
  end;
end;

function THLVMLinkerVariableScope.FindLocalVariableNew(const Name: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := Variables.Count - 1 downto 0 do
    if Variables[i].Name = Name then begin
      Result := i;
      { This method does not check if variable is strictly local, because now }
      { linker stores all locally bound variables in one scope, so cached highest }
      { variables are stored here too, but they are not marked with `Local` for }
      { linker not to dispose them on the end of scope }
      Break;
    end;
end;

function THLVMLinkerVariableScope.ResolveHighestVariableLinkNew(const VarName: String): Integer;
var i: Integer;
begin
  { If it is not root, then }
  if not (HighestVariables = nil) then begin
    { It should be changed }
//    Result := HighestVariables.FindLocalVariableNew(Name);
    Result := -1;
    for i := HighestVariables.Variables.Count - 1 downto 0 do begin
      {#! Specification question - should raise if ForbidHighUsage }
      if not HighestVariables.Variables.Items[i].IsLoopVariable and not HighestVariables.Variables.Items[i].ForbidHighUsage and (HighestVariables.Variables.Items[i].Name = VarName) then begin
        Result := i;
        Break;
      end;
    end;

    if Result >= 0 then begin
      HighestVariables.Variables.Items[Result].IsHighestVariable := True;

      // Result := Variables.Count;
      with Variables.AddReferenced^ do begin
        Name := VarName;
        { It is not strict local, but locally bound to the scope }
        Local := False;
        IsHighestVariable := True;
        IsLoopVariable := False;
        ForbidHighUsage := False;
      end;
      Variables.LastItemPointer.Index := Result;
      Result := Variables.Count - 1;

      Exit;
    end;

    Result := HighestVariables.ResolveHighestVariableLinkNew(VarName);
    if Result >= 0 then begin
//      Result := Variables.Count;
      with Variables.AddReferenced^ do begin
        Name := VarName;
        { The same }
        Local := False;
        IsHighestVariable := True;
        IsLoopVariable := False;
        ForbidHighUsage := False;
      end;
      Variables.LastItemPointer^.Index := Result;
      Result := Variables.Count - 1;
      
      Exit;
    end;
  end;

  { Nothing found }
  Result := -1;
end;

function THLVMLinkerVariableScope.TryFindHighestVariable(const Name: String): Integer;
begin
  { This is ~very unoptimised variant without caching. KadJIT was in pre-alpha }
  Result := FindHighestCachedVariable(Name);
  if Result >= 0 then
    Exit;
  { It is not root then }
  if not (HighestVariables = nil) then begin
    Result := HighestVariables.FindLocalVariable(Name);
    if Result >= 0 then begin
      CachedHighestVariables.Add(THLVMLinkerHighestVariable.CreateLocal(Name, Result, CachedHighestVariables.Count));
      HighestVariables.Variables.Items[Result].IsHighestVariable := True;
      Result := CachedHighestVariables.Count - 1;
      Exit;
    end;
    Result := HighestVariables.TryFindHighestVariable(Name);
    if Result >= 0 then begin 
      CachedHighestVariables.Add(THLVMLinkerHighestVariable.CreateHighest(Name, Result, CachedHighestVariables.Count));
      Result := CachedHighestVariables.Count - 1;
      Exit;
    end;
    { Tried, but no local or highest root-variable was found. }
    Result := -1;
  end;
end;

{ THLVMLinkerHighestVariable }

constructor THLVMLinkerHighestVariable.CreateHighest(const Name: String; const Index, CurrentIndex: Integer);
begin
  Self.Name := Name;
  Self.Index := CurrentIndex;
  Self.IsLocal := False;
  Self.PreviousIndex := Index;
end;

constructor THLVMLinkerHighestVariable.CreateLocal(const Name: String; const Index, CurrentIndex: Integer);
begin
  Self.Name := Name;
  Self.Index := CurrentIndex;
  Self.IsLocal := True;
  Self.PreviousIndex := Index;
end;

{ THLVMLinkerVariableObject }

procedure THLVMLinkerVariableObject.Nullify;
begin
  Self.Name := '';
  Self.Local := False;
  Self.Index := -1;
  Self.IsHighestVariable := False;
  Self.InstructionPointer := nil;
end;

end.
