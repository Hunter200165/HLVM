unit HLVM.JIT.Context.Storage.Table;

interface

uses
  uTExtendedX87,
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.Storage.Commons,
  HLVM.Context.Hashing,
  System.SysUtils,
  System.Hash;

const
  { Defines a constant grow threshold }
  HLVM_TableGrowThresoldMultiplier = 4;

type
  { Defines a storage shell for key! }
  THLVMKeyRecord = record
  public var
    Numeric: HLVM_Float;
    Stringy: String;
    { Link is weakened in order to }
    [Weak] Objective: IHLVMUniversal;
  public var
    Typed: Byte;
  public
    function GetInt: HLVM_Integer; inline;
    function GetFloat: HLVM_Float; inline;
    function GetBoolean: Boolean; inline;
    function GetPointer: Pointer; inline;
    function GetForeignPointer: Pointer; inline;

    procedure SetInt(const AContent: HLVM_Integer); inline;
    procedure SetBoolean(const AContent: Boolean); inline;
    procedure SetPointer(const AContent: Pointer); inline;
  public
    function Equals(const [Ref] ARec: HLVM_Variable): Boolean; inline;
  public
    constructor CreateFrom(const [Ref] ARec: HLVM_Variable);
  end;

type
  { Nodes in table to store data. }
  THLVMTableNode = record
  public type
    PHLVMTableNode = ^THLVMTableNode;
  public var
    NextNode: PHLVMTableNode;
    PrevNode: PHLVMTableNode;

    KeyHash: Integer;
    KeyType: Byte;
    Key: THLVMKeyRecord;

    Content: HLVM_Variable;
  public
    class function Create: PHLVMTableNode; static;
    procedure Free;
  end;
  PHLVMTableNode = THLVMTableNode.PHLVMTableNode;

type
  THLVMArrayShell = record
  public type
    PHLVMArrayShell = ^THLVMArrayShell;
  public var
    Content: HLVM_Variable;
  public
    class function Create(const [Ref] AContent: HLVM_Variable): PHLVMArrayShell; static;
    procedure Free;
  end;
  PHLVMArrayShell = THLVMArrayShell.PHLVMArrayShell;

type
  THLVMVariableArray = array of PHLVMArrayShell;
  THLVMNodesArray = array of PHLVMTableNode;

type
  { Main table implementation }
  THLVMTable = record
  public type
    PHLVMTable = ^THLVMTable;
  public var
    RecordCounter: Integer;
    ArraySpace: Integer;
    Threshold: Integer;

    IndexedArray: THLVMVariableArray;
    NodesArray: THLVMNodesArray;

    Metatable: PHLVMTable;
  public
    function GetEntry(const AType: Byte; const [Ref] AKey: HLVM_Variable): PHLVM_Variable;
    function GetArrayEntry(const AIndex: Integer): PHLVM_Variable;

    function InsertEntry(const AType: Byte; const [Ref] AKey: HLVM_Variable): PHLVM_Variable;
    function InsertArrayEntry(const AIndex: Integer): PHLVM_Variable;

    function GetHashPosition(const AHash: Integer): Integer; inline;
    function SearchNodeAt(const AType: Byte; const AHash: Integer; const [Ref] AKey: HLVM_Variable): PHLVMTableNode; inline;

    function IsInArray(const [Ref] AKey: HLVM_Variable): Boolean; inline;
    procedure FitSize(AIndex: Integer); inline;

    procedure RehashSize(const ASize: Integer);

    procedure FreeAllNodes;
    procedure FreeAllIndexes;
    procedure FreeAllContents;

    procedure Nullify;
  end;
  PHLVMTable = THLVMTable.PHLVMTable;

implementation

{ THLVMTable }

procedure THLVMTable.FitSize(AIndex: Integer);
begin
  AIndex := AIndex - 1;
  if (AIndex >= Length(IndexedArray)) then begin
    // Outdated! SetLength(IndexedArray, (Length(IndexedArray) * 3) div 2);
    SetLength(IndexedArray, TCommons.GetGrow(Length(IndexedArray)));
    ArraySpace := Length(IndexedArray);
  end;
end;

procedure THLVMTable.FreeAllContents;
begin
  FreeAllNodes;
  FreeAllIndexes;
end;

procedure THLVMTable.FreeAllIndexes;
var i: Integer;
begin
  for i := 0 to Length(IndexedArray) - 1 do begin
    if (IndexedArray[i] = nil) then
      Continue;
    try
      IndexedArray[i].Free;
      IndexedArray[i] := nil;
    except on E: Exception do
    end;
  end;
end;

procedure THLVMTable.FreeAllNodes;
var i: Integer;
    Cur, CurNext: PHLVMTableNode;
begin
  for i := 0 to Length(NodesArray) - 1 do begin
    Cur := NodesArray[i];
    while not (Cur = nil) do begin
      CurNext := Cur.NextNode;
      try
        Cur.Free;
      except on E: Exception do end;
      Cur := CurNext;
    end;
    NodesArray[i] := nil;
  end;
end;

function THLVMTable.GetArrayEntry(const AIndex: Integer): PHLVM_Variable;
begin
  if (AIndex > 0) and (AIndex <= Length(IndexedArray)) then
    Result := @IndexedArray[AIndex - 1].Content
  else
    Result := nil;
end;

function THLVMTable.GetEntry(const AType: Byte; const [Ref] AKey: HLVM_Variable): PHLVM_Variable;
var Node: PHLVMTableNode;
begin
  Result := nil;
  if AType = HLVM_Type_Integer then
    Result := GetArrayEntry(AKey.GetInt);
  if Result = nil then begin
    Node := SearchNodeAt(AType, AKey.GetHash, AKey);
    if not (Node = nil) then
      Result := @Node.Content;
  end;
end;

function THLVMTable.GetHashPosition(const AHash: Integer): Integer;
begin
  Result := Cardinal(AHash) mod Cardinal(Length(NodesArray));
end;

function THLVMTable.InsertArrayEntry(const AIndex: Integer): PHLVM_Variable;
begin
  FitSize(AIndex);
  IndexedArray[AIndex] := THLVMArrayShell.Create(HLVM_NIL);
  Result := @IndexedArray[AIndex].Content;
end;

function THLVMTable.InsertEntry(const AType: Byte; const [Ref] AKey: HLVM_Variable): PHLVM_Variable;
var Hash, Pos: Integer;
    Rec: PHLVMTableNode;
begin
  Hash := AKey.GetHash;
  if (RecordCounter >= Threshold) then
    { Constant multiplier - x1.5 }
    { Outdated size computing! RehashSize((Threshold * 3) shr 1); }
    RehashSize(TCommons.GetGrow(Threshold));
  Pos := Cardinal(Hash) mod Cardinal(Length(NodesArray));
  Rec := NodesArray[Pos];
  if Rec = nil then begin
    NodesArray[Pos] := THLVMTableNode.Create;
    with NodesArray[Pos]^ do begin
      NextNode := nil;
      PrevNode := nil;
      KeyHash := Hash;
      KeyType := AType;
      Key := THLVMKeyRecord.CreateFrom(AKey);
      Content := HLVM_NIL;
      Result := @Content;
    end;
    RecordCounter := RecordCounter + 1;
    Exit;
  end;
  { Main foundation loop! If the same record is found, it will be returned, else the new one is going to be created. }
  repeat
    if (Rec.KeyType = AType) and (Rec.KeyHash = Hash) and (Rec.Key.Equals(AKey)) then begin
      { Found duplication! }
      Rec.Content := HLVM_NIL;
      Result := @Rec.Content;
      Exit;
    end;
    if not (Rec.NextNode = nil) then
      Rec := Rec.NextNode;
  until (Rec.NextNode = nil);
  { Did not find any occurences }
  Rec.NextNode := THLVMTableNode.Create;
  with Rec.NextNode^ do begin
    NextNode := nil;
    PrevNode := Rec;
    KeyHash := Hash;
    KeyType := AType;
    Key := THLVMKeyRecord.CreateFrom(AKey);
    Content := HLVM_NIL;
    Result := @Content;
  end;
  RecordCounter := RecordCounter + 1;
end;

function THLVMTable.IsInArray(const [Ref] AKey: HLVM_Variable): Boolean;
begin
  Result := (AKey.Typed = HLVM_Type_Integer) and (TCommons.WithinRange(AKey.GetInt, 1, ArraySpace));
end;

procedure THLVMTable.Nullify;
begin
  SetLength(IndexedArray, 1);
  SetLength(NodesArray, 1);
  RecordCounter := 0;
  IndexedArray[0] := nil;
  NodesArray[0] := nil;
  ArraySpace := 1;
  Threshold := HLVM_TableGrowThresoldMultiplier;
  { No metatable by default }
  Metatable := nil;
end;

procedure THLVMTable.RehashSize(const ASize: Integer);
var ANewArray: THLVMNodesArray;
    i, NewPos: Integer;
    Rec, RecAnother, RecSave: PHLVMTableNode;
begin
  SetLength(ANewArray, ASize);
  Threshold := ASize * HLVM_TableGrowThresoldMultiplier;
  for i := 0 to Length(NodesArray) - 1 do begin
    Rec := NodesArray[i];
    while not (Rec = nil) do begin
      RecSave := Rec.NextNode;
      // NewPos := Rec.KeyHash mod ASize;
      NewPos := TCommons.GetHashedPosition(Rec.KeyHash, ASize);
      RecAnother := ANewArray[NewPos];
      if RecAnother = nil then begin
        ANewArray[NewPos] := Rec;
        Rec.PrevNode := nil;
        Rec.NextNode := nil;
      end
      else begin
        while not (RecAnother.NextNode = nil) do
          RecAnother := RecAnother.NextNode;
        RecAnother.NextNode := Rec;
        Rec.NextNode := nil;
        Rec.PrevNode := RecAnother;
      end;
      Rec := RecSave;
    end;
  end;
  NodesArray := ANewArray;
end;

function THLVMTable.SearchNodeAt(const AType: Byte; const AHash: Integer; const [Ref] AKey: HLVM_Variable): PHLVMTableNode;
begin
  Result := NodesArray[GetHashPosition(AHash)];
  while not (Result = nil) do begin
    if (Result.KeyType = AType) and (Result.KeyHash = AHash) and (Result.Key.Equals(AKey)) then
      Break;
    Result := Result.NextNode;
  end;
end;

{ THLVMTableNode }

class function THLVMTableNode.Create: PHLVMTableNode;
begin
  New(Result);
end;

procedure THLVMTableNode.Free;
begin
  Dispose(@Self);
end;

{ THLVMArrayShell }

class function THLVMArrayShell.Create(const [Ref] AContent: HLVM_Variable): PHLVMArrayShell;
begin
  New(Result);
  Result^.Content := AContent;
end;

procedure THLVMArrayShell.Free;
begin
  Dispose(@Self);
end;

{ THLVMKeyRecord }

constructor THLVMKeyRecord.CreateFrom(const [Ref] ARec: HLVM_Variable);
begin
  Self.Typed := ARec.Typed;
  case Typed of
    HLVM_Type_Integer: SetInt(ARec.GetInt);
    HLVM_Type_Float: Numeric := ARec.Numeric;
    HLVM_Type_String: Stringy := ARec.Stringy;
    HLVM_Type_Boolean: SetBoolean(ARec.GetBoolean);
    HLVM_Type_BuiltInFunction: SetPointer(ARec.GetPointer);
  else
    Objective := ARec.Objective;
  end;
end;

function THLVMKeyRecord.Equals(const [Ref] ARec: HLVM_Variable): Boolean;
begin
  Result := Typed = ARec.Typed;
  if Result then
    case Typed of
      HLVM_Type_Nil: Result := True;
      HLVM_Type_Integer: Result := GetInt = ARec.GetInt;
      HLVM_Type_Float: Result := GetFloat = ARec.GetFloat;
      HLVM_Type_String: Result := Stringy = ARec.Stringy;
      HLVM_Type_Boolean: Result := GetBoolean = ARec.GetBoolean;
    end;
end;

function THLVMKeyRecord.GetBoolean: Boolean;
var A: HLVM_Integer absolute Numeric;
begin
  Result := not (A = 0);
end;

function THLVMKeyRecord.GetFloat: HLVM_Float;
begin
  Result := Numeric;
end;

function THLVMKeyRecord.GetForeignPointer: Pointer;
begin
  Result := Pointer(Objective);
end;

function THLVMKeyRecord.GetInt: HLVM_Integer;
var A: HLVM_Integer absolute Numeric;
begin
  Result := A;
end;

function THLVMKeyRecord.GetPointer: Pointer;
var A: Pointer absolute Numeric;
begin
  Result := A;
end;

procedure THLVMKeyRecord.SetBoolean(const AContent: Boolean);
var A: HLVM_Integer absolute Numeric;
begin
  A := Byte(AContent);
end;

procedure THLVMKeyRecord.SetInt(const AContent: HLVM_Integer);
var A: HLVM_Integer absolute Numeric;
begin
  A := AContent;
end;

procedure THLVMKeyRecord.SetPointer(const AContent: Pointer);
var A: Pointer absolute Numeric;
begin
  A := AContent;
end;

end.
