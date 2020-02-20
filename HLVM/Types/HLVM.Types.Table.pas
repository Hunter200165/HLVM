unit HLVM.Types.Table;

interface

uses
  HLVM.Runtime.Types,
  HLVM.Runtime.Hashes,
  System.SysUtils,
  System.Hash;

type
  TTablePair = record
  public var
    Key: TVariableShell;
    Value: TVariableShell;
    Hash: Integer;
  end;
  TTablePairs = array of TTablePair;

type
  TFunctionalTable = record
  public var
    Items: TTablePairs;
    Count: Integer;
    Capacity: Integer;
  public
    procedure Grow;
  public
    function BinarySearchPosition(const AHashCode: Integer): Integer; inline;
    function BinarySearchByType(const AType: HLVM_Types; const HashCode: Integer; out Index: Integer): Boolean; inline;

    function GetItem(const [Ref] Key: TVariableShell): PVariableShell; inline;
    function AddOrSetItem(const [Ref] AKey: TVariableShell; const [Ref] AValue: TVariableShell): Integer; inline;

    function InternalInsertHashCode(const HashCode: Integer): Integer; overload; inline;
    function InternalInsertHashCode(const HashCode, Position: Integer): Integer; overload; inline;
  public
    constructor Create(const InitialCount: Integer);
  end;

implementation

{ TFunctionalTable }

function TFunctionalTable.AddOrSetItem(const [Ref] AKey, AValue: TVariableShell): Integer;
var Pos, HashCode: Integer;
begin
  HashCode := AKey.GetHashCode;
  if not BinarySearchByType(AKey.TypeOf, HashCode, Pos) then begin
    Pos := InternalInsertHashCode(HashCode, Pos);
    with Items[Pos] do begin
      Hash := HashCode;
      Key := AKey;
      Value := AValue;
    end;
  end
  else begin
    Items[Pos].Value := AValue;
  end;
  Result := Pos;
end;

function TFunctionalTable.BinarySearchByType(const AType: HLVM_Types; const HashCode: Integer; out Index: Integer): Boolean;
var Pos, PosA: Integer;
begin
  Pos := BinarySearchPosition(HashCode);
  Index := Pos;
  if (Pos < 0) or not (Items[Pos].Hash = HashCode) then begin
    Result := False;
    Exit;
  end;
  PosA := Pos;
  while (PosA > 0) and (Items[PosA].Hash = HashCode) do begin
    if Items[PosA].Value.TypeOf = AType then begin
      Result := True;
      Index := PosA;
      Exit;
    end;
    Dec(PosA);
  end;
  PosA := Pos + 1;
  while (PosA < Count) and (Items[PosA].Hash = HashCode) do begin
    if Items[PosA].Value.TypeOf = AType then begin
      Result := True;
      Index := PosA;
      Exit;
    end;
    Inc(PosA);
  end;
  Result := False;
end;

function TFunctionalTable.BinarySearchPosition(const AHashCode: Integer): Integer;
var Position, Left, Right, A: Integer;
begin
  { Binary search algorithm }
  Left := 0;
  Right := Count - 1;
  if Right < 0 then
    Exit(-1);
  Position := (Left + Right) shr 1;
  while True do begin
//    if Left = Right then begin
      { Position is up. Ending this endless loop }
//      Result := Left;
//      Break;
//    end;
    if (Position = Left) or (Position = Right) then begin
//      A := Items[Position].Hash;
      Result := Left;
      if AHashCode > Items[Left].Hash then
        Result := Right;
      Break;
    end;
    with Items[Position] do begin
      if (Hash > AHashCode) then begin
        Right := Position;
        Position := (Left + Right) shr 1;
      end
      else if (Hash < AHashCode) then begin
        Left := Position;
        A := Left + Right;
        { UpDiv! }
        Position := A shr 1 + A and 1;
      end
      else begin
        { Found hashcode position }
        Result := Position;
        Break;
      end;
    end;
  end;
end;

constructor TFunctionalTable.Create(const InitialCount: Integer);
begin
  Count := InitialCount;
  Capacity := 0;
end;

function TFunctionalTable.GetItem(const [Ref] Key: TVariableShell): PVariableShell;
var Pos, HashCode: Integer;
begin
  HashCode := Key.GetHashCode;
  if not BinarySearchByType(Key.TypeOf, HashCode, Pos) then
    Result := nil
  else
    Result := @Items[Pos].Value;
end;

procedure TFunctionalTable.Grow;
begin
  if Capacity < 4 then
    Inc(Capacity, 4)
  else if Capacity < 16 then
    Inc(Capacity, 8)
  else
    { Increase in ~1.5 times }
    Inc(Capacity, (Capacity * 3) shr 1);
  SetLength(Items, Capacity);
end;

function TFunctionalTable.InternalInsertHashCode(const HashCode, Position: Integer): Integer;
begin
  Result := Position;
  if (Result < 0) or (Items[Position].Hash < HashCode) then
    Result := Result + 1;
  Inc(Count);
  if (Count >= Capacity) then
    Grow;
  if (Result < Count - 1) then
    { If we have to move. }
    Move(Items[Result], Items[Result + 1], SizeOf(TTablePair) * (Count - 1 - Result));
end;

function TFunctionalTable.InternalInsertHashCode(const HashCode: Integer): Integer;
var Pos: Integer;
begin
  Pos := BinarySearchPosition(HashCode);
  if (Pos < 0) or (Items[Pos].Hash < HashCode) then Pos := Pos + 1;
  Inc(Count);
  if Count >= Capacity then Grow;
  if (Pos < Count - 1) then
    { If we have to move. }
    Move(Items[Pos], Items[Pos + 1], SizeOf(TTablePair) * (Count - 1 - Pos));
  Result := Pos;
end;

end.
