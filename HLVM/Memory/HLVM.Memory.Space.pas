unit HLVM.Memory.Space;

interface

const HLVM_PageSize = 32;

{$Define HLVM_UseDefinedPageSize}

{$IfDef HLVM_UseDefinedPageSize}
const HLVM_UseFixedPageSize = True;
{$Else}
const HLVM_UseFixedPageSize = False;
{$EndIf}

type
  TMemoryArray<T> = record
  public type
    TInternalArray = array of T;
    PType = ^T;
  public var
    Items: TInternalArray;
  public var
    Size: Integer;
    Count: Integer;
    function LastPos: Integer; inline;
  public
    procedure SetSize(const ANewSize: Integer); inline;
    procedure Add(const AItem: T); inline;
    procedure Delete(const AAmount: Integer = 1); inline;
    procedure ChangeCount(const ANewCount: Integer); inline;
    procedure Grow; inline;
    procedure GrowUntil(const ANewSize: Integer); inline;

    function Pick: T; inline;
    function GetAsPointer(const AIndex: Integer): PType; inline;
  public
    SP: PType;
    HighSP: PType;
    LowSP: PType;
  public
    constructor Create(const AInitialSize: Integer);
  end;

{$If SizeOf(Pointer) = 4}
type TPointer = Int32;
{$ElseIf SizeOf(Pointer) = 8}
type TPointer = Int64;
{$EndIf}

type 
  TSegmentationSpace = TMemoryArray<TPointer>;
  
type
  TMemoryStorage<T> = record 
  public type 
    TInternalSpace = TMemoryArray<T>;
  public var 
    MemorySpace: TInternalSpace;
    SegmentationSpace: TSegmentationSpace;
  public 
    procedure AllocateSpaceAndSegment; //inline;
    function Add(const ANew: T): Integer; //inline;
  public
    class function Create: TMemoryStorage<T>; static;
  end;

implementation

{ TMemoryArray<T> }

procedure TMemoryArray<T>.Add(const AItem: T);
begin
  ChangeCount(Count + 1);
  if Count > Size then Grow;
  Items[LastPos] := AItem;
end;

procedure TMemoryArray<T>.ChangeCount(const ANewCount: Integer);
begin
  Count := ANewCount;
//  if ANewCount > 0 then
//    HighSP := @Items[ANewCount - 1];
//  LastPos := Count - 1;
end;

constructor TMemoryArray<T>.Create(const AInitialSize: Integer);
begin
  SetSize(AInitialSize);
  ChangeCount(0);
end;

procedure TMemoryArray<T>.Delete(const AAmount: Integer);
begin
  ChangeCount(Count - AAmount);
end;

function TMemoryArray<T>.GetAsPointer(const AIndex: Integer): PType;
begin
  Result := @Items[AIndex];
end;

procedure TMemoryArray<T>.Grow;
begin
//  if HLVM_UseFixedPageSize then
  {$IfDef HLVM_UseDefinedPageSize}
    Size := Size + HLVM_PageSize;
  {$Else}
//  else
    if Size = 0 then
      Size := 1;
    Size := (Size * 3) div 2;
  {$EndIf}
  SetLength(Items, Size);
end;

procedure TMemoryArray<T>.GrowUntil(const ANewSize: Integer);
begin
  while (Size <= ANewSize) do begin 
    {$IfDef HLVM_UseDefinedPageSize}
      Size := Size + HLVM_PageSize;
    {$Else}
      if (Size = 0) then Size := 1;
      Size := (Size * 3) div 2;
    {$EndIf}
  end;
  SetLength(Items, Size);
end;

function TMemoryArray<T>.LastPos: Integer;
begin
  Result := Count - 1;
end;

function TMemoryArray<T>.Pick: T;
begin
  Result := Items[LastPos];
  ChangeCount(Count - 1);
end;

procedure TMemoryArray<T>.SetSize(const ANewSize: Integer);
begin
  Size := ANewSize;
  SetLength(Items, Size);
//  if ANewSize > 0 then
//    LowSP := @Items[0]
//  else
//    LowSP := nil;
  if Count > Size then
    ChangeCount(Size);
//  if ANewSize > 0 then
//    HighSP := @Items[ANewSize - 1];
end;

{ TMemoryStorage<T> }

function TMemoryStorage<T>.Add(const ANew: T): Integer;
begin
  if (SegmentationSpace.Count <= 0) then AllocateSpaceAndSegment;
  Result := SegmentationSpace.Pick;
  MemorySpace.Items[Result] := ANew;
end;

procedure TMemoryStorage<T>.AllocateSpaceAndSegment;
var
  i: Integer;
{$IfNDef HLVM_UseDefinedPageSize}
  PrevSpace: Integer;
{$EndIf}
begin
  {$IfDef HLVM_UseDefinedPageSize}
    MemorySpace.Grow;
    if (SegmentationSpace.Size - SegmentationSpace.Count) < HLVM_PageSize then SegmentationSpace.Grow;
    for i := MemorySpace.Size - 1 downto MemorySpace.Size - HLVM_PageSize do begin
      SegmentationSpace.Add(i);
    end;
  {$Else}
    PrevSpace := MemorySpace.Size;
    MemorySpace.Grow;
    PrevSpace := MemorySpace.Size - PrevSpace;
    if (SegmentationSpace.Size - SegmentationSpace.Count) < PrevSpace then SegmentationSpace.GrowUntil(SegmentationSpace.Count + PrevSpace + 1);
    for i := MemorySpace.Size - 1 downto MemorySpace.Size - PrevSpace do
      SegmentationSpace.Add(i);
  {$EndIf}
end;

class function TMemoryStorage<T>.Create: TMemoryStorage<T>;
begin
  with Result do begin
    MemorySpace := MemorySpace.Create(0);
    SegmentationSpace := SegmentationSpace.Create(0);
  end;
end;

end.
