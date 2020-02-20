unit HLVM.Generics.HList;

interface

uses
  System.SysUtils;

type
  HList<T> = record
  public const 
    AllowFastMove = True;
  public type
    TInternalArray = array of T;
    TInternalPointer = ^T;
  public var
    Items: TInternalArray;
  private var
    FCount: Integer;
    FSize: Integer;

    procedure SetSize(const NewSize: Integer);
    procedure Grow;
    function GetItem(const AIndex: Integer): T; inline;
    procedure SetItem(const AIndex: Integer; const Value: T); inline;
  public
    property Count: Integer read FCount;
    property Size: Integer read FSize write SetSize;

    function Add(const [Ref] Item: T): Integer;
    function AddReferenced: TInternalPointer;
    function LastIndex: Integer; inline;
    function LastItem: T; inline;
    function LastItemPointer: TInternalPointer; inline;

    procedure Insert(const [Ref] Item: T; const Position: Integer);
    procedure InsertRange(const AnItems: TInternalArray; const Position: Integer; Count: Integer = -1);

    procedure Reverse;
    procedure ResizeNullify(const ASize: Integer);

    procedure SetCount(const ANewCount: Integer);

    procedure Clear;
    procedure Remove;

    procedure Nullify;
  public
    property Indexed[const AIndex: Integer]: T read GetItem write SetItem; default;
  end;

implementation

{ HList<T> }

function HList<T>.Add(const [Ref] Item: T): Integer;
begin
  Result := FCount;
  FCount := FCount + 1;
  if FCount > FSize then
    Grow;
  Items[Result] := Item;
end;

function HList<T>.AddReferenced: TInternalPointer;
var Index: Integer;
begin
  Index := FCount;
  FCount := FCount + 1;
  if FCount > FSize then
    Grow;
  Result := @Items[Index];
end;

procedure HList<T>.Clear;
begin
  FCount := 0;
end;

function HList<T>.GetItem(const AIndex: Integer): T;
begin
  Result := Items[AIndex];
end;

procedure HList<T>.Grow;
begin
  if FSize < 4 then
    SetSize(FSize + 8)
  else if FSize < 8 then
    SetSize(FSize + 4)
  else
    SetSize((FSize * 3) shr 1);
end;

procedure HList<T>.Insert(const [Ref] Item: T; const Position: Integer);
var i: Integer;
begin
  FCount := FCount + 1;
  if FCount > FSize then
    Grow;
  { Move is under crossing memory, I will not use it }
  for i := FCount downto Position + 1 do 
    Items[i] := Items[i - 1];
  Items[Position] := Item;
end;

procedure HList<T>.InsertRange(const AnItems: TInternalArray; const Position: Integer; Count: Integer);
var i: Integer;
begin
  { Count is -1 by default. Should use that. }
  if Count < 0 then Count := Length(AnItems);
  if Count = 0 then Exit;
  
  FCount := FCount + Count;
  while FCount > FSize do 
    Grow;
  
  for i := FCount to Position + Count do
    Items[i] := Items[i - Count];
  for i := 0 to Count do
    Items[Position + i] := AnItems[i];
end;

function HList<T>.LastIndex: Integer;
begin
  Result := FCount - 1;
end;

function HList<T>.LastItem: T;
begin
  Result := Items[LastIndex];
end;

function HList<T>.LastItemPointer: TInternalPointer;
begin
  Result := @Items[FCount - 1];
end;

procedure HList<T>.Nullify;
begin
  FSize := 0;
  FCount := 0;
  SetLength(Items, 0);
end;

procedure HList<T>.Remove;
begin
  if FCount <= 0 then
    raise EArgumentOutOfRangeException.Create('Count of elements is not positive!');
  FCount := FCount - 1;
end;

procedure HList<T>.ResizeNullify(const ASize: Integer);
begin
  FCount := 0;
  SetLength(Items, 0);
  SetLength(Items, ASize);
  FSize := ASize;
  FCount := 0;
end;

procedure HList<T>.Reverse;
var Count, i: Integer;
    HIndex, LIndex: Integer;
    Temp: T;
begin
  Count := FCount div 2;
  HIndex := FCount - 1;
  LIndex := 0;
  for i := 1 to Count do begin
    Temp := Items[LIndex];
    Items[LIndex] := Items[HIndex];
    Items[HIndex] := Temp;
    LIndex := LIndex + 1;
    HIndex := HIndex - 1;
  end;
end;

procedure HList<T>.SetCount(const ANewCount: Integer);
begin
  if (ANewCount < 0) or (ANewCount > FSize) then
    raise EArgumentException.Create('Invalid new size was provided to set.');
  FCount := ANewCount;
end;

procedure HList<T>.SetItem(const AIndex: Integer; const Value: T);
begin
  Items[AIndex] := Value;
  if AIndex >= FCount then
    FCount := AIndex + 1;
end;

procedure HList<T>.SetSize(const NewSize: Integer);
begin
  SetLength(Items, NewSize);
  FSize := NewSize;
  if FSize < Count then
    FCount := FSize
  else if Count < 0 then
    FCount := 0;
end;

end.
