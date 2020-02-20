unit HLVM.Generics.HArray;

interface

{ Generic record }

type
  HArray<T> = record
  public 
    { We need to define it here }
    type HInitialArray = array of T;
  private
    FItems: HInitialArray;
    function GetReadIndexed(const AIndex: Integer): T;
    procedure SetWriteIndexed(const AIndex: Integer; const Value: T);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
  public
    property Items: HInitialArray read FItems write FItems;
    property ItemsIndexer[const AIndex: Integer]: T read GetReadIndexed write SetWriteIndexed; Default;

    property Size: Integer read GetSize write SetSize;

    procedure Add(const AItem: T);
    procedure ResizeNullify(const NewSize: Integer);
  end;

implementation

{ HArray<T> }

procedure HArray<T>.Add(const AItem: T);
var Len: Integer;
begin
  Len := Size;
  Size := Size + 1;
  Self[Len] := AItem; 
end;

function HArray<T>.GetReadIndexed(const AIndex: Integer): T;
begin
  Result := FItems[AIndex];
end;

function HArray<T>.GetSize: Integer;
begin
  Result := Length(FItems);
end;

procedure HArray<T>.ResizeNullify(const NewSize: Integer);
begin
  SetLength(FItems, 0);
  SetLength(FItems, NewSize);
end;

procedure HArray<T>.SetSize(const Value: Integer);
begin
  SetLength(FItems, Value);
end;

procedure HArray<T>.SetWriteIndexed(const AIndex: Integer; const Value: T);
begin
  FItems[AIndex] := Value;
end;

end.
