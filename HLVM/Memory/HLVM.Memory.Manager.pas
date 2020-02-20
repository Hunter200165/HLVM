unit HLVM.Memory.Manager;

interface

uses
  HLVM.Memory.Space,
  HLVM.RunTime.Types,
  HLVM.Types.Ordinal,
  HLVM.Types.Utilities,
  HLVM.Types.Containers,
  System.SysUtils,
  System.Rtti;

type
  TIntegerMemorySpace  = TMemoryStorage<TRunTimeVariable_Integer >;
  TFloatMemorySpace    = TMemoryStorage<TRunTimeVariable_Float   >;
  TStringMemorySpace   = TMemoryStorage<TRunTimeVariable_String  >;
  TTableMemorySpace    = TMemoryStorage<TRunTimeVariable_Table   >;
  TFunctionMemorySpace = TMemoryStorage<TRunTimeVariable_Function>;
  TBooleanMemorySpace  = TMemoryStorage<TRunTimeVariable_Boolean >;
  TBuiltInFunctionMemorySpace = TMemoryStorage<TRunTimeVariable_BuiltInFunction>;
  TBuiltInObjectMemorySpace   = TMemoryStorage<TRunTimeVariable_BuiltInObject  >;
  TBuiltInFieldMemorySpace    = TMemoryStorage<TRunTimeVariable_BuiltInField   >;
  TBuiltInPropertyMemorySpace = TMemoryStorage<TRunTimeVariable_BuiltInProperty>;
  TBuiltInMethod              = TMemoryStorage<TRunTimeVariable_BuiltInMethod  >;

const
  { This is number of ~infinity links, that will not be processed anyway. Useful for service variables. }
  HLVM_MemoryManager_InfinityLinks: Integer = $7FFFFFFF;

type
  TMemoryManager = record
  public var
    IntegerMemorySpace : TIntegerMemorySpace ;
    FloatMemorySpace   : TFloatMemorySpace   ;
    StringMemorySpace  : TStringMemorySpace  ;
    TableMemorySpace   : TTableMemorySpace   ;
    FunctionMemorySpace: TFunctionMemorySpace;
    BooleanMemorySpace : TBooleanMemorySpace ;
    BuiltInFunctionMemorySpace: TBuiltInFunctionMemorySpace;
    BuiltInObjectMemorySpace  : TBuiltInObjectMemorySpace  ;
    BuiltInFieldMemorySpace   : TBuiltInFieldMemorySpace   ;
    BuiltInPropertyMemorySpace: TBuiltInPropertyMemorySpace;
    BuiltInMethodMemorySpace  : TBuiltInMethod             ;
  public
    function AllocateNewInteger(const AContent: HSInteger; const InitialReferenceCount: Integer = 1): TVariableShell; inline;
    function AllocateNewFloat(const AContent: HSFloat; const InitialReferenceCount: Integer = 1): TVariableShell; inline;
    function AllocateNewString(const AContent: String; const InitialReferenceCount: Integer = 1): TVariableShell; //inline;
    function AllocateNewBoolean(const AContent: Boolean; const InitialReferenceCount: Integer = 1): TVariableShell; inline;
    function AllocateNewTable(const InitialReferenceCounter: Integer = 1): TVariableShell; inline;
    function AllocateNewFunction(const InitialContent: THLVMBytecodeChunk; const Environment: PNewEnvironment; const InitialReferenceCount: Integer = 1): TVariableShell; inline;
    function AllocateNewBuiltInFunction(const AContent: TBuiltInFunction; const InitialReferenceCount: Integer = 1): TVariableShell; inline;
    function AllocateNewBuiltInField(const AContent: String; const AParent: PHBuiltInObject; const ARefCounter: Integer = 1): TVariableShell; inline;
    function AllocateNewBuiltInProperty(const AContent: String; const AParent: PHBuiltInObject; const ARefCounter: Integer = 1): TVariableShell; inline;
    function AllocateNewBuiltInMethod(const AContent: String; const AParent: PHBuiltInObject; const ARefCounter: Integer = 1): TVariableShell; inline;
    function AllocateNewBuiltInObject(const AContent: TObject; const LMemManager: Pointer; const AttachedToMachine: Boolean; const ARefCounter: Integer): TVariableShell; inline;

    function AllocateRawInteger(const AContent: HSInteger; const InitialReferenceCount: Integer = 1): PHInteger; inline;
    function AllocateRawFloat(const AContent: HSFloat; const InitialReferenceCount: Integer = 1): PHFloat; inline;
    function AllocateRawString(const AContent: String; const InitialReferenceCount: Integer = 1): PHString; //inline;
    function AllocateRawBoolean(const AContent: Boolean; const InitialReferenceCount: Integer = 1): PHBoolean; inline;
    function AllocateRawFunction(const InitialReferenceCount: Integer = 1): PHFunction; inline;
    function AllocateRawTable(const InitialReferenceCount: Integer = 1): PHTable; inline;
    function AllocateRawBuiltInField(const AField: String; const AParent: PRunTimeVariable_BuiltInObject; const ARefCounter: Integer = 1): PHBuiltInField; inline;
    function AllocateRawBuiltInProperty(const AProp: String; const AParent: PRunTimeVariable_BuiltInObject; const ARefCounter: Integer = 1): PHBuiltInProperty; inline;
    function AllocateRawBuiltInMethod(const AMethod: String; const AParent: PRunTimeVariable_BuiltInObject; const ARefCounter: Integer = 1): PHBuiltInMethod; inline;
    function AllocateRawBuiltInObject(const AObject: TObject; const LMemManager: Pointer; const AttachToMachine: Boolean; const ARefCounter: Integer = 1): PHBuiltInObject; inline;
  public
    procedure DisposeInteger        (const Index: Integer); inline;
    procedure DisposeFloat          (const Index: Integer); inline;
    procedure DisposeString         (const Index: Integer); // inline;
    procedure DisposeTable          (const Index: Integer); //inline;
    procedure DisposeFunction       (const Index: Integer); inline;
    procedure DisposeBoolean        (const Disp: PHBoolean); inline;
    procedure DisposeBuiltInFunction(const Index: Integer); inline;
    procedure DisposeBuiltInObject  (const Index: Integer); inline;
    procedure DisposeBuiltInField   (const Index: Integer); inline;
    procedure DisposeBuiltInProperty(const Index: Integer); inline;
    procedure DisposeBuiltInMethod  (const Index: Integer); inline;
  public
    class function Create: TMemoryManager; static;
  end;
  PMemoryManager = ^TMemoryManager;

type
  TAssigner = record
  public var
    MemoryManager: PMemoryManager;
  public
    procedure ChangeObjectiveVariableReferenceCounter(const AObject: PHBuiltInObject; const AValue: Integer); 
    procedure DecrementVariableReferenceCounter(var Shell: TVariableShell); //inline;
    procedure IncrementVariableReferenceCounter(var Shell: TVariableShell); //inline;
    procedure DestroyShell(var Shell: TVariableShell); //inline;
  public
    procedure AssignShells(var AFirstShell, ASecondShell: TVariableShell); //inline;
    procedure AssignValues(var AFirstShell, ASecondShell: TVariableShell); overload; //inline;
    procedure AssignValues(var AFirstShell, ASecondShell: PVariableShell); overload;
    procedure AssignPointed(var AFirstShell, ASecondShell: TVariableShell);// inline;
    procedure AssignLinked(const AFrom: PVariableShell; var ATo: TVariableShell);

    procedure AssignNewInteger(var ToShell: TVariableShell; const InitialContent: HSInteger = 0; const RefCounter: Integer = 1);
    procedure AssignNewFloat(var ToShell: TVariableShell; const InitialContent: HSFloat = 0; const RefCounter: Integer = 1);
    procedure AssignNewString(var ToShell: TVariableShell; const InitialContent: String = ''; const RefCounter: Integer = 1);
  public
    function CreateLinked(var Shell: TVariableShell): TVariableShell;
  end;
  PAssigner = ^TAssigner;

implementation

const TestAlloc = not True;

{ TMemoryManager }

function TMemoryManager.AllocateNewBoolean(const AContent: Boolean; const InitialReferenceCount: Integer): TVariableShell;
var Id: Integer;
    Rec: PHBoolean;
begin
  if not TestAlloc then begin
    Id := BooleanMemorySpace.Add(TRunTimeVariable_Boolean.Create(AContent));
    Rec := PHBoolean(BooleanMemorySpace.MemorySpace.GetAsPointer(Id));
    Rec.ID := Id;
    Rec.RefCounter := InitialReferenceCount;
    Result.TypeOf := Type_Boolean;
    Result.Content := Rec;
    Result.Links := 0;
    Result.Referenced := nil;
  end
  else begin
    New(Rec);
    with Rec^ do begin
      ID := -1;
      RefCounter := InitialReferenceCount;
      Content := AContent;
    end;
    with Result do begin
      TypeOf := Type_Boolean;
      Content := Rec;
      Links := 0;
      Result.Referenced := nil;
    end;
  end;
end;

function TMemoryManager.AllocateNewBuiltInField(const AContent: String; const AParent: PHBuiltInObject; const ARefCounter: Integer): TVariableShell;
begin

end;

function TMemoryManager.AllocateNewBuiltInFunction(const AContent: TBuiltInFunction; const InitialReferenceCount: Integer): TVariableShell;
var Id: Integer;
    Rec: PRunTimeVariable_BuiltInFunction;
begin
  Id := BuiltInFunctionMemorySpace.Add(TRunTimeVariable_BuiltInFunction.Create(AContent));
  Rec := PHBuiltInFunction(BuiltInFunctionMemorySpace.MemorySpace.GetAsPointer(Id));
  Rec.ID := Id;
  Rec.RefCounter := InitialReferenceCount;
  Result.TypeOf := Type_BuiltInFunction;
  Result.Content := Rec;
  Result.Links := 0;
  Result.Referenced := nil;
end;

function TMemoryManager.AllocateNewBuiltInMethod(const AContent: String; const AParent: PHBuiltInObject; const ARefCounter: Integer): TVariableShell;
begin
  with Result do begin
    TypeOf := Type_BuiltInMethod;
    Content := AllocateRawBuiltInMethod(AContent, AParent, ARefCounter);
    Links := 0;
    Referenced := nil;
  end;
end;

function TMemoryManager.AllocateNewBuiltInObject(const AContent: TObject; const LMemManager: Pointer; const AttachedToMachine: Boolean; const ARefCounter: Integer): TVariableShell;
begin
  with Result do begin
    TypeOf := Type_BuiltInObject;
    Content := AllocateRawBuiltInObject(AContent, LMemManager, AttachedToMachine, ARefCounter);
    Links := 0;
    Referenced := nil;
  end;
end;

function TMemoryManager.AllocateNewBuiltInProperty(const AContent: String; const AParent: PHBuiltInObject; const ARefCounter: Integer): TVariableShell;
begin
  with Result do begin
    TypeOf := Type_BuiltInProperty;
    Content := AllocateRawBuiltInProperty(AContent, AParent, ARefCounter);
    Links := 0;
    Referenced := nil;
  end;
end;

function TMemoryManager.AllocateNewFloat(const AContent: HSFloat; const InitialReferenceCount: Integer): TVariableShell;
var Id: Integer;
    Rec: PRunTimeVariable_Float;
begin
  Id := FloatMemorySpace.Add(TRunTimeVariable_Float.Create(AContent));
  Rec := PRunTimeVariable_Float(FloatMemorySpace.MemorySpace.GetAsPointer(Id));
  Rec.ID := Id;
  Rec.RefCounter := InitialReferenceCount;
  Result.TypeOf := Type_Float;
  Result.Content := Rec;
  Result.Links := 0;
  Result.Referenced := nil;
end;

function TMemoryManager.AllocateNewFunction(const InitialContent: THLVMBytecodeChunk; const Environment: PNewEnvironment; const InitialReferenceCount: Integer): TVariableShell;
var Id: Integer;
    Rec: PHFunction;
begin
  Id := FunctionMemorySpace.Add(TRunTimeVariable_Function.Create(InitialContent, Environment));
  Rec := PHFunction(FunctionMemorySpace.MemorySpace.GetAsPointer(Id));
  Rec.ID := Id;
  Rec.RefCounter := InitialReferenceCount;
  Result.TypeOf := Type_Function;
  Result.Content := Rec;
  Result.Links := 0;
  Result.Referenced := nil;
end;

function TMemoryManager.AllocateNewInteger(const AContent: HSInteger; const InitialReferenceCount: Integer): TVariableShell;
var Id: Integer;
    Rec: PRunTimeVariable_Integer;
begin
  Id := IntegerMemorySpace.Add(TRunTimeVariable_Integer.Create(AContent));
  Rec := PRunTimeVariable_Integer(IntegerMemorySpace.MemorySpace.GetAsPointer(Id));
  Rec.ID := Id;
  Rec.RefCounter := InitialReferenceCount;
  Result.TypeOf := Type_Integer;
  Result.Content := Rec;
  Result.Links := 0;
  Result.Referenced := nil;
end;

function TMemoryManager.AllocateNewString(const AContent: String; const InitialReferenceCount: Integer): TVariableShell;
var Id: Integer;
    Rec: PRunTimeVariable_String;
begin
  Id := StringMemorySpace.Add(TRunTimeVariable_String.Create(AContent));
  Rec := PRunTimeVariable_String(StringMemorySpace.MemorySpace.GetAsPointer(Id));
  Rec.ID := Id;
  Rec.RefCounter := InitialReferenceCount;
  Result.TypeOf := Type_String;
  Result.Content := Rec;
  Result.Links := 0;
  Result.Referenced := nil;
end;

function TMemoryManager.AllocateNewTable(const InitialReferenceCounter: Integer): TVariableShell;
var Id: Integer;
    Rec: PHTable;
begin
  Id := TableMemorySpace.Add(TRunTimeVariable_Table.Create(nil));
  Rec := PHTable(TableMemorySpace.MemorySpace.GetAsPointer(Id));
  Rec.Id := Id;
  Rec.RefCounter := InitialReferenceCounter;
  Result.TypeOf := Type_Table;
  Result.Content := Rec;
  Result.Links := 0;
  Result.Referenced := nil;
end;

function TMemoryManager.AllocateRawBoolean(const AContent: Boolean; const InitialReferenceCount: Integer): PHBoolean;
var ID: Integer;
begin
  ID := BooleanMemorySpace.Add(TRunTimeVariable_Boolean.Create(AContent));
  Result := PHBoolean(BooleanMemorySpace.MemorySpace.GetAsPointer(ID));
  Result.RefCounter := InitialReferenceCount;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawBuiltInField(const AField: String; const AParent: PRunTimeVariable_BuiltInObject; const ARefCounter: Integer): PHBuiltInField;
var ID: Integer;
begin
  with BuiltInFieldMemorySpace do begin
    ID := Add(TRunTimeVariable_BuiltInField.Create(AField, AParent));
    Result := PHBuiltInField(MemorySpace.GetAsPointer(ID));
  end;
  Result.RefCounter := ARefCounter;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawBuiltInMethod(const AMethod: string; const AParent: PRunTimeVariable_BuiltInObject; const ARefCounter: Integer): PHBuiltInMethod;
var ID: Integer;
begin
  with BuiltInMethodMemorySpace do begin
    ID := Add(TRunTimeVariable_BuiltInMethod.Create(AMethod, AParent));
    Result := PHBuiltInMethod(MemorySpace.GetAsPointer(ID));
  end;
  Result.RefCounter := ARefCounter;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawBuiltInObject(const AObject: TObject; const LMemManager: Pointer; const AttachToMachine: Boolean; const ARefCounter: Integer): PHBuiltInObject;
var ID: Integer;
begin
  with BuiltInObjectMemorySpace do begin 
    ID := Add(TRunTimeVariable_BuiltInObject.Create(AObject, LMemManager));
    Result := PHBuiltInObject(MemorySpace.GetAsPointer(ID));
  end;
  Result.RefCounter := ARefCounter;
  Result.AttachedToMahine := AttachToMachine;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawBuiltInProperty(const AProp: String; const AParent: PRunTimeVariable_BuiltInObject; const ARefCounter: Integer): PHBuiltInProperty;
var ID: Integer;
begin
  with BuiltInPropertyMemorySpace do begin 
    ID := Add(TRunTimeVariable_BuiltInProperty.Create(AProp, AParent));
    Result := PHBuiltInProperty(MemorySpace.GetAsPointer(ID));
  end;
  Result.RefCounter := ARefCounter;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawFloat(const AContent: HSFloat; const InitialReferenceCount: Integer): PHFloat;
var ID: Integer;
begin
  ID := FloatMemorySpace.Add(TRunTimeVariable_Float.Create(AContent));
  Result := PHFloat(FloatMemorySpace.MemorySpace.GetAsPointer(ID));
  Result.RefCounter := InitialReferenceCount;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawFunction(const InitialReferenceCount: Integer): PHFunction;
var ID: Integer;
begin
  ID := FunctionMemorySpace.Add(TRunTimeVariable_Function.Create(nil, nil));
  Result := PHFunction(FunctionMemorySpace.MemorySpace.GetAsPointer(ID));
  Result.RefCounter := InitialReferenceCount;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawInteger(const AContent: HSInteger; const InitialReferenceCount: Integer): PHInteger;
var ID: Integer;
begin
  ID := IntegerMemorySpace.Add(TRunTimeVariable_Integer.Create(AContent));
  Result := PHInteger(IntegerMemorySpace.MemorySpace.GetAsPointer(ID));
  Result.RefCounter := InitialReferenceCount;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawString(const AContent: String; const InitialReferenceCount: Integer): PHString;
var ID: Integer;
begin
  ID := StringMemorySpace.Add(TRunTimeVariable_String.Create(AContent));
  Result := PHString(StringMemorySpace.MemorySpace.GetAsPointer(ID));
  Result.RefCounter := InitialReferenceCount;
  Result.ID := ID;
end;

function TMemoryManager.AllocateRawTable(const InitialReferenceCount: Integer): PHTable;
var Id: Integer;
begin
  Id := TableMemorySpace.Add(TRunTimeVariable_Table.Create(nil));
  Result := PHTable(TableMemorySpace.MemorySpace.GetAsPointer(Id));
  Result.RefCounter := InitialReferenceCount;
  Result.Id := Id;
end;

class function TMemoryManager.Create: TMemoryManager;
begin
  with Result do begin
    IntegerMemorySpace := IntegerMemorySpace.Create;
    FloatMemorySpace := FloatMemorySpace.Create;
    StringMemorySpace := StringMemorySpace.Create;
    TableMemorySpace := TableMemorySpace.Create;
    FunctionMemorySpace := FunctionMemorySpace.Create;
    BooleanMemorySpace := BooleanMemorySpace.Create;
    BuiltInFunctionMemorySpace := BuiltInFunctionMemorySpace.Create;
    BuiltInObjectMemorySpace := BuiltInObjectMemorySpace.Create;
    BuiltInFieldMemorySpace := BuiltInFieldMemorySpace.Create;
    BuiltInPropertyMemorySpace := BuiltInPropertyMemorySpace.Create;
    BuiltInMethodMemorySpace := BuiltInMethodMemorySpace.Create;
  end;
end;

procedure TMemoryManager.DisposeBoolean(const Disp: PHBoolean);
var Index: Integer;
begin
  Index := Disp^.ID;
  if TestAlloc then begin
    Dispose(Disp);
  end
  else if not (Index < 0) then
    BooleanMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeBuiltInField(const Index: Integer);
begin
  if Index < 0 then Exit;
  BuiltInFieldMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeBuiltInFunction(const Index: Integer);
begin
  if Index < 0 then Exit;
  BuiltInFunctionMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeBuiltInMethod(const Index: Integer);
begin
  if Index < 0 then Exit;
  BuiltInMethodMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeBuiltInObject(const Index: Integer);
begin
  if Index < 0 then Exit;
  BuiltInObjectMemorySpace.SegmentationSpace.Add(Index);
end;                                                    

procedure TMemoryManager.DisposeBuiltInProperty(const Index: Integer);
begin
  if Index < 0 then Exit;
  BuiltInPropertyMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeFloat(const Index: Integer);
begin
//  Writeln('Float disposal!');
  if Index < 0 then Exit;
  FloatMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeFunction(const Index: Integer);
//var Func: PHFunction;
{    Tabl: PHTable;}
begin
  if Index < 0 then Exit;
//  Func := PHFunction(FunctionMemorySpace.MemorySpace.GetAsPointer(Index));
//  Tabl := Func.LinkedEnvironment.Table;
//  Func.LinkedEnvironment.DecrementRefCounter(1);
//  if Tabl.RefCounter <= 0 then
//    DisposeTable(Tabl.ID);
  FunctionMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeInteger(const Index: Integer);
begin
//  Writeln('Integer disposal!');
  if Index < 0 then Exit;
  IntegerMemorySpace.SegmentationSpace.Add(Index);
end;

//var S: Integer = 0;

procedure TMemoryManager.DisposeString(const Index: Integer);
begin
  //Writeln('String disposal! ', S);
  //S := S + 1;
  if Index < 0 then Exit;
  { To cleanup memory in disposal. }
  { So, basicly, this code:

    A := '';
    for i := 1 to 1000000 do A := A + 'a';
    A := '';

    Will cost RAM, because, although `A` is disposed, its content still remains in segmented space, so in memory. }
  StringMemorySpace.MemorySpace.Items[Index].Content := '';
  StringMemorySpace.SegmentationSpace.Add(Index);
end;

procedure TMemoryManager.DisposeTable(const Index: Integer);
var //Key: TVariableShell;
    Value: PVariableShell;
    Table: PHTable;
    Pair: TTable.TItem;
begin
  if Index < 0 then Exit;
//  with TableMemorySpace.MemorySpace.Items[Index] do begin
//    Content.Free;
//    Content := nil;
//  end;
//  Writeln('Table disposal');
  Table := PHTable(TableMemorySpace.MemorySpace.GetAsPointer(Index));
  //for Key in Table.Content.Keys do begin
  for Pair in Table.Content.FItems^ do begin
    try
      if Pair.Key.TypeOf = Type_Nil then Continue;
      if Pair.HashCode < 0 then Continue;
      //Value := PVariableShell(Table.Content.PointedItems[Key]);
      Value := @Pair.Value;
      case Value.TypeOf of
        Type_Integer: with ToInt(Value^)^ do begin
          RefCounter := RefCounter - 1;
          if RefCounter <= 0 then DisposeInteger(ID);
        end;
        Type_Float: with ToFlt(Value^)^ do begin
          RefCounter := RefCounter - 1;
          if RefCounter <= 0 then DisposeFloat(ID);
        end;
        Type_String: with ToStr(Value^)^ do begin
          RefCounter := RefCounter - 1;
          if RefCounter <= 0 then DisposeString(ID);
          Writeln('Disposed String! ID: ', ID);
        end;
        Type_Boolean: with ToBool(Value^)^ do begin 
          RefCounter := RefCounter - 1;
          if RefCounter <= 0 then DisposeBoolean(PHBoolean(Value.Content));
        end;
        Type_Function: with ToFunc(Value^)^ do begin 
          RefCounter := RefCounter - 1;
          if RefCounter <= 0 then DisposeFunction(ID);
        end;
        Type_BuiltInFunction: with ToBIFunc(Value^)^ do begin 
          RefCounter := RefCounter - 1;
          if RefCounter <= 0 then DisposeBuiltInFunction(ID);
        end;
        Type_Table: with ToTable(Value^)^ do begin 
          RefCounter := RefCounter - 1;
          { We are not disposing it, as we can get infinite loop }
        end
      else
        raise Exception.Create('Unknown type to dispose!');
      end;
    except
      on E: Exception do ;
    end;
  end;
  try
    Table.Content.Free;
  except 
    on E: Exception do 
  end;
  TableMemorySpace.SegmentationSpace.Add(Index);
end;

{ TAssigner }

procedure TAssigner.AssignLinked(const AFrom: PVariableShell; var ATo: TVariableShell);
begin
  IncrementVariableReferenceCounter(AFrom^);
  DecrementVariableReferenceCounter(ATo);
  ATo.TypeOf := Type_Nil;
  ATo.Content := nil;
  ATo.Links := 0;
  ATo.Referenced := Pointer(AFrom);
 // IncrementVariableReferenceCounter(ATo);
end;

procedure TAssigner.AssignNewFloat(var ToShell: TVariableShell; const InitialContent: HSFloat; const RefCounter: Integer);
begin
  DecrementVariableReferenceCounter(ToShell);
  ToShell.TypeOf := Type_Float;
  ToShell.Content := MemoryManager.AllocateRawFloat(InitialContent, RefCounter);
end;

procedure TAssigner.AssignNewInteger(var ToShell: TVariableShell; const InitialContent: HSInteger; const RefCounter: Integer);
begin
  DecrementVariableReferenceCounter(ToShell);
  ToShell.TypeOf := Type_Integer;
  ToShell.Content := MemoryManager.AllocateRawInteger(InitialContent, RefCounter);
end;

procedure TAssigner.AssignNewString(var ToShell: TVariableShell; const InitialContent: String; const RefCounter: Integer);
begin
  DecrementVariableReferenceCounter(ToShell);
  ToShell.TypeOf := Type_String;
  ToShell.Content := MemoryManager.AllocateRawString(InitialContent, RefCounter);
end;

procedure TAssigner.AssignPointed(var AFirstShell, ASecondShell: TVariableShell);
begin
  if @AFirstShell = @ASecondShell then Exit;
  DecrementVariableReferenceCounter(AFirstShell);
  AFirstShell.Content := ASecondShell.Content;
  AFirstShell.TypeOf := ASecondShell.TypeOf;
  IncrementVariableReferenceCounter(AFirstShell);
end;

procedure TAssigner.AssignShells(var AFirstShell, ASecondShell: TVariableShell);
begin
  if @AFirstShell = @ASecondShell then Exit;
  // DecrementVariableReferenceCounter(AFirstShell);
  { It must be destroyed }
  DestroyShell(AFirstShell);
  AFirstShell := ASecondShell;
  with ASecondShell do Links := Links + 1;
end;

procedure TAssigner.AssignValues(var AFirstShell, ASecondShell: PVariableShell);
begin
  if AFirstShell = ASecondShell then Exit;
  if IsSimpleType(ASecondShell.TypeOf) then begin
    DecrementVariableReferenceCounter(AFirstShell^);
    case ASecondShell.TypeOf of
      Type_Integer: AFirstShell.Content := MemoryManager.AllocateRawInteger(PHInteger(ASecondShell.Content).Content);
      Type_Float  : AFirstShell.Content := MemoryManager.AllocateRawFloat(PHFloat(ASecondShell.Content).Content);
      Type_String : AFirstShell.Content := MemoryManager.AllocateRawString(PHString(ASecondShell.Content).Content);
      Type_Boolean: AFirstShell.Content := MemoryManager.AllocateRawBoolean(PHBoolean(ASecondShell.Content).Content)
    end;
  end
  else begin
    AssignPointed(AFirstShell^, ASecondShell^);
  end;
end;

procedure TAssigner.ChangeObjectiveVariableReferenceCounter(const AObject: PHBuiltInObject; const AValue: Integer);
begin
  if AObject = nil then
    Exit;
  with AObject^ do begin
    RefCounter := RefCounter + AValue;
    if RefCounter <= 0 then begin 
      if AttachedToMahine then
        try
          Content.Free; 
        except 
          on E: Exception do
            { Critical error. } 
        end;
      MemoryManager.DisposeBuiltInObject(ID);
    end;
  end;
end;

function TAssigner.CreateLinked(var Shell: TVariableShell): TVariableShell;
begin
  with Result do begin
    TypeOf := Type_Nil;
    Content := nil;
    Referenced := @Shell;
  end;
  // Inc(Shell.Links);
end;

procedure TAssigner.AssignValues(var AFirstShell, ASecondShell: TVariableShell);
begin
  if @AFirstShell = @ASecondShell then Exit;
  if IsSimpleType(ASecondShell.TypeOf) then begin
    DecrementVariableReferenceCounter(AFirstShell);
    AFirstShell.TypeOf := ASecondShell.TypeOf;
    case ASecondShell.TypeOf of
      Type_Integer: AFirstShell.Content := MemoryManager.AllocateRawInteger(PHInteger(ASecondShell.Content).Content);
      Type_Float  : AFirstShell.Content := MemoryManager.AllocateRawFloat(PHFloat(ASecondShell.Content).Content);
      Type_String : AFirstShell.Content := MemoryManager.AllocateRawString(PHString(ASecondShell.Content).Content);
      Type_Boolean: AFirstShell.Content := MemoryManager.AllocateRawBoolean(PHBoolean(ASecondShell.Content).Content)
    end;
  end
  else begin
    AssignPointed(AFirstShell, ASecondShell);
//    AFirstShell.TypeOf := ASecondShell.TypeOf;
  end;
end;

procedure TAssigner.DecrementVariableReferenceCounter(var Shell: TVariableShell);
var Ref: ^Integer;
begin
  if not (Shell.Referenced = nil) then begin
    DecrementVariableReferenceCounter(Shell.Referenced^);
    Exit;
  end;
  case Shell.TypeOf of
    Type_Integer: Ref := @PHInteger(Shell.Content).RefCounter;
    Type_Float: Ref := @PHFloat(Shell.Content).RefCounter;
    Type_String: Ref := @PHString(Shell.Content).RefCounter;
    Type_Boolean: Ref := @PHBoolean(Shell.Content).RefCounter;
    Type_Table: Ref := @PHTable(Shell.Content).RefCounter;
    Type_Function: Ref := @PHFunction(Shell.Content).RefCounter;
    Type_BuiltInFunction: Ref := @PHBuiltInFunction(Shell.Content).RefCounter;
    Type_BuiltInField: Ref := @PHBuiltInField(Shell.Content).RefCounter;
    Type_BuiltInProperty: Ref := @PHBuiltInProperty(Shell.Content).RefCounter;
    Type_BuiltInMethod: Ref := @PHBuiltInMethod(Shell.Content).RefCounter;
    Type_BuiltInObject: Ref := @PHBuiltInObject(Shell.Content).RefCounter;
  else
    Ref := nil;
  end;
  if Ref = nil then Exit;
  if Ref^ = HLVM_MemoryManager_InfinityLinks then Exit;
  Ref^ := Ref^ - 1;
  if (Ref^ <= 0) then begin
    Ref^ := 0;
    case Shell.TypeOf of
      Type_Integer: MemoryManager^.DisposeInteger(PHInteger(Shell.Content).ID);
      Type_Float  : MemoryManager^.DisposeFloat(PHFloat(Shell.Content).ID);
      Type_String : MemoryManager^.DisposeString(PHString(Shell.Content).ID);
      Type_Boolean: MemoryManager^.DisposeBoolean(PHBoolean(Shell.Content));
      Type_Table  : MemoryManager^.DisposeTable(PHTable(Shell.Content).ID);
      Type_Function: MemoryManager^.DisposeFunction(PHFunction(Shell.Content).ID);
      Type_BuiltInFunction: MemoryManager^.DisposeBuiltInFunction(PHBuiltInFunction(Shell.Content).ID);

      Type_BuiltInField: begin 
        with PHBuiltInField(Shell.Content)^ do begin 
          ChangeObjectiveVariableReferenceCounter(ParentObject, -1);
          MemoryManager^.DisposeBuiltInField(ID);
        end;
      end;
      Type_BuiltInProperty: begin
        with PHBuiltInProperty(Shell.Content)^ do begin                         
          ChangeObjectiveVariableReferenceCounter(ParentObject, -1);
          MemoryManager^.DisposeBuiltInProperty(ID);
        end;
      end;
      Type_BuiltInMethod: begin 
        with PHBuiltInProperty(Shell.Content)^ do begin 
          ChangeObjectiveVariableReferenceCounter(ParentObject, -1);
          MemoryManager^.DisposeBuiltInMethod(ID);
        end;
      end;
      Type_BuiltInObject: begin
        with PHBuiltInObject(Shell.Content)^ do begin
          if AttachedToMahine then
            try
              Content.Free;
            except
              on E: Exception do
            end;
          MemoryManager^.DisposeBuiltInObject(ID);
        end;
      end;
    end;
    { Assign nil to it }
    Shell.TypeOf := Type_Nil;
    Shell.Content := nil;
  end;
end;

procedure TAssigner.DestroyShell(var Shell: TVariableShell);
begin
  if Shell.Links <= 0 then begin
    DecrementVariableReferenceCounter(Shell);
//    Shell.TypeOf := Type_Nil;
//    Shell.Content := nil;
  end
  else
    Shell.Links := Shell.Links - 1;
end;

procedure TAssigner.IncrementVariableReferenceCounter(var Shell: TVariableShell);
var Ref: ^Integer;
begin
  if not (Shell.Referenced = nil) then begin
    IncrementVariableReferenceCounter(Shell.Referenced^);
    Exit;
  end;
  case Shell.TypeOf of
    Type_Integer: Ref := @PHInteger(Shell.Content).RefCounter;
    Type_Float: Ref := @PHFloat(Shell.Content).RefCounter;
    Type_String: Ref := @PHString(Shell.Content).RefCounter;
    Type_Boolean: Ref := @PHBoolean(Shell.Content).RefCounter;
    Type_Table: Ref := @PHTable(Shell.Content).RefCounter;
    Type_Function: Ref := @PHFunction(Shell.Content).RefCounter;
    Type_BuiltInFunction: Ref := @PHBuiltInFunction(Shell.Content).RefCounter;
    Type_BuiltInField: Ref := @PHBuiltInField(Shell.Content).RefCounter;
    Type_BuiltInProperty: Ref := @PHBuiltInProperty(Shell.Content).RefCounter;
    Type_BuiltInMethod: Ref := @PHBuiltInObject(Shell.Content).RefCounter;
    Type_BuiltInObject: Ref := @PHBuiltInMethod(Shell.Content).RefCounter;
  else
    Ref := nil;
  end;
  if Ref = nil then Exit;
  if Ref^ = HLVM_MemoryManager_InfinityLinks then Exit;
  Ref^ := Ref^ + 1;
end;

end.
