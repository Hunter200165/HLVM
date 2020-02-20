unit HLVM.RunTime.Types;

interface

uses
  HLVM.Runtime.Hashes,
//  System.Generics.Collections,
  HLVM.Generics.Collections,
  HLVM.Types.Ordinal,
  HLVM.Types.Containers,
  System.Generics.Defaults,
  System.SysUtils,
  System.Hash,
  System.RTTI;

const
  Metatable_MethodAdd = 0;
  Metatable_MethodSub = 1;
  Metatable_MethodMul = 2;
  Metatable_MethodDiv = 3;
  Metatable_MethodNeg = 4;
  Metatable_MethodIndex    = 5;
  Metatable_MethodNewIndex = 6;
  Metatable_MethodCall = 7;
  Metatable_MethodType = 8;
  Metatable_MethodToString = 9;

  Metatable_MethodBitwiseAND = 10;
  Metatable_MethodBitwiseOR  = 11;
  Metatable_MethodBitwiseXOR = 12;
  Metatable_MethodBitwiseNOT = 13;

  Metatable_MethodEquals = 14;
  Metatable_MethodGreater = 15;
  Metatable_MethodGreaterEquals = 16;

const 
  MetaEventsWithTable = [Metatable_MethodIndex, Metatable_MethodNewIndex];

const
  Metatable_MetaAdd_Name = '__MetaAdd';
  Metatable_MetaSub_Name = '__MetaSub';
  Metatable_MetaMul_Name = '__MetaMul';
  Metatable_MetaDiv_Name = '__MetaDiv';
  Metatable_MetaNeg_Name = '__MetaNeg';

  Metatable_MetaIndex_Name    = '__MetaIndex';
  Metatable_MetaNewIndex_Name = '__MetaNewIndex';
  Metatable_MetaCall_Name = '__Call';
  Metatable_MetaType_Name = '__Type';
  Metatable_MetaToString_Name = '__ToString';

  Metatable_MetaBitwiseAND_Name = '__BitwiseAND';
  Metatable_MetaBitwiseOR_Name  = '__BitwiseOR';
  Metatable_MetaBitwiseXOR_Name = '__BitwiseXOR';
  Metatable_MetaBitwiseNOT_Name = '__BitwiseNOT';

  Metatable_MetaEquals_Name = '__Equals';
  Metatable_MetaGreater_Name = '__Greater';
  Metatable_MetaGreaterEquals_Name = '__GreaterEquals';
  
type 
  EMachineException = class(Exception);
  
type
  HLVM_Types = (
    Type_Nil,
    Type_Integer,
    Type_Float,
    Type_String,
    Type_Boolean,
    Type_Table,
    Type_Function,
    Type_BuiltInFunction,
    Type_BuiltInObject,
    Type_BuiltInField,
    Type_BuiltInProperty,
    Type_BuiltInMethod
  );
  SetOfTypes = set of HLVM_Types;

  TRunTimeVariable_Integer = record
  public var
    Content: HSInteger;
    RefCounter: Integer;
    ID: Integer;
  public
    class function Create(const InitialContent: HSInteger): TRunTimeVariable_Integer; static; inline;
  end;
  PRunTimeVariable_Integer = ^TRunTimeVariable_Integer;

  TRunTimeVariable_Float = record
  public var
    Content: HSFloat;
    RefCounter: Integer;
    ID: Integer;
  public
    class function Create(const InitialContent: HSFloat): TRunTimeVariable_Float; static; inline;
  end;
  PRunTimeVariable_Float = ^TRunTimeVariable_Float;

  TRunTimeVariable_String = record
  public var
    Content: String;
    RefCounter: Integer;
    ID: Integer;
  public
    class function Create(const InitialContent: String): TRunTimeVariable_String; static; inline;
  end;
  PRunTimeVariable_String = ^TRunTimeVariable_String;

  TRunTimeVariable_Boolean = record
  public var
    Content: Boolean;
    RefCounter: Integer;
    ID: Integer;
  public
    class function Create(const InitialContent: Boolean): TRunTimeVariable_Boolean; static; inline;
  end;
  PRunTimeVariable_Boolean = ^TRunTimeVariable_Boolean;

  PHInteger = PRunTimeVariable_Integer;
  PHFloat = PRunTimeVariable_Float;
  PHString = PRunTimeVariable_String;
  PHBoolean = PRunTimeVariable_Boolean;

  TVariableShell = record
  public var
    TypeOf: HLVM_Types;
    Content: Pointer;
    Links: Integer;
    Referenced: ^TVariableShell;
  public
    function GetHashCode: Integer; inline;
  public
    class function Create(const AType: HLVM_Types; const APointer: Pointer): TVariableShell; overload; static; inline;
    class function Create(const AInteger: PRunTimeVariable_Integer): TVariableShell; overload; static; inline;
    class function Create(const AFloat: PRunTimeVariable_Float): TVariableShell; overload; static; inline;
    class function Create(const AString: PRunTimeVariable_String): TVariableShell; overload; static; inline;
  end;
  PVariableShell = ^TVariableShell;

type
  TTablePrototype = TDictionary<TVariableShell, TVariableShell>;
  TVariableShellComparer = class(TEqualityComparer<TVariableShell>)
  public 
    function Equals(const Left, Right: TVariableShell): Boolean; override; 
    function GetHashCode(const Value: TVariableShell): Integer; override; 
  end;
  
type
  TMetatable = class; 
  TTable = class;
  
  TRunTimeVariable_Table = record
  public type 
    MT_Pointer = ^TRunTimeVariable_Table;
  public var
    Content: TTable;
    RefCounter: Integer;
    ID: Integer;
  public
    constructor Create(const Metatable: MT_Pointer);
  end;
  PRunTimeVariable_Table = ^TRunTimeVariable_Table;
  PHTable = PRunTimeVariable_Table;
  
  TTable = class(TTablePrototype)
  public 
    Metatable: PHTable;
    DataPointer: Pointer;
    procedure SetupMetatable(const Metatable: PHTable); inline;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  
  TMetatable = class(TTable)
  public 
    function GetMetamethod(const ID: Integer): PVariableShell; inline;
  end;
  
  TEnvironment = record //class(TObject)
  public type
    SelfPointer = ^TEnvironment;
  public var 
    Table: PHTable;
    ParentEnvironment: SelfPointer;
    RefCounter: Integer;
  public
    CurrentPosition: ^THPosition;
    CarryFlag: Boolean;

    BreakPos: Integer;
    ContinuePos: Integer;

    ForStep: Integer;
    ForVar: TRunTimeVariable_Integer;
    ForStart: Integer;
    ForFinish: Integer;
    ForAscending: Boolean;
    ForShell: PVariableShell;
  public
    function GetParentEnviroment: SelfPointer; inline;
    function GetValue(const Key: TVariableShell): TVariableShell; inline;
    function GetLocal(const Key: TVariableShell): TVariableShell; inline;
    function GetReferencedLocal(const Key: TVariableShell): PVariableShell; inline;
    function GetReferencedValue(const Key: TVariableShell): PVariableShell; inline;
    function GetPrehashedValue(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
    function GetPrehashedLocal(const AType: HLVM_Types; const Hash: Integer): PVariableShell; //inline;

    function PushNew(const Key: TVariableShell): PVariableShell; inline;

    procedure IncrementRefCounter(const Value: Integer = 1); inline;
    procedure DecrementRefCounter(const Value: Integer = 1); inline;
  public 
    class function Create(const ATable: PHTable; const AParent: SelfPointer; const InitialRefCounter: Integer = 1): SelfPointer; static;
//    destructor Destroy; override;
    procedure Free;
  end;
  PEnvironment = ^TEnvironment;

  TExpandedBaseEnvironment = class(TTablePrototype)
  public var
    ParentEnv: TExpandedBaseEnvironment;

    function GetValue(const [Ref] Key: TVariableShell): TVariableShell; //inline;
    function GetLocal(const [Ref] Key: TVariableShell): TVariableShell; //inline;
    function GetReferencedValue(const [Ref] Key: TVariableShell): PVariableShell; //inline;
    function GetReferencedLocal(const [Ref] Key: TVariableShell): PVariableShell; //inline;
    function GetPrehashedValue(const AType: HLVM_Types; const Hash: Integer): PVariableShell;// inline;
    function GetPrehashedLocal(const AType: HLVM_Types; const Hash: Integer): PVariableShell; //inline;

    function PushNew(const [Ref] Key: TVariableShell): PVariableShell; //inline;
    function ContainsPrehashedKey(const AType: HLVM_Types; const Hash: Integer): Integer; //inline;
  public
    constructor Create(const AParent: TExpandedBaseEnvironment);
  end;

  TTableArray = TTablePrototype.TItemArray;
  TFreeItemsFunction = procedure(const Items: TTableArray) of Object;
  TNewEnvironment = record
  public type
    PEnv = ^TNewEnvironment;
  public var
    Table: TExpandedBaseEnvironment;
    EnvironmentArray: TTableArray;
    Parent: PEnv;
    RefCounter: Integer;
  public var
    CurrentPosition: ^THPosition;
    CarryFlag: Boolean;

    BreakPos: Integer;
    ContinuePos: Integer;

    ForStep: Integer;
    ForVar: TRunTimeVariable_Integer;
    ForStart: Integer;
    ForFinish: Integer;
    ForAscending: Boolean;
    ForShell: Pointer;
  public var
    Name_GlobalVar: Boolean;
    Name_VarialbleName: String;
  public var
    { Preserved }
    PreservedCount: Integer;
    PreservedHashThreshold: Integer;
  public
    procedure IncrementRefCounter(const AFor: Integer); inline;
    procedure DecrementRefCounter(const AFor: Integer); inline;

    procedure SetupEnvironment; inline;
    procedure PreserveEnvironment; inline;
  public
    function GetReferencedValue(const [Ref] Key: TVariableShell): PVariableShell; //inline;
    function GetReferencedLocal(const [Ref] Key: TVariableShell): PVariableShell; //inline;
    function GetPrehashedValue(const AType: HLVM_Types; const Hash: Integer): PVariableShell; //inline;
    function GetPrehashedLocal(const AType: HLVM_Types; const Hash: Integer): PVariableShell; //inline;
  public var
    FreeItemsFunction: TFreeItemsFunction;
  public
    class function Create(const ATable: TExpandedBaseEnvironment; const AParent: PEnv; const AFinalFunc: TFreeItemsFunction; const InitialRefCounter: Integer = 1): PEnv; static; inline;
    procedure Free; inline;
  end;
  PNewEnvironment = ^TNewEnvironment;

type 
  TRunTimeVariable_Function = record
  public var
    Content: THLVMBytecodeChunk;
    LinkedEnvironment: PNewEnvironment;
    RefCounter: Integer;
    ID: Integer;
  public
    class function Create(const InitialContent: THLVMBytecodeChunk; const Environment: PNewEnvironment): TRunTimeVariable_Function; static; inline;
    procedure Destroy;
  end;
  PRunTimeVariable_Function = ^TRunTimeVariable_Function;
  PHFunction = PRunTimeVariable_Function;

type 
  TFunctionOutput = array of TVariableShell;
  TBuiltInFunction = reference to function(const ASubState: TObject): TFunctionOutput;
  TRunTimeVariable_BuiltInFunction = record
  public var
    Content: TBuiltInFunction;
    RefCounter: Integer;
    ID: Integer;
  public
    class function Create(const InitialContent: TBuiltInFunction): TRunTimeVariable_BuiltInFunction; static; inline;
  end;
  PRunTimeVariable_BuiltInFunction = ^TRunTimeVariable_BuiltInFunction;
  PHBuiltInFunction = PRunTimeVariable_BuiltInFunction;

type
  { To work with classes }
  TRunTimeVariable_BuiltInObject = record
  public var
    Content: TObject;
    RefCounter: Integer;
    ID: Integer;
    AttachedToMahine: Boolean;
    LinkedMemoryManager: Pointer;
  public
    class function Create(const InitialContent: TObject; const LMemoryManager: Pointer): TRunTimeVariable_BuiltInObject; static; inline;
  end;
  PRunTimeVariable_BuiltInObject = ^TRunTimeVariable_BuiltInObject;

  TRunTimeVariable_BuiltInProperty = record
  public var
    Content: String;
    RefCounter: Integer;
    ID: Integer;
    ParentObject: PRunTimeVariable_BuiltInObject;
  public
    class function Create(const InitialContent: String; const AParent: PRunTimeVariable_BuiltInObject): TRunTimeVariable_BuiltInProperty; static; inline;
  end;
  PRunTimeVariable_BuiltInProperty = ^TRunTimeVariable_BuiltInProperty;

  { Raw and low-level built-in field }
  TRunTimeVariable_BuiltInField = record
  public var
    Content: String;
    RefCounter: Integer;
    ID: Integer;
    ParentObject: PRunTimeVariable_BuiltInObject;
  public
    class function Create(const InitialContent: String; const AParent: PRunTimeVariable_BuiltInObject): TRunTimeVariable_BuiltInField; static; inline;
  end;
  PRunTimeVariable_BuiltInField = ^TRunTimeVariable_BuiltInField;

  TRunTimeVariable_BuiltInMethod = record
  public var
    Content: String;
    RefCounter: Integer;
    ID: Integer;
    ParentObject: PRunTimeVariable_BuiltInObject;
  public
    class function Create(const InitialContent: String; const AParent: PRunTimeVariable_BuiltInObject): TRunTimeVariable_BuiltInMethod; static; inline;
  end;
  PRunTimeVariable_BuiltInMethod = ^TRunTimeVariable_BuiltInMethod;

type
  PHBuiltInObject = PRunTimeVariable_BuiltInObject;
  PHBuiltInField = PRunTimeVariable_BuiltInField;
  PHBuiltInProperty = PRunTimeVariable_BuiltInProperty;
  PHBuiltInMethod = PRunTimeVariable_BuiltInMethod;
  
procedure AssignNil(var AShell: TVariableShell); inline;
  
implementation

procedure AssignNil(var AShell: TVariableShell);
begin 
  with AShell do begin 
    Content := nil;
    TypeOf := Type_Nil;
    Links := 0;
  end;
end;

{ TRunTimeVariable_Integer }

class function TRunTimeVariable_Integer.Create(const InitialContent: HSInteger): TRunTimeVariable_Integer;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    Id := -1;
  end;
end;

{ TRunTimeVariable_String }

class function TRunTimeVariable_String.Create(const InitialContent: String): TRunTimeVariable_String;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    ID := -1;
  end;
end;

{ TRunTimeVariable_Float }

class function TRunTimeVariable_Float.Create(const InitialContent: HSFloat): TRunTimeVariable_Float;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    Id := -1;
  end;
end;

{ TVariableShell }

class function TVariableShell.Create(const AType: HLVM_Types; const APointer: Pointer): TVariableShell;
begin
  with Result do begin
    TypeOf := AType;
    Content := APointer;
    Links := 0;
    Referenced := nil;
  end;
end;

class function TVariableShell.Create(const AInteger: PRunTimeVariable_Integer): TVariableShell;
begin
  Result := Create(Type_Integer, AInteger);
end;

class function TVariableShell.Create(const AFloat: PRunTimeVariable_Float): TVariableShell;
begin
  Result := Create(Type_FLoat, AFloat);
end;

class function TVariableShell.Create(const AString: PRunTimeVariable_String): TVariableShell;
begin
  Result := Create(Type_String, AString);
end;

function TVariableShell.GetHashCode: Integer;
begin
  case TypeOf of
    Type_Nil: raise EMachineException.Create('Cannot get hashcode of nil.');
    Type_Integer: Result := THasher.HashInteger(PHInteger(Content).Content);
    Type_Float: Result := THasher.HashFloat(PHFloat(Content).Content);
    Type_String: Result := THasher.HashString(PHString(Content).Content);
    Type_Boolean: Result := THasher.HashBoolean(PHBoolean(Content).Content);
  else 
    //raise EMachineException.Create('Unknown type');
    Result := Integer(Content);
  end;
end;

{ TVariableShellComparer }

function TVariableShellComparer.Equals(const Left, Right: TVariableShell): Boolean;
begin
  Result := Left.TypeOf = Right.TypeOf;
  if Result and (Left.TypeOf in [Type_Integer, Type_Float, Type_Nil, Type_String, Type_Boolean])then
    Exit;
  if Result then case Left.TypeOf of
    Type_Nil: Result := True;
    Type_Integer: Result := PHInteger(Left.Content).Content = PHInteger(Right.Content).Content;
    Type_Float: Result := PHFloat(Left.Content).Content = PHFloat(Right.Content).Content;
    Type_String: Result := PHString(Left.Content).Content = PHString(Right.Content).Content; 
  else 
    Result := Left.Content = Right.Content;
  end;
end;

function TVariableShellComparer.GetHashCode(const Value: TVariableShell): Integer;
begin
  Result := Value.GetHashCode;
end;

{ TTable }

constructor TTable.Create;
begin
  inherited Create(TVariableShellComparer.Create);
  Metatable := nil;
end;

destructor TTable.Destroy;
begin
  if not (Metatable = nil) then begin
    with Metatable^ do RefCounter := RefCounter - 1;
  end;
  inherited;
end;

procedure TTable.SetupMetatable(const Metatable: PHTable);
begin
  if not (Self.Metatable = nil) then with Self.Metatable^ do 
    RefCounter := RefCounter - 1;
  Self.Metatable := Metatable;
end;

{ TMetatable }

function TMetatable.GetMetamethod(const ID: Integer): PVariableShell;
var Key: TVariableShell;
    Value: PVariableShell;
    KeyVar: TRunTimeVariable_String;
    KeyName: String;
begin
  case ID of
    Metatable_MethodAdd: KeyName := Metatable_MetaAdd_Name;
    Metatable_MethodSub: KeyName := Metatable_MetaSub_Name;
    Metatable_MethodMul: KeyName := Metatable_MetaMul_Name;
    Metatable_MethodDiv: KeyName := Metatable_MetaDiv_Name;
    Metatable_MethodNeg: KeyName := Metatable_MetaNeg_Name;
    Metatable_MethodIndex: KeyName := Metatable_MetaIndex_Name;
    Metatable_MethodNewIndex: KeyName := Metatable_MetaNewIndex_Name;
    Metatable_MethodCall: KeyName := Metatable_MetaCall_Name;
    Metatable_MethodType: KeyName := Metatable_MetaType_Name;
    Metatable_MethodToString: KeyName := Metatable_MetaToString_Name;
    Metatable_MethodBitwiseAND: KeyName := Metatable_MetaBitwiseAND_Name;
    Metatable_MethodBitwiseOR: KeyName := Metatable_MetaBitwiseOR_Name;
    Metatable_MethodBitwiseXOR: KeyName := Metatable_MetaBitwiseXOR_Name;
    Metatable_MethodBitwiseNOT: KeyName := Metatable_MetaBitwiseNOT_Name;
    Metatable_MethodEquals: KeyName := Metatable_MetaEquals_Name;
    Metatable_MethodGreater: KeyName := Metatable_MetaGreater_Name;
    Metatable_MethodGreaterEquals: KeyName := Metatable_MetaGreaterEquals_Name;
  else 
    Result := nil;
    Exit;
  end;
  KeyVar := TRunTimeVariable_String.Create(KeyName);
  Key := TVariableShell.Create(PHString(@KeyVar));
  if ContainsKey(Key) then begin 
    Value := PVariableShell(Self.PointedItems[Key]);
    if not (Value.TypeOf in [Type_Function, Type_BuiltInFunction]) and not ((ID in MetaEventsWithTable) and (Value.TypeOf = Type_Table)) then begin
      // AssignNil(Result); 
      Result := nil;
    end
    else begin 
      Result := Value;
    end;
  end
  else begin 
    Result := nil;
  end;
end;

{ TRunTimeVariable_Table }

constructor TRunTimeVariable_Table.Create(const Metatable: MT_Pointer);
begin
  Content := TTable.Create;
  // Content.Metatable := PHTable(Metatable);
  if not (Metatable = nil) then
    Content.SetupMetatable(PHTable(Metatable));
  RefCounter := 0;
  ID := -1;
end;

{ TEnvironment }

class function TEnvironment.Create(const ATable: PHTable; const AParent: SelfPointer; const InitialRefCounter: Integer): SelfPointer;
begin
  New(Result);
  with Result^ do begin
    Table := ATable;
    with Table^ do RefCounter := RefCounter + 1;
    RefCounter := InitialRefCounter;
    ParentEnvironment := AParent;
    if not (AParent = nil) then
      AParent.IncrementRefCounter;
  end;
end;

procedure TEnvironment.DecrementRefCounter(const Value: Integer);
begin
  RefCounter := RefCounter - Value;
  if not (ParentEnvironment = nil) then
    TEnvironment(ParentEnvironment^).DecrementRefCounter;
  if RefCounter <= 0 then begin
//    Writeln('TEnvironment destroyed');
    Free;
  end;
end;

procedure TEnvironment.Free;
begin
  with Table^ do 
    RefCounter := RefCounter - 1;
  //inherited;
  Dispose(@Self);
end;

function TEnvironment.GetLocal(const Key: TVariableShell): TVariableShell;
begin
  Result := GetReferencedLocal(Key)^;
end;

function TEnvironment.GetParentEnviroment: SelfPointer;
begin
  Result := ParentEnvironment;
end;

function TEnvironment.GetPrehashedLocal(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
var Pos: Integer;
    Shell: TVariableShell;
begin
  Shell.TypeOf := AType;
  with Table.Content do
    if Count >= FGrowThreshold then Grow;
  Pos := Self.Table.Content.GetBucketIndex(Shell, Hash);
  if Pos < 0 then begin
      Table.Content.DoAdd(Hash, not Pos, Shell, TVariableShell.Create(Type_Nil, nil));
      Result := @Table.Content.FItems^[not Pos].Value;
  end
  else
    Result := @Table.Content.FItems^[Pos].Value;
end;

function TEnvironment.GetPrehashedValue(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
var Pos: Integer;
    Shell: TVariableShell;
begin
  Shell.TypeOf := AType;
  with Table.Content do
    if Count >= FGrowThreshold then Grow;
  Pos := Self.Table.Content.GetBucketIndex(Shell, Hash);
  if Pos < 0 then
    if not (ParentEnvironment = nil) then
      Result := ParentEnvironment.GetPrehashedValue(AType, Hash)
    else begin
      Table.Content.DoAdd(Hash, not Pos, Shell, TVariableShell.Create(Type_Nil, nil));
      Result := @Table.Content.FItems^[not Pos].Value;
    end
  else
    Result := @Table.Content.FItems^[Pos].Value;
end;

function TEnvironment.GetReferencedLocal(const Key: TVariableShell): PVariableShell;
begin
  if Table.Content.ContainsKey(Key) then
    Result := PVariableShell(Table.Content.PointedItems[Key])
  else
    Result := PushNew(Key);
end;

function TEnvironment.GetReferencedValue(const Key: TVariableShell): PVariableShell;
var PEnv: SelfPointer;
begin
  if Table.Content.ContainsKey(Key) then
    Result := PVariableShell(Table.Content.PointedItems[Key])
  else begin 
    PEnv := GetParentEnviroment;
    if PEnv = nil then
      Result := PushNew(Key)
    else 
      Result := PEnv.GetReferencedValue(Key);
  end;
end;

function TEnvironment.GetValue(const Key: TVariableShell): TVariableShell;
var PEnv: SelfPointer;
begin
  if Table.Content.ContainsKey(Key) then
    Result := Table.Content.Items[Key]
  else begin 
    PEnv := GetParentEnviroment;
    if (PEnv = nil) then
      Result := PushNew(Key)^
    else 
      Result := PEnv.GetValue(Key);
  end;
end;

procedure TEnvironment.IncrementRefCounter(const Value: Integer);
begin
  RefCounter := RefCounter + Value;
  if not (ParentEnvironment = nil) then
    ParentEnvironment.IncrementRefCounter(Value);
end;

function TEnvironment.PushNew(const Key: TVariableShell): PVariableShell;
begin
  Table.Content.AddOrSetValue(Key, TVariableShell.Create(Type_Nil, nil));
  Result := PVariableShell(Table.Content.PointedItems[Key]);
end;

{ TRunTimeVariable_Function }

class function TRunTimeVariable_Function.Create(const InitialContent: THLVMBytecodeChunk; const Environment: PNewEnvironment): TRunTimeVariable_Function;
begin
  with Result do begin
    Content := InitialContent;
    LinkedEnvironment := Environment;
    if not (Environment = nil) then
      Environment.IncrementRefCounter(1);
    RefCounter := 0;
    ID := -1;
  end;
end;

procedure TRunTimeVariable_Function.Destroy;
begin
//  with LinkedEnvironment^ do 
//    RefCounter := RefCounter - 1;
  LinkedEnvironment.DecrementRefCounter(1);
end;

{ TRunTimeVariable_BuiltInFunction }

class function TRunTimeVariable_BuiltInFunction.Create(const InitialContent: TBuiltInFunction): TRunTimeVariable_BuiltInFunction;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    ID := -1;
  end;
end;

{ TRunTimeVariable_Boolean }

class function TRunTimeVariable_Boolean.Create(const InitialContent: Boolean): TRunTimeVariable_Boolean;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    ID := -1;
  end;
end;

{ TNewEnvironment }

class function TNewEnvironment.Create(const ATable: TExpandedBaseEnvironment; const AParent: PEnv; const AFinalFunc: TFreeItemsFunction; const InitialRefCounter: Integer): PEnv;
begin
  New(Result);
  with Result^ do begin
    Parent := AParent;
    Table := ATable;
    RefCounter := InitialRefCounter;
    PreservedCount := 0;
    PreservedHashThreshold := 0;
    FreeItemsFunction := AFinalFunc;
    { For indexer not to free this space! }
    ForVar.ID := -1;
    { And cross reference, of course }
    { Link is infinite }
    ForVar.RefCounter := $7FFFFFFF;

//    ForVar := nil;
    ForShell := nil;
  end;
  if not (AParent = nil) then AParent.IncrementRefCounter(1);
end;

procedure TNewEnvironment.DecrementRefCounter(const AFor: Integer);
begin
  Dec(RefCounter, AFor);
  if not (Parent = nil) then
    Parent.DecrementRefCounter(AFor);
  if RefCounter <= 0 then
    Free;
end;

procedure TNewEnvironment.Free;
begin
  { Free all elements in scope }
  FreeItemsFunction(EnvironmentArray);
  SetLength(EnvironmentArray, 0);
  ForShell := nil;
  Name_GlobalVar := False;
  Name_VarialbleName := '';
//  try if not (ForVar = nil) then Dispose(ForVar); except on E: Exception do end;
  Dispose(@Self);
end;

function TNewEnvironment.GetPrehashedLocal(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
begin
  Result := Table.GetPrehashedLocal(AType, Hash);
  PreserveEnvironment;
end;

function TNewEnvironment.GetPrehashedValue(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
var Pos: Integer;
begin
  SetupEnvironment;
  Pos := Table.ContainsPrehashedKey(AType, Hash);
  if Pos >= 0 then begin
    Result := @(Table.FItems^[Pos].Value);
  end
  else if (Parent = nil) then begin
    Result := Table.GetPrehashedValue(AType, Hash);
    PreserveEnvironment;
  end
  else begin
    PreserveEnvironment;
    Result := Self.Parent.GetPrehashedValue(AType, Hash);
    SetupEnvironment;
  end;
end;

function TNewEnvironment.GetReferencedLocal(const [Ref] Key: TVariableShell): PVariableShell;
begin
//  SetupEnvironment;
  Result := Table.GetReferencedLocal(Key);
  PreserveEnvironment;
end;

function TNewEnvironment.GetReferencedValue(const [Ref] Key: TVariableShell): PVariableShell;
// var Pos: Integer;
begin
  SetupEnvironment;
  if Table.ContainsKey(Key) then begin
    Result := PVariableShell(Table.PointedItems[Key]);
  end
  else if not (Parent = nil) then begin
    PreserveEnvironment;
    Result := Parent.GetReferencedValue(Key);
    SetupEnvironment;
  end
  else begin
    Result := Table.GetReferencedValue(Key);
    PreserveEnvironment;
  end;
end;

procedure TNewEnvironment.IncrementRefCounter(const AFor: Integer);
begin
  Inc(RefCounter, AFor);
  if not (Parent = nil) then
    Parent.IncrementRefCounter(AFor);
end;

procedure TNewEnvironment.PreserveEnvironment;
begin
//  EnvironmentArray := Table.FItems;
  PreservedCount := Table.FCount;
  PreservedHashThreshold := Table.FGrowThreshold;
end;

procedure TNewEnvironment.SetupEnvironment;
begin
  with Table do begin
    FItems := @EnvironmentArray;
    FCount := PreservedCount;
    FGrowThreshold := PreservedHashThreshold;
  end;
end;

{ TExpandedBaseEnvironment }

function TExpandedBaseEnvironment.ContainsPrehashedKey(const AType: HLVM_Types; const Hash: Integer): Integer;
var Shell: TVariableShell;
begin
  Shell.TypeOf := AType;
  Result := GetBucketIndex(Shell, Hash);
end;

constructor TExpandedBaseEnvironment.Create(const AParent: TExpandedBaseEnvironment);
begin
  inherited Create(TVariableShellComparer.Create);
  ParentEnv := AParent;
end;

function TExpandedBaseEnvironment.GetLocal(const [Ref] Key: TVariableShell): TVariableShell;
begin
  if ContainsKey(Key) then
    Result := Items[Key]
  else begin
    AssignNil(Result);
    AddOrSetValue(Key, Result);
  end;
end;

function TExpandedBaseEnvironment.GetPrehashedLocal(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
var Pos: Integer;
    Shell: TVariableShell;
begin
  Shell.TypeOf := AType;
  if Count >= FGrowThreshold then Grow;
  Pos := GetBucketIndex(Shell, Hash);
  if Pos < 0 then
    {if not (ParentEnv = nil) then
      Result := ParentEnv.GetPrehashedValue(AType, Hash)
    else }
    begin
      DoAdd(Hash, not Pos, Shell, TVariableShell.Create(Type_Nil, nil));
      Result := @FItems^[not Pos].Value;
    end
  else
    Result := @FItems^[Pos].Value;
end;

function TExpandedBaseEnvironment.GetPrehashedValue(const AType: HLVM_Types; const Hash: Integer): PVariableShell;
var Pos: Integer;
    Shell: TVariableShell;
begin
  Shell.TypeOf := AType;
  if Count >= FGrowThreshold then Grow;
  Pos := GetBucketIndex(Shell, Hash);
  if Pos < 0 then
    if not (ParentEnv = nil) then
      Result := ParentEnv.GetPrehashedValue(AType, Hash)
    else begin
      DoAdd(Hash, not Pos, Shell, TVariableShell.Create(Type_Nil, nil));
      Result := @FItems^[not Pos].Value;
    end
  else
    Result := @FItems^[Pos].Value;
end;

function TExpandedBaseEnvironment.GetReferencedLocal(const [Ref] Key: TVariableShell): PVariableShell;
begin
  if ContainsKey(Key) then
    Result := PVariableShell(PointedItems[Key])
  else begin
    AddOrSetValue(Key, TVariableShell.Create(Type_Nil, nil));
    Result := PVariableShell(PointedItems[Key]);
  end;
end;

function TExpandedBaseEnvironment.GetReferencedValue(const [Ref] Key: TVariableShell): PVariableShell;
begin
  if ContainsKey(Key) then
    Result := PVariableShell(PointedItems[Key])
  else if not (ParentEnv = nil) then
    Result := ParentEnv.GetReferencedValue(Key)
  else begin
    AddOrSetValue(Key, TVariableShell.Create(Type_Nil, nil));
    Result := PVariableShell(PointedItems[Key]);
  end;
end;

function TExpandedBaseEnvironment.GetValue(const [Ref] Key: TVariableShell): TVariableShell;
begin
  if ContainsKey(Key) then
    Result := Self.Items[Key]
  else if not (ParentEnv = nil) then
    Result := ParentEnv.GetValue(Key)
  else begin
    AssignNil(Result);
//    Self.Items[Key] := Result;
    AddOrSetValue(Key, Result);
  end;
end;

function TExpandedBaseEnvironment.PushNew(const [Ref] Key: TVariableShell): PVariableShell;
begin
  AddOrSetValue(Key, TVariableShell.Create(Type_Nil, nil));
  Result := PVariableShell(PointedItems[Key]);
end;

{ TRunTimeVariable_BuiltInMethod }

class function TRunTimeVariable_BuiltInMethod.Create(const InitialContent: String; const AParent: PRunTimeVariable_BuiltInObject): TRunTimeVariable_BuiltInMethod;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    Id := -1;
//    InstanceOfClass := AInstance;
    ParentObject := AParent;
  end;
end;

{ TRunTimeVariable_BuiltInProperty }

class function TRunTimeVariable_BuiltInProperty.Create(const InitialContent: String; const AParent: PRunTimeVariable_BuiltInObject): TRunTimeVariable_BuiltInProperty;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    Id := -1;
    ParentObject := AParent;
  end;
end;

{ TRunTimeVariable_BuiltInField }

class function TRunTimeVariable_BuiltInField.Create(const InitialContent: String; const AParent: PRunTimeVariable_BuiltInObject): TRunTimeVariable_BuiltInField;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    Id := -1;
    ParentObject := AParent;
  end;
end;

{ TRunTimeVariable_BuiltInObject }

class function TRunTimeVariable_BuiltInObject.Create(const InitialContent: TObject; const LMemoryManager: Pointer): TRunTimeVariable_BuiltInObject;
begin
  with Result do begin
    Content := InitialContent;
    RefCounter := 0;
    Id := -1;
    AttachedToMahine := False;
    LinkedMemoryManager := LMemoryManager;
  end;
end;

end.
