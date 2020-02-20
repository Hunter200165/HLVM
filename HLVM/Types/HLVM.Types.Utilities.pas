unit HLVM.Types.Utilities;

interface

uses
  HLVM.Runtime.Types,
  System.SysUtils;


function IsSimpleType(const AType: HLVM_Types): Boolean; inline;
function IsTypedWith(const [ref] Shell: TVariableShell; const ATypes: SetOfTypes): Boolean; inline;

function IsNumbericType(const [ref] Shell: TVariableShell): Boolean; inline;
function IsIntegerType(const [ref] Shell: TVariableShell): Boolean; inline;
function IsFloatType(const [ref] Shell: TVariableShell): Boolean; inline;

function ToInt(const [ref] Shell: TVariableShell): PHInteger; inline;
function ToFlt(const [ref] Shell: TVariableShell): PHFloat; inline;
function ToStr(const [ref] Shell: TVariableShell): PHString; // inline;
function ToBool(const [ref] Shell: TVariableShell): PHBoolean; inline;
function ToFunc(const [ref] Shell: TVariableShell): PHFunction; inline;
function ToBIFunc(const [ref] Shell: TVariableShell): PHBuiltInFunction; inline;
function ToTable(const [ref] Shell: TVariableShell): PHTable; inline;

function TypeToString(const AType: HLVM_Types): String; inline;
function VarPointerToString(const APointer: Pointer): String; inline;

implementation

function IsSimpleType(const AType: HLVM_Types): Boolean;
begin
  Result := AType in [Type_Nil, Type_Integer, Type_Float, Type_String, Type_Boolean];
end;

function IsTypedWith(const [ref] Shell: TVariableShell; const ATypes: SetOfTypes): Boolean;
begin
  Result := Shell.TypeOf in ATypes;
end;

function IsNumbericType(const [ref] Shell: TVariableShell): Boolean;
begin
  Result := IsTypedWith(Shell, [Type_Integer, Type_Float]);
end;

function IsIntegerType(const [ref] Shell: TVariableShell): Boolean;
begin
  Result := Shell.TypeOf = Type_Integer;
end;

function IsFloatType(const [ref] Shell: TVariableShell): Boolean;
begin
  Result := Shell.TypeOf = Type_Float;
end;

function ToInt(const [ref] Shell: TVariableShell): PHInteger; inline;
begin
  Result := PHInteger(Shell.Content);
end;

function ToFlt(const [ref] Shell: TVariableShell): PHFloat; inline;
begin
  Result := PHFloat(Shell.Content);
end;

function ToStr(const [ref] Shell: TVariableShell): PHString; //inline;
begin
  Result := PHString(Shell.Content);
end;

function ToBool(const [ref] Shell: TVariableShell): PHBoolean;
begin
  Result := PHBoolean(Shell.Content);
end;

function ToFunc(const [ref] Shell: TVariableShell): PHFunction;
begin
  Result := PHFunction(Shell.Content);
end;

function ToBIFunc(const [ref] Shell: TVariableShell): PHBuiltInFunction;
begin
  Result := PHBuiltInFunction(Shell.Content);
end;

function ToTable(const [ref] Shell: TVariableShell): PHTable;
begin
  Result := PHTable(Shell.Content);
end;

function TypeToString(const AType: HLVM_Types): String;
begin
  case AType of
    Type_Nil:             Result := 'Nil';
    Type_Integer:         Result := 'Integer';
    Type_Float:           Result := 'Float';
    Type_String:          Result := 'String';
    Type_Boolean:         Result := 'Boolean';
    Type_Table:           Result := 'Table';
    Type_Function:        Result := 'Function';
    Type_BuiltInFunction: Result := 'BuiltIn_Funcion';
    Type_BuiltInObject:   Result := 'BuiltIn_Object';
    Type_BuiltInField:    Result := 'BuiltIn_Field';
    Type_BuiltInProperty: Result := 'BuiltIn_Property';
    Type_BuiltInMethod:   Result := 'BuiltIn_Method';
  else
    Result := 'Unknown';
  end;
end;

function VarPointerToString(const APointer: Pointer): String;
begin
  if UInt64(APointer) > High(Cardinal) then
    Result := IntToHex(UInt64(APointer), 8)
  else
    Result := IntToHex(UInt64(APointer), 16);
end;

end.
