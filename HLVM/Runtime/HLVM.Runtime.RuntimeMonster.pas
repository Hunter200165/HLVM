unit HLVM.Runtime.RuntimeMonster;

interface

uses
  HLVM.Runtime.Types,
  HLVM.Types.Utilities,
  HLVM.Types.Stack,
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.Variants,
  HLVM.Memory.Manager;

type
  TStringArray = array of String;
  TRuntimeMonster = class
  public
    class function DumpMethod(const AMethod: TRttiMethod; const AOverload: Integer): String;
    class function DumpParameter(const AParameter: TRttiParameter): String;

    class function CastDynamicValueToTValue(const DynValue: TVariableShell; const AType: TRttiType): TValue;
    class function CastTValueToDynamicValue(const Value: TValue; const MemoryManager: PMemoryManager): TVariableShell;
    class function CanCastDynamicValueToTValue(const DynValue: TVariableShell; const AType: TRttiType): Boolean;

    class function InvokeMethod(const ASubState: TObject; const AInstance: TObject; const AnMethod: String): TVariableShell;
    class function GetClassPart(const MemoryManager: PMemoryManager; const AInstance: PHBuiltInObject; const Name: String): TVariableShell;

    class function GetFieldValue(const MemoryManager: PMemoryManager; const AInstance: TObject; const AField: String): TVariableShell;
    class function GetPropertyValue(const MemoryManager: PMemoryManager; const AInstance: TObject; const AField: String): TVariableShell;
    class procedure SetFieldValue(const AInstance: TObject; const AField: String; const AValue: TVariableShell);
    class procedure SetPropertyValue(const AInstance: TObject; const AProperty: String; const AValue: TVariableShell);

    class function GetMethods(const AInstance: TObject): TStringArray;
//    class function Index(const AObject: TObject; const Name: String);
  end;
  ERuntimeMonster = class(Exception);

const AMsg = 'Cannot convert dynamic type "%s" to type kind "%s"';

implementation

uses 
  HLVM.Runtime.Machine;

{ TRuntimeMonster }

class function TRuntimeMonster.CanCastDynamicValueToTValue(const DynValue: TVariableShell; const AType: TRttiType): Boolean;
var AFType: TTypeKind;
begin
  AFType := AType.TypeKind;
  case AFType of
    tkInteger, tkInt64:
      Result := DynValue.TypeOf = Type_Integer;
    tkEnumeration:
      Result := DynValue.TypeOf in [Type_Boolean, Type_Integer];
    tkFloat:
      Result := DynValue.TypeOf in [Type_Integer, Type_Float];
    tkString, tkUString, tkWString, tkChar, tkWChar:
      Result := DynValue.TypeOf = Type_String;
    tkVariant:
      Result := True;
    tkClassRef, tkClass:
      Result := (DynValue.TypeOf in [Type_BuiltInObject, Type_Nil]);
    tkPointer:
      Result := (DynValue.TypeOf in [Type_Integer, Type_Nil]);
  else
    Result := False;
  end;
end;

class function TRuntimeMonster.CastDynamicValueToTValue(const DynValue: TVariableShell; const AType: TRttiType): TValue;
var AFType: TTypeKind;
begin
  AFType := AType.TypeKind;
  case AFType of
    tkInteger: begin
      if DynValue.TypeOf = Type_Integer then begin
        Result := PHInteger(DynValue.Content).Content;
      end
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
    end;
    tkChar, tkWideChar: begin
      if DynValue.TypeOf = Type_String then begin
        if Length(PHString(DynValue.Content).Content) <= 0 then
          raise ERuntimeMonster.Create('Unexpected empty string while expected char.');
        Result := PHString(DynValue.Content).Content[1];
      end
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
    end;
    tkFloat: begin
      case DynValue.TypeOf of
        Type_Integer: begin
          Result := Extended(PHInteger(DynValue.Content).Content);
        end;
        Type_Float: begin
          Result := PHFloat(DynValue.Content).Content;
        end;
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
      end;
    end;
    tkString, tkLString, tkWideString, tkUnicodeString: begin
      if DynValue.TypeOf = Type_String then begin
        Result := PHString(DynValue.Content).Content;
      end
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
    end;
    tkEnumeration: begin
      case DynValue.TypeOf of
        Type_Integer: begin
          Result := Byte(PHInteger(DynValue.Content).Content and 255);
        end;
        Type_Boolean: begin
          Result := PHBoolean(DynValue.Content).Content;
        end;
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
      end;
    end;
//    tkLString: ;
//    tkWString: ;
    tkVariant: begin
      case DynValue.TypeOf of
        Type_Nil: Result := TValue.FromVariant(Null);
        Type_Integer: Result := PHInteger(DynValue.Content).Content;
        Type_Float: Result := PHFloat(DynValue.Content).Content;
        Type_String: Result := PHString(DynValue.Content).Content;
        Type_Boolean: Result := PHBoolean(DynValue.Content).Content;
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
      end;
    end;
    tkInt64: begin
      if DynValue.TypeOf = Type_Integer then begin
        Result := PHInteger(DynValue.Content).Content;
      end
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
    end;
//    tkUString: ;
    tkClassRef, tkClass: begin
      case DynValue.TypeOf of
        Type_Nil: Result := TObject(nil);
        Type_BuiltInObject: begin
          Result := PHBuiltInObject(DynValue.Content).Content;
        end;
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
      end;
    end;
    tkPointer: begin
      case DynValue.TypeOf of
        Type_Nil: Result := Pointer(nil);
        Type_BuiltInObject: Result := Pointer(PHBuiltInObject(DynValue.Content).Content);
        Type_Integer: Result := Pointer(PHInteger(DynValue.Content).Content);
      else
        raise ERuntimeMonster.Create(Format(AMsg, [ TypeToString(DynValue.TypeOf), AType.ToString ]));
      end;
    end;
//    tkProcedure: ;
//    tkMRecord: ;
  else
    raise ERuntimeMonster.Create('Cannot resolve type kind: ' + AType.ToString);
  end;
end;

class function TRuntimeMonster.CastTValueToDynamicValue(const Value: TValue; const MemoryManager: PMemoryManager): TVariableShell;
//var This: TSubState;
begin
//  This := ASubState as TSubState;
  case Value.Kind of
//    tkUnknown: ;
    tkInteger, tkInt64:
      Result := MemoryManager.AllocateNewInteger(Value.AsInt64);
    tkEnumeration: begin
      if Value.AsInteger in [0 .. 1] then
        Result := MemoryManager.AllocateNewBoolean(Value.AsInteger = 1)
      else
        Result := MemoryManager.AllocateNewInteger(Value.AsInteger);
    end;
    tkFloat:
      Result := MemoryManager.AllocateNewFloat(Value.AsExtended);
    tkString, tkChar, tkWChar, tkLString, tkWideString, tkUnicodeString:
      Result := MemoryManager.AllocateNewString(Value.AsString);
//    tkSet: ;
//    tkClass: ;
//    tkMethod: ;
//    tkVariant: ;
//    tkArray: ;
//    tkRecord: ;
//    tkInterface: ;
//    tkDynArray: ;
    tkClassRef: begin
      if Value.AsClass = nil then
        Result := TVariableShell.Create(Type_Nil, nil)
      else
        Result := MemoryManager.AllocateNewInteger(Int64(Value.AsInt64));
    end;
    tkPointer: begin
      if Value.AsInteger = 0 then
        Result := TVariableShell.Create(Type_Nil, nil)
      else
        Result := MemoryManager.AllocateNewInteger(Int64(Value.AsInt64));
    end;
  else
    raise ERuntimeMonster.Create('Cannot convert type kind ' + Integer(Value.Kind).ToString);
  end;
end;

class function TRuntimeMonster.DumpMethod(const AMethod: TRttiMethod; const AOverload: Integer): String;
var Params: TArray<TRttiParameter>;
    i: Integer;
begin
  if not (AMethod.ReturnType = nil) then
    Result := 'function '
  else
    Result := 'procedure ';
  Result := Result + AMethod.Name;
  Params := AMethod.GetParameters;
  Result := Result + '(';
  if Length(Params) > 0 then
    Result := Result + DumpParameter(Params[0]);
  for i := 1 to Length(Params) - 1 do
    Result := Result + '; ' + DumpParameter(Params[i]);
  Result := Result + ')';
  if not (AMethod.ReturnType = nil) then
    Result := Result + ': ' + AMethod.ReturnType.ToString + ';'
  else
    Result := Result + ';';
  if AOverload > 0 then
    Result := Result + ' (overload of ' + IntToStr(AOverload) + ')';
end;

class function TRuntimeMonster.DumpParameter(const AParameter: TRttiParameter): String;
begin
  Result := '';
  if pfVar in AParameter.Flags then
    Result := 'var '
  else if pfConst in AParameter.Flags then
    Result := 'const '
  else if pfOut in AParameter.Flags then
    Result := 'out ';
  Result := Result + AParameter.Name;
  if not (AParameter.ParamType = nil) then begin
    Result := Result + ': ';
    if pfArray in AParameter.Flags then
      Result := Result + 'array of ';
    Result := Result + AParameter.ParamType.ToString;
  end;
end;

class function TRuntimeMonster.GetClassPart(const MemoryManager: PMemoryManager; const AInstance: PHBuiltInObject; const Name: String): TVariableShell;
var RttiContext: TRttiContext;
    RttiType: TRttiType;
    TempMethod: TRttiMethod;
//    This: TSubState;
    TempField: TRttiField;
    TempProperty: TRttiProperty;
begin
//  This := ASubState as TSubState;

  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(AInstance.Content.ClassType);
  TempMethod := RttiType.GetMethod(Name);
  if not (TempMethod = nil) then begin
    Result := MemoryManager.AllocateNewBuiltInMethod(Name, AInstance);
    Exit;
  end;
  TempField := RttiType.GetField(Name);
  if not (TempField = nil) then begin
    Result := MemoryManager.AllocateNewBuiltInField(Name, AInstance);
    Exit;
  end;
  TempProperty := RttiType.GetProperty(Name);
  if not (TempProperty = nil) then begin
    Result := MemoryManager.AllocateNewBuiltInProperty(Name, AInstance);
    Exit;
  end;

  Result := TVariableShell.Create(Type_Nil, nil);
end;

class function TRuntimeMonster.GetFieldValue(const MemoryManager: PMemoryManager; const AInstance: TObject; const AField: String): TVariableShell;
var RttiContext: TRttiContext;
    RttiType: TRttiType;
    AFieldItself: TRttiField;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(AInstance.ClassType);
  AFieldItself := RttiType.GetField(AField);
  Result := CastTValueToDynamicValue(AFieldItself.GetValue(AInstance), MemoryManager);
end;

class function TRuntimeMonster.GetMethods(const AInstance: TObject): TStringArray;
var Rtti: TRttiType;
    RttiContext: TRttiContext;
    RttiMethods: TArray<TRttiMethod>;
    i: Integer;
begin
  Rtti := RttiContext.GetType(AInstance.ClassType);
  RttiMethods := Rtti.GetMethods;
  SetLength(Result, Length(RttiMethods));
  for i := 0 to Length(RttiMethods) - 1 do begin
    Result[i] := DumpMethod(RttiMethods[i], -1);
    case RttiMethods[i].Visibility of
      mvPrivate: Result[i] := 'private ' + Result[i];
      mvProtected: Result[i] := 'protected ' + Result[i];
      mvPublic: Result[i] := 'public ' + Result[i];
      mvPublished: Result[i] := 'published ' + Result[i];
    end;
  end;
end;

class function TRuntimeMonster.GetPropertyValue(const MemoryManager: PMemoryManager; const AInstance: TObject; const AField: String): TVariableShell;
var RttiContext: TRttiContext;
    RttiType: TRttiType;
    AFieldItself: TRttiProperty;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(AInstance.ClassType);
  AFieldItself := RttiType.GetProperty(AField);
  Result := CastTValueToDynamicValue(AFieldItself.GetValue(AInstance), MemoryManager);
end;

class function TRuntimeMonster.InvokeMethod(const ASubState: TObject; const AInstance: TObject; const AnMethod: String): TVariableShell;
type PStackContainer = ^TStackContainer;
var Params: TArray<TRttiParameter>;
    ParamCount, i, k, AConter, AOverloads: Integer;
    AMadeIt: Boolean;
    Arguments: TArray<TValue>;
    This: TSubState;
    Returned: TValue;
    AType: TRttiType;
    AContext: TRttiContext;
    AMethod: TRttiMethod;
    AMethods: TArray<TRttiMethod>;
//    AllArguments: TArray<TStackContainer>;
begin
  This := ASubState as TSubState;

  AType := AContext.GetType(AInstance.ClassType);

  AConter := (Integer(This.StateStack.SP) - Integer(This.StateStack.LowBound) + SizeOf(TStackContainer)) div SizeOf(TStackContainer);

  AMethods := AType.GetMethods;
  AMethod := nil;
  AOverloads := 0;
  for i := 0 to Length(AMethods) - 1 do begin
    if not SameText(AMethods[i].Name, AnMethod) then Continue;
    AOverloads := AOverloads + 1;
    AMadeIt := True;
    Params := AMethods[i].GetParameters;
    ParamCount := Length(Params);
    if ParamCount > AConter then Continue;
    for k := 0 to ParamCount - 1 do begin
      if (Params[k].ParamType = nil) or not CanCastDynamicValueToTValue(PStackContainer(Integer(This.StateStack.LowBound) + k * SizeOf(TStackContainer)).GetShell^, Params[k].ParamType) then begin
        AMadeIt := False;
        Break;
      end;
    end;

    if AMadeIt then begin
      AMethod := AMethods[i];
      Break;
    end;
  end;

  if AMethod = nil then
    raise ERuntimeMonster.Create('Cannot find method with this spectre of parameters.');

//  AMethod := AType.GetMethod(AnMethod);

  Params := AMethod.GetParameters;
  ParamCount := Length(Params);

  SetLength(Arguments, ParamCount);

  for i := 0 to ParamCount - 1 do begin 
    with This.StateStack do begin 
      if Integer(SP) < Integer(Base) then
        raise ERuntimeMonster.CreateFmt('Cannot call method [%s]: Not enough parameters (%d of %d)', [DumpMethod(AMethod, AOverloads), i, ParamCount]);
      try
        if Params[i].ParamType = nil then
          raise ERuntimeMonster.Create('Unknown type of argument -> Cannot pass');
        Arguments[i] := CastDynamicValueToTValue(PickAddress^.GetShell^, Params[i].ParamType);
      except 
        on E: Exception do begin 
          raise ERuntimeMonster.CreateFmt('Cannot call method [%s]: %s (%d of %d)', [DumpMethod(AMethod, AOverloads), E.Message, i + 1, ParamCount]);
        end;
      end;
    end;
  end;

  try
    try
      if not AMethod.IsClassMethod then
        Returned := AMethod.Invoke(AInstance, Arguments)
      else
        Returned := AMethod.Invoke(AInstance.ClassType, Arguments);
    except
      on E: Exception do begin
        E.Message := E.Message + ' &[METHOD_INVOKE]';
      end;
    end;
    if AMethod.ReturnType = nil then
      Result := TVariableShell.Create(Type_Nil, nil)
    else
      Result := CastTValueToDynamicValue(Returned, This.MemoryManager);
  except
    on E: Exception do
      raise ERuntimeMonster.CreateFmt('Invoked method [%s] raised an exception [%s]: %s', [DumpMethod(AMethod, AOverloads), E.ClassName, E.Message]);
  end;
end;

class procedure TRuntimeMonster.SetFieldValue(const AInstance: TObject; const AField: String; const AValue: TVariableShell);
var RttiContext: TRttiContext;
    RttiType: TRttiType;
    RttiField: TRttiField;
begin
  RttiType := RttiContext.GetType(AInstance.ClassType);
  RttiField := RttiType.GetField(AField);
  RttiField.SetValue(AInstance, CastDynamicValueToTValue(AValue, RttiField.FieldType));
end;

class procedure TRuntimeMonster.SetPropertyValue(const AInstance: TObject; const AProperty: String; const AValue: TVariableShell);
var RttiContext: TRttiContext;
    RttiType: TRttiType;
    RttiProperty: TRttiProperty;
begin
  RttiType := RttiContext.GetType(AInstance.ClassType);
  RttiProperty := RttiType.GetProperty(AProperty);
  RttiProperty.SetValue(AInstance, CastDynamicValueToTValue(AValue, RttiProperty.PropertyType));
end;

end.
