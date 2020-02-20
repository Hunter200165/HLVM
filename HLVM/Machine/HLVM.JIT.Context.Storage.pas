unit HLVM.JIT.Context.Storage;

interface

uses
  {$IfDef Win64} uTExtendedX87, {$EndIf}
  System.SysUtils,
  System.Hash,

  HLVM.Context.Hashing;

type
  HLVM_Command = Byte;
  HLVM_Integer = Int64;
  HLVM_Float = Extended; // {$IfNDef Win64} Extended {$Else} uTExtendedX87.TExtendedX87 {$EndIf};

{$if SizeOf(HLVM_Integer) > SizeOf(HLVM_Float)}
  {$message error 'Could not use integer bigger than float value'}
{$endif}

{$if SizeOf(HLVM_Integer) < SizeOf(Pointer)}
  {$message error 'Could not use integer smaller than pointer value'}
{$endif}

type
  IHLVMUniversal = interface end;

const
  HLVM_Type_Nil     = 0;
  HLVM_Type_Integer = 1;
  HLVM_Type_Float   = 2;
  HLVM_Type_String  = 3;
  HLVM_Type_Boolean = 4;
  HLVM_Type_Table   = 5;
  HLVM_Type_Object  = 6;
  HLVM_Type_Function = 7;
  HLVM_Type_BuiltInFunction = 8;
  { KadJIT alpha v2.1 (massive typed update) }
  HLVM_Type_GRCObject = 9;
  HLVM_Type_Type      = 10;

type
  HLVM_Variable = record
  public var
    Numeric: HLVM_Float;
    Stringy: String;
    Objective: IHLVMUniversal;
    Typed: Integer;
  public
    function GetInt: HLVM_Integer; inline;
    function GetFloat: HLVM_Float; inline;
    function GetBoolean: Boolean; inline;
    function GetPointer: Pointer; inline;
    function GetForeignPointer: Pointer; inline;
    procedure SetInt(const A: HLVM_Integer); inline;
    procedure SetFloat(const A: HLVM_Float); inline;
    procedure SetBoolean(const A: Boolean); inline;
    procedure SetPointer(const A: Pointer); inline;

    function GetHash: Integer; inline;
    function Equals(const [Ref] ATo: HLVM_Variable): Boolean;
    function ToString: String;
  public
    class function NewString(const Content: String): HLVM_Variable; static;
    class function NewInteger(const Content: HLVM_Integer): HLVM_Variable; static;
    class function NewFloat(const Content: HLVM_Float): HLVM_Variable; static;
  end;
  { Basically using pointers. }
  PHLVM_Variable = ^HLVM_Variable;

const HLVM_NIL: HLVM_Variable = (
  {$IfNDef Win64} Numeric: 0; {$EndIf}
  Stringy: '';
  Objective: nil;
  Typed: HLVM_Type_Nil;
);

implementation

{ HLVM_Variable }

uses
  HLVM.JIT.Context.Storage.Commons;

function HLVM_Variable.Equals(const [Ref] ATo: HLVM_Variable): Boolean;
begin
  Result := Self.Typed = ATo.Typed;
  if Result then
    case Self.Typed of
      HLVM_Type_Nil: Result := True;
      HLVM_Type_Integer: Result := Self.GetInt = ATo.GetInt;
      HLVM_Type_Float: Result := Self.GetFloat = ATo.GetFloat;
      HLVM_Type_String: Result := Self.Stringy = ATo.Stringy;
      HLVM_Type_Boolean: Result := Self.GetBoolean = ATo.GetBoolean;
    else
      Result := Pointer(Self.Objective) = Pointer(Self.Objective);
    end;
end;

function HLVM_Variable.GetBoolean: Boolean;
var A: HLVM_Integer absolute Numeric;
begin
  Result := not (A = 0);
end;

function HLVM_Variable.GetFloat: HLVM_Float;
begin
  Result := Numeric;
end;

function HLVM_Variable.GetForeignPointer: Pointer;
begin
  Result := Pointer(Self.Objective);
end;

function HLVM_Variable.GetHash: Integer;
begin
  case Self.Typed of
    HLVM_Type_Nil: Result := 0;
    HLVM_Type_Integer: Result := HashNumber(GetInt);
    HLVM_Type_Float:
      {$IfNDef Win64}
        Result := HashFloat(GetFloat);
      {$Else}
        Result := HashRaw(Numeric, SizeOf(HLVM_Type_Float), HASHING_MASK_FLOAT);
      {$EndIf}
    HLVM_Type_String: Result := HashString(Stringy);
    HLVM_Type_Boolean: Result := HashBoolean(GetBoolean);
    HLVM_Type_BuiltInFunction: Result := HashPointer(GetPointer);
  else
    Result := HashPointer(GetForeignPointer);
  end;
end;

function HLVM_Variable.GetInt: HLVM_Integer;
var A: HLVM_Integer absolute Numeric;
begin
  Result := A;
end;

function HLVM_Variable.GetPointer: Pointer;
var A: Pointer absolute Numeric;
begin
  Result := A;
end;

class function HLVM_Variable.NewFloat(const Content: HLVM_Float): HLVM_Variable;
begin
  Result.Typed := HLVM_Type_Float;
  Result.SetFloat(Content);
end;

class function HLVM_Variable.NewInteger(const Content: HLVM_Integer): HLVM_Variable;
begin
  Result.Typed := HLVM_Type_Integer;
  Result.SetInt(Content);
end;

class function HLVM_Variable.NewString(const Content: String): HLVM_Variable;
begin
  Result.Typed := HLVM_Type_String;
  Result.Stringy := Content;
end;

procedure HLVM_Variable.SetBoolean(const A: Boolean);
var B: Int64 absolute Numeric;
begin
  B := Byte(A);
end;

procedure HLVM_Variable.SetFloat(const A: HLVM_Float);
begin
  Numeric := A;
end;

procedure HLVM_Variable.SetInt(const A: HLVM_Integer);
var B: HLVM_Integer absolute Numeric;
begin
  B := A;
end;

procedure HLVM_Variable.SetPointer(const A: Pointer);
var B: Pointer absolute Numeric;
begin
  B := A;
end;

function HLVM_Variable.ToString: String;
begin
  case Typed of
    HLVM_Type_Nil: Result := 'nil';
    HLVM_Type_Integer: Result := IntToStr(GetInt);
    HLVM_Type_Float: Result := TCommons.FloatToString(GetFloat).Replace(',', '.');
    HLVM_Type_String: Result := Stringy;
    HLVM_Type_Boolean: Result := BoolToStr(GetBoolean, True);
  else
    Result := '<Unimplemented in tostring>';
  end;
end;

initialization

  PHLVM_Variable(@HLVM_NIL).Numeric := 0;

end.
