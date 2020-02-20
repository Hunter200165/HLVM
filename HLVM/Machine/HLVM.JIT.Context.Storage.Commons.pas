unit HLVM.JIT.Context.Storage.Commons;

interface

uses
  System.SysUtils,
  System.Math,
  { Parts }
  uTExtendedX87,
  ExactFloatToStr_JH0,
  { HLVM }
  HLVM.JIT.Context.Storage;

type
  TIntArray = array of Integer;

{ Common classes }
type
  THLVMObject = class(TObject);
  EMachineException = class(Exception);

type
  TCommons = record
  public
    class function WithinRange(const A: Int64; const Min, Max: Int64): Boolean; static;
    class function GetGrow(const A: Int64): Int64; static;
    class function GetHashedPosition(const AHash: Cardinal; const Field: Cardinal): Integer; static;

    class function FloatToString(const A: HLVM_Float): String; static;
  end;

implementation

{ TCommons }

class function TCommons.FloatToString(const A: HLVM_Float): String;
begin
  {$IfNDef Win64}
    Result := FloatToStr(A);
  {$Else}

  Result := ExactFloatToStrEx(A, '.', '');

  {$EndIf}
end;

class function TCommons.GetGrow(const A: Int64): Int64;
begin
  if A < 4 then
    Result := A + 8
  else if A < 8 then
    Result := A + 4
  else
    Result := (A * 3) div 2;
end;

class function TCommons.GetHashedPosition(const AHash, Field: Cardinal): Integer;
begin
  Result := AHash mod Field;
end;

class function TCommons.WithinRange(const A, Min, Max: Int64): Boolean;
begin
  Result := (A >= Min) and (A <= Max);
end;

end.
