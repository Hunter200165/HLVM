unit HLVM.Context.Hashing;

interface

uses
  System.Hash;

type
  Int = Integer;

const
  {
    F671F193
    2AA0B53F
    4D7717C3
    63819908
    6C9F3584
    5D72185B
    2AE5D136
    6E460CCC
    FFFF4F97
    42CBCD47
  }
  HASHING_MASK_BYTE    = Int($F671F193);
  HASHING_MASK_INTEGER = Int($2AA0B53F);
  HASHING_MASK_FLOAT   = Int($4D7717C3);
  HASHING_MASK_STRING  = Int($63819908);
  HASHING_MASK_BOOLEAN = Int($6C9F3584);
  HASHING_MASK_POINTER = Int($5D72185B);

function HashByte(const AByte: Byte): Integer; inline;
function HashNumber(const ANum: Int64): Integer; inline;
function HashFloat(const ANum: Extended): Integer; inline;
function HashString(const AStr: String): Integer; inline;
function HashBoolean(const ABool: Boolean): Integer; inline;
function HashPointer(const APointer: Pointer): Integer; inline;
function HashRaw(const AValue; const Size: Integer; const Mask: Integer): Integer; inline;

implementation

function HashByte(const AByte: Byte): Integer; inline;
begin
  Result := THashBobJenkins.GetHashValue(AByte, SizeOf(Byte), HASHING_MASK_BYTE);
end;

function HashNumber(const ANum: Int64): Integer; inline;
begin
  Result := THashBobJenkins.GetHashValue(ANum, SizeOf(Int64), HASHING_MASK_INTEGER);
end;

function HashFloat(const ANum: Extended): Integer; inline;
begin
  Result := THashBobJenkins.GetHashValue(ANum, SizeOf(Extended), HASHING_MASK_FLOAT);
end;

function HashString(const AStr: String): Integer; inline;
begin
  Result := THashBobJenkins.GetHashValue(PChar(AStr)^, SizeOf(Char) * Length(AStr), HASHING_MASK_STRING);
end;

function HashBoolean(const ABool: Boolean): Integer; inline;
begin
  Result := THashBobJenkins.GetHashValue(Byte(ABool), SizeOf(Byte), HASHING_MASK_BYTE);
end;

function HashPointer(const APointer: Pointer): Integer; inline;
begin
  Result := THashBobJenkins.GetHashValue(APointer, SizeOf(Pointer), HASHING_MASK_POINTER);
end;

function HashRaw(const AValue; const Size: Integer; const Mask: Integer): Integer;
begin
  Result := THashBobJenkins.GetHashValue(AValue, Size, Mask);
end;

end.
