unit HLVM.Runtime.Hashes;

interface

uses
  System.Hash;

const
  Hash_Float_Initial   = $57FBA6A;
  Hash_Integer_Initial = $30D021D;
  Hash_String_Initial  = $4C3F644;
  Hash_Boolean_Initial = $1A2D973;

const
  PositiveMask = not Integer($80000000);

type
  THasher = record
  public
    class function HashInteger(const AInteger: Int64): Integer; static; inline;
    class function HashFloat(const AFloat: Extended): Integer; static; inline;
    class function HashString(const AString: String): Integer; static; inline;
    class function HashBoolean(const ABoolean: Boolean): Integer; static; inline;

    class function FixHash(const AHash: Integer): Integer; static; inline;
  end;

var
  HashOfTrue, HashOfFalse: Integer;

implementation

{ THasher }

class function THasher.FixHash(const AHash: Integer): Integer;
begin
  Result := PositiveMask and ((PositiveMask and AHash) + 1);
end;

class function THasher.HashBoolean(const ABoolean: Boolean): Integer;
begin
  Result := THashBobJenkins.GetHashValue(ABoolean, SizeOf(ABoolean), Hash_Boolean_Initial);
end;

class function THasher.HashFloat(const AFloat: Extended): Integer;
begin
  Result := THashBobJenkins.GetHashValue(AFloat, SizeOf(Extended), Hash_Float_Initial);
end;

class function THasher.HashInteger(const AInteger: Int64): Integer;
begin
  Result := THashBobJenkins.GetHashValue(AInteger, SizeOf(Int64), Hash_Integer_Initial);
end;

class function THasher.HashString(const AString: String): Integer;
begin
  Result := THashBobJenkins.GetHashValue(PChar(AString)^, SizeOf(Char) * Length(AString), Hash_String_Initial);
end;

begin
  HashOfTrue := THasher.HashBoolean(True);
  HashOfFalse := THasher.HashBoolean(False);
end.
