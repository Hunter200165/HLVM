unit HCmdAPI.Commons;

interface

uses
  System.Math,
  System.StrUtils,
  System.SysUtils;

function HCmd_BinToInt64(BinStr: String): Int64;
function HCmd_SwapBits(const Content: UInt64; const BitPos1, BitPos2: Byte): UInt64;

implementation

{ Yeah, it is copy of HCommon_BinToInt64 function from HCommon API }
{ Because I do not want to bring HCommonAPI -> HCryptoAPI binds }

function HCmd_BinToInt64(BinStr: String): Int64;
var Pos: int64;
    i: Integer;
begin
  Pos := 0;
  Result := 0;
  for i := 1 to Length(BinStr) do begin
    Result := Result + (StrToInt(BinStr[i]) * Round(Power(2, Pos)));
    inc(Pos);
  end;
end;

function HCmd_SwapBits(const Content: UInt64; const BitPos1, BitPos2: Byte): UInt64;
var Mask1, Mask2, TempContainer1, TempContainer2, Shift: UInt64;
    Pos1IsBigger: Boolean;
begin
  Mask1 := Round(Power(2, BitPos1));
  Mask2 := Round(Power(2, BitPos2));
  Result := Content and not Mask1 and not Mask2; // Nulify this bits
  if BitPos1 > BitPos2 then
    Pos1IsBigger := True
  else if BitPos2 > BitPos1 then
    Pos1IsBigger := False
  else
    raise Exception.Create('Cannot swap one bit');
  TempContainer1 := Content and Mask1;
  TempContainer2 := Content and Mask2;
  if Pos1IsBigger then begin
    Shift := BitPos1 - BitPos2;
    TempContainer1 := TempContainer1 shr Shift;
    TempContainer2 := TempContainer2 shl Shift;
  end
  else begin
    Shift := BitPos2 - BitPos1;
    TempContainer1 := TempContainer1 shl Shift;
    TempContainer2 := TempContainer2 shr Shift;
  end;
  Result := Result or TempContainer1 or TempContainer2;
end;

end.
