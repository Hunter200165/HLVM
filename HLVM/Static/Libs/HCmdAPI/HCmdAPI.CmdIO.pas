unit HCmdAPI.CmdIO;

interface

uses
  System.SysUtils,
  Winapi.Windows,
  HCmdAPI.IO,
  HCmdAPI.Commons;

const
  HCmd_ColourForegroundMask: Byte = {1111} 15;
  HCmd_ColourBackgroundMask: Byte = {11110000 = 255 - 15} 240;

const
  HCmd_Low_FirstChangeBit = 0;
  HCmd_Low_SecondChangeBit = 2;
  HCmd_High_FirstChangeBit = 4;
  HCmd_High_SecondChangeBit = 6;

type
  HCmd_TCmdIO = class(HCmd_TCustomIO)
  private
    FStdIO_Input_Handle: Cardinal;
    FStdIO_Output_Handle: Cardinal;
  public
    property StdIO_Input_Handle: Cardinal read FStdIO_Input_Handle;
    property StdIO_Output_Handle: Cardinal read FStdIO_Output_Handle;
    procedure Write(const Msg: String); override;

    constructor Create;
  end;

type
  HCmd_TCmdAdvancedIO = class(HCmd_TCmdIO)
  private
    FColourActive: Byte;
    FColourActiveBackground: Byte;
    FColourActiveForeground: Byte;
    FDefaultColourForeground: Byte;
    FDefaultColourBackground: Byte;
    function GetColourActiveRGBH: Byte;
    function GetColourActiveBackground: Byte;
    function GetColourActiveForeground: Byte;
  protected
    property ColourActive: Byte read FColourActive write FColourActive;
    property ColourActiveBackground: Byte read FColourActiveBackground write FColourActiveBackground;
    property ColourActiveForeground: Byte read FColourActiveForeground write FColourActiveForeground;

    function LowByteConvert(const Convert: Byte): Byte;
  public
    property ColourActiveBGRB: Byte read FColourActive;
    property ColourActiveRGBH: Byte read GetColourActiveRGBH;
    property ColourActiveBackgroundBGRB: Byte read FColourActiveBackground;
    property ColourActiveForegroundBGRB: Byte read FColourActiveForeground;
    property ColourActiveBackgroundRGBH: Byte read GetColourActiveBackground;
    property ColourActiveForegroundRGBH: Byte read GetColourActiveForeground;
    property DefaultColourForeground: Byte read FDefaultColourForeground write FDefaultColourForeground;
    property DefaultColourBackground: Byte read FDefaultColourBackground write FDefaultColourBackground;

    procedure SetColourStandard_BGRB(const Colour: Byte); overload;
    procedure SetColourStandard_BGRB(const Colour: String); overload;
    procedure SetColour_RGBH(const Colour: Byte); overload;
    procedure SetColour_RGBH(const Colour: String); overload;
    procedure SetColourStandardForeground_BGRB(const ColourForeground: Byte); overload;
    procedure SetColourStandardForeground_BGRB(const ColourForeground: String); overload;
    procedure SetColourStandardBackground_BGRB(const ColourBackground: Byte); overload;
    procedure SetColourStandardBackground_BGRB(const ColourBackground: String); overload;
    procedure SetColourStandardForeground_RGBH(const ColourForeground: Byte); overload;
    procedure SetColourStandardForeground_RGBH(const ColourForeground: String); overload;
    procedure SetColourStandardBackground_RGBH(const ColourBackground: Byte); overload;
    procedure SetColourStandardBackground_RGBH(const ColourBackground: String); overload;

    procedure SetDefaultColourForeground;
    procedure SetDefaultColourBackground;

    procedure SetDefaultColour;

    constructor Create;
  end;

var
  CmdIO: HCmd_TCmdAdvancedIO;

implementation

{ HCmd_TCmdIO }

constructor HCmd_TCmdIO.Create;
begin
  FStdIO_Output_Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  FStdIO_Input_Handle := GetStdHandle(STD_INPUT_HANDLE);
end;

procedure HCmd_TCmdIO.Write(const Msg: String);
begin
  System.Write(Msg);
end;

{ HCmd_TCmdAdvancedIO }

procedure HCmd_TCmdAdvancedIO.SetColourStandard_BGRB(const Colour: Byte);
begin
  SetConsoleTextAttribute(StdIO_Output_Handle, Colour);
  ColourActive := Colour;
  ColourActiveForeground := ColourActive and HCmd_ColourForegroundMask;
  ColourActiveBackground := ColourActive and HCmd_ColourBackgroundMask;
end;

constructor HCmd_TCmdAdvancedIO.Create;
begin
  inherited;
  DefaultColourBackground := 0; // Black
  DefaultColourForeground := Byte(HCmd_BinToInt64('1110')); // Silver
end;

function HCmd_TCmdAdvancedIO.GetColourActiveBackground: Byte;
begin
  Result := LowByteConvert(FColourActiveBackground shr 4) shl 4;
end;

function HCmd_TCmdAdvancedIO.GetColourActiveForeground: Byte;
begin
  Result := LowByteConvert(FColourActiveForeground);
end;

function HCmd_TCmdAdvancedIO.GetColourActiveRGBH: Byte;
begin
  Result := Byte(HCmd_SwapBits(ColourActive, HCmd_Low_SecondChangeBit, HCmd_Low_FirstChangeBit));
  Result := Byte(HCmd_SwapBits(Result, HCmd_High_SecondChangeBit, HCmd_High_FirstChangeBit));
end;

function HCmd_TCmdAdvancedIO.LowByteConvert(const Convert: Byte): Byte;
begin
  Result := Byte(HCmd_SwapBits(Convert, HCmd_Low_SecondChangeBit, HCmd_Low_FirstChangeBit));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardBackground_BGRB(const ColourBackground: String);
begin
  SetColourStandardBackground_BGRB(Byte(HCmd_BinToInt64(ColourBackground)));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardBackground_RGBH(const ColourBackground: String);
begin
  SetColourStandardBackground_RGBH(Byte(HCmd_BinToInt64(ColourBackground)));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardBackground_RGBH(const ColourBackground: Byte);
begin
  SetColourStandardBackground_BGRB(LowByteConvert(ColourBackground));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardBackground_BGRB(const ColourBackground: Byte);
begin
  SetColourStandard_BGRB((ColourBackground shl 4) or ColourActiveForeground);
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardForeground_BGRB(const ColourForeground: String);
begin
  SetColourStandardForeground_BGRB(Byte(HCmd_BinToInt64(ColourForeground)));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardForeground_RGBH(const ColourForeground: String);
begin
  SetColourStandardForeground_RGBH(Byte(HCmd_BinToInt64(ColourForeground)));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardForeground_RGBH(const ColourForeground: Byte);
begin
  SetColourStandardForeground_BGRB(LowByteConvert(ColourForeground));
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandardForeground_BGRB(const ColourForeground: Byte);
begin
  SetColourStandard_BGRB((ColourForeground) or ColourActiveBackground);
end;

procedure HCmd_TCmdAdvancedIO.SetColourStandard_BGRB(const Colour: String);
begin
  SetColourStandard_BGRB(Byte(HCmd_BinToInt64(Colour)));
end;

procedure HCmd_TCmdAdvancedIO.SetColour_RGBH(const Colour: String);
begin
  SetColour_RGBH(Byte(HCmd_BinToInt64(Colour)));
end;

procedure HCmd_TCmdAdvancedIO.SetDefaultColour;
begin
  SetDefaultColourForeground;
  SetDefaultColourBackground;
end;

procedure HCmd_TCmdAdvancedIO.SetDefaultColourBackground;
begin
  SetColourStandardBackground_BGRB(DefaultColourBackground);
end;

procedure HCmd_TCmdAdvancedIO.SetDefaultColourForeground;
begin
  SetColourStandardForeground_BGRB(DefaultColourForeground);
end;

procedure HCmd_TCmdAdvancedIO.SetColour_RGBH(const Colour: Byte);
var ColourStandard: Byte;
begin
  ColourStandard := Byte(HCmd_SwapBits(Colour, HCmd_Low_SecondChangeBit, HCmd_Low_FirstChangeBit));
  ColourStandard := Byte(HCmd_SwapBits(ColourStandard, HCmd_High_SecondChangeBit, HCmd_High_FirstChangeBit));
  SetColourStandard_BGRB(ColourStandard);
end;

initialization

  CmdIO := HCmd_TCmdAdvancedIO.Create;
  CmdIO.SetDefaultColour;

finalization

  try CmdIO.Free; except on E: Exception do; end;

end.
