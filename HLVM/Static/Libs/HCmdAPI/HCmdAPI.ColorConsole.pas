unit HCmdAPI.ColorConsole;

interface

uses
  System.StrUtils,
  HCmdAPI.CmdIO;

const
  HCmd_Backspace = #8;
  HCmd_DeleteCharacter = HCmd_Backspace + ' ' + HCmd_Backspace;

procedure SetColour(const Colour: String); overload;
procedure SetColour(const Colour: Byte); overload;

procedure WriteLnColour(const Msg, Colour: String; const RememberColour: Boolean = False);
procedure WriteColour(const Msg, Colour: String; const RememberColour: Boolean = False);

procedure ClearChars(const Count: Cardinal);

implementation

procedure SetColour(const Colour: String); overload;
begin
  CmdIO.SetColour_RGBH(Colour);
end;

procedure SetColour(const Colour: Byte); overload;
begin
  CmdIO.SetColour_RGBH(Colour);
end;

procedure WriteLnColour(const Msg, Colour: String; const RememberColour: Boolean = False);
var BackColour: Byte;
begin
  BackColour := CmdIO.ColourActiveBGRB;
  SetColour(Colour);
  CmdIO.Writeln(Msg);
  if not RememberColour then
    CmdIO.SetColourStandard_BGRB(BackColour);
end;

procedure WriteColour(const Msg, Colour: String; const RememberColour: Boolean = False);
var BackColour: Byte;
begin
  BackColour := CmdIO.ColourActiveBGRB;
  SetColour(Colour);
  CmdIO.Write(Msg);
  if not RememberColour then
    CmdIO.SetColourStandard_BGRB(BackColour);
end;

procedure ClearChars(const Count: Cardinal);
begin
  Write(DupeString(HCmd_DeleteCharacter, Count));
end;

end.
