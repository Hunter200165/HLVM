unit HCmdAPI.CmdProgress;

interface

uses
  System.Math,
  System.StrUtils,
  System.SysUtils,
  HCmdAPI.CmdIO,
  HCmdAPI.ColorConsole;

type
  HCmd_TCmdProgress = record
  private
    FDrawn: Boolean;
    FNeedSymbols: Integer;
    FPercentage: Integer;
    FSymbolToDraw: Char;
    FDoneColour: String;
    FInProcessColour: String;
    FOtherColour: String;
    FPrevious: Integer;
  public
    property Drawn: Boolean read FDrawn write FDrawn;
    property NeedSymbols: Integer read FNeedSymbols write FNeedSymbols;
    property Percentage: Integer read FPercentage write FPercentage;
    property SymbolToDraw: Char read FSymbolToDraw write FSymbolToDraw;

    property DoneColour: String read FDoneColour write FDoneColour;
    property InProcessColour: String read FInProcessColour write FInProcessColour;
    property OtherColour: String read FOtherColour write FOtherColour;

    procedure Draw;

    class function Create: HCmd_TCmdProgress; static;
  end;

implementation

{ HCmd_TCmdProgress }

class function HCmd_TCmdProgress.Create: HCmd_TCmdProgress;
begin
  with Result do begin
    Drawn := False;
    NeedSymbols := 32;
    Percentage := 0;
    SymbolToDraw := '#';
    DoneColour := '1101'; // Yellow
    InProcessColour := '0000'; // Black
    OtherColour := '1110'; // Silver
  end;
  Result.FPrevious := -1;
end;

procedure HCmd_TCmdProgress.Draw;
var PercentPerSymbol, SymbolsReal: Real;
    Symbols: Integer;
begin
  if Drawn and not (Percentage = FPrevious) then
    ClearChars(NeedSymbols + 2 + 5);
  if Percentage = FPrevious then begin
    Exit;
  end;
  FPrevious := Percentage;
  PercentPerSymbol := 100 / NeedSymbols;
  SymbolsReal := Percentage / PercentPerSymbol;
  Symbols := Round(SymbolsReal);
  WriteColour('[', OtherColour);
  WriteColour(DupeString(String(SymbolToDraw), Symbols), DoneColour);
  WriteColour(DupeString(String(SymbolToDraw), NeedSymbols - Symbols), InProcessColour);
  WriteColour('] ', OtherColour);
  WriteColour(Format('%d%%', [Percentage]), OtherColour);
  Drawn := True;
end;

end.
