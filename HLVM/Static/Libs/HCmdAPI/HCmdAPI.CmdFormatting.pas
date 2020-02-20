unit HCmdAPI.CmdFormatting;

interface

uses
  System.SysUtils,
  HCmdAPI.CmdIO;

type
  HCmd_FormattingArgument = record
  private
    FQueryString: String;
    FSettingPart: String;
    FArgumentPart: String;
  public
    property QueryString: String read FQueryString write FQueryString;
    property SettingPart: String read FSettingPart write FSettingPart;
    property ArgumentPart: String read FArgumentPart write FArgumentPart;
  end;

  HCmd_FormattingResult = record
  private
    FSuccess: Boolean;
    FData: String;
  public
    property Success: Boolean read FSuccess write FSuccess;
    property Data: String read FData write FData;
  end;

  HCmd_TCmdFormatter = record
  private
    FIO: HCmd_TCmdAdvancedIO;
  public
    property IO: HCmd_TCmdAdvancedIO read FIO write FIO;

    function ProcessOption(const Args: HCmd_FormattingArgument): HCmd_FormattingResult;
    procedure ProcessString(const FmtString: String);

    class function CreateStd: HCmd_TCmdFormatter; static;
  end;

var CmdFormatter: HCmd_TCmdFormatter;
implementation

{ HCmd_TCmdFormatter }

class function HCmd_TCmdFormatter.CreateStd: HCmd_TCmdFormatter;
begin
  Result.IO := CmdIO;
end;

function HCmd_TCmdFormatter.ProcessOption(const Args: HCmd_FormattingArgument): HCmd_FormattingResult;
begin
  Result.Success := False;
  Result.Data := '';
  if Args.FSettingPart = 'n' then begin 
    { New line }
    Result.Success := True;
    Result.Data := #13#10;
  end
  else if Args.SettingPart = 'r' then begin
    { Default colour }
    Result.Success := True;
    Result.Data := '';
    try
      IO.SetDefaultColour;
    except
      on E: Exception do Result.Success := False;
    end;
  end
  else if Args.SettingPart = '\' then begin
    Result.Success := True;
    Result.Data := '\';
  end
  else if Args.SettingPart = 'c' then begin 
    Result.Success := Args.ArgumentPart <> '';
    Result.Data := '';
    if Result.Success then begin 
      try
        IO.SetColour_RGBH(Args.ArgumentPart);
      except 
        on E: Exception do begin 
          Result.Success := False;
        end;
      end;
    end;
  end
  else if Args.SettingPart = 't' then begin 
    Result.Success := True;
    Result.Data := #9;
  end
  else if Args.SettingPart = '[' then begin 
    Result.Success := True;
    Result.Data := '[';
  end;
end;

procedure HCmd_TCmdFormatter.ProcessString(const FmtString: String);
var Special, Captured, Argumented: Boolean;
    Capture, Argument, ArgsPr, CharDeleted: String;
    i: Integer;
    Args: HCmd_FormattingArgument;
    Res: HCmd_FormattingResult;
begin
  Special := False;
  Captured := False;
  Argumented := False;
  for i := 1 to Length(FmtString) do begin
    if Special and not Captured then begin
      Capture := Capture + FmtString[i];
      Args.SettingPart := FmtString[i];
      Args.QueryString := Capture;
      Captured := True;
      Argumented := False;
      Argument := '';
      ArgsPr := '';
    end
    else if Captured then begin
      if not Argumented then begin
        if (FmtString[i] = '[') then begin
          Argumented := True;
          ArgsPr := ArgsPr + '[%s';
          CharDeleted := '';
        end
        else begin
          Argumented := False;
          CharDeleted := FmtString[i];
          Argument := '';
        end;
      end
      else begin
        if not (FmtString[i] = ']') then begin
          Argument := Argument + FmtString[i];
        end
        else begin
          Argumented := False;
          ArgsPr := ArgsPr + ']';
        end;
      end;
      if not Argumented then begin
        { Found (or not) argumented part of option }
        { \c[1101]Hello in red! }
        Args.ArgumentPart := Argument;
        Res := ProcessOption(Args);
        if not Res.Success then begin 
          IO.Write(Capture + Format(ArgsPr, [Argument]));
        end
        else begin
          IO.Write(Res.Data);
        end;

        Special := False;
        Argumented := False;
        Captured := False;

        if CharDeleted = '\' then begin 
          Special := True;
          Capture := '\';
        end
        else 
          IO.Write(CharDeleted);
        
        CharDeleted := '';
      end;
    end
    else begin 
      if FmtString[i] = '\' then begin
        Special := True;
        Capture := '\';
      end
      else 
        IO.Write(FmtString[i]);
    end;
  end;
end;

begin
  CmdFormatter := HCmd_TCmdFormatter.CreateStd;
end.
