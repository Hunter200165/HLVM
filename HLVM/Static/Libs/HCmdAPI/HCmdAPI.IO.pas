unit HCmdAPI.IO;

interface

type
  HCmd_TCustomIO = class(TObject)
  public
    procedure Write(const Msg: String); virtual; abstract;
    procedure Writeln(const Msg: String); virtual;
  end;

implementation

{ HCmd_TCustomIO }

procedure HCmd_TCustomIO.Writeln(const Msg: String);
begin
  Write(Msg + #13#10);
end;

end.
