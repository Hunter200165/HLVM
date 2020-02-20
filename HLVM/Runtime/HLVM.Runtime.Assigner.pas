unit HLVM.Runtime.Assigner;

interface

uses
  HLVM.Runtime.Types,
  HLVM.Types.Utilities;

type
  TAssigner = record
  public
    procedure AssignShells(var AFirstShell, ASecondShell: TVariableShell); inline;
  end;

implementation

{ TAssigner }

procedure TAssigner.AssignShells(var AFirstShell, ASecondShell: TVariableShell);
begin
  AFirstShell := ASecondShell;
end;

end.
