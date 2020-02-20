unit HLVM.JIT.Execution.FSVariables;

interface

{ Unit to make simple storage for locals and globals }
{ Introduced in KadJIT }

uses
  HLVM.JIT.Context.Storage;

type
  TLocalVariable = record
  public var
    Name: String;
    Content: HLVM_Variable;
  end;
  PLocalVariable = ^TLocalVariable;
  TLocalScope = array of TLocalVariable;

type
  TGlobalVariable = record
  public var
    Name: String;
    Reference: PHLVM_Variable;
  end;
  PGlobalVariable = ^TGlobalVariable;
  TGlobalScope = array of TGlobalVariable;

implementation

end.
