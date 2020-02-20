unit HLVM.JIT.Types;

interface

{ Test module to provide types, records, reflections and classes }

uses
  HLVM.JIT.Context.Storage.Commons;

type
  HLVMTypeOperator = (
    toAdd,
    toAddR,
    toSubtract,
    toSubtractR,
    toMultiply,
    toMultiplyR,
    toDivide,
    toDivideR,

    toBitAnd,
    toBitAndR,
    toBitOr,
    toBitOrR
  );

type
  TTypeOperators = array[0 .. Int64(High(HLVMTypeOperator))] of Pointer;
  THLVMType = class(THLVMObject)
  public
    ID: Integer;
    Name: String;
    Scope: String;
    { Methods - ? }
    { Operators - ? }
  end;

implementation

end.
