unit HLVM.Types.Ordinal;

interface

uses
  HLVM.Generics.Collections,
  HLVM.Generics.HArray,
  HLVM.Static.Command,
  System.SysUtils;

type
  HSInteger = Int64;
  HSFloat = Extended;

type
  HCommand = Byte;
  HInteger = Integer;

type
  THPosition = record
  public var
    ScopeName: String;
    LineNumber: Integer;
    RowNumber: Integer;
  end;

type
  EHLVMCommonException = class(Exception);
  THLVMCommonObject = class(TObject);

type
  { Compiler's only type }
  THCVariable = record
  private
    FStartPosition: Integer;
    FFinishPosition: Integer;
  public
    property StartPosition: Integer read FStartPosition write FStartPosition;
    property FinishPosition: Integer read FFinishPosition write FFinishPosition;

    constructor Create(AStartPos, AFinishPos: Integer);
  end;
  THCVariables = HArray<THCVariable>;

implementation

{ THCVariable }

constructor THCVariable.Create(AStartPos, AFinishPos: Integer);
begin
  StartPosition := AStartPos;
  FinishPosition := AFinishPos;
end;

end.
