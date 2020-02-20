unit HLVM.Types.Containers;

interface

uses
  HLVM.Types.Ordinal,
  HLVM.Static.Command,
  HLVM.Generics.Collections,
  System.Classes;

const
  HLVM_Infused_Abstract = 0;
  HLVM_Infused_None     = 1;
  HLVM_Infused_Integer  = 2;
  HLVM_Infused_Float    = 3;
  HLVM_Infused_String   = 4;

type
  THLVMCommandLetContainter_Abstract = class(THLVMCommonObject)
  private
    FCmdLetType: Byte;
    FCommand: HCommand;
    FPosition: THPosition;
  public
    property CmdLetType: Byte read FCmdLetType write FCmdLetType;
    property Command: HCommand read FCommand write FCommand;
    property Position: THPosition read FPosition write FPosition;

    constructor Create; virtual; abstract;
  end;

  { None }
  THLVMCommandLetContainer_None = class(THLVMCommandLetContainter_Abstract)
  public
    constructor Create; override;
  end;

  { Integer }
  THLVMCommandLetContainer_Integer = class(THLVMCommandLetContainter_Abstract)
  private
    FContent: HSInteger;
  public
    property Content: HSInteger read FContent write FContent;
    constructor Create; override;
  end;

  { Real }
  THLVMCommandLetContainer_Float = class(THLVMCommandLetContainter_Abstract)
  private
    FContent: HSFloat;
  public
    property Content: HSFloat read FContent write FContent;
    constructor Create; override;
  end;

  { String }
  THLVMCommandLetContainer_String = class(THLVMCommandLetContainter_Abstract)
  private
    FContent: String;
  public
    property Content: String read FContent write FContent;
    var HashOf: Integer;
    constructor Create; override;
  end;

  TUniversalContainer = class
  public var
    Command: HCommand;
    Position: THPosition;
  public var
    IntContent: Int64;
    StringContent: String;
    FloatContent: HSFloat;
  end;
  PUniversalContainer = ^TUniversalContainer;

type
//  THLVMCommandLetArray = TObjectList<THLVMCommandLetContainter_Abstract>;
  THLVMCommandLetArray = TObjectList<TUniversalContainer>;
  THLVMRawChunk = class(THLVMCommandLetArray)
    private FNameOfChunk: String;
    public property NameOfChunk: String read FNameOfChunk write FNameOfChunk;
  end;
  THLVMBytecodeChunk = class(THLVMCommandLetArray)
    private FNameOfChunk: String;
    public property NameOfChunk: String read FNameOfChunk write FNameOfChunk;
  end;

implementation

{ THLVMCommandLetContainer_String }

constructor THLVMCommandLetContainer_String.Create;
begin
  Self.CmdLetType := HLVM_Infused_String;
end;

{ THLVMCommandLetContainer_Float }

constructor THLVMCommandLetContainer_Float.Create;
begin
  Self.CmdLetType := HLVM_Infused_Float;
end;

{ THLVMCommandLetContainer_Integer }

constructor THLVMCommandLetContainer_Integer.Create;
begin
  Self.CmdLetType := HLVM_Infused_Integer;
end;

{ THLVMCommandLetContainer_None }

constructor THLVMCommandLetContainer_None.Create;
begin
  Self.CmdLetType := HLVM_Infused_None;
end;

end.


