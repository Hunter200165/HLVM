unit HLVM.Types.Reader;

interface

uses
  HLVM.Types.Ordinal,
  HLVM.Static.Command,
  HLVM.Types.AbstractReader,
  HLVM.Types.Containers,
  HLVM.Runtime.Hashes,
  System.SysUtils,
  System.Hash;

type
  EBytecodeReaderException = class(EHLVMCommonException);
  THLVM_BytecodeReader = class(THAbstractReader)
  private
    FVersionOfBytecode: Integer;
    FCheckForVersionCompatibility: Boolean;
  public
    property VersionOfBytecode: Integer read FVersionOfBytecode write FVersionOfBytecode;
    property CheckForVersionCompatibility: Boolean read FCheckForVersionCompatibility write FCheckForVersionCompatibility;

    function RetrieveBytecode: THLVMRawChunk;
    procedure RetrieveBytecodeReady(var Chunk: THLVMRawChunk);
  end;

implementation

{ THLVM_BytecodeReader }

const
  PositiveMask = not Integer($80000000);

{ THLVM_BytecodeReader }

function THLVM_BytecodeReader.RetrieveBytecode: THLVMRawChunk;
begin
  Result := THLVMRawChunk.Create();
  Self.RetrieveBytecodeReady(Result);
end;

procedure THLVM_BytecodeReader.RetrieveBytecodeReady(var Chunk: THLVMRawChunk);
var CommandLet: {THLVMCommandLetContainter_Abstract} TUniversalContainer;
    Command: HCommand;
    Position: THPosition;
begin
  { Chunk is ready to write into }
  Chunk.NameOfChunk := ReadString;
  Chunk.Clear;
  while not EndOfStream do begin

    Position := ReadPosition;
    Command := ReadCommand;

    case Command of

      { Void commands }
      Command_ClearStack,
      Command_MoveStackPointer,
      Command_RestoreStackPointer,
      Command_Index,
      Command_PushNil,
      Command_PushTrue,
      Command_PushFalse,
      Command_ArADD, Command_ArSUB,
      Command_ArMUL, Command_ArDIV,
      Command_LgAND, Command_BtAND,
      Command_LgOR,  Command_BtOR,
      Command_LgXOR, Command_BtXOR,
      Command_LgNOT, Command_BtNOT,
      Command_CheckStack,
      Command_Call,
      Command_SimpleCall,
      Command_GetLength,
      Command_DoInterpolation,
      Command_OperatorIs,
      Command_OperatorAs,
      Command_Equals,
      Command_GreaterThan,
      Command_GreaterEqual,
      Command_PushLongArguments,
      Command_PushShortArguments,
      Command_Assign,
      Command_UnaryMinus,
      Command_CreateTable,
      Command_CallAsync,
      Command_Duplicate,
      Command_OpenScope,
      Command_CloseScope,
      Command_BreakLoop,
      Command_ContinueLoop,
      Command_CheckStackInvisible,
      Command_StrongAssign,
      Command_ConsumeArguments,
      Command_Return,
      Command_IsIn,
      Command_CreateRunTimeTable,
      Command_CreateFunction,
      Command_AssignPointer,
      Command_ForSetStart,
      Command_ForSetFinish,
      Command_ForCheck,
      Command_NewIndex,
      Command_MarkAsNative,
      Command_DoMonumentalPointer:
      begin

//        CommandLet := THLVMCommandLetContainer_None.Create;
        CommandLet := TUniversalContainer.Create;
        CommandLet.Command := Command;
        CommandLet.Position := Position;
//        CommandLet.Position.ScopeName := Chunk.NameOfChunk;

      end;

      { Integer commands }

      Command_LABEL,
      Command_JMP,
      Command_JC,
      Command_JNC,
      Command_SetBreak,
      Command_SetContinue,
      Command_RestrictType,
      Command_ForSetType,
      Command_ForSetStep:
      begin

//        CommandLet := THLVMCommandLetContainer_Integer.Create;
        CommandLet := TUniversalContainer.Create;
        CommandLet.Command := Command;
        CommandLet.Position := Position;
        CommandLet.IntContent := ReadInteger;

      end;

      { Int64 commands }
      Command_PushInteger:
      begin

//        CommandLet := THLVMCommandLetContainer_Integer.Create;
        CommandLet := TUniversalContainer.Create;
        CommandLet.Command := Command;
        CommandLet.Position := Position;
        CommandLet.IntContent := ReadIntegerValue;

      end;

      { String commands }
      Command_PushVar,
      Command_PushString,
      Command_PushLocal,
      Command_MarkAsLoopVar,
      Command_AssignLocalReference,
      Command_ForSetVar:
      begin

//        CommandLet := THLVMCommandLetContainer_String.Create;
        CommandLet := TUniversalContainer.Create;
        CommandLet.Command := Command;
        CommandLet.Position := Position;
        CommandLet.StringContent := ReadString;

        CommandLet.IntContent :=
//          Container.IntContent :=
            PositiveMask and ((PositiveMask and THasher.HashString(CommandLet.StringContent)) + 1);

      end;

      { Real (values) commands }
      Command_PushReal: begin

//        CommandLet := THLVMCommandLetContainer_Float.Create;
        CommandLet := TUniversalContainer.Create;
        CommandLet.Command := Command;
        CommandLet.Position := Position;
        CommandLet.FloatContent := ReadRealValue;

      end;

    else

      raise EBytecodeReaderException.Create('Malformed bytecode: Could not resolve command: ' + Integer(Command).ToString);

    end;

    Chunk.Add(CommandLet);

  end;
end;

end.
