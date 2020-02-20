unit HLVM.Types.AoT;

interface

{ Ahead of Time compiled for HLVM_ASM bytecode to increase speed of execution }

uses
  HLVM.Types.Ordinal,
  HLVM.Types.Containers,
  HLVM.Static.Command,
  HLVM.Runtime.Hashes,
  HLVM.Generics.Collections,
  System.SysUtils,
  System.Hash;

type
  THLVM_AoTNumerationFunction = function: Integer of object;
  { Additional }
  THLVM_AoT_FunctionContainer = record
  private
    FCompiledBytecode: THLVMBytecodeChunk;
    FFunctionMark: Integer;
  public
    property CompiledBytecode: THLVMBytecodeChunk read FCompiledBytecode write FCompiledBytecode;
    property FunctionMark: Integer read FFunctionMark write FFunctionMark;
  end;
  THLVM_AoT_FunctionContainers = TList<THLVM_AoT_FunctionContainer>;

type
  { Exceptions }
  EHLVM_AoTCompilationException = class(EHLVMCommonException);

  { Result of compilation }
  THLVM_AoTCompiledBytecode = record
  private
    FFunctions: THLVM_AoT_FunctionContainers;
    FCompiledBytecode: THLVMBytecodeChunk;
  public
    property Functions: THLVM_AoT_FunctionContainers read FFunctions write FFunctions;
    property CompiledBytecode: THLVMBytecodeChunk read FCompiledBytecode write FCompiledBytecode;

    procedure TerminateAll;
  end;
  THLVM_AoTMap = TDictionary<Integer, Integer>;

  { Effectiveness' prince }
  THLVM_BytecodeAoTCompiler = class(THLVMCommonObject)
  private
    FFunctionMark: Integer;
    FAoTNumerationFunction: THLVM_AoTNumerationFunction;
    function GetFunctionMark: Integer;
    procedure SetNumerationMark(const Value: Integer);
  public
    { Mark of function in Function Stack }
    property AoTNumerationFunction: THLVM_AoTNumerationFunction read FAoTNumerationFunction write FAoTNumerationFunction;
    property FunctionMark: Integer read GetFunctionMark write SetNumerationMark;

    function AoTCompile(ARawBytecode: THLVMRawChunk): THLVM_AoTCompiledBytecode;
    procedure InternalCompileFunction(var Result: THLVM_AoTCompiledBytecode; const ARawBytecode: THLVMRawChunk);
  end;

implementation

{ THLVM_BytecodeAoTCompiler }

function THLVM_BytecodeAoTCompiler.AoTCompile(ARawBytecode: THLVMRawChunk): THLVM_AoTCompiledBytecode;
var Pos, InitPos, EndPos, Counter, i, Mark: Integer;
    PathMap: THLVM_AoTMap;
    Functionised: THLVMRawChunk;
//    Container: TUniversalContainer;
begin
  { AoT compilation }
  Result.Functions := THLVM_AoT_FunctionContainers.Create;
  PathMap := THLVM_AoTMap.Create;
  try
    Pos := 0;
    while (Pos < ARawBytecode.Count) do begin

      if (ARawBytecode[Pos].Command = Command_CreateFunction) then begin

        { Extract bytecode }
        InitPos := Pos + 1;
        Counter := 0;

        repeat

          Pos := Pos + 1;
          if (ARawBytecode[Pos].Command = Command_OpenScope) then begin
            Counter := Counter + 1;
          end
          else if (ARawBytecode[Pos].Command = Command_CloseScope) then begin
            Counter := Counter - 1;
          end;

        until not ((Pos < ARawBytecode.Count - 1) and (Counter > 0));

        if not (Counter = 0) then
          raise EHLVM_AoTCompilationException.Create('Malformed bytecode.');

        EndPos := Pos;

        Functionised := THLVMRawChunk.Create(False);
        Functionised.NameOfChunk := ARawBytecode.NameOfChunk;
        for i := InitPos to EndPos do
          Functionised.Add(ARawBytecode[i]);

        InternalCompileFunction(Result, Functionised);
        { Strip function from bytecode }
        ARawBytecode.DeleteRange(InitPos, EndPos - InitPos + 1);

        Pos := InitPos - 1;
//        ARawBytecode[Pos].Free;
//        ARawBytecode[Pos] := THLVMCommandLetContainer_Integer.Create;

//        FunctionMark := FunctionMark + 1;
//        FunctionMark := Result.Functions.Last.FunctionMark;
        Mark := Result.Functions.Last.FunctionMark;
//        ARawBytecode.Items[Pos].Command := Command_Internal_PushFunction;

//        Container.Command := Command_Internal_PushFunction;

//        with ({ARawBytecode[Pos]} Container) do begin
//          Command := Command_Internal_PushFunction;
//          {Content := FunctionMark; }
//          IntContent := Mark;
//        end;
          with ARawBytecode[Pos] do begin
            Command := Command_Internal_PushFunction;
            IntContent := Mark;
          end;

//        Container.Command := Command_Internal_PushFunction;
//        Container.IntContent := Mark;
//        ARawBytecode[Pos] := Container;

//        { If not a function, it will increment }
//        FunctionMark := FunctionMark + 1;

      end
      else
        Pos := Pos + 1;

    end;

    Pos := 0;
    while (Pos < ARawBytecode.Count) do begin

      if (ARawBytecode[Pos].Command = Command_LABEL) then begin

        { Add LABEL to Map }
//        PathMap.AddOrSetValue(Pos, Integer((ARawBytecode[Pos] as THLVMCommandLetContainer_Integer).Content));
        PathMap.AddOrSetValue(Integer((ARawBytecode[Pos]).IntContent), Pos);
        { Delete abstract label call }
        if not ARawBytecode.OwnsObjects then ARawBytecode[Pos].Free;
        ARawBytecode.Delete(Pos);

      end
      else if (ARawBytecode[Pos].Command = Command_CloseScope) and (ARawBytecode[Pos - 1].Command = Command_OpenScope) then begin
        ARawBytecode.Delete(Pos);
        ARawBytecode.Delete(Pos - 1);
        Pos := Pos - 1;
      end
      else
        Pos := Pos + 1;
    end;

    Pos := 0;
    while (Pos < ARawBytecode.Count) do begin

      case (ARawBytecode[Pos].Command) of
        Command_JMP, Command_JC, Command_JNC,
        Command_SetBreak, Command_SetContinue:
        begin
          Counter := Integer((ARawBytecode[Pos]).IntContent);
          if not PathMap.ContainsKey(Counter) then
            raise EHLVM_AoTCompilationException.Create('Malformed bytecode.');
//          Container := ARawBytecode[Pos];
//          Container.IntContent := PathMap[Counter];
//          ARawBytecode[Pos] := Container;
          (ARawBytecode[Pos]).IntContent := PathMap.Items[Counter];
        end;
        Command_PushVar, Command_PushLocal: begin
//          Container := ARawBytecode[Pos];
//          (ARawBytecode[Pos]).IntContent :=
////          Container.IntContent :=
//            PositiveMask and ((PositiveMask and THasher.HashString((ARawBytecode[Pos]).StringContent)) + 1);
//          ARawBytecode[Pos] := Container;
        end;
      end;

      Pos := Pos + 1;
    end;

    Result.CompiledBytecode := THLVMBytecodeChunk.Create(False);
    Result.CompiledBytecode.Count := ARawBytecode.Count;
    Result.CompiledBytecode.NameOfChunk := ARawBytecode.NameOfChunk;
    { Copy it! }
    for Pos := 0 to ARawBytecode.Count - 1 do begin
      //Result.CompiledBytecode[Pos] := ARawBytecode[Pos];
      Result.CompiledBytecode[Pos] := TUniversalContainer.Create;
      with Result.CompiledBytecode[Pos] do begin
        Command := ARawBytecode[Pos].Command;
        Position := ARawBytecode[Pos].Position;
        IntContent := ARawBytecode[Pos].IntContent;
        StringContent := ARawBytecode[Pos].StringContent;
        FloatContent := ARawBytecode[Pos].FloatContent;
      end;
    end;
  finally
    PathMap.Free;
  end;
end;

function THLVM_BytecodeAoTCompiler.GetFunctionMark: Integer;
begin
  if not (@FAoTNumerationFunction = nil) then begin
    Result := AoTNumerationFunction;
    FFunctionMark := Result;
  end
  else
    Result := FFunctionMark;
end;

procedure THLVM_BytecodeAoTCompiler.InternalCompileFunction(var Result: THLVM_AoTCompiledBytecode; const ARawBytecode: THLVMRawChunk);
var PathMap: THLVM_AoTMap;
    Pos, InitPos, EndPos, Counter, i, Mark: Integer;
    Functionised: THLVMRawChunk;
    Func: THLVM_AoT_FunctionContainer;
    Container: TUniversalContainer;
begin
  { AoT compilation }
  { Add the function. }
  FunctionMark := FunctionMark + 1;
  Func.FunctionMark := FunctionMark;

//  Func.CompiledBytecode.NameOfChunk := ARawBytecode.NameOfChunk;

  PathMap := THLVM_AoTMap.Create;
  try
    Pos := 0;
    while (Pos < ARawBytecode.Count) do begin

      if (ARawBytecode[Pos].Command = Command_CreateFunction) then begin

        { Extract bytecode }
        InitPos := Pos + 1;
        Counter := 0;

        repeat

          Pos := Pos + 1;
          if (ARawBytecode[Pos].Command = Command_OpenScope) then begin
            Counter := Counter + 1;
          end
          else if (ARawBytecode[Pos].Command = Command_CloseScope) then begin
            Counter := Counter - 1;
          end;

        until not ((Pos < ARawBytecode.Count - 1) and (Counter > 0));

        if not (Counter = 0) then
          raise EHLVM_AoTCompilationException.Create('Malformed bytecode.');

        EndPos := Pos;

        Functionised := THLVMRawChunk.Create(False);
        for i := InitPos to EndPos do
          Functionised.Add(ARawBytecode[i]);

        InternalCompileFunction(Result, Functionised);
        { Strip function from bytecode }
        ARawBytecode.DeleteRange(InitPos, EndPos - InitPos + 1);

        Pos := InitPos - 1;
//        ARawBytecode[Pos].Free;
//        ARawBytecode[Pos] := THLVMCommandLetContainer_Integer.Create;

//        FunctionMark := Result.Functions.Last.FunctionMark;
        Mark := Result.Functions.Last.FunctionMark;

//        with (ARawBytecode[Pos] as THLVMCommandLetContainer_Integer) do begin
//          Command := Command_Internal_PushFunction;
//          Content := Mark;
//        end;

        with ARawBytecode[Pos] do begin
          Command := Command_Internal_PushFunction;
          IntContent := Mark;
        end;

//        Container.Command := Command_Internal_PushFunction;
//        Container.IntContent := Mark;

      end
      else
        Pos := Pos + 1;

    end;

    Pos := 0;
    while (Pos < ARawBytecode.Count) do begin

      if (ARawBytecode[Pos].Command = Command_LABEL) then begin

        { Add LABEL to Map }
//        PathMap.AddOrSetValue(Pos, Integer((ARawBytecode[Pos] as THLVMCommandLetContainer_Integer).Content));
        PathMap.AddOrSetValue(Integer((ARawBytecode[Pos]).IntContent), Pos);
        { Delete abstract label call }
//        if not ARawBytecode.OwnsObjects then ARawBytecode[Pos].Free;
        ARawBytecode.Delete(Pos);

      end
      else
        Pos := Pos + 1;
    end;

    Pos := 0;
    while (Pos < ARawBytecode.Count) do begin

      case (ARawBytecode[Pos].Command) of
        Command_JMP, Command_JC, Command_JNC,
        Command_SetBreak, Command_SetContinue:
        begin
          Counter := Integer((ARawBytecode[Pos]).IntContent);
          if not PathMap.ContainsKey(Counter) then
            raise EHLVM_AoTCompilationException.Create('Malformed bytecode.');
          Container := ARawBytecode[Pos];
          Container.IntContent := PathMap[Counter];
          ARawBytecode[Pos] := Container;
//          (ARawBytecode[Pos] as THLVMCommandLetContainer_Integer).Content := PathMap.Items[Counter];
        end;
      end;

      Pos := Pos + 1;
    end;

//    Result.CompiledBytecode := THLVMBytecodeChunk.Create(True);
//    Result.CompiledBytecode.Count := ARawBytecode.Count;
    Func.CompiledBytecode := THLVMBytecodeChunk.Create(False);
    Func.CompiledBytecode.Count := ARawBytecode.Count;
    Func.CompiledBytecode.NameOfChunk := ARawBytecode.NameOfChunk;
    { Copy it! }
    for Pos := 0 to ARawBytecode.Count - 1 do begin
//      Func.CompiledBytecode[Pos] := ARawBytecode[Pos];
      Func.CompiledBytecode[Pos] := TUniversalContainer.Create;
      with Func.CompiledBytecode[Pos] do begin
        Command := ARawBytecode[Pos].Command;
        Position := ARawBytecode[Pos].Position;
        IntContent := ARawBytecode[Pos].IntContent;
        StringContent := ARawBytecode[Pos].StringContent;
        FloatContent := ARawBytecode[Pos].FloatContent;
      end;
    end;
    Result.Functions.Add(Func);
  finally
    PathMap.Free;
    ARawBytecode.Free;
  end;
end;

procedure THLVM_BytecodeAoTCompiler.SetNumerationMark(const Value: Integer);
begin
  if (@FAoTNumerationFunction = nil) then
    FFunctionMark := Value;
end;

{ THLVM_AoTCompiledBytecode }

procedure THLVM_AoTCompiledBytecode.TerminateAll;
var i: Integer;
begin
  CompiledBytecode.OwnsObjects := True;
  CompiledBytecode.Free;
  for i := 0 to Functions.Count - 1 do begin 
    Functions[i].CompiledBytecode.OwnsObjects := True;
    Functions[i].CompiledBytecode.Free;
  end;
  Functions.Free;
end;

end.

