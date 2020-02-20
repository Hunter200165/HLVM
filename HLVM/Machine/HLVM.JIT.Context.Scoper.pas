unit HLVM.JIT.Context.Scoper;

interface

{ TScoper class to link all local variables and globals in way to make faster }
{ JIT Expansion, introduced in Alpha 2.0 }
{ Helper for Linker class and it is not recommended to use it in raw way }

uses
  System.SysUtils,
  { HLVM }
  HLVM.Static.Command,
  HLVM.JIT.Context.Linker,
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.CommandExpansion,
  HLVM.Generics.HList;

type
  TScoperVar = record
  public var
    Name: String;
    ID: Integer;
    Depth: Integer;
    { KadJIT Grand Update v1.5.3 }
    UpValue: Boolean;

    { If value is forwarded and Globally reference counted }
    IsProtected: Boolean;
  public 
    procedure Nullify;
  end;
  TScoperScope = HList<TScoperVar>;

type 
  TScoperGlobalVar = record 
  public var 
    Name: String;
  public 
    procedure Nullify;
  end;
  TScoperGlobalVars = HList<TScoperGlobalVar>;
  
type
  EScoperException = class(Exception);
  TScoper = record
  public var
    Scope: TScoperScope;
    Globals: TScoperGlobalVars;
    
    Depth: Integer;
    Size: Integer;
    IDCounter: Integer;
    CurrentScopeID: Integer;
    ScopeIDs: HList<Integer>;

    Linked: PHLVMBytecodeSentence;
  public 
    procedure Nullify;
  
    function FindLocal(const Name: String): Integer;
    function FindGlobal(const Name: String): Integer;

    function PushLocal(const Name: String): Integer;
    function PushGlobal(const Name: String): Integer;
  
    procedure DoScoping(const Sentence: THLVMBytecodeSentence);
    procedure ScopeOperands(var LWord: HLVM_LinkerWord);
  end;

implementation

{ TScoper }

procedure TScoper.DoScoping(const Sentence: THLVMBytecodeSentence);
var i, k, Pos, HeaderSize: Integer;
    LWord: PHLVM_LinkerWord;
    HeaderEntry: THLVMHeaderEntry;
    NewLinked: THLVMBytecodeSentence;
begin
  Nullify;
  Linked := @Sentence;
  Depth := 0;
  ScopeIDs.Add(0);
  CurrentScopeID := 0;

  for i := 0 to Sentence.ActualStorage.Count - 1 do begin
    Pos := Sentence.ActualStorage[i];
    LWord := @Sentence.InternalStorage.Items[Pos];

    case LWord.CommandID of
      Command_OpenScope: begin
        Inc(Depth);
        { Next ID }
        Inc(IDCounter);
        { Add it to IDs }
        ScopeIDs.Add(IDCounter);
        CurrentScopeID := IDCounter;

        { Link scopes }
        LWord.IntegerStorage := Depth;
        LWord.AdditionalData := IDCounter;

        { Continue loop execution }
        Continue;
      end;
      Command_CloseScope: begin
        Dec(Depth);
        if Depth < 0 then
          raise EScoperException.Create('Internal Scoper exception: Scopes mismatch!');
        { Link scopes }
        LWord.IntegerStorage := Depth;
        { Storing depth of next-previous scope }
        LWord.AdditionalData := ScopeIDs.LastItem;

        { Remove last state in IDs }
        ScopeIDs.Remove;
        CurrentScopeID := ScopeIDs.LastItem;
        { Continue loop execution }
        Continue;
      end;
    end;

    ScopeOperands(LWord^);
  end;

  HeaderSize :=
    { SetLocalHeapSize } 1 +
    { SetGlobalHeapSize } 1 +
    { All globals } + Globals.Count;

  NewLinked.InternalStorage.ResizeNullify(Linked.InternalStorage.Count + HeaderSize);
  NewLinked.ActualStorage.ResizeNullify(Linked.ActualStorage.Count + HeaderSize);

  NewLinked.InternalStorage.Items[0] := HLVM_LinkerWord.Create(JIT_Command_Heading_SetLocalHeapSize);
  NewLinked.InternalStorage.Items[0].IntegerStorage := Size;
  NewLinked.ActualStorage[0] := 0;

  NewLinked.InternalStorage.Items[1] := HLVM_LinkerWord.Create(JIT_Command_Heading_SetGlobalHeapSize);
  NewLinked.InternalStorage.Items[1].IntegerStorage := Globals.Count;
  NewLinked.ActualStorage[1] := 1;

  for i := 0 to Globals.Count - 1 do begin
    NewLinked.InternalStorage.Items[i + 2] := HLVM_LinkerWord.Create(JIT_Command_Heading_CacheGlobal);
    NewLinked.InternalStorage.Items[i + 2].IntegerStorage := i;
    NewLinked.InternalStorage.Items[i + 2].StringStorage := Globals[i].Name;
    NewLinked.ActualStorage[i + 2] := i + 2;
  end;

  for i := HeaderSize to Linked.InternalStorage.Count + HeaderSize - 1 do begin
    NewLinked.InternalStorage[i] := Linked.InternalStorage[i - HeaderSize];
    if i < Linked.ActualStorage.Count + HeaderSize then
      NewLinked.ActualStorage[i] := Linked.ActualStorage[i - HeaderSize] + HeaderSize;
    for k := 0 to NewLinked.InternalStorage[i].AbstractOperands.Count - 1 do
      Inc(NewLinked.InternalStorage.Items[i].AbstractOperands.Items[k], HeaderSize);
  end;

  Linked^ := NewLinked;

  (*
  { Write header }
  with HeaderEntry do begin
    Nullify;

    Code := JIT_Header_LocalHeapSize;
    IntStorage := Size;
  end;
  Linked.Header.Add(HeaderEntry);

  with HeaderEntry do begin
    Nullify;

    Code := JIT_Header_GlobalCacheSize;
    IntStorage := Globals.Count;
  end;
  Linked.Header.Add(HeaderEntry);

  for i := 0 to Globals.Count - 1 do begin
    with HeaderEntry do begin
      Nullify;

      Code := JIT_Header_CacheGlobal;
      StringStorage := Globals[i].Name;
      IntStorage := i;
    end;
    Linked.Header.Add(HeaderEntry);
  end;
  *)
end;

function TScoper.FindGlobal(const Name: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Globals.Count - 1 do
    if Globals[i].Name = Name then begin
      Result := i;
      Break;
    end;
end;

function TScoper.FindLocal(const Name: String): Integer;
var i, k, Dep: Integer;
    SVar: TScoperVar;
begin
  { We are altering speed of execution to safety of execution }
  Result := -1;
  Dep := -1;
  for i := Scope.Count - 1 downto 0 do begin 
    SVar := Scope[i];
    if not SVar.IsProtected and (SVar.Name = Name) and (SVar.Depth <= Depth) then begin
      for k := ScopeIDs.Count - 1 downto 0 do
        if ScopeIDs[k] = SVar.ID then begin
          if Dep < SVar.Depth then begin
            Dep := SVar.Depth;
            Result := i;
            if Dep = Depth then
              { Highest possible, so exit }
              Exit;
          end;
          Break;
        end;
    end
    else if SVar.IsProtected then
      raise EScoperException.Create('GlobalRC is not supported by scoper yet!');
  end;
end;

procedure TScoper.Nullify;
begin
  Depth := 0;
  Size := 0;
  IDCounter := 0;

  Scope.Nullify;
  Globals.Nullify;
  ScopeIDs.Nullify;
end;

function TScoper.PushGlobal(const Name: String): Integer;
var Global: TScoperGlobalVar;
begin
  { Global variables cannot be overriden and they do not have depth of scope }
  { Therefore pushing is simply adding them to List }
  Global.Nullify;
  Global.Name := Name;

  Globals.Add(Global);
  Result := Globals.LastIndex;
end;

function TScoper.PushLocal(const Name: String): Integer;
var Local, SVar: TScoperVar;
    Pos, i, k: Integer;
    Check: Boolean;
begin
  Local.Nullify;
  Local.Name := Name;

//  IDCounter := IDCounter + 1;
  Local.ID := CurrentScopeID;
  Local.Depth := Depth;
  Local.IsProtected := False;

  Pos := -1;
  for i := Scope.Count - 1 downto 0 do begin
    SVar := Scope[i];
    if not SVar.IsProtected and (SVar.Depth <= Depth) then begin
      Check := True;
      for k := ScopeIDs.Count - 1 downto 0 do
        if ScopeIDs[k] = SVar.ID then begin 
          Check := False;
          Break;
        end;
      { No information about scope ID variable is currently in }
      if Check then begin 
        Pos := i;
        Break;
      end
      else if SVar.Name = Name then begin 
        { 
          If scope is not freed currently, there can be collision:

            begin
              local A := N;
              local B := A;

              -- It is collision, and although scope is holding previous `A`, we must override it 
              local A := B + 1;
            end;
        }
        Pos := i;
        Break;
      end;
    end
    else if SVar.IsProtected then
      raise EScoperException.Create('GlobalRC is not supported by scoper yet!')
    else begin 
      { Depth is invalid on current variable }
      Pos := i;
      Break;
    end;
  end;

  if Pos >= 0 then begin 
    { Found good place for substitution }
    Scope[Pos] := Local;
    Result := Pos;
  end 
  else begin
    { All places are used currently; Adding new }
    Scope.Add(Local);
    Result := Scope.LastIndex;
    { Moreover, increasing size of maximum count of locals }
    Inc(Self.Size);  
  end;
end;

procedure TScoper.ScopeOperands(var LWord: HLVM_LinkerWord);
var i, Pos, VPos: Integer;
    OP: PHLVM_LinkerWord;
begin
  for i := 0 to LWord.AbstractOperands.Count - 1 do begin
    Pos := LWord.AbstractOperands[i];
    OP := @Linked.InternalStorage.Items[Pos];
    ScopeOperands(OP^);
  end;
  { And optimise command itself }
  OP := @LWord;
  case OP.CommandID of
    Command_PushVar: 
      case OP.AdditionalData and 1 of
        0: begin
          { Not forced }
          VPos := FindLocal(OP.StringStorage);
          if VPos >= 0 then begin 
            OP.CommandID := Command_PushVar;
            OP.AdditionalData := ScoperFlag_Var_PushKnownLocal;
            OP.AdditionalPlusData := VPos;
          end
          else begin
            VPos := FindGlobal(OP.StringStorage);
            if VPos >= 0 then begin 
              OP.CommandID := Command_PushVar;
              OP.AdditionalData := ScoperFlag_Var_PushKnownGlobal;
              OP.AdditionalPlusData := VPos;
            end
            else begin 
              { Very unoptimised variant. }
              { For example:
                  local A := n; 
                  local B := n; 
                Will make 2 raw requests to global variables;
                But better is:
                  uses global var n;
                  local A := n;
                  local B := n;
              }
              OP.CommandID := Command_PushVar;
              OP.AdditionalData := ScoperFlag_Var_PushRawGlobal;
              OP.AdditionalPlusData := 0;
            end;
          end;
        end;
        1: begin
          { Forced }
          VPos := FindLocal(OP.StringStorage);
          if VPos >= 0 then begin 
            { Ambiguous, in way we try to push local ONLY if we have this local }
            OP.CommandID := Command_PushVar;
            OP.AdditionalData := ScoperFlag_Var_PushKnownLocal;
            OP.AdditionalPlusData := VPos;
          end
          else begin
            VPos := FindGlobal(OP.StringStorage);
            if VPos >= 0 then begin 
              OP.CommandID := Command_PushVar;
              OP.AdditionalData := ScoperFlag_Var_PushKnownGlobal;
              OP.AdditionalPlusData := VPos;
            end
            else begin 
              { We know, that there will never be UndefinedNil value, so cache, Luke! }
              VPos := PushGlobal(OP.StringStorage);
              OP.CommandID := Command_PushVar;
              OP.AdditionalData := ScoperFlag_Var_PushKnownGlobal;
              OP.AdditionalPlusData := VPos;
            end;
          end;
        end;
      end;
    Command_PushLocal: begin
      { No matter, only used for forced: `local A;` -> `PushLocal 'A';` }
      VPos := PushLocal(OP.StringStorage);
      OP.CommandID := Command_PushVar;
      OP.AdditionalData := ScoperFlag_Var_PushKnownLocal or 1;
      OP.AdditionalPlusData := VPos;
    end;
    Command_ForSetVar: begin
      { Force local }
      VPos := PushLocal(OP.StringStorage);
      OP.CommandID := Command_ForSetVar;
      OP.AdditionalData := ScoperFlag_Var_PushKnownLocal;
      OP.AdditionalPlusData := VPos;
    end;
  end;
end;

{ TScoperVar }

procedure TScoperVar.Nullify;
begin
  Name := '';
  ID := 0;
  Depth := 0;
  IsProtected := False;
end;

{ TScoperGlobalVar }

procedure TScoperGlobalVar.Nullify;
begin
  Name := '';
end;

end.
