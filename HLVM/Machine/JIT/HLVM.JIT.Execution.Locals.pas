unit HLVM.JIT.Execution.Locals;

interface

{ All we need to know about Scopes and Locals }

uses
  HLVM.JIT.Context.Storage,
  HLVM.JIT.Context.Storage.Commons,
  HLVM.JIT.Constants,
  HLVM.Generics.HList;

type
  ELocalHeapException = class(EMachineException);

type
  TLocalVariable = record
  public
    Name: String;
    Hash: Cardinal;
    ScopeID: Integer;
    Value: HLVM_Variable;
  end;
  PLocalVariable = ^TLocalVariable;

type
  //TLocalVariableList = HList<TLocalVariable>;
  TLocalVariableList = array of TLocalVariable;
  TLocalVariablePointersList = HList<PLocalVariable>;
  TLocalVariablesRegister = record
  public var
    List: TLocalVariableList;
    ID: Integer;
    Size: Cardinal;
  public
    function IsFree(const Position: Integer): Boolean; inline;
    procedure RehashExpand;

    /// <summary>
    ///  Searches for given parameters through the List of variables. <br />
    ///  In best case it is O(1), in worst - O(n)
    /// </summary>
    /// <returns>
    ///  Integer : Position of given parameters of -1, if nothing was found
    /// </returns>
    function PositionOf(const Hash: Cardinal; const Name: String): Integer;
    function PositionForWithoutExpand(const Hash: Cardinal; const Name: String): Integer; inline;
    function PositionFor(const Hash: Cardinal; const Name: String): Integer;
  public
    procedure Nullify;
  end;

implementation

{ TLocalVariablesRegister }

function TLocalVariablesRegister.IsFree(const Position: Integer): Boolean;
begin
  Result := not (List[Position].ScopeID = ID);
end;

procedure TLocalVariablesRegister.Nullify;
begin
  { Reset array for proper usage. }
  SetLength(List, 0);
  SetLength(List, 1);
  Size := 1;
end;

function TLocalVariablesRegister.PositionFor(const Hash: Cardinal; const Name: String): Integer;
begin
  Result := PositionForWithoutExpand(Hash, Name);
  if Result < 0 then begin 
    RehashExpand;
    Result := PositionForWithoutExpand(Hash, Name);
    if Result < 0 then
      raise ELocalHeapException.Create('Local Heap stack critical exception: Could not allocate free space for new local variable.');
  end;
end;

function TLocalVariablesRegister.PositionForWithoutExpand(const Hash: Cardinal; const Name: String): Integer;
var i, Pos, ID: Integer;
    PVar: PLocalVariable;
begin
  Result := -1;
  Pos := Hash mod Cardinal(Size);
  ID := Self.ID;
  for i := Pos to Size - 1 do begin 
    PVar := @List[i];
    if not (PVar.ScopeID = ID) then begin
      { Occupy by ID }
      PVar.ScopeID := ID;
      PVar.Name := Name;
      PVar.Hash := Hash;
      Result := i;
      Exit;
    end
    else if (PVar.Hash = Hash) and (PVar.Name = Name) then begin 
      Result := i;
      Exit;
    end;
  end;
  { From the beginning }
  for i := 0 to Pos - 1 do begin 
    PVar := @List[i];
    if not (PVar.ScopeID = ID) then begin
      PVar.ScopeID := ID;
      Result := i;
      PVar.Name := Name;
      PVar.Hash := Hash;
      Exit;
    end
    else if (PVar.Hash = Hash) and (PVar.Name = Name) then begin 
      Result := i;
      Exit;
    end;
  end;
end;

function TLocalVariablesRegister.PositionOf(const Hash: Cardinal; const Name: String): Integer;
var Pos: Integer;
    i, ID: Integer;
    PVar: PLocalVariable;
begin
  { Lookup in best case will be O(1), in worst - O(n) }
  ID := Self.ID;
  Pos := Hash mod Size;
  Result := -1;
  for i := Pos to Size - 1 do begin
    PVar := @List[i];
    if (PVar.ScopeID = ID) and (PVar.Hash = Hash) and (PVar.Name = Name) then begin
      Result := i;
      Exit;
    end;
    { We met the blank space for variable, means nothing can be futher. }
    if not (PVar.ScopeID = ID) then
      Exit;
  end;
  for i := 0 to Pos - 1 do begin
    PVar := @List[i];
    if (PVar.ScopeID = ID) and (PVar.Hash = Hash) and (PVar.Name = Name) then begin
      Result := i;
      Exit;
    end;
    { The same thing }
    if not (PVar.ScopeID = ID) then
      Exit;
  end;
end;

procedure TLocalVariablesRegister.RehashExpand;
var TempSize, i, k, Pos, ID: Integer;
    VarList: TLocalVariableList;
    PVar, Another: PLocalVariable;
    Check: Boolean;
begin
  { Always does the *2 on size }
  if Size >= HLVM_MAX_LOCAL_VARIABLES then
    raise ELocalHeapException.Create('Local Heap overflow!');
  if Size = 0 then
    TempSize := 1
  else
    TempSize := Size * 2;
  ID := Self.ID;
  SetLength(VarList, TempSize);

  { Rehash routine }
  for i := 0 to Size - 1 do begin
    PVar := @List[i];
    if not (PVar.ScopeID = ID) then
      Continue;
    Check := False;
    Pos := PVar.Hash mod Cardinal(TempSize);
    for k := Pos to TempSize - 1 do begin
      Another := @VarList[k];
      if not (Another.ScopeID = ID) then begin
        Another^ := PVar^;
        Check := True;
        Break;
      end;
    end;
    if Check then
      Continue;
    for k := 0 to Pos - 1 do begin
      Another := @VarList[k];
      if not (Another.ScopeID = ID) then begin
        Another^ := PVar^;
        Check := True;
        Break;
      end;
    end;
    if not Check then
      { It can be there if someone forgot, that scope ID should never be 0! }
      raise ELocalHeapException.Create('Local Heap has a critical allocation error');
  end;

  List := VarList;
  Size := TempSize;
end;

end.
