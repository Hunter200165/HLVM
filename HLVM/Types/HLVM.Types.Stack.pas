unit HLVM.Types.Stack;

interface

uses
  HLVM.Memory.Space,
  HLVM.Runtime.Types,
  HLVM.Generics.Collections;

type
  TStackContainer = record
  public var
    VariableShell: TVariableShell;
    TempShell: TVariableShell;
    ReferencedShell: PVariableShell;
    IsReferenced: Boolean;
    IsConstant: Boolean;
  public
    function GetShell: PVariableShell; inline;
  public
    class function Create(const AVariableShell: TVariableShell; const AConstant: Boolean = False): TStackContainer; overload; static; //inline;
    class function Create(const AVariableShell: PVariableShell; const AConstant: Boolean = False): TStackContainer; overload; static; //inline;
  end;

type
  TMemoryStack = TMemoryArray<TStackContainer>;
  TStackPointers = TMemoryArray<Integer>;
  TStack = record
  public var
    MemoryStack: TMemoryStack;
    StackPointers: TStackPointers;
  public var
    CurrentStackPointer: Integer;
  public
    function GetNil: TVariableShell; inline;
    function GetSafeCount: Integer; inline;
  public
    procedure MoveStackPointer; inline;
    procedure ReturnStackPointer; inline;
  public
    function Pick: TStackContainer; inline;
    procedure Push(const AVariableShell: TVariableShell); overload; inline;
    procedure Push(const AVariableShell: PVariableShell); overload; inline;
  end;

  TFastStack<T> = record
  public type
    TInternalArray = array of T;
    PContainer = ^T;
  public var
    Stack: TInternalArray;
    SP: PContainer;
    Top: PContainer;
    Base: PContainer;
    Size: Integer;
  public
    procedure Grow; inline;
    procedure Push(const AContainer: T); inline;
    function Pick: T; inline;
  public
    procedure Init; inline;
  end;
  TFastPointersStack = TFastStack<Integer>;
  TFastManagedStack<T> = record
  public type
    TInternalArray = array of T;
    PContainer = ^T;
  public var
    SPs: TFastPointersStack;

    Stack: TInternalArray;
    SP: PContainer;
    LowBound: PContainer;
    Base: PContainer;
    Top: PContainer;
    Size: Integer;
  public
    procedure Grow; //inline;
    function NextSP: PContainer; //inline;
    function NextSPPost: PContainer;// inline;
    procedure Push(const AContainer: T); //inline;
    function Pick: T; //inline;
    function PickAddress: PContainer;// inline;

    function IsEmpty: Boolean; //inline;

    procedure MoveStackPointer; //inline;
    procedure RestoreStackPointer; //inline;
  public
    procedure Init; inline;
  end;

//type
//  TFastStack = class(TStack<TStackContainer>)
//  end;

function GetNil: TVariableShell; inline;
function UnpackShell(const VariableShell: PVariableShell): PVariableShell; inline;

implementation

{ TStack }

uses
  HLVM.Runtime.RuntimeMonster;

function GetNil: TVariableShell; inline;
begin
  with Result do begin
    TypeOf := Type_Nil;
    Content := nil;
    Links := 0;
    Referenced := nil;
  end;
end;

function UnpackShell(const VariableShell: PVariableShell): PVariableShell;
begin
  Result := VariableShell;
  while not (Result.Referenced = nil) and not (Result = PVariableShell(Result.Referenced)) do begin
    Result := PVariableShell(Result.Referenced);
  end;
end;

function TStack.GetNil: TVariableShell;
begin
  with Result do begin
    TypeOf := Type_Nil;
    Content := nil;
    Links := 0;
  end;
end;

function TStack.GetSafeCount: Integer;
begin
  Result := MemoryStack.Count - (CurrentStackPointer + 1);
end;

procedure TStack.MoveStackPointer;
begin
  StackPointers.Add(CurrentStackPointer);
  CurrentStackPointer := MemoryStack.Count - 1;
end;

function TStack.Pick: TStackContainer;
begin
  if GetSafeCount > 0 then
    Result := MemoryStack.Pick
  else
    Result := TStackContainer.Create(GetNil);
end;

procedure TStack.Push(const AVariableShell: TVariableShell);
begin
  MemoryStack.Add(TStackContainer.Create(AVariableShell));
end;

procedure TStack.Push(const AVariableShell: PVariableShell);
begin
  MemoryStack.Add(TStackContainer.Create(AVariableShell));
end;

procedure TStack.ReturnStackPointer;
begin
  if StackPointers.Count > 0 then begin
    CurrentStackPointer := StackPointers.Pick;
  end
  else
    CurrentStackPointer := -1;
end;

{ TStackContainer }

class function TStackContainer.Create(const AVariableShell: PVariableShell; const AConstant: Boolean): TStackContainer;
begin
  with Result do begin
    ReferencedShell := AVariableShell;
    IsReferenced := True;
    IsConstant := AConstant;
//    while not (ReferencedShell.Referenced = nil) do
//      ReferencedShell := PVariableShell(ReferencedShell.Referenced);
  end;
end;

class function TStackContainer.Create(const AVariableShell: TVariableShell; const AConstant: Boolean): TStackContainer;
begin
  if not AConstant and not (AVariableShell.Referenced = nil) then begin
    Result := Create(PVariableShell(AVariableShell.Referenced), False);
    Exit;
  end;
  with Result do begin
    VariableShell := AVariableShell;
    IsReferenced := False;
    IsConstant := AConstant;
  end;
end;

function TStackContainer.GetShell: PVariableShell;
begin
  case IsConstant of
    False: begin
      if IsReferenced then
        Result := UnpackShell(ReferencedShell)
      else
        Result := UnpackShell(PVariableShell(@VariableShell));
      case Result.TypeOf of
        Type_BuiltInField: begin
          with PHBuiltInField(Result.Content)^ do
            TempShell := TRuntimeMonster.GetFieldValue(PHBuiltInObject(ParentObject).LinkedMemoryManager, PHBuiltInObject(ParentObject).Content, Content);
          Result := @TempShell;
        end;
        Type_BuiltInProperty: begin
          with PHBuiltInProperty(Result.Content)^ do
            TempShell := TRuntimeMonster.GetPropertyValue(PHBuiltInObject(ParentObject).LinkedMemoryManager, PHBuiltInObject(ParentObject).Content, Content);
          Result := @TempShell;
        end;
      end;
    end;
    True: if IsReferenced then
        Result := ReferencedShell
      else
        Result := PVariableShell(@VariableShell);
  end;
end;

{ TAnotherStack }

procedure TFastStack<T>.Grow;
var A: Integer;
begin
  { Grow is done ONLY when SP is on top }
  A := Size;
  Inc(Size, 16);
  SetLength(Stack, Size);
  Top := @Stack[Size - 1];
  SP := @Stack[A];
  Base := @Stack[0];
end;

procedure TFastStack<T>.Init;
begin
  SP := nil;
  Top := nil;
  Base := nil;
  Size := 0;
end;

function TFastStack<T>.Pick: T;
begin
  Result := SP^;
//  if Integer(SP) > Integer(Base) then
  Dec(SP);
end;

procedure TFastStack<T>.Push(const AContainer: T);
begin
  if SP = Top then
    Grow
  else
    Inc(SP);
  SP^ := AContainer;
end;

{ TFastManagedStack<T> }

procedure TFastManagedStack<T>.Grow;
var A, B: Integer;
begin
  { Grow is done ONLY when SP is on top }
  A := Size;
  if Size = 0 then
    B := 0
  else
    B := Integer(LowBound) - Integer(@Stack[0]);
  Inc(Size, 16);
  SetLength(Stack, Size);
  Top := @Stack[Size - 1];
  SP := @Stack[A];
  Base := @Stack[0];
//  if LowBound = nil then LowBound := @Stack[0];
  LowBound := PContainer(Integer(Base) + B);
end;

procedure TFastManagedStack<T>.Init;
begin
  SP := nil;
  Top := nil;
  LowBound := nil;
  Size := 0;
  SPs.Init;
end;

function TFastManagedStack<T>.IsEmpty: Boolean;
begin
  Result := Integer(SP) < Integer(LowBound);
end;

procedure TFastManagedStack<T>.MoveStackPointer;
begin
  if Size = 0 then
    SPs.Push(0)
  else
    SPs.Push(Integer(LowBound) - Integer(Base) {+ SizeOf(T)});
end;

function TFastManagedStack<T>.NextSP: PContainer;
begin
  if SP = Top then
    Grow
  else
    Inc(SP);
  Result := SP;
end;

function TFastManagedStack<T>.NextSPPost: PContainer;
begin
  Result := SP;
  if SP = Top then
    Grow
  else
    Inc(SP);
end;

function TFastManagedStack<T>.Pick: T;
begin
  Result := SP^;
//  if Integer(SP) >= Integer(LowBound) then
  Dec(SP);
end;

function TFastManagedStack<T>.PickAddress: PContainer;
begin
  Result := SP;
  Dec(SP);
end;

procedure TFastManagedStack<T>.Push(const AContainer: T);
begin
  if SP = Top then
    Grow
  else
    Inc(SP);
  SP^ := AContainer;
  // Move(AContainer, SP^, SizeOf(T));
end;

procedure TFastManagedStack<T>.RestoreStackPointer;
begin
  LowBound := PContainer(Integer(Base) + SPs.Pick);
end;

end.
