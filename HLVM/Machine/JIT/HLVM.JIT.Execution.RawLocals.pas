unit HLVM.JIT.Execution.RawLocals;

interface

{ This module brings implementation of Fast Locals System }
{ It contains reference counting for scopes and all that stuff }
{ Only works under FSM (Fast-Static Mode) of JIT compiler }

{ Highly recommended to use those modules of FSM bundle }
{ All obsolete stuff will be deprecated and then removed in updated }

uses
  HLVM.JIT.Context.Storage;

type
  { Implements local bundle }
  THLVMLocalBundle = record
  public type
    TInternalPointer = ^THLVMLocalBundle;
    TContainer = array of HLVM_Variable;
  public var
    RefCounter: Integer;

    ParentBundle: TInternalPointer;
    Container: TContainer;
  public
    { Inlines }
    procedure IncRC(const ABy: Integer = 1); inline;
    procedure DecRC(const ABy: Integer = 1); inline;

    procedure SetSize(const N: Integer); inline;

    procedure Attach(const AParent: TInternalPointer); inline;
    procedure Detach; inline;
  public
    { Default constructors }
    class function Create(const ARefCounter: Integer = 1): TInternalPointer; overload; static;
    class function Create(const AParent: TInternalPointer; const ASize: Integer; const ARefCounter: Integer = 1): TInternalPointer; overload; static;

    { Default destructor }
    procedure Free;
  end;
  PHLVMLocalBundle = THLVMLocalBundle.TInternalPointer;

type
  THLVMLocalPointersBundle = array of PHLVM_Variable;

implementation

{ THLVMLocalBundle }

class function THLVMLocalBundle.Create(const ARefCounter: Integer): TInternalPointer;
begin
  New(Result);

  Result.ParentBundle := nil;
  Result.RefCounter := ARefCounter;
end;

procedure THLVMLocalBundle.Attach(const AParent: TInternalPointer);
begin
  Detach;
  Self.ParentBundle := AParent;
end;

class function THLVMLocalBundle.Create(const AParent: TInternalPointer; const ASize, ARefCounter: Integer): TInternalPointer;
begin
  Result := THLVMLocalBundle.Create(ARefCounter);
  Result.SetSize(ASize);
  Result.Attach(AParent);
end;

procedure THLVMLocalBundle.DecRC(const ABy: Integer);
begin
  Dec(RefCounter, ABy);
  if RefCounter <= 0 then
    Free;
end;

procedure THLVMLocalBundle.Detach;
begin
  if not (ParentBundle = nil) then
    ParentBundle.DecRC;
end;

procedure THLVMLocalBundle.Free;
begin
  if not (ParentBundle = nil) then
    ParentBundle.DecRC;
  Dispose(@Self);
end;

procedure THLVMLocalBundle.IncRC(const ABy: Integer);
begin
  Inc(RefCounter, ABy);
end;

procedure THLVMLocalBundle.SetSize(const N: Integer);
begin
  SetLength(Container, N);
end;

end.
