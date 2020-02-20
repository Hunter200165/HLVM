unit HLVM.Types.AbstractReader;

interface

uses
  HLVM.Types.Ordinal,
  HLVM.Static.Command,
  System.Types,
  System.Classes,
  System.SysUtils;

type
  THAbstractReader = class(TObject)
  private
    FInputStream: TStream;
  public
    property InputStream: TStream read FInputStream write FInputStream;

    function ReadCommand: HCommand;
    function ReadInteger: HInteger;
    function ReadString:  String;

    function ReadPosition: THPosition;

    function ReadIntegerValue: HSInteger;
    function ReadRealValue: HSFloat;

    function EndOfStream: Boolean;
  end;

implementation

{ THAbstractReader }

function THAbstractReader.EndOfStream: Boolean;
begin
  Result := InputStream.Position >= InputStream.Size;
end;

function THAbstractReader.ReadCommand: HCommand;
begin
  Result := Command_NoOperation;
  InputStream.ReadBuffer(Result, SizeOf(HCommand));
end;

function THAbstractReader.ReadInteger: HInteger;
begin
  Result := 0;
  InputStream.ReadBuffer(Result, SizeOf(HInteger));
end;

function THAbstractReader.ReadIntegerValue: HSInteger;
begin
  Result := 0;
  InputStream.ReadBuffer(Result, SizeOf(HSInteger));
end;

function THAbstractReader.ReadPosition: THPosition;
begin
//  Result.ScopeName := ReadString;
  Result.LineNumber := ReadInteger;
  Result.RowNumber := ReadInteger;
end;

function THAbstractReader.ReadRealValue: HSFloat;
begin
  Result := 0;
  InputStream.ReadBuffer(Result, SizeOf(HSFloat));
end;

function THAbstractReader.ReadString: String;
var Len: Integer;
    Bytes: TBytes;
begin
  Len := ReadInteger;
//  SetLength(Result, Len);
  SetLength(Bytes, Len);
//  InputStream.ReadBuffer(Result[1], Len);
  InputStream.ReadBuffer(Bytes[0], Len);
  Result := TEncoding.UTF8.GetString(Bytes);
end;

end.

