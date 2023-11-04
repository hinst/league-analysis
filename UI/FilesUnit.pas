unit FilesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function ReadTextFromFile(filePath: string): string;
procedure WriteTextToFile(filePath: string; text: string);

implementation

function ReadTextFromFile(filePath: string): string;
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.LoadFromFile(filePath);
  SetLength(result, stream.Size);
  stream.Read(result[1], stream.Size);
  stream.Free;
end;

procedure WriteTextToFile(filePath: string; text: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filePath, fmCreate or fmOpenWrite);
  stream.WriteBuffer(text[1], Length(text));
  stream.Free;
end;

end.

