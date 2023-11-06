unit FilesUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function ReadTextFromFile(filePath: string): string;
function ReadTextFromStream(stream: TStream): string;
procedure WriteTextToFile(filePath: string; text: string);

implementation

function ReadTextFromFile(filePath: string): string;
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.LoadFromFile(filePath);
  result := ReadTextFromStream(stream);
end;

function ReadTextFromStream(stream: TStream): string;
var
  size: Int64;
begin
  size := stream.Size;
  SetLength(result, size);
  stream.Read(result[1], size);
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

