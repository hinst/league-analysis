unit ConfigurationFileUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FpJson, JsonParser, FilesUnit;

const
  DefaultConfigurationFileLocation = '../config.json';

type

  { TConfigurationFile }

  TConfigurationFile = class
  public
    GameName: string;
    TagLine: string;
    ApiKey: string;
    constructor Create;
    procedure Read;
    procedure Write;
  private
    procedure WriteText(text: string);
    function ReadText(): string;
  end;

implementation

{ TConfigurationFile }

constructor TConfigurationFile.Create;
begin
  inherited Create;
end;

procedure TConfigurationFile.Read;
var
  jsonData: TJSONData;
begin
  jsonData := GetJSON(ReadText);
  GameName := jsonData.FindPath('gameName').AsString;
  TagLine := jsonData.FindPath('tagLine').AsString;
  ApiKey := jsonData.FindPath('apiKey').AsString;
  jsonData.Free;
end;

procedure TConfigurationFile.Write;
var
  jsonData: TJSONData;
  jsonText: string;
begin
  jsonData := GetJSON(ReadText);
  if jsonData is TJSONObject then
  begin
    TJSONObject(jsonData).Elements['gameName'].AsString := GameName;
    TJSONObject(jsonData).Elements['tagLine'].AsString := TagLine;
    TJSONObject(jsonData).Elements['apiKey'].AsString := ApiKey;
  end;
  jsonText := jsonData.FormatJSON();
  jsonData.Free;
  WriteText(jsonText);
end;

procedure TConfigurationFile.WriteText(text: string);
begin
  WriteTextToFile(DefaultConfigurationFileLocation, text);
end;

function TConfigurationFile.ReadText: string;
begin
  result := ReadTextFromFile(DefaultConfigurationFileLocation);
end;

end.

