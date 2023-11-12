unit IntegrationUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process, CommonUnit, IntegrationDataUnit, fpJson;

type

  { TIntegration }

  TIntegration = class
  public
    function ReadSummary: TSummaryInfo;
    function ReadChampion: TChampionInfo;
  end;

implementation

{ TIntegration }

function TIntegration.ReadSummary: TSummaryInfo;
var
  output: string;
  data: TJSONData;
begin
  RunCommand('deno', ['task', 'run', '--summary', '--json'], output);
  data := GetJSON(output);
  if data is TJSONObject then
  begin
    result := TSummaryInfo.Create;
    result.ReadFromJson(TJSONObject(data));
  end
  else
    result := nil;
  data.Free;
end;

function TIntegration.ReadChampion: TChampionInfo;
begin

end;

end.

