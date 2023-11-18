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
    function ReadChampion(const championName: string): TChampionWinRateSummary;
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

function TIntegration.ReadChampion(const championName: string): TChampionWinRateSummary;
var
  output: string;
  data: TJSONData;
begin
  RunCommand('deno', ['task', 'run', '--champion=' + championName, '--json'], output);
  data := GetJSON(output);
  if data is TJSONObject then
  begin
    result := TChampionWinRateSummary.Create;
    result.ReadFromJson(TJSONObject(data));
  end
  else
    result := nil;
  data.Free;
end;

end.

