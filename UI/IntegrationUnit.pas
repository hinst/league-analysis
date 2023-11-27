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
    function ReadAdvice(const allyNames: TStringArray; const enemyNames: TStringArray): TTeamChanceAdviceList;
  end;

implementation

function StringifyChampionNames(championNames: TStringArray): string;
var
  i: Integer;
  trimmedName: string;
begin
  result := '';
  for i := 0 to Length(championNames) - 1 do
  begin
    trimmedName := championNames[i].Trim;
    if trimmedName.Length > 0 then
      result := result + '+' + trimmedName + ',';
  end;
end;

function StringifyTeamLine(allyNames: TStringArray; enemyNames: TStringArray): string;
begin
  result := StringifyChampionNames(allyNames) + StringifyChampionNames(enemyNames);
  if result.EndsWith(',') then
    result := result.Substring(0, result.Length - 1);
end;


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

function TIntegration.ReadAdvice(const allyNames: TStringArray; const enemyNames: TStringArray): TTeamChanceAdviceList;
var
  output: string;
begin
  RunCommand('deno', ['task', 'run', '--advice=' + StringifyTeamLine(allyNames, enemyNames)], output);
  WriteLn(output);
end;

end.

