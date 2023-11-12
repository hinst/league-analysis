unit IntegrationUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process, CommonUnit, IntegrationDataUnit, fpJson;

type

  { TIntegration }

  TIntegration = class
  public
    function ReadSummary: TSummaryInformation;
  end;

implementation

{ TIntegration }

function TIntegration.ReadSummary: TSummaryInformation;
var
  output: string;
  data: TJSONData;
begin
  RunCommand('deno', ['task', 'run', '--summary', '--json'], output);
  data := GetJSON(output);
  if data is TJSONObject then
  begin
    result := TSummaryInformation.Create;
    result.ReadFromJson(TJSONObject(data));
  end
  else
    result := nil;
  data.Free;
end;

end.

