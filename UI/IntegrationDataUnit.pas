unit IntegrationDataUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpJson;

type

  { TSummaryInformation }

  TSummaryInformation = class
  public
    StoredMatchesLength: Integer;
    OldestMatchDate: TDateTime;
    NewestMatchDate: TDateTime;
    StoredMatchesDays: Integer;
    AllChampionsLength: Integer;
    procedure ReadFromJson(data: TJSONObject);
  end;

implementation

{ TSummaryInformation }

procedure TSummaryInformation.ReadFromJson(data: TJSONObject);
begin
  StoredMatchesLength := data.Get('storedMatchesLength', 0);
end;

end.

