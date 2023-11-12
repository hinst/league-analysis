unit IntegrationDataUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpJson, fgl;

type

	{ TUserChampionSummary }

  TUserChampionSummary = class
  public
    ChampionName: string;
    MatchCount: Integer;
    WinRate: Double;
    VictoryCount: Integer;
    procedure ReadFromJson(data: TJSONObject);
	end;

  { TSummaryInformation }

  TSummaryInformation = class
  public
    StoredMatchesLength: Integer;
    OldestMatchDate: TDateTime;
    NewestMatchDate: TDateTime;
    StoredMatchesDays: Integer;
    AllChampionsLength: Integer;
    UserChampions: specialize TFPGObjectList<TUserChampionSummary>;
    procedure ReadFromJson(data: TJSONObject);
    destructor Destroy; override;
  end;

implementation

{ TUserChampionSummary }

procedure TUserChampionSummary.ReadFromJson(data: TJSONObject);
begin
  ChampionName := data.Get('championName', '');
  MatchCount := data.Get('matchCount', Int64(0));
  WinRate := data.Get('winRate', 0.0);
  VictoryCount := data.Get('victoryCount', Int64(0));
end;

{ TSummaryInformation }

procedure TSummaryInformation.ReadFromJson(data: TJSONObject);
var
  userChampionsArray: TJSONArray = nil;
  i: Integer;
  userChampionData: TJSONData;
  userChampion: TUserChampionSummary;
begin
  StoredMatchesLength := data.Get('storedMatchesLength', Int64(0));
  OldestMatchDate := data.Get('oldestMatchDate', Int64(0));
  NewestMatchDate := data.Get('newestMatchDate', Int64(0));
  StoredMatchesDays := data.Get('storedMatchesDays', Int64(0));
  AllChampionsLength := data.Get('allChampionsLength', Int64(0));
  userChampionsArray := data.Get('userChampions', userChampionsArray);
  if userChampionsArray <> nil then
  begin
  	UserChampions := specialize TFPGObjectList<TUserChampionSummary>.Create(true);
    for i := 0 to userChampionsArray.Count - 1 do
    begin
      userChampionData := userChampionsArray[i];
      if (userChampionData <> nil) and (userChampionData is TJSONObject) then
      begin
        userChampion := TUserChampionSummary.Create;
        userChampion.ReadFromJson(TJSONObject(userChampionData));
        UserChampions.Add(userChampion);
			end;
		end;
	end;
end;

destructor TSummaryInformation.Destroy;
begin
  UserChampions.Free;
  UserChampions := nil;
  inherited Destroy;
end;

end.

