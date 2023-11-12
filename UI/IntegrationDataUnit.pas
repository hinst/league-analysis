unit IntegrationDataUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpJson, fgl;

type

	{ TWinRateInfo }

  TWinRateInfo = class
	private
    function GetWinRate: Double;
  public
    MatchCount: Int64;
    VictoryCount: Int64;
    property WinRate: Double read GetWinRate;
    procedure ReadFromJson(jsonObject: TJSONObject);
	end;

	{ TChampionSummary }

  TChampionSummary = class
  public
    ChampionName: string;
    WinRate: TWinRateInfo;
    procedure ReadFromJson(data: TJSONObject);
    destructor Destroy; override;
	end;

  { TSummaryInfo }

  TSummaryInfo = class
  public
    StoredMatchesLength: Integer;
    OldestMatchDate: Int64;
    NewestMatchDate: Int64;
    StoredMatchesDays: Integer;
    AllChampionsLength: Integer;
    UserChampions: specialize TFPGObjectList<TChampionSummary>;
    procedure ReadFromJson(data: TJSONObject);
    destructor Destroy; override;
  end;

  TChampionInfo = class
	end;

implementation

{ TWinRateInfo }

function TWinRateInfo.GetWinRate: Double;
begin
  if MatchCount <> 0 then
    result := VictoryCount / MatchCount
  else
    result := 0;
end;

procedure TWinRateInfo.ReadFromJson(jsonObject: TJSONObject);
begin
  MatchCount := jsonObject.Get('matchCount', Int64(0));
  VictoryCount := jsonObject.Get('victoryCount', Int64(0));
end;

{ TChampionSummary }

procedure TChampionSummary.ReadFromJson(data: TJSONObject);
var
  winRateObject: TJSONObject;
begin
  ChampionName := data.Get('championName', '');
  winRateObject := data.Get('winRate', TJSONObject(nil));
  if winRateObject <> nil then
  begin
    WinRate := TWinRateInfo.Create;
    WinRate.ReadFromJson(winRateObject);
  end;
end;

destructor TChampionSummary.Destroy;
begin
  if WinRate <> nil then
  begin
    WinRate.Free;
    WinRate := nil;
  end;
  inherited Destroy;
end;

{ TSummaryInfo }

procedure TSummaryInfo.ReadFromJson(data: TJSONObject);
var
  userChampionsArray: TJSONArray = nil;
  i: Integer;
  userChampionData: TJSONData;
  userChampion: TChampionSummary;
begin
  StoredMatchesLength := data.Get('storedMatchesLength', Int64(0));
  OldestMatchDate := data.Get('oldestMatchDate', Int64(0));
  NewestMatchDate := data.Get('newestMatchDate', Int64(0));
  StoredMatchesDays := data.Get('storedMatchesDays', Int64(0));
  AllChampionsLength := data.Get('allChampionsLength', Int64(0));
  userChampionsArray := data.Get('userChampions', userChampionsArray);
  if userChampionsArray <> nil then
  begin
  	UserChampions := specialize TFPGObjectList<TChampionSummary>.Create(true);
    for i := 0 to userChampionsArray.Count - 1 do
    begin
      userChampionData := userChampionsArray[i];
      if (userChampionData <> nil) and (userChampionData is TJSONObject) then
      begin
        userChampion := TChampionSummary.Create;
        userChampion.ReadFromJson(TJSONObject(userChampionData));
        UserChampions.Add(userChampion);
			end;
		end;
	end;
end;

destructor TSummaryInfo.Destroy;
begin
  UserChampions.Free;
  UserChampions := nil;
  inherited Destroy;
end;

end.

