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
    class function CreateFromJson(jsonObject: TJSONObject): TWinRateInfo;
	end;

  { TMonthWinRateInfo }

  TMonthWinRateInfo = class
  private
    function GetMonth: Integer;
    function GetYear: Integer;
  public
    Key: string;
    Value: TWinRateInfo;
    property Year: Integer read GetYear;
    property Month: Integer read GetMonth;
    destructor Destroy; override;
  end;

  TMonthWinRateInfoList = specialize TFPGObjectList<TMonthWinRateInfo>;

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

  { TChampionWinRateInfo }

  TChampionWinRateInfo = class
  public
    ChampionName: string;
    AllyInfo: TWinRateInfo;
    EnemyInfo: TWinRateInfo;
    procedure ReadFromJson(data: TJSONObject);
    destructor Destroy; override;
  end;

  TChampionWinRateInfoList = specialize TFPGObjectList<TChampionWinRateInfo>;

  { TChampionWinRateSummary }

  TChampionWinRateSummary = class
  public
    ChampionName: string;
    WinRate: TWinRateInfo;
    BestAllies: TChampionWinRateInfoList;
    WorstAllies: TChampionWinRateInfoList;
    EasiestEnemies: TChampionWinRateInfoList;
    HardestEnemies: TChampionWinRateInfoList;
    WinRateMonths: TMonthWinRateInfoList;
    procedure ReadFromJson(data: TJSONObject);
    destructor Destroy; override;
  private
    class function ReadChampionList(data: TJSONObject; const fieldName: string): TChampionWinRateInfoList;
    class function ReadMonthsWinRate(data: TJSONObject): TMonthWinRateInfoList;
	end;

  TTeam = (Enemy := -1, Ally := 1);

  { TChampionChanceAdvice }

  TChampionChanceAdvice = class
  public
    ChampionName: string;
    Team: TTeam;
    WinRate: TWinRateInfo;
    procedure ReadFromJson(data: TJSONObject);
  end;

  TChampionChanceAdviceList = specialize TFPGObjectList<TChampionChanceAdvice>;

  { TTeamChanceAdvice }

  TTeamChanceAdvice = class
  public
    ChampionName: string;
    TotalWinRate: TWinRateInfo;
    Champions: TChampionChanceAdviceList;
    procedure ReadFromJson(data: TJSONObject);
    destructor Destroy; override;
  end;

  TTeamChanceAdviceList = specialize TFPGObjectList<TTeamChanceAdvice>;

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

class function TWinRateInfo.CreateFromJson(jsonObject: TJSONObject): TWinRateInfo;
begin
  result := TWinRateInfo.Create;
  result.ReadFromJson(jsonObject);
end;

{ TMonthWinRateInfo }

function TMonthWinRateInfo.GetMonth: Integer;
begin
  result := StrToInt(Copy(Key, 6, 2));
end;

function TMonthWinRateInfo.GetYear: Integer;
begin
  result := StrToInt(Copy(Key, 1, 4));
end;

destructor TMonthWinRateInfo.Destroy;
begin
  Key := '';
  FreeAndNil(Value);
  inherited Destroy;
end;

{ TChampionSummary }

procedure TChampionSummary.ReadFromJson(data: TJSONObject);
begin
  ChampionName := data.Get('championName', '');
  WinRate := TWinRateInfo.CreateFromJson(data.Get('winRate', TJSONObject(nil)));
end;

destructor TChampionSummary.Destroy;
begin
  FreeAndNil(WinRate);
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
  	UserChampions := specialize TFPGObjectList<TChampionSummary>.Create;
    UserChampions.Capacity := userChampionsArray.Count;
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

{ TChampionWinRateInfo }

procedure TChampionWinRateInfo.ReadFromJson(data: TJSONObject);
begin
  ChampionName := data.Get('championName', '');
  AllyInfo := TWinRateInfo.CreateFromJson(data.Get('allyInfo', TJSONObject(nil)));
  EnemyInfo := TWinRateInfo.CreateFromJson(data.Get('enemyInfo', TJSONObject(nil)));
end;

destructor TChampionWinRateInfo.Destroy;
begin
  FreeAndNil(AllyInfo);
  FreeAndNil(EnemyInfo);
  inherited Destroy;
end;

{ TChampionWinRateSummary }

procedure TChampionWinRateSummary.ReadFromJson(data: TJSONObject);
begin
  ChampionName := data.Get('championName', '');
  WinRate := TWinRateInfo.CreateFromJson(data.Get('winRate', TJSONObject(nil)));
  BestAllies := ReadChampionList(data, 'bestAllies');
  WorstAllies := ReadChampionList(data, 'worstAllies');
  EasiestEnemies := ReadChampionList(data, 'easiestEnemies');
  HardestEnemies := ReadChampionList(data, 'hardestEnemies');
  WinRateMonths := ReadMonthsWinRate(data.Get('winRateMonths', TJSONObject(nil)));
end;

destructor TChampionWinRateSummary.Destroy;
begin
  FreeAndNil(WinRate);
  FreeAndNil(BestAllies);
  FreeAndNil(WorstAllies);
  FreeAndNil(EasiestEnemies);
  FreeAndNil(HardestEnemies);
  FreeAndNil(WinRateMonths);
  inherited Destroy;
end;

class function TChampionWinRateSummary.ReadChampionList(data: TJSONObject; const fieldName: string): TChampionWinRateInfoList;
var
  jsonArray: TJSONArray;
  i: Integer;
  championWinRateInfo: TChampionWinRateInfo;
begin
  jsonArray := data.Get(fieldName, TJSONArray(nil));
  if jsonArray <> nil then
  begin
    result := TChampionWinRateInfoList.Create;
    result.Capacity := jsonArray.Count;
    for i := 0 to jsonArray.Count - 1 do
    begin
      championWinRateInfo := TChampionWinRateInfo.Create;
      championWinRateInfo.ReadFromJson(jsonArray.Items[i] as TJSONObject);
      result.Add(championWinRateInfo);
    end;
  end
  else
    result := nil;
end;

class function TChampionWinRateSummary.ReadMonthsWinRate(data: TJSONObject): TMonthWinRateInfoList;
var
  i: Integer;
  monthWinRateInfo: TMonthWinRateInfo;
begin
  if data <> nil then
  begin
    result := TMonthWinRateInfoList.Create;
    result.Capacity := data.Count;
    for i := 0 to data.Count - 1 do
    begin
      monthWinRateInfo := TMonthWinRateInfo.Create;
      monthWinRateInfo.Key := data.Names[i];
      monthWinRateInfo.Value := TWinRateInfo.CreateFromJson(data.Elements[monthWinRateInfo.Key] as TJsonObject);
      result.Add(monthWinRateInfo);
    end;
  end
  else
    result := nil;
end;

{ TChampionChanceAdvice }

procedure TChampionChanceAdvice.ReadFromJson(data: TJSONObject);
begin

end;

{ TTeamChanceAdvice }

procedure TTeamChanceAdvice.ReadFromJson(data: TJSONObject);
var
  championArray: TJSONArray;
  championItem: TJSONEnum;
  championChanceAdvice: TChampionChanceAdvice;
begin
  ChampionName := data.Get('championName', '');
  TotalWinRate := TWinRateInfo.CreateFromJson(data.Get('totalWinRage', TJSONObject(nil)));
  championArray := data.Get('champions', TJSONArray(nil));
  if championArray <> nil then
  begin
    Champions := TChampionChanceAdviceList.Create();
    Champions.Capacity := championArray.Count;
    for championItem in championArray do
    begin
      championChanceAdvice := TChampionChanceAdvice.Create();
      championChanceAdvice.ReadFromJson(championItem.Value as TJSONObject);
      Champions.Add(championChanceAdvice);
    end;
  end;
end;

destructor TTeamChanceAdvice.Destroy;
begin
  FreeAndNil(TotalWinRate);
  FreeAndNil(Champions);
  inherited Destroy;
end;

end.

