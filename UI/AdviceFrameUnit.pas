unit AdviceFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Grids, fgl, IntegrationUnit,
  IntegrationDataUnit, StringUnit, CommonUnit;

type

  { TAdviceFrame }

  TAdviceFrame = class(TFrame)
    AllyTeamBox: TGroupBox;
    EnemyTeamBox: TGroupBox;
    AllyEdit0: TLabeledEdit;
    AllyEdit2: TLabeledEdit;
    AllyEdit1: TLabeledEdit;
    AllyEdit3: TLabeledEdit;
    AllyEdit4: TLabeledEdit;
    FoeEdit0: TLabeledEdit;
    FoeEdit1: TLabeledEdit;
    FoeEdit2: TLabeledEdit;
    FoeEdit3: TLabeledEdit;
    FoeEdit4: TLabeledEdit;
    ChampionNamesPanel: TPanel;
    ChancesStringGrid: TStringGrid;
    LoadingTimer: TIdleTimer;
    procedure LoadingTimerTimer(Sender: TObject);
  private
    LatestAllyChampionNames: TStringArray;
    LatestEnemyChampionNames: TStringArray;
    Threads: TThreadList;
    function GetAllyChampionNames: TStringArray;
    function GetEnemyChampionNames: TStringArray;
    procedure RefreshIfNecessary;
    procedure Refresh;
    procedure ClearEdits;
    procedure ReceiveAdviceList(adviceList: TTeamChanceAdviceList);
  public
    property AllyChampionNames: TStringArray read GetAllyChampionNames;
    property EnemyChampionNames: TStringArray read GetEnemyChampionNames;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.lfm}

type
  { TReadAdviceThread }

  TReadAdviceThread = class(TThread)
  public
    constructor Create(aOwner: TAdviceFrame; allyChampionNames: TStringArray; enemyChampionNames: TStringArray);
  protected
    procedure Execute; override;
  private
    Owner: TAdviceFrame;
    AllyChampionNames: TStringArray;
    EnemyChampionNames: TStringArray;
    AdviceList: TTeamChanceAdviceList;
    procedure ShowAdvice;
  end;

{ TAdviceFrame }

procedure TAdviceFrame.LoadingTimerTimer(Sender: TObject);
begin
  RefreshIfNecessary;
end;

function TAdviceFrame.GetAllyChampionNames: TStringArray;
begin
  result := nil;
  SetLength(result, 5);
  result[0] := AllyEdit0.Text;
  result[1] := AllyEdit1.Text;
  result[2] := AllyEdit2.Text;
  result[3] := AllyEdit3.Text;
  result[4] := AllyEdit4.Text;
end;

function TAdviceFrame.GetEnemyChampionNames: TStringArray;
begin
  result := nil;
  SetLength(result, 5);
  result[0] := FoeEdit0.Text;
  result[1] := FoeEdit1.Text;
  result[2] := FoeEdit2.Text;
  result[3] := FoeEdit3.Text;
  result[4] := FoeEdit4.Text;
end;

procedure TAdviceFrame.RefreshIfNecessary;
var
  allEmpty: Boolean;
  needChange: Boolean;
begin
  allEmpty := CheckArrayEmpty(AllyChampionNames) and CheckArrayEmpty(EnemyChampionNames);
  needChange := not CheckArraysEqual(LatestAllyChampionNames, AllyChampionNames) or
    not CheckArraysEqual(LatestEnemyChampionNames, EnemyChampionNames);
  if allEmpty then
    ChancesStringGrid.Clear
  else if needChange then
    Refresh;
end;

procedure TAdviceFrame.Refresh;
var
  thread: TReadAdviceThread;
begin
  LatestAllyChampionNames := AllyChampionNames;
  LatestEnemyChampionNames := EnemyChampionNames;
  thread := TReadAdviceThread.Create(self, AllyChampionNames, EnemyChampionNames);
  thread.FreeOnTerminate := true;
  Threads.Add(thread);
  thread.Start;
end;

procedure TAdviceFrame.ClearEdits;
begin
  AllyEdit0.Text := '';
  AllyEdit1.Text := '';
  AllyEdit2.Text := '';
  AllyEdit3.Text := '';
  AllyEdit4.Text := '';
  FoeEdit0.Text := '';
  FoeEdit1.Text := '';
  FoeEdit2.Text := '';
  FoeEdit3.Text := '';
  FoeEdit4.Text := '';
end;

procedure TAdviceFrame.ReceiveAdviceList(adviceList: TTeamChanceAdviceList);
var
  teamChanceAdvice: TTeamChanceAdvice;
  championChanceAdvice: TChampionChanceAdvice;
  rowIndex: Integer;
  columnIndex: Integer;
begin
  rowIndex := 1;
  ChancesStringGrid.RowCount := adviceList.Count + 1;
  ChancesStringGrid.ColCount := 11;
  for teamChanceAdvice in adviceList do
  begin
    columnIndex := 0;
    ChancesStringGrid.Cells[columnIndex, rowIndex] := teamChanceAdvice.ChampionName + ' ' +
      teamChanceAdvice.TotalWinRate.ToString();
    Inc(columnIndex);
    for championChanceAdvice in teamChanceAdvice.Champions do
    begin
      ChancesStringGrid.Cells[columnIndex, rowIndex] := GetTeamSign(championChanceAdvice.Team) + ' ' +
        championChanceAdvice.ChampionName + ' ' + championChanceAdvice.WinRate.ToString();
      Inc(columnIndex);
    end;
    Inc(rowIndex);
  end;
  ChancesStringGrid.AutoSizeColumns;
end;

procedure TAdviceFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  ClearEdits;
  ChancesStringGrid.Clear;
  Threads := TThreadList.Create();
end;

procedure TAdviceFrame.BeforeDestruction;
var
  threadList: TList;
  threadCount: Integer;
begin
  while true do
  begin
    threadList := Threads.LockList;
    threadCount := threadList.Count;
    Threads.UnlockList;
    if threadCount = 0 then
      break;
    Sleep(SleepWhenExitingMilliseconds);
    Application.ProcessMessages;
  end;
  FreeAndNil(Threads);
  inherited BeforeDestruction;
end;

{ TReadAdviceThread }

constructor TReadAdviceThread.Create(aOwner: TAdviceFrame;
  allyChampionNames: TStringArray; enemyChampionNames: TStringArray);
begin
  inherited Create(true);
  Owner := aOwner;
  self.AllyChampionNames := allyChampionNames;
  self.EnemyChampionNames := enemyChampionNames;
end;

procedure TReadAdviceThread.Execute;
var
  integration: TIntegration;
begin
  integration := TIntegration.Create;
  AdviceList := integration.ReadAdvice(AllyChampionNames, EnemyChampionNames);
  integration.Free;
  Synchronize(@ShowAdvice);
  FreeAndNil(AdviceList);
  Owner.Threads.Remove(self);
end;

procedure TReadAdviceThread.ShowAdvice;
begin
  Owner.ReceiveAdviceList(AdviceList);
end;

end.

