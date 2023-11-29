unit AdviceFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Grids, IntegrationUnit, StringUnit;

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
    function GetAllyChampionNames: TStringArray;
    function GetEnemyChampionNames: TStringArray;
    procedure RefreshIfNecessary;
    procedure Refresh;
    procedure ClearEdits;
  public
    property AllyChampionNames: TStringArray read GetAllyChampionNames;
    property EnemyChampionNames: TStringArray read GetEnemyChampionNames;
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

{ TAdviceFrame }

procedure TAdviceFrame.LoadingTimerTimer(Sender: TObject);
begin
  RefreshIfNecessary;
end;

function TAdviceFrame.GetAllyChampionNames: TStringArray;
begin
  SetLength(result, 5);
  result[0] := AllyEdit0.Text;
  result[1] := AllyEdit1.Text;
  result[2] := AllyEdit2.Text;
  result[3] := AllyEdit3.Text;
  result[4] := AllyEdit4.Text;
end;

function TAdviceFrame.GetEnemyChampionNames: TStringArray;
begin
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
  integration: TIntegration;
  allyChampionNamesLocal: TStringArray;
  enemyChampionNamesLocal: TStringArray;
begin
  allyChampionNamesLocal := AllyChampionNames;
  enemyChampionNamesLocal := EnemyChampionNames;
  LatestAllyChampionNames := allyChampionNamesLocal;
  LatestEnemyChampionNames := enemyChampionNamesLocal;
  integration := TIntegration.Create;
  integration.ReadAdvice(allyChampionNamesLocal, enemyChampionNamesLocal);
  integration.Free;
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

procedure TAdviceFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  ClearEdits;
end;

end.

