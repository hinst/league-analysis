unit AlliesAndEnemiesFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, IntegrationDataUnit, CommonUnit;

type

	{ TAlliesAndEnemiesFrame }

  TAlliesAndEnemiesFrame = class(TFrame)
 		BestAlliesBox: TGroupBox;
    BestAlliesListView: TListView;
    EasiestEnemiesListView: TListView;
    HardestEnemiesListView: TListView;
    WorstAlliesListView: TListView;
  	WorstAlliesBox: TGroupBox;
	  EasiestEnemiesBox: TGroupBox;
		HardestEnemiesBox: TGroupBox;
  private
    procedure FillListView(listView: TListView; infoList: TChampionWinRateInfoList; isAlly: Boolean);
    procedure SetupColumns(listView: TListView);
  public
    procedure ShowInfo(summary: TChampionWinRateSummary);
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

{ TAlliesAndEnemiesFrame }

procedure TAlliesAndEnemiesFrame.FillListView(listView: TListView; infoList: TChampionWinRateInfoList; isAlly: Boolean);
var
  i: Integer;
  viewItem: TListItem;
  infoItem: TChampionWinRateInfo;
begin
  listView.Clear;
  for i := 0 to infoList.Count - 1 do
  begin
    viewItem := listView.Items.Add;
    infoItem := infoList[i];
    viewItem.Caption := infoItem.ChampionName;
    if isAlly then
      viewItem.SubItems.Add(IntToStr(infoItem.AllyInfo.MatchCount))
    else
      viewItem.SubItems.Add(IntToStr(infoItem.EnemyInfo.MatchCount));
    if isAlly then
      viewItem.SubItems.Add(FormatPercent(infoItem.AllyInfo.WinRate))
    else
      viewItem.SubItems.Add(FormatPercent(infoItem.EnemyInfo.WinRate));
  end;
end;

procedure TAlliesAndEnemiesFrame.SetupColumns(listView: TListView);
var
  column: TListColumn;
  i: Integer;
begin
  listView.Columns.Clear;
  column := listView.Columns.Add;
  column.Caption := 'Champion';
  column := listView.Columns.Add;
  column.Caption := 'Game count';
  column := listView.Columns.Add;
  column.Caption := 'Win rate';
  for i := 0 to listView.ColumnCount - 1 do
    listView.Column[i].AutoSize := true;
end;

procedure TAlliesAndEnemiesFrame.ShowInfo(summary: TChampionWinRateSummary);
begin
  FillListView(BestAlliesListView, summary.BestAllies, true);
  FillListView(WorstAlliesListView, summary.WorstAllies, true);
  FillListView(EasiestEnemiesListView, summary.EasiestEnemies, false);
  FillListView(HardestEnemiesListView, summary.HardestEnemies, false);
end;

procedure TAlliesAndEnemiesFrame.AfterConstruction;
begin
  SetupColumns(BestAlliesListView);
  SetupColumns(WorstAlliesListView);
  SetupColumns(EasiestEnemiesListView);
  SetupColumns(HardestEnemiesListView);
end;

end.

