unit SummaryFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, DateUtils, Graphics,
  IntegrationUnit, IntegrationDataUnit, AlliesAndEnemiesFrameUnit, MonthlyWinrateFrameUnit,
  CommonUnit;

type

  { TSummaryFrame }

  TSummaryFrame = class(TFrame)
    ChampionBox: TGroupBox;
    ChampionSummaryListView: TListView;
		ChampionInfoBox: TGroupBox;
		ChampionInfoTabs: TTabControl;
    procedure ChampionInfoTabsChange(Sender: TObject);
    procedure ChampionSummaryListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    AlliesAndEnemiesFrame: TAlliesAndEnemiesFrame;
    MonthlyWinrateFrame: TMonthlyWinrateFrame;
    Threads: TThreadList;
    procedure ClearChampionFrame;
    procedure ReadSummaryInfo;
    procedure ReadChampionInfo(const aChampionName: string);
    procedure ShowChampionInfoTab;
  protected
    procedure ReceiveSummaryInfo(summary: TSummaryInfo);
    procedure ReceiveChampionInfo(info: TChampionWinRateSummary);
  public
    constructor Create(theOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R SummaryFrameUnit.lfm}

type

	{ TReadSummaryInfoThread }

  TReadSummaryInfoThread = class(TThread)
  private
    Owner: TSummaryFrame;
    Summary: TSummaryInfo;
    procedure ShowSummaryInfo;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TSummaryFrame);
	end;

  { TReadChampionInfoThread }

  TReadChampionInfoThread = class(TThread)
  private
    Owner: TSummaryFrame;
    ChampionName: string;
    Summary: TChampionWinRateSummary;
    procedure ShowSummary;
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TSummaryFrame; const aChampionName: string);
  end;

{ TSummaryFrame }

procedure TSummaryFrame.ChampionSummaryListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    ClearChampionFrame;
    ReadChampionInfo(Item.Caption);
  end;
end;

procedure TSummaryFrame.ChampionInfoTabsChange(Sender: TObject);
begin
  ShowChampionInfoTab;
end;

procedure TSummaryFrame.ClearChampionFrame;
begin
  FreeAndNil(AlliesAndEnemiesFrame);
  FreeAndNil(MonthlyWinrateFrame);
end;

procedure TSummaryFrame.ReadSummaryInfo;
var
  thread: TReadSummaryInfoThread;
begin
  ChampionSummaryListView.Enabled := false;
  thread := TReadSummaryInfoThread.Create(self);
  Threads.Add(thread);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TSummaryFrame.ReadChampionInfo(const aChampionName: string);
var
  thread: TReadChampionInfoThread;
begin
  ChampionInfoBox.Caption := 'Champion: loading info...';
  thread := TReadChampionInfoThread.Create(self, aChampionName);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TSummaryFrame.ShowChampionInfoTab;
begin
  case ChampionInfoTabs.TabIndex of
    0: if AlliesAndEnemiesFrame <> nil then AlliesAndEnemiesFrame.BringToFront;
    1: if MonthlyWinrateFrame <> nil then MonthlyWinrateFrame.BringToFront;
  end;
end;

procedure TSummaryFrame.ReceiveSummaryInfo(summary: TSummaryInfo);
var
  i: Integer;
  champion: TChampionSummary;
  timeRangeFrom, timeRangeTo: TDateTime;
begin
  ChampionSummaryListView.Clear;
  if summary <> nil then
  begin
    if summary.UserChampions <> nil then
    begin
      timeRangeFrom := UniversalTimeToLocal(UnixToDateTime(summary.OldestMatchDate div 1000));
      timeRangeTo := UniversalTimeToLocal(UnixToDateTime(summary.NewestMatchDate div 1000));
  		ChampionBox.Caption := 'Your champions [' + IntToStr(summary.UserChampions.Count) + '] ' +
        ' in time range ' + FormatDateTime('yyyy-mm-dd hh:nn', timeRangeFrom) +
        ' ... ' + FormatDateTime('yyyy-mm-dd hh:nn', timeRangeTo);
      for i := 0 to summary.UserChampions.Count - 1 do
      begin
        champion := summary.UserChampions.Items[i];
        ChampionSummaryListView.AddItem(champion.ChampionName, nil);
        ChampionSummaryListView.Items[ChampionSummaryListView.Items.Count - 1]
          .SubItems.Add(IntToStr(champion.WinRate.MatchCount));
        ChampionSummaryListView.Items[ChampionSummaryListView.Items.Count - 1]
          .SubItems.Add(FloatToStrF(champion.WinRate.WinRate * 100, ffFixed, 0, 1));
			end;
		end
    else
      ChampionBox.Caption := 'Your champions: not found';
	end
  else
    ChampionBox.Caption := 'Your champions: error';
  ChampionSummaryListView.Enabled := true;
end;

procedure TSummaryFrame.ReceiveChampionInfo(info: TChampionWinRateSummary);
begin
  if info <> nil then
  begin
    ClearChampionFrame;
    AlliesAndEnemiesFrame := TAlliesAndEnemiesFrame.Create(self);
    AlliesAndEnemiesFrame.ShowInfo(info);
    AlliesAndEnemiesFrame.Parent := ChampionInfoTabs;
    AlliesAndEnemiesFrame.Align := alClient;
    AlliesAndEnemiesFrame.Color := clDefault;

    MonthlyWinrateFrame := TMonthlyWinrateFrame.Create(self);
    MonthlyWinrateFrame.Parent := ChampionInfoTabs;
    MonthlyWinrateFrame.Align := alClient;
    MonthlyWinrateFrame.ShowInfo(info.WinRateMonths);

    ShowChampionInfoTab;
    ChampionInfoBox.Caption := 'Champion: ' + info.ChampionName;
  end
  else
    ChampionInfoBox.Caption := 'Champion: loading failed';
end;

constructor TSummaryFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
end;

procedure TSummaryFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  Threads := TThreadList.Create;
  ChampionSummaryListView.ViewStyle := vsReport;
  ReadSummaryInfo;
end;

procedure TSummaryFrame.BeforeDestruction;
begin
  WaitForEmptyThreadList(Threads);
  FreeAndNil(Threads);
  inherited BeforeDestruction;
end;

{ TReadSummaryInfoThread }

constructor TReadSummaryInfoThread.Create(aOwner: TSummaryFrame);
begin
  inherited Create(true);
  Owner := aOwner;
end;

procedure TReadSummaryInfoThread.ShowSummaryInfo;
begin
  Owner.ReceiveSummaryInfo(Summary);
end;

procedure TReadSummaryInfoThread.Execute;
var
  integration: TIntegration;
begin
  try
    integration := TIntegration.Create;
    Summary := integration.ReadSummary;
    integration.Free;
	except
    Summary := nil;
	end;
  Synchronize(@ShowSummaryInfo);
  FreeAndNil(Summary);
  Owner.Threads.Remove(self);
end;

{ TReadChampionInfoThread }

constructor TReadChampionInfoThread.Create(aOwner: TSummaryFrame; const aChampionName: string);
begin
  inherited Create(true);
  Owner := aOwner;
  ChampionName := aChampionName;
end;

procedure TReadChampionInfoThread.ShowSummary;
begin
  Owner.ReceiveChampionInfo(Summary);
end;

procedure TReadChampionInfoThread.Execute;
var
  integration: TIntegration;
begin
  try
    integration := TIntegration.Create;
    Summary := integration.ReadChampion(ChampionName);
    integration.Free;
	except
    Summary := nil;
	end;
  Synchronize(@ShowSummary);
  FreeAndNil(Summary);
  Owner.Threads.Remove(self);
end;

end.

