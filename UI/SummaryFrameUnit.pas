unit SummaryFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, DateUtils, Graphics,
  IntegrationUnit, IntegrationDataUnit, AlliesAndEnemiesFrameUnit;

type

  { TSummaryFrame }

  TSummaryFrame = class(TFrame)
    ChampionBox: TGroupBox;
    ChampionSummaryListView: TListView;
		ChampionInfoBox: TGroupBox;
		ChampionInfoTabs: TTabControl;
		procedure ChampionSummaryListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    ChampionFrame: TFrame;
    procedure ClearChampionFrame;
    procedure ReadSummaryInfo;
    procedure ReadChampionInfo(const aChampionName: string);
  protected
    procedure ReceiveSummaryInfo(pSummary: PtrInt);
    procedure ReceiveChampionInfo(pInfo: PtrInt);
  public
    constructor Create(theOwner: TComponent); override;
  end;

implementation

{$R SummaryFrameUnit.lfm}

type

	{ TReadSummaryInfoThread }

  TReadSummaryInfoThread = class(TThread)
  private
    Owner: TSummaryFrame;
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
  protected
    procedure Execute; override;
  public
    constructor Create(aOwner: TSummaryFrame; const aChampionName: string);
  end;

{ TSummaryFrame }

procedure TSummaryFrame.ChampionSummaryListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  ClearChampionFrame;
  if Selected then
    ReadChampionInfo(Item.Caption);
end;

procedure TSummaryFrame.ClearChampionFrame;
begin
  if ChampionFrame <> nil then
  begin
    ChampionFrame.Free;
    ChampionFrame := nil;
	end;
end;

procedure TSummaryFrame.ReadSummaryInfo;
var
  thread: TReadSummaryInfoThread;
begin
  ChampionSummaryListView.Enabled := false;
  thread := TReadSummaryInfoThread.Create(self);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TSummaryFrame.ReadChampionInfo(const aChampionName: string);
var
  thread: TReadChampionInfoThread;
begin
  thread := TReadChampionInfoThread.Create(self, aChampionName);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TSummaryFrame.ReceiveSummaryInfo(pSummary: {*TSummaryInfo*} PtrInt);
var
  summary: TSummaryInfo;
  i: Integer;
  champion: TChampionSummary;
  timeRangeFrom, timeRangeTo: TDateTime;
begin
  summary := TSummaryInfo(pSummary);
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
    begin
      ChampionBox.Caption := 'Your champions: not found';
		end;
	end
  else
    ChampionBox.Caption := 'Your champions: error';
  summary.Free;
  ChampionSummaryListView.Enabled := true;
end;

procedure TSummaryFrame.ReceiveChampionInfo(pInfo: PtrInt);
var
  info: TChampionWinRateSummary;
  alliesAndEnemiesFrame: TAlliesAndEnemiesFrame;
begin
  info := TChampionWinRateSummary(pInfo);
  if info <> nil then
  begin
    alliesAndEnemiesFrame := TAlliesAndEnemiesFrame.Create(self);
    alliesAndEnemiesFrame.ShowInfo(info);
    ChampionFrame := alliesAndEnemiesFrame;
    ChampionFrame.Parent := ChampionInfoTabs;
    ChampionFrame.Align := alClient;
    ChampionFrame.Color := clDefault;
  end;
  info.Free;
end;

constructor TSummaryFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ChampionSummaryListView.ViewStyle := vsReport;
  ReadSummaryInfo;
end;

{ TReadSummaryInfoThread }

constructor TReadSummaryInfoThread.Create(aOwner: TSummaryFrame);
begin
  inherited Create(true);
  Owner := aOwner;
end;

procedure TReadSummaryInfoThread.Execute;
var
  integration: TIntegration;
  summary: TSummaryInfo;
begin
  try
    integration := TIntegration.Create;
    summary := integration.ReadSummary;
    integration.Free;
    Application.QueueAsyncCall(@Owner.ReceiveSummaryInfo, PtrInt(summary));
	except
    Application.QueueAsyncCall(@Owner.ReceiveSummaryInfo, 0);
	end;
end;

{ TReadChampionInfoThread }

constructor TReadChampionInfoThread.Create(aOwner: TSummaryFrame; const aChampionName: string);
begin
  inherited Create(true);
  Owner := aOwner;
  ChampionName := aChampionName;
end;

procedure TReadChampionInfoThread.Execute;
var
  integration: TIntegration;
  summary: TChampionWinRateSummary;
begin
  try
    integration := TIntegration.Create;
    summary := integration.ReadChampion(ChampionName);
    integration.Free;
    Application.QueueAsyncCall(@Owner.ReceiveChampionInfo, PtrInt(summary));
	except
    Application.QueueAsyncCall(@Owner.ReceiveChampionInfo, 0);
	end;
end;

end.

