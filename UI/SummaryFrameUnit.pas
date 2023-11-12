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
    procedure ReadChampionInfo;
  protected
    procedure ReceiveSummaryInfo(pSummary: PtrInt);
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

{ TReadInfoThread }

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

{ TSummaryFrame }

procedure TSummaryFrame.ChampionSummaryListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  ClearChampionFrame;
  ChampionFrame := TAlliesAndEnemiesFrame.Create(self);
  ChampionFrame.Parent := ChampionInfoTabs;
  ChampionFrame.Align := alClient;
  ChampionFrame.Color := clDefault;
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
  thread := TReadSummaryInfoThread.Create(self);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TSummaryFrame.ReadChampionInfo;
begin

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
end;

constructor TSummaryFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ChampionSummaryListView.ViewStyle := vsReport;
  ReadSummaryInfo;
end;

end.

