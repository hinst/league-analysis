unit SummaryFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, IntegrationUnit, IntegrationDataUnit;

type

  { TSummaryFrame }

  TSummaryFrame = class(TFrame)
    ChampionBox: TGroupBox;
    ChampionSummaryListView: TListView;
  private
    procedure ReadSummaryInfo;
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
  summary: TSummaryInformation;
begin
  integration := TIntegration.Create;
  summary := integration.ReadSummary;
  integration.Free;
  Application.QueueAsyncCall(@Owner.ReceiveSummaryInfo, PtrInt(summary));
end;

{ TSummaryFrame }

procedure TSummaryFrame.ReadSummaryInfo;
var
  thread: TReadSummaryInfoThread;
begin
  thread := TReadSummaryInfoThread.Create(self);
  thread.FreeOnTerminate := true;
  thread.Start;
end;

procedure TSummaryFrame.ReceiveSummaryInfo(pSummary: {*TSummaryInformation*} PtrInt);
var
  summary: TSummaryInformation;
  i: Integer;
  champion: TUserChampionSummary;
begin
  summary := TSummaryInformation(pSummary);
  ChampionSummaryListView.Clear;
  if summary <> nil then
  begin
    if summary.UserChampions <> nil then
    begin
  		ChampionBox.Caption := 'Your champions: ' + IntToStr(summary.UserChampions.Count);
      for i := 0 to summary.UserChampions.Count - 1 do
      begin
        champion := summary.UserChampions.Items[i];
        ChampionSummaryListView.AddItem(champion.ChampionName, nil);
        ChampionSummaryListView.Items[ChampionSummaryListView.Items.Count - 1]
          .SubItems.Add(IntToStr(champion.MatchCount));
        ChampionSummaryListView.Items[ChampionSummaryListView.Items.Count - 1]
          .SubItems.Add(FloatToStrF(champion.WinRate * 100, ffFixed, 0, 1));
			end;
		end
    else
    begin
      ChampionBox.Caption := 'Your champions: not found';
		end;
	end;
  summary.Free;
end;

constructor TSummaryFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ChampionSummaryListView.ViewStyle := vsReport;
  ReadSummaryInfo;
end;

end.

