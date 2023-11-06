unit SummaryFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, IntegrationUnit;

type

  { TSummaryFrame }

  TSummaryFrame = class(TFrame)
    ChampionBox: TGroupBox;
    ChampionSummaryListView: TListView;
  private
    procedure ReadInfo;
  public
    constructor Create(theOwner: TComponent); override;
  end;

implementation

{$R SummaryFrameUnit.lfm}

{ TSummaryFrame }

procedure TSummaryFrame.ReadInfo;
var
  integration: TIntegration;
begin
  integration := TIntegration.Create;
  integration.ReadSummary;
  integration.Free;
end;

constructor TSummaryFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ChampionSummaryListView.ViewStyle := vsReport;
  ReadInfo;
end;

end.

