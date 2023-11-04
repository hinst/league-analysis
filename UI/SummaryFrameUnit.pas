unit SummaryFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls;

type

  { TSummaryFrame }

  TSummaryFrame = class(TFrame)
    ChampionBox: TGroupBox;
    ChampionSummaryListView: TListView;
  private
  public
    constructor Create(theOwner: TComponent); override;
  end;

implementation

{$R SummaryFrameUnit.lfm}

{ TSummaryFrame }

constructor TSummaryFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ChampionSummaryListView.ViewStyle := vsReport;
end;

end.

