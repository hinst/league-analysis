unit AlliesAndEnemiesFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, IntegrationDataUnit;

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
  public
    procedure ShowInfo(summary: TChampionWinRateSummary);
  end;

implementation

{$R *.lfm}

{ TAlliesAndEnemiesFrame }

procedure TAlliesAndEnemiesFrame.ShowInfo(summary: TChampionWinRateSummary);
begin

end;

end.

