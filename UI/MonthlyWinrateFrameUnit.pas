unit MonthlyWinrateFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, TAGraph, TASeries, IntegrationDataUnit, Graphics;

type

  { TMonthlyWinrateFrame }

  TMonthlyWinrateFrame = class(TFrame)
    Chart: TChart;
    ChartTimeSeries: TLineSeries;
  private
  public
    procedure ShowInfo(monthlyWinRate: TMonthWinRateInfoList);
  end;

implementation

{$R *.lfm}

{ TMonthlyWinrateFrame }

procedure TMonthlyWinrateFrame.ShowInfo(monthlyWinRate: TMonthWinRateInfoList);
var
  i: Integer;
  item: TMonthWinRateInfo;
begin
  ChartTimeSeries.Clear;
  ChartTimeSeries.SeriesColor := clRed;
  for i := 0 to monthlyWinRate.Count - 1 do
  begin
    item := monthlyWinRate.Items[i];
    ChartTimeSeries.AddXY(item.Year * 12 + item.Month, item.Value.WinRate * 100, item.Key);
  end;
  Chart.BottomAxis.Marks.Source := ChartTimeSeries.Source;
end;

end.

