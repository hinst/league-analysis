program league_analysis;

{$apptype console}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, MainWindowUnit, ConfigurationFrameUnit,
  ConfigurationFileUnit, FilesUnit, JsonUnit, SummaryFrameUnit, IntegrationUnit,
  IntegrationDataUnit, CommonUnit
  { you can add units after this }
  , SysUtils;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  ExecutableDirectory := ExtractFileDir(Application.ExeName);
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end.

