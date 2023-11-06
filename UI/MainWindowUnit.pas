unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  RTTICtrls, ConfigurationFrameUnit, SummaryFrameUnit;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    ConfigurationMenuItem: TMenuItem;
    SummaryMenuItem: TMenuItem;
    procedure ConfigurationMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SummaryMenuItemClick(Sender: TObject);
  private
    ActiveFrame: TFrame;
    procedure ClearFrame;
    procedure ActivateFrame(frame: TFrame);
  public
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.ConfigurationMenuItemClick(Sender: TObject);
begin
  ClearFrame;
  ActivateFrame(TConfigurationFrame.Create(self));
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  ClearFrame;
  ActivateFrame(TSummaryFrame.Create(self));
end;

procedure TMainWindow.SummaryMenuItemClick(Sender: TObject);
begin
  ClearFrame;
  ActivateFrame(TSummaryFrame.Create(self));
end;

procedure TMainWindow.ClearFrame;
begin
  if ActiveFrame <> nil then
  begin
    ActiveFrame.Free;
    ActiveFrame := nil;
  end;
end;

procedure TMainWindow.ActivateFrame(frame: TFrame);
begin
  ActiveFrame := frame;
  ActiveFrame.Parent := self;
  ActiveFrame.Align := alClient;
end;

end.

