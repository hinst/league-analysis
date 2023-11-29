unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus, Buttons,
  RTTICtrls, ConfigurationFrameUnit, SummaryFrameUnit, AdviceFrameUnit;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    SummaryButton: TBitBtn;
    ConfigurationButton: TBitBtn;
    AdviceButton: TBitBtn;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    ConfigurationMenuItem: TMenuItem;
    AdviceMenuItem: TMenuItem;
    ResetWindowSizeMenuItem: TMenuItem;
    WindowMenuItem: TMenuItem;
    SummaryMenuItem: TMenuItem;
    procedure AdviceButtonClick(Sender: TObject);
    procedure AdviceMenuItemClick(Sender: TObject);
    procedure ConfigurationButtonClick(Sender: TObject);
    procedure ConfigurationMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ResetWindowSizeMenuItemClick(Sender: TObject);
    procedure SummaryButtonClick(Sender: TObject);
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

procedure TMainWindow.AdviceMenuItemClick(Sender: TObject);
begin
  ClearFrame;
  ActivateFrame(TAdviceFrame.Create(self));
end;

procedure TMainWindow.AdviceButtonClick(Sender: TObject);
begin
  ClearFrame;
  ActivateFrame(TAdviceFrame.Create(self));
end;

procedure TMainWindow.ConfigurationButtonClick(Sender: TObject);
begin
  ClearFrame;
  ActivateFrame(TConfigurationFrame.Create(self));
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin
end;

procedure TMainWindow.ResetWindowSizeMenuItemClick(Sender: TObject);
begin
  Width := 1200;
  Height := 700;
end;

procedure TMainWindow.SummaryButtonClick(Sender: TObject);
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

