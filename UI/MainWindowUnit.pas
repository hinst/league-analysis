unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  RTTICtrls, ConfigurationFrameUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    ConfigurationMenuItem: TMenuItem;
    procedure ConfigurationMenuItemClick(Sender: TObject);
  private
    ActiveFrame: TFrame;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ConfigurationMenuItemClick(Sender: TObject);
begin
  ActiveFrame := TConfigurationFrame.Create(self);
  ActiveFrame.Parent := self;
  ActiveFrame.Align := alClient;
end;

end.

