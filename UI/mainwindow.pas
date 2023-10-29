unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  RTTICtrls, UpdateFrame;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    UpdateMenuItem: TMenuItem;
    procedure UpdateMenuItemClick(Sender: TObject);
  private
    ActiveFrame: TFrame;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.UpdateMenuItemClick(Sender: TObject);
begin
  ActiveFrame := TUpdateFrameComponent.Create(self);
  ActiveFrame.Parent := self;
  ActiveFrame.Align := alClient;
end;

end.

