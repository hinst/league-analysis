unit ConfigurationFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Buttons, fpjson;

type

  { TConfigurationFrame }

  TConfigurationFrame = class(TFrame)
    ApiKeyEdit: TLabeledEdit;
    GameNameEdit: TLabeledEdit;
    TagLineEdit: TLabeledEdit;
    ProgressBar1: TProgressBar;
    RefreshApiKeyButton: TSpeedButton;
    SpeedButton2: TSpeedButton;
  private

  public
    procedure Initialize(configurationFilePath: string);
  end;

implementation

{$R *.lfm}

{ TConfigurationFrame }

procedure TConfigurationFrame.Initialize(configurationFilePath: string);
begin

end;

end.

