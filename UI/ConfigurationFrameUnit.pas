unit ConfigurationFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Buttons;

type

  { TConfigurationFrame }

  TConfigurationFrame = class(TFrame)
    ApiKeyEdit: TLabeledEdit;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
  private

  public

  end;

implementation

{$R *.lfm}

end.

