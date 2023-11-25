unit AdviceFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  { TAdviceFrame }

  TAdviceFrame = class(TFrame)
    FoeEdit0: TLabeledEdit;
    AllyTeamBox: TGroupBox;
    EnemyTeamBox: TGroupBox;
    AllyEdit0: TLabeledEdit;
    AllyEdit2: TLabeledEdit;
    AllyEdit1: TLabeledEdit;
    AllyEdit3: TLabeledEdit;
    AllyEdit4: TLabeledEdit;
    FoeEdit1: TLabeledEdit;
    FoeEdit2: TLabeledEdit;
    FoeEdit3: TLabeledEdit;
    FoeEdit4: TLabeledEdit;
  private

  public

  end;

implementation

{$R *.lfm}

end.

