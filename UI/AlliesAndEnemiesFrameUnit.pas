unit AlliesAndEnemiesFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls;

type

		{ TAlliesAndEnemiesFrame }

    TAlliesAndEnemiesFrame = class(TFrame)
				BestAlliesBox: TGroupBox;
				WorstAlliesBox: TGroupBox;
				EasiestEnemiesBox: TGroupBox;
				HardestEnemiesBox: TGroupBox;
				procedure WorstAlliesBoxClick(Sender: TObject);
    private

    public

    end;

implementation

{$R *.lfm}

{ TAlliesAndEnemiesFrame }

procedure TAlliesAndEnemiesFrame.WorstAlliesBoxClick(Sender: TObject);
begin

end;

end.

