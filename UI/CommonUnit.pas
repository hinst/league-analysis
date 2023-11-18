unit CommonUnit;

{$mode ObjFPC}{$H+}

interface

uses SysUtils;

var
  ExecutableDirectory: string;

function FormatPercent(value: Double): string;

implementation

function FormatPercent(value: Double): string;
begin
  result := FloatToStrF(value * 100, ffFixed, 0, 1);
end;

end.

