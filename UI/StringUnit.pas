unit StringUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CheckArraysEqual(const a1: TStringArray; const a2: TStringArray): Boolean;

implementation

function CheckArraysEqual(const a1: TStringArray; const a2: TStringArray): Boolean;
var
  i: Integer;
begin
  if Length(a1) <> Length(a2) then
    Exit(false);
  for i := 0 to Length(a1) do
    if a1[i] <> a2[i] then
      Exit(false);
  result := true;
end;

end.

