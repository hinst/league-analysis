unit StringUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function CheckArraysEqual(const a1: TStringArray; const a2: TStringArray): Boolean;
function CheckArrayEmpty(const a: TStringArray): Boolean;

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

function CheckArrayEmpty(const a: TStringArray): Boolean;
var
  s: string;
begin
  for s in a do
    if s <> '' then
      Exit(False);
  Result := True;
end;

end.

