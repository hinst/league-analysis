unit CommonUnit;

{$mode ObjFPC}{$H+}

interface

uses Forms, SysUtils, Classes;

var
  ExecutableDirectory: string;

const
  SleepWhenExitingMilliseconds = 30;

procedure WaitForEmptyThreadList(threads: TThreadList);

implementation

procedure WaitForEmptyThreadList(threads: TThreadList);
var
  threadList: TList;
  threadCount: Integer;
begin
  while true do
  begin
    threadList := Threads.LockList;
    threadCount := threadList.Count;
    Threads.UnlockList;
    if threadCount = 0 then
      break;
    Sleep(SleepWhenExitingMilliseconds);
    Application.ProcessMessages;
  end;
end;

end.

