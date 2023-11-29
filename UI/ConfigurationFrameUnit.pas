unit ConfigurationFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Buttons, LCLIntf, StdCtrls,
  ConfigurationFileUnit, IntegrationUnit;

type

  { TConfigurationFrame }

  TConfigurationFrame = class(TFrame)
    ApiKeyEdit: TLabeledEdit;
    UpdateButton: TBitBtn;
    ConfigurationBox: TGroupBox;
    UpdateResultMemo: TMemo;
    UpdateBox: TGroupBox;
    SaveButton: TBitBtn;
    GameNameEdit: TLabeledEdit;
    TagLineEdit: TLabeledEdit;
    RefreshApiKeyButton: TSpeedButton;
    procedure SaveButtonClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure RefreshApiKeyButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
  public
    constructor Create(theOwner: TComponent); override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  private
    ConfigurationFile: TConfigurationFile;
    UpdateThread: TThread;
    procedure ReadConfigurationFile;
    procedure WriteConfigurationFile;
  end;

implementation

{$R *.lfm}

type

  { TUpdateReaderThread }

  TUpdateReaderThread = class(TThread)
  public
    OutputText: string;
    UpdateResult: boolean;
    constructor Create(owner: TConfigurationFrame);
  protected
    procedure Execute; override;
    procedure ShowOutput;
  private
    Owner: TConfigurationFrame;
  end;

{ TConfigurationFrame }

procedure TConfigurationFrame.FrameEnter(Sender: TObject);
begin
  ReadConfigurationFile;
end;

procedure TConfigurationFrame.SaveButtonClick(Sender: TObject);
begin
  WriteConfigurationFile;
end;

procedure TConfigurationFrame.RefreshApiKeyButtonClick(Sender: TObject);
begin
  OpenURL('https://developer.riotgames.com');
end;

procedure TConfigurationFrame.UpdateButtonClick(Sender: TObject);
begin
  if UpdateThread = nil then
  begin
    UpdateResultMemo.Clear;
    UpdateButton.Enabled := false;
    UpdateThread := TUpdateReaderThread.Create(self);
    UpdateThread.FreeOnTerminate := true;
    UpdateThread.Start;
  end;
end;

procedure TConfigurationFrame.ReadConfigurationFile;
begin
  ConfigurationFile.Read;
  GameNameEdit.Text := ConfigurationFile.GameName;
  TagLineEdit.Text := ConfigurationFile.TagLine;
  ApiKeyEdit.Text := ConfigurationFile.ApiKey;
end;

procedure TConfigurationFrame.WriteConfigurationFile;
begin
  ConfigurationFile.GameName := GameNameEdit.Text;
  ConfigurationFile.TagLine := TagLineEdit.Text;
  ConfigurationFile.ApiKey := ApiKeyEdit.Text;
  ConfigurationFile.Write;
end;

constructor TConfigurationFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ConfigurationFile := TConfigurationFile.Create;
  ReadConfigurationFile;
end;

procedure TConfigurationFrame.BeforeDestruction;
begin
  inherited BeforeDestruction;
  while UpdateThread <> nil do
    Sleep(100);
end;

destructor TConfigurationFrame.Destroy;
begin
  FreeAndNil(ConfigurationFile);
  inherited Destroy;
end;

{ TUpdateReaderThread }

constructor TUpdateReaderThread.Create(owner: TConfigurationFrame);
begin
  inherited Create(true);
  self.Owner := owner;
end;

procedure TUpdateReaderThread.Execute;
var
  integration: TIntegration;
begin
  integration := TIntegration.Create;
  UpdateResult := integration.Update(OutputText);
  integration.Free;
  Synchronize(@ShowOutput);
end;

procedure TUpdateReaderThread.ShowOutput;
begin
  try
    Owner.UpdateResultMemo.Lines.Text := OutputText;
    if not UpdateResult then
      Owner.UpdateResultMemo.Lines.Add('An error occurred');
    Owner.UpdateButton.Enabled := true;
    Owner.UpdateThread := nil;
  except
    on e: Exception do
      WriteLn(e.Message);
  end;
end;

end.

