unit ConfigurationFrameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, Buttons, LCLIntf, ConfigurationFileUnit;

type

  { TConfigurationFrame }

  TConfigurationFrame = class(TFrame)
    ApiKeyEdit: TLabeledEdit;
    SaveButton: TBitBtn;
    GameNameEdit: TLabeledEdit;
    TagLineEdit: TLabeledEdit;
    RefreshApiKeyButton: TSpeedButton;
    procedure SaveButtonClick(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure RefreshApiKeyButtonClick(Sender: TObject);
  private
    ConfigurationFile: TConfigurationFile;
    procedure ReadConfigurationFile;
    procedure WriteConfigurationFile;
  public
    constructor Create(theOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

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

destructor TConfigurationFrame.Destroy;
begin
  FreeAndNil(ConfigurationFile);
  inherited Destroy;
end;

end.

