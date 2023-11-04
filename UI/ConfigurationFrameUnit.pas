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
    ConfigurationFileUnit: TConfigurationFile;
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
  ConfigurationFileUnit.Read;
  GameNameEdit.Text := ConfigurationFileUnit.GameName;
  TagLineEdit.Text := ConfigurationFileUnit.TagLine;
  ApiKeyEdit.Text := ConfigurationFileUnit.ApiKey;
end;

procedure TConfigurationFrame.WriteConfigurationFile;
begin
  ConfigurationFileUnit.GameName := GameNameEdit.Text;
  ConfigurationFileUnit.TagLine := TagLineEdit.Text;
  ConfigurationFileUnit.ApiKey := ApiKeyEdit.Text;
  ConfigurationFileUnit.Write;
end;

constructor TConfigurationFrame.Create(theOwner: TComponent);
begin
  inherited Create(theOwner);
  ConfigurationFileUnit := TConfigurationFile.Create;
  ReadConfigurationFile;
end;

destructor TConfigurationFrame.Destroy;
begin
  if ConfigurationFileUnit <> nil then
  begin
     ConfigurationFileUnit.Free;
     ConfigurationFileUnit := nil;
  end;
  inherited Destroy;
end;

end.

