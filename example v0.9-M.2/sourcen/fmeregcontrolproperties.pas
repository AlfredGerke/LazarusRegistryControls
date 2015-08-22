unit fmeregcontrolproperties;

{$mode delphi}

interface

uses
  Classes,
  Forms,
  Controls,
  ExtCtrls,
  fmeregcontrolcaptionsettings,
  regsourcen;

type

  { TRegControlProperties }

  TRegControlProperties = class(TFrame)
    pnlCaptionSettings: TPanel;
    pnlRegistrySettings: TPanel;
    spSplitter: TSplitter;
  private
    FRegComponent: TComponent;
    FCaptionSettingsFrame: TRegControlCaptionSettings;
    FDoCreateRegistryProperties: boolean;
    FDoCreateCaptionSettings: boolean;
  protected
    function GetRegControlCaptionSettings: TCaptionSettings; virtual;
    procedure CreatePropertyFrames; virtual;

    procedure _Initialize; virtual;

    property DoCreateRegistryProperties: boolean
      read FDoCreateRegistryProperties
      write FDoCreateRegistryProperties;

    property DoCreateCaptionSettings: boolean
      read FDoCreateCaptionSettings
      write FDoCreateCaptionSettings;

    property RegComponent: TComponent
      read FRegComponent;
  public
    constructor Create(aOwner: TComponent); override;

    procedure SetRegComponent(AComponent: TComponent);
  end;

implementation

{$R *.lfm}

{ TRegControlProperties }

function TRegControlProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  // Nur wenn DoCreateCaptionSettings=True überschreiben
  Result := nil;
end;

procedure TRegControlProperties.CreatePropertyFrames;
var
  caption_settings: TCaptionSettings;
begin
  if FDoCreateRegistryProperties then
  begin
    { TODO -oAlfred Gerke -cRegControlPropertiesFrame ausprogrammieren : An dieser Stelle muss der Frame für RegistrySettings eines RegControls erstellt werden }
  end;

  if FDoCreateCaptionSettings then
  begin
    FCaptionSettingsFrame := TRegControlCaptionSettings.Create(self.pnlCaptionSettings);
    FCaptionSettingsFrame.Parent := self.pnlCaptionSettings;
    FCaptionSettingsFrame.Align := alClient;

    self.pnlCaptionSettings.Show;
    self.spSplitter.Show;

    caption_settings := self.GetRegControlCaptionSettings;
    if Assigned(caption_settings) then
      FCaptionSettingsFrame.SetRegControlSettings(caption_settings);
  end;
end;

procedure TRegControlProperties._Initialize;
begin
  // In der Ableitung überschreiben
end;

procedure TRegControlProperties.SetRegComponent(AComponent: TComponent);
begin
  FRegComponent := AComponent;

  CreatePropertyFrames;
end;

constructor TRegControlProperties.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FDoCreateRegistryProperties := True;
  FDoCreateCaptionSettings := False;

  _Initialize;
end;

end.

