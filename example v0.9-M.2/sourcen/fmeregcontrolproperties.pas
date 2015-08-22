unit fmeregcontrolproperties;

{$mode delphi}

interface

uses
  Classes,
  Forms,
  Controls,
  ExtCtrls, ValEdit,
  fmeregcontrolcaptionsettings,
  regsourcen;

type

  { TRegControlProperties }

  TRegControlProperties<_T> = class(TFrame)
    pnlCaptionSettings: TPanel;
    pnlRegistrySettings: TPanel;
    spSplitter: TSplitter;
    ValueListEditor1: TValueListEditor;
  private
    FRegControl: _T;
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

    property RegControl: _T
      read FRegControl;
  public
    constructor Create(aOwner: TComponent); override;

    procedure SetRegControl(AControl: _T);
  end;

implementation

{$R *.lfm}

{ TRegControlProperties<_T> }

function TRegControlProperties<_T>.GetRegControlCaptionSettings: TCaptionSettings;
begin
  // Nur wenn DoCreateCaptionSettings=True überschreiben
  Result := nil;
end;

procedure TRegControlProperties<_T>.CreatePropertyFrames;
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

    { TODO -oAlfred Gerke -cCaptionSettings zuweisen : Dem CaptionSettingFrame müssen die CaptionSettings des RegControl übergeben werden }
    caption_settings := self.GetRegControlCaptionSettings;
    if Assigned(caption_settings) then
      FCaptionSettingsFrame.SetRegControlSettings(caption_settings);
  end;
end;

procedure TRegControlProperties<_T>._Initialize;
begin
  // In der Ableitung überschreiben
end;

procedure TRegControlProperties<_T>.SetRegControl(AControl: _T);
begin
  FRegControl := aControl;

  CreatePropertyFrames;
end;

constructor TRegControlProperties<_T>.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FDoCreateRegistryProperties := True;
  FDoCreateCaptionSettings := False;

  _Initialize;
end;

end.

