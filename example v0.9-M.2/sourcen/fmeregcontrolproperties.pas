unit fmeregcontrolproperties;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  ExtCtrls,
  reglistbox,
  fmeregcontrolcaptionsettings,
  regsourcen;

type

  { TRegControlProperties }

  TRegControlProperties<_T> = class(TFrame)
    pnlCaptionSettings: TPanel;
    pnlRegistrySettings: TPanel;
  private
    FRegControl: _T;
    FCaptionSettingsFrame: TRegControlCaptionSettings;

    FDoCreateRegistryProperties: boolean;
    FDoCreateCaptionProperites: boolean;

    procedure CreatePropertyFrames;
  protected
    procedure _Initialize; virtual;

    property DoCreateRegistryProperties: boolean
      read FDoCreateRegistryProperties
      write FDoCreateRegistryProperties;

    property DoCreateCaptionProperites: boolean
      read FDoCreateCaptionProperites
      write FDoCreateCaptionProperites;

    property RegControl: _T
      read FRegControl;
  public
    constructor Create(aOwner: TComponent); override;

    procedure SetRegControl(AControl: _T);
  end;

implementation

{$R *.lfm}

{ TRegControlProperties<_T> }

procedure TRegControlProperties<_T>.CreatePropertyFrames;
begin
  if FDoCreateRegistryProperties then
  begin
    { TODO -oAlfred Gerke -cRegControlPropertiesFrame ausprogrammieren : An dieser Stelle muss der Frame für RegistrySettings eines RegControls erstellt werden }
  end;

  if FDoCreateCaptionProperites then
  begin
    FCaptionSettingsFrame := TRegControlCaptionSettings.Create(pnlCaptionSettings);
    FCaptionSettingsFrame.Parent := pnlCaptionSettings;
    FCaptionSettingsFrame.Align := alClient;

    { TODO -oAlfred Gerke -cRegControlPropertiesFrame ausprogrammieren : An dieser Stellen müssen die CaptionSettings eines RegControls übergeben werden }
    FCaptionSettingsFrame.SetRegControlSettings(nil);
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
  FDoCreateCaptionProperites := False;

  _Initialize;
end;

end.

