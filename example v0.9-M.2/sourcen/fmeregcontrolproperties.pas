unit fmeregcontrolproperties;

{$mode delphi}

interface

uses
  Classes,
  Forms,
  Controls,
  ExtCtrls,
  fmeregcontrolcaptionsettings;

type

  { TRegControlProperties }

  TRegControlProperties<_T> = class(TFrame)
    pnlCaptionSettings: TPanel;
    pnlRegistrySettings: TPanel;
    spSplitter: TSplitter;
  private
    FRegControl: _T;
    FCaptionSettingsFrame: TRegControlCaptionSettings;
    FDoCreateRegistryProperties: boolean;
    FDoCreateCaptionSettings: boolean;
  protected
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

procedure TRegControlProperties<_T>.CreatePropertyFrames;
begin
  if FDoCreateRegistryProperties then
  begin
    { TODO -oAlfred Gerke -cRegControlPropertiesFrame ausprogrammieren : An dieser Stelle muss der Frame für RegistrySettings eines RegControls erstellt werden }
  end;

  if FDoCreateCaptionSettings then
  begin
    spSplitter.Visible := True;
    pnlCaptionSettings.Visible := True;

    FCaptionSettingsFrame := TRegControlCaptionSettings.Create(pnlCaptionSettings);
    FCaptionSettingsFrame.Parent := pnlCaptionSettings;
    FCaptionSettingsFrame.Align := alClient;

    { TODO -oAlfred Gerke -cCaptionSettings zuweisen : Dem CaptionSettingFrame müssen die CaptionSettings des RegControl übergeben werden }
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

