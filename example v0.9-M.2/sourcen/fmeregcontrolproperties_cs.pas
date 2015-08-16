unit fmeregcontrolproperties_cs;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  fmeregcontrolproperties,
  fmeregcontrolcaptionsettings;

type

  { TRegControlPropertiesCS }

  TRegControlPropertiesCS = class(TRegControlProperties)
    pnlCaptionSettings: TPanel;
  private
    FCaptionSettingsFrame: TRegControlCaptionSettings;
    FDoCreateCaptionSettings: boolean;
  protected
    procedure CreatePropertyFrames; override;

    procedure _Initialize; virtual;

    property DoCreateCaptionSettings: boolean
      read FDoCreateCaptionSettings
      write FDoCreateCaptionSettings;
  end;

implementation

{$R *.lfm}

{ TRegControlPropertiesCS }

procedure TRegControlPropertiesCS.CreatePropertyFrames;
begin
  // Immer übergeben um den Frame für RegistrySettings zu erstellen
  inherited CreatePropertyFrames;

  if FDoCreateCaptionSettings then
  begin
    FCaptionSettingsFrame
  end;
end;

procedure TRegControlPropertiesCS._Initialize;
begin
  FDoCreateRegistryProperties := True;
end;

end.

