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
  public type
    TOnRefreshSettings = procedure of object;
  private
    FRegComponent: TComponent;
    FDoCreateRegistryProperties: boolean;
    FDoCreateCaptionSettings: boolean;

    procedure CreateCaptionSettingsFrame;
  protected
    procedure CreateRegistrySettingsFrame; virtual; abstract;
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
    class procedure FreeFrame(aPanel: TPanel);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetRegComponent(AComponent: TComponent);
  end;

implementation

{$R *.lfm}

uses
  SysUtils;

{ TRegControlProperties }

class procedure TRegControlProperties.FreeFrame(aPanel: TPanel);
var
  anz: integer;
  frame: TFrame;
begin
  for anz := 0 to aPanel.ComponentCount-1 do
  begin
    if (aPanel.Components[anz] is TFrame) then
    begin
      frame := TFrame(aPanel.Components[anz]);
      FreeAndNil(frame);
      Exit;
    end;
  end;
end;

procedure TRegControlProperties.CreateCaptionSettingsFrame;
var
  caption_settings: TCaptionSettings;
begin
  with TRegControlCaptionSettings.Create(self.pnlCaptionSettings) do
  begin
    Parent := self.pnlCaptionSettings;
    Align := alClient;

    pnlCaptionSettings.Show;
    spSplitter.Show;

    caption_settings := self.GetRegControlCaptionSettings;
    if Assigned(caption_settings) then
      SetRegControlSettings(caption_settings);
  end;
end;

function TRegControlProperties.GetRegControlCaptionSettings: TCaptionSettings;
begin
  // Nur wenn DoCreateCaptionSettings=True in einer Ableitung überschreiben
  Result := nil;
end;

procedure TRegControlProperties.CreatePropertyFrames;
begin
  if FDoCreateRegistryProperties then
    CreateRegistrySettingsFrame;

  if FDoCreateCaptionSettings then
    CreateCaptionSettingsFrame;
end;

procedure TRegControlProperties._Initialize;
begin
  // In einer Ableitung überschreiben
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

destructor TRegControlProperties.Destroy;
begin
  if FDoCreateCaptionSettings then
    TRegControlProperties.FreeFrame(pnlCaptionSettings);
  
  if FDoCreateRegistryProperties then
    TRegControlProperties.FreeFrame(pnlRegistrySettings);

  inherited Destroy;
end;

end.

