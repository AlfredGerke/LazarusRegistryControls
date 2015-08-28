unit fmeregistrysourceproperties;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ValEdit;

type

  { TRegistrySourceProperties }

  TRegistrySourceProperties = class(TFrame)
    lblRegistrySettings: TLabel;
    ValueListEditor1: TValueListEditor;
  private
    procedure SetSettings;
  public
    constructor Create(aOwner: TComponent); override;

    procedure RefreshSettings;
  end;

implementation

{$R *.lfm}

uses
  datregistry;

{ TRegistrySourceProperties }

procedure TRegistrySourceProperties.SetSettings;
begin
  with ValueListEditor1.Strings, RegistrySourceModule.rsRegistrySource do
  begin
    Clear;
    Add('RootKey=' + RootKey);
    Add('RootKeyForDefaults=' + RootKeyForDefaults);
    Add('RootKeyForCommon=' + RootKeyForCommon);
    Add('Project=' + Project);
    Add('Organisation=' + Organisation);
    Add('RootForDefaults=' + RootForDefaults);
    Add('ReadDefaults=' + BoolToStr(ReadDefaults, True));
    Add('WriteDefaults=' + BoolToStr(WriteDefaults, True));
    Add('GUID=' + GUID);
    Add('DoSyncData=' + BoolToStr(DoSyncData, True));
    Add('PrefereStrings=' + BoolToStr(PrefereStrings, True));
    Add('CheckRTLAnsi=' + BoolToStr(CheckRTLAnsi, True));
  end
end;

constructor TRegistrySourceProperties.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  SetSettings;
end;

procedure TRegistrySourceProperties.RefreshSettings;
begin
  SetSettings;
end;

end.

