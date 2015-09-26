unit datRegistry;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  regsourcen;

type

  { TRegistrySourceModule }

  TRegistrySourceModule = class(TDataModule)
    rsRegistrySource: TRegistrySource;
  private
  public
  end;

var
  RegistrySourceModule: TRegistrySourceModule;

implementation

{$R *.lfm}

uses
  SysUtils,
  Dialogs;

{ TRegistrySourceModule }

end.

