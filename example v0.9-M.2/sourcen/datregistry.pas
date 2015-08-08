unit datRegistry;

{$mode delphi}

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

end.

