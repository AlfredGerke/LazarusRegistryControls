program example_09_M2;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  datRegistry,
  frmMain,
  fmeControlDetails,
  fmecustomcontrolframe,
  fmecustomsettings,
  fmeregcontrolproperties,
  fmeregcontrolcaptionsettings,
  fmereglistbox,
  fmereglistboxproperties,
  fmeregcheckbox,
  fmeregcheckboxproperties,
  fmeregistrysettingsbooleandefault;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TRegistrySourceModule, RegistrySourceModule);
  Application.Run;
end.

