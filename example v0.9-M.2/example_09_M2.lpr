{ TODO 3 -oAGE -cProjektpflege : Prüfen ob regsource.pas noch im Projekt vorhanden ist und wenn ja entfernen }
program example_09_M2;

{$mode Delphi}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}

  {$ifdef Debug}
  SysUtils,
  {$endif}

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
  fmeregistrysettingsbooleandefault,
  fmeregistrylistsettings,
  fmeregcontrolbuttonframe,
  fmeregistrysource,
  fmeregistrysourceproperties,
  fmeregistrysourcebuttonframe,
  fmeregedit,
  fmeregeditproperties,
  fmeregistrysettingsstringdefault,
  fmereglabel,
  fmereglabelproperties,
  fmeregradiobutton,
  fmeregradiobuttonproperties,
  fmeregcombobox,
  fmeregcomboboxproperties,
  fmeregradiogroup,
  fmeregradiogroupproperties,
  fmeregistrysettingscheckedlist,
  fmeregchecklistbox,
  fmeregchecklistboxproperties,
  fmeregcheckgroup,
  fmeregcheckgroupproperties,
  fmeregvaluelisteditor,
  fmeregistrysettingsvaluelist,
  fmeregvaluelisteditorproperties;

{$R *.res}

begin
  // Leak and Trace einrichten
  // example.trc: im selben Ordner wie die Ausführungsdatei
  {$ifdef Debug}
  if FileExists('example.trc') then
    DeleteFile('example.trc');
  SetHeapTraceOutput('example.trc');
  {$endif}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.CreateForm(TRegistrySourceModule, RegistrySourceModule);
  Application.Run;
end.
