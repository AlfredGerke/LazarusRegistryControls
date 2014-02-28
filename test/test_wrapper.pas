unit test_wrapper;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  regsourcen,
  regtype,
  fpcunit,
  regconvutils;

type

  { TWrapper }

  TWrapper<_T> = class
  private
    FRegControl: _T;
  protected
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource); virtual;
  public
    procedure CheckRootKeys(aTypeName: string;
                            aRegistrySource: TRegistrySource;
                            aRootKeys: TRootKeysStruct;
                            aCheckRTLAnsi: boolean); virtual;

    constructor Create(aRegistrySource: TRegistrySource); virtual;
    destructor Destroy; override;
  public
    property RegControl : _T
      read FRegControl;
  end;


implementation

{ TWrapper<_T> }

procedure TWrapper<_T>.SetRegistryEntries;
begin
  //
end;

procedure TWrapper<_T>.SetRegistrySettings(aRegistrySource: TRegistrySource);
begin
  FRegControl.RegistrySource := aRegistrySource;

  RegControl.RegistrySettings.CanRead := True;
  RegControl.RegistrySettings.CanWrite := True;
  RegControl.RegistrySettings.DoWriteAdHoc := True;
  RegControl.RegistrySettings.GroupIndex := 0;
  RegControl.RegistrySettings.DoSyncData := False;
end;

procedure TWrapper<_T>.CheckRootKeys(aTypeName: string;
  aRegistrySource: TRegistrySource;
  aRootKeys: TRootKeysStruct;
  aCheckRTLAnsi: boolean);
begin
  // Jeder Getter für ein String-Property besitzt ein UTF8ToSysIfNeeded

  TAssert.AssertNotNull('TypeName', aTypeName);

  TAssert.AssertEquals(Format('%s.RegistrySettings.RootKey', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.RootKey, aCheckRTLAnsi),
    RegControl.RegistrySettings.RootKey);

  TAssert.AssertEquals(Format('%s.RegistrySettings.RootKeyForDefaults', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.RootKeyForDefaults, aCheckRTLAnsi),
    RegControl.RegistrySettings.RootKeyForDefaults);

  TAssert.AssertEquals(Format('%s.RegistrySettings.RootForDefaults', [aTypeName]),
    aRegistrySource.RootForDefaults,
    RegControl.RegistrySettings.RootForDefaults);

  TAssert.AssertEquals(Format('%s.RegistrySettings.Project', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.Project, aCheckRTLAnsi),
    RegControl.RegistrySettings.Project);

  TAssert.AssertEquals(Format('%s.RegistrySettings.Organisation', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.Organisation, aCheckRTLAnsi),
    RegControl.RegistrySettings.Organisation);

  TAssert.AssertEquals(Format('%s.RegistrySettings.GUID', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.GUID, aCheckRTLAnsi),
    RegControl.RegistrySettings.GUID);

  TAssert.AssertEquals(Format('%s.RegistrySettings.ReadDefaults', [aTypeName]),
    aRegistrySource.ReadDefaults,
    RegControl.RegistrySettings.ReadDefaults);

  TAssert.AssertEquals(Format('%s.RegistrySettings.WriteDefaults', [aTypeName]),
    aRegistrySource.WriteDefaults,
    RegControl.RegistrySettings.WriteDefaults);
end;

constructor TWrapper<_T>.Create(aRegistrySource: TRegistrySource);
begin
  FRegControl := _T.Create(nil);
  SetRegistrySettings(aRegistrySource);
  SetRegistryEntries;
end;

destructor TWrapper<_T>.Destroy;
begin
  FRegControl.RegistrySource := nil;

  FreeAndNil(FRegControl);

  inherited Destroy;
end;

end.

