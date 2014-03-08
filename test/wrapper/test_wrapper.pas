unit test_wrapper;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  regsourcen,
  regtype;

type

  { TWrapper }

  // Grundklasse für alle spezialisierten Wrapper
  TWrapper<_T> = class
  private
    FRegControl: _T;
  protected
    procedure SetRegControl; virtual;
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); virtual;
  public
    procedure ReadFromReg(aExpected: boolean;
                          aMsg: string = ''); virtual;
    procedure WriteToReg(aExpected: boolean;
                         aMsg: string = ''); virtual;

    procedure RootKeys(aTypeName: string;
                       aRegistrySource: TRegistrySource;
                       aRootKeys: TRootKeysStruct;
                       aCheckRTLAnsi: boolean); virtual;
    procedure PublishedProperties(aMsg: string); virtual;

    constructor Create(aRegistrySource: TRegistrySource); virtual;
    destructor Destroy; override;
  public
    property RegControl : _T
      read FRegControl;
  end;

  { TWrapperCS }

  // Grundklasse für alle spezialisierten Wrapper mit CaptionSettings
  TWrapperCS<_T> = class(TWrapper<_T>)
  private
  protected
    procedure SetCaptionSettings; virtual;
    procedure DeleteCaptionEntries; virtual;
  public
    procedure ReadFromReg(aExpected: boolean;
                          aOrigin: TRegistryDataOrigin;
                          aMsg: string = ''); overload;
    procedure ReadCaption(aCaptionByDefault: string;
                          aCaptionByRegistry: string;
                          aMsg: string);

    constructor Create(aRegistrySource: TRegistrySource); override;
  end;

  // Grundklasse für alle spezialisierten Wrapper mit Listen-Control
  TWrapperLST<_T> = class(TWrapper<_T>)
  private
  protected
  public
  end;

  // Grundklasse für alle spezialisierten Wrapper mit Listen-Control und
  // CaptionSettings
  TWrapperCSLST<_T> = class(TWrapperCS<_T>)
  private
  protected
  public
  end;

implementation

uses
  test_utils,
  fpcunit,
  regconvutils;

{ TWrapper<_T> }

procedure TWrapper<_T>.SetRegControl;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

procedure TWrapper<_T>.SetRegistryEntries;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

procedure TWrapper<_T>.SetRegistrySettings(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  if aSetRegSrc then
    FRegControl.RegistrySource := aRegistrySource;

  RegControl.RegistrySettings.CanRead := True;
  RegControl.RegistrySettings.CanWrite := True;
  RegControl.RegistrySettings.DoWriteAdHoc := True;
  RegControl.RegistrySettings.GroupIndex := 0;
  RegControl.RegistrySettings.DoSyncData := False;
end;

procedure TWrapper<_T>.RootKeys(aTypeName: string;
  aRegistrySource: TRegistrySource;
  aRootKeys: TRootKeysStruct;
  aCheckRTLAnsi: boolean);
var
  root_keys: TRootKeysStruct;
begin
  // Jeder Getter für ein String-Property besitzt ein UTF8ToSysIfNeeded

  // 1. Fall: RootKeys aus dem Control mit den RootKeys aus TRegistrySource
  // vergleichen; dabei geht dieser Test davon aus das per Standard die RootKeys
  // des Controls mit den RootKeys der TRegistrySource identisch sind
  TAssert.AssertNotNull('TypeName', aTypeName);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.RootKey',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.RootKey, aCheckRTLAnsi),
    RegControl.RegistrySettings.RootKey);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.RootKeyForDefaults',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.RootKeyForDefaults, aCheckRTLAnsi),
    RegControl.RegistrySettings.RootKeyForDefaults);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.RootForDefaults',
    [aTypeName]), aRegistrySource.RootForDefaults,
    RegControl.RegistrySettings.RootForDefaults);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.Project',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.Project, aCheckRTLAnsi),
    RegControl.RegistrySettings.Project);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.Organisation',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.Organisation, aCheckRTLAnsi),
    RegControl.RegistrySettings.Organisation);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.GUID', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.GUID, aCheckRTLAnsi),
    RegControl.RegistrySettings.GUID);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.ReadDefaults',
    [aTypeName]), aRegistrySource.ReadDefaults,
    RegControl.RegistrySettings.ReadDefaults);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.WriteDefaults',
    [aTypeName]), aRegistrySource.WriteDefaults,
    RegControl.RegistrySettings.WriteDefaults);

  // 2. Fall: von TRegistrySource unterschiedliche RootKeys
  {%H-}root_keys.Clear;
  root_keys.SetRootKeys('RootKey', 'RootKeyForDefaults', True, True,
    'RootForDefaults', '%%PROJECT%%', '%%ORGANISATION%%', '%%GUID%%');

  RegControl.RegistrySettings.SetRootKeys(root_keys);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.RootKey',
    [aTypeName]), 'RootKey\', RegControl.RegistrySettings.RootKey);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.RootKeyForDefaults',
    [aTypeName]), 'RootKeyForDefaults\',
    RegControl.RegistrySettings.RootKeyForDefaults);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.RootForDefaults',
    [aTypeName]), 'RootForDefaults', RegControl.RegistrySettings.RootForDefaults);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.Project',
    [aTypeName]), '%%PROJECT%%', RegControl.RegistrySettings.Project);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.Organisation',
    [aTypeName]), '%%ORGANISATION%%', RegControl.RegistrySettings.Organisation);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.GUID', [aTypeName]),
    '%%GUID%%', RegControl.RegistrySettings.GUID);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.ReadDefaults',
    [aTypeName]), True, RegControl.RegistrySettings.ReadDefaults);

  TAssert.AssertEquals(Format('2. Fall - %s.RegistrySettings.WriteDefaults',
    [aTypeName]), True, RegControl.RegistrySettings.WriteDefaults);

  // 3. Fall: RootKeys des Controls mit den RootKeys von TRegistrySource
  // synchronisieren
  RegControl.RegistrySource.RefreshSettings;

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.RootKey',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.RootKey, aCheckRTLAnsi),
    RegControl.RegistrySettings.RootKey);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.RootKeyForDefaults',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.RootKeyForDefaults, aCheckRTLAnsi),
    RegControl.RegistrySettings.RootKeyForDefaults);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.RootForDefaults',
    [aTypeName]), aRegistrySource.RootForDefaults,
    RegControl.RegistrySettings.RootForDefaults);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.Project',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.Project, aCheckRTLAnsi),
    RegControl.RegistrySettings.Project);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.Organisation',
    [aTypeName]), UTF8ToSysIfNeeded(aRootKeys.Organisation, aCheckRTLAnsi),
    RegControl.RegistrySettings.Organisation);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.GUID', [aTypeName]),
    UTF8ToSysIfNeeded(aRootKeys.GUID, aCheckRTLAnsi),
    RegControl.RegistrySettings.GUID);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.ReadDefaults',
    [aTypeName]), aRegistrySource.ReadDefaults,
    RegControl.RegistrySettings.ReadDefaults);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.WriteDefaults',
    [aTypeName]), aRegistrySource.WriteDefaults,
    RegControl.RegistrySettings.WriteDefaults);
end;

procedure TWrapper<_T>.ReadFromReg(aExpected: boolean;
  aMsg: string = '');
var
  success: boolean;
  msg: string;
begin
  msg := _IfEmptyThen(aMsg, 'ReadFromReg');

  success := FRegControl.ReadFromReg;

  if aExpected then
    TAssert.AssertTrue(msg, success)
  else
    TAssert.AssertFalse(msg, success);
end;

procedure TWrapper<_T>.WriteToReg(aExpected: boolean;
  aMsg: string = '');
var
  success: boolean;
  msg: string;
begin
  msg := _IfEmptyThen(aMsg, 'WriteToReg');

  success := FRegControl.WriteToReg;

  if aExpected then
    TAssert.AssertTrue(msg, success)
  else
    TAssert.AssertFalse(msg, success);
end;

procedure TWrapper<_T>.PublishedProperties(aMsg: string);
begin
  TAssert.AssertEquals(Format('%s.RegistrySettings.CanRead', [aMsg]), True,
    RegControl.RegistrySettings.CanRead);
  TAssert.AssertEquals(Format('%s.RegistrySettings.CanWrite', [aMsg]), True,
    RegControl.RegistrySettings.CanWrite);
  TAssert.AssertEquals(Format('%s.RegistrySettings.DoWriteAdHoc', [aMsg]), True,
    RegControl.RegistrySettings.DoWriteAdHoc);
  TAssert.AssertEquals(Format('%s.RegistrySettings.GroupIndex', [aMsg]), 0,
    RegControl.RegistrySettings.GroupIndex);
  TAssert.AssertEquals(Format('%s.RegistrySettings.DoSyncData', [aMsg]), False,
    RegControl.RegistrySettings.DoSyncData);
end;

constructor TWrapper<_T>.Create(aRegistrySource: TRegistrySource);
begin
  FRegControl := _T.Create(nil);
  SetRegControl;
  SetRegistrySettings(aRegistrySource);
  SetRegistryEntries;
end;

destructor TWrapper<_T>.Destroy;
begin
  FRegControl.RegistrySource := nil;

  FreeAndNil(FRegControl);

  inherited Destroy;
end;

{ TWrapperCS<_T> }

procedure TWrapperCS<_T>.DeleteCaptionEntries;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

procedure TWrapperCS<_T>.ReadFromReg(aExpected: boolean;
  aOrigin: TRegistryDataOrigin;
  aMsg: string = '');
var
  success: boolean;
  msg: string;
begin
  msg := _IfEmptyThen(aMsg, 'ReadFromReg');

  success := FRegControl.ReadFromReg(aOrigin);

  if aExpected then
    TAssert.AssertTrue(msg, success)
  else
    TAssert.AssertFalse(msg, success);
end;

procedure TWrapperCS<_T>.ReadCaption(aCaptionByDefault: string;
  aCaptionByRegistry: string;
  aMsg: string);
var
  success: boolean;
begin
  //--- 1. Fall: Caption aus der Registry lesen
  RegControl.CaptionSettings.CaptionByRegistry := True;

  // Caption zurücksetzten auf Default
  RegControl.Caption := aCaptionByDefault;

  success := RegControl.ReadFromReg(rdoCaption);

  // War der Aufruf von ReadFromReg erfolgreich
  TAssert.AssertTrue('1. Fall [ReadFromReg]', success);

  // Nach lesen aus der Registry: Caption muss Registryeintrag anzeigen
  TAssert.AssertEquals(Format('1. Fall [Compare] - %s', [aMsg]),
    aCaptionByRegistry, RegControl.Caption);

  //--- 2. Fall: Voreinstellung in dem Property Caption anzeigen
  RegControl.CaptionSettings.CaptionByRegistry := False;

  // Caption zurücksetzten auf Default
  RegControl.Caption := aCaptionByDefault;

  success := RegControl.ReadFromReg(rdoCaption);

  // War der Aufruf von ReadFromReg erfolgreich
  TAssert.AssertTrue('2. Fall [ReadFromReg]', success);

  // Nach lesen aus der Registry: Caption muss Defaulteintrag anzeigen
  TAssert.AssertEquals(Format('2. Fall [Compare] - %s', [aMsg]),
    aCaptionByDefault, RegControl.Caption);

  //--- 3. Fall: Caption aus der Registry lesen
  RegControl.CaptionSettings.CaptionByRegistry := True;

  // Captionsettings in der Registry entfernen
  DeleteCaptionEntries;

  // Caption zurücksetzten auf Default
  RegControl.Caption := aCaptionByDefault;

  success := RegControl.ReadFromReg(rdoCaption);

  // War der Aufruf von ReadFromReg erfolgreich
  TAssert.AssertTrue('3. Fall [ReadFromReg]', success);

  // Nach lesen aus der Registry: Da keine Captionsettings vorhanden
  // Defaulteintrag anzeigen
  TAssert.AssertEquals(Format('3. Fall [Compare] - %s', [aMsg]),
    aCaptionByDefault, RegControl.Caption);
end;

constructor TWrapperCS<_T>.Create(aRegistrySource: TRegistrySource);
begin
  inherited Create(aRegistrySource);

  SetCaptionSettings;
end;

procedure TWrapperCS<_T>.SetCaptionSettings;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

end.

