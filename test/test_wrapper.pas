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
    procedure SetCaptionSettings; virtual;
  public
    procedure RootKeys(aTypeName: string;
                       aRegistrySource: TRegistrySource;
                       aRootKeys: TRootKeysStruct;
                       aCheckRTLAnsi: boolean); virtual;
    procedure ReadFromReg(aExpected: boolean;
                          aMsg: string = ''); virtual;
    procedure WriteToReg(aExpected: boolean;
                         aMsg: string = ''); virtual;

    { TODO 1 -oAlfred Gerke -cRegistrySettings lesen/schreiben : Lesen von Registrywerten testen:
1. Fall CanRead = False
2. Fall CanRead = True
Schreiben von Registrywerten testen:
3. Fall DoWriteAdHoc = True
3.1 Fall CanWrite = False
3.2 Fall Can Write = True
4. Fall DoWriteAdHoc = False
4.1 Fall CanWrite = False
4.2 Fall Can Write = True
Lesen von CaptionSettings:
5. Fall CaptionByRegistry = True
6. Fall CaptionByRegistry = False}

    procedure ReadRegistry;
    procedure WriteRegistry;

    constructor Create(aRegistrySource: TRegistrySource); virtual;
    destructor Destroy; override;
  public
    property RegControl : _T
      read FRegControl;
  end;

  { TWrapperCS }

  TWrapperCS<_T> = class(TWrapper<_T>)
  private
  protected
    procedure DeleteCaptionEntries; virtual;
    procedure ReadCaption(aCaptionByDefault: string;
                          aCaptionByRegistry: string;
                          aMsg: string);
  public
  end;

function _IfEmptyThen(aString: string;
                      aDefault: string): string;

implementation

function _IfEmptyThen(aString: string;
  aDefault: string): string;
begin
  if (Trim(aString) = EmptyStr) then
    Result := aDefault
  else
    Result := aString;
end;

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

procedure TWrapper<_T>.SetCaptionSettings;
begin
  //
end;

procedure TWrapper<_T>.RootKeys(aTypeName: string;
  aRegistrySource: TRegistrySource;
  aRootKeys: TRootKeysStruct;
  aCheckRTLAnsi: boolean);
//var
//  root_keys: TRootKeysStruct;
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
  //root_keys.Clear;
  //
  // hier neur RootKeys eintragen
  //
  // RegControl.RegistrySettings.SetRootKesy(rook_keys);
  //
  // hier RootKeys vergleichen

  // 3. Fall: RootKeys des Controls mit den RootKeys von TRegistrySource
  // synchronisieren
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

procedure TWrapper<_T>.ReadRegistry;
begin

end;

procedure TWrapper<_T>.WriteRegistry;
begin

end;

constructor TWrapper<_T>.Create(aRegistrySource: TRegistrySource);
begin
  FRegControl := _T.Create(nil);
  SetRegistrySettings(aRegistrySource);
  SetCaptionSettings;
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
  //
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

end.

