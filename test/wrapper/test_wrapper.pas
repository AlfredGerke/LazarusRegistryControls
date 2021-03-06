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
    FCanRead: boolean;
    FCanWrite: boolean;
    FDoWriteAdHoc: boolean;
    FGroupIndex: integer;
    FDoSyncData: boolean;

    FSection: string;
    FIdent: string;
  protected
    procedure _Initialize; virtual;
    procedure _Finalize; virtual;
    function SetUniqueName(aName: string): string;
    procedure SetRegControl; virtual;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); virtual;

    property Section: string
      read FSection
      write FSection;

    property Ident: string
      read FIdent
      write FIdent;
  public
    procedure SetRegistryEntries; virtual;

    procedure ReadFromReg(aExpected: boolean;
                          aMsg: string = ''); virtual;
    procedure WriteToReg(aExpected: boolean;
                         aMsg: string = ''); virtual;

    procedure RootKeys(aTypeName: string;
                       aRegistrySource: TRegistrySource;
                       aRootKeys: TRootKeysStruct;
                       aCheckRTLAnsi: boolean); virtual;
    procedure PublishedProperties(aMsg: string); virtual;

    constructor Create(aRegistrySource: TRegistrySource;
                       aSetRegSrc: boolean = True;
                       aCanRead: boolean = True;
                       aCanWrite: boolean = True;
                       aDoWriteAdHoc: boolean = True;
                       aGroupIndex: integer = 0;
                       aDoSyncData: boolean = False); virtual;
    destructor Destroy; override;
  public
    property RegControl : _T
      read FRegControl;
  end;

  { TWrapperCS }

  // Grundklasse für alle spezialisierten Wrapper mit CaptionSettings
  TWrapperCS<_T> = class(TWrapper<_T>)
  private
    FCaptionSection: string;
    FCaptionIdent: string;
    FCaptionValueByReg: string;
  protected
    procedure SetCaptionSettings; virtual;
    procedure DeleteCaptionEntries; virtual;

    property CaptionSection: string
      read FCaptionSection
      write FCaptionSection;

    property CaptionIdent: string
      read FCaptionIdent
      write FCaptionIdent;

    property CaptionValueByReg: string
      read FCaptionValueByReg
      write FCaptionValueByReg;
  public
    procedure ReadFromReg(aExpected: boolean;
                          aOrigin: TRegistryDataOrigin;
                          aMsg: string = ''); overload;
    procedure ReadCaption(aCaptionByDefault: string;
                          aCaptionByRegistry: string;
                          aMsg: string);

    constructor Create(aRegistrySource: TRegistrySource;
                       aSetRegSrc: boolean = True;
                       aCanRead: boolean = True;
                       aCanWrite: boolean = True;
                       aDoWriteAdHoc: boolean = True;
                       aGroupIndex: integer = 0;
                       aDoSyncData: boolean = False); override;
  end;

  // Alle zusätzlichen Eigenschaften für ListenControls

  { TExtraListRegProperties }

  TExtraListRegProperties = record
    DoMergeData: boolean;
    ItemsByRegistry: boolean;
    ListSection: string;
    SourceKind: TListSourceKind;

    procedure AddListSection(aListSection: string);

    procedure Clear;
  end;

  // Grundklasse für alle spezialisierten Wrapper mit Listen-Control

  { TWrapperLST }

  TWrapperLST<_T> = class(TWrapper<_T>)
  private
     FListProperties: TExtraListRegProperties;
  protected
    procedure _Initialize; override;

    property SpecialRegistryPropertiesForList: TExtraListRegProperties
      read FListProperties
      write FListProperties;
  public
    constructor Create(aRegistrySource: TRegistrySource;
                       aSetRegSrc: boolean = True;
                       aCanRead: boolean = True;
                       aCanWrite: boolean = True;
                       aDoWriteAdHoc: boolean = True;
                       aGroupIndex: integer = 0;
                       aDoSyncData: boolean = False;
                       aDoMergeData: boolean = False;
                       aItemsByRegistry: boolean = True;
                       aSourceKind: TListSourceKind = lskByKey); reintroduce; overload;

    property SpecialListProperties: TExtraListRegProperties
      read FListProperties
      write FListProperties;
  end;

  // Grundklasse für alle spezialisierten Wrapper mit Listen-Control und
  // CaptionSettings
  TWrapperCSLST<_T> = class(TWrapperCS<_T>)
  private
    FListProperties: TExtraListRegProperties;
  protected
  public
  end;

implementation

uses
  test_utils,
  fpcunit;

{ TWrapper<_T> }

procedure TWrapper<_T>._Initialize;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

procedure TWrapper<_T>._Finalize;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

function TWrapper<_T>.SetUniqueName(aName: string): string;
begin
  Result := Format('%s%d', [aName, GetNextCount]);
end;

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

  RegControl.RegistrySettings.CanRead := FCanRead;
  RegControl.RegistrySettings.CanWrite := FCanWrite;
  RegControl.RegistrySettings.DoWriteAdHoc := FDoWriteAdHoc;
  RegControl.RegistrySettings.GroupIndex := FGroupIndex;
  RegControl.RegistrySettings.DoSyncData := FDoSyncData;
end;

procedure TWrapper<_T>.RootKeys(aTypeName: string;
  aRegistrySource: TRegistrySource;
  aRootKeys: TRootKeysStruct;
  aCheckRTLAnsi: boolean);
var
  root_keys: TRootKeysStruct;
begin
  // Jeder Getter für ein String-Property besitzt ein UTF8ToSysIfNeeded

  TAssert.AssertEquals('CheckRTLAnsi', aCheckRTLAnsi, RegControl.RegistrySource.CheckRTLAnsi);

  // 1. Fall: RootKeys aus dem Control mit den RootKeys aus TRegistrySource
  // vergleichen; dabei geht dieser Test davon aus das per Standard die RootKeys
  // des Controls mit den RootKeys der TRegistrySource identisch sind
  TAssert.AssertNotNull('TypeName', aTypeName);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.RootKey',
    [aTypeName]), aRootKeys.RootKey,
    RegControl.RegistrySettings.RootKey);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.RootKeyForDefaults',
    [aTypeName]), aRootKeys.RootKeyForDefaults,
    RegControl.RegistrySettings.RootKeyForDefaults);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.RootForDefaults',
    [aTypeName]), aRegistrySource.RootForDefaults,
    RegControl.RegistrySettings.RootForDefaults);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.Project',
    [aTypeName]), aRootKeys.Project,
    RegControl.RegistrySettings.Project);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.Organisation',
    [aTypeName]), aRootKeys.Organisation,
    RegControl.RegistrySettings.Organisation);

  TAssert.AssertEquals(Format('1. Fall - %s.RegistrySettings.GUID',
    [aTypeName]), aRootKeys.GUID,
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
    [aTypeName]), aRootKeys.RootKey,
    RegControl.RegistrySettings.RootKey);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.RootKeyForDefaults',
    [aTypeName]), aRootKeys.RootKeyForDefaults,
    RegControl.RegistrySettings.RootKeyForDefaults);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.RootForDefaults',
    [aTypeName]), aRegistrySource.RootForDefaults,
    RegControl.RegistrySettings.RootForDefaults);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.Project',
    [aTypeName]), aRootKeys.Project,
    RegControl.RegistrySettings.Project);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.Organisation',
    [aTypeName]), aRootKeys.Organisation,
    RegControl.RegistrySettings.Organisation);

  TAssert.AssertEquals(Format('3. Fall - %s.RegistrySettings.GUID',
    [aTypeName]), aRootKeys.GUID,
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

constructor TWrapper<_T>.Create(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True;
  aCanRead: boolean = True;
  aCanWrite: boolean = True;
  aDoWriteAdHoc: boolean = True;
  aGroupIndex: integer = 0;
  aDoSyncData: boolean = False);
begin
  FCanRead := aCanRead;
  FCanWrite := aCanWrite;
  FDoWriteAdHoc := aDoWriteAdHoc;
  FGroupIndex := aGroupIndex;
  FDoSyncData := aDoSyncData;

  _Initialize;

  FRegControl := _T.Create(nil);
  SetRegControl;
  SetRegistrySettings(aRegistrySource, aSetRegSrc);
  SetRegistryEntries;
end;

destructor TWrapper<_T>.Destroy;
begin
  _Finalize;

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

constructor TWrapperCS<_T>.Create(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True;
  aCanRead: boolean = True;
  aCanWrite: boolean = True;
  aDoWriteAdHoc: boolean = True;
  aGroupIndex: integer = 0;
  aDoSyncData: boolean = False);
begin
  inherited Create(aRegistrySource, aSetRegSrc, aCanRead, aCanWrite,
    aDoWriteAdHoc, aGroupIndex, aDoSyncData);

  SetCaptionSettings;
end;

procedure TWrapperCS<_T>.SetCaptionSettings;
begin
  // Wird im spezialisierten Wrapper überschrieben
end;

{ TExtraListRegProperties }

procedure TExtraListRegProperties.AddListSection(aListSection: string);
begin
  ListSection := aListSection;
end;

procedure TExtraListRegProperties.Clear;
begin
  FillChar(Self, SizeOf(Self), #0);
end;

{ TWrapperLST }

procedure TWrapperLST<_T>._Initialize;
begin
  inherited _Initialize;
end;

constructor TWrapperLST<_T>.Create(aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True;
  aCanRead: boolean = True;
  aCanWrite: boolean = True;
  aDoWriteAdHoc: boolean = True;
  aGroupIndex: integer = 0;
  aDoSyncData: boolean = False;
  aDoMergeData: boolean = False;
  aItemsByRegistry: boolean = True;
  aSourceKind: TListSourceKind = lskByKey);
begin
  FListProperties.Clear;

  FListProperties.DoMergeData := aDoMergeData;
  FListProperties.ItemsByRegistry := aItemsByRegistry;
  FListProperties.SourceKind := aSourceKind;

  inherited Create(aRegistrySource, aSetRegSrc, aCanRead, aCanWrite, aDoWriteAdHoc, aGroupIndex,
    aDoSyncData);
end;

end.

