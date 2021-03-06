unit regsourcen;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  LResources,
  regconst,
  regtype;

type

  { TCaptionSettings }

   TCaptionSettings = class(TCustomCaptionSettings)
   public
     property OnChange;
   published
     property Section;
     property Ident;
     property CaptionByRegistry;
   end;

   { TRegistrySettingsBooleanDefault }

   {$ifndef fpdoc}
   TRegistrySettingsBooleanDefault = class(TCustomRegistrySettings<boolean>)
   published
     property Default;
     property Section;
     property Ident;
   end;
  {$endif}

  { TRegistrySettingsItemIndexDefault }

  {$ifndef fpdoc}
  TRegistrySettingsItemIndexDefault = class(TCustomRegistrySettings<integer>)
  protected
    procedure _Initialize; override;
  published
    property Default;
    property Section;
    property Ident;
  end;
  {$endif}

  { TRegistrySettingsIntegerDefault }

  {$ifndef fpdoc}
  TRegistrySettingsIntegerDefault = class(TCustomRegistrySettings<integer>)
  published
    property Default;
    property Section;
    property Ident;
  end;
  {$endif}
 
  { TRegistrySettingsStringDefault }

  {$ifndef fpdoc}
  TRegistrySettingsStringDefault = class(TCustomRegistrySettings<string>)
  protected
    function GetDefault: string;
  published
    property Default;
    property Section;
    property Ident;
  end;
 {$endif}
 
  { TRegistrySettingsValueList }
  
  {$ifndef fpdoc}
  TRegistrySettingsValueList = class(TCustomRegistrySettings<string>)
  private
    FListSection: string;
    FSourceKind: TListSourceKind;
  protected
    procedure _Initialize; override;
    procedure _Finalize; override;
  public
    property SourceKind: TListSourceKind
      read FSourceKind;
  published
    property ListSection: string
      read FListSection
      write FListSection;
    property DoMergeData;
  end;
 {$endif}

  { TRegistrySettingsList }

  TRegistrySettingsList = class(TRegistrySettingsItemIndexDefault)
  private
    FItemsByRegistry: boolean;
    FListSection: string;
    FSourceKind: TListSourceKind;
  protected
  public
  published
    property ItemsByRegistry: boolean
      read FItemsByRegistry
      write FItemsByRegistry;
    property ListSection: string
      read FListSection
      write FListSection;
    property SourceKind: TListSourceKind
      read FSourceKind
      write FSourceKind;
    property DoMergeData;
  end;

  { TRegistrySettingsCheckedList }

  TRegistrySettingsCheckedList = class(TRegistrySettingsItemIndexDefault)
  private
    FItemsByRegistry: boolean;
    FListSection: string;
    FSourceKind: TListSourceKind;
  protected
    procedure _Initialize; override;
    procedure _Finalize; override;
  public
    property SourceKind: TListSourceKind
      read FSourceKind;
  published
    property ItemsByRegistry: boolean
      read FItemsByRegistry
      write FItemsByRegistry;
    property ListSection: string
      read FListSection
      write FListSection;
    property DoMergeData;
  end;

  { TCustomRegistrySource }

  TCustomRegistrySource = class(TComponent)
  private
    FRootKey: string;
    FRootKeyForDefaults: string;
    FRootKeyForCommon: string;
    FProject: string;
    FOrganisation: string;
    FRootForDefaults: string;
    FReadDefaults: boolean;
    FWriteDefaults: boolean;
    FGUID: string;
    FClientList: TStrings;
    FDoSyncData: boolean;
    FPrefereStrings: boolean;
    FEditClientRootKeys: boolean;
    FCheckRTLAnsi: boolean;

    procedure OnSyncData(aGroupIndex: Cardinal = 0);
    procedure DeliverMessage(aMessageConst: cardinal;
                             aClientName: string = '';
                             aGroupIndex: cardinal = 0;
                             aWParam: integer = 0);
  protected
    function GetRootKey: string;
    procedure SetRootKey(aRootKey: string);
    property RootKey: string
      read GetRootKey
      write SetRootKey;

    function GetRootKeyForDefaults: string;
    procedure SetRootKeyForDefaults(aRootKeyForDefaults: string);
    property RootKeyForDefaults: string
      read GetRootKeyForDefaults
      write SetRootKeyForDefaults;

    function GetRootKeyForCommon: string;
    procedure SetRootKeyForCommon(aRootKeyForCommon: string);
    property RootKeyForCommon: string
      read GetRootKeyForCommon
      write SetRootKeyForCommon;

    function GetProject: string;
    procedure SetProject(AValue: string);
    property Project: string
      read GetProject
      write SetProject;

    function GetOrganisation: string;
    procedure SetOrganisation(AValue: string);
    property Organisation: string
      read GetOrganisation
      write SetOrganisation;

    property RootForDefaults: string
      read FRootForDefaults
      write FRootForDefaults;

    property ReadDefaults: boolean
      read FReadDefaults
      write FReadDefaults;

    property WriteDefaults: boolean
      read FWriteDefaults
      write FWriteDefaults;

    { TODO 1 -oAlfred Gerke -cEntwicklung : Setter und Getter einführen }
    function GetGUID: string;
    procedure SetGUID(aGUID: string);
    property GUID: string
      read GetGUID
      write SetGUID;

    property DoSyncData: boolean
      read FDoSyncData
      write FDoSyncData;

    property PrefereStrings: boolean
      read FPrefereStrings
      write FPrefereStrings;

    function GetClientCount: integer;
    property ClientCount: integer
      read GetClientCount;

    property EditClientRootKeys: boolean
      read FEditClientRootKeys
      write FEditClientRootKeys;

    property CheckRTLAnsi: boolean
      read FCheckRTLAnsi
      write FCheckRTLAnsi;
  public
    procedure PostClientData(aClientName: string = '';
                             aGroupIndex: cardinal = 0);
    procedure RefreshMergeDataProperty(aMergeData: boolean = False;
                                       aClientName: string = '';
                                       aGroupIndex: cardinal = 0);
    function GetClientList(aList: TStrings): boolean;
    procedure ClearClientItems(aClientName: string = '';
                               aAskFor: boolean = True;
                               aMsg: string = 'Clear Items?';
                               aGroupIndex: cardinal = 0);
    procedure DeleteRootKey; reintroduce; overload;
    procedure DeleteRootKey(aRootKey: string;
                            aRootKeyForDefaults: string;
                            aRootForDefaults: string;
                            aUseDefaults: boolean); reintroduce; overload;
    procedure RenameClient(aOldName: TComponentName;
                           aNewName: TComponentName);
    procedure ShowClientEditDialog(aClientName: string);
    function GetClientByName(aClientName: string): TComponent;
    function GetClientNameByIndex(aIndex: integer) : string;
    procedure FreeRegistrySource(aClientName: string = '';
                                 aGroupIndex: cardinal = 0);
    procedure RefreshWriteAdHocProperty(aDoWriteAdHoc : boolean = True;
                                        aClientName: string = '';
                                        aGroupIndex: cardinal = 0);
    procedure RefreshSyncProperty(aDoSync : boolean = True;
                                  aClientName: string = '';
                                  aGroupIndex: cardinal = 0);
    procedure RefreshSettings(aClientName: string = '');
    procedure RefreshClientData(aClientName: string = '';
                                aGroupIndex: cardinal = 0);
    procedure RegisterClient(aClient: TComponent);
    procedure UnRegisterClient(aClient: TComponent);
    { TODO 1 -oAlfred Gerke -cEntwicklung : Umbennenen: ???? }
    function GetComposedRootKey: string;
    function GetComposedRootKeyForDefaults: string;
    function GetComposedRootKeyForCommon: string;

    function IdentExists(aSection: string;
                         aIdent: string;
                         aCheckDefaults: boolean = False): boolean; reintroduce; overload;
    function IdentExists(aRootKey: string;
                         aRootKeyForDefaults: string;
                         aRootForDefaults: string;
                         aSection: string;
                         aIdent: string;
                         aCheckDefaults: boolean = False): boolean; reintroduce; overload;
    function SectionExists(aRootKey: string;
                           aRootKeyForDefaults: string;
                           aRootForDefaults: string;
                           aSection: string;
                           aCheckDefaults: boolean = False): boolean; reintroduce; overload;
    function SectionExists(aSection: string;
                           aCheckDefaults: boolean = False): boolean; reintroduce; overload;
    function ReadString(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aIdent: string;
                        aDefault: string;
                        aUseDefaults: boolean): string; reintroduce; overload;
    function ReadString(aSection: string;
                        aIdent: string;
                        aDefault: string): string; reintroduce; overload;
    function ReadInteger(aRootKey: string;
                         aRootKeyForDefaults: string;
                         aRootForDefaults: string;
                         aSection: string;
                         aIdent: string;
                         aDefault: integer;
                         aUseDefaults: boolean): integer; reintroduce; overload;
    function ReadInteger(aSection: string;
                         aIdent: string;
                         aDefault: integer): integer; reintroduce; overload;
    function ReadBool(aRootKey: string;
                      aRootKeyForDefaults: string;
                      aRootForDefaults: string;
                      aSection: string;
                      aIdent: string;
                      aDefault: boolean;
                      aUseDefaults: boolean): boolean; reintroduce; overload;
    function ReadBool(aSection: string;
                      aIdent: string;
                      aDefault: boolean): boolean; reintroduce; overload;
    procedure ReadSection(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aRootForDefaults: string;
                          aSection: string;
                          aStrings: TStrings;
                          aMerge: boolean;
                          aUseDefaults: boolean;
                          aListSource: TListSourceKind = lskByKey); reintroduce; overload;
    procedure ReadSection(aSection: string;
                          aStrings: TStrings;
                          aMerge: boolean = False); reintroduce; overload;
    procedure WriteString(aRootKey: string;
                          aRootKeyForDefaults: string;
                          aRootForDefaults: string;
                          aSection: string;
                          aIdent: string;
                          aDefault: string;
                          aUseDefaults: boolean;
                          aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteString(aSection: string;
                          aIdent: string;
                          aDefault: string;
                          aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteInteger(aRootKey: string;
                           aRootKeyForDefaults: string;
                           aRootForDefaults: string;
                           aSection: string;
                           aIdent: string;
                           aDefault: integer;
                           aUseDefaults: boolean;
                           aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteInteger(aSection: string;
                           aIdent: string;
                           aDefault: integer;
                           aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteBool(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aIdent: string;
                        aDefault: boolean;
                        aUseDefaults: boolean;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure WriteBool(aSection: string;
                        aIdent: string;
                        aDefault: boolean;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure RenameKey(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aOldKey: string;
                        aNewKey: string;
                        aUseDefaults: boolean;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure RenameKey(aSection: string;
                        aOldKey: string;
                        aNewKey: string;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure DeleteKey(aRootKey: string;
                        aRootKeyForDefaults: string;
                        aRootForDefaults: string;
                        aSection: string;
                        aKey: string;
                        aUseDefaults: boolean;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure DeleteKey(aSection: string;
                        aKey: string;
                        aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure EraseSection(aRootKey: string;
                           aRootKeyForDefaults: string;
                           aRootForDefaults: string;
                           aSection: string;
                           aUseDefaults: boolean;
                           aGroupIndex: Cardinal = 0); reintroduce; overload;
    procedure EraseSection(aSection: string;
                           aGroupIndex: Cardinal = 0); reintroduce; overload;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TRegistrySource }

  TRegistrySource = class(TCustomRegistrySource)
  private
  protected
  public
    property ClientCount;
    property EditClientRootKeys;
  published
    property RootKey;
    property RootKeyForDefaults;
    property RootKeyForCommon;
    property Project;
    property Organisation;
    property RootForDefaults;
    property ReadDefaults;
    property WriteDefaults;
    property GUID;
    property DoSyncData;
    property PrefereStrings;
    property CheckRTLAnsi;
  end;

procedure Register;

implementation

uses
  regutils,
  regmsg,
  LMessages,
  PropEdits,
  ComponentEditors,
  regpropedits,
  regconvutils;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegistrySource]);
  RegisterComponentEditor(TRegistrySource, TRegistrySourceComponentEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsStringDefault, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsItemIndexDefault, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsIntegerDefault, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsBooleanDefault, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsList, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsCheckedList, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TRegistrySettingsValueList, 'OnBeforeRegistrySettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TOnRegistrySettingsChange),
    TCaptionSettings, 'OnBeforeCaptionSettingChange',
    TRegistrySettingsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string),
    TRegistrySource, 'GUID', TGUIDStringProperty);
end;

{ TRegistrySettingsStringDefault }

function TRegistrySettingsStringDefault.GetDefault: string;
var
  value: string;
begin
  value := inherited GetDefault;

  Result:= UTF8ToSysIfNeeded(value, CheckRTLAnsi);
end;

{ TRegistrySettingsItemIndexDefault }

procedure TRegistrySettingsItemIndexDefault._Initialize;
begin
  Ident := _ItemIndex;

  inherited _Initialize;
end;

{ TRegistrySettingsValueList }

procedure TRegistrySettingsValueList._Initialize;
begin
  FSourceKind := lskByKeyValue;

  inherited;
end;

procedure TRegistrySettingsValueList._Finalize;
begin
  FSourceKind := lskUnknown;

  inherited;
end;

{ TRegistrySettingsCheckedList }

procedure TRegistrySettingsCheckedList._Initialize;
begin
  FSourceKind := lskByKeyValue;

  inherited;
end;

procedure TRegistrySettingsCheckedList._Finalize;
begin
  FSourceKind := lskUnknown;

  inherited;
end;

{ TCustomRegistrySource }

procedure TCustomRegistrySource.OnSyncData(aGroupIndex: cardinal = 0);
begin
  RefreshClientData('', aGroupIndex);
end;

function TCustomRegistrySource.GetRootKeyForDefaults: string;
begin
  Result := UTF8ToSysIfNeeded(FRootKeyForDefaults, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetOrganisation: string;
begin
  Result := UTF8ToSysIfNeeded(FOrganisation, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetProject: string;
begin
  Result := UTF8ToSysIfNeeded(FProject, FCheckRTLAnsi);
end;

procedure TCustomRegistrySource.DeliverMessage(aMessageConst: cardinal;
  aClientName: string = '';
  aGroupIndex: cardinal = 0;
  aWParam: integer = 0);
var
  anz: integer;
  msg: TLMessage;
begin
  // löst Hint: Local variable "msg" does not seem to be initialized
  msg.Result := 0;
  FillChar(msg, SizeOf(msg), #0);
  msg.Msg := aMessageConst;
  msg.lParam := AGroupIndex;
  msg.wParam := aWParam;
  for anz := FClientList.count-1 downto 0 do
  begin
    if Assigned(FClientList.Objects[anz]) then
      if FClientList.Objects[anz] is TComponent then
      begin
        if (AClientName = EmptyStr) then
          TComponent(FClientList.Objects[anz]).Dispatch(msg)
        else
          if (LowerCase(AClientName) = LowerCase(TComponent(FClientList.Objects[anz]).Name)) then
            TComponent(FClientList.Objects[anz]).Dispatch(msg);
      end;
  end;
end;

function TCustomRegistrySource.GetRootKey: string;
begin
  Result := UTF8ToSysIfNeeded(FRootKey, FCheckRTLAnsi);
end;

procedure TCustomRegistrySource.SetRootKey(aRootKey: string);
begin
  FRootKey := SysToUTF8IfNeeded(aRootKey, FCheckRTLAnsi);
end;

procedure TCustomRegistrySource.SetRootKeyForDefaults(
  aRootKeyForDefaults: string);
begin
  FRootKeyForDefaults := SysToUTF8IfNeeded(aRootKeyForDefaults, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetRootKeyForCommon: string;
begin
  Result := UTF8ToSysIfNeeded(FRootKeyForCommon, FCheckRTLAnsi);
end;

procedure TCustomRegistrySource.SetRootKeyForCommon(aRootKeyForCommon: string);
begin
  FRootKeyForCommon := SysToUTF8IfNeeded(aRootKeyForCommon, FCheckRTLAnsi);
end;

procedure TCustomRegistrySource.SetOrganisation(AValue: string);
begin
  FOrganisation := SysToUTF8IfNeeded(AValue, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetGUID: string;
begin
  Result := UTF8ToSysIfNeeded(FGUID, CheckRTLAnsi);
end;

procedure TCustomRegistrySource.SetGUID(aGUID: string);
begin
  FGUID := SysToUTF8IfNeeded(aGUID, FCheckRTLAnsi);
end;

procedure TCustomRegistrySource.SetProject(AValue: string);
begin
  FProject := SysToUTF8IfNeeded(AValue, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetClientCount: integer;
begin
  if Assigned(FClientList) then
    Result := FClientList.Count
  else
    Result := 0;
end;

procedure TCustomRegistrySource.PostClientData(aClientName: string = '';
  aGroupIndex: cardinal = 0);
begin
  DeliverMessage(LM_REGISTRY_CONTROL_POST_DATA, aClientName, aGroupIndex);
end;

procedure TCustomRegistrySource.RefreshMergeDataProperty(aMergeData: boolean = False;
  aClientName: string = '';
  aGroupIndex: cardinal = 0);
var
  w_param: integer;
begin
  if aMergeData then
    w_param := 1
  else
    w_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_MERGE_LIST, aClientName, aGroupIndex, w_param);
end;

function TCustomRegistrySource.GetClientList(aList: TStrings): boolean;
begin
  aList.Clear;
  aList.AddStrings(FClientList);
  Result := (aList.Count > 0);
end;

procedure TCustomRegistrySource.ClearClientItems(aClientName: string = '';
  aAskFor: boolean = True;
  aMsg: string = 'Clear Items?';
  aGroupIndex: cardinal = 0);
var
  start: boolean;
begin
  start := not aAskFor;
  if aAskFor then
    start := (MessageDlg(aMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  if start then
    DeliverMessage(LM_REGISTRY_CONTROL_CLEAR_LIST, aClientName, aGroupIndex, 0);
end;

procedure TCustomRegistrySource.DeleteRootKey;
begin
  try
    DeleteRootKey(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      WriteDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.DeleteRootKey(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aUseDefaults: boolean);
var
  streg: TDataByCurrentUser;
  list: TStrings;
  anz: Integer;
  section: string;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  list := TStringlist.Create;
  try
    try
      streg.ReadSections(list);

      for anz := 0 to list.count-1 do
      begin
        section := list.Strings[anz];
        if aUseDefaults then
          streg.EraseSectionForDefaults(section)
        else
          streg.EraseSection(section);
      end;
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(list) then
      FreeAndNil(list);
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.RenameClient(aOldName: TComponentName;
  aNewName: TComponentName);
var
  index: integer;
begin
  if Assigned(FClientList) then
    if (FClientList.Count > 0) then
    begin
      index := FClientList.IndexOf(aOldName);
      if (index <> -1) then
        FClientList.Strings[index] := aNewName;
    end;
end;

procedure TCustomRegistrySource.ShowClientEditDialog(aClientName: string);
var
  w_param: integer;
  l_param: cardinal;
begin
  if FEditClientRootKeys then
    w_param := 1
  else
    w_param := 0;

  if (csDesigning in ComponentState) then
    l_param := 1
  else
    l_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_SHOW_EDITDIALOG, aClientName, l_param, w_param);
end;

function TCustomRegistrySource.GetClientByName(aClientName: string): TComponent;
var
  index: integer;
begin
  Result := nil;
  if (ClientCount > 0) then
  begin
    index := FClientList.IndexOf(aClientName);
    if (index <> -1) then
      if Assigned(FClientList.Objects[index]) then
        if (FClientList.Objects[index] is TComponent) then
          Result := TComponent(FClientList.Objects[index]);
  end;
end;

function TCustomRegistrySource.GetClientNameByIndex(aIndex: integer): string;
begin
  if ((ClientCount > 0) and ((ClientCount-1) >= aIndex)) then
    Result := FClientList.Strings[aIndex];
end;

procedure TCustomRegistrySource.FreeRegistrySource(aClientName: string = '';
  aGroupIndex: cardinal = 0);
begin
  DeliverMessage(LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE, aClientName, aGroupIndex);
end;

procedure TCustomRegistrySource.RefreshWriteAdHocProperty(aDoWriteAdHoc: boolean = True;
  aClientName: string = '';
  aGroupIndex: cardinal = 0);
var
  w_param: integer;
begin
  if aDoWriteAdHoc then
    w_param := 1
  else
    w_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_SET_WRITEADHOC, aClientName, aGroupIndex, w_param);
end;

procedure TCustomRegistrySource.RefreshSyncProperty(aDoSync: boolean = True;
  aClientName: string = '';
  aGroupIndex: cardinal = 0);
var
  w_param: integer;
begin
  if aDoSync then
    w_param := 1
  else
    w_param := 0;

  DeliverMessage(LM_REGISTRY_CONTROL_SET_SYNC, aClientName, aGroupIndex, w_param);
end;

procedure TCustomRegistrySource.RefreshSettings(aClientName: string = '');
begin
  DeliverMessage(LM_REGISTRY_CONTROL_REFRESH_SETTINGS, aClientName);
end;

procedure TCustomRegistrySource.RefreshClientData(aClientName: string = '';
  aGroupIndex: cardinal = 0);
begin
  DeliverMessage(LM_REGISTRY_CONTROL_REFRESH_DATA, aClientName, aGroupIndex);
end;

procedure TCustomRegistrySource.RegisterClient(aClient: TComponent);
var
  index: integer;
  name: string;
begin
  if Assigned(FClientList) then
  begin
    name := aClient.Name;
    index := FClientList.IndexOf(name);
    if (index = -1) then
      FClientList.AddObject(name, aClient);
  end;
end;

procedure TCustomRegistrySource.UnRegisterClient(aClient: TComponent);
var
  index: integer;
  name: string;
begin
  if Assigned(FClientList) then
  begin
    name := aClient.Name;
    index := FClientList.IndexOf(name);
    if (index <> -1) then
      FClientList.Delete(index);
  end;
end;

function TCustomRegistrySource.GetComposedRootKey: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKey);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);

  Result := UTF8ToSysIfNeeded(root_key, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetComposedRootKeyForDefaults: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForDefaults);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);

  Result := UTF8ToSysIfNeeded(root_key, FCheckRTLAnsi);
end;

function TCustomRegistrySource.GetComposedRootKeyForCommon: string;
var
  root_key: string;
begin
  root_key := IncludeTrailingPathDelimiter(FRootKeyForCommon);

  root_key := _ChangeTokenForKey(TokenTypeStr[ttProject], FProject, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttOrganisation], FOrganisation, root_key);
  root_key := _ChangeTokenForKey(TokenTypeStr[ttGUID], FGUID, root_key);

  Result := UTF8ToSysIfNeeded(root_key, FCheckRTLAnsi);
end;

constructor TCustomRegistrySource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRootKey := EmptyStr;
  FRootKeyForDefaults := EmptyStr;
  FRootKeyForCommon := EmptyStr;
  FProject := EmptyStr;
  FOrganisation := EmptyStr;
  FRootForDefaults := EmptyStr;
  FReadDefaults := False;
  FWriteDefaults := False;
  FGUID := EmptyStr;
  FDoSyncData := False;
  FPrefereStrings := False;
  FEditClientRootKeys := False;
  FCheckRTLAnsi := True;

  FClientList := TStringList.Create;
end;

destructor TCustomRegistrySource.Destroy;
begin
  FreeRegistrySource;

  if Assigned(FClientList) then
    FreeAndNil(FClientList);

  inherited Destroy;
end;

function TCustomRegistrySource.IdentExists(aSection: string;
  aIdent: string;
  aCheckDefaults: boolean = False): boolean;
begin
  try
    Result := IdentExists(GetComposedRootKey,
                GetComposedRootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aCheckDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

function TCustomRegistrySource.IdentExists(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aCheckDefaults: boolean = False): boolean;
var
  streg: TDataByCurrentUser;
begin
  Result := False;

  if aCheckDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aCheckDefaults then
        Result := streg.IdentExistsForDefaults(aSection, aIdent)
      else
        Result := streg.IdentExists(aSection, aIdent);
    except
      on E: Exception do
      begin
        Result := False;
        raise;
      end;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.SectionExists(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aCheckDefaults: boolean = False): boolean;
var
  streg: TDataByCurrentUser;
begin
  Result := False;

  if aCheckDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aCheckDefaults then
        Result := streg.SectionExistsForDefaults(aSection)
      else
        Result := streg.SectionExists(aSection);
    except
      on E: Exception do
      begin
        Result := False;
        raise;
      end;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.SectionExists(aSection: string;
  aCheckDefaults: boolean = False): boolean;
begin
  try
    Result := SectionExists(GetComposedRootKey,
                GetComposedRootKeyForDefaults,
                RootForDefaults,
                aSection,
                aCheckDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

function TCustomRegistrySource.ReadString(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: string;
  aUseDefaults: boolean): string;
var
  streg: TDataByCurrentUser;
begin
  Result := '';

  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        Result := streg.ReadStringCheckForDefaults(aSection, aIdent, aDefault)
      else
        Result := streg.ReadString(aSection, aIdent, aDefault);
    except
      on E: Exception do
      begin
        Result := '';
        raise;
      end;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.ReadString(aSection: string;
  aIdent: string;
  aDefault: string): string;
begin
  try
    Result := ReadString(GetComposedRootKey,
                GetComposedRootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aDefault,
                ReadDefaults);
  except
    on E: Exception do
      raise;
  end;
end;

function TCustomRegistrySource.ReadInteger(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: integer;
  aUseDefaults: boolean): integer;
var
  streg: TDataByCurrentUser;
begin
  Result := aDefault;

  if aUseDefaults then
    streg := TDataByCurrentUser.Create(
               aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        Result := streg.ReadIntegerCheckForDefaults(aSection, aIdent, aDefault)
      else
        Result := streg.ReadInteger(aSection, aIdent, aDefault);
    except
      on E: Exception do
      begin
        Result := aDefault;
        raise;
      end;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.ReadInteger(aSection: string;
  aIdent: string;
  aDefault: integer): integer;
begin
  try
    Result := ReadInteger(GetComposedRootKey,
                GetComposedRootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aDefault,
                ReadDefaults);
  except
    on E: Exception do
    begin
      Result := aDefault;
      raise;
    end;
  end;
end;

function TCustomRegistrySource.ReadBool(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: boolean;
  aUseDefaults: boolean): boolean;
var
  streg: TDataByCurrentUser;
begin
  Result := aDefault;

  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        Result := streg.ReadBoolCheckForDefaults(aSection, aIdent, aDefault)
      else
        Result := streg.ReadBool(aSection, aIdent, aDefault);
    except
      on E: Exception do
      begin
        Result := aDefault;
        raise;
      end;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

function TCustomRegistrySource.ReadBool(aSection: string;
  aIdent: string;
  aDefault: boolean): boolean;
begin
  try
    Result := ReadBool(GetComposedRootKey,
                GetComposedRootKeyForDefaults,
                RootForDefaults,
                aSection,
                aIdent,
                aDefault,
                ReadDefaults);
  except
    on E: Exception do
    begin
      Result := aDefault;
      raise;
    end;
  end;
end;

procedure TCustomRegistrySource.ReadSection(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aStrings: TStrings;
  aMerge: boolean;
  aUseDefaults: boolean;
  aListSource: TListSourceKind = lskByKey);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
      begin
        case aListSource of
          lskByKey:
            streg.ReadSectionCheckForDefaults(aSection, aStrings, aMerge);
          lskByValue:
            streg.ReadSectionValuesOnlyForDefaults(aSection, aStrings, aMerge);
          lskByKeyValue:
            streg.ReadSectionValuesCheckForDefaults(aSection, aStrings, aMerge);
        else
          aStrings.clear;
        end;
      end
      else
      begin
        case aListSource of
          lskByKey: streg.ReadSection(aSection, aStrings);
          lskByValue: streg.ReadSectionValuesOnly(aSection, aStrings);
          lskByKeyValue: streg.ReadSectionValuesEx(aSection, aStrings);
        else
          aStrings.clear;
        end;
      end;
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.ReadSection(aSection: string;
  aStrings: TStrings;
  aMerge: boolean = False);
begin
  try
    ReadSection(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aStrings,
      aMerge,
      ReadDefaults,
      lskByKey);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.WriteString(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: string;
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        streg.WriteStringCheckForDefaults(aSection, aIdent, aDefault)
      else
        streg.WriteString(aSection, aIdent, aDefault);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.WriteString(aSection: string;
  aIdent: string;
  aDefault: string;
  aGroupIndex: Cardinal = 0);
begin
  try
    WriteString(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.WriteInteger(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: integer;
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        streg.WriteIntegerCheckForDefaults(aSection, aIdent, aDefault)
      else
        streg.WriteInteger(aSection, aIdent, aDefault);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.WriteInteger(aSection: string;
  aIdent: string;
  aDefault: integer;
  aGroupIndex: Cardinal = 0);
begin
  try
    WriteInteger(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

procedure TCustomRegistrySource.WriteBool(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aIdent: string;
  aDefault: boolean;
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        streg.WriteBoolCheckForDefaults(aSection, aIdent, aDefault)
      else
        streg.WriteBool(aSection, aIdent, aDefault);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.WriteBool(aSection: string;
  aIdent: string;
  aDefault: boolean;
  aGroupIndex: Cardinal = 0);
begin
  try
    WriteBool(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aIdent,
      aDefault,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.RenameKey(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aOldKey: string;
  aNewKey: string;
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        streg.RenameKeyForDefaults(aSection, aOldKey, aNewKey)
      else
        streg.RenameKey(aSection, aOldKey, aNewKey);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.RenameKey(aSection: string;
  aOldKey: string;
  aNewKey: string;
  aGroupIndex: Cardinal = 0);
begin
  try
    RenameKey(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aOldKey,
      aNewKey,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.DeleteKey(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aKey: string;
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        streg.DeleteKeyForDefaults(aSection, aKey)
      else
        streg.DeleteKey(aSection, aKey);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.DeleteKey(aSection: string;
  aKey: string;
  aGroupIndex: Cardinal = 0);
begin
  try
    DeleteKey(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      aKey,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
      raise;
  end;
end;

procedure TCustomRegistrySource.EraseSection(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aSection: string;
  aUseDefaults: boolean;
  aGroupIndex: Cardinal = 0);
var
  streg: TDataByCurrentUser;
begin
  if aUseDefaults then
    streg := TDataByCurrentUser.Create(aRootKey,
               aRootForDefaults,
               aRootKeyForDefaults,
               FPrefereStrings,
               FCheckRTLAnsi)
  else
    streg := TDataByCurrentUser.Create(aRootKey,
               FPrefereStrings,
               FCheckRTLAnsi);

  try
    try
      if aUseDefaults then
        streg.EraseSectionForDefaults(aSection)
      else
        streg.EraseSection(aSection);

      if FDoSyncData then
        OnSyncData(aGroupIndex);
    except
      on E: Exception do
        raise;
    end;
  finally
    if Assigned(streg) then
      FreeAndNil(streg);
  end;
end;

procedure TCustomRegistrySource.EraseSection(aSection: string;
  aGroupIndex: Cardinal = 0);
begin
  try
    EraseSection(GetComposedRootKey,
      GetComposedRootKeyForDefaults,
      RootForDefaults,
      aSection,
      WriteDefaults,
      aGroupIndex);
  except
    on E: Exception do
      raise;
  end;
end;

initialization
  {$I ..\package\registrysource.lrs}

end.
