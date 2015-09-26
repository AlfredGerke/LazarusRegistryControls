{ TODO -oAlfred Gerke -cControlButtonFrame : Spezielle Testmethoden für Listen-RegControls implementieren }

unit fmeregcontrolbuttonframe;

{$mode delphi}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls,
  ActnList,
  Spin,
  ComCtrls,
  Controls,
  fmeregcontrolproperties,
  Classes,
  regtype;

type

  { TRegControlButtonFrame }

  TRegControlButtonFrame = class(TFrame)
    acList: TActionList;
    acGetRootKeys: TAction;
    acCanRead: TAction;
    acCanWrite: TAction;
    acDoWriteAdHoc: TAction;
    acDoSyncData: TAction;
    acDoMergeData: TAction;
    acSetGroupIndex: TAction;
    acReadFromReg: TAction;
    acSetItems: TAction;
    acItemsByRegistry: TAction;
    acClearItems: TAction;
    acSetListSourceKind: TAction;
    acDeleteItem: TAction;
    btnGetRootKeys: TSpeedButton;
    cbxCanRead: TCheckBox;
    cbxCanWrite: TCheckBox;
    cbxDoMergeData: TCheckBox;
    cbxItemsByRegistry: TCheckBox;
    cbxDoSyncData: TCheckBox;
    cbxDoWriteAdHoc: TCheckBox;
    cbxEditRootKeys: TCheckBox;
    edtGroupIndex: TSpinEdit;
    grbDialogs: TGroupBox;
    grpProperties: TGroupBox;
    ilImages: TImageList;
    lblGroupIndex: TLabel;
    pnlWorkspace: TPanel;
    barButtonFrameToolbar: TToolBar;
    rgbListSourceKind: TRadioGroup;
    ScrollBox1: TScrollBox;
    btnRefresh: TToolButton;
    btnClear: TToolButton;
    btnAdd: TToolButton;
    btnDeleteItem: TToolButton;
    procedure acCanReadExecute(Sender: TObject);
    procedure acCanWriteExecute(Sender: TObject);
    procedure acClearItemsExecute(Sender: TObject);
    procedure acDeleteItemExecute(Sender: TObject);
    procedure acDoMergeDataExecute(Sender: TObject);
    procedure acDoSyncDataExecute(Sender: TObject);
    procedure acDoWriteAdHocExecute(Sender: TObject);
    procedure acGetRootKeysExecute(Sender: TObject);
    procedure acItemsByRegistryExecute(Sender: TObject);
    procedure acReadFromRegExecute(Sender: TObject);
    procedure acSetGroupIndexExecute(Sender: TObject);
    procedure acSetItemsExecute(Sender: TObject);
    procedure acSetListSourceKindExecute(Sender: TObject);
  public type
    TOnGetRootKeys = procedure(aEdit: boolean) of object;
    TOnSetBooleanProperty = procedure(AValue: boolean) of object;
    TOnGetBooleanProperty = function: boolean of object;
    TOnSetIntegerProperty = procedure(aIndex: integer) of object;
    TOnGetIntegerProperty = function: integer of object;
    TOnSetListSourceKind = procedure(aSetListSourceKind: TListSourceKind) of object;
    TOnGetListSourceKind = function: TListSourceKind of object;
    TOnReadFromRegistry = procedure of object;
    TOnSetItems = procedure of object;
    TOnDeleteItem = function : boolean of object;
    TOnClearItems = function : boolean of object;
  private
    FOnGetRootKeys: TOnGetRootKeys;
    //
    FOnSetCanRead: TOnSetBooleanProperty;
    FOnSetCanWrite: TOnSetBooleanProperty;
    FOnSetDoWriteAdHoc: TOnSetBooleanProperty;
    FOnSetDoSyncData: TOnSetBooleanProperty;
    FOnSetDoMergeData: TOnSetBooleanProperty;
    FOnSetGroupIndex: TOnSetIntegerProperty;
    FOnSetItemsByRegistry: TOnSetBooleanProperty;
    FOnSetListSourceKind: TOnSetListSourceKind;
    //
    FOnGetCanRead: TOnGetBooleanProperty;
    FOnGetCanWrite: TOnGetBooleanProperty;
    FOnGetDoWriteAdHoc: TOnGetBooleanProperty;
    FOnGetDoSyncData: TOnGetBooleanProperty;
    FOnGetDoMergeData: TOnGetBooleanProperty;
    FOnGetGroupIndex: TOnGetIntegerProperty;
    FOnGetItemsByRegistry: TOnGetBooleanProperty;
    FOnGetListSourceKind: TOnGetListSourceKind;
    //
    FOnReadFromRegistry: TOnReadFromRegistry;
    FOnSetItems: TOnSetItems;
    FOnClearItems: TOnClearItems;
    FOnDeleteItem: TOnDeleteItem;
    //
    FOnRefreshSettings: TRegControlProperties.TOnRefreshSettings;

    procedure RefreshRegControl;
    procedure SetCanRead;
    procedure SetCanWrite;
    procedure SetDoWriteAdHoc;
    procedure SetDoSyncData;
    procedure SetDoMegerData;
    procedure SetGroupIndex;
    procedure SetItemsByRegistry;
    procedure SetItems;
    procedure SetListSourceKind;
    function ClearItems: boolean;
    function DeleteItem: boolean;
  public
    procedure Refresh;

    constructor Create(aOwner: TComponent); override;

    property OnGetRootKeys: TOnGetRootKeys
      read FOnGetRootKeys
      write FOnGetRootKeys;

    property OnRefreshSettings: TRegControlProperties.TOnRefreshSettings
      read FOnRefreshSettings
      write FOnRefreshSettings;

      property OnSetCanRead: TOnSetBooleanProperty
        read FOnSetCanRead
        write FOnSetCanRead;

      property OnSetCanWrite: TOnSetBooleanProperty
        read FOnSetCanWrite
        write FOnSetCanWrite;

      property OnSetDoWriteAdHoc: TOnSetBooleanProperty
        read FOnSetDoWriteAdHoc
        write FOnSetDoWriteAdHoc;

      property OnSetDoSyncData: TOnSetBooleanProperty
        read FonSetDoSyncData
        write FonSetDoSyncData;

      property OnSetDoMergeData: TOnSetBooleanProperty
        read FOnSetDoMergeData
        write FOnSetDoMergeData;

      property OnSetGroupIndex: TOnSetIntegerProperty
        read FOnSetGroupIndex
        write FOnSetGroupIndex;

      property OnGetCanRead: TOnGetBooleanProperty
        read FOnGetCanRead
        write FOnGetCanRead;

      property OnGetCanWrite: TOnGetBooleanProperty
        read FOnGetCanWrite
        write FOnGetCanWrite;

      property OnGetDoWriteAdHoc: TOnGetBooleanProperty
        read FOnGetDoWriteAdHoc
        write FOnGetDoWriteAdHoc;

      property OnGetDoSyncData: TOnGetBooleanProperty
        read FOnGetDoSyncData
        write FOnGetDoSyncData;

      property OnGetDoMergeData: TOnGetBooleanProperty
        read FOnGetDoMergeData
        write FOnGetDoMergeData;

      property OnGetGroupIndex: TOnGetIntegerProperty
        read FOnGetGroupIndex
        write FOnGetGroupIndex;

      property OnSetItemsByRegistry: TOnSetBooleanProperty
        read FOnSetItemsByRegistry
        write FOnSetItemsByRegistry;

      property OnGetItemsByRegistry: TOnGetBooleanProperty
        read FOnGetItemsByRegistry
        write FOnGetItemsByRegistry;

      property OnSetListSourceKind: TOnSetListSourceKind
        read FOnSetListSourceKind
        write FOnSetListSourceKind;

      property OnGetListSourceKind: TOnGetListSourceKind
        read FOnGetListSourceKind
        write FOnGetListSourceKind;

      property OnReadFromRegistry: TOnReadFromRegistry
        read FOnReadFromRegistry
        write FOnReadFromRegistry;

      property OnSetItems: TOnSetItems
        read FOnSetItems
        write FOnSetItems;

      property OnClearItems: TOnClearItems
        read FOnClearItems
        write FOnClearItems;

      property OnDeleteItem: TOnDeleteItem
        read FOnDeleteItem
        write FOnDeleteItem;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

{ TRegControlButtonFrame }

procedure TRegControlButtonFrame.acGetRootKeysExecute(Sender: TObject);
begin
  if Assigned(FOnGetRootKeys) then
    FOnGetRootKeys(cbxEditRootKeys.Checked);
end;

procedure TRegControlButtonFrame.acItemsByRegistryExecute(Sender: TObject);
begin
  SetItemsByRegistry;
end;

procedure TRegControlButtonFrame.acReadFromRegExecute(Sender: TObject);
begin
  RefreshRegControl;
end;

procedure TRegControlButtonFrame.acSetGroupIndexExecute(Sender: TObject);
begin
  SetGroupIndex;
end;

procedure TRegControlButtonFrame.acSetItemsExecute(Sender: TObject);
begin
  SetItems;
end;

procedure TRegControlButtonFrame.acSetListSourceKindExecute(Sender: TObject);
begin
  SetListSourceKind;
end;

procedure TRegControlButtonFrame.RefreshRegControl;
begin
  if Assigned(FOnReadFromRegistry) then
    FOnReadFromRegistry;
end;

procedure TRegControlButtonFrame.acCanReadExecute(Sender: TObject);
begin
  SetCanRead;
end;

procedure TRegControlButtonFrame.acCanWriteExecute(Sender: TObject);
begin
  SetCanWrite;
end;

procedure TRegControlButtonFrame.acClearItemsExecute(Sender: TObject);
begin
  if not ClearItems then
    MessageDlg('Daten wurden nicht gelöscht!', mtInformation, [mbOK], 0);
end;

procedure TRegControlButtonFrame.acDeleteItemExecute(Sender: TObject);
begin
  if not DeleteItem then
    MessageDlg('Eintrag wurde nicht gelöscht!', mtInformation, [mbOK], 0);
end;

procedure TRegControlButtonFrame.acDoMergeDataExecute(Sender: TObject);
begin
  SetDoMegerData;
end;

procedure TRegControlButtonFrame.acDoSyncDataExecute(Sender: TObject);
begin
  SetDoSyncData;
end;

procedure TRegControlButtonFrame.acDoWriteAdHocExecute(Sender: TObject);
begin
  SetDoWriteAdHoc;
end;

procedure TRegControlButtonFrame.SetCanRead;
begin
  if Assigned(FOnSetCanRead) then
    if cbxCanRead.Checked then
      FOnSetCanRead(True)
    else
      FOnSetCanRead(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetCanWrite;
begin
  if Assigned(FOnSetCanWrite) then
    if cbxCanWrite.Checked then
      FOnSetCanWrite(True)
    else
      FOnSetCanWrite(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetDoWriteAdHoc;
begin
  if Assigned(FOnSetDoWriteAdHoc) then
    if cbxDoWriteAdHoc.Checked then
      FOnSetDoWriteAdHoc(True)
    else
      FOnSetDoWriteAdHoc(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetDoSyncData;
begin
  if Assigned(FOnSetDoSyncData) then
    if cbxDoSyncData.Checked then
      FOnSetDoSyncData(True)
    else
      FOnSetDoSyncData(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetDoMegerData;
begin
  if Assigned(FOnSetDoMergeData) then
    if cbxDoMergeData.Checked then
      FOnSetDoMergeData(True)
    else
      FOnSetDoMergeData(False);

  if Assigned(FOnRefreshSettings) then
    FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetGroupIndex;
begin
  if Assigned(FOnSetGroupIndex) then
    FOnSetGroupIndex(edtGroupIndex.Value);

  if Assigned(FOnRefreshSettings) then
    FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetItemsByRegistry;
begin
  if Assigned(FOnSetItemsByRegistry) then
    if cbxItemsByRegistry.Checked then
      FOnSetItemsByRegistry(True)
    else
      FOnSetItemsByRegistry(False);

  if Assigned(FOnRefreshSettings) then
    FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetItems;
begin
  if Assigned(FOnSetItems) then
     FOnSetItems;
end;

procedure TRegControlButtonFrame.SetListSourceKind;
begin
  if Assigned(FOnSetListSourceKind) then
    FOnSetListSourceKind(TListSourceKind(rgbListSourceKind.ItemIndex));

  if Assigned(FOnRefreshSettings) then
    FOnRefreshSettings;

  RefreshRegControl;
end;

function TRegControlButtonFrame.ClearItems: boolean;
begin
  Result := Assigned(FOnClearItems);
  if Result then
    Result := FOnClearItems;
end;

function TRegControlButtonFrame.DeleteItem: boolean;
begin
  Result := Assigned(FOnDeleteItem);
  if Result then
    Result := FOnDeleteItem;
end;

procedure TRegControlButtonFrame.Refresh;
begin
  if Assigned(FOnGetCanRead) then
    cbxCanRead.Checked := FOnGetCanRead;

  if Assigned(FOnGetCanWrite) then
    cbxCanWrite.Checked := FOnGetCanWrite;

  if Assigned(FOnGetDoWriteAdHoc) then
    cbxDoWriteAdHoc.Checked := FOnGetDoWriteAdHoc;

  if Assigned(FOnGetDoSyncData) then
    cbxDoSyncData.Checked := FOnGetDoSyncData;

  if Assigned(FOnGetDoMergeData) then
  begin
    cbxDoMergeData.Enabled := True;
    cbxDoMergeData.Checked := FOnGetDoMergeData
  end
  else
    cbxDoMergeData.Enabled := False;

  if Assigned(FOnGetItemsByRegistry) then
  begin
    cbxItemsByRegistry.Enabled := True;
    cbxItemsByRegistry.Checked := FOnGetItemsByRegistry;
  end
  else
    cbxItemsByRegistry.Enabled := False;

  btnAdd.Enabled := Assigned(FOnSetItems);
  btnClear.Enabled := Assigned(FOnClearItems);
  btnDeleteItem.Enabled := Assigned(FOnDeleteItem);

  if Assigned(FOnGetListSourceKind) then
  begin
    rgbListSourceKind.Enabled := True;
    rgbListSourceKind.ItemIndex := Integer(FOnGetListSourceKind);
  end
  else
    rgbListSourceKind.Enabled := False;

  if Assigned(FOnGetGroupIndex) then
    edtGroupIndex.Value := FOnGetGroupIndex;
end;

constructor TRegControlButtonFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Refresh
end;

end.

