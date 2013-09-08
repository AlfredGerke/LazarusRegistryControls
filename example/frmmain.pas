unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Menus,
  ActnList,
  ComCtrls,
  regsourcen,
  regedit,
  regcheckgroup,
  reglistbox,
  regcombobox,
  regvaluelisteditor,
  regchecklistbox,
  regradiogroup,
  regcheckbox,
  regradiobutton,
  DefaultTranslator;

type

  { TMain }

  TMain = class(TForm)
    acClose: TAction;
    acCreateExampleSettings: TAction;
    acCheckExampleSettings: TAction;
    acRefreshData: TAction;
    acSyncDataOff: TAction;
    acRefreshSettingsSingleSource: TAction;
    acRefreshSettingListSource: TAction;
    acTestDeleteKey: TAction;
    acTestEraseSection: TAction;
    acTestRenameKey: TAction;
    acRefreshSettingsKombination: TAction;
    acAddValue: TAction;
    acEraseSectionValueEdit: TAction;
    acRefreshDataKombi: TAction;
    acEraseSectionValueEdit2: TAction;
    acDeleteRootKey: TAction;
    acClearItems: TAction;
    acShowClientEditDialog: TAction;
    acRefreshMergeDataOn: TAction;
    acRefreshMergeDataOff: TAction;
    acWriteAdHocOffList: TAction;
    acSyncDataListOff: TAction;
    acWriteAdHocOff: TAction;
    acWriteAdHocOn: TAction;
    acSyncDataOn: TAction;
    acRefreshDataList: TAction;
    acWriteAdHocOnList: TAction;
    acSyncDataListOn: TAction;
    ActionList1: TActionList;
    btnSetMergePropertyOn: TButton;
    btnSetMergePropertyOff: TButton;
    btnShowClientDialog: TButton;
    btnRefreshControls: TButton;
    btnRefreshControlsList: TButton;
    btnRefreshControlsList1: TButton;
    btnClearItems: TButton;
    btnSyncDataOfList: TButton;
    btnSyncDataOnList: TButton;
    btnWriteAdHocOffList: TButton;
    btnWriteAdHocOn: TButton;
    btnSyncDataOn: TButton;
    btnWriteAdHocOff: TButton;
    btnSyncDataOf: TButton;
    btnWriteAdHocOnList: TButton;
    cbxClientList: TComboBox;
    lblCheckListBox: TLabel;
    lblCheckListBox2: TLabel;
    lblCheckListBox3: TLabel;
    lblCheckGroup2: TLabel;
    lblComboBox2: TLabel;
    lblEditSingleValue1: TLabel;
    lblListBox2: TLabel;
    lblRadioGroup2: TLabel;
    lblRadioGroupList: TLabel;
    lblEditSingleValue: TLabel;
    lblListBox1: TLabel;
    lblComboBox1: TLabel;
    lblRadioGroup1: TLabel;
    lbListBoxList: TLabel;
    lblComboBox: TLabel;
    lblCheckGroup: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuDeleteRootKey: TMenuItem;
    mnuTestRenameKey: TMenuItem;
    mnuTestEraseSection: TMenuItem;
    mnuTestDeleteKey: TMenuItem;
    mnuTest: TMenuItem;
    mnuCheckCreateSettings: TMenuItem;
    mnuCreateSettings: TMenuItem;
    mnuSettingsList: TMenuItem;
    mnuRefresh: TMenuItem;
    mnuSettingsSingle: TMenuItem;
    mnuExampleSettings: TMenuItem;
    mnuClose: TMenuItem;
    mnuFile: TMenuItem;
    PageControl1: TPageControl;
    pnlClientKombination: TPanel;
    pnlClient1: TPanel;
    pnlClientList: TPanel;
    pnlClient: TPanel;
    pnlTop: TPanel;
    pnlTopList: TPanel;
    pnlTopKombination: TPanel;
    rcbxCheckBox1: TRegCheckBox;
    rcbxCheckBox2: TRegCheckBox;
    rcgCheckGroup3: TRegCheckGroup;
    redtComboBox2: TRegComboBox;
    redtComboBoxList1: TRegComboBox;
    redtControlName: TRegEdit;
    redtControlName1: TRegEdit;
    redtEdit: TRegEdit;
    redtComboBox1: TRegComboBox;
    rcgCheckGroup1: TRegCheckGroup;
    rcgCheckGroup2: TRegCheckGroup;
    redtEdit1: TRegEdit;
    redtEditKombi: TRegEdit;
    rcbxCheckBoxKombi: TRegCheckBox;
    rlbCheckedListBox: TRegCheckListBox;
    rcgCheckedGroup: TRegCheckGroup;
    RegistrySource3: TRegistrySource;
    rvlValueListEditorKombi2: TRegValueListEditor;
    rvlValueListEditorKombi1: TRegValueListEditor;
    rvlValueListEditor: TRegValueListEditor;
    rgrpRadioGroup2: TRegRadioGroup;
    rlbCheckListBox2: TRegCheckListBox;
    rlbCheckListBox1: TRegCheckListBox;
    RegistrySource1: TRegistrySource;
    RegistrySource2: TRegistrySource;
    rlbCheckListBox3: TRegCheckListBox;
    rlbListBox2: TRegListBox;
    rlbListBox1: TRegListBox;
    rgrpRadioGroup1: TRegRadioGroup;
    rgrpRadioGroupList1: TRegRadioGroup;
    rlbListBox3: TRegListBox;
    rrbRadioButton1: TRegRadioButton;
    rrbRadioButton2: TRegRadioButton;
    rrbRadioButton3: TRegRadioButton;
    rrbRadioButton4: TRegRadioButton;
    SpeedButton1: TSpeedButton;
    btnEraseValueEditList: TSpeedButton;
    SpeedButton2: TSpeedButton;
    tabSingleValue: TTabSheet;
    tabList: TTabSheet;
    tabKombination: TTabSheet;
    procedure acAddValueExecute(Sender: TObject);
    procedure acCheckExampleSettingsExecute(Sender: TObject);
    procedure acClearItemsExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCreateExampleSettingsExecute(Sender: TObject);
    procedure acDeleteRootKeyExecute(Sender: TObject);
    procedure acEraseSectionValueEdit2Execute(Sender: TObject);
    procedure acEraseSectionValueEditExecute(Sender: TObject);
    procedure acRefreshDataExecute(Sender: TObject);
    procedure acRefreshDataKombiExecute(Sender: TObject);
    procedure acRefreshDataListExecute(Sender: TObject);
    procedure acRefreshMergeDataOffExecute(Sender: TObject);
    procedure acRefreshMergeDataOnExecute(Sender: TObject);
    procedure acRefreshSettingListSourceExecute(Sender: TObject);
    procedure acRefreshSettingsKombinationExecute(Sender: TObject);
    procedure acRefreshSettingsSingleSourceExecute(Sender: TObject);
    procedure acShowClientEditDialogExecute(Sender: TObject);
    procedure acSyncDataListOffExecute(Sender: TObject);
    procedure acSyncDataListOnExecute(Sender: TObject);
    procedure acSyncDataOffExecute(Sender: TObject);
    procedure acTestDeleteKeyExecute(Sender: TObject);
    procedure acTestEraseSectionExecute(Sender: TObject);
    procedure acTestRenameKeyExecute(Sender: TObject);
    procedure acWriteAdHocOffExecute(Sender: TObject);
    procedure acWriteAdHocOffListExecute(Sender: TObject);
    procedure acWriteAdHocOnExecute(Sender: TObject);
    procedure acSyncDataOnExecute(Sender: TObject);
    procedure acWriteAdHocOnListExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure RefreshWriteAdHocOnOff(aFlag: integer;
                                     aSet: boolean);
    procedure RefreshSyncDataOnOff(aFlag: integer;
                                   aSet: boolean);
    function CheckForExampleSettings: boolean;
    procedure CreateSettings;
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.RefreshWriteAdHocOnOff(aFlag: integer;
  aSet: boolean);
begin
  case aFlag of
    0: RegistrySource1.RefreshWriteAdHocProperty(aSet);
    1: RegistrySource2.RefreshWriteAdHocProperty(aSet);
    2:
    begin
      RegistrySource3.RefreshWriteAdHocProperty(aSet);
      RegistrySource3.RefreshWriteAdHocProperty(False, 'redtEditKombi');
    end;
  end;
end;

procedure TMain.RefreshSyncDataOnOff(aFlag: integer;
  aSet: boolean);
begin
  case aFlag of
    0: RegistrySource1.RefreshSyncProperty(aSet);
    1: RegistrySource1.RefreshSyncProperty(aSet);
    2: RegistrySource1.RefreshSyncProperty(aSet);
  end;
end;

procedure TMain.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMain.acCheckExampleSettingsExecute(Sender: TObject);
begin
  if CheckForExampleSettings then
    MessageDlg('Der Schlüssel "Desktop" ist vorhanden und gefüllt. Beispieleinträge sind grundsätzlich vorhanden!',
      mtInformation,
      [mbOK],
      0)
  else
  begin
    MessageDlg('Der Schlüssel "Desktop" ist entweder nicht vorhanden oder nicht gefüllt. Beispieleinträge sind wahrscheinlich nicht vorhanden!',
      mtWarning,
      [mbOK],
      0);
    CreateSettings;
  end;
end;

procedure TMain.acClearItemsExecute(Sender: TObject);
begin
  RegistrySource3.ClearClientItems('',
    True,
    'Sollen die Listen gelöscht werden? (Wenn ReadDefaults=True werden sofort Defaults nachgeladen)');
end;

procedure TMain.acAddValueExecute(Sender: TObject);
var
  ident: string;
begin
  ident := Format('Key%d', [rvlValueListEditorKombi1.Strings.Count]);
  // BeginUpdate verhindert das Triggern das OnChange der RegistrySettings
  redtEditKombi.RegistrySettings.BeginUpdate;
  try
    redtEditKombi.RegistrySettings.Ident := ident;
  finally
    // EndUpdate muss unbedingt aufgerufen werden
    redtEditKombi.RegistrySettings.EndUpdate;
  end;
  redtEditKombi.WriteToReg;
end;

procedure TMain.acCreateExampleSettingsExecute(Sender: TObject);
begin
  CreateSettings;
end;

procedure TMain.acDeleteRootKeyExecute(Sender: TObject);
var
  old_sync_data: boolean;
  old_write_defaults: boolean;
begin
  with RegistrySource1 do
  begin
    Screen.Cursor := crHourGlass;
    old_sync_data := DoSyncData;
    old_write_defaults := WriteDefaults;
    // schalten die Synchronisierung für Element Clients dieser RegistrySource aus
    DoSyncData := False;
    // ermöglicht das Schreiben/Löschen von Defaults in der Registry
    WriteDefaults := True;

    DeleteRootKey;

    // aktualisiert alle Clients dieser RegistrySource
    RefreshControlData('', 0);
    WriteDefaults := old_write_defaults;
    DoSyncData := old_sync_data;
    Screen.Cursor := crDefault;
  end;

  with RegistrySource2 do
  begin
    Screen.Cursor := crHourGlass;
    old_sync_data := DoSyncData;
    old_write_defaults := WriteDefaults;
    // schalten die Synchronisierung für Element Clients dieser RegistrySource aus
    DoSyncData := False;
    // ermöglicht das Schreiben/Löschen von Defaults in der Registry
    WriteDefaults := True;

    DeleteRootKey;

    // aktualisiert alle Clients dieser RegistrySource
    RefreshControlData('', 0);
    WriteDefaults := old_write_defaults;
    DoSyncData := old_sync_data;
    Screen.Cursor := crDefault;
  end;

  with RegistrySource3 do
  begin
    Screen.Cursor := crHourGlass;
    old_sync_data := DoSyncData;
    old_write_defaults := WriteDefaults;
    // schalten die Synchronisierung für Element Clients dieser RegistrySource aus
    DoSyncData := False;
    // ermöglicht das Schreiben/Löschen von Defaults in der Registry
    WriteDefaults := True;

    DeleteRootKey;

    // aktualisiert alle Clients dieser RegistrySource
    RefreshControlData('', 0);
    WriteDefaults := old_write_defaults;
    DoSyncData := old_sync_data;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.acEraseSectionValueEdit2Execute(Sender: TObject);
begin
  rvlValueListEditorKombi2.ClearItems(True, 'Soll die Liste gelöscht werden?');
end;

procedure TMain.acEraseSectionValueEditExecute(Sender: TObject);
begin
  rvlValueListEditorKombi1.ClearItems;
end;

procedure TMain.acRefreshDataExecute(Sender: TObject);
begin
  RegistrySource1.RefreshControlData(Trim(redtControlName.Text));
end;

procedure TMain.acRefreshDataKombiExecute(Sender: TObject);
begin
  RegistrySource3.RefreshControlData;
end;

procedure TMain.acRefreshDataListExecute(Sender: TObject);
begin
  RegistrySource2.RefreshControlData(Trim(redtControlName1.Text));
end;

procedure TMain.acRefreshMergeDataOffExecute(Sender: TObject);
begin
  RegistrySource3.RefreshMergeDataProperty(False);
end;

procedure TMain.acRefreshMergeDataOnExecute(Sender: TObject);
begin
  RegistrySource3.RefreshMergeDataProperty(True);
end;

procedure TMain.acRefreshSettingListSourceExecute(Sender: TObject);
begin
  RegistrySource2.RefreshSettings;
end;

procedure TMain.acRefreshSettingsKombinationExecute(Sender: TObject);
begin
  RegistrySource3.RefreshSettings;
end;

procedure TMain.acRefreshSettingsSingleSourceExecute(Sender: TObject);
begin
  RegistrySource1.RefreshSettings;
end;

procedure TMain.acShowClientEditDialogExecute(Sender: TObject);
var
  client_name: string;
begin
  with RegistrySource3 do
  begin
    EditClientRootKeys := True;
    try
      client_name := Trim(cbxClientList.Text);
      if (client_name <> EmptyStr) then
        ShowClientEditDialog(client_name)
      else
        MessageDlg('Ungültiger Clientname gewählt!', mtWarning, [mbOk], 0);
    finally
      EditClientRootKeys := False;
    end;
  end;
end;

procedure TMain.acSyncDataListOffExecute(Sender: TObject);
begin
  RefreshSyncDataOnOff(1, False);
end;

procedure TMain.acSyncDataListOnExecute(Sender: TObject);
begin
  RefreshSyncDataOnOff(1, True);
end;

procedure TMain.acSyncDataOffExecute(Sender: TObject);
begin
  RefreshSyncDataOnOff(0, False);
end;

procedure TMain.acTestDeleteKeyExecute(Sender: TObject);
begin
  if (PageControl1.ActivePageIndex <> 1) then
    PageControl1.ActivePageIndex := 1;

  RegistrySource2.DeleteKey('CheckGroupListe',
    'CheckGroup5Key',
    0);
end;

procedure TMain.acTestEraseSectionExecute(Sender: TObject);
begin
  if (PageControl1.ActivePageIndex <> 1) then
    PageControl1.ActivePageIndex := 1;

 RegistrySource2.EraseSection('CheckGroupListe',
   0);
end;

procedure TMain.acTestRenameKeyExecute(Sender: TObject);
begin
  if (PageControl1.ActivePageIndex <> 1) then
    PageControl1.ActivePageIndex := 1;

  RegistrySource2.RenameKey('CheckGroupListe',
    'CheckGroup1Key',
    'CheckGroup1_1Key',
    0);
end;

procedure TMain.acWriteAdHocOffExecute(Sender: TObject);
begin
  RefreshWriteAdHocOnOff(0, False);
end;

procedure TMain.acWriteAdHocOffListExecute(Sender: TObject);
begin
  RefreshWriteAdHocOnOff(1, False);
end;

procedure TMain.acWriteAdHocOnExecute(Sender: TObject);
begin
  RefreshWriteAdHocOnOff(0, True);
end;

procedure TMain.acSyncDataOnExecute(Sender: TObject);
begin
  RefreshSyncDataOnOff(0, True);
end;

procedure TMain.acWriteAdHocOnListExecute(Sender: TObject);
begin
  RefreshWriteAdHocOnOff(1, True);
end;

procedure TMain.FormShow(Sender: TObject);
begin
  RefreshWriteAdHocOnOff(0, True);
  RefreshSyncDataOnOff(0, True);
  RefreshWriteAdHocOnOff(1, True);
  RefreshSyncDataOnOff(1, True);
  RefreshWriteAdHocOnOff(2, True);
  RefreshSyncDataOnOff(2, True);
  RegistrySource3.GetClientList(cbxClientList.Items);
  if cbxClientList.Items.Count > 0 then
    cbxClientList.ItemIndex := 0
  else
    cbxClientList.ItemIndex := -1;

end;

function TMain.CheckForExampleSettings: boolean;
var
  list: TStrings;
begin
  Result := False;
  with RegistrySource1 do
  begin
    list := TStringList.Create;
    try
      ReadSection('Desktop', list, False);
      Result := (list.Count > 0);
    finally
      if Assigned(list) then
        FreeAndNil(list);
    end;
  end;
end;

procedure TMain.CreateSettings;
var
  use_defaults: boolean;
  old_sync_data: boolean;
begin
  if (MessageDlg(
    'Sollen Beispieleinträge in der Registry erstellt werden? (Einträge werden im Root: HKEY_CURRENT_USER erstellt)',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    use_defaults :=
      (MessageDlg('Zu den Beispieleinträgen können Defaults vergeben werden. Sollen Defaults erstellt werden? (Defaults werden im Root: HKEY_LOCAL_MACHINE erstellt, Adminrechte eventuell notwendig!!!)',
         mtConfirmation,
         [mbYes, mbNo],
         0) = mrYes);

    with RegistrySource1 do
    begin
      Screen.Cursor := crHourGlass;
      old_sync_data := DoSyncData;
      DoSyncData := False;
      WriteDefaults := use_defaults;

      WriteString('Desktop', 'Version', '1.0.0');
      WriteString('Desktop', 'Projekt', 'LazarusRegistryControls');
      WriteString('Desktop', 'Git',
        'https://github.com/AlfredGerke/LazarusRegistryControls.git');
      WriteString('Desktop', 'Test', 'Einzelwerte');

      // Einzelwerte an Controls übergeben
      WriteBool('Einzelwerte', 'DoWriteAdHoc', False);
      WriteString('Einzelwerte', 'Edit', 'Test für Edit');
      WriteBool('Einzelwerte', 'CheckBox1', True);
      WriteBool('Einzelwerte', 'CheckBox2', False);
      WriteBool('Einzelwerte', 'RadioButton1', True);
      WriteBool('Einzelwerte', 'RadioButton2', False);

      // Index für Listen sichern
      WriteInteger('RadioGroup1', 'ItemIndex', 1);
      WriteInteger('Combobox1', 'ItemIndex', 1);
      WriteInteger('ListBox1', 'ItemIndex', 1);

      RefreshControlData('', 0);
      DoSyncData := old_sync_data;
      Screen.Cursor := crDefault;
    end;

    with RegistrySource2 do
    begin
      Screen.Cursor := crHourGlass;
      old_sync_data := DoSyncData;
      DoSyncData := False;
      WriteDefaults := use_defaults;

      WriteString('Desktop', 'Test', 'Listen');

      // Section als Liste laden
      WriteString('RaidoGroupListe', 'RaidoGroup1Key', 'RaidoGroup1Value');
      WriteString('RaidoGroupListe', 'RaidoGroup2Key', 'RaidoGroup2Value');
      WriteString('RaidoGroupListe', 'RaidoGroup3Key', 'RaidoGroup3Value');
      WriteString('RaidoGroupListe', 'RaidoGroup4Key', 'RaidoGroup4Value');
      WriteString('RaidoGroupListe', 'RaidoGroup5Key', 'RaidoGroup5Value');

      // Section als Liste laden
      WriteString('ListBoxListe', 'ListBox1Key', 'ListBox1Value');
      WriteString('ListBoxListe', 'ListBox2Key', 'ListBox2Value');
      WriteString('ListBoxListe', 'ListBox3Key', 'ListBox3Value');
      WriteString('ListBoxListe', 'ListBox4Key', 'ListBox4Value');
      WriteString('ListBoxListe', 'ListBox5Key', 'ListBox5Value');

      // Section als Liste laden
      // nur wenn als Value 1 oder True dann Ident in die Liste laden
      WriteString('ComboBoxListe', 'ComboBox1Key', 'ComboBox1Value');
      WriteString('ComboBoxListe', 'ComboBox2Key', 'ComboBox2Value');
      WriteString('ComboBoxListe', 'ComboBox3Key', 'ComboBox3Value');
      WriteString('ComboBoxListe', 'ComboBox4Key', 'ComboBox4Value');
      WriteString('ComboBoxListe', 'ComboBox5Key', 'ComboBox5Value');

      // Section als Liste laden
      // wenn als Value 1 oder True dann Ident gechecked
      WriteInteger('CheckGroupListe', 'CheckGroup1Key', 1);
      WriteInteger('CheckGroupListe', 'CheckGroup2Key', 1);
      WriteInteger('CheckGroupListe', 'CheckGroup3Key', 0);
      WriteBool('CheckGroupListe', 'CheckGroup4Key', True);
      WriteBool('CheckGroupListe', 'CheckGroup5Key', False);

      // Section als Liste laden
      // wenn als Value 1 oder True dann Ident gechecked
      WriteInteger('CheckListBoxListe', 'CheckListBox1Key', 1);
      WriteInteger('CheckListBoxListe', 'CheckListBox2Key', 0);
      WriteInteger('CheckListBoxListe', 'CheckListBox3Key', 1);
      WriteBool('CheckListBoxListe', 'CheckListBox4Key', True);
      WriteBool('CheckListBoxListe', 'CheckListBox5Key', False);

      WriteString('CheckListBoxListe2', 'CheckListBox1Key', '1');
      WriteString('CheckListBoxListe2', 'CheckListBox2Key', '0');
      WriteString('CheckListBoxListe2', 'CheckListBox3Key', '1');
      WriteString('CheckListBoxListe2', 'CheckListBox4Key', 'True');
      WriteString('CheckListBoxListe2', 'CheckListBox5Key', 'False');


      // Section als Liste laden (Key=Value)
      WriteString('RegValueListe', 'Key1', 'Value1');
      WriteString('RegValueListe', 'Key2', 'Value2');
      WriteString('RegValueListe', 'Key3', 'Value3');
      WriteString('RegValueListe', 'Key4', 'Value4');
      WriteString('RegValueListe', 'Key5', 'Value5');

      RefreshControlData('', 0);
      DoSyncData := old_sync_data;
      Screen.Cursor := crDefault;
    end;

    with RegistrySource3 do
    begin
      Screen.Cursor := crHourGlass;
      old_sync_data := DoSyncData;
      DoSyncData := False;
      WriteDefaults := use_defaults;

      WriteString('Desktop', 'Test', 'Kombinationen');

      // Section als Liste laden (Key=Value)
      WriteInteger('RegValueListe', 'Key1', 0);
      WriteInteger('RegValueListe', 'Key2', 1);
      WriteInteger('RegValueListe', 'Key3', 2);
      WriteBool('RegValueListe', 'Key4', True);
      WriteBool('RegValueListe', 'Key5', False);

      RefreshControlData('', 0);
      DoSyncData := old_sync_data;
      Screen.Cursor := crDefault;
    end;
  end;
end;

end.
