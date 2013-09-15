unit frmmain;

{$mode Delphi}{$H+}

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
  DefaultTranslator,
  regtype;

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
    acShowClientEditDialogKombi: TAction;
    acRefreshMergeDataOn: TAction;
    acRefreshMergeDataOff: TAction;
    acShowClientEditDialogList: TAction;
    acShowClientEditDialogSingle: TAction;
    acRefreshDataAllSingle: TAction;
    acRefreshDataAllList: TAction;
    acRefreshDataAllKombi: TAction;
    acPostDataSingle: TAction;
    acPostDataList: TAction;
    acPostDataKombi: TAction;
    acWriteAdHocOffList: TAction;
    acSyncDataListOff: TAction;
    acWriteAdHocOff: TAction;
    acWriteAdHocOn: TAction;
    acSyncDataOn: TAction;
    acRefreshDataList: TAction;
    acWriteAdHocOnList: TAction;
    acSyncDataListOn: TAction;
    ActionList1: TActionList;
    btnEraseValueEditList: TSpeedButton;
    btnSetMergePropertyOn: TButton;
    btnSetMergePropertyOff: TButton;
    btnShowClientDialogKombi: TButton;
    btnRefreshControls: TButton;
    btnRefreshControlsList: TButton;
    btnRefreshControlsList1: TButton;
    btnClearItems: TButton;
    btnShowClientDialogList: TButton;
    btnShowClientDialogSingle: TButton;
    btnSyncDataOfList: TButton;
    btnSyncDataOnList: TButton;
    btnWriteAdHocOffList: TButton;
    btnWriteAdHocOn: TButton;
    btnSyncDataOn: TButton;
    btnWriteAdHocOff: TButton;
    btnSyncDataOf: TButton;
    btnWriteAdHocOnList: TButton;
    cbxClientNameDynamic: TRegComboBox;
    lblCheckListBox: TLabel;
    lblCheckListBox3: TLabel;
    lblCheckGroup2: TLabel;
    lblComboBox2: TLabel;
    lblValueEditList: TLabel;
    lblEditSingleValue1: TLabel;
    lblListBox2: TLabel;
    lblRadioGroup2: TLabel;
    lblRadioGroupList: TLabel;
    lbListBoxList: TLabel;
    lblComboBox: TLabel;
    lblCheckGroup: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuPostDataKombi: TMenuItem;
    mnuPostDataSingle: TMenuItem;
    mnuPostDataList: TMenuItem;
    mnuRefreshDataKombi: TMenuItem;
    mnuRefreshDataList: TMenuItem;
    mnuSettingsKombi: TMenuItem;
    MenuItem3: TMenuItem;
    mnuRefreshDataSingle: TMenuItem;
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
    PageControl2: TPageControl;
    pnlClientKombination: TPanel;
    pnlClient: TPanel;
    pnlClientList: TPanel;
    pnlTop: TPanel;
    pnlTopList: TPanel;
    pnlTopKombination: TPanel;
    rcbxCheckBoxStatic: TRegCheckBox;
    rcbxCheckBoxKombi: TRegCheckBox;
    rcgCheckedGroupKombi: TRegCheckGroup;
    rcgCheckGroupStatic: TRegCheckGroup;
    redtComboBoxStatic: TRegComboBox;
    redtComboBoxDynamic: TRegComboBox;
    rcgCheckGroupDynamic: TRegCheckGroup;
    redtEditStatic: TRegEdit;
    redtEditKombi: TRegEdit;
    cbxClientNameStatic: TRegComboBox;
    cbxClientNameKombi: TRegComboBox;
    rcbxHintOnCustomItemCheck: TRegCheckBox;
    RegistrySource3: TRegistrySource;
    rlbCheckedListBoxKombi: TRegCheckListBox;
    rvlValueListEditorDynamic: TRegValueListEditor;
    rgrpRadioGroupStatic: TRegRadioGroup;
    rlbCheckListBoxDynamic: TRegCheckListBox;
    RegistrySource1: TRegistrySource;
    RegistrySource2: TRegistrySource;
    rlbCheckListBoxStatic: TRegCheckListBox;
    rlbListBoxDynamic: TRegListBox;
    rgrpRadioGroupDynamic: TRegRadioGroup;
    rlbListBoxStatic: TRegListBox;
    rrbRadioButtonStatic1: TRegRadioButton;
    rrbRadioButtonStatic2: TRegRadioButton;
    rvlValueListEditorKombi1: TRegValueListEditor;
    rvlValueListEditorKombi2: TRegValueListEditor;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    tabGroupIndex_1: TTabSheet;
    tabGroupIndex_2: TTabSheet;
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
    procedure acPostDataKombiExecute(Sender: TObject);
    procedure acPostDataListExecute(Sender: TObject);
    procedure acPostDataSingleExecute(Sender: TObject);
    procedure acRefreshDataAllKombiExecute(Sender: TObject);
    procedure acRefreshDataAllListExecute(Sender: TObject);
    procedure acRefreshDataAllSingleExecute(Sender: TObject);
    procedure acRefreshDataExecute(Sender: TObject);
    procedure acRefreshDataKombiExecute(Sender: TObject);
    procedure acRefreshDataListExecute(Sender: TObject);
    procedure acRefreshMergeDataOffExecute(Sender: TObject);
    procedure acRefreshMergeDataOnExecute(Sender: TObject);
    procedure acRefreshSettingListSourceExecute(Sender: TObject);
    procedure acRefreshSettingsKombinationExecute(Sender: TObject);
    procedure acRefreshSettingsSingleSourceExecute(Sender: TObject);
    procedure acShowClientEditDialogListExecute(Sender: TObject);
    procedure acShowClientEditDialogKombiExecute(Sender: TObject);
    procedure acShowClientEditDialogSingleExecute(Sender: TObject);
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
    procedure rcbxCheckBoxKombiBeforeRegistrySettingChange(
      aOldSettingInfo: TRegistrySettingValue;
      aNewSettingInfo: TRegistrySettingValue; var aIsOk: boolean);
    procedure rcgCheckGroupStaticCustomItemCheck(Sender: TObject; Index: integer
      );
    procedure rcgCheckGroupStaticItemClick(Sender: TObject; Index: integer);
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
    // Für alle Clients der RegistrySource1 wird die Eigenschaft DoWriteAdHoc
    // auf den Wert von aSet gesetzt
    0: RegistrySource1.RefreshWriteAdHocProperty(aSet);
    // Für alle Clients der RegistrySource2 wird die Eigenschaft DoWriteAdHoc
    // auf den Wert von aSet gesetzt
    1: RegistrySource2.RefreshWriteAdHocProperty(aSet);
    2:
    begin
      // Für alle Clients der RegistrySource3 wird  wird die Eigenschaft DoWriteAdHoc
      // auf den Wert von aSet gesetzt
      RegistrySource3.RefreshWriteAdHocProperty(aSet);
      // Für den Client: redtEditKombi der RegistrySource3  wird die Eigenschaft
      // DoWriteAdHoc auf den Wert von False gesetzt
      RegistrySource3.RefreshWriteAdHocProperty(False, 'redtEditKombi');
    end;
  end;
end;

procedure TMain.RefreshSyncDataOnOff(aFlag: integer;
  aSet: boolean);
begin
  case aFlag of
    // Für alle Clients der RegistrySource1 wird die Eigenschaft DoSyncData
    // auf den Wert von aSet gesetzt
    0: RegistrySource1.RefreshSyncProperty(aSet);
    // Für alle Clients der RegistrySource2 wird die Eigenschaft DoSyncData
    // auf den Wert von aSet gesetzt
    1: RegistrySource2.RefreshSyncProperty(aSet);
    // Für alle Clients der RegistrySource3 wird die Eigenschaft DoSyncData
    // auf den Wert von aSet gesetzt
    2: RegistrySource3.RefreshSyncProperty(aSet);
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
      mtInformation, [mbOK], 0)
  else
  begin
    MessageDlg('Der Schlüssel "Desktop" ist entweder nicht vorhanden oder nicht gefüllt. Beispieleinträge sind wahrscheinlich nicht vorhanden!',
      mtWarning, [mbOK], 0);

    CreateSettings;
  end;
end;

procedure TMain.acClearItemsExecute(Sender: TObject);
begin
  // Allen Clients der RegistrySource3 welche eine Liste verwalten (z.B.: TRegListBox, etc.)
  // wird nach positiver Beantwortung der Benutzeranfrage diese Liste gelöscht
  // Wenn die Eigenschaft ReadDefaults auf True steht, werden Standards aus der
  // Registry nachgeladen, wenn vorhanden
  RegistrySource3.ClearClientItems('', True,
    'Sollen die Listen gelöscht werden? (Wenn ReadDefaults=True werden sofort Defaults nachgeladen)');
end;

procedure TMain.acAddValueExecute(Sender: TObject);
var
  ident: string;
begin
  // Ident neu definieren
  ident := Format('Key%d', [rvlValueListEditorKombi1.Strings.Count]);
  // BeginUpdate verhindert das Triggern von OnChange-Events der RegistrySettings
  redtEditKombi.RegistrySettings.BeginUpdate;
  try
    // Ident neu setzen
    redtEditKombi.RegistrySettings.Ident := ident;
  finally
    // EndUpdate muss unbedingt aufgerufen werden damit OnChange-Events der RegistrySettings
    // wider getriggert werden
    redtEditKombi.RegistrySettings.EndUpdate;
  end;
  // Daten des Steuerelementes in die Registry schreiben
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
    // Aktuelle Einstellung für DoSyncData sichern
    old_sync_data := DoSyncData;
    // Aktuelle Einstellung für WriteDefaults sichern
    old_write_defaults := WriteDefaults;
    // Schaltet die Synchronisierung von Clients dieser RegistrySource aus
    DoSyncData := False;
    // Ermöglicht das Schreiben/Löschen von Defaults in der Registry
    WriteDefaults := True;

    // Ermittelt alle Sectionen unterhalb von RegistrySettings.RootKey und entfernt diese
    DeleteRootKey;

    // Aktualisiert alle Clients dieser RegistrySource
    RefreshClientData('', 0);
    // Gesicherten Wert von WriteDefaults setzen
    WriteDefaults := old_write_defaults;
    // Gesicherten Wert von DoSyncData setzen
    DoSyncData := old_sync_data;
    Screen.Cursor := crDefault;
  end;

  with RegistrySource2 do
  begin
    Screen.Cursor := crHourGlass;
    // Aktuelle Einstellung für DoSyncData sichern
    old_sync_data := DoSyncData;
    // Aktuelle Einstellung für WriteDefaults sichern
    old_write_defaults := WriteDefaults;
    // Schalten die Synchronisierung von Clients dieser RegistrySource aus
    DoSyncData := False;
    // Ermöglicht das Schreiben/Löschen von Defaults in der Registry
    WriteDefaults := True;

    // Ermittelt alle Sectionen unterhalb von RegistrySettings.RootKey und entfernt diese
    DeleteRootKey;

    // Aktualisiert alle Clients dieser RegistrySource
    RefreshClientData('', 0);
    // Gesicherten Wert von WriteDefaults setzen
    WriteDefaults := old_write_defaults;
    // Gesicherten Wert von DoSyncData setzen
    DoSyncData := old_sync_data;
    Screen.Cursor := crDefault;
  end;

  with RegistrySource3 do
  begin
    Screen.Cursor := crHourGlass;
    // Aktuelle Einstellung für DoSyncData sichern
    old_sync_data := DoSyncData;
    // Aktuelle Einstellung für WriteDefaults sichern
    old_write_defaults := WriteDefaults;
    // Schalten die Synchronisierung von Clients dieser RegistrySource aus
    DoSyncData := False;
    // Ermöglicht das Schreiben/Löschen von Defaults in der Registry
    WriteDefaults := True;

    // Ermittelt alle Sectionen unterhalb von RegistrySettings.RootKey und entfernt diese
    DeleteRootKey;

    // Aktualisiert alle Clients dieser RegistrySource
    RefreshClientData('', 0);
    // Gesicherten Wert von WriteDefaults setzen
    WriteDefaults := old_write_defaults;
    // Gesicherten Wert von DoSyncData setzen
    DoSyncData := old_sync_data;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMain.acEraseSectionValueEdit2Execute(Sender: TObject);
begin
  // Löscht nach positiver Beantwortung der Benutzeranfrage die Liste des
  // Steuerelementes (Client)
  // Die Benutzeranfrage wird frei formuliert
  rvlValueListEditorKombi2.ClearItems(True, 'Soll die Liste gelöscht werden?');
end;

procedure TMain.acEraseSectionValueEditExecute(Sender: TObject);
begin
  // Löscht nach positiver Beantwortung der Benutzeranfrage die Liste des
  // Steuerelementes (Client)
  // Es wird eine Standardbenutzeranfrage formuliert
  rvlValueListEditorKombi1.ClearItems;
end;

procedure TMain.acPostDataKombiExecute(Sender: TObject);
var
  old_sync_data_value: boolean;
begin
  // Daten aller Steuerelemente (Clients) der RegistrySource in die Registry
  // schreiben
  with RegistrySource3 do
  begin
    old_sync_data_value := DoSyncData;
    // Das Sichern von Werten in die Registry für mehrere Clients bei DoWriteAdHoc=False
    // kann nur dann erfolgreich geschehen, wenn die Synchronisierung ausgeschaltet wird
    DoSyncData := False;
    // Eingeschaltete Synchronisierung würde dafür sorgen, das nach dem Schreiben des ersten
    // Clients alle anderen Clients mit Daten aus der Registry aktualisiert würden, was deren
    // bis dahin eingestellte Werte verwerfen würde
    PostClientData;
    DoSyncData := old_sync_data_value;
  end;
end;

procedure TMain.acPostDataListExecute(Sender: TObject);
var
  old_sync_data_value: boolean;
begin
  // Daten aller Steuerelemente (Clients) der RegistrySource in die Registry
  // schreiben
  with RegistrySource2 do
  begin
    old_sync_data_value := DoSyncData;
    // Das Sichern von Werten in die Registry für mehrere Clients bei DoWriteAdHoc=False
    // kann nur dann erfolgreich geschehen, wenn die Synchronisierung ausgeschaltet wird
    DoSyncData := False;
    // Eingeschaltete Synchronisierung würde dafür sorgen, das nach dem Schreiben des ersten
    // Clients alle anderen Clients mit Daten aus der Registry aktualisiert würden, was deren
    // bis dahin eingestellte Werte verwerfen würde
    PostClientData;
    DoSyncData := old_sync_data_value;
  end;
end;

procedure TMain.acPostDataSingleExecute(Sender: TObject);
var
  old_sync_data_value: boolean;
begin
  // Daten aller Steuerelemente (Clients) der RegistrySource in die Registry
  // schreiben
  with RegistrySource1 do
  begin
    old_sync_data_value := DoSyncData;
    // Das Sichern von Werten in die Registry für mehrere Clients bei DoWriteAdHoc=False
    // kann nur dann erfolgreich geschehen, wenn die Synchronisierung ausgeschaltet wird
    DoSyncData := False;
    // Eingeschaltete Synchronisierung würde dafür sorgen, das nach dem Schreiben des ersten
    // Clients alle anderen Clients mit Daten aus der Registry aktualisiert würden, was deren
    // bis dahin eingestellte Werte verwerfen würde
    PostClientData;
    DoSyncData := old_sync_data_value;
  end;
end;

procedure TMain.acRefreshDataAllKombiExecute(Sender: TObject);
begin
  // Alle Steuerelemente (Clients) der RegistrySource mit aktuellen Werten aus
  // der Registry vesorgen
  RegistrySource3.RefreshClientData;
end;

procedure TMain.acRefreshDataAllListExecute(Sender: TObject);
begin
  // Alle Steuerelemente (Clients) der RegistrySource mit aktuellen Werten aus
  // der Registry vesorgen
  RegistrySource2.RefreshClientData;
end;

procedure TMain.acRefreshDataAllSingleExecute(Sender: TObject);
begin
  // Alle Steuerelemente (Clients) der RegistrySource mit aktuellen Werten aus
  // der Registry vesorgen
  RegistrySource1.RefreshClientData;
end;

procedure TMain.acRefreshDataExecute(Sender: TObject);
begin
  // Aktualisert das Steuerelement (Client), dessen Name als String übergeben wurde
  // Ein Leerstring wird alle Steuerelemente (Clients) der RegistrySource aktualisieren
  RegistrySource1.RefreshClientData(Trim(cbxClientNameStatic.Text));
end;

procedure TMain.acRefreshDataKombiExecute(Sender: TObject);
begin
  // Aktualisert alle Steuerelemente (Clients) der RegistrySource
  RegistrySource3.RefreshClientData;
end;

procedure TMain.acRefreshDataListExecute(Sender: TObject);
begin
  // Aktualisert das Steuerelement (Client), dessen Name als String übergeben wurde
  // Ein Leerstring wird alle Steuerelemente (Clients) der RegistrySource aktualisieren
  RegistrySource2.RefreshClientData(Trim(cbxClientNameDynamic.Text));
end;

procedure TMain.acRefreshMergeDataOffExecute(Sender: TObject);
begin
  // Schaltet die Eigenschaft MergeData aller Listenelemente der RegistrySource aus
  // Wenn MergeData auf False steht, werden Listen nicht automatisch mit Standards
  // aufgefüllt
  // MergeData greift immer dann, wenn eine Liste nicht vollständig ist und durch
  // Standards automatisch aufgefüllt werden könnte
  // Ist die Liste komplett leer, werden Standards immer geladen unabhängig ob
  // MergeData auf False steht
  // Dieses Verhalten lässt sich auschalten in dem man ReadDefaults auf False setzt
  RegistrySource3.RefreshMergeDataProperty(False);
end;

procedure TMain.acRefreshMergeDataOnExecute(Sender: TObject);
begin
  // Schaltet die Eigenschaft MergeData aller Listenelemente der RegistrySource an
  // MergeData greift immer dann, wenn eine Liste nicht vollständig ist und durch
  // Standards automatisch aufgefüllt werden könnte
  // Ist die Liste komplett leer, werden Standards immer geladen unabhängig ob
  // MergeData auf False steht
  // Dieses Verhalten lässt sich auschalten in dem man ReadDefaults auf False setzt
  RegistrySource3.RefreshMergeDataProperty(True);
end;

procedure TMain.acRefreshSettingListSourceExecute(Sender: TObject);
begin
  // Alle RootKeys der Steuerelemente (Clients) der RegistrySource werden mit aktuellen
  // Schlüsseln versorgt
  RegistrySource2.RefreshSettings;
end;

procedure TMain.acRefreshSettingsKombinationExecute(Sender: TObject);
begin
  // Alle RootKeys der Steuerelemente (Clients) der RegistrySource werden mit aktuellen
  // Schlüsseln versorgt
  RegistrySource3.RefreshSettings;
end;

procedure TMain.acRefreshSettingsSingleSourceExecute(Sender: TObject);
begin
  // Alle RootKeys der Steuerelemente (Clients) der RegistrySource werden mit aktuellen
  // Schlüsseln versorgt
  RegistrySource1.RefreshSettings;
end;

procedure TMain.acShowClientEditDialogListExecute(Sender: TObject);
var
  client_name: string;
begin
  // Mit diesem Code wird auf Eigenschaften (RootKeys) eines Steuerelementes (Client) zugegriffen,
  // welche nicht in den RegistrySettings veröffentlicht wurden
  with RegistrySource2 do
  begin
    // Sorgt dafür das der Dialog im Editiermodus erscheint und nicht im Ansichtsmodus
    EditClientRootKeys := True;
    try
      // Auswahl des Steuerelementes (Client), dessen RootKeys angepasst werden sollen
      client_name := Trim(cbxClientNameDynamic.Text);
      // Der Dialog kann nur für ein gültiges Steuerelement aufgerufen werden
      if (client_name <> EmptyStr) then
        ShowClientEditDialog(client_name)
      else
        MessageDlg('Ungültiger Clientname gewählt!', mtWarning, [mbOk], 0);
    finally
      // Sorgt dafür der Editiermodus wieder auf False gesetzt wird
      EditClientRootKeys := False;
    end;
  end;
end;

procedure TMain.acShowClientEditDialogKombiExecute(Sender: TObject);
var
  client_name: string;
begin
  // Mit diesem Code wird auf Eigenschaften (RootKeys) eines Steuerelementes (Client) zugegriffen,
  // welche nicht in den RegistrySettings veröffentlicht wurden
  with RegistrySource3 do
  begin
    // Sorgt dafür das der Dialog im Editiermodus erscheint und nicht im Ansichtsmodus
    EditClientRootKeys := True;
    try
      // Auswahl des Steuerelementes (Client), dessen RootKeys angepasst werden sollen
      client_name := Trim(cbxClientNameKombi.Text);
      // Der Dialog kann nur für ein gültiges Steuerelement aufgerufen werden
      if (client_name <> EmptyStr) then
        ShowClientEditDialog(client_name)
      else
        MessageDlg('Ungültiger Clientname gewählt!', mtWarning, [mbOk], 0);
    finally
      // Sorgt dafür der Editiermodus wieder auf False gesetzt wird
      EditClientRootKeys := False;
    end;
  end;
end;

procedure TMain.acShowClientEditDialogSingleExecute(Sender: TObject);
var
  client_name: string;
begin
  // Mit diesem Code wird auf Eigenschaften (RootKeys) eines Steuerelementes (Client) zugegriffen,
  // welche nicht in den RegistrySettings veröffentlicht wurden
  with RegistrySource1 do
  begin
    // Sorgt dafür das der Dialog im Editiermodus erscheint und nicht im Ansichtsmodus
    EditClientRootKeys := True;
    try
      // Auswahl des Steuerelementes (Client), dessen RootKeys angepasst werden sollen
      client_name := Trim(cbxClientNameStatic.Text);
      // Der Dialog kann nur für ein gültiges Steuerelement aufgerufen werden
      if (client_name <> EmptyStr) then
        ShowClientEditDialog(client_name)
      else
        MessageDlg('Ungültiger Clientname gewählt!', mtWarning, [mbOk], 0);
    finally
      // Sorgt dafür der Editiermodus wieder auf False gesetzt wird
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
var
  section: string;
  key: string;
begin
  if (PageControl1.ActivePageIndex <> 1) then
    PageControl1.ActivePageIndex := 1;

  section := 'CheckGroupListe';
  key := 'CheckGroup5Key';

  MessageDlg(Format('Datenwert: %s in Section: %s im Schlüssel: %s löschen!',
    [key, section, RegistrySource1.GetRootKey]), mtInformation, [mbOk], 0);

  // Löscht den Datenwerte (key) der Section (section)
  // Der GroupIndex 0 sorgt dafür das alle zugehörigen Steuerelemente (Clients)
  // synchronisiert werden
  RegistrySource2.DeleteKey(section, key, 0);
end;

procedure TMain.acTestEraseSectionExecute(Sender: TObject);
var
  section: string;
begin
  if (PageControl1.ActivePageIndex <> 1) then
    PageControl1.ActivePageIndex := 1;

  section := 'CheckGroupListe';

  MessageDlg(Format('Section: %s im Schlüssel: %s löschen!',
    [section, RegistrySource1.GetRootKey]), mtInformation, [mbOk], 0);

  // Löscht die Section (section)
  // Der GroupIndex 0 sorgt dafür das alle zugehörigen Steuerelemente (Clients)
  // synchronisiert werden
  RegistrySource2.EraseSection(section, 0);
end;

procedure TMain.acTestRenameKeyExecute(Sender: TObject);
var
  section: string;
  key_old: string;
  key_new: string;
begin
  if (PageControl1.ActivePageIndex <> 1) then
    PageControl1.ActivePageIndex := 1;

  section := 'CheckGroupListe';
  key_old := 'CheckGroup1Key';
  key_new := 'CheckGroup1_1Key';

  MessageDlg(Format('Datenwert: %s in Section: %s im Schlüssel: %s in %s umbenennen!',
    [key_old, section, RegistrySource1.GetRootKey, key_new]), mtInformation, [mbOk], 0);

  // Der Datenwert key_old wird in key_new umbenannt
  // Der GroupIndex 0 sorgt dafür das alle zugehörigen Steuerelemente (Clients)
  // synchronisiert werden
  RegistrySource2.RenameKey(section, key_old, key_new, 0);
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
  // Liste aller zugehörigen Steuerelemente (Clients) ermitteln
  RegistrySource1.GetClientList(cbxClientNameStatic.Items);
  // Daten für das Stuerelement aus der Registry lesen
  cbxClientNameStatic.ReadFromReg;
  // Liste aller zugehörigen Steuerelemente (Clients) ermitteln
  RegistrySource2.GetClientList(cbxClientNameDynamic.Items);
  // Daten für das Stuerelement aus der Registry lesen
  cbxClientNameDynamic.ReadFromReg;
  // Liste aller zugehörigen Steuerelemente (Clients) ermitteln
  RegistrySource3.GetClientList(cbxClientNameKombi.Items);
  // Daten für das Stuerelement aus der Registry lesen
  cbxClientNameKombi.ReadFromReg;
end;

procedure TMain.rcbxCheckBoxKombiBeforeRegistrySettingChange(
  aOldSettingInfo: TRegistrySettingValue;
  aNewSettingInfo: TRegistrySettingValue; var aIsOk: boolean);
begin
  // Mit diesem Code wird demonstriert wie man Eingaben in die RegistrySettings
  // überprüfen und auf Wunsch ablehnen kann
  // aNewSettingInfo.Kind informiert welcher Eintrag geändert werden soll
  case aNewSettingInfo.Kind of
    rskWriteDefaults:
    begin
      // aOldSettingInfo informiert über die aktuelle Einstellung
      // aNewSettingInfo informiert über die gewünscht Änderung
      // Es kann immer nur die Eigenschaft abgefragt werden, welche durch
      // aNewSettingInfo.Kind vorgegeben wird
      // z.B.: rskWriteDefaults -> WriteDefaults; rsgReadDefaults -> ReadDefaults
      if (aOldSettingInfo.WriteDefaults <> aNewSettingInfo.WriteDefaults) then
      begin
        // Der Parameter wird an die aufrufende Methode übergeben und ausgewertet
        // aIsOk := False verhindert das setzen der Eigenschaft auf den neuen Wert
        aIsOk := False;
        if aNewSettingInfo.WriteDefaults then
          MessageDlg('Das Schreiben von Standards ist nicht erlaubt!',
            mtWarning, [mbOk], 0)
        else
          MessageDlg('Das Schreiben von Standards muss eingeschaltet bleiben!',
            mtWarning, [mbOk], 0)
      end;
    end;
    rskReadDefaults:
    begin
      if (aOldSettingInfo.ReadDefaults <> aNewSettingInfo.ReadDefaults) then
      begin
        if aNewSettingInfo.ReadDefaults then
          aIsOk :=
            (MessageDlg('Soll das Lesen von Standards eingeschaltet werden?',
               mtConfirmation, [mbYes, mbNo], 0) = mrYes)
        else
          aIsOk :=
            (MessageDlg('Soll das Lesen von Standards abgeschaltet werden?',
               mtConfirmation, [mbYes, mbNo], 0) = mrYes);
      end;
    end;
  else
    aIsOk := True;
  end;
end;

procedure TMain.rcgCheckGroupStaticCustomItemCheck(Sender: TObject;
  Index: integer);
begin
  if rcbxHintOnCustomItemCheck.Checked then
    ShowMessage('2. OnCustomItemCheck [Sender: ' + Self.Name + ' - Index: ' + IntToStr(index) + '] - Wenn möglich für OnItemClick verwenden!');
end;

procedure TMain.rcgCheckGroupStaticItemClick(Sender: TObject; Index: integer);
begin
  if rcbxHintOnCustomItemCheck.Checked then
    ShowMessage('1. OnItemClick [Sender: ' + Self.Name + ' - Index: ' + IntToStr(index) + '] - Wenn möglich durch OnCustomItemCheck ersetzen!');
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
      // List die gesamte Section 'Desctop' aus
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

    // Die RegistrySource ist mit Steuerelementen (Clients) verbunden, die nur
    // Einzelwerte sicher wie den Text eines TRegEdit oder den ItemIndex einer
    // TRegListbox
    // Listen werden in diesem Beispielen nicht dynamisch geladen sondern zur
    // statisch festgelegt
    with RegistrySource1 do
    begin
      Screen.Cursor := crHourGlass;
      // Aktuallen Wert für DoSyncData sichern
      old_sync_data := DoSyncData;
      DoSyncData := False;
      // Wenn use_defaults auf TRUE steht, werden Defaulteinträge (Standards) in
      // der Registry eingerichtet
      // In den RegistrySettings wird unter RootForDefaults und RootKeyForDefaults
      // festgelegt wo diese Defaults hinterlegt werden
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

      // Aktualisiert alle mit der RegistrySource verbundenen Steuerelemente (Clients)
      RefreshClientData('', 0);
      DoSyncData := old_sync_data;
      Screen.Cursor := crDefault;
    end;

    // Die RegistrySource ist mit Steuerelementen (Clients) verbunden, die
    // demonstrieren sollen wie Listen dynamisch aus der Registry geladen und
    // verwaltet werden (z.B.: TRegCheckListBox)
    with RegistrySource2 do
    begin
      Screen.Cursor := crHourGlass;
      // Aktuallen Wert für DoSyncData sichern
      old_sync_data := DoSyncData;
      DoSyncData := False;
      // Wenn use_defaults auf TRUE steht, werden Defaulteinträge (Standards) in
      // der Registry eingerichtet
      // In den RegistrySettings wird unter RootForDefaults und RootKeyForDefaults
      // festgelegt wo diese Defaults hinterlegt werden
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

      // Aktualisiert alle mit der RegistrySource verbundenen Steuerelemente (Clients)
      RefreshClientData('', 0);
      DoSyncData := old_sync_data;
      Screen.Cursor := crDefault;
    end;

    // Die RegistrySource ist mit Steuerelementen (Clients) verbunden, die die
    // Kombination von verschiedenen Steuerelementen demonstrieren sollen
    with RegistrySource3 do
    begin
      Screen.Cursor := crHourGlass;
      // Aktuallen Wert für DoSyncData sichern
      old_sync_data := DoSyncData;
      DoSyncData := False;
      // Wenn use_defaults auf TRUE steht, werden Defaulteinträge (Standards) in
      // der Registry eingerichtet
      // In den RegistrySettings wird unter RootForDefaults und RootKeyForDefaults
      // festgelegt wo diese Defaults hinterlegt werden
      WriteDefaults := use_defaults;

      WriteString('Desktop', 'Test', 'Kombinationen');

      // Section als Liste laden (Key=Value)
      WriteInteger('RegValueListe', 'Key1', 0);
      WriteInteger('RegValueListe', 'Key2', 1);
      WriteInteger('RegValueListe', 'Key3', 2);
      WriteBool('RegValueListe', 'Key4', True);
      WriteBool('RegValueListe', 'Key5', False);

      // Aktualisiert alle mit der RegistrySource verbundenen Steuerelemente (Clients)
      RefreshClientData('', 0);
      DoSyncData := old_sync_data;
      Screen.Cursor := crDefault;
    end;
  end;
end;

end.
