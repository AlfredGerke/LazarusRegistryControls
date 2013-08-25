unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Menus, ActnList, ComCtrls, CheckLst, regsourcen, regedit,
  regcheckgroup, reglistbox, regcombobox, regvaluelisteditor, regchecklistbox,
  regradiogroup, regcheckbox, regradiobutton, regtype;

type

  { TMain }

  TMain = class(TForm)
    acClose: TAction;
    acCreateExampleSettings: TAction;
    acCheckExampleSettings: TAction;
    acRefreshData: TAction;
    acSyncDataOff: TAction;
    acWriteAdHocOffList: TAction;
    acSyncDataListOff: TAction;
    acWriteAdHocOff: TAction;
    acWriteAdHocOn: TAction;
    acSyncDataOn: TAction;
    acRefreshDataList: TAction;
    acWriteAdHocOnList: TAction;
    acSyncDataListOn: TAction;
    ActionList1: TActionList;
    btnRefreshControls: TButton;
    btnRefreshControlsList: TButton;
    btnSyncDataOfList: TButton;
    btnSyncDataOnList: TButton;
    btnWriteAdHocOffList: TButton;
    btnWriteAdHocOn: TButton;
    btnSyncDataOn: TButton;
    btnWriteAdHocOff: TButton;
    btnSyncDataOf: TButton;
    btnWriteAdHocOnList: TButton;
    lblCheckListBox1: TLabel;
    lblRadioGroupList1: TLabel;
    lblEditSingleValue: TLabel;
    lblListBox1: TLabel;
    lblComboBox1: TLabel;
    lblRadioGroup1: TLabel;
    lbListBoxList2: TLabel;
    lblRadioGroupList2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuExampleSettings: TMenuItem;
    mnuClose: TMenuItem;
    mnuFile: TMenuItem;
    PageControl1: TPageControl;
    pnlClientList: TPanel;
    pnlClient: TPanel;
    pnlTop: TPanel;
    pnlTopList: TPanel;
    rcbxCheckBox1: TRegCheckBox;
    redtComboBoxList1: TRegComboBox;
    redtControlName: TRegEdit;
    redtControlName1: TRegEdit;
    redtEdit: TRegEdit;
    redtComboBox1: TRegComboBox;
    rlbCheckListBox1: TRegCheckListBox;
    RegistrySource1: TRegistrySource;
    RegistrySource2: TRegistrySource;
    rlbListBox2: TRegListBox;
    rlbListBox1: TRegListBox;
    rgrpRadioGroup1: TRegRadioGroup;
    rgrpRadioGroupList1: TRegRadioGroup;
    rrbRadioButton1: TRegRadioButton;
    rrbRadioButton2: TRegRadioButton;
    tabSingleValue: TTabSheet;
    tabList: TTabSheet;
    tabKombination: TTabSheet;
    procedure acCheckExampleSettingsExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCreateExampleSettingsExecute(Sender: TObject);
    procedure acRefreshDataExecute(Sender: TObject);
    procedure acRefreshDataListExecute(Sender: TObject);
    procedure acSyncDataListOffExecute(Sender: TObject);
    procedure acSyncDataListOnExecute(Sender: TObject);
    procedure acSyncDataOffExecute(Sender: TObject);
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
  end;
end;

procedure TMain.RefreshSyncDataOnOff(aFlag: integer;
  aSet: boolean);
begin
  case aFlag of
    0: RegistrySource1.RefreshSyncProperty(aSet);
    1: RegistrySource1.RefreshSyncProperty(aSet);
  end;
end;

procedure TMain.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMain.acCheckExampleSettingsExecute(Sender: TObject);
begin
  if CheckForExampleSettings then
    MessageDlg('Der Schlüssel "Desktop" ist vorhanden und gefüllt. Beispieleinträge sind grundsätzlich vorhanden!', mtInformation, [mbOk], 0)
  else
  begin
    MessageDlg('Der Schlüssel "Desktop" ist entweder nicht vorhanden oder nicht gefüllt. Beispieleinträge sind wahrscheinlich nicht vorhanden!', mtWarning, [mbOk], 0);
    CreateSettings;
  end;
end;

procedure TMain.acCreateExampleSettingsExecute(Sender: TObject);
begin
  CreateSettings;
end;

procedure TMain.acRefreshDataExecute(Sender: TObject);
begin
  RegistrySource1.RefreshControlData(Trim(redtControlName.Text));
end;

procedure TMain.acRefreshDataListExecute(Sender: TObject);
begin
  RegistrySource2.RefreshControlData(Trim(redtControlName1.Text));
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
      ReadSection('Desktop', list);
      Result := (list.count > 0);
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
  if (MessageDlg('Sollen Beispieleinträge in der Registry erstellt werden? (Einträge werden im Root: HKEY_CURRENT_USER erstellt)', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    use_defaults := (MessageDlg('Zu den Beispieleinträgen können Defaults vergeben werden. Sollen Defaults erstellt werden? (Defaults werden im Root: HKEY_LOCAL_MACHINE erstellt, Adminrechte eventuell notwendig!!!)', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    with RegistrySource1 do
    begin
      Screen.Cursor:=crHourGlass;
      old_sync_data := DoSyncData;
      DoSyncData := False;
      WriteDefaults := use_defaults;

      WriteString('Desktop', 'Version', '1.0.0');
      WriteString('Desktop', 'Projekt', 'LazarusRegistryControls');
      WriteString('Desktop', 'Git', 'https://github.com/AlfredGerke/LazarusRegistryControls.git');
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
      Screen.Cursor:=crDefault;
    end;

    with RegistrySource2 do
    begin
      Screen.Cursor:=crHourGlass;
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
      Screen.Cursor:=crDefault;
    end;
  end;
end;

end.

