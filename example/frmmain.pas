unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Menus, ActnList, ComCtrls, regsourcen, regedit,
  regcheckgroup, reglistbox, regcombobox, regvaluelisteditor, regchecklistbox,
  regradiogroup, regcheckbox, regradiobutton;

type

  { TMain }

  TMain = class(TForm)
    acClose: TAction;
    acCreateExampleSettings: TAction;
    acCheckExampleSettings: TAction;
    acRefreshData: TAction;
    acRefreshWriteAdHoc: TAction;
    acRefreshSyncData: TAction;
    ActionList1: TActionList;
    btnRefreshControls: TButton;
    btnRefreshControls1: TButton;
    lblEditSingleValue: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuExampleSettings: TMenuItem;
    mnuClose: TMenuItem;
    mnuFile: TMenuItem;
    PageControl1: TPageControl;
    pnlClient: TPanel;
    pnlTop: TPanel;
    pnlTop1: TPanel;
    rcbSetSyncData1: TRegCheckBox;
    rcbWriteAdHoc: TRegCheckBox;
    rcbSetSyncData: TRegCheckBox;
    rcbWriteAdHoc1: TRegCheckBox;
    rcbxCheckBox1: TRegCheckBox;
    rcbxCheckBox2: TRegCheckBox;
    redtControlName: TRegEdit;
    redtControlName1: TRegEdit;
    redtEdit: TRegEdit;
    RegistrySource1: TRegistrySource;
    RegistrySource2: TRegistrySource;
    rgrpRadioGroup1: TRegRadioGroup;
    rrbRadioButton1: TRegRadioButton;
    rrbRadioButton2: TRegRadioButton;
    tabSingleValue: TTabSheet;
    tabList: TTabSheet;
    tabKombination: TTabSheet;
    procedure acCheckExampleSettingsExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCreateExampleSettingsExecute(Sender: TObject);
    procedure acRefreshDataExecute(Sender: TObject);
    procedure acRefreshWriteAdHocExecute(Sender: TObject);
    procedure acRefreshSyncDataExecute(Sender: TObject);
  private
    procedure RefreshWriteAdHocProperty;
    procedure RefershSyncDataProperty;
    function CheckForExampleSettings: boolean;
    procedure CreateSettings;
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.RefreshWriteAdHocProperty;
begin
  rcbWriteAdHoc.RegistrySettings.BeginUpdate;
  if rcbWriteAdHoc.Checked then
    RegistrySource1.RefreshWriteAdHocProperty(True)
  else
    RegistrySource1.RefreshWriteAdHocProperty(False);
  rcbWriteAdHoc.RegistrySettings.EndUpdate;
end;

procedure TMain.RefershSyncDataProperty;
begin
  if rcbSetSyncData.Checked then
    RegistrySource1.RefreshSyncProperty(True)
  else
    RegistrySource1.RefreshSyncProperty(False);
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

procedure TMain.acRefreshWriteAdHocExecute(Sender: TObject);
begin
  RefreshWriteAdHocProperty;
end;

procedure TMain.acRefreshSyncDataExecute(Sender: TObject);
begin
  RefershSyncDataProperty;
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
begin
  if (MessageDlg('Sollen Beispieleinträge in der Registry erstellt werden? (Einträge werden im Root: HKEY_CURRENT_USER erstellt)', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    use_defaults := (MessageDlg('Zu den Beispieleinträgen können Defaults vergeben werden. Sollen Defaults erstellt werden? (Defaults werden im Root: HKEY_LOCAL_MACHINE erstellt, Adminrechte eventuell notwendig!!!)', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    with RegistrySource1 do
    begin
      Screen.Cursor:=crHourGlass;
      DoSyncData := False;
      WriteDefaults := use_defaults;

      WriteString('Desktop', 'Version', '1.0.0');
      WriteString('Desktop', 'Projekt', 'LazarusRegistryControls');
      WriteString('Desktop', 'Git', 'https://github.com/AlfredGerke/LazarusRegistryControls.git');

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
      WriteInteger('ListBox1', 'TopIndex', 1);

      DoSyncData := False;
      RefreshControlData('', 0);
      Screen.Cursor:=crDefault;
    end;

    with RegistrySource2 do
    begin
      Screen.Cursor:=crHourGlass;
      DoSyncData := False;
      WriteDefaults := use_defaults;

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

      // Section als Liste laden (Key=Value)
      WriteString('RegValueListe', 'Key1', 'Value1');
      WriteString('RegValueListe', 'Key2', 'Value2');
      WriteString('RegValueListe', 'Key3', 'Value3');
      WriteString('RegValueListe', 'Key4', 'Value4');
      WriteString('RegValueListe', 'Key5', 'Value5');

      DoSyncData := False;
      RefreshControlData('', 0);
      Screen.Cursor:=crDefault;
    end;
  end;
end;

end.

