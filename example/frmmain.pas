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
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuExampleSettings: TMenuItem;
    mnuClose: TMenuItem;
    mnuFile: TMenuItem;
    PageControl1: TPageControl;
    RegCheckBox1: TRegCheckBox;
    RegCheckBox2: TRegCheckBox;
    RegEdit1: TRegEdit;
    RegistrySource1: TRegistrySource;
    RegRadioButton1: TRegRadioButton;
    RegRadioButton2: TRegRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure acCheckExampleSettingsExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCreateExampleSettingsExecute(Sender: TObject);
  private
    function CheckForExampleSettings: boolean;
    procedure CreateSettings;
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

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

      WriteDefaults := use_defaults;
      WriteString('Desktop', 'Version', '1.0.0');
      WriteString('Desktop', 'Projekt', 'LazarusRegistryControls');
      WriteString('Desktop', 'Git', 'https://github.com/AlfredGerke/LazarusRegistryControls.git');

      // Einzelwerte an Controls übergeben
      WriteString('Einzelwerte', 'Edit', 'Test für Edit');
      WriteBool('Einzelwerte', 'CheckBox1', True);
      WriteBool('Einzelwerte', 'CheckBox2', False);
      WriteBool('Einzelwerte', 'RadioButton1', True);
      WriteBool('Einzelwerte', 'RadioButton2', False);

      // Index für Listen sichern
      WriteInteger('RadioGroup', 'ItemIndex', 1);
      WriteInteger('Combobox', 'ItemIndex', 1);
      WriteInteger('ListBox', 'TopIndex', 1);

      // Section als Liste laden
      // nur wenn als Value '1', 1 oder True dann Ident in die Liste laden
      WriteInteger('RaidoGroupListe', 'RaidoGroup1', 1);
      WriteInteger('RaidoGroupListe', 'RaidoGroup2', 1);
      WriteInteger('RaidoGroupListe', 'RaidoGroup3', 1);
      WriteInteger('RaidoGroupListe', 'RaidoGroup4', 1);
      WriteInteger('RaidoGroupListe', 'RaidoGroup5', 1);

      // Section als Liste laden
      // nur wenn als Value '1', 1 oder True dann Ident in die Liste laden
      WriteInteger('ListBoxListe', 'ListBox1', 1);
      WriteInteger('ListBoxListe', 'ListBox2', 1);
      WriteInteger('ListBoxListe', 'ListBox3', 1);
      WriteInteger('ListBoxListe', 'ListBox4', 1);
      WriteInteger('ListBoxListe', 'ListBox5', 1);

      // Section als Liste laden
      // nur wenn als Value '1', 1 oder True dann Ident in die Liste laden
      WriteString('ComboBoxListe', 'ComboBox1', '1');
      WriteInteger('ComboBoxListe', 'ComboBox2', 0);
      WriteInteger('ComboBoxListe', 'ComboBox3', 1);
      WriteBool('ComboBoxListe', 'ComboBox4', True);
      WriteBool('ComboBoxListe', 'ComboBox5', False);

      // Section als Liste laden
      // nur wenn als Value '1', 1 oder True dann Ident in die Liste laden
      WriteString('CheckGroupListe', 'CheckGroup1', '1');
      WriteInteger('CheckGroupListe', 'CheckGroup2', 1);
      WriteInteger('CheckGroupListe', 'CheckGroup3', 0);
      WriteBool('CheckGroupListe', 'CheckGroup4', True);
      WriteBool('CheckGroupListe', 'CheckGroup5', False);

      // Section als Liste laden
      // nur wenn als Value '1', 1 oder True dann Ident in die Liste laden
      WriteString('CheckListBoxListe', 'CheckListBox1', '1');
      WriteInteger('CheckListBoxListe', 'CheckListBox2', 0);
      WriteInteger('CheckListBoxListe', 'CheckListBox3', 1);
      WriteBool('CheckListBoxListe', 'CheckListBox4', True);
      WriteBool('CheckListBoxListe', 'CheckListBox5', False);

      // Section als Liste laden (Key=Value)
      WriteString('RegValueListe', 'Key1', 'Value1');
      WriteString('RegValueListe', 'Key2', 'Value2');
      WriteString('RegValueListe', 'Key3', 'Value3');
      WriteString('RegValueListe', 'Key4', 'Value4');
      WriteString('RegValueListe', 'Key5', 'Value5');

      Screen.Cursor:=crDefault;
    end;
  end;
end;

end.

