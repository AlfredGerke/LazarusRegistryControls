unit fmeregistrysourcebuttonframe;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls, ActnList, StdCtrls;

type

  { TTRegistrySoruceButtonFrame }

  TTRegistrySoruceButtonFrame = class(TFrame)
    acList: TActionList;
    acCreateSettings: TAction;
    acDeleteHKCUSettings: TAction;
    btnCreateSettings: TButton;
    Button1: TButton;
    cbxDeleteDefaults: TCheckBox;
    cbxCreateDefaults: TCheckBox;
    procedure acCreateSettingsExecute(Sender: TObject);
    procedure acDeleteHKCUSettingsExecute(Sender: TObject);
  private
    procedure CreateRegistrySettings(aCreateDefaults: boolean); overload;
    procedure CreateRegistrySettings; overload;
    procedure DeleteRegistrySettings(aDeleteDefaults: boolean); overload;
    procedure DeleteRegistrySettings; overload;
  end;

implementation

{$R *.lfm}

uses
  datregistry;

{ TTRegistrySoruceButtonFrame }

procedure TTRegistrySoruceButtonFrame.acCreateSettingsExecute(Sender: TObject);
begin
  CreateRegistrySettings;
end;

procedure TTRegistrySoruceButtonFrame.acDeleteHKCUSettingsExecute(Sender: TObject);
begin
  DeleteRegistrySettings;
end;

procedure TTRegistrySoruceButtonFrame.CreateRegistrySettings(aCreateDefaults: boolean);
begin
  Screen.Cursor := crHourGlass;

  with RegistrySourceModule.rsRegistrySource do
  begin
    DoSyncData := False;
    WriteDefaults := aCreateDefaults;

    WriteString('About', 'Version', '1.0.0');
    WriteString('About', 'Projekt', 'LazarusRegistryControls');
    WriteString('About', 'Git',
      'https://github.com/AlfredGerke/LazarusRegistryControls.git');

    WriteString('TRegEdit', 'Key', 'Value');
    WriteString('TRegLabel', 'Key', 'Standardcaption');

    WriteBool('TRegCheckBox', 'Key', True);
    WriteString('TRegCheckBox', 'Caption', 'Standardcaption');

    WriteBool('TRegRadioButton', 'Key', True);
    WriteString('TRegRadioButton', 'Caption', 'Standardcaption');

    WriteString('TRegListBoxItems', 'Key1', 'Value1');
    WriteString('TRegListBoxItems', 'Key2', 'Value2');
    WriteString('TRegListBoxItems', 'Key3', 'Value3');
    WriteString('TRegListBoxItems', 'Key4', 'Value4');
    WriteString('TRegListBoxItems', 'Key5', 'Value5');
    WriteInteger('TRegListBox', 'Index', 0);

    WriteString('TRegComboBoxItems', 'Key1', 'Value1');
    WriteString('TRegComboBoxItems', 'Key2', 'Value2');
    WriteString('TRegComboBoxItems', 'Key3', 'Value3');
    WriteString('TRegComboBoxItems', 'Key4', 'Value4');
    WriteString('TRegComboBoxItems', 'Key5', 'Value5');
    WriteInteger('TRegComboBox', 'Index', 0);

    WriteString('TRegValueListEditorItems', 'Key1', 'Value1');
    WriteString('TRegValueListEditorItems', 'Key2', 'Value2');
    WriteString('TRegValueListEditorItems', 'Key3', 'Value3');
    WriteString('TRegValueListEditorItems', 'Key4', 'Value4');
    WriteString('TRegValueListEditorItems', 'Key5', 'Value5');

    WriteString('TRegRadioGroupItems', 'Key1', 'Value1');
    WriteString('TRegRadioGroupItems', 'Key2', 'Value2');
    WriteString('TRegRadioGroupItems', 'Key3', 'Value3');
    WriteString('TRegRadioGroupItems', 'Key4', 'Value4');
    WriteString('TRegRadioGroupItems', 'Key5', 'Value5');
    WriteInteger('TRegRadioGroup', 'index', 0);
    WriteString('TRegRadioGroup', 'Caption', 'Standardcaption');

    WriteBool('TRegCheckListBox', 'Key1', True);
    WriteBool('TRegCheckListBox', 'Key2', True);
    WriteBool('TRegCheckListBox', 'Key3', True);
    WriteBool('TRegCheckListBox', 'Key4', True);
    WriteBool('TRegCheckListBox', 'Key5', True);

    WriteBool('TRegCheckGroupBoxItems', 'Key1', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key1', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key1', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key1', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key1', True);
    WriteInteger('TRegCheckGroupBox', 'Index', 0);
    WriteString('TRegCheckGroupBox', 'Caption', 'Standardcaption');

    DoSyncData := True;
    WriteDefaults := False;
  end;

  Screen.Cursor := crDefault;
end;

procedure TTRegistrySoruceButtonFrame.CreateRegistrySettings;
begin
  CreateRegistrySettings(cbxCreateDefaults.Checked);
end;

procedure TTRegistrySoruceButtonFrame.DeleteRegistrySettings(aDeleteDefaults: boolean);
begin
  Screen.Cursor := crHourGlass;

  with RegistrySourceModule.rsRegistrySource do
  begin
    DoSyncData := False;
    WriteDefaults := aDeleteDefaults;

    DeleteRootKey;

    DoSyncData := False;
    WriteDefaults := True;
  end;

  Screen.Cursor := crDefault;
end;

procedure TTRegistrySoruceButtonFrame.DeleteRegistrySettings;
begin
  DeleteRegistrySettings(cbxDeleteDefaults.Checked);
end;

end.

