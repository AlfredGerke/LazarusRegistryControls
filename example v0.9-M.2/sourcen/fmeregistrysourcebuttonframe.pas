unit fmeregistrysourcebuttonframe;

{$mode delphi}

interface

uses
  Classes,
  Forms,
  Controls,
  ActnList,
  StdCtrls,
  fmeregcontrolproperties;

type

  { TRegistrySourceButtonFrame }

  TRegistrySourceButtonFrame = class(TFrame)
    acList: TActionList;
    acCreateSettings: TAction;
    acDeleteHKCUSettings: TAction;
    acDoSyncData: TAction;
    acPrefereStrings: TAction;
    acReadDefaults: TAction;
    acWriteDefaults: TAction;
    btnCreateSettings: TButton;
    Button1: TButton;
    cbxCreateDefaults: TCheckBox;
    cbxDeleteDefaults: TCheckBox;
    cbxDoSyncData: TCheckBox;
    cbxPrefereStrings: TCheckBox;
    cbxReadDefaults: TCheckBox;
    cbxWriteDefaults: TCheckBox;
    grbRegistrySettings: TGroupBox;
    grpProperties: TGroupBox;
    procedure acCreateSettingsExecute(Sender: TObject);
    procedure acDeleteHKCUSettingsExecute(Sender: TObject);
    procedure acDoSyncDataExecute(Sender: TObject);
    procedure acPrefereStringsExecute(Sender: TObject);
    procedure acReadDefaultsExecute(Sender: TObject);
    procedure acWriteDefaultsExecute(Sender: TObject);
  private
    FOnRefreshSettings: TRegControlProperties.TOnRefreshSettings;

    procedure SetReadDefaults;
    procedure SetWriteDefaults;
    procedure SetPrefereStrings;
    procedure SetDoSyncData;
    procedure CreateRegistrySettings(aCreateDefaults: boolean); overload;
    procedure CreateRegistrySettings; overload;
    procedure DeleteRegistrySettings(aDeleteDefaults: boolean); overload;
    procedure DeleteRegistrySettings; overload;
  protected
    procedure _Initialize;
  public
    constructor Create(aOwner: TComponent); override;

    property OnRefreshSettings: TRegControlProperties.TOnRefreshSettings
      read FOnRefreshSettings
      write FOnRefreshSettings;
  end;


implementation

{$R *.lfm}

uses
  datregistry,
  Dialogs,
  SysUtils;

{ TRegistrySourceButtonFrame }

procedure TRegistrySourceButtonFrame.acCreateSettingsExecute(Sender: TObject);
begin
  CreateRegistrySettings;
end;

procedure TRegistrySourceButtonFrame.acDeleteHKCUSettingsExecute(Sender: TObject);
begin
  DeleteRegistrySettings;
end;

procedure TRegistrySourceButtonFrame.acDoSyncDataExecute(Sender: TObject);
begin
  SetDoSyncData;
end;

procedure TRegistrySourceButtonFrame.acPrefereStringsExecute(Sender: TObject);
begin
  SetPrefereStrings;
end;

procedure TRegistrySourceButtonFrame.acReadDefaultsExecute(Sender: TObject);
begin
  SetReadDefaults;
end;

procedure TRegistrySourceButtonFrame.acWriteDefaultsExecute(Sender: TObject);
begin
  SetWriteDefaults;
end;

procedure TRegistrySourceButtonFrame.SetReadDefaults;
begin
  with RegistrySourceModule.rsRegistrySource do
  begin
    if cbxReadDefaults.checked then
      ReadDefaults := True
    else
      ReadDefaults := False;
  end;

  if Assigned(FOnRefreshSettings) then
      OnRefreshSettings;
end;

procedure TRegistrySourceButtonFrame.SetWriteDefaults;
begin
  with RegistrySourceModule.rsRegistrySource do
  begin
    if cbxWriteDefaults.checked then
      WriteDefaults := True
    else
      WriteDefaults := False;
  end;

  if Assigned(FOnRefreshSettings) then
      OnRefreshSettings;
end;

procedure TRegistrySourceButtonFrame.SetPrefereStrings;
begin
  with RegistrySourceModule.rsRegistrySource do
  begin
    if cbxPrefereStrings.checked then
      PrefereStrings := True
    else
      PrefereStrings := False;
  end;

  if Assigned(FOnRefreshSettings) then
      OnRefreshSettings;
end;

procedure TRegistrySourceButtonFrame.SetDoSyncData;
begin
  with RegistrySourceModule.rsRegistrySource do
  begin
    if cbxDoSyncData.checked then
      DoSyncData := True
    else
      DoSyncData := False;
  end;

  if Assigned(FOnRefreshSettings) then
      OnRefreshSettings;
end;

procedure TRegistrySourceButtonFrame.CreateRegistrySettings(
  aCreateDefaults: boolean);
begin
  Screen.Cursor := crHourGlass;

  with RegistrySourceModule.rsRegistrySource do
  begin
    DoSyncData := False;
    WriteDefaults := aCreateDefaults;

    WriteString('About', 'Version', 'LRC 0.9 M2');
    WriteString('About', 'Project', 'LazarusRegistryControls');
    WriteString('About', 'Git',
      'https://github.com/AlfredGerke/LazarusRegistryControls.git');
    WriteString('About', 'Author', 'Dipl. Ing. (FH) Alfred Gerke');

    WriteString('TRegEdit', 'Key', 'Value');
    WriteString('TRegLabel', 'Key', 'Standardcaption');

    WriteBool('TRegCheckBox', 'Key', True);
    WriteString('TRegCheckBoxCaption', 'Caption', 'Standardcaption');
    WriteString('TRegCheckBoxCaption', 'CaptionKey1', 'Key1');
    WriteString('TRegCheckBoxCaption', 'CaptionKey2', 'Key2');
    WriteString('TRegCheckBoxCaption', 'CaptionKey3', 'Key3');
    WriteString('TRegCheckBoxCaption', 'CaptionKey4', 'Key4');
    WriteString('TRegCheckBoxCaption', 'CaptionKey5', 'Key5');
    WriteInteger('TRegCheckGroup1', 'ItemIndex', 0);

    WriteBool('TRegRadioButton', 'Key1', True);
    WriteBool('TRegRadioButton', 'Key2', False);
    WriteBool('TRegRadioButton', 'Key3', False);
    WriteBool('TRegRadioButton', 'Key4', False);
    WriteBool('TRegRadioButton', 'Key5', False);
    WriteString('TRegRadioButton1', 'Caption', 'Standardcaption');
    WriteString('TRegRadioButton2', 'Caption', 'Standardcaption2');
    WriteString('TRegRadioButton3', 'Caption', 'Standardcaption3');
    WriteString('TRegRadioButton4', 'Caption', 'Standardcaption4');

    WriteString('TRegListBoxItems', 'Key1', 'Value1');
    WriteString('TRegListBoxItems', 'Key2', 'Value2');
    WriteString('TRegListBoxItems', 'Key3', 'Value3');
    WriteString('TRegListBoxItems', 'Key4', 'Value4');
    WriteString('TRegListBoxItems', 'Key5', 'Value5');
    WriteInteger('TRegListBox', 'ItemIndex', 0);

    WriteString('TRegComboBoxItems', 'Key1', 'Value1');
    WriteString('TRegComboBoxItems', 'Key2', 'Value2');
    WriteString('TRegComboBoxItems', 'Key3', 'Value3');
    WriteString('TRegComboBoxItems', 'Key4', 'Value4');
    WriteString('TRegComboBoxItems', 'Key5', 'Value5');
    WriteInteger('TRegComboBox', 'ItemIndex', 0);

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
    WriteInteger('TRegRadioGroup', 'ItemIndex', 0);
    WriteString('TRegRadioGroup', 'Caption', 'Standardcaption');

    WriteBool('TRegCheckListBoxItems', 'Key1', True);
    WriteBool('TRegCheckListBoxItems', 'Key2', True);
    WriteBool('TRegCheckListBoxItems', 'Key3', True);
    WriteBool('TRegCheckListBoxItems', 'Key4', True);
    WriteBool('TRegCheckListBoxItems', 'Key5', True);
    WriteInteger('TRegCheckListBox', 'ItemIndex', 0);

    WriteBool('TRegCheckGroupBoxItems', 'Key1', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key2', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key3', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key4', True);
    WriteBool('TRegCheckGroupBoxItems', 'Key5', True);
    WriteInteger('TRegCheckGroupBox', 'ItemIndex', 0);
    WriteString('TRegCheckGroupBox', 'Caption', 'Standardcaption');

    DoSyncData := True;
    WriteDefaults := False;
  end;

  Screen.Cursor := crDefault;

  MessageDlg('Schlüssel eingerichtet!', mtInformation, [mbOK], 0);
end;

procedure TRegistrySourceButtonFrame.CreateRegistrySettings;
begin
  CreateRegistrySettings(cbxCreateDefaults.Checked);
end;

procedure TRegistrySourceButtonFrame.DeleteRegistrySettings(
  aDeleteDefaults: boolean);
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

  MessageDlg('Schlüssel entfernt!', mtInformation, [mbOK], 0);
end;

procedure TRegistrySourceButtonFrame.DeleteRegistrySettings;
begin
  DeleteRegistrySettings(cbxDeleteDefaults.Checked);
end;

procedure TRegistrySourceButtonFrame._Initialize;
begin
  cbxDoSyncData.Checked :=
    RegistrySourceModule.rsRegistrySource.DoSyncData;
  cbxPrefereStrings.Checked :=
    RegistrySourceModule.rsRegistrySource.PrefereStrings;
  cbxReadDefaults.Checked :=
    RegistrySourceModule.rsRegistrySource.ReadDefaults;
  cbxWriteDefaults.Checked :=
    RegistrySourceModule.rsRegistrySource.WriteDefaults;
end;

constructor TRegistrySourceButtonFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  _Initialize;
end;

end.

