unit fmeregradiogroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regradiogroup,
  regcheckbox,
  regchecklistbox,
  regcombobox,
  ExtCtrls,
  Forms;

type

  { TControlRegRadioGroup }

  TControlRegRadioGroup = class(TCustomRegControlFrame<TRegRadioGroup>)
    Bevel1: TBevel;
    RegComboBox1: TRegComboBox;
    RegRadioGroup1: TRegRadioGroup;
    ScrollBox1: TScrollBox;
  protected
     procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;

    { TODO -oAlfred Gerke -cListen-RegControls : Spezielle Testmethoden für Listen-RegControls implementieren }
    procedure SetItemsByRegistry(aValue: boolean);
    function GetItemsByRegistry: boolean;
    function ClearItems: boolean;
    procedure SetItems;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

procedure TControlRegRadioGroup._Initialize;
begin
  inherited;

  SetRegControl(RegRadioGroup1);

  RegRadioGroup1.CaptionSettings.OnBeforeCaptionSettingChange :=
    BeforeRegistrySettingChangeProc;
end;

procedure TControlRegRadioGroup.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegRadioGroup.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

function TControlRegRadioGroup.ClearItems: boolean;
begin
  Result := RegControl.ClearItems(True, 'Einträge löschen?');
end;

procedure TControlRegRadioGroup.SetItems;
begin
  if GetItemsByRegistry then
    MessageDlg('Listendaten werden aus der Registry gelesen, ItemsByRegistry auf False setzen!', mtWarning, [mbOK], 0)
  else
  begin
    RegComboBox1.Items.Add('Default1');
    RegComboBox1.Items.Add('Default2');
    RegComboBox1.Items.Add('Default3');
    RegComboBox1.Items.Add('Default4');
    RegComboBox1.Items.Add('Default5');
  end;
end;

procedure TControlRegRadioGroup.SetItemsByRegistry(aValue: boolean);
begin
  RegControl.RegistrySettings.ItemsByRegistry := aValue;
end;

function TControlRegRadioGroup.GetItemsByRegistry: boolean;
begin
  Result := RegControl.RegistrySettings.ItemsByRegistry;
end;

end.

