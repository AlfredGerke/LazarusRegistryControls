unit fmeregradiogroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regradiogroup,
  regcombobox,
  ExtCtrls,
  Forms,
  Controls,
  ActnList,
  regtype;

type

  { TControlRegRadioGroup }

  TControlRegRadioGroup = class(TCustomRegControlFrame<TRegRadioGroup>)
    acDeleteItem: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    ImageList1: TImageList;
    RegComboBox1: TRegComboBox;
    RegRadioGroup1: TRegRadioGroup;
    ScrollBox1: TScrollBox;
    procedure acDeleteItemExecute(Sender: TObject);
  protected
     procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;

    { TODO -oAlfred Gerke -cListen-RegControls : Spezielle Testmethoden für Listen-RegControls implementieren }
    procedure SetItemsByRegistry(aValue: boolean);
    function GetItemsByRegistry: boolean;
    procedure SetListSourceKind(aListSourceKind: TListSourceKind);
    function GetListSourceKind: TListSourceKind;
    function ClearItems: boolean;
    procedure SetItems;
    function DeleteItem: boolean;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

procedure TControlRegRadioGroup.acDeleteItemExecute(Sender: TObject);
begin
  DeleteItem;
end;

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
  if GetItemsByRegistry then
    Result := RegControl.ClearItems(True, 'Einträge löschen?')
  else
    RegControl.Items.Clear;
end;

procedure TControlRegRadioGroup.SetItems;
begin
  if GetItemsByRegistry then
    MessageDlg('Listendaten werden aus der Registry gelesen, ItemsByRegistry '
      + 'auf False setzen!', mtWarning, [mbOK], 0)
  else
  begin
    RegControl.Items.Add('Default1');
    RegControl.Items.Add('Default2');
    RegControl.Items.Add('Default3');
    RegControl.Items.Add('Default4');
    RegControl.Items.Add('Default5');
  end;
end;

function TControlRegRadioGroup.DeleteItem: boolean;
begin
  Result := RegControl.DeleteItem(-1, True, 'Eintrag löschen?');
end;

procedure TControlRegRadioGroup.SetItemsByRegistry(aValue: boolean);
begin
  RegControl.RegistrySettings.ItemsByRegistry := aValue;
end;

function TControlRegRadioGroup.GetItemsByRegistry: boolean;
begin
  Result := RegControl.RegistrySettings.ItemsByRegistry;
end;

procedure TControlRegRadioGroup.SetListSourceKind(aListSourceKind: TListSourceKind);
begin
  RegControl.RegistrySettings.SourceKind := aListSourceKind;
end;

function TControlRegRadioGroup.GetListSourceKind: TListSourceKind;
begin
  Result := RegControl.RegistrySettings.SourceKind;
end;

end.

