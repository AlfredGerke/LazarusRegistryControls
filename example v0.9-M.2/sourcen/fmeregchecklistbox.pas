unit fmeregchecklistbox;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regchecklistbox,
  regcheckbox,
  Forms,
  ExtCtrls,
  regtype;

type

  { TControlRegCheckListBox }

  TControlRegCheckListBox = class(TCustomRegControlFrame<TRegCheckListBox>)
    Bevel1: TBevel;
    RegCheckBox1: TRegCheckBox;
    RegCheckBox2: TRegCheckBox;
    RegCheckBox3: TRegCheckBox;
    RegCheckBox4: TRegCheckBox;
    RegCheckBox5: TRegCheckBox;
    RegCheckListBox1: TRegCheckListBox;
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

procedure TControlRegCheckListBox._Initialize;
begin
  inherited;

  SetRegControl(RegCheckListBox1);
end;

procedure TControlRegCheckListBox.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegCheckListBox.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

procedure TControlRegCheckListBox.SetItemsByRegistry(aValue: boolean);
begin
  RegControl.RegistrySettings.ItemsByRegistry := aValue;
end;

function TControlRegCheckListBox.GetItemsByRegistry: boolean;
begin
  Result := RegControl.RegistrySettings.ItemsByRegistry;
end;

function TControlRegCheckListBox.ClearItems: boolean;
begin
  if GetItemsByRegistry then
    Result := RegControl.ClearItems(True, 'Einträge löschen?')
  else
    RegControl.Items.Clear;
end;

procedure TControlRegCheckListBox.SetItems;
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

end.
