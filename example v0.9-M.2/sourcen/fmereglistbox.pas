unit fmereglistbox;

{$mode Delphi}{$H+}

interface

uses
  fmecustomcontrolframe,
  reglistbox,
  regradiogroup,
  ExtCtrls,
  ActnList,
  Menus,
  ComCtrls,
  Controls,
  regtype;

type

  { TControlRegListBox }

  TControlRegListBox = class(TCustomRegControlFrame<TRegListBox>)
    acDeleteItem: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    RegListBox1: TRegListBox;
    RegRadioGroup1: TRegRadioGroup;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
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
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

procedure TControlRegListBox.acDeleteItemExecute(Sender: TObject);
begin
  RegListBox1.DeleteItem;
end;

procedure TControlRegListBox._Initialize;
begin
  inherited;

  SetRegControl(RegListBox1);
end;

procedure TControlRegListBox.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegListBox.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

procedure TControlRegListBox.SetItemsByRegistry(aValue: boolean);
begin
  RegControl.RegistrySettings.ItemsByRegistry := aValue;
end;

function TControlRegListBox.GetItemsByRegistry: boolean;
begin
  Result := RegControl.RegistrySettings.ItemsByRegistry;
end;

procedure TControlRegListBox.SetListSourceKind(aListSourceKind: TListSourceKind);
begin
  RegControl.RegistrySettings.SourceKind := aListSourceKind;
end;

function TControlRegListBox.GetListSourceKind: TListSourceKind;
begin
  Result := RegControl.RegistrySettings.SourceKind;
end;

function TControlRegListBox.ClearItems: boolean;
begin
  if GetItemsByRegistry then
    Result := RegControl.ClearItems(True, 'Einträge löschen?')
  else
    RegControl.Items.Clear;
end;

procedure TControlRegListBox.SetItems;
begin
  if GetItemsByRegistry then
    MessageDlg('Listendaten werden aus der Registry gelesen, ItemsByRegistry ' +
      'auf False setzen!', mtWarning, [mbOK], 0)
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

