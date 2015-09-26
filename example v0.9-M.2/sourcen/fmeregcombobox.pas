unit fmeregcombobox;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls, ActnList, ComCtrls,
  fmecustomcontrolframe,
  regcombobox,
  reglistbox,
  regtype;

type

  { TControlRegComobBox }

  TControlRegComobBox = class(TCustomRegControlFrame<TRegComboBox>)
    acDeleteItem: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    ImageList1: TImageList;
    RegComboBox1: TRegComboBox;
    RegListBox1: TRegListBox;
    ScrollBox1: TScrollBox;
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

procedure TControlRegComobBox.acDeleteItemExecute(Sender: TObject);
begin
  RegComboBox1.DeleteItem;
end;

procedure TControlRegComobBox._Initialize;
begin
  inherited;

  SetRegControl(RegComboBox1);
end;

procedure TControlRegComobBox.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegComobBox.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

procedure TControlRegComobBox.SetItemsByRegistry(aValue: boolean);
begin
  RegControl.RegistrySettings.ItemsByRegistry := aValue;
end;

function TControlRegComobBox.GetItemsByRegistry: boolean;
begin
  Result := RegControl.RegistrySettings.ItemsByRegistry;
end;

procedure TControlRegComobBox.SetListSourceKind(aListSourceKind: TListSourceKind);
begin
  RegControl.RegistrySettings.SourceKind := aListSourceKind;
end;

function TControlRegComobBox.GetListSourceKind: TListSourceKind;
begin
  Result := RegControl.RegistrySettings.SourceKind;
end;

function TControlRegComobBox.ClearItems: boolean;
begin
  if GetItemsByRegistry then
    Result := RegControl.ClearItems(True, 'Einträge löschen?')
  else
    RegControl.Items.Clear;
end;

procedure TControlRegComobBox.SetItems;
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

