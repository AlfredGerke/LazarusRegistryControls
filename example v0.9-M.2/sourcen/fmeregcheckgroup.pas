unit fmeregcheckgroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regcheckgroup,
  regcheckbox,
  Forms,
  ExtCtrls, Controls, ActnList, ComCtrls,
  regtype, Classes;

type

  { TControlRegCheckGroup }

  TControlRegCheckGroup = class(TCustomRegControlFrame<TRegCheckGroup>)
    acDeleteItem: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    ImageList1: TImageList;
    RegCheckBox1: TRegCheckBox;
    RegCheckBox2: TRegCheckBox;
    RegCheckBox3: TRegCheckBox;
    RegCheckBox4: TRegCheckBox;
    RegCheckBox5: TRegCheckBox;
    RegCheckGroup1: TRegCheckGroup;
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
    function ClearItems: boolean;
    procedure SetItems;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

procedure TControlRegCheckGroup.acDeleteItemExecute(Sender: TObject);
begin
  RegCheckGroup1.DeleteItem;
end;

procedure TControlRegCheckGroup._Initialize;
begin
  inherited;

  SetRegControl(RegCheckGroup1);

  RegCheckGroup1.CaptionSettings.OnBeforeCaptionSettingChange :=
    BeforeRegistrySettingChangeProc;
end;

procedure TControlRegCheckGroup.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegCheckGroup.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

procedure TControlRegCheckGroup.SetItemsByRegistry(aValue: boolean);
begin
  RegControl.RegistrySettings.ItemsByRegistry := aValue;
end;

function TControlRegCheckGroup.GetItemsByRegistry: boolean;
begin
  Result := RegControl.RegistrySettings.ItemsByRegistry;
end;

function TControlRegCheckGroup.ClearItems: boolean;
begin
  if GetItemsByRegistry then
    Result := RegControl.ClearItems(True, 'Einträge löschen?')
  else
    RegControl.Items.Clear;
end;

procedure TControlRegCheckGroup.SetItems;
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

