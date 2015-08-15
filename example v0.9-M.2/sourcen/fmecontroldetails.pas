unit fmeControlDetails;

{$mode delphi}

interface

uses
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  ActnList,
  Buttons,
  fmereglistbox,
  fmereglistboxproperties,
  reglistbox;

type

  { TControlDetails }

  TControlDetails = class(TFrame)
    acList: TActionList;
    acShowRootKeys: TAction;
    cbxEditRootKeys: TCheckBox;
    pnlClient: TPanel;
    pnlLeft: TPanel;
    pnlButtom: TPanel;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    procedure acShowRootKeysExecute(Sender: TObject);
  strict private type
    TOnGetRootKeys = procedure(aEdit: boolean) of object;
    TOnSetWriteAdHoc = procedure(aWriteAdHoc: boolean) of object;
  private
    FRegControlFrame: TFrame;
    FRegControlProperties: TFrame;
    //
    FGetRootKeysProc: TOnGetRootKeys;
    FSetWriteAdHoc: TOnSetWriteAdHoc;
  public
    procedure GetRegControl(aLabel: string);
  end;

implementation

{$R *.lfm}


{ TControlDetails }

procedure TControlDetails.acShowRootKeysExecute(Sender: TObject);
begin
  if Assigned(FGetRootKeysProc) then
    FGetRootKeysProc(cbxEditRootKeys.Checked);
end;

procedure TControlDetails.GetRegControl(aLabel: string);
begin
  if Assigned(FRegControlFrame) then
    FreeAndNil(FRegControlFrame);

  if Assigned(FRegControlProperties) then
    FreeAndNil(FRegControlProperties);

  FGetRootKeysProc := nil;
  FSetWriteAdHoc := nil;


  if (aLabel = 'TRegListBox') then
  begin
    // ControlFrame
    FRegControlFrame := TControlRegListBox.Create(pnlLeft);
    FRegControlFrame.Parent := pnlLeft;
    FRegControlFrame.Align := alClient;

    FGetRootKeysProc := TControlRegListBox(FRegControlFrame).GetRootKeys;

    // PropertiesFrame
    FRegControlProperties := TRegListBoxProperties.Create(pnlClient);
    FRegControlProperties.Parent := pnlClient;
    FRegControlProperties.Align := alClient;

    TRegListBoxProperties(FRegControlProperties).SetRegControl(
      TRegListBox(TControlRegListBox(FRegControlFrame).RegControl));
  end
  else
  if (aLabel = 'TRegistrySource') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Wichtig: TRegistrySource ist keine visuelle Komponente }
  end
  else
  if (aLabel = 'TRegCheckBox') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegRadioButton') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegRadioGroup') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegEdit') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegListBox') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegComboBox') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegCheckListBox') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegCheckGroupBox') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegValueListEditor') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end
  else
  if (aLabel = 'TRegLabel') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Ähnlich verfahren wie mit TRegListBox }
  end;
end;


end.

