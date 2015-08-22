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
  reglistbox,
  fmeregcheckbox,
  fmeregcheckboxproperties,
  regcheckbox;

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
    FGetRootKeysProc: TOnGetRootKeys;
    FSetWriteAdHoc: TOnSetWriteAdHoc;

    procedure FreeFrame(aPanel: TPanel);
    procedure FreeControlFrame;
    procedure FreePropertiesFrame;
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

procedure TControlDetails.FreeFrame(aPanel: TPanel);
var
  anz: integer;
  frame: TFrame;
begin
  for anz := 0 to aPanel.ComponentCount-1 do
  begin
    if (aPanel.Components[anz] is TFrame) then
    begin
      frame := TFrame(aPanel.Components[anz]);
      FreeAndNil(frame);
      Exit;
    end;
  end;
end;

procedure TControlDetails.FreeControlFrame;
begin
  FreeFrame(pnlLeft);
end;

procedure TControlDetails.FreePropertiesFrame;
begin
  FreeFrame(pnlClient);
end;

procedure TControlDetails.GetRegControl(aLabel: string);
begin
  FreeControlFrame;
  FreePropertiesFrame;

  FGetRootKeysProc := nil;
  FSetWriteAdHoc := nil;

  if (aLabel = 'TRegListBox') then
  begin
    // ControlFrame
    with TControlRegListBox.Create(pnlLeft) do
    begin
      Parent := pnlLeft;
      Align := alClient;

      FGetRootKeysProc := GetRootKeys;

      // PropertiesFrame
      with TRegListBoxProperties.Create(pnlClient) do
      begin
        Parent := pnlClient;
        Align := alClient;

        SetRegComponent(RegControl);
      end;
    end;
  end
  else
  if (aLabel = 'TRegistrySource') then
  begin
    { TODO -oAlfred Gerke -cControlDetailsFrame einrichten : Wichtig: TRegistrySource ist keine visuelle Komponente }
  end
  else
  if (aLabel = 'TRegCheckBox') then
  begin
    // ControlFrame
    with TControlRegCheckBox.Create(pnlLeft) do
    begin
      Parent := pnlLeft;
      Align := alClient;

      FGetRootKeysProc := GetRootKeys;

      // PropertiesFrame
      with TRegCheckBoxProperties.Create(pnlClient) do
      begin
        Parent := pnlClient;
        Align := alClient;

        SetRegComponent(RegControl);
      end;
    end;
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

