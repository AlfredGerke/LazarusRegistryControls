unit fmeControlDetails;

{$mode delphi}

interface

uses
  Forms,
  Controls,
  ExtCtrls,
  ActnList,
  fmereglistbox,
  fmereglistboxproperties,
  fmeregcheckbox,
  fmeregcheckboxproperties,
  fmeregcontrolproperties,
  fmeregcontrolbuttonframe;

type

  { TControlDetails }

  TControlDetails = class(TFrame)
    acList: TActionList;
    acShowRootKeys: TAction;
    pnlWorkspace: TPanel;
    pnlButton: TPanel;
    pnlClient: TPanel;
    pnlLeft: TPanel;
    spHSplitter: TSplitter;
    spVSplitter: TSplitter;
  private
    procedure CreateButtonFrame(aGetRootKeys: TRegControlButtonFrame.TOnGetRootKeys);
    procedure FreeControlFrame;
    procedure FreePropertiesFrame;
    procedure FreeButtonFrame;
  public
    procedure GetRegControl(aLabel: string);
  end;

implementation

{$R *.lfm}


{ TControlDetails }

procedure TControlDetails.CreateButtonFrame(aGetRootKeys: TRegControlButtonFrame.TOnGetRootKeys);
begin
  with TRegControlButtonFrame.Create(pnlButton) do
  begin
    Parent := pnlButton;
    Align := alClient;

    OnGetRootKeys := AGetRootKeys;
  end;
end;

procedure TControlDetails.FreeControlFrame;
begin
  TRegControlProperties.FreeFrame(pnlLeft);
end;

procedure TControlDetails.FreePropertiesFrame;
begin
  TRegControlProperties.FreeFrame(pnlClient);
end;

procedure TControlDetails.FreeButtonFrame;
begin
  TRegControlProperties.FreeFrame(pnlButton);
end;

procedure TControlDetails.GetRegControl(aLabel: string);
begin
  FreeControlFrame;
  FreePropertiesFrame;
  FreeButtonFrame;

  if (aLabel = 'TRegListBox') then
  begin
    // ControlFrame
    with TControlRegListBox.Create(pnlLeft) do
    begin
      Parent := pnlLeft;
      Align := alClient;

      // PropertiesFrame
      with TRegListBoxProperties.Create(pnlClient) do
      begin
        Parent := pnlClient;
        Align := alClient;

        SetRegComponent(RegControl);
      end;

      // Buttonframe
      CreateButtonFrame(GetRootKeys);
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

      // PropertiesFrame
      with TRegCheckBoxProperties.Create(pnlClient) do
      begin
        Parent := pnlClient;
        Align := alClient;

        SetRegComponent(RegControl);
      end;

      // Buttonframe
      CreateButtonFrame(GetRootKeys);
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

