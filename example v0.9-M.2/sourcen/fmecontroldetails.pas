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
  fmeregcontrolbuttonframe,
  fmeregistrysource,
  fmeregistrysourceproperties,
  fmeregistrysourcebuttonframe;

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
    procedure CreateTRegistrySourceFrame;
    procedure CreateTRegListBoxFrame;
    procedure CreateTRegCheckBoxFrame;
    procedure CreateTRegRadioButtonFrame;
    procedure CreateTRegRadioGroupFrame;
    procedure CreateTRegEditFrame;
    procedure CreateTRegComboBoxFrame;
    procedure CreateTRegCheckListBoxFrame;
    procedure CreateTRegCheckGroupBoxFrame;
    procedure CreateTRegValueListEditorFrame;
    procedure CreateTRegLabelFrame;

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

procedure TControlDetails.CreateTRegistrySourceFrame;
begin
  with TControlRegistrySource.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;
  end;

  with TRegistrySourceProperties.Create(pnlClient) do
  begin
    PArent := pnlClient;
    Align := alClient;
  end;

  with TTRegistrySoruceButtonFrame.Create(pnlButton) do
  begin
    Parent := pnlButton;
    Align := alClient;
  end;
end;

procedure TControlDetails.CreateTRegListBoxFrame;
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
end;

procedure TControlDetails.CreateTRegCheckBoxFrame;
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
end;

procedure TControlDetails.CreateTRegRadioButtonFrame;
begin
end;

procedure TControlDetails.CreateTRegRadioGroupFrame;
begin

end;

procedure TControlDetails.CreateTRegEditFrame;
begin

end;

procedure TControlDetails.CreateTRegComboBoxFrame;
begin

end;

procedure TControlDetails.CreateTRegCheckListBoxFrame;
begin

end;

procedure TControlDetails.CreateTRegCheckGroupBoxFrame;
begin

end;

procedure TControlDetails.CreateTRegValueListEditorFrame;
begin

end;

procedure TControlDetails.CreateTRegLabelFrame;
begin

end;

procedure TControlDetails.CreateButtonFrame(aGetRootKeys: TRegControlButtonFrame.TOnGetRootKeys);
begin
  with TRegControlButtonFrame.Create(pnlButton) do
  begin
    Parent := pnlButton;
    Align := alClient;

    OnGetRootKeys := aGetRootKeys;
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
    CreateTRegListBoxFrame
  else
  if (aLabel = 'TRegistrySource') then
    CreateTRegistrySourceFrame
  else
  if (aLabel = 'TRegCheckBox') then
    CreateTRegCheckBoxFrame
  else
  if (aLabel = 'TRegRadioButton') then
    CreateTRegRadioButtonFrame
  else
  if (aLabel = 'TRegRadioGroup') then
    CreateTRegRadioGroupFrame
  else
  if (aLabel = 'TRegEdit') then
    CreateTRegEditFrame
  else
  if (aLabel = 'TRegComboBox') then
    CreateTRegComboBoxFrame
  else
  if (aLabel = 'TRegCheckListBox') then
    CreateTRegCheckListBoxFrame
  else
  if (aLabel = 'TRegCheckGroupBox') then
    CreateTRegCheckGroupBoxFrame
  else
  if (aLabel = 'TRegValueListEditor') then
    CreateTRegValueListEditorFrame
  else
  if (aLabel = 'TRegLabel') then
    CreateTRegLabelFrame;
end;


end.
