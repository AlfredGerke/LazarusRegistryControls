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
  fmeregistrysourcebuttonframe,
  fmeregedit,
  fmeregeditproperties,
  fmereglabel,
  fmereglabelproperties,
  fmeregradiobutton,
  fmeregradiobuttonproperties,
  fmeregcombobox,
  fmeregcomboboxproperties,
  fmeregradiogroup,
  fmeregradiogroupproperties,
  fmeregchecklistbox,
  fmeregchecklistboxproperties,
  fmeregcheckgroup,
  fmeregcheckgroupproperties,
  fmeregvaluelisteditor,
  fmeregvaluelisteditorproperties;

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

    procedure CreateButtonFrame(aRefreshSettings:
      TRegControlProperties.TOnRefreshSettings;
      aGetRootKeys: TRegControlButtonFrame.TOnGetRootKeys;
      aSetCanRead:
      TRegControlButtonFrame.TOnSetBooleanProperty;
      aSetCanWrite:
      TRegControlButtonFrame.TOnSetBooleanProperty;
      aSetDoWriteAdHoc:
      TRegControlButtonFrame.TOnSetBooleanProperty;
      aSetDoSyncData:
      TRegControlButtonFrame.TOnSetBooleanProperty;
      aSetDoMergeData:
      TRegControlButtonFrame.TOnSetBooleanProperty;
      aSetGroupIndex:
      TRegControlButtonFrame.TOnSetIntegerProperty;
      aGetCanRead:
      TRegControlButtonFrame.TOnGetBooleanProperty;
      aGetCanWrite:
      TRegControlButtonFrame.TOnGetBooleanProperty;
      aGetDoWriteAdHoc:
      TRegControlButtonFrame.TOnGetBooleanProperty;
      aGetDoSyncData:
      TRegControlButtonFrame.TOnGetBooleanProperty;
      aGetDoMergeData:
      TRegControlButtonFrame.TOnGetBooleanProperty;
      aGetGroupIndex:
      TRegControlButtonFrame.TOnGetIntegerProperty);
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


    with TRegistrySourceButtonFrame.Create(pnlButton) do
    begin
      Parent := pnlButton;
      Align := alClient;

      OnRefreshSettings := RefreshSettings;
    end;
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

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, SetDoMergeData, SetGroupIndex,
        GetCanRead, GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, GetDoMergeData,
        GetGroupIndex);
    end;
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

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, nil, SetGroupIndex, GetCanRead,
        GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, nil, GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegRadioButtonFrame;
begin
  // ControlFrame
  with TControlRegRadioButton.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegRadioButtonProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, nil, SetGroupIndex, GetCanRead,
        GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, nil, GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegRadioGroupFrame;
begin
  // ControlFrame
  with TControlRegRadioGroup.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegRadioGroupProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, SetDoMergeData, SetGroupIndex,
        GetCanRead, GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, GetDoMergeData,
        GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegEditFrame;
begin
  // ControlFrame
  with TControlRegEdit.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegEditProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, nil, SetGroupIndex, GetCanRead,
        GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, nil, getGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegComboBoxFrame;
begin
  // ControlFrame
  with TControlRegComobBox.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegComboBoxProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, SetDoMergeData, SetGroupIndex,
        GetCanRead, GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, GetDoMergeData,
        GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegCheckListBoxFrame;
begin
  // ControlFrame
  with TControlRegCheckListBox.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegCheckListBoxProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, SetDoMergeData, SetGroupIndex,
        GetCanRead, GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, GetDoMergeData,
        GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegCheckGroupBoxFrame;
begin
  // ControlFrame
  with TControlRegCheckGroup.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegCheckGroupProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, SetDoMergeData, SetGroupIndex,
        GetCanRead, GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, GetDoMergeData,
        GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegValueListEditorFrame;
begin
  // ControlFrame
  with TControlRegValueListEditor.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegValueListEditorProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, SetDoMergeData, SetGroupIndex,
        GetCanRead, GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, GetDoMergeData,
        GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateTRegLabelFrame;
begin
  // ControlFrame
  with TControlRegLabel.Create(pnlLeft) do
  begin
    Parent := pnlLeft;
    Align := alClient;

    // PropertiesFrame
    with TRegLabelProperties.Create(pnlClient) do
    begin
      Parent := pnlClient;
      Align := alClient;

      SetRegComponent(RegControl);

      // Buttonframe
      CreateButtonFrame(OnRefreshSettings, GetRootKeys, SetCanRead, SetCanWrite,
        SetDoWriteAdHoc, SetDoSyncData, nil, SetGroupIndex, GetCanRead,
        GetCanWrite, GetDoWriteAdHoc, GetDoSyncData, nil, GetGroupIndex);
    end;
  end;
end;

procedure TControlDetails.CreateButtonFrame(
  aRefreshSettings: TRegControlProperties.TOnRefreshSettings;
  aGetRootKeys: TRegControlButtonFrame.TOnGetRootKeys;
  aSetCanRead: TRegControlButtonFrame.TOnSetBooleanProperty;
  aSetCanWrite: TRegControlButtonFrame.TOnSetBooleanProperty;
  aSetDoWriteAdHoc: TRegControlButtonFrame.TOnSetBooleanProperty;
  aSetDoSyncData: TRegControlButtonFrame.TOnSetBooleanProperty;
  aSetDoMergeData: TRegControlButtonFrame.TOnSetBooleanProperty;
  aSetGroupIndex: TRegControlButtonFrame.TOnSetIntegerProperty;
  aGetCanRead: TRegControlButtonFrame.TOnGetBooleanProperty;
  aGetCanWrite: TRegControlButtonFrame.TOnGetBooleanProperty;
  aGetDoWriteAdHoc: TRegControlButtonFrame.TOnGetBooleanProperty;
  aGetDoSyncData: TRegControlButtonFrame.TOnGetBooleanProperty;
  aGetDoMergeData: TRegControlButtonFrame.TOnGetBooleanProperty;
  aGetGroupIndex: TRegControlButtonFrame.TOnGetIntegerProperty);
begin
  with TRegControlButtonFrame.Create(pnlButton) do
  begin
    Parent := pnlButton;
    Align := alClient;

    OnGetRootKeys := aGetRootKeys;

    OnSetCanRead := aSetCanRead;
    OnSetCanWrite := aSetCanWrite;
    OnSetDoSyncData := aSetDoSyncData;
    OnSetDoWriteAdHoc := aSetDoWriteAdHoc;
    OnSetDoMergeData := aSetDoMergeData;
    OnSetGroupIndex := aSetGroupIndex;

    OnGetCanRead := aGetCanRead;
    OnGetCanWrite := aGetCanWrite;
    OnGetDoSyncData := aGetDoSyncData;
    OnGetDoWriteAdHoc := aGetDoWriteAdHoc;
    OnGetDoMergeData := aGetDoMergeData;
    OnGetGroupIndex := aGetGroupIndex;

    OnRefreshSettings := aRefreshSettings;

    Refresh;
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
