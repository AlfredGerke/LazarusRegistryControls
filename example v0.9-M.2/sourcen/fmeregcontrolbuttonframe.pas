unit fmeregcontrolbuttonframe;

{$mode delphi}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls,
  ActnList, Spin, ComCtrls, Controls,
  fmeregcontrolproperties, Classes;

type

  { TRegControlButtonFrame }

  TRegControlButtonFrame = class(TFrame)
    acList: TActionList;
    acGetRootKeys: TAction;
    acCanRead: TAction;
    acCanWrite: TAction;
    acDoWriteAdHoc: TAction;
    acDoSyncData: TAction;
    acDoMergeData: TAction;
    acSetGroupIndex: TAction;
    acReadFromReg: TAction;
    cbxDoMergeData: TCheckBox;
    cbxEditRootKeys: TCheckBox;
    cbxCanRead: TCheckBox;
    cbxCanWrite: TCheckBox;
    cbxDoWriteAdHoc: TCheckBox;
    cbxDoSyncData: TCheckBox;
    grbDialogs: TGroupBox;
    grpProperties: TGroupBox;
    ilImages: TImageList;
    lblGroupIndex: TLabel;
    pnlWorkspace: TPanel;
    btnGetRootKeys: TSpeedButton;
    edtGroupIndex: TSpinEdit;
    barButtonFrameToolbar: TToolBar;
    ToolButton1: TToolButton;
    procedure acCanReadExecute(Sender: TObject);
    procedure acCanWriteExecute(Sender: TObject);
    procedure acDoMergeDataExecute(Sender: TObject);
    procedure acDoSyncDataExecute(Sender: TObject);
    procedure acDoWriteAdHocExecute(Sender: TObject);
    procedure acGetRootKeysExecute(Sender: TObject);
    procedure acReadFromRegExecute(Sender: TObject);
    procedure acSetGroupIndexExecute(Sender: TObject);
  public type
    TOnGetRootKeys = procedure(aEdit: boolean) of object;
    TOnSetBooleanProperty = procedure(AValue: boolean) of object;
    TOnGetBooleanProperty = function : boolean of object;
    TOnSetIntegerProperty = procedure(aIndex: integer) of object;
    TOnGetIntegerProperty = function : integer of object;
    TOnReadFromRegistry = procedure of object;
  private
    FOnGetRootKeys: TOnGetRootKeys;
    //
    FOnSetCanRead: TOnSetBooleanProperty;
    FOnSetCanWrite: TOnSetBooleanProperty;
    FOnSetDoWriteAdHoc: TOnSetBooleanProperty;
    FOnSetDoSyncData: TOnSetBooleanProperty;
    FOnSetDoMergeData: TOnSetBooleanProperty;
    FOnSetGroupIndex: TOnSetIntegerProperty;
    //
    FOnGetCanRead: TOnGetBooleanProperty;
    FOnGetCanWrite: TOnGetBooleanProperty;
    FOnGetDoWriteAdHoc: TOnGetBooleanProperty;
    FOnGetDoSyncData: TOnGetBooleanProperty;
    FOnGetDoMergeData: TOnGetBooleanProperty;
    FOnGetGroupIndex: TOnGetIntegerProperty;
    //
    FOnReadFromRegistry: TOnReadFromRegistry;
    //
    FOnRefreshSettings: TRegControlProperties.TOnRefreshSettings;

    procedure RefreshRegControl;
    procedure SetCanRead;
    procedure SetCanWrite;
    procedure SetDoWriteAdHoc;
    procedure SetDoSyncData;
    procedure SetDoMegerData;
    procedure SetGroupIndex;
  public
    procedure Refresh;

    constructor Create(aOwner: TComponent); override;

    property OnGetRootKeys: TOnGetRootKeys
      read FOnGetRootKeys
      write FOnGetRootKeys;

    property OnRefreshSettings: TRegControlProperties.TOnRefreshSettings
      read FOnRefreshSettings
      write FOnRefreshSettings;

      property OnSetCanRead: TOnSetBooleanProperty
        read FOnSetCanRead
        write FOnSetCanRead;

      property OnSetCanWrite: TOnSetBooleanProperty
        read FOnSetCanWrite
        write FOnSetCanWrite;

      property OnSetDoWriteAdHoc: TOnSetBooleanProperty
        read FOnSetDoWriteAdHoc
        write FOnSetDoWriteAdHoc;

      property OnSetDoSyncData: TOnSetBooleanProperty
        read FonSetDoSyncData
        write FonSetDoSyncData;

      property OnSetDoMergeData: TOnSetBooleanProperty
        read FOnSetDoMergeData
        write FOnSetDoMergeData;

      property OnSetGroupIndex: TOnSetIntegerProperty
        read FOnSetGroupIndex
        write FOnSetGroupIndex;

      property OnGetCanRead: TOnGetBooleanProperty
        read FOnGetCanRead
        write FOnGetCanRead;

      property OnGetCanWrite: TOnGetBooleanProperty
        read FOnGetCanWrite
        write FOnGetCanWrite;

      property OnGetDoWriteAdHoc: TOnGetBooleanProperty
        read FOnGetDoWriteAdHoc
        write FOnGetDoWriteAdHoc;

      property OnGetDoSyncData: TOnGetBooleanProperty
        read FOnGetDoSyncData
        write FOnGetDoSyncData;

      property OnGetDoMergeData: TOnGetBooleanProperty
        read FOnGetDoMergeData
        write FOnGetDoMergeData;

      property OnGetGroupIndex: TOnGetIntegerProperty
        read FOnGetGroupIndex
        write FOnGetGroupIndex;

      property OnReadFromRegistry: TOnReadFromRegistry
        read FOnReadFromRegistry
        write FOnReadFromRegistry;
  end;

implementation

{$R *.lfm}

{ TRegControlButtonFrame }

procedure TRegControlButtonFrame.acGetRootKeysExecute(Sender: TObject);
begin
  if Assigned(FOnGetRootKeys) then
    FOnGetRootKeys(cbxEditRootKeys.Checked);
end;

procedure TRegControlButtonFrame.acReadFromRegExecute(Sender: TObject);
begin
  RefreshRegControl;
end;

procedure TRegControlButtonFrame.acSetGroupIndexExecute(Sender: TObject);
begin
  SetGroupIndex;
end;

procedure TRegControlButtonFrame.RefreshRegControl;
begin
  if Assigned(FOnReadFromRegistry) then
    FOnReadFromRegistry;
end;

procedure TRegControlButtonFrame.acCanReadExecute(Sender: TObject);
begin
  SetCanRead;
end;

procedure TRegControlButtonFrame.acCanWriteExecute(Sender: TObject);
begin
  SetCanWrite;
end;

procedure TRegControlButtonFrame.acDoMergeDataExecute(Sender: TObject);
begin
  SetDoMegerData;
end;

procedure TRegControlButtonFrame.acDoSyncDataExecute(Sender: TObject);
begin
  SetDoSyncData;
end;

procedure TRegControlButtonFrame.acDoWriteAdHocExecute(Sender: TObject);
begin
  SetDoWriteAdHoc;
end;

procedure TRegControlButtonFrame.SetCanRead;
begin
  if Assigned(FOnSetCanRead) then
    if cbxCanRead.Checked then
      FOnSetCanRead(True)
    else
      FOnSetCanRead(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetCanWrite;
begin
  if Assigned(FOnSetCanWrite) then
    if cbxCanWrite.Checked then
      FOnSetCanWrite(True)
    else
      FOnSetCanWrite(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetDoWriteAdHoc;
begin
  if Assigned(FOnSetDoWriteAdHoc) then
    if cbxDoWriteAdHoc.Checked then
      FOnSetDoWriteAdHoc(True)
    else
      FOnSetDoWriteAdHoc(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetDoSyncData;
begin
  if Assigned(FOnSetDoSyncData) then
    if cbxDoSyncData.Checked then
      FOnSetDoSyncData(True)
    else
      FOnSetDoSyncData(False);

  if Assigned(FOnRefreshSettings) then
     FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetDoMegerData;
begin
  if Assigned(FOnSetDoMergeData) then
    if cbxDoMergeData.Checked then
      FOnSetDoMergeData(True)
    else
      FOnSetDoMergeData(False);

  if Assigned(FOnRefreshSettings) then
    FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.SetGroupIndex;
begin
  if Assigned(FOnSetGroupIndex) then
    FOnSetGroupIndex(edtGroupIndex.Value);

  if Assigned(FOnRefreshSettings) then
    FOnRefreshSettings;
end;

procedure TRegControlButtonFrame.Refresh;
begin
  if Assigned(FOnGetCanRead) then
    cbxCanRead.Checked := FOnGetCanRead;

  if Assigned(FOnGetCanWrite) then
    cbxCanWrite.Checked := FOnGetCanWrite;

  if Assigned(FOnGetDoWriteAdHoc) then
    cbxDoWriteAdHoc.Checked := FOnGetDoWriteAdHoc;

  if Assigned(FOnGetDoSyncData) then
    cbxDoSyncData.Checked := FOnGetDoSyncData;

  if Assigned(FOnGetDoMergeData) then
  begin
    cbxDoMergeData.Enabled := True;
    cbxDoMergeData.Checked := FOnGetDoMergeData
  end
  else
    cbxDoMergeData.Enabled := False;

  if Assigned(FOnGetGroupIndex) then
    edtGroupIndex.Value := FOnGetGroupIndex;
end;

constructor TRegControlButtonFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Refresh
end;

end.

