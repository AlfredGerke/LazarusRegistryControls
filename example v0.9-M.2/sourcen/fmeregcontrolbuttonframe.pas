unit fmeregcontrolbuttonframe;

{$mode delphi}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls,
  ActnList,
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
    cbxEditRootKeys: TCheckBox;
    cbxCanRead: TCheckBox;
    cbxCanWrite: TCheckBox;
    cbxDoWriteAdHoc: TCheckBox;
    cbxDoSyncData: TCheckBox;
    grbDialogs: TGroupBox;
    grpProperties: TGroupBox;
    pnlWorkspace: TPanel;
    btnGetRootKeys: TSpeedButton;
    procedure acCanReadExecute(Sender: TObject);
    procedure acCanWriteExecute(Sender: TObject);
    procedure acDoSyncDataExecute(Sender: TObject);
    procedure acDoWriteAdHocExecute(Sender: TObject);
    procedure acGetRootKeysExecute(Sender: TObject);
  public type
    TOnGetRootKeys = procedure(aEdit: boolean) of object;
    TOnSetBooleanProperty = procedure(AValue: boolean) of object;
    TOnGetBooleanProperty = function : boolean of object;
  private
    FOnGetRootKeys: TOnGetRootKeys;
    //
    FOnSetCanRead: TOnSetBooleanProperty;
    FOnSetCanWrite: TOnSetBooleanProperty;
    FOnSetDoWriteAdHoc: TOnSetBooleanProperty;
    FOnSetDoSyncData: TOnSetBooleanProperty;
    //
    FOnGetCanRead: TOnGetBooleanProperty;
    FOnGetCanWrite: TOnGetBooleanProperty;
    FOnGetDoWriteAdHoc: TOnGetBooleanProperty;
    FOnGetDoSyncData: TOnGetBooleanProperty;
    //
    FOnRefreshSettings: TRegControlProperties.TOnRefreshSettings;

    procedure SetCanRead;
    procedure SetCanWrite;
    procedure SetDoWriteAdHoc;
    procedure SetDoSyncData;
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
  end;

implementation

{$R *.lfm}

{ TRegControlButtonFrame }

procedure TRegControlButtonFrame.acGetRootKeysExecute(Sender: TObject);
begin
  if Assigned(FOnGetRootKeys) then
    FOnGetRootKeys(cbxEditRootKeys.Checked);
end;

procedure TRegControlButtonFrame.acCanReadExecute(Sender: TObject);
begin
  SetCanRead;
end;

procedure TRegControlButtonFrame.acCanWriteExecute(Sender: TObject);
begin
  SetCanWrite;
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
end;

constructor TRegControlButtonFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Refresh
end;

end.

