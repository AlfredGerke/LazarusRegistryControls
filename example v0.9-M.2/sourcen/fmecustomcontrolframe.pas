unit fmecustomcontrolframe;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  Classes,
  regtype;

type

  { TCustomRegControlFrame }

  TCustomRegControlFrame<_T> = class(TFrame)
  private
    FRegControl: _T;

  protected
    procedure _Initialize; virtual;

    procedure BeforeRegistrySettingChangeProc(aOldSettingInfo: TRegistrySettingValue;
                                              aNewSettingInfo: TRegistrySettingValue;
                                              var aIsOk: boolean);
  public
    constructor Create(aOwner: TComponent); override;

    function GetRegControlName: string;
    procedure SetRegControl(aControl: _T);

    procedure GetRootKeys(aEdit: boolean);

    procedure SetCanRead(aValue: boolean);
    procedure SetCanWrite(aValue: boolean);
    procedure SetDoWriteAdHoc(aValue: boolean);
    procedure SetDoSyncData(aValue: boolean);
    procedure SetGroupIndex(aValue: integer);

    function GetCanRead: boolean;
    function GetCanWrite: boolean;
    function GetDoWriteAdHoc: boolean;
    function GetDoSyncData: boolean;
    function GetGroupIndex: integer;

    procedure ReadFromRegProc;

    property RegControl: _T
      read FRegControl;
  end;

implementation

{$R *.lfm}

uses
  Dialogs,
  Controls;

{ TCustomRegControlFrame<_T> }

procedure TCustomRegControlFrame<_T>.BeforeRegistrySettingChangeProc(
  aOldSettingInfo: TRegistrySettingValue;
  aNewSettingInfo: TRegistrySettingValue;
  var aIsOk: boolean);
begin
  aIsOk := False;

  case aNewSettingInfo.Kind of
    rskRootKey:
      if (aNewSettingInfo.RootKey <> aOldSettingInfo.RootKey) then
        aIsOk :=
          (MessageDlg('RootKey: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskRootKeyForDefault:
      if (aNewSettingInfo.RootKeyForDefault <> aOldSettingInfo.RootKeyForDefault) then
        aIsOk :=
          (MessageDlg('RootKeyForDefault: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskSection:
      if (aNewSettingInfo.Section <> aOldSettingInfo.Section) then
        aIsOk :=
          (MessageDlg('Section: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskIdent:
      if (aNewSettingInfo.Ident <> aOldSettingInfo.Ident) then
        aIsOk :=
          (MessageDlg('Ident: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskDefault:
      if (aNewSettingInfo.Default <> aOldSettingInfo.Default) then
        aIsOk :=
          (MessageDlg('Default: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskReadDefaults:
      if (aNewSettingInfo.ReadDefaults <> aOldSettingInfo.ReadDefaults) then
        aIsOk :=
          (MessageDlg('ReadDefaults: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskWriteDefaults:
      if (aNewSettingInfo.WriteDefaults <> aOldSettingInfo.WriteDefaults) then
        aIsOk :=
          (MessageDlg('WriteDefaults: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskRootForDefaults:
      if (aNewSettingInfo.RootForDefaults <> aOldSettingInfo.RootForDefaults) then
        aIsOk :=
          (MessageDlg('RootForDefaults: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskCanRead:
      if (aNewSettingInfo.CanRead <> aOldSettingInfo.CanRead) then
        aIsOk :=
          (MessageDlg('CanRead: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskCanWrite:
      if (aNewSettingInfo.CanWrite <> aOldSettingInfo.CanWrite) then
        aIsOk :=
          (MessageDlg('CanWrite: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskDoWriteAdHoc:
      if (aNewSettingInfo.DoWriteAdHoc <> aOldSettingInfo.DoWriteAdHoc) then
        aIsOk :=
          (MessageDlg('DoWriteAdHoc: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskDoSyncData:
      if (aNewSettingInfo.DoSyncData <> aOldSettingInfo.DoSyncData) then
        aIsOk :=
          (MessageDlg('DoSyncData: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskDoMergeData:
      if (aNewSettingInfo.DoMergeData <> aOldSettingInfo.DoMergeData) then
        aIsOk :=
          (MessageDlg('DoMergeData: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskSectionForCaption:
      if (aNewSettingInfo.SectionForCaption <> aOldSettingInfo.SectionForCaption) then
        aIsOk :=
          (MessageDlg('SectionForCaption: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    rskIdentForCaption:
      if (aNewSettingInfo.IdentForCaption <> aOldSettingInfo.IdentForCaption) then
        aIsOk :=
          (MessageDlg('IdentForCaption: Anpassungen erlauben?',
             mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  else
    MessageDlg('Unbekannter Vorgang erkannt und abgewiesen!',
      mtConfirmation, [mbYes, mbNo], 0);
  end;
end;

procedure TCustomRegControlFrame<_T>._Initialize;
begin
  // In Ableitung überschreiben
end;

constructor TCustomRegControlFrame<_T>.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  _Initialize;
end;

function TCustomRegControlFrame<_T>.GetRegControlName: string;
begin
  Result := FRegControl.Name;
end;

procedure TCustomRegControlFrame<_T>.SetRegControl(aControl: _T);
begin
  FRegControl := aControl;
  FRegControl.RegistrySettings.OnBeforeRegistrySettingChange :=
    BeforeRegistrySettingChangeProc;
end;

procedure TCustomRegControlFrame<_T>.GetRootKeys(aEdit: boolean);
var
  curr_edit: boolean;
begin
  curr_edit := FRegControl.RegistrySource.EditClientRootKeys;
  try
    if (curr_edit <> aEdit) then
      FRegControl.RegistrySource.EditClientRootKeys := aEdit;

    FRegControl.RegistrySource.ShowClientEditDialog(GetRegControlName);
  finally
    if (curr_edit <> aEdit) then
      FRegControl.RegistrySource.EditClientRootKeys := curr_edit;
  end;
end;

procedure TCustomRegControlFrame<_T>.SetCanRead(aValue: boolean);
begin
  FRegControl.RegistrySettings.CanRead := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetCanWrite(aValue: boolean);
begin
  FRegControl.RegistrySettings.CanWrite := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetDoWriteAdHoc(aValue: boolean);
begin
  FRegControl.RegistrySettings.DoWriteAdHoc := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetDoSyncData(aValue: boolean);
begin
  FRegControl.RegistrySettings.DoSyncData := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetGroupIndex(aValue: integer);
begin
 FRegControl.RegistrySettings.GroupIndex := aValue;
end;

function TCustomRegControlFrame<_T>.GetCanRead: boolean;
begin
  Result := FRegControl.RegistrySettings.CanRead;
end;

function TCustomRegControlFrame<_T>.GetCanWrite: boolean;
begin
  Result := FRegControl.RegistrySettings.CanWrite;
end;

function TCustomRegControlFrame<_T>.GetDoWriteAdHoc: boolean;
begin
  Result := FRegControl.RegistrySettings.DoWriteAdHoc;
end;

function TCustomRegControlFrame<_T>.GetDoSyncData: boolean;
begin
  Result := FRegControl.RegistrySettings.DoSyncData;
end;

function TCustomRegControlFrame<_T>.GetGroupIndex: integer;
begin
  Result := FRegControl.RegistrySettings.GroupIndex;
end;

procedure TCustomRegControlFrame<_T>.ReadFromRegProc;
begin
  FRegControl.ReadFromReg;
end;

end.

