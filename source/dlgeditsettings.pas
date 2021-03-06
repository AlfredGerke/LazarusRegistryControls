unit dlgeditsettings;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls,
  regtype,
  RegBaseForm;

resourcestring
  rsEditRootKeys = 'Edit RootKeys';
  rsShowRootKeys = 'Show RootKeys';
  rsLblRootKey = 'RootKey';
  rsLblRootKeyForDefaults = 'RootKey for Defaults';
  rsLblRootForDefaults = 'Root for Defaults';
  rsLblGUID = 'GUID';
  rsLblOrganisation = 'Organisation';
  rsLblProject = 'Project';
  rsCbxReadDefaults = 'Read Defaults';
  rsCbxWriteDefaults = 'Write Defaults';

type

  { TEditSettings }

  TEditSettings = class(TRegBaseForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    cbxReadDefaults: TCheckBox;
    cbxWriteDefaults: TCheckBox;
    edtRootKey: TLabeledEdit;
    edtRootKeyForDefaults: TLabeledEdit;
    edtRootForDefaults: TLabeledEdit;
    edtGUID: TLabeledEdit;
    edtOrganisation: TLabeledEdit;
    edtProject: TLabeledEdit;
    lblGUID: TBoundLabel;
    lblOrganisation: TBoundLabel;
    lblProject: TBoundLabel;
    lblRookKey: TBoundLabel;
    lblRootForDefaults: TBoundLabel;
    lblRootKeyForDefaults: TBoundLabel;
    pnlBottom: TPanel;
    pnlClient: TPanel;
  private
    FEdit: boolean;

    procedure SetCaptions;
    procedure SetEdit(aEdit: boolean);
  public
    procedure SetData(aRootKeys: TRootKeysStruct);
    procedure GetData(var aRootKeys: TRootKeysStruct);
    function ShowModalEx(aEdit: boolean = False): integer; virtual;

    property Edit: boolean
      read FEdit;
  end;

var
  EditSettings: TEditSettings;

implementation

{$R *.lfm}

uses
  FileUtil,
  Classes;

{ TEditSettings }

procedure TEditSettings.SetCaptions;
begin
  edtRootKey.EditLabel.Caption := SetUTF8IfNeeded(rsLblRootKey);
  edtRootKeyForDefaults.EditLabel.Caption := SetUTF8IfNeeded(rsLblRootKeyForDefaults);
  edtRootForDefaults.EditLabel.Caption := SetUTF8IfNeeded(rsLblRootForDefaults);
  edtGUID.EditLabel.Caption := SetUTF8IfNeeded(rsLblGUID);
  edtOrganisation.EditLabel.Caption := SetUTF8IfNeeded(rsLblOrganisation);
  edtProject.EditLabel.Caption := SetUTF8IfNeeded(rsLblProject);
  cbxReadDefaults.Caption := SetUTF8IfNeeded(rsCbxReadDefaults);
  cbxWriteDefaults.Caption := SetUTF8IfNeeded(rsCbxWriteDefaults);
end;

procedure TEditSettings.SetEdit(aEdit: boolean);
begin
  FEdit := aEdit;
  if FEdit then
    Caption := rsEditRootKeys
  else
    Caption := rsShowRootKeys;
  edtRootKey.ReadOnly := not aEdit;
  edtRootKeyForDefaults.ReadOnly := not aEdit;
  edtRootForDefaults.ReadOnly := not aEdit;
  edtGUID.ReadOnly := not aEdit;
  edtOrganisation.ReadOnly := not aEdit;
  edtProject.ReadOnly := not aEdit;
  cbxReadDefaults.Enabled := aEdit;
  cbxWriteDefaults.Enabled := aEdit;
  btnOk.Enabled := aEdit;
  btnOk.Visible := aEdit;
end;

procedure TEditSettings.SetData(aRootKeys: TRootKeysStruct);
begin
  edtRootKey.Text := aRootKeys.RootKey;
  edtRootKeyForDefaults.Text := aRootKeys.RootKeyForDefaults;
  edtRootForDefaults.Text := aRootKeys.RootForDefaults;
  edtGUID.Text := aRootKeys.GUID;
  edtOrganisation.Text := aRootKeys.Organisation;
  edtProject.Text := aRootKeys.Project;
  cbxReadDefaults.Checked := aRootKeys.ReadDefaults;
  cbxWriteDefaults.Checked := aRootKeys.WriteDefaults;
end;

procedure TEditSettings.GetData(var aRootKeys: TRootKeysStruct);
begin
  if FEdit then
  begin
    aRootKeys.SetRootKeys(edtRootKey.Text,
       edtRootKeyForDefaults.Text,
       cbxReadDefaults.Checked,
       cbxWriteDefaults.Checked,
       edtRootForDefaults.Text,
       edtProject.Text,
       edtOrganisation.Text,
       edtGUID.Text);
  end;
end;

function TEditSettings.ShowModalEx(aEdit: boolean): integer;
begin
  SetCaptions;
  SetEdit(aEdit);
  Result := ShowModal;
end;

end.







