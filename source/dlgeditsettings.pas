unit dlgeditsettings;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls,
  regtype;

type

  { TEditSettings }

  TEditSettings = class(TForm)
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

{ TEditSettings }

procedure TEditSettings.SetEdit(aEdit: boolean);
begin
  FEdit := aEdit;
  if FEdit then
    Caption := 'Edit RootKeys'
  else
    Caption := 'Show RootKeys';
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
    aRootKeys.RootKey := edtRootKey.Text;
    aRootKeys.RootKeyForDefaults := edtRootKeyForDefaults.Text;
    aRootKeys.RootForDefaults := edtRootForDefaults.Text;
    aRootKeys.GUID := edtGUID.Text;
    aRootKeys.Organisation := edtOrganisation.Text;
    aRootKeys.Project := edtProject.Text;
    aRootKeys.ReadDefaults := cbxReadDefaults.Checked;
    aRootKeys.WriteDefaults := cbxWriteDefaults.Checked;
  end;
end;

function TEditSettings.ShowModalEx(aEdit: boolean): integer;
begin
  SetEdit(aEdit);
  Result := ShowModal;
end;

end.







