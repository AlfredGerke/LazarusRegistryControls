unit dlgeditsettings;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls;

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

    procedure SetEditMode(aEdit: boolean);
  public
    procedure SetData(aRootKey: string;
                      aRootKeyForDefaults: string;
                      aRootForDefaults: string;
                      aGUID: string;
                      aOrganisation: string;
                      aProject: string;
                      aReadDefaults: boolean;
                      aWriteDefaults: boolean);
    function ShowModalEx(aEdit: boolean = False): integer; virtual;

    property Edit: boolean
      read FEdit;
  end;

var
  EditSettings: TEditSettings;

implementation

{$R *.lfm}

{ TEditSettings }

procedure TEditSettings.SetEditMode(aEdit: boolean);
begin
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

procedure TEditSettings.SetData(aRootKey: string;
  aRootKeyForDefaults: string;
  aRootForDefaults: string;
  aGUID: string;
  aOrganisation: string;
  aProject: string;
  aReadDefaults: boolean;
  aWriteDefaults: boolean);
begin
  edtRootKey.Text := aRootKey;
  edtRootKeyForDefaults.Text := aRootKeyForDefaults;
  edtRootForDefaults.Text := aRootForDefaults;
  edtGUID.Text := aGUID;
  edtOrganisation.Text := aOrganisation;
  edtProject.Text := aProject;
  cbxReadDefaults.Checked := aReadDefaults;
  cbxWriteDefaults.Checked := aWriteDefaults;
end;

function TEditSettings.ShowModalEx(aEdit: boolean): integer;
begin
  SetEditMode(aEdit);
  Result := ShowModal;
end;

end.







