unit dlgguid;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  Controls,
  Dialogs,
  ExtCtrls,
  Buttons,
  RegBaseForm;

resourcestring
  rsEditGUID = 'Edit GUID';
  rsGUID = 'GUID';
  rsErrorCreateGUID_FMT = 'Error by creating GUID [%d]';

type

  { TEditGUID }

  TEditGUID = class(TRegBaseForm)
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    edtGUID: TLabeledEdit;
    pnlBottom: TPanel;
    pnlClient: TPanel;
    btnGUID: TSpeedButton;
    procedure btnGUIDClick(Sender: TObject);
  private
    procedure GenGUID;
    procedure SetCaptions;
  public
    function GetGUID: string;
    procedure SetGUID(aGUID: string);
    function ShowModal: integer; override;
  end;

function ShowGUIDDlg(var aGUID: string;
                     aAtDesignTime: boolean = True): boolean;

implementation

{$R *.lfm}

uses
  regutils;

function ShowGUIDDlg(var aGUID: string;
  aAtDesignTime: boolean = True): boolean;
var
  dlg_guid: TEditGUID;
  guid: TGuid;
  guid_str: string;
begin
  guid_str := EmptyStr;
  dlg_guid := TEditGUID.Create(nil);
  try
    with dlg_guid do
    begin
      AtDesignTime := aAtDesignTime;
      SetGUID(aGUID);
      case ShowModal of
        mrOK: guid_str := GetGUID;
      end;
    end;
  finally
    if Assigned(dlg_guid) then
      FreeAndNil(dlg_guid);

    Result := TryStringToGUID(guid_str, guid);
    if Result then
      aGUID := guid_str;
  end;
end;

{ TEditGUID }

procedure TEditGUID.btnGUIDClick(Sender: TObject);
begin
  GenGUID;
end;

procedure TEditGUID.GenGUID;
var
  guid: string;
  error: integer;
begin
  guid := EmptyStr;
  error := _GenGUIDAsStr(guid);
  if (error = 0) then
    edtGUID.Text := guid
  else
    MessageDlg(Format(rsErrorCreateGUID_FMT, [error]), mtError, [mbOK], 0);
end;

procedure TEditGUID.SetCaptions;
begin
  Caption := rsEditGUID;
  edtGUID.EditLabel.Caption := rsGUID;
end;

function TEditGUID.GetGUID: string;
begin
  Result := edtGUID.Text;
end;

procedure TEditGUID.SetGUID(aGUID: string);
begin
  edtGUID.Text := aGUID;
end;

function TEditGUID.ShowModal: integer;
begin
  SetCaptions;

  Result := inherited ShowModal;
end;

end.

