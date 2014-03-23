unit dlgguid;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
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
begin
  dlg_guid := TEditGUID.Create(nil);
  try
    with dlg_guid do
    begin
      AtDesignTime := AtDesignTime;
      case ShowModal of
        mrOK: aGUID := GetGUID;
      end;
    end;
  finally
    if Assigned(dlg_guid) then
      FreeAndNil(dlg_guid);

    Result := TryStringToGUID(aGUID, guid);
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

function TEditGUID.ShowModal: integer;
begin
  SetCaptions;

  Result := inherited ShowModal;
end;

end.

