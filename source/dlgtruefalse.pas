unit dlgTrueFalse;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  Buttons,
  ExtCtrls,
  StdCtrls;

type

  { TTrueFalse }

  TTrueFalse = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    pnlBottom: TPanel;
    pnlClient: TPanel;
    rbtnFalse: TRadioButton;
    rbtnTrue: TRadioButton;
  private
  protected
    procedure SetCaptions; virtual; abstract;
    function GetSelectedIndex: integer;
    property SelectedIndex: integer
      read GetSelectedIndex;
  public
    function ShowModal: integer; override;
  end;

  { TSetSyncData }

  TSetSyncData = class(TTrueFalse)
  protected
    procedure SetCaptions; override;
  public
    property SelectedIndex;
  end;

  { TSetWriteAdHoc }

  TSetWriteAdHoc = class(TTrueFalse)
  protected
    procedure SetCaptions; override;
  public
    property SelectedIndex;
  end;

implementation

{$R *.lfm}

{ TSetSyncData }

procedure TSetSyncData.SetCaptions;
begin
  Self.Caption := 'Set SyncData-Property';
  Self.rbtnFalse.Caption := 'Synchronisation Off';
  Self.rbtnTrue.Caption := 'Synchronisation On';
end;

{ TSetWriteAdHoc }

procedure TSetWriteAdHoc.SetCaptions;
begin
  Self.Caption := 'Set WriteAdHoc-Property';
  Self.rbtnFalse.Caption := 'WriteAdHoc Off';
  Self.rbtnTrue.Caption := 'WriteAdHoc On';
end;

{ TTrueFalse }

function TTrueFalse.GetSelectedIndex: integer;
begin
  Result := -1;

  if rbtnFalse.Checked then
    Result := 0
  else
    if rbtnTrue.Checked then
      Result := 1;
end;

function TTrueFalse.ShowModal: integer;
begin
  SetCaptions;
  rbtnFalse.Checked := True;
  Result := inherited ShowModal;
end;

end.
