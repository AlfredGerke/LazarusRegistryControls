unit dlgTrueFalse;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  Buttons,
  ExtCtrls,
  StdCtrls;

resourcestring
  rsSetMergeDataProperty = 'Set MergeData-Property';
  rsSetMergeDataOff = 'MergeData Off';
  rsSetMergeDataOn = 'MergeData On';
  rsSetSyncDataProperty = 'Set SyncData-Property';
  rsSynchronisationOff = 'Synchronisation Off';
  rsSynchronisationOn = 'Synchronisation On';
  rsSetWriteAdHocProperty = 'Set WriteAdHoc-Property';
  rsWriteAdHocOff = 'WriteAdHoc Off';
  rsWriteAdHocOn = 'WriteAdHoc On';

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

  { TSetMergeData }

  TSetMergeData = class(TTrueFalse)
  protected
    procedure SetCaptions; override;
  public
    property SelectedIndex;
  end;

implementation

{$R *.lfm}

{ TSetMergeData }

procedure TSetMergeData.SetCaptions;
begin
  Self.Caption := rsSetMergeDataProperty;
  Self.rbtnFalse.Caption := rsSetMergeDataOff;
  Self.rbtnTrue.Caption := rsSetMergeDataOn;
end;

{ TSetSyncData }

procedure TSetSyncData.SetCaptions;
begin
  Self.Caption := rsSetSyncDataProperty;
  Self.rbtnFalse.Caption := rsSynchronisationOff;
  Self.rbtnTrue.Caption := rsSynchronisationOn;
end;

{ TSetWriteAdHoc }

procedure TSetWriteAdHoc.SetCaptions;
begin
  Self.Caption := rsSetWriteAdHocProperty;
  Self.rbtnFalse.Caption := rsWriteAdHocOff;
  Self.rbtnTrue.Caption := rsWriteAdHocOn;
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
