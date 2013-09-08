unit dlgTrueFalse;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  Buttons,
  ExtCtrls,
  StdCtrls,
  regbaseform;

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

  TTrueFalse = class(TRegBaseForm)
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

uses
  FileUtil,
  Classes;

{ TSetMergeData }

procedure TSetMergeData.SetCaptions;
begin
  Self.Caption := SetUTF8IfNeeded(rsSetMergeDataProperty);
  Self.rbtnFalse.Caption := SetUTF8IfNeeded(rsSetMergeDataOff);
  Self.rbtnTrue.Caption := SetUTF8IfNeeded(rsSetMergeDataOn);
end;

{ TSetSyncData }

procedure TSetSyncData.SetCaptions;
begin
  Self.Caption := SetUTF8IfNeeded(rsSetSyncDataProperty);
  Self.rbtnFalse.Caption := SetUTF8IfNeeded(rsSynchronisationOff);
  Self.rbtnTrue.Caption := SetUTF8IfNeeded(rsSynchronisationOn);
end;

{ TSetWriteAdHoc }

procedure TSetWriteAdHoc.SetCaptions;
begin
  Self.Caption := SetUTF8IfNeeded(rsSetWriteAdHocProperty);
  Self.rbtnFalse.Caption := SetUTF8IfNeeded(rsWriteAdHocOff);
  Self.rbtnTrue.Caption := SetUTF8IfNeeded(rsWriteAdHocOn);
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
