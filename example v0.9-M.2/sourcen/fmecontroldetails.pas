unit fmeControlDetails;

{$mode delphi}

interface

uses
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls,
  ActnList, Buttons,
  fmereglistbox;

type

  { TControlDetails }

  TControlDetails = class(TFrame)
    acList: TActionList;
    acShowRootKeys: TAction;
    cbxEditRootKeys: TCheckBox;
    pnlClient: TPanel;
    pnlLeft: TPanel;
    pnlButtom: TPanel;
    SpeedButton1: TSpeedButton;
    procedure acShowRootKeysExecute(Sender: TObject);
  strict private type
    TOnGetRootKeys = procedure(aEdit: boolean) of object;
    TOnSetWriteAdHoc = procedure(aWriteAdHoc: boolean) of object;
  private
    FRegControlFrame: TFrame;
    FGetRootKeysProc: TOnGetRootKeys;
    FSetWriteAdHoc: TOnSetWriteAdHoc;
  public
    procedure GetRegControl(aLabel: string);
  end;

implementation

{$R *.lfm}


{ TControlDetails }

procedure TControlDetails.acShowRootKeysExecute(Sender: TObject);
begin
  if Assigned(FGetRootKeysProc) then
    FGetRootKeysProc(cbxEditRootKeys.Checked);
end;

procedure TControlDetails.GetRegControl(aLabel: string);
begin
  if Assigned(FRegControlFrame) then
    FreeAndNil(FRegControlFrame);

  FGetRootKeysProc := nil;

  if (aLabel = 'TRegListBox') then
  begin
    FRegControlFrame := TControlRegListBox.Create(pnlLeft);
    FRegControlFrame.Parent := pnlLeft;
    FRegControlFrame.Align := alClient;

    FGetRootKeysProc := TControlRegListBox(FRegControlFrame).GetRootKeys;
  end;
end;


end.

