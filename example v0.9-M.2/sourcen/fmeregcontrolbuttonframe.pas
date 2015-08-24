unit fmeregcontrolbuttonframe;

{$mode delphi}

interface

uses
  Forms,
  ExtCtrls,
  Buttons,
  StdCtrls, ActnList;

type

  { TRegControlButtonFrame }

  TRegControlButtonFrame = class(TFrame)
    acList: TActionList;
    acGetRootKeys: TAction;
    cbxEditRootKeys: TCheckBox;
    pnlWorkspace: TPanel;
    SpeedButton1: TSpeedButton;
    procedure acGetRootKeysExecute(Sender: TObject);
  public type
    TOnGetRootKeys = procedure(aEdit: boolean) of object;
    TOnSetWriteAdHoc = procedure(aWriteAdHoc: boolean) of object;
  private
    FOnGetRootKeys: TOnGetRootKeys;
  public
    property OnGetRootKeys: TOnGetRootKeys
      read FOnGetRootKeys
      write FOnGetRootKeys;
  end;

implementation

{$R *.lfm}

{ TRegControlButtonFrame }

procedure TRegControlButtonFrame.acGetRootKeysExecute(Sender: TObject);
begin
  if Assigned(FOnGetRootKeys) then
    FOnGetRootKeys(cbxEditRootKeys.Checked);
end;

end.

