unit frmMain;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  ComCtrls,
  ExtCtrls,
  Controls, StdCtrls,
  fmeControlDetails,
  Classes;

type

  { TMain }

  TMain = class(TForm)
    lstImageListOutline: TImageList;
    pnlOutline: TPanel;
    pnlDesktop: TPanel;
    RadioButton1: TRadioButton;
    spOutline: TSplitter;
    tvOutline: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure tvOutlineChange(Sender: TObject; Node: TTreeNode);
  private
    FDesktopFrame: TControlDetails;

    procedure SetTitelProc(aCaption: string);
  public
    constructor Create(aOwner: TComponent); override;
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

uses
  SysUtils;

{ TMain }

procedure TMain.tvOutlineChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(FDesktopFrame) then
    FDesktopFrame.GetRegControl(Node.Text);
end;

procedure TMain.SetTitelProc(aCaption: string);
begin
  if (Trim(aCaption) = EmptyStr) then
    Self.Caption := 'Example (LRC 0.9 M2)'
  else
    Self.Caption := Format('Example (LRC 0.9 M2): %s', [aCaption]);
end;

procedure TMain.FormShow(Sender: TObject);
begin
  FDesktopFrame.GetRegControl('TRegistrySource');
end;

constructor TMain.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FDesktopFrame := TControlDetails.Create(pnlDesktop);
  FDesktopFrame.Parent := pnlDesktop;
  FDesktopFrame.Align := alClient;
  FDesktopFrame.OnSetTitel := SetTitelProc;
end;

end.

