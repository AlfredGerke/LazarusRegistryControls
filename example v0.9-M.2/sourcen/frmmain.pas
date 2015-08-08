unit frmMain;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  ComCtrls,
  ExtCtrls,
  Controls,
  fmeControlDetails,
  Classes;

type

  { TMain }

  TMain = class(TForm)
    lstImageListOutline: TImageList;
    pnlOutline: TPanel;
    pnlDesktop: TPanel;
    spOutline: TSplitter;
    tvOutline: TTreeView;
    procedure tvOutlineChange(Sender: TObject; Node: TTreeNode);
  private
    FDesktopFrame: TControlDetails;
  public
    constructor Create(aOwner: TComponent); override;
  end;

var
  Main: TMain;

implementation

{$R *.lfm}


{ TMain }

procedure TMain.tvOutlineChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(FDesktopFrame) then
    FDesktopFrame.GetRegControl(Node.Text);
end;

constructor TMain.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FDesktopFrame := TControlDetails.Create(pnlDesktop);
  FDesktopFrame.Parent := pnlDesktop;
  FDesktopFrame.Align := alClient;
end;

end.

