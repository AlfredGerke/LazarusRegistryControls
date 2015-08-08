unit frmmain_09_M2;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  ComCtrls,
  ExtCtrls,
  Controls, fmeControlDetails, Classes;

type

  { TMain }

  TMain = class(TForm)
    FrameControlDetails: TControlDetails;
    lstImageListOutline: TImageList;
    pnlOutline: TPanel;
    pnlDesktop: TPanel;
    spOutline: TSplitter;
    tvOutline: TTreeView;
    procedure FrameControlDetailsClick(Sender: TObject);
    procedure tvOutlineChange(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

uses
  Dialogs;

{ TMain }

procedure TMain.tvOutlineChange(Sender: TObject; Node: TTreeNode);
begin
  MessageDlg(Node.Text, mtInformation, [mbOK], 0);
end;

procedure TMain.FrameControlDetailsClick(Sender: TObject);
begin

end;

end.

