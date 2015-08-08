unit frmmain_09_M2;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  StdCtrls, DBGrids, ComCtrls, ExtCtrls, Controls, regcombobox, reglistbox,
  regchecklistbox, regcheckgroup, regradiogroup, regcheckbox, regradiobutton,
  reglabel, regvaluelisteditor, regsourcen, regedit, Classes;

type

  { TMain }

  TMain = class(TForm)
    lstImageListOutline: TImageList;
    pnlOutline: TPanel;
    pnlDesktop: TPanel;
    Splitter1: TSplitter;
    tvOutline: TTreeView;
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
  Registry,
  Dialogs;

{ TMain }

procedure TMain.tvOutlineChange(Sender: TObject; Node: TTreeNode);
begin
  MessageDlg(Node.Text, mtInformation, [mbOK], 0);
end;

{ TMain }



end.

