unit dlgaboutcomponent;

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
  Buttons;

resourcestring
  rsAboutFormCaption = 'About...';
  rsLblAuthorCaption = 'Author:';

type

  { TAboutComponent }

  TAboutComponent = class(TForm)
    btnOk: TBitBtn;
    imgPic: TImage;
    lblCompName1: TLabel;
    lblCompName2: TLabel;
    lblAuthor: TLabel;
    lblName: TLabel;
    lblGitHub: TLabel;
    pnlBottom: TPanel;
    pnlClient: TPanel;
  private
    procedure SetCaptions;
  public
    function ShowModal: integer; override;
  end;

procedure StartAbout;

implementation

procedure StartAbout;
var
  about: TAboutComponent;
begin
  about := TAboutComponent.Create(nil);
  try
    about.ShowModal;
  finally
    if Assigned(about) then
      FreeAndNil(about);
  end;
end;

{ TAboutComponent }

procedure TAboutComponent.SetCaptions;
begin
  Self.Caption := rsAboutFormCaption;
  lblAuthor.Caption := rsLblAuthorCaption;
end;

function TAboutComponent.ShowModal: integer;
begin
  SetCaptions;
  Result:=inherited ShowModal;
end;

{$R *.lfm}

end.
