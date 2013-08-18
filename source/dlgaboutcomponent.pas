unit dlgaboutcomponent;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

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
    { private declarations }
  public
    { public declarations }
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

{$R *.lfm}


end.

