unit fmeregistrysource;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls, ExtCtrls, StdCtrls;

type

  { TControlRegistrySource }

  TControlRegistrySource = class(TFrame)
    imgPic: TImage;
    lblAuthor: TLabel;
    lblCompName1: TLabel;
    lblCompName2: TLabel;
    lblGitHub: TLabel;
    lblName: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

