unit fmeregistrysource;

{$mode delphi}

interface

uses
  Forms,
  ExtCtrls,
  StdCtrls,
  reglabel;

type

  { TControlRegistrySource }

  TControlRegistrySource = class(TFrame)
    imgPic: TImage;
    lblAuthor: TLabel;
    lblVersion: TLabel;
    lblGit: TRegLabel;
    lblAGE: TRegLabel;
    lblLRC: TRegLabel;
    lblProject: TRegLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

