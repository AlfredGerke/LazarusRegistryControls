unit fmeControlDetails;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, fmereglistbox;

type

  { TControlDetails }

  TControlDetails = class(TFrame)
    FrameRegListBox: TControlRegListBox;
    pnlClient: TPanel;
    pnlLeft: TPanel;
    pnlButtom: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

