unit fmereglistbox;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, reglistbox;

type

  { TControlRegListBox }

  TControlRegListBox = class(TFrame)
    pnlClient: TPanel;
    rlstListBox: TRegListBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

