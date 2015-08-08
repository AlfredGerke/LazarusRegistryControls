unit fmereglistbox;

{$mode delphi}

interface

uses
  Classes,
  ExtCtrls,
  reglistbox,
  regbaseframe;

type

  { TControlRegListBox }

  TControlRegListBox = class(TRegControlFrame<TRegListBox>)
    pnlClient: TPanel;
    RegListBox1: TRegListBox;
  private
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TControlRegListBox }

constructor TControlRegListBox.Create(aOwner: TComponent);
begin
  inherited;

  SetRegControl(RegListBox1);
end;

end.

