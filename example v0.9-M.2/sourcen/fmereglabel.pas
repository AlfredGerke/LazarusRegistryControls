unit fmereglabel;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  reglabel,
  regedit, ExtCtrls, Forms;

type

  { TControlRegLabel }

  TControlRegLabel = class(TCustomRegControlFrame<TRegLabel>)
    Bevel1: TBevel;
    RegEdit1: TRegEdit;
    RegLabel1: TRegLabel;
    ScrollBox1: TScrollBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TControlRegLabel }

procedure TControlRegLabel._Initialize;
begin
  inherited;

  SetRegControl(RegLabel1);
end;

end.

