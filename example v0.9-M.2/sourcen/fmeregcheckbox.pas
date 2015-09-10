unit fmeregcheckbox;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regcheckbox, regradiobutton, regcheckgroup, Forms, ExtCtrls;

type

  { TControlRegCheckBox }

  TControlRegCheckBox = class(TCustomRegControlFrame<TRegCheckBox>)
    Bevel1: TBevel;
    RegCheckBox1: TRegCheckBox;
    RegCheckGroup1: TRegCheckGroup;
    RegRadioButton1: TRegRadioButton;
    ScrollBox1: TScrollBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegCheckBox._Initialize;
begin
  inherited;

  SetRegControl(RegCheckBox1);
end;

end.

