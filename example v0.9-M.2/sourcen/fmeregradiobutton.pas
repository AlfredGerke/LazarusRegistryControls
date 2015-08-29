unit fmeregradiobutton;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regradiobutton;

type

  { TControlRegRadioButton }

  TControlRegRadioButton = class(TCustomRegControlFrame<TRegRadioButton>)
    RegRadioButton1: TRegRadioButton;
    RegRadioButton2: TRegRadioButton;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegRadioButton._Initialize;
begin
  inherited;

  SetRegControl(RegRadioButton1);
end;


end.

