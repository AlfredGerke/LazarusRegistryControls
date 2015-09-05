unit fmeregradiobutton;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regradiobutton,
  regchecklistbox,
  Forms;

type

  { TControlRegRadioButton }

  TControlRegRadioButton = class(TCustomRegControlFrame<TRegRadioButton>)
    RegCheckListBox1: TRegCheckListBox;
    RegRadioButton1: TRegRadioButton;
    RegRadioButton2: TRegRadioButton;
    RegRadioButton3: TRegRadioButton;
    RegRadioButton4: TRegRadioButton;
    RegRadioButton5: TRegRadioButton;
    ScrollBox1: TScrollBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TControlRegRadioButton }

procedure TControlRegRadioButton._Initialize;
begin
  inherited;

  SetRegControl(RegRadioButton1);
end;


end.

