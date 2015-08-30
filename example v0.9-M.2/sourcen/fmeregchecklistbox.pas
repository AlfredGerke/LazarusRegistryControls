unit fmeregchecklistbox;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regchecklistbox;

type

  { TControlRegCheckListBox }

  TControlRegCheckListBox = class(TCustomRegControlFrame<TRegCheckListBox>)
    RegCheckListBox1: TRegCheckListBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegCheckListBox._Initialize;
begin
  inherited;

  SetRegControl(RegCheckListBox1);
end;


end.
