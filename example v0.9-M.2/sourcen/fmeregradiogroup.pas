unit fmeregradiogroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regradiogroup;

type

  { TControlRegRadioGroup }

  TControlRegRadioGroup = class(TCustomRegControlFrame<TRegRadioGroup>)
    RegRadioGroup1: TRegRadioGroup;
  protected
     procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegRadioGroup._Initialize;
begin
  inherited;

  SetRegControl(RegRadioGroup1);
end;

end.

