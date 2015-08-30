unit fmeregcheckgroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regcheckgroup;

type

  { TControlRegCheckGroup }

  TControlRegCheckGroup = class(TCustomRegControlFrame<TRegCheckGroup>)
    RegCheckGroup1: TRegCheckGroup;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegCheckGroup._Initialize;
begin
  inherited;

  SetRegControl(RegCheckGroup1);
end;


end.

