unit fmereglistbox;

{$mode Delphi}{$H+}

interface

uses
  fmecustomcontrolframe,
  reglistbox;

type

  { TControlRegListBox }

  TControlRegListBox = class(TCustomRegControlFrame<TRegListBox>)
    RegListBox1: TRegListBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegListBox._Initialize;
begin
  inherited;

  SetRegControl(RegListBox1);
end;

end.

