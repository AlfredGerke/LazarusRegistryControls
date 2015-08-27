unit fmeregedit;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regedit;

type

  { TControlRegEdit }

  TControlRegEdit = class(TCustomRegControlFrame<TRegEdit>)
    RegEdit1: TRegEdit;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TControlRegEdit }

procedure TControlRegEdit._Initialize;
begin
  inherited;

  SetRegControl(RegEdit1);
end;

end.

