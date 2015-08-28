unit fmereglabel;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  fmecustomcontrolframe,
  reglabel, regedit;

type

  { TControlRegLabel }

  TControlRegLabel = class(TCustomRegControlFrame<TRegLabel>)
    RegEdit1: TRegEdit;
    RegLabel1: TRegLabel;
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

