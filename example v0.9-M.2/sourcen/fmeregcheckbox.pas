unit fmeregcheckbox;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls, Graphics, Dialogs, fmecustomcontrolframe,
  regcheckbox;

type

  { TControlRegCheckBox }

  TControlRegCheckBox = class(TCustomRegControlFrame<TRegCheckBox>)
    RegCheckBox1: TRegCheckBox;
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

