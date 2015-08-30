unit fmeregcombobox;

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
  regcombobox;

type

  { TControlRegComobBox }

  TControlRegComobBox = class(TCustomRegControlFrame<TRegComboBox>)
    RegComboBox1: TRegComboBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

procedure TControlRegComobBox._Initialize;
begin
  inherited;

  SetRegControl(RegComboBox1);
end;

end.

