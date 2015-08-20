{ TODO -oAlfred Gerke -cRegControlFrames erstellen : Nach dem Vorbild dieser Unit müssen für jedes RegControl  ein eigener Frame erstellt werden }
unit fmereglistbox;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  ExtCtrls,
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

