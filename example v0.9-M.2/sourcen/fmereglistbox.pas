unit fmereglistbox;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, fmecustomcontrolframe,
  reglistbox;

type

  { TControlRegListBox }

  TControlRegListBox = class(TCustomRegControlFrame<TRegListBox>)
    RegListBox1: TRegListBox;
  private
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

constructor TControlRegListBox.Create(aOwner: TComponent);
begin
  inherited;

  SetRegControl(RegListBox1);
end;

end.

