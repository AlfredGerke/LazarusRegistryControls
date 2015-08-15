unit fmeregcontrolproperties;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, reglistbox;

type

  { TRegControlProperties }

  TRegControlProperties<_T> = class(TFrame)
  private
    FRegControl: _T;
  public
    procedure SetRegControl(AControl: _T);
  end;

implementation

{$R *.lfm}

{ TRegControlProperties<_T> }

procedure TRegControlProperties<_T>.SetRegControl(aControl: _T);
begin
  FRegControl := aControl;
end;

end.

