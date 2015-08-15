unit fmecustomsettings;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls;

type

  { TCustomRegControlSettings }

  TCustomRegControlSettings<_T> = class(TFrame)
  private
    FRegControlSettings: _T;
  public
    procedure SetRegControlSettings(aControlSettings: _T);
  end;

implementation

{$R *.lfm}

{ TCustomRegControlSettings<_T> }

procedure TCustomRegControlSettings<_T>.SetRegControlSettings(
  aControlSettings: _T);
begin
  FRegControlSettings := aControlSettings;
end;

end.

