unit fmeregcontrolcaptionsettings;

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
  fmecustomsettings,
  regsourcen;

type
  TRegControlCaptionSettings = class(TCustomRegControlSettings<TCaptionSettings>)
  private
  public
  end;

implementation

{$R *.lfm}

end.

