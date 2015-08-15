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
  public type
    TOnSetSettings = procedure(ASettings: _T) of object;
  private
    FRegControlSettings: _T;
    FOnSetSettings: TOnSetSettings;
  protected
    procedure _Initialize; virtual;
  public
    constructor Create(aOwner: TComponent); override;

    procedure SetRegControlSettings(aControlSettings: _T);

    property RegControlSettings: _T
      read FRegControlSettings;

    property OnSetSettings: TOnSetSettings
      read FOnSetSettings
      write FOnSetSettings;
  end;

implementation

{$R *.lfm}

{ TCustomRegControlSettings<_T> }

procedure TCustomRegControlSettings<_T>._Initialize;
begin
  //
end;

constructor TCustomRegControlSettings<_T>.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  _Initialize;
end;

procedure TCustomRegControlSettings<_T>.SetRegControlSettings(
  aControlSettings: _T);
begin
  FRegControlSettings := aControlSettings;

  if Assigned(FOnSetSettings) then
    FOnSetSettings(FRegControlSettings);
end;

end.

