unit fmereglistboxproperties;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  fmeregcontrolproperties, reglistbox;

type

  { TRegListBoxProperties }

  TRegListBoxProperties = class(TRegControlProperties<TRegListBox>)
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TRegListBoxProperties }

procedure TRegListBoxProperties._Initialize;
begin
  // Default
  DoCreateCaptionProperites := False;
end;

end.

