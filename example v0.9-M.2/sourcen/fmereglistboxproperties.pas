unit fmereglistboxproperties;

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
  fmeregcontrolproperties,
  reglistbox;

type

  { TRegListBoxProperties }

  TRegListBoxProperties = class(TRegControlProperties<TRegListBox>);

implementation

{$R *.lfm}

{ TRegListBoxProperties }

end.

