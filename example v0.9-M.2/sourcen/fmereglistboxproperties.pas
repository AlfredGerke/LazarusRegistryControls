unit fmereglistboxproperties;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
  fmeregcontrolproperties, reglistbox;

type
  TRegListBoxProperties = class(TRegControlProperties<TRegListBox>)
  private
  public
  end;

implementation

{$R *.lfm}

end.

