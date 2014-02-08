unit frmmain_09_M2;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, regcombobox,
  reglistbox, regchecklistbox, regcheckgroup, regradiogroup, regcheckbox,
  regradiobutton, reglabel, regvaluelisteditor, regtype, regsourcen;

type

  { TMain }

  TMain = class(TForm)
    RegCheckBox1: TRegCheckBox;
    RegistrySource1: TRegistrySource;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }


end.

