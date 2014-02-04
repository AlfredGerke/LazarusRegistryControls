unit frmmain_09_M2;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, regcombobox,
  reglistbox, regchecklistbox, regcheckgroup, regradiogroup, regcheckbox,
  regradiobutton, reglabel, regvaluelisteditor;

type

  { TMain }

  TMain = class(TForm)
    RegCheckBox1: TRegCheckBox;
    RegCheckGroup1: TRegCheckGroup;
    RegCheckListBox1: TRegCheckListBox;
    RegComboBox1: TRegComboBox;
    RegLabel1: TRegLabel;
    RegListBox1: TRegListBox;
    RegRadioButton1: TRegRadioButton;
    RegRadioGroup1: TRegRadioGroup;
    RegValueListEditor1: TRegValueListEditor;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

end.

