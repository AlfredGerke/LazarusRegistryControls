unit frmmain_09_M2;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, regcombobox,
  reglistbox, regchecklistbox, regcheckgroup, regradiogroup, regcheckbox,
  regradiobutton, reglabel, regvaluelisteditor, regsourcen, regedit;

type

  { TMain }

  TMain = class(TForm)
    RegCheckBox1: TRegCheckBox;
    RegCheckGroup1: TRegCheckGroup;
    RegCheckListBox1: TRegCheckListBox;
    RegComboBox1: TRegComboBox;
    RegEdit1: TRegEdit;
    RegistrySource1: TRegistrySource;
    RegListBox1: TRegListBox;
    RegRadioButton1: TRegRadioButton;
    RegRadioButton2: TRegRadioButton;
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

{ TMain }


end.

