unit frmmain_09_M2;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  regcombobox, reglistbox, regchecklistbox, regcheckgroup, regradiogroup,
  regcheckbox, regradiobutton, reglabel, regvaluelisteditor, regsourcen,
  regedit;

type

  { TMain }

  TMain = class(TForm)
    Button1: TButton;
    RegCheckBox1: TRegCheckBox;
    RegCheckGroup1: TRegCheckGroup;
    RegCheckListBox1: TRegCheckListBox;
    RegComboBox1: TRegComboBox;
    RegEdit1: TRegEdit;
    RegistrySource1: TRegistrySource;
    RegLabel1: TRegLabel;
    RegListBox1: TRegListBox;
    RegRadioButton1: TRegRadioButton;
    RegRadioButton2: TRegRadioButton;
    RegRadioGroup1: TRegRadioGroup;
    RegValueListEditor1: TRegValueListEditor;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

uses
  Registry;

{ TMain }


procedure TMain.Button1Click(Sender: TObject);
var
  ini: TRegistryIniFile;
begin
  ini := TRegistryIniFile.Create('Software\Alfred');
  ini.WriteString('Section', 'Test', 'Hallo');
  ini.free;
end;

end.

