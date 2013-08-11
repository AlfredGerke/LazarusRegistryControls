unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, regsourcen,
  regedit;

type

  { TForm1 }

  TForm1 = class(TForm)
    RegEdit1: TRegEdit;
    RegistrySource1: TRegistrySource;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

