unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Menus, ActnList, regsourcen, regedit;

type

  { TMain }

  TMain = class(TForm)
    acClose: TAction;
    acCreateExampleSettings: TAction;
    acCheckExampleSettings: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuExampleSettings: TMenuItem;
    mnuClose: TMenuItem;
    mnuFile: TMenuItem;
    RegistrySource1: TRegistrySource;
    procedure acCheckExampleSettingsExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCreateExampleSettingsExecute(Sender: TObject);
  private
    function CheckForExampleSettings: boolean;
    procedure CreateSettings;
  public
  end;

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMain.acCheckExampleSettingsExecute(Sender: TObject);
begin
  if CheckForExampleSettings then
    MessageDlg('Der Schlüssel "Desktop" ist vorhanden und gefüllt. Beispieleinträge sind grundsätzlich vorhanden!', mtInformation, [mbOk], 0)
  else
  begin
    MessageDlg('Der Schlüssel "Desktop" ist entweder nicht vorhanden oder nicht gefüllt. Beispieleinträge sind wahrscheinlich nicht vorhanden!', mtWarning, [mbOk], 0);
    CreateSettings;
  end;
end;

procedure TMain.acCreateExampleSettingsExecute(Sender: TObject);
begin
  CreateSettings;
end;

function TMain.CheckForExampleSettings: boolean;
var
  list: TStrings;
begin
  Result := False;
  with RegistrySource1 do
  begin
    list := TStringList.Create;
    try
      ReadSection('Desktop', list);
      Result := (list.count > 0);
    finally
      if Assigned(list) then
        FreeAndNil(list);
    end;
  end;
end;

procedure TMain.CreateSettings;
var
  use_defaults: boolean;
begin
  if (MessageDlg('Sollen Beispieleinträge in der Registry erstellt werden? (Einträge werden im Root: HKEY_CURRENT_USER erstellt)', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    use_defaults := (MessageDlg('Zu den Beispieleinträgen können Defaults vergeben werden. Sollen Defaults erstellt werden? (Defaults werden im Root: HKEY_LOCAL_MACHINE erstellt, Adminrechte eventuell notwendig!!!)', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
    with RegistrySource1 do
    begin
      WriteDefaults := use_defaults;
      WriteString('Desktop', 'Version', '1.0.0');
    end;
  end;
end;

end.

