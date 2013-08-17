unit regchecklistbox;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  CheckLst,
  regconst,
  LMessages,
  regmsg;

type

  { TCustomRegCheckListBox }

  TCustomRegCheckListBox = class(TCheckListBox)
  private
  protected
  public
  published
  end;

  { TCheckListBox }

  TRegCheckListBox = class(TCustomRegCheckListBox)
  private
  protected
  public
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegCheckListBox]);
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
