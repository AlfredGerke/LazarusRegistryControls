unit regcheckgroup;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  regconst,
  LMessages,
  regmsg;

type

  { TCustomRegCheckGroup }

  TCustomRegCheckGroup = class(TCheckGroup)
  private
  protected
  public
  published
  end;

  { TCheckGroup }

  TRegCheckGroup = class(TCustomRegCheckGroup)
  private
  protected
  public
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegCheckGroup]);
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
