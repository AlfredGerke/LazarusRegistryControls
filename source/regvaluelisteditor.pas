unit regvaluelisteditor;

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
  ValEdit,
  regconst,
  LMessages,
  regmsg;

type

  { TCustomRegValueListEditor }

  TCustomRegValueListEditor = class(TValueListEditor)
  private
  protected
  public
  published
  end;

  { TRegValueListEditor }

  TRegValueListEditor = class(TCustomRegValueListEditor)
  private
  protected
  public
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegValueListEditor]);
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
