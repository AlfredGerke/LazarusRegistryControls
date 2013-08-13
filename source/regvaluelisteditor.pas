unit regvaluelisteditor;

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
  ValEdit;

type
  TCustomRegValueListEditor = class(TValueListEditor)
  private
  protected
  public
  published
  end;

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

end.
