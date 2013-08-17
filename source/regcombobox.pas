unit regcombobox;

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
  StdCtrls,
  regconst,
  LMessages,
  regmsg;

type

  { TCustomRegComboBox }

  TCustomRegComboBox = class(TComboBox)
  private
  protected
  public
  published
  end;

  { TRegComboBox }

  TRegComboBox = class(TCustomRegComboBox)
  private
  protected
  public
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegComboBox]);
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
