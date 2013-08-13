unit regcombobox;

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
  StdCtrls;

type
  TCustomRegComboBox = class(TComboBox)
  private
  protected
  public
  published
  end;

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

end.
