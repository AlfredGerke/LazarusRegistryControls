unit reglistbox;

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
  TCustomRegListBox = class(TListBox)
  private
  protected
  public
  published
  end;

  TRegListBox = class(TCustomRegListBox)
  private
  protected
  public
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegListBox]);
end;

end.
