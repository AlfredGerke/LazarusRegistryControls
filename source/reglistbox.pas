unit reglistbox;

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

  { TCustomRegListBox }

  TCustomRegListBox = class(TListBox)
  private
  protected
  public
  published
  end;

  { TRegListBox }

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

initialization
  {$I ..\package\registrycontrols.lrs}

end.
