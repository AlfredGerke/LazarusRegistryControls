unit reglabel;

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
  StdCtrls;

type

  {TCustomRegLabel}

  TCustomRegLabel = class(TLabel)
  private
  protected
  public
  published
  end;

  {TRegLabel}

  TRegLabel = class(TCustomRegLabel)
  private
  protected
  public
  published
  end;


procedure Register;

implementation

uses
  regpropedits,
  ComponentEditors;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegLabel]);
  RegisterComponentEditor(TRegLabel, TRegistryControlComponentEditor);
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
