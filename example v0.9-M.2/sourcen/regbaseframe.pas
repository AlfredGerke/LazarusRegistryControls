unit regbaseframe;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  ExtCtrls;

type

  { TRegControlFrame }

  TRegControlFrame<_T> = class(TFrame)
  private
    FRegControl: _T;
  public
    function GetRegControlName: string;
    procedure SetRegControl(aControl: _T);
    procedure GetRootKeys(aEdit: boolean);
  end;

implementation

{$R *.lfm}

{ TRegControlFrame<_T> }

function TRegControlFrame<_T>.GetRegControlName: string;
begin
  Result := FRegControl.Name;
end;

procedure TRegControlFrame<_T>.SetRegControl(aControl: _T);
begin
  FRegControl := aControl;
end;

procedure TRegControlFrame<_T>.GetRootKeys(aEdit: boolean);
var
  curr_edit: boolean;
begin
  curr_edit := FRegControl.RegistrySource.EditClientRootKeys;
  try
    if (curr_edit <> aEdit) then
      FRegControl.RegistrySource.EditClientRootKeys := aEdit;

    FRegControl.RegistrySource.ShowClientEditDialog(GetRegControlName);
  finally
    if (curr_edit <> aEdit) then
      FRegControl.RegistrySource.EditClientRootKeys := curr_edit;
  end;
end;

end.

