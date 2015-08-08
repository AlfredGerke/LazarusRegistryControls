unit regbaseframe;

{$mode delphi}

interface

uses
  Forms, ExtCtrls;

type

  { TRegControlFrame }

  TRegControlFrame<_T> = class(TFrame)
    pnlDesktop: TPanel;
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
begin
  FRegControl.RegistrySource.ShowClientEditDialog(GetRegControlName);
end;

end.

