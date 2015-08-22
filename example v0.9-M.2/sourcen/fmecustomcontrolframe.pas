unit fmecustomcontrolframe;

{$mode Delphi}{$H+}

interface

uses
  Forms,
  Classes;

type

  { TCustomRegControlFrame }

  TCustomRegControlFrame<_T> = class(TFrame)
  private
    FRegControl: _T;
  protected
    procedure _Initialize; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    function GetRegControlName: string;
    procedure SetRegControl(aControl: _T);
    procedure GetRootKeys(aEdit: boolean);

    property RegControl: _T
      read FRegControl;
  end;

implementation

{$R *.lfm}

{ TCustomRegControlFrame<_T> }

procedure TCustomRegControlFrame<_T>._Initialize;
begin
  // In Ableitung überschreiben
end;

constructor TCustomRegControlFrame<_T>.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  _Initialize;
end;

function TCustomRegControlFrame<_T>.GetRegControlName: string;
begin
  Result := FRegControl.Name;
end;

procedure TCustomRegControlFrame<_T>.SetRegControl(aControl: _T);
begin
  FRegControl := aControl;
end;

procedure TCustomRegControlFrame<_T>.GetRootKeys(aEdit: boolean);
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

