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

    procedure SetCanRead(aValue: boolean);
    procedure SetCanWrite(aValue: boolean);
    procedure SetDoWriteAdHoc(aValue: boolean);
    procedure SetDoSyncData(aValue: boolean);
    procedure SetGroupIndex(aValue: integer);

    function GetCanRead: boolean;
    function GetCanWrite: boolean;
    function GetDoWriteAdHoc: boolean;
    function GetDoSyncData: boolean;
    function GetGroupIndex: integer;

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

procedure TCustomRegControlFrame<_T>.SetCanRead(aValue: boolean);
begin
  FRegControl.RegistrySettings.CanRead := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetCanWrite(aValue: boolean);
begin
  FRegControl.RegistrySettings.CanWrite := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetDoWriteAdHoc(aValue: boolean);
begin
  FRegControl.RegistrySettings.DoWriteAdHoc := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetDoSyncData(aValue: boolean);
begin
  FRegControl.RegistrySettings.DoSyncData := aValue;
end;

procedure TCustomRegControlFrame<_T>.SetGroupIndex(aValue: integer);
begin
 FRegControl.RegistrySettings.GroupIndex := aValue;
end;

function TCustomRegControlFrame<_T>.GetCanRead: boolean;
begin
  Result := FRegControl.RegistrySettings.CanRead;
end;

function TCustomRegControlFrame<_T>.GetCanWrite: boolean;
begin
  Result := FRegControl.RegistrySettings.CanWrite;
end;

function TCustomRegControlFrame<_T>.GetDoWriteAdHoc: boolean;
begin
  Result := FRegControl.RegistrySettings.DoWriteAdHoc;
end;

function TCustomRegControlFrame<_T>.GetDoSyncData: boolean;
begin
  Result := FRegControl.RegistrySettings.DoSyncData;
end;

function TCustomRegControlFrame<_T>.GetGroupIndex: integer;
begin
  Result := FRegControl.RegistrySettings.GroupIndex;
end;

end.

