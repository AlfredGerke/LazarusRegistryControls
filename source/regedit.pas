unit regedit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  regsourcen,
  LResources;

const
  Read = True;
  Write = False;

type
  TSTEdit = class(TEdit)
  private
  protected
  public
  published
  end;

  { TRegEdit }

  TRegEdit = class(TSTEdit)
  private
    //Registrykomponente
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsStringDefault;

    procedure ReadWriteInfo(aRead: boolean);
  protected
    procedure OnChangeSettings(aSender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource);
    procedure Change; override;
  public
    procedure AfterConstruction; override;
    function ReadFromReg: boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RegistrySettings: TRegistrySettingsStringDefault
      read FRegistrySettings
      write FRegistrySettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
  end;

procedure Register;

implementation

uses
  Forms;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegEdit]);
end;

procedure TRegEdit.OnChangeSettings(aSender: TObject);
begin
  ReadFromReg;
end;

procedure TRegEdit.ReadWriteInfo(aRead: boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if assigned(RegistrySource) then
    begin
      if ((FRegistrySettings.RookKey <> '') and
        (FRegistrySettings.RootKeyForDefaults <> '') and
        (FRegistrySettings.RootForDefaults <> '') and
        (FRegistrySettings.Section <> '') and
        (FRegistrySettings.Ident <> '')) then
      begin
        case aRead of
          Read:
          begin
            if FRegistrySettings.CanRead then
              Text := RegistrySource.ReadString(FRegistrySettings.RookKey,
                        FRegistrySettings.RootKeyForDefaults,
                        FRegistrySettings.RootForDefaults,
                        FRegistrySettings.Section,
                        FRegistrySettings.Ident,
                        FRegistrySettings.Default,
                        FRegistrySettings.ReadDefaults);
          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and Modified) then
              RegistrySource.WriteString(FRegistrySettings.RookKey,
                FRegistrySettings.RootKeyForDefaults,
                FRegistrySettings.RootForDefaults,
                FRegistrySettings.Section,
                FRegistrySettings.Ident,
                Text,
                FRegistrySettings.WriteDefaults);
          end;
        end;
      end;
    end;
  end;
end;

procedure TRegEdit.SetRegistrySource(aRegistrySource: TRegistrySource);
begin
  FRegistrySource := aRegistrySource;
  FRegistrySettings.RookKey := FRegistrySource.RootKey;
  FRegistrySettings.RootKeyForDefaults := FRegistrySource.RootKeyForDefaults;
  FRegistrySettings.RootForDefaults := FRegistrySource.RootForDefaults;
  FRegistrySettings.ReadDefaults := FRegistrySource.ReadDefaults;
  FRegistrySettings.WriteDefaults := FRegistrySource.WriteDefaults;
  FRegistrySettings.CanRead := FRegistrySource.ReadDefaults;
  FRegistrySettings.CanWrite := FRegistrySource.WriteDefaults;
  FRegistrySettings.Project:= FRegistrySource.Project;
  if FRegistrySource.UseGUID then
    FRegistrySettings.GUID := FRegistrySource.GUID
  else
    FRegistrySettings.GUID := EmptyStr;

  ReadFromReg;
end;

procedure TRegEdit.Change;
begin
  inherited;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

constructor TRegEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRegistrySettings := TRegistrySettingsStringDefault.Create;
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TRegEdit.Destroy;
begin
  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TRegEdit.AfterConstruction;
begin
  inherited;

end;

function TRegEdit.ReadFromReg: boolean;
begin
  Result := False;

  if not Assigned(FRegistrySource) then
    Exit;

  try
    ReadWriteInfo(Read);
    Application.ProcessMessages;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TRegEdit.WriteToReg: boolean;
begin
  Result := False;

  if not Assigned(FRegistrySource) then
    Exit;

  try
    ReadWriteInfo(Write);
    Application.ProcessMessages;
    Modified := True;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
