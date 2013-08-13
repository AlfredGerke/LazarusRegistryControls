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

  { TCustomRegEdit }
  TCustomRegEdit = class(TEdit)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsStringDefault;

    procedure ReadWriteInfo(aRead: boolean);
  protected
    procedure OnChangeSettings(aSender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource);
    procedure Change; override;

    property RegistrySettings: TRegistrySettingsStringDefault
      read FRegistrySettings
      write FRegistrySettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
  public
    procedure AfterConstruction; override;
    function ReadFromReg: boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TRegEdit }
  TRegEdit = class(TCustomRegEdit)
  private
  protected
  public
  published
    property RegistrySettings;
    property RegistrySource;
  end;

procedure Register;

implementation

uses
  Forms, dialogs;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegEdit]);
end;

{ TCustomRegEdit }
procedure TCustomRegEdit.OnChangeSettings(aSender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegEdit.ReadWriteInfo(aRead: boolean);
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

procedure TCustomRegEdit.SetRegistrySource(aRegistrySource: TRegistrySource);
begin
  FRegistrySource := aRegistrySource;
  FRegistrySettings.RookKey := FRegistrySource.RootKey;
  FRegistrySettings.RootKeyForDefaults := FRegistrySource.RootKeyForDefaults;
  FRegistrySettings.RootForDefaults := FRegistrySource.RootForDefaults;
  FRegistrySettings.Project:= FRegistrySource.Project;
  FRegistrySettings.Organisation := FRegistrySource.Organisation;
  FRegistrySettings.GUID := FRegistrySource.GUID;
  if (csDesigning in ComponentState) then
  begin
    FRegistrySettings.ReadDefaults := FRegistrySource.ReadDefaults;
    FRegistrySettings.WriteDefaults := FRegistrySource.WriteDefaults;
  end;

  ReadFromReg;
end;

procedure TCustomRegEdit.Change;
begin
  inherited;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

constructor TCustomRegEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FRegistrySettings := TRegistrySettingsStringDefault.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TCustomRegEdit.Destroy;
begin
  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TCustomRegEdit.AfterConstruction;
begin
  inherited;

end;

function TCustomRegEdit.ReadFromReg: boolean;
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

function TCustomRegEdit.WriteToReg: boolean;
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
