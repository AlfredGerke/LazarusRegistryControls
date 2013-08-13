unit regcheckbox;

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

  { TCustomRegCheckBox }
  TCustomRegCheckBox = class(TCheckBox)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsBooleanDefault;
    FIsModified: boolean;

    procedure ReadWriteInfo(aRead: boolean);
  protected
    procedure OnChangeSettings(aSender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    procedure Click; override;

    property RegistrySettings: TRegistrySettingsBooleanDefault
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

    property IsModified: boolean
      read FIsModified;
  published
  end;

  { TRegCheckBox }
  TRegCheckBox = class(TCustomRegCheckBox)
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
  Forms;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegCheckBox]);
end;

procedure TCustomRegCheckBox.OnChangeSettings(aSender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegCheckBox.ReadWriteInfo(aRead: boolean);
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
              Checked := RegistrySource.ReadBool(FRegistrySettings.RookKey,
                           FRegistrySettings.RootKeyForDefaults,
                           FRegistrySettings.RootForDefaults,
                           FRegistrySettings.Section,
                           FRegistrySettings.Ident,
                           FRegistrySettings.Default,
                           FRegistrySettings.ReadDefaults);

          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and FIsModified) then
              RegistrySource.WriteBool(FRegistrySettings.RookKey,
                FRegistrySettings.RootKeyForDefaults,
                FRegistrySettings.RootForDefaults,
                FRegistrySettings.Section,
                FRegistrySettings.Ident,
                Checked,
                FRegistrySettings.WriteDefaults);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomRegCheckBox.SetRegistrySource(aRegistrySource: TRegistrySource);
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

procedure TCustomRegCheckBox.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

constructor TCustomRegCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsBooleanDefault.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TCustomRegCheckBox.Destroy;
begin
  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TCustomRegCheckBox.AfterConstruction;
begin
  inherited;
end;

function TCustomRegCheckBox.ReadFromReg: boolean;
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

function TCustomRegCheckBox.WriteToReg: boolean;
begin
  Result := False;

  if not Assigned(FRegistrySource) then
    Exit;

  try
    ReadWriteInfo(Write);
    Application.ProcessMessages;

    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

initialization
  {$I ..\package\registrycontrols.lrs}

end.
