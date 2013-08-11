unit regradiogroup;

interface

uses
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  regsourcen,
  LResources;

const
  Read = True;
  Write = False;

type
  TSTRadioGroup = class(TRadioGroup)
  private
  protected
  public
  published
  end;

  { TRegRadioGroup }

  TRegRadioGroup = class(TSTRadioGroup)
  private
    FRegistrySource: TRegistrySource;
    FRegistrySettings: TRegistrySettingsList;
    FIsModified: boolean;

    procedure ReadWriteInfo(aRead: boolean);
    function GetItemsByRegistry: boolean;
  protected
    procedure OnChangeSettings(aSender: TObject); virtual;
    procedure SetRegistrySource(aRegistrySource: TRegistrySource); virtual;
    procedure Click; override;
  public
    procedure AfterConstruction; override;
    function ReadFromReg: boolean; virtual;
    function WriteToReg: boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsModified: boolean
      read FIsModified;
  published
    property RegistrySettings: TRegistrySettingsList
      read FRegistrySettings
      write FRegistrySettings;
    property RegistrySource: TRegistrySource
      read FRegistrySource
      write SetRegistrySource;
  end;

procedure Register;

implementation

uses
  Forms,
  Dialogs;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegRadioGroup]);
end;

procedure TRegRadioGroup.OnChangeSettings(aSender: TObject);
begin
  ReadFromReg;
end;

procedure TRegRadioGroup.ReadWriteInfo(aRead: boolean);
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
            if (FRegistrySettings.CanRead and FRegistrySettings.ItemsByRegistry) then
              GetItemsByRegistry
            else
            begin
              if (FRegistrySettings.CanRead and not FRegistrySettings.ItemsByRegistry) then
                ItemIndex := RegistrySource.ReadInteger(FRegistrySettings.RookKey,
                               FRegistrySettings.RootKeyForDefaults,
                               FRegistrySettings.RootForDefaults,
                               FRegistrySettings.Section,
                               FRegistrySettings.Ident,
                               FRegistrySettings.Default,
                               FRegistrySettings.ReadDefaults);
            end;
          end;
          Write:
          begin
            if (FRegistrySettings.CanWrite and FIsModified) then
              RegistrySource.WriteInteger(FRegistrySettings.RookKey,
                FRegistrySettings.RootKeyForDefaults,
                FRegistrySettings.RootForDefaults,
                FRegistrySettings.Section,
                FRegistrySettings.Ident,
                ItemIndex,
                FRegistrySettings.WriteDefaults);
          end;
        end;
      end;
    end;
  end;
end;

function TRegRadioGroup.GetItemsByRegistry: boolean;
var
  list: TStrings;
begin
  Result := False;
  try
    try
      list := TStringList.Create;

      if assigned(Items) then
      begin
        list.Clear;
        Items.Clear;
      end;

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
            RegistrySource.ReadSection(FRegistrySettings.RookKey,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.RootForDefaults,
              FRegistrySettings.ListSection,
              list,
              FRegistrySettings.ReadDefaults);
            Items.AddStrings(list);
            ItemIndex := RegistrySource.ReadInteger(FRegistrySettings.RookKey,
                           FRegistrySettings.RootKeyForDefaults,
                           FRegistrySettings.RootForDefaults,
                           FRegistrySettings.Section,
                           FRegistrySettings.Ident,
                           FRegistrySettings.Default,
                           FRegistrySettings.ReadDefaults);
          end;
        end;
      end;

      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    list.Free;
  end;
end;

procedure TRegRadioGroup.SetRegistrySource(aRegistrySource: TRegistrySource);
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

procedure TRegRadioGroup.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

constructor TRegRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsList.Create;
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TRegRadioGroup.Destroy;
begin
  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TRegRadioGroup.AfterConstruction;
begin
  inherited;
end;

function TRegRadioGroup.ReadFromReg: boolean;
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

function TRegRadioGroup.WriteToReg: boolean;
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
