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

  TCustomRegRadioGroup = class(TRadioGroup)
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

    property RegistrySettings: TRegistrySettingsList
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

  { TRegRadioGroup }

  TRegRadioGroup = class(TCustomRegRadioGroup)
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
  Forms,
  Dialogs;

procedure Register;
begin
  RegisterComponents('Registry Controls', [TRegRadioGroup]);
end;

procedure TCustomRegRadioGroup.OnChangeSettings(aSender: TObject);
begin
  ReadFromReg;
end;

procedure TCustomRegRadioGroup.ReadWriteInfo(aRead: boolean);
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

function TCustomRegRadioGroup.GetItemsByRegistry: boolean;
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

procedure TCustomRegRadioGroup.SetRegistrySource(aRegistrySource: TRegistrySource);
begin
  FRegistrySource := aRegistrySource;
  FRegistrySettings.RookKey := FRegistrySource.RootKey;
  FRegistrySettings.RootKeyForDefaults := FRegistrySource.RootKeyForDefaults;
  FRegistrySettings.RootForDefaults := FRegistrySource.RootForDefaults;
  FRegistrySettings.ReadDefaults := FRegistrySource.ReadDefaults;
  FRegistrySettings.WriteDefaults := FRegistrySource.WriteDefaults;
  FRegistrySettings.Project:= FRegistrySource.Project;
  FRegistrySettings.Organisation := FRegistrySource.Organisation;
  FRegistrySettings.GUID := FRegistrySource.GUID;

  ReadFromReg;
end;

procedure TCustomRegRadioGroup.Click;
begin
  inherited;

  FIsModified := True;

  if FRegistrySettings.DoWriteAdHoc then
    WriteToReg;
end;

constructor TCustomRegRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIsModified := False;
  FRegistrySettings := TRegistrySettingsList.Create(Self);
  FRegistrySettings.OnChange:= OnChangeSettings;
end;

destructor TCustomRegRadioGroup.Destroy;
begin
  if Assigned(FRegistrySettings) then
    FreeAndNil(FRegistrySettings);

  inherited Destroy;
end;

procedure TCustomRegRadioGroup.AfterConstruction;
begin
  inherited;
end;

function TCustomRegRadioGroup.ReadFromReg: boolean;
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

function TCustomRegRadioGroup.WriteToReg: boolean;
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
