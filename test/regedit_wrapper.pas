unit regedit_wrapper;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  regedit,
  regsourcen;

type

  { TRegEditWrapper }

  TRegEditWrapper = class
  private
    FRegEdit: TRegEdit;
  protected
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource); virtual;
  public
    constructor Create(aRegistrySource: TRegistrySource); virtual;
    destructor Destroy; override;
  public
    property RegEdit : TRegEdit
      read FRegEdit;
  end;

implementation

uses
  test_const;

{ TRegEditWrapper }

procedure TRegEditWrapper.SetRegistryEntries;
begin
  FRegEdit.RegistrySource.WriteString(SEC_TREGEDIT, IDENT_TEXT_PROPERTY,
    _TEXT_ENTRY);
end;

procedure TRegEditWrapper.SetRegistrySettings(aRegistrySource: TRegistrySource);
begin
  FRegEdit.RegistrySource := aRegistrySource;

  FRegEdit.RegistrySettings.CanRead := True;
  FRegEdit.RegistrySettings.CanWrite := True;
  FRegEdit.RegistrySettings.DoWriteAdHoc := True;
  FRegEdit.RegistrySettings.GroupIndex := 0;
  FRegEdit.RegistrySettings.DoSyncData := False;
  FRegEdit.RegistrySettings.Default := DEFAULT_TEXT_ENTRY;
  FRegEdit.RegistrySettings.Section := SEC_TREGEDIT;
  FRegEdit.RegistrySettings.Ident := IDENT_TEXT_PROPERTY;
end;

constructor TRegEditWrapper.Create(aRegistrySource: TRegistrySource);
begin
  FRegEdit := TRegEdit.Create(nil);
  SetRegistrySettings(aRegistrySource);
  SetRegistryEntries;
end;

destructor TRegEditWrapper.Destroy;
begin
  FRegEdit.RegistrySource := nil;

  if Assigned(FRegEdit) then
    FreeAndNil(FRegEdit);

  inherited Destroy;
end;

end.

