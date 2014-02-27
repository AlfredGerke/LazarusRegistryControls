unit test_wrapper;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  regsourcen;

type

  { TWrapper }

  TWrapper<_T> = class
  private
    FRegControl: _T;
  protected
    procedure SetRegistryEntries; virtual;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource); virtual;
  public
    constructor Create(aRegistrySource: TRegistrySource); virtual;
    destructor Destroy; override;
  public
    property RegControl : _T
      read FRegControl;
  end;


implementation

{ TWrapper<_T> }

procedure TWrapper<_T>.SetRegistryEntries;
begin
  //
end;

procedure TWrapper<_T>.SetRegistrySettings(aRegistrySource: TRegistrySource);
begin
  FRegControl.RegistrySource := aRegistrySource;

  RegControl.RegistrySettings.CanRead := True;
  RegControl.RegistrySettings.CanWrite := True;
  RegControl.RegistrySettings.DoWriteAdHoc := True;
  RegControl.RegistrySettings.GroupIndex := 0;
  RegControl.RegistrySettings.DoSyncData := False;
end;

constructor TWrapper<_T>.Create(aRegistrySource: TRegistrySource);
begin
  FRegControl := _T.Create(nil);
  SetRegistrySettings(aRegistrySource);
  SetRegistryEntries;
end;

destructor TWrapper<_T>.Destroy;
begin
  FRegControl.RegistrySource := nil;

  FreeAndNil(FRegControl);

  inherited Destroy;
end;

end.

