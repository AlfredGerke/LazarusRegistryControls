unit datRegistry;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  regsourcen,
  regtype;

type

  { TRegistrySourceModule }

  TRegistrySourceModule = class(TDataModule)
    rsRegistrySource: TRegistrySource;
  private
  public
    procedure DeleteListItemByKeyValue(aSection: string;
                                       aKeyValue: string);
    procedure DeleteListItemByValue(aSection: string;
                                    aValue: string);
    procedure DeleteListItemsByKey(aSection: string;
                                   aKey: string);
    procedure DeleteListItem(aSection: string;
                             aIdent: string); overload;
    procedure DeleteListItem(aSection: string;
                             aListValue: string;
                             aListSourceKind: TListSourceKind); overload;
  end;

var
  RegistrySourceModule: TRegistrySourceModule;

implementation

{$R *.lfm}

uses
  SysUtils,
  Dialogs;

{ TRegistrySourceModule }

procedure TRegistrySourceModule.DeleteListItemByKeyValue(aSection: string;
  aKeyValue: string);
begin
  aKeyValue := Copy(aKeyValue, 1, pos('=', aKeyValue)-1);
  DeleteListItemsByKey(aSection, aKeyValue);
end;

procedure TRegistrySourceModule.DeleteListItemByValue(aSection: string;
  aValue: string);
var
  number_str: string;
  number: integer;
begin
  number_str := Copy(aValue, 6, 1);
  if TryStrToInt(number_str, number) then
  begin
    aValue := Format('Key%d', [number]);
    DeleteListItemsByKey(aSection, aValue);
  end
  else
    MessageDlg(Format('Kein gültiger Index aus %s ermittelt [%s]!',
      [aValue, number_str]), mtWarning, [mbOK], 0);
end;

procedure TRegistrySourceModule.DeleteListItemsByKey(aSection: string;
  aKey: string);
begin
  DeleteListItem(aSection, aKey);
end;

procedure TRegistrySourceModule.DeleteListItem(aSection: string;
  aIdent: string);
begin
  rsRegistrySource.DeleteKey(aSection, aIdent);
end;

procedure TRegistrySourceModule.DeleteListItem(aSection: string;
  aListValue: string;
  aListSourceKind: TListSourceKind);
begin
  case AListSourceKind of
    lskUnknown:
      MessageDlg('Unbekannte Quelle für Listdaten gewählt!',
        mtWarning, [mbOK], 0);
    lskByKey:
      DeleteListItemsByKey(aSection, aListValue);
    lskByValue:
      DeleteListItemByValue(aSection, aListValue);
    lskByKeyValue:
      DeleteListItemByKeyValue(aSection, aListValue);
  end;
end;

end.

