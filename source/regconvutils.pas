unit regconvutils;

{$mode Delphi}{$H+}

interface

uses
  classes;

type
  { TStrConvertTarget }

  TStrConvertTarget = (sctUnknown, sctToUTF8, sctToAnsi);

function UTF8ToSysIfNeeded(aString: string;
                           aCheckRTLAnsi: boolean): string;

function SysToUTF8IfNeeded(aString: string;
                           aCheckRTLAnsi: boolean): string;

procedure SysToUTF8StringsIfNeeded(aStrings: TStrings;
                                   aCheckRTLAnsi: boolean);

procedure UTF8ToSysStringsIfNeeded(aStrings: TStrings;
                                   aCheckRTLAnsi: boolean);

function UTF8DecodeIfNeeded(aString: string;
                            aCheckRTLAnsi: boolean): string;

function UTF8EncodeIfNeeded(aString: string;
                            aCheckRTLAnsi: boolean): string;

implementation

uses
  LazUTF8,
  SysUtils;

function ConvertString(aString: string;
  aCheckRTLAnsi: boolean;
  aTarget: TStrConvertTarget): string;
begin
  if (aCheckRTLAnsi and NeedRTLAnsi) then
  begin
    case aTarget of
      sctToUTF8:
        Result := SysToUTF8(aString);
      sctToAnsi:
        Result := UTF8ToSys(aString);
    else
      Result := aString;
    end;
  end
  else
    Result := aString;
end;

function SysToUTF8IfNeeded(aString: string;
  aCheckRTLAnsi: boolean): string;
begin
  Result := ConvertString(aString, aCheckRTLAnsi, sctToUTF8);
end;

function UTF8ToSysIfNeeded(aString: string;
  aCheckRTLAnsi: boolean): string;
begin
  Result := ConvertString(aString, aCheckRTLAnsi, sctToAnsi);
end;

procedure ConvertStrings(aStrings: TStrings;
  aCheckRTLAnsi: boolean;
  aTarget: TStrConvertTarget);
var
  anz: integer;
  list: TStrings;
  item: string;
begin
  if (aCheckRTLAnsi and NeedRTLAnsi) then
  begin
    list := TStringList.Create;
    try
      list.AddStrings(aStrings);
      aStrings.Clear;
      for anz := 0 to list.count-1 do
      begin
        case aTarget of
          sctToUTF8:
            item := SysToUTF8(list.strings[anz]);
          sctToAnsi:
            item := UTF8ToSys(list.strings[anz]);
        else
          item := list.strings[anz];
        end;

        aStrings.add(item);
      end;
    finally
      if Assigned(list) then
        FreeAndNil(list);
    end;
  end;
end;

procedure SysToUTF8StringsIfNeeded(aStrings: TStrings;
  aCheckRTLAnsi: boolean);
begin
  ConvertStrings(aStrings, aCheckRTLAnsi, sctToUTF8);
end;

procedure UTF8ToSysStringsIfNeeded(aStrings: TStrings;
  aCheckRTLAnsi: boolean);
begin
  ConvertStrings(aStrings, aCheckRTLAnsi, sctToAnsi);
end;

function UTF8DecodeIfNeeded(aString: string;
  aCheckRTLAnsi: boolean): string;
begin
  if (aCheckRTLAnsi and NeedRTLAnsi) then
    Result := UTF8Decode(aString)
  else
    Result := aString;
end;

function UTF8EncodeIfNeeded(aString: string; aCheckRTLAnsi: boolean): string;
begin
  if (aCheckRTLAnsi and NeedRTLAnsi) then
    Result := UTF8Encode(aString)
  else
    Result := aString;
end;

end.

