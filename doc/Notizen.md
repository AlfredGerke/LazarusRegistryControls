Notizen
=======

CheckRTLAnsi wird im ersten Anlauf nicht angpasst.
CheckRTLAnsi wird eventl. druch Gebietsschema ersetzt werden.

//==================================================================================================
Unit: regutils.pas

//==================================================================================================
Unit: regconvutils.pas
Methoden: alle
Anmerkung: diese Unit verliert komplett ihre Bedeutung, eventuell wird sie umgestellt für Zugriffe auf Codepages 
           (s. Unit: LConvEncoding.pas)
//==================================================================================================
Unit: regbasics.pas
Methoden:
//--------------------------------------------------------------------------------------------------
function TLRCRegIniFile.ReadSectionValuesByKindProc(aReg: TRegistry;
  aOpenKey: string): boolean;

  procedure AddString(aStrings: TStrings;
                      aValue: string;
                      aMerge: boolean);
  var
    index: Integer;
    value_name: string;
    pos_index: SizeInt;
  begin
     if aMerge then
     begin
       pos_index := Pos('=', aValue);
       if (pos_index = 0) then
         index := aStrings.IndexOf(aValue)
       else
       begin
         value_name := Copy(aValue, 1, pos_index-1);
         index := aStrings.IndexOfName(value_name);
       end;

       if (index = -1) then
         aStrings.Add(aValue);
     end
     else
       aStrings.Add(aValue);
  end;

var
  current_list: TStrings;
  new_list: TStrings;
  check_rtl_ansi: boolean;
  do_merge: Boolean;
  list_source_kind: TListSourceKind;
  anz: integer;
  value: string;
  value_name: string;
  value_name_utf8_decoded: string;
  value_data_type: TRegDataType;
begin
  Result := False;

  with FReg do
  begin
    current_list := GetStrings;
    check_rtl_ansi := CheckRTLAnsi;
    do_merge := Merge;
    list_source_kind := ListSourceKind;
  end;

  if Assigned(current_list) then
  begin
    if not do_merge then
      current_list.Clear;

    new_list := TStringList.Create;
    try
      with aReg do
      begin
        GetValueNames(new_list);

        // Aus dieser Liste werden die Idents entnommen
        // !: Liste vorerst immer umwandeln
        SysToUTF8StringsIfNeeded(new_list, check_rtl_ansi);

        for anz := 0 to new_list.Count-1 do
	begin
          value_name := new_list.Strings[anz];

          value_name_utf8_decoded :=
            UTF8ToSysIfNeeded(value_name, check_rtl_ansi);

          value_data_type := GetDataType(value_name_utf8_decoded);

          case value_data_type of
            rdString,
            rdExpandString:
            begin
              value := ReadString(value_name_utf8_decoded);

              if check_rtl_ansi then
                if NeedRTLAnsi then
                  value := SysToUTF8(value);

            end;
            rdBinary:
              Continue;
            rdInteger:
              value := IntToStr(ReadInteger(value_name));
          else
            Continue;
          end;

          case list_source_kind of
            lskByValue: AddString(current_list, value, do_merge);
            lskByKey: AddString(current_list, value_name, do_merge);
            lskByKeyValue: AddString(current_list, value_name + '=' + value, do_merge);
          else
            Break;
          end;

          Result := True;
	end;

      end;
    finally
      if Assigned(new_list) then
        FreeAndNil(new_list);
    end;
  end;
end;
//--------------------------------------------------------------------------------------------------