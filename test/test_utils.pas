unit test_utils;

{$mode delphi}

interface

uses
  SysUtils;

function _IfEmptyThen(aString: string;
                      aDefault: string): string;

implementation

function _IfEmptyThen(aString: string;
  aDefault: string): string;
begin
  if (Trim(aString) = EmptyStr) then
    Result := aDefault
  else
    Result := aString;
end;

end.

