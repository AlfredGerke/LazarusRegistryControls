unit regbaseform;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs;

type

  TRegBaseForm = class(TForm)
  private
    FAtDesignTime: boolean;
  protected
    function SetUTF8IfNeeded(const aString: string): string; virtual;
  public
    property AtDesignTime: boolean
      read FAtDesignTime
      write FAtDesignTime;
  end;

implementation

{$R *.lfm}

function TRegBaseForm.SetUTF8IfNeeded(const aString: string): string;
begin
  if FAtDesignTime then
    Result := SysToUTF8(aString)
  else
    Result := aString;
end;

end.
