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
  public
    property AtDesignTime: boolean
      read FAtDesignTime
      write FAtDesignTime;
  end;

implementation

{$R *.lfm}

end.
