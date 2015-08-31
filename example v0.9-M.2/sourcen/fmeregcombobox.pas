unit fmeregcombobox;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  fmecustomcontrolframe,
  regcombobox;

type

  { TControlRegComobBox }

  TControlRegComobBox = class(TCustomRegControlFrame<TRegComboBox>)
    RegComboBox1: TRegComboBox;
  protected
    procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;
  end;

implementation

{$R *.lfm}

procedure TControlRegComobBox._Initialize;
begin
  inherited;

  SetRegControl(RegComboBox1);
end;

procedure TControlRegComobBox.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegComobBox.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

end.

