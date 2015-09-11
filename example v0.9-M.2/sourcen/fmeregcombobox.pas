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
  ExtCtrls,
  fmecustomcontrolframe,
  regcombobox,
  reglistbox;

type

  { TControlRegComobBox }

  TControlRegComobBox = class(TCustomRegControlFrame<TRegComboBox>)
    Bevel1: TBevel;
    RegComboBox1: TRegComboBox;
    RegListBox1: TRegListBox;
    ScrollBox1: TScrollBox;
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

