unit fmeregchecklistbox;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regchecklistbox, regcheckbox, Forms, ExtCtrls;

type

  { TControlRegCheckListBox }

  TControlRegCheckListBox = class(TCustomRegControlFrame<TRegCheckListBox>)
    Bevel1: TBevel;
    RegCheckBox1: TRegCheckBox;
    RegCheckBox2: TRegCheckBox;
    RegCheckBox3: TRegCheckBox;
    RegCheckBox4: TRegCheckBox;
    RegCheckBox5: TRegCheckBox;
    RegCheckListBox1: TRegCheckListBox;
    ScrollBox1: TScrollBox;
  protected
    procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;
  end;

implementation

{$R *.lfm}

procedure TControlRegCheckListBox._Initialize;
begin
  inherited;

  SetRegControl(RegCheckListBox1);
end;

procedure TControlRegCheckListBox.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegCheckListBox.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

end.
