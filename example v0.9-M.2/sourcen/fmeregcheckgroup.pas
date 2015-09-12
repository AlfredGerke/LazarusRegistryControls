unit fmeregcheckgroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regcheckgroup, regcheckbox, Forms, ExtCtrls;

type

  { TControlRegCheckGroup }

  TControlRegCheckGroup = class(TCustomRegControlFrame<TRegCheckGroup>)
    Bevel1: TBevel;
    RegCheckBox1: TRegCheckBox;
    RegCheckBox2: TRegCheckBox;
    RegCheckBox3: TRegCheckBox;
    RegCheckBox4: TRegCheckBox;
    RegCheckBox5: TRegCheckBox;
    RegCheckGroup1: TRegCheckGroup;
    ScrollBox1: TScrollBox;
  protected
    procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;
  end;

implementation

{$R *.lfm}

procedure TControlRegCheckGroup._Initialize;
begin
  inherited;

  SetRegControl(RegCheckGroup1);
end;

procedure TControlRegCheckGroup.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegCheckGroup.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

end.

