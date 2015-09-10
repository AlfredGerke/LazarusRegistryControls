unit fmeregradiogroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regradiogroup, regcheckbox, regchecklistbox, ExtCtrls;

type

  { TControlRegRadioGroup }

  TControlRegRadioGroup = class(TCustomRegControlFrame<TRegRadioGroup>)
    RegRadioGroup1: TRegRadioGroup;
  protected
     procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;
  end;

implementation

{$R *.lfm}

procedure TControlRegRadioGroup._Initialize;
begin
  inherited;

  SetRegControl(RegRadioGroup1);
end;

procedure TControlRegRadioGroup.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegRadioGroup.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

end.

