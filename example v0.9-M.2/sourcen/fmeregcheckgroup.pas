unit fmeregcheckgroup;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regcheckgroup;

type

  { TControlRegCheckGroup }

  TControlRegCheckGroup = class(TCustomRegControlFrame<TRegCheckGroup>)
    RegCheckGroup1: TRegCheckGroup;
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

