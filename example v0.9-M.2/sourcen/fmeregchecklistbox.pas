unit fmeregchecklistbox;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regchecklistbox;

type

  { TControlRegCheckListBox }

  TControlRegCheckListBox = class(TCustomRegControlFrame<TRegCheckListBox>)
    RegCheckListBox1: TRegCheckListBox;
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
