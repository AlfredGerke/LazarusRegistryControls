unit fmereglistbox;

{$mode Delphi}{$H+}

interface

uses
  fmecustomcontrolframe,
  reglistbox;

type

  { TControlRegListBox }

  TControlRegListBox = class(TCustomRegControlFrame<TRegListBox>)
    RegListBox1: TRegListBox;
  protected
    procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;
  end;

implementation

{$R *.lfm}

procedure TControlRegListBox._Initialize;
begin
  inherited;

  SetRegControl(RegListBox1);
end;

procedure TControlRegListBox.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegListBox.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;

end.

