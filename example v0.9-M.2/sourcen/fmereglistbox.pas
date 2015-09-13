unit fmereglistbox;

{$mode Delphi}{$H+}

interface

uses
  fmecustomcontrolframe,
  reglistbox,
  regradiogroup,
  ExtCtrls;

type

  { TControlRegListBox }

  TControlRegListBox = class(TCustomRegControlFrame<TRegListBox>)
    RegListBox1: TRegListBox;
    RegRadioGroup1: TRegRadioGroup;
    Splitter1: TSplitter;
  protected
    procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;

    { TODO -oAlfred Gerke -cListen-RegControls : Spezielle Testmethoden für Listen-RegControls implementieren }
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

