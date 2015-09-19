unit fmeregvaluelisteditor;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regvaluelisteditor;

type

  { TControlRegValueListEditor }

  TControlRegValueListEditor = class(TCustomRegControlFrame<TRegValueListEditor>)
    RegValueListEditor1: TRegValueListEditor;
  protected
    procedure _Initialize; override;
  public
    procedure SetDoMergeData(aValue: boolean);
    function GetDoMergeData: boolean;

    { TODO -oAlfred Gerke -cListen-RegControls : Spezielle Testmethoden für Listen-RegControls implementieren }
    { TODO -oAlfred Gerke -cListen-RegControls : Für TRegValueListEditor Set-/GetItemsByRegistry nicht implementieren }
  end;

implementation

{$R *.lfm}

{ TControlRegValueListEditor }

procedure TControlRegValueListEditor._Initialize;
begin
  inherited;

  SetRegControl(RegValueListEditor1);
end;

procedure TControlRegValueListEditor.SetDoMergeData(aValue: boolean);
begin
  RegControl.RegistrySettings.DoMergeData := aValue;
end;

function TControlRegValueListEditor.GetDoMergeData: boolean;
begin
  Result := RegControl.RegistrySettings.DoMergeData;
end;


end.

