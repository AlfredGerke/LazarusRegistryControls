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
    { TODO -oAlfred Gerke -cListen-RegControls : Für TRegValueListEditor Set-/GetListSourceKind nicht implementieren }
    function ClearItems: boolean;
    function DeleteItem: boolean;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

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

function TControlRegValueListEditor.ClearItems: boolean;
begin
  Result := RegControl.ClearItems(True, 'Einträge löschen?');
end;

function TControlRegValueListEditor.DeleteItem: boolean;
begin
  Result := RegControl.DeleteItem(-1, True, 'Eintrag löschen?');
end;

end.

