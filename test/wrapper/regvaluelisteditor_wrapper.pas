unit regvaluelisteditor_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  regvaluelisteditor,
  regsourcen;

type

  { TRegValueListEditorWrapper }

  TRegValueListEditorWrapper = class(TWrapperLST<TRegValueListEditor>)
  private
  protected
    procedure SetRegControl; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;

implementation

uses
  test_const;

{ TRegValueListEditorWrapper }

procedure TRegValueListEditorWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGVALUELISTEDITOR_NAME);
end;

procedure TRegValueListEditorWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegValueListEditorWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

