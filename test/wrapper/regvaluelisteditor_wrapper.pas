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
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;

implementation

{ TRegValueListEditorWrapper }

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

