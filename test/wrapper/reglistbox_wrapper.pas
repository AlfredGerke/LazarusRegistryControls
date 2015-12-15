unit reglistbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  reglistbox,
  regsourcen;

type

  { TRegListBoxWrapper }

  TRegListBoxWrapper = class(TWrapperLST<TRegListBox>)
  private
  protected
    procedure _Initialize; override;
    procedure SetRegControl; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
    procedure SectionIdentDefault;
  end;


implementation

uses
  test_const,
  fpcunit;

{ TRegListBoxWrapper }

procedure TRegListBoxWrapper._Initialize;
begin
  inherited _Initialize;

  SetSectionsAndIdents;
end;

procedure TRegListBoxWrapper.SetRegControl;
begin
  inherited SetRegControl;

  RegControl.Name := SetUniqueName(TREGLISTBOX_NAME);
end;

procedure TRegListBoxWrapper.SetSectionsAndIdents;
begin
  Section := SEC_TREGLISTBOX;
  //Default := DEFAULT_ITEMINDEX_VALUE;
end;

procedure TRegListBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegListBoxWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

procedure TRegListBoxWrapper.SectionIdentDefault;
begin
  //TAssert.AssertEquals('TRegListBox.RegistrySection.Section',
  //    Section, FRegControl.RegistrySettings.Section);
  //TAssert.AssertEquals('TRegListBox.RegistrySection.Default',
  //    Default, FRegControl.RegistrySettings.Default);
end;

end.

