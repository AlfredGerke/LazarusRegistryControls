unit reglistbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  reglistbox,
  regsourcen;

type

  { TRegListBoxForTest }

  TRegListBoxForTest = class(TRegListBox)
  end;

  { TRegListBoxWrapper }

  TRegListBoxWrapper = class(TWrapperLST<TRegListBoxForTest>)
  private
    FDefault: integer;
  protected
    procedure _Initialize; override;
    procedure SetRegControl; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;

    property Default : integer
      read FDefault
      write FDefault;
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
  Ident := IDENT_TREGLISTBOX;
  Default := DEFAULT_ITEMINDEX_VALUE;
  SpecialListProperties.AddListSection(SEC_TREGLISTBOXITEMS);
end;

procedure TRegListBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  with RegControl.RegistrySource, RegControl.RegistrySettings do
  begin
    WriteString(ListSection, 'Key1', 'Value1');
    WriteString(ListSection, 'Key2', 'Value2');
    WriteString(ListSection, 'Key3', 'Value3');
    WriteString(ListSection, 'Key4', 'Value4');
    WriteString(ListSection, 'Key5', 'Value5');
    WriteInteger(Section, Ident, DEFAULT_ITEMINDEX_VALUE);
  end;
end;

procedure TRegListBoxWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource;
  aSetRegSrc: boolean = True);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);

  RegControl.RegistrySettings.Default := Default;
  RegControl.RegistrySettings.Section := Section;
  RegControl.RegistrySettings.Ident := Ident;
  RegControl.RegistrySettings.ListSection := SpecialListProperties.ListSection;
  // Dieser Anteil muss vorerst in der Ableitung implementiert werden
  //!<--
  RegControl.RegistrySettings.DoMergeData := SpecialListProperties.DoMergeData;
  RegControl.RegistrySettings.ItemsByRegistry := SpecialListProperties.ItemsByRegistry;
  RegControl.RegistrySettings.SourceKind := SpecialListProperties.SourceKind;
  //-->
end;

procedure TRegListBoxWrapper.SectionIdentDefault;
begin
  TAssert.AssertEquals('TRegListBox.RegistrySection.Section',
    Section, FRegControl.RegistrySettings.Section);
  TAssert.AssertEquals('TRegListBox.RegistrySection.Ident',
    Ident, FRegControl.RegistrySettings.Ident);
  TAssert.AssertEquals('TRegListBox.RegistrySection.Default',
    Default, FRegControl.RegistrySettings.Default);
end;

end.

