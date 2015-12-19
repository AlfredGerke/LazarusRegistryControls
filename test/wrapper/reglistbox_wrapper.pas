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
    function GetItemsCount: integer;
    function GetDoSyncData: boolean;

    procedure _Initialize; override;
    procedure SetSectionsAndIdents; virtual;
    procedure SetRegistryEntries; override;
    procedure SetRegControl; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;

    property Default : integer
      read FDefault
      write FDefault;
  public
    procedure SectionIdentDefault;

    function GetItemByIndex(aIndex: integer): string;
    function GetIndexOfItem(aItem: string): integer;
    function DeleteItem(aIndex: integer): boolean;

    property ItemsCount: integer
      read GetItemsCount;

    property DoSyncData: boolean
      read GetDoSyncData;
  end;

  { TRegListBoxWrapperUTF8 }

  TRegListBoxWrapperUTF8 = class(TRegListBoxWrapper)
  private
  protected
    procedure SetSectionsAndIdents; override;
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  end;

  { TRegListBoxWrapperDeleteItem }

  TRegListBoxWrapperDeleteItem = class(TRegListBoxWrapper)
  private
  protected
  public
  end;


implementation

uses
  test_const,
  fpcunit,
  SysUtils;

{ TRegListBoxWrapperUTF8 }

procedure TRegListBoxWrapperUTF8.SetSectionsAndIdents;
begin
  inherited SetSectionsAndIdents;
end;

procedure TRegListBoxWrapperUTF8.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegListBoxWrapperUTF8.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

{ TRegListBoxWrapper }

function TRegListBoxWrapper.GetItemsCount: integer;
begin
  Result := RegControl.Items.Count;
end;

function TRegListBoxWrapper.GetDoSyncData: boolean;
begin
  Result := RegControl.RegistrySettings.DoSyncData;
end;

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

  with RegControl, RegControl.RegistrySource, RegControl.RegistrySettings do
  begin
    WriteString(ListSection, 'Key1', 'Value1');
    WriteString(ListSection, 'Key2', 'Value2');
    WriteString(ListSection, 'Key3', 'Value3');
    WriteString(ListSection, 'Key4', 'Value4');
    WriteString(ListSection, 'Key5', 'Value5');
    WriteInteger(Section, Ident, DEFAULT_ITEMINDEX_VALUE);

    ReadFromReg;
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

function TRegListBoxWrapper.GetItemByIndex(aIndex: integer): string;
begin
  Result := EmptyStr;
  if (aIndex > -1) then
    if (aIndex < ItemsCount) then
      Result := RegControl.Items.Strings[aIndex];
end;

function TRegListBoxWrapper.GetIndexOfItem(aItem: string): integer;
begin
   Result := RegControl.Items.IndexOf(aItem);
end;

function TRegListBoxWrapper.DeleteItem(aIndex: integer): boolean;
begin
  Result := RegControl.DeleteItem(aIndex, False, EmptyStr);
end;

end.

