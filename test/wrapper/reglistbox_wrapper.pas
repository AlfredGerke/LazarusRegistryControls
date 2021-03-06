unit reglistbox_wrapper;

{$mode Delphi}{$H+}

interface

uses
  test_wrapper,
  reglistbox,
  regsourcen,
  Classes;

type

  { TRegListBoxForTest }

  TRegListBoxForTest = class(TRegListBox)
  end;

  { TRegListBoxWrapper }

  TRegListBoxWrapper = class(TWrapperLST<TRegListBoxForTest>)
  private
    FDefault: integer;
    FDoReadReg: boolean;
    FListBoxItemsDef: TStrings;
  protected
    function GetItemIndex: integer;
    procedure SetItemIndex(aItemIndex: integer);

    function GetCanWriteProperty: boolean;
    procedure SetCanWriteProperty(aCanWrite: boolean);

    function GetCanReadProperty: boolean;
    procedure SetCanReadProperty(aCanRead: boolean);
    function GetItemsCount: integer;
    function GetDoSyncData: boolean;
    procedure SetDoSyncData(aDoSyncData: boolean);

    procedure _Initialize; override;
    procedure _Finalize; override;

    procedure SetSectionsAndIdents; virtual;
    procedure SetRegistryEntries; override;
    procedure SetRegControl; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;

    property Default : integer
      read FDefault
      write FDefault;

    property ListBoxItemsDef: TStrings
      read FListBoxItemsDef;
  public
    procedure Click;
    procedure ClearItems;
    procedure SectionIdentDefault;
    function GetItemByIndex(aIndex: integer): string;
    function GetIndexOfItem(aItem: string): integer;
    function DeleteItem(aIndex: integer): boolean;

    property ItemIndex: integer
      read GetItemIndex
      write SetItemIndex;

    property ItemsCount: integer
      read GetItemsCount;

    property DoSyncData: boolean
      read GetDoSyncData
      write SetDoSyncData;

    property DoReadReg: boolean
      read FDoReadReg
      write FDoReadReg;

    property CanReadProperty: boolean
      read GetCanReadProperty
      write SetCanReadProperty;

    property CanWriteProperty: boolean
      read GetCanWriteProperty
      write SetCanWriteProperty;
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
  Section := SEC_TREGLISTBOX_UTF8;
  Ident := IDENT_TREGLISTBOX_UTF8;
  Default := DEFAULT_ITEMINDEX_VALUE;
  SpecialListProperties.AddListSection(SEC_TREGLISTBOXITEMS);

  ListBoxItemsDef.Clear;
  ListBoxItemsDef.Add('Key1_mit_ßÜÖÄüöä=Value1_mit_ßÜÖÄüöä');
  ListBoxItemsDef.Add('Key2_mit_ßÜÖÄüöä=Value2_mit_ßÜÖÄüöä');
  ListBoxItemsDef.Add('Key3_mit_ßÜÖÄüöä=Value3_mit_ßÜÖÄüöä');
  ListBoxItemsDef.Add('Key4_mit_ßÜÖÄüöä=Value4_mit_ßÜÖÄüöä');
  ListBoxItemsDef.Add('Key5_mit_ßÜÖÄüöä=Value5_mit_ßÜÖÄüöä');
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

function TRegListBoxWrapper.GetItemIndex: integer;
begin
  Result := RegControl.ItemIndex;
end;

procedure TRegListBoxWrapper.SetItemIndex(aItemIndex: integer);
begin
  RegControl.ItemIndex := aItemIndex;
end;

function TRegListBoxWrapper.GetCanWriteProperty: boolean;
begin
  Result := RegControl.RegistrySettings.CanWrite;
end;

procedure TRegListBoxWrapper.SetCanWriteProperty(aCanWrite: boolean);
begin
  RegControl.RegistrySettings.CanWrite := aCanWrite;
end;

function TRegListBoxWrapper.GetCanReadProperty: boolean;
begin
  Result := RegControl.RegistrySettings.CanRead;
end;

procedure TRegListBoxWrapper.SetCanReadProperty(aCanRead: boolean);
begin
  RegControl.RegistrySettings.CanRead := aCanRead;
end;

function TRegListBoxWrapper.GetItemsCount: integer;
begin
  Result := RegControl.Items.Count;
end;

function TRegListBoxWrapper.GetDoSyncData: boolean;
begin
  Result := RegControl.RegistrySettings.DoSyncData;
end;

procedure TRegListBoxWrapper.SetDoSyncData(aDoSyncData: boolean);
begin
  RegControl.RegistrySettings.DoSyncData := aDoSyncData;
end;

procedure TRegListBoxWrapper._Initialize;
begin
  inherited _Initialize;

  FDoReadReg := True;

  FListBoxItemsDef := TStringlist.Create;

  SetSectionsAndIdents;
end;

procedure TRegListBoxWrapper._Finalize;
begin
  if Assigned(FListBoxItemsDef) then
    FreeAndNil(FListBoxItemsDef);

  inherited _Finalize;
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

  ListBoxItemsDef.Clear;
  ListBoxItemsDef.Add('Key1=Value1');
  ListBoxItemsDef.Add('Key2=Value2');
  ListBoxItemsDef.Add('Key3=Value3');
  ListBoxItemsDef.Add('Key4=Value4');
  ListBoxItemsDef.Add('Key5=Value5');
end;

procedure TRegListBoxWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;

  with RegControl, RegControl.RegistrySource, RegControl.RegistrySettings do
  begin
    WriteString(ListSection, ListBoxItemsDef.Names[0], ListBoxItemsDef.ValueFromIndex[0]);
    WriteString(ListSection, ListBoxItemsDef.Names[1], ListBoxItemsDef.ValueFromIndex[1]);
    WriteString(ListSection, ListBoxItemsDef.Names[2], ListBoxItemsDef.ValueFromIndex[2]);
    WriteString(ListSection, ListBoxItemsDef.Names[3], ListBoxItemsDef.ValueFromIndex[3]);
    WriteString(ListSection, ListBoxItemsDef.Names[4], ListBoxItemsDef.ValueFromIndex[4]);
    WriteInteger(Section, Ident, DEFAULT_ITEMINDEX_VALUE);

    if FDoReadReg then
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

procedure TRegListBoxWrapper.Click;
begin
  FRegControl.Click;
end;

procedure TRegListBoxWrapper.ClearItems;
begin
  RegControl.Items.Clear;
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

