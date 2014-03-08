unit issue00030_test;

{$mode Delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  registrysource_wrapper,
  reglistbox_wrapper,
  regchecklistbox_wrapper,
  regcombobox_wrapper,
  regradiogroup_wrapper,
  regcheckgroup_wrapper,
  regvaluelisteditor_wrapper;

type

  TDoMergeDataTest= class(TTestCase)
  protected
    FRegSrcWrapper: TRegistrySourceWrapper;
    FRegListBoxWrapper: TRegListBoxWrapper;
    FRegCheckListBoxWrapper: TRegCheckListBoxWrapper;
    FRegComboBoxWrapper: TRegComboBoxWrapper;
    FRegRadioGroupWrapper: TRegRadioGroupWrapper;
    FRegCheckGroupWrapper: TRegCheckGroupWrapper;
    FRegValueListEditorWrapper: TRegValueListEditorWrapper;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckProperty;
  end;

implementation

uses
  test_utils,
  test_const;

procedure TDoMergeDataTest.CheckProperty;
begin
  //1. Fall: TRegListBox testen
  CheckPropertyAvailable(FRegListBoxWrapper.RegControl.RegistrySettings,
    DO_MERGE_DATA_PROPERTY_NAME, '1. Fall: TRegListBox');

  //2. Fall: TRegCheckListBox testen
  CheckPropertyAvailable(FRegCheckListBoxWrapper.RegControl.RegistrySettings,
    DO_MERGE_DATA_PROPERTY_NAME, '2. Fall: TRegCheckListBox');

  //3. Fall: TRegComboBox testen
  CheckPropertyAvailable(FRegComboBoxWrapper.RegControl.RegistrySettings,
    DO_MERGE_DATA_PROPERTY_NAME, '3. Fall: TRegComboBox');

  //4. Fall: TRegRadioGroup testen
  CheckPropertyAvailable(FRegRadioGroupWrapper.RegControl.RegistrySettings,
    DO_MERGE_DATA_PROPERTY_NAME, '4. Fall: TRegRadioGroup');

  //5. Fall: TRegCheckGroup testen
  CheckPropertyAvailable(FRegCheckGroupWrapper.RegControl.RegistrySettings,
    DO_MERGE_DATA_PROPERTY_NAME, '5. Fall: TRegCheckGroup');

  //6. Fall: TRegValueListEditor testen
  CheckPropertyAvailable(FRegValueListEditorWrapper.RegControl.RegistrySettings,
    DO_MERGE_DATA_PROPERTY_NAME, '6. Fall: TRegValueListEditor');
end;

procedure TDoMergeDataTest.SetUp;
begin
  FRegSrcWrapper := TRegistrySourceWrapper.Create;

  FRegListBoxWrapper :=
    TRegListBoxWrapper.Create(FRegSrcWrapper.RegistrySource);

  FRegCheckListBoxWrapper :=
    TRegCheckListBoxWrapper.Create(FRegSrcWrapper.RegistrySource);

  FRegComboBoxWrapper :=
    TRegComboBoxWrapper.Create(FRegSrcWrapper.RegistrySource);

  FRegRadioGroupWrapper :=
    TRegRadioGroupWrapper.Create(FRegSrcWrapper.RegistrySource);

  FRegCheckGroupWrapper :=
    TRegCheckGroupWrapper.Create(FRegSrcWrapper.RegistrySource);

  FRegValueListEditorWrapper :=
    TRegValueListEditorWrapper.Create(FRegSrcWrapper.RegistrySource);
end;

procedure TDoMergeDataTest.TearDown;
begin
  if Assigned(FRegValueListEditorWrapper) then
    FreeAndNil(FRegValueListEditorWrapper);

  if Assigned(FRegCheckGroupWrapper) then
    FreeAndNil(FRegCheckGroupWrapper);

  if Assigned(FRegRadioGroupWrapper) then
    FreeAndNil(FRegRadioGroupWrapper);

  if Assigned(FRegComboBoxWrapper) then
    FreeAndNil(FRegComboBoxWrapper);

  if Assigned(FRegCheckListBoxWrapper) then
    FreeAndNil(FRegCheckListBoxWrapper);

  if Assigned(FRegListBoxWrapper) then
    FreeAndNil(FRegListBoxWrapper);

  if Assigned(FRegSrcWrapper) then
    FreeAndNil(FRegSrcWrapper);
end;

end.

