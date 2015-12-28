unit test_const;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils;

const
  LRCREGINIFILE_TESTROOT = 'SOFTWARE\ExampleFactory\LRCRegInifile';

  TREGISTRYSOURCE_NAME = 'TRegistrySource';
  TREGCHECKBOX_NAME = 'TRegCheckBox';
  TREGRADIOBUTTON_NAME = 'TRegRadioButton';
  TREGRADIOGROUP_NAME = 'TRegRadioGroup';
  TREGEDIT_NAME = 'TRegEdit';
  TREGCOMBOBOX_NAME = 'TRegComboBox';
  TREGLISTBOX_NAME = 'TRegListBox';
  TREGCHECKLISTBOX_NAME = 'TRegCheckListBox';
  TREGCHECKGROUP_NAME = 'TRegCheckGroup';
  TREGVALUELISTEDITOR_NAME = 'TRegValueListEditor';
  TREGLABEL_NAME = 'TRegLabel';

  SEC_FPCUNIT_URF8TEST = 'UTF8Bug_mit_ßÖÜÄöüä';
  IDENT_LRC_VERSION_UTF8TEST = 'Version_mit_ßÖÜÄöüä';


  DO_MERGE_DATA_PROPERTY_NAME = 'DoMergeData';
  MERGE_DATA_PROPERTY_NAME = 'MergeData';

  SEC_FPCUNIT_TEST = 'FPCUnit-Test';
  IDENT_VERSION = 'Version';
  IDENT_LRC_VERSION = 'LRC-Version';
  _VERSION = '0.9.8.5';

  SEC_TREGEDIT = 'TRegEdit';
  DEFAULT_TEXT_ENTRY = 'Default-Eintrag';
  _TEXT_ENTRY = 'Text-Eintrag';
  _TEST_STRING = 'Test-String';
  IDENT_TEXT_PROPERTY = 'Text-Property';

  SEC_TREGCHECKBOX = 'TRegCheckBox';
  SEC_TREGCHECKBOX_UTF8 = 'TRegCheckBox_mit_ßÜÖÄüöä';
  IDENT_CHECK_PROPERTY = 'Checked-Property';
  IDENT_CHECK_PROPERTY_UTF8 = 'Checked-Property_mit_ßÜÖÄüöä';
  _TREGCHECKBOX_CAPTION_VALUE = 'Dynamisch';
  _TREGCHECKBOX_CAPTION_VALUE_UTF8 = 'Dynamisch_mit_ßÜÖÄüöä';
  _CHECKED_ENTRY = True;
  DEFAULT_CHECKED_ENTRY = False;

  SEC_TREGRADIOBUTTON = 'TRegRadioButton';
  SEC_TREGRADIOBUTTON_UTF8 = 'TRegRadioButton_mit_ßÜÖÄüöä';
  _TREGRADIOBUTTON_CAPTION_VALUE = 'Dynamisch';
  _TREGRADIOBUTTON_CAPTION_VALUE_UTF8 = 'Dynamisch_mit_ßÜÖÄüöä';
  DEFAULT_CAPTION_VALUE = 'Statisch';

  IDENT_CAPTION = 'Caption';
  IDENT_CAPTION_UTF8 = 'Caption_mit_ßÜÖÄüöä';

  SEC_TREGLABEL = 'TRegLabel';
  SEC_TREGLABEL_UTF8 = 'TRegLabel_mit_ßÜÖÄüöä';
  _TREGLABEL_CAPTION_VALUE = 'Dynamisch';
  _TREGLABEL_CAPTION_VALUE_UTF8 = 'Dynamisch_mit_ßÜÖÄüöä';

  CAPTION_FOR_TREGLABEL = 'LRC RegLabel';

  SEC_TREGLISTBOX = 'TRegListBox';
  SEC_TREGLISTBOX_UTF8 = 'TRegListBox_mit_ßÜÖÄüöä';
  SEC_TREGLISTBOXITEMS = 'TRegListBoxItems';
  SEC_TREGLISTBOXITEMS_UTF8 = 'TRegListBoxItems_mit_ßÜÖÄüöä';
  DEFAULT_ITEMINDEX_VALUE = -1;
  IDENT_TREGLISTBOX = 'ItemIndex';
  IDENT_TREGLISTBOX_UTF8 = 'ItemIndex_mit_ßÜÖÄüöä';

implementation

end.

