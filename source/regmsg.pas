unit regmsg;

{$mode Delphi}{$H+}

interface

uses
  LMessages;

const
  LM_REGISTRY_CONTROL_REFRESH_DATA = LM_USER + 100;
  LM_REGISTRY_CONTROL_REFRESH_SETTINGS = LM_USER + 101;
  LM_REGISTRY_CONTROL_SET_SYNC = LM_USER + 102;
  LM_REGISTRY_CONTROL_SET_WRITEADHOC = LM_USER + 103;
  LM_REGISTRY_CONTROL_FREE_REGISTR_SOURCE = LM_USER + 104;
  LM_REGISTRY_CONTROL_SHOW_EDITDIALOG = LM_USER + 105;
  LM_REGISTRY_CONTROL_CLEAR_LIST = LM_USER + 106;
  LM_REGISTRY_CONTROL_MERGE_LIST = LM_USER + 107;
  LM_REGISTRY_CONTROL_POST_DATA = LM_USER + 108;

implementation

end.

