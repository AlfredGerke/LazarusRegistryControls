unit fmeregedit;

{$mode delphi}

interface

uses
  fmecustomcontrolframe,
  regedit,
  regvaluelisteditor,
  reglabel,
  ExtCtrls,
  Forms;

type

  { TControlRegEdit }

  TControlRegEdit = class(TCustomRegControlFrame<TRegEdit>)
    Bevel1: TBevel;
    RegEdit1: TRegEdit;
    RegLabel1: TRegLabel;
    RegValueListEditor1: TRegValueListEditor;
    ScrollBox1: TScrollBox;
  protected
    procedure _Initialize; override;
  end;

implementation

{$R *.lfm}

{ TControlRegEdit }

procedure TControlRegEdit._Initialize;
begin
  inherited;

  SetRegControl(RegEdit1);
end;

end.

