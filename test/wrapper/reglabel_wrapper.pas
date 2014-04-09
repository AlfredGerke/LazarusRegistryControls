unit reglabel_wrapper;

{$mode delphi}

interface

uses
  regsourcen,
  reglabel,
  test_wrapper;

type

  { TRegLabelWrapper }

  TRegLabelWrapper = class(TWrapper<TRegLabel>)
  private
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;

  { TRegLabelWrapperUTF8 }

  TRegLabelWrapperUTF8 = class(TRegLabelWrapper)
  private
  protected
    procedure SetRegistryEntries; override;
    procedure SetRegistrySettings(aRegistrySource: TRegistrySource;
                                  aSetRegSrc: boolean = True); override;
  public
  public
  end;

implementation

{ TRegLabelWrapperUTF8 }

procedure TRegLabelWrapperUTF8.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegLabelWrapperUTF8.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

{ TRegLabelWrapper }

procedure TRegLabelWrapper.SetRegistryEntries;
begin
  inherited SetRegistryEntries;
end;

procedure TRegLabelWrapper.SetRegistrySettings(
  aRegistrySource: TRegistrySource; aSetRegSrc: boolean);
begin
  inherited SetRegistrySettings(aRegistrySource, aSetRegSrc);
end;

end.

