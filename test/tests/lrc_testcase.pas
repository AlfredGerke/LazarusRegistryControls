unit lrc_testcase;

{$mode Delphi}{$H+}

interface

uses
  fpcunit;

type

  { TLRCTestCase }

  TLRCTestCase<_T> = class(TTestCase)
  private
    FCaptionSection: string;
    FCaptionIdent: string;
    FCaptionValueByReg: string;

    FSection: string;
    FIdent: string;
    FDefault: _T;

    FListSection: string;

    FCheckRTLNeeded: boolean;
  protected
    procedure SetSectionsAndIdents; virtual;
    procedure SetUp; override;

    function GetListSectionUTF8Decoded: string;
    property ListSection: string
      read FListSection
      write FListSection;

    function GetSectionUTF8Decoded: string;
    property Section: string
      read FSection
      write FSection;

    function GetIdentUTF8Decoded: string;
    property Ident: string
      read FIdent
      write FIdent;

    property Default: _T
      read FDefault
      write FDefault;

    property CheckRTLNeeded: boolean
      read FCheckRTLNeeded
      write FCheckRTLNeeded;

    property CaptionSection: string
      read FCaptionSection
      write FCaptionSection;

    property CaptionIdent: string
     read FCaptionIdent
     write FCaptionIdent;

    property CaptionValueByReg: string
      read FCaptionValueByReg
      write FCaptionValueByReg;
  public
  end;

implementation

uses
  regconvutils;

{ TLRCTestCase }

procedure TLRCTestCase<_T>.SetSectionsAndIdents;
begin
  // In einer Ableitung mit Code belegen
end;

procedure TLRCTestCase<_T>.SetUp;
begin
  inherited SetUp;

  SetSectionsAndIdents;
end;

function TLRCTestCase<_T>.GetListSectionUTF8Decoded: string;
begin
  Result := UTF8DecodeIfNeeded(FListSection, FCheckRTLNeeded);
end;

function TLRCTestCase<_T>.GetSectionUTF8Decoded: string;
begin
  Result := UTF8DecodeIfNeeded(FSection, FCheckRTLNeeded);
end;

function TLRCTestCase<_T>.GetIdentUTF8Decoded: string;
begin
  Result := UTF8DecodeIfNeeded(FIdent, FCheckRTLNeeded);
end;

end.

