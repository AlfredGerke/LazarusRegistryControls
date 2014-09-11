unit lrcreginifile_test;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry;

type

  { TLRCRegInifileTest }

  TLRCRegInifileTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DeleteKey;
    procedure EraseSection;
    procedure ReadBool;
    procedure ReadInteger;
    procedure ReadString;
    procedure ReadSection;
    procedure ReadSections;
    procedure ReadSectionValues;
    procedure WriteBool;
    procedure WriteInteger;
    procedure WriteString;
    procedure TestFilename;

  end;

implementation


procedure TLRCRegInifileTest.SetUp;
begin

end;

procedure TLRCRegInifileTest.TearDown;
begin

end;

procedure TLRCRegInifileTest.DeleteKey;
begin

end;

procedure TLRCRegInifileTest.EraseSection;
begin

end;

procedure TLRCRegInifileTest.ReadBool;
begin

end;

procedure TLRCRegInifileTest.ReadInteger;
begin

end;

procedure TLRCRegInifileTest.ReadString;
begin

end;

procedure TLRCRegInifileTest.ReadSection;
begin

end;

procedure TLRCRegInifileTest.ReadSections;
begin

end;

procedure TLRCRegInifileTest.ReadSectionValues;
begin

end;

procedure TLRCRegInifileTest.WriteBool;
begin

end;

procedure TLRCRegInifileTest.WriteInteger;
begin

end;

procedure TLRCRegInifileTest.WriteString;
begin

end;

procedure TLRCRegInifileTest.TestFilename;
begin

end;

end.
