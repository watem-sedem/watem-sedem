unit test_sgrd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, rdata_cn;


type

  TTestReadHeaders= class(TTestCase)
  published
    procedure TestReadSGRD;
    procedure TestReadRDC;
  end;

implementation

procedure TTestReadHeaders.TestReadSGRD;
var
  header: theader;
begin
  header:=ReadSGRD('../testfiles/molenbeek/modelinput/test.sgrd');
  assertequals(20, header.res);
  assertequals (201620, header.minx);
  assertequals (153880, header.miny);
  // calculated fields
  assertequals('Test maxx', 207500, header.maxx);
  assertequals (164060, header.maxy);
end;

procedure TTestReadHeaders.TestReadRDC;
var
  header: theader;
begin
  header:=ReadRDC('../testfiles/molenbeek/modelinput/dtm.rdc');
  assertequals(20, header.res);
  assertequals (201620, header.minx);
  assertequals (153880, header.miny);
  // calculated fields
  assertequals('Test maxx', 207500, header.maxx);
  assertequals (164060, header.maxy);
end;


initialization

  RegisterTest(TTestReadHeaders);
end.

