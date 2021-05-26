unit test_sgrd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, rdata_cn, Write_raster;


type

  TTestReadHeaders= class(TTestCase)
  published
    procedure TestReadSGRD;
    procedure TestReadRDC;
    procedure TestReadWriteRDC;
    procedure TestReadWriteSGRD;
  private
    function CreateTempdir: string;
  end;

implementation

function TTestReadHeaders.CreateTempdir: string;
begin
  CreateTempdir := gettempdir + 'unit_test_cnws/';
  createdir(CreateTempdir);
end;

procedure TTestReadHeaders.TestReadSGRD;
var
  header: theader;
begin
  header:=ReadSGRD('../testfiles/molenbeek/modelinput_sdat/K3_CNWS_molenbeek.sdat');
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
  assertequals('Test maxx', 207500, header.maxx);
  assertequals (164060, header.maxy);
end;

procedure TTestReadHeaders.TestReadWriteRDC;
var
   header: theader;
   tempdir: string;
begin
   header:=ReadRDC('../testfiles/molenbeek/modelinput/dtm.rdc');
   tempdir := CreateTempdir();
   header.datafile:=tempdir + 'test.rst';
   writeIdrisi32header(header);
   header:=ReadRDC(tempdir + 'test.rdc');
   assertequals(20, header.res);
   assertequals (201620, header.minx);
   assertequals (153880, header.miny);
   assertequals('Test maxx', 207500, header.maxx);
   assertequals (164060, header.maxy);
   removedir(tempdir);
end;

procedure TTestReadHeaders.TestReadWriteSGRD;
var
   header: theader;
   tempdir: string;
begin
   header:=ReadRDC('../testfiles/molenbeek/modelinput/dtm.sdat');
   tempdir := CreateTempdir();
   header.datafile:=tempdir + 'test.sdat';
   writeIdrisi32header(header);
   header:=ReadRDC(tempdir + 'test.sgrd');
   assertequals(20, header.res);
   assertequals (201620, header.minx);
   assertequals (153880, header.miny);
   assertequals('Test maxx', 207500, header.maxx);
   assertequals (164060, header.maxy);
   removedir(tempdir);

end;

initialization

  RegisterTest(TTestReadHeaders);
end.

