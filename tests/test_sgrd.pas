unit test_sgrd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, rdata_cn, Write_raster, GData_CN;


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

    TTestReadRaster= class(TTestCase)
  published
    procedure TestReadSDAT_smallint;
    procedure TestReadRST_smallint;
    procedure TestReadSDAT_integer;
    procedure TestReadSDAT_unsignedinteger;
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


procedure TTestReadRaster.TestReadSDAT_smallint;
var
   header: theader;
   tempdir: string;
   z: graster;
begin
   GetGFile(z, '../testfiles/molenbeek/modelinput_sdat/perceelskaart_2018_molenbeek_s1.sdat');
   assertequals(521, z[100,100]);
end;

procedure TTestReadRaster.TestReadRST_smallint;
var
   header: theader;
   tempdir: string;
   z: graster;
begin
   GetGFile(z, '../testfiles/molenbeek/modelinput/perceelskaart_2018_molenbeek_s1.rst');
   assertequals(521, z[100,100]);
end;

procedure TTestReadRaster.TestReadSDAT_integer;
var
   header: theader;
   tempdir: string;
   z: graster;
begin
   GetGFile(z, '../testfiles/molenbeek/modelinput_sdat/perceelskaart_2018_molenbeek_s1_signed32bit.sdat');
   assertequals(521, z[100,100]);
   assertequals(3000000, z[494,150]); // make sure large values are read
end;


procedure TTestReadRaster.TestReadSDAT_unsignedinteger;
var
   header: theader;
   tempdir: string;
   z: graster;
   EC : TClass;
begin
   // this test should fail as we do not support unsigned types
   try
     GetGFile(z, '../testfiles/molenbeek/modelinput_sdat/perceelskaart_2018_molenbeek_s1_unsigned32bit.sdat');
   except
     on E: Exception do
     EC:= E.ClassType;
   end;
  If EC=Nil then
  Fail('Expected exception, but no exception was raised');
end;

initialization

  RegisterTest(TTestReadHeaders);
  RegisterTest(TTestReadRaster);
end.

