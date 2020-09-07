
Unit GData_CN;
{In deze unit worden kaarten van integers ingelezen}
{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils;

Type 
  Graster = array Of array Of smallint;
  ERasterException = Class(Exception);

Procedure GetGFile(Var Z:GRaster; Filename:String);
Procedure SetDynamicGData(Var Z:GRaster);
Procedure SetzeroG(Var z:Graster);
Procedure DisposeDynamicGdata(Var Z:GRaster);
Procedure ReadHeadersIdrisi(docNfileIMG: string; Var Datatype: string);
Procedure SetGRasterBorders(Var Z:GRaster);

Implementation

Uses RData_CN;


//**************************************************************************
//De waarden van de buitenste cellen worden vervangen door de nullen
//WANT de buitenste cellen zijn steeds 0 wanneer het bestand wordt aangemaakt:
//in Lazarus is de eerste cel (0,0) terwijl deze in Idrisi (1,1) is!!!
//**************************************************************************

Procedure SetGRasterBorders(Var Z:GRaster);

Var 
  i,j       : integer;
Begin
  Z[0,0] := 0;
  Z[0,(ncol+1)] := 0;
  Z[nrow+1,0] := 0;
  Z[nrow+1,ncol+1] := 0;
  For j := 1 To ncol Do
    Begin
      Z[0,j] := 0;
      Z[(nrow+1),j] := 0;
    End;
  For  i := 1 To nrow Do
    Begin
      Z[i,0] := 0;
      Z[i,ncol+1] := 0;
    End;
End;

//**********************************************
//Er wordt geheugen vrijgemaakt voor de matrix Z
//**********************************************

Procedure SetDynamicGData(Var Z:GRaster);
Begin
  SetLength(Z,nrow+2, ncol+2);
End;


//***************************************************************************
//Met deze procedure wordt het dynamisch toegekende geheugen weer vrijgegeven
//***************************************************************************
Procedure DisposeDynamicGdata(Var Z:GRaster);
Begin
  Z := Nil;
End;


//********************************************************************
//In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
//en wordt de nodige informatie hieruit gehaald.
//********************************************************************

Procedure GetGFile(Var Z:GRaster; Filename:String);

Var 
  i,j: integer;
  fileIMG : file Of smallint;
  textfileIMG : textfile ;
  bytefileIMG : file Of byte;
  docnfileIMG,NfileIMG,dumstr: string;
  bytedata : byte;
  datatype: string;
Begin
  dumstr := extractfilename(filename);
  case ExtractFileExt(filename) of
  '.img', '.doc' :
    begin
      docNfileIMG := ChangeFileExt(dumstr, '.doc');
      NfileIMG := ChangeFileExt(dumstr, '.img');
    end;
  '.rst', '.rdc' : begin docNfileIMG := changefileext(dumstr, '.rdc') ; NfileIMG :=  changefileext(dumstr, '.rst'); end;
  '.sdat', 'sgrd': begin docNfileIMG := changefileext(dumstr, '.sgrd') ; NfileIMG :=  changefileext(dumstr, '.sdat'); end;
  end;

  ReadHeadersIdrisi(docNfileIMG, Datatype);

  // Inlezen gegevens
  SetDynamicGData(Z);
  //Er wordt geheugen vrijgemaakt voor de matrix Z
  If Datatype = 'ascii' Then
    Begin
      assignfile(textFileIMG, NfileIMG);
      reset (textfileIMG);
      For i:= 1 To nrow Do
        For j:= 1 To ncol Do
          read(textfileIMG, Z[i,j]);
      Closefile(textfileimg);
    End
  Else
    Begin
      If DataType = 'byte' Then
        Begin
          assignfile(byteFileIMG, NfileIMG);
          reset (bytefileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              Begin
                read(bytefileIMG, bytedata);
                Z[i,j] := bytedata;
              End;
          Closefile(byteFileimg);
        End
      Else
        Begin
          assignfile(FileIMG, NfileIMG);
          reset (fileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              Begin
                read(fileIMG,Z[i,j]);
              End;
          Closefile(fileimg);
        End;
    End;

  SetGRasterBorders(Z);

  //ncol, nrow en res worden opgeslagen in array zodat achteraf kan worden nagegaan
  //of deze voor alle kaarten gelijk zijn
  lengthAR := lengthAR + 1;
  SetLength(nrowAR, lengthAR);
  SetLength(ncolAR, lengthAR);
  SetLength(resAR, lengthAR);
  nrowAR[lengthAR-1] := NROW;
  ncolAR[lengthAR-1] := NCOL;
  resAR[lengthAR-1] := RES;

End;

Procedure ReadHeadersIdrisi(docNfileIMG: string; Var Datatype: string);
Var
    docfileIMG : textfile;
    dumstr: string;
    i: integer;
Begin
// INLEZEN NCOLS
Assignfile(docfileIMG, docNfileIMG);

if not FileExists(docNfileIMG) then
  Raise ERasterException.Create('Error: raster does not exist:' + docNfileIMG);
//Een 'filehandle' wordt toegewezen aan de bestanden
reset(docfileIMG);
//Het .rdc bestand wordt geopend om te lezen
If ExtractFileExt(docNfileIMG)= '.rdc' Then
  For i := 1 To 3 Do
    readln(docfileIMG, dumstr);
delete (dumstr,1,14);
//Na 14 tekens staat het data type

case dumstr of
 'real', 'byte': Datatype:= dumstr;
end;

if Datatype='real' then
 Begin
   closefile(docfileIMG);
   Raise ERasterException.Create('Error in reading one of the rasters: data type must be integer, please re-enter data');
   exit;
 End;

readln(docfileIMG, dumstr);
delete (dumstr,1,14);

If dumstr<>'binary' Then
  Datatype:= 'ascii';

readln(docfileIMG, dumstr);
delete (dumstr,1,14);
ncol := strtoint(dumstr);
// Number of columns is saved

// INLEZEN NROWS
readln(docfileIMG, dumstr);
delete (dumstr,1,14);
nrow := strtoint(dumstr);
// Number of rows is saved

readln(docfileIMG, dumstr);
delete(dumstr,1,14);

If (dumstr='plane') Or (dumstr='') Then Raster_Projection := plane
Else Raster_Projection := LATLONG;

For i := 1 To 2 Do
  // Er worden 2 lijnen gelezen
  readln(docfileIMG);
  readln(docfileIMG,dumstr);
  delete(dumstr,1,14);
  MINX := strtofloat(dumstr);
  readln(docfileIMG,dumstr);
  delete(dumstr,1,14);
  MAXX := strtofloat(dumstr);
  readln(docfileIMG,dumstr);
  delete(dumstr,1,14);
  MINY := strtofloat(dumstr);
  readln(docfileIMG,dumstr);
  delete(dumstr,1,14);
  MAXY :=  strtofloat(dumstr);
  readln(docfileIMG);
  readln(docfileIMG, dumstr);
  delete(dumstr,1,14);
  res := strtofloat(dumstr);

    Closefile(DocfileImg);

end;

//*****************************************************************
//Deze procedure geeft een nulwaarde aan elk element in een Graster
//*****************************************************************
Procedure SetzeroG(Var z:Graster);
Var
    i: integer;
Begin
  For i:=Low(Z) To High(Z) Do
    Fillword(z[i][0], ncol+2, 0);
End;


End.
