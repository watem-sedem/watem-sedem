
Unit GData_CN;
{In deze unit worden kaarten van integers ingelezen}
{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN, strutils;

Type 
  Graster = specialize Traster<smallint>;
  ERasterException = Class(Exception);

Procedure GetGFile(Var Z:GRaster; Filename:String);
Procedure SetDynamicGData(Var Z:GRaster);
Procedure SetzeroG(Var z:Graster);
Procedure DisposeDynamicGdata(Var Z:GRaster);

Implementation

//**********************************************
//Er wordt geheugen vrijgemaakt voor de matrix Z
//**********************************************

Procedure SetDynamicGData(Var Z:GRaster);
Begin
  Z:= GRaster.create(nrow, ncol);
End;


//***************************************************************************
//Met deze procedure wordt het dynamisch toegekende geheugen weer vrijgegeven
//***************************************************************************
Procedure DisposeDynamicGdata(Var Z:GRaster);
Begin
  Z := Nil;
End;


generic procedure readbinaryfile<T>(var Z:graster; header: Theader);
var
    byteFileimg: file of T;
    bytedata: T;
    i, j, irow: integer;
begin
   assignfile(byteFileIMG, header.datafile);
    reset (bytefileIMG);
    For i:= 1 To nrow Do
      begin
        if header.toptobottom then irow:=i else irow:=nrow-i+1;
        For j:= 1 To ncol Do
          Begin
            read(bytefileIMG, bytedata);
            Z[irow,j] := bytedata;
          End
        end;
    Closefile(byteFileimg);
end;


procedure readasciifile(var Z:graster; header: Theader);
var
    textfileIMG : textfile;
    i, j, irow: integer;
Begin
  assignfile(textFileIMG, header.datafile);
  reset (textfileIMG);
  For i:= 1 To nrow Do
    For j:= 1 To ncol Do
      if header.toptobottom then irow:=i else irow:=nrow-i+1;
      read(textfileIMG, Z.r[irow,j]);
  Closefile(textfileimg);
End;

//********************************************************************
//In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
//en wordt de nodige informatie hieruit gehaald.
//********************************************************************

Procedure GetGFile(Var Z:GRaster; Filename:String);

Var 
  i,j, irow: integer;
  fileIMG : file Of smallint;
  bytefileIMG : file Of byte;
  bytedata : byte;
  header: THeader;
Begin

  if  matchstr(ExtractFileExt(filename),saga_extensions) then
    header:= ReadSGRD(filename)
  else
   header := readrdc(filename);

    If (header.datatype ='real') Then
      Begin

        Raise ERasterException.Create(
               'Error in reading one of the rasters: data type must be integer, please re-enter data'
        );
      End;

    ncol := header.ncol;
    nrow := header.nrow;
    res := header.res;

    // it would make more sense to keep these in a header object for the future
    minx := header.minx;
    maxx := header.maxx;
    miny := header.miny;
    maxy := header.maxy;

  SetDynamicGData(Z);
  //Er wordt geheugen vrijgemaakt voor de matrix Z

  case (header.Datatype) of
      'ascii': readasciifile(z, header);
      'byte': specialize readbinaryfile<byte>(z, header);
      'smallint': specialize readbinaryfile<smallint>(z, header);
      'integer': specialize readbinaryfile<integer>(z, header);
      else  raise Exception.Create('invalid integer datatype '+ header.Datatype + ' in ' + filename);
  end;
  z.SetRasterBorders;

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


//*****************************************************************
//Deze procedure geeft een nulwaarde aan elk element in een Graster
//*****************************************************************
Procedure SetzeroG(Var z:Graster);
Var
    i: integer;
Begin
  For i:=Low(Z.r) To High(Z.r) Do
    Fillword(z.r[i][0], ncol+2, 0);
End;




End.
