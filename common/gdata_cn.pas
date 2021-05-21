
Unit GData_CN;
{In deze unit worden kaarten van integers ingelezen}
{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN;

Type 
  Graster = specialize Traster<smallint>;
  ERasterException = Class(Exception);

Procedure GetGFile(Var Z:GRaster; Filename:String);
Procedure SetDynamicGData(Var Z:GRaster);
Procedure SetzeroG(Var z:Graster);
Procedure DisposeDynamicGdata(Var Z:GRaster);
Procedure SetGRasterBorders(Var Z:GRaster);

Implementation


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
  bytedata : byte;
  header: THeader;
Begin

  if  matchstr(filename, saga_extensions) then
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
  If header.Datatype = 'ascii' Then
    Begin
      assignfile(textFileIMG, header.datafile);
      reset (textfileIMG);
      For i:= 1 To nrow Do
        For j:= 1 To ncol Do
          read(textfileIMG, Z[i,j]);
      Closefile(textfileimg);
    End
  Else
    Begin
      If header.DataType = 'byte' Then
        Begin
          assignfile(byteFileIMG, header.datafile);
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
          assignfile(FileIMG, header.datafile);
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
