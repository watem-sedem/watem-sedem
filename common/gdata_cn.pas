
Unit GData_CN;
{In deze unit worden kaarten van integers ingelezen}
{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, Dialogs;

Type 
  Graster = array Of array Of smallint;
  ERasterException = Class(Exception);

Procedure GetGFile(Var Z:GRaster; Filename:String);
Procedure SetDynamicGData(Var Z:GRaster);
Procedure SetzeroG(Var z:Graster);
Procedure DisposeDynamicGdata(Var Z:GRaster);

Implementation

Uses RData_CN;


//**************************************************************************
//De waarden van de buitenste cellen worden vervangen door de nullen
//WANT de buitenste cellen zijn steeds 0 wanneer het bestand wordt aangemaakt:
//in Lazarus is de eerste cel (0,0) terwijl deze in Idrisi (1,1) is!!!
//**************************************************************************

Procedure SetRasterBorders(Var Z:GRaster);

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

Var 
  i       : integer;
Begin
  SetLength(Z,nrow+2);
  For i := Low(Z) To high(Z) Do
    Setlength(Z[i],ncol+2);
End;


//***************************************************************************
//Met deze procedure wordt het dynamisch toegekende geheugen weer vrijgegeven
//***************************************************************************
Procedure DisposeDynamicGdata(Var Z:GRaster);

Var 
  i       : integer;
Begin
  For i := Low(Z) To high(Z) Do
    Z[i] := Nil;
  Z := Nil;
End;


//********************************************************************
//In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
//en wordt de nodige informatie hieruit gehaald.
//********************************************************************

Procedure GetGFile(Var Z:GRaster; Filename:String);

Var 
  i,j,hulpgetal: integer;
  docfileIMG : textfile;
  fileIMG : file Of smallint;
  textfileIMG : textfile ;
  bytefileIMG : file Of byte;
  docnfileIMG,NfileIMG,dumstr : string;
  idrisi32,asciidatatype,bytebinary : boolean;
  bytedata : byte;
Begin
  dumstr := extractfilename(filename);
  If ExtractFileExt(dumstr)='.img' Then  idrisi32 := false
  Else idrisi32 := true;
  hulpgetal := length(dumstr)-2;
  delete(dumstr,hulpgetal,3);
  If Idrisi32 Then
    Begin
      docNfileIMG := dumstr + 'rdc' ;
      NfileIMG := dumstr + 'rst';
      // ==> De namen van de betreffende .rst en .rdc bestanden worden nagemaakt
    End
  Else //Voor .IMG bestanden
    Begin
      docNfileIMG := dumstr + 'doc' ;
      NfileIMG := dumstr + 'img';
    End;

  // INLEZEN NCOLS
  Assignfile(docfileIMG, docNfileIMG);
  //Een 'filehandle' wordt toegewezen aan de bestanden
  reset(docfileIMG);
  //Het .rdc bestand wordt geopend om te lezen
  If Idrisi32 Then
    For i := 1 To 3 Do
      readln(docfileIMG, dumstr);
  delete (dumstr,1,14);
  //Na 14 tekens staat het data type
  If (dumstr='real')  Then
    Begin
      closefile(docfileIMG);
      Raise ERasterException.Create('Error in reading one of the rasters: data type must be integer, please re-enter data');
      exit;
    End;
  If dumstr='byte' Then
    bytebinary := true
  Else bytebinary := false;
  readln(docfileIMG, dumstr);
  delete (dumstr,1,14);
  If dumstr='binary' Then
    asciidatatype := false
  Else asciidatatype := true;
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

  // Inlezen gegevens
  SetDynamicGData(Z);
  //Er wordt geheugen vrijgemaakt voor de matrix Z
  If asciidatatype Then
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
      If bytebinary Then
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
  Closefile(DocfileImg);
  SetRasterBorders(Z);

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
  i,j: integer;
Begin
  For i:=Low(Z) To High(Z) Do
    For j:=Low(Z[i]) To High(Z[i]) Do
      Begin
        Z[i,j] := 0
      End;
End;


End.
