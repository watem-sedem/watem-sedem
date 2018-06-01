
Unit RData_CN;

//In de oorspronkelijk unit was er ook een formulier gedefinieerd.
//Dit is hier voorlopig uitgelaten!

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils;

Type 
  Rraster = array Of array Of single ;
  TRaster_Projection = (plane,LATLONG);

  ERasterException = Class(Exception);

    Procedure GetRfile(Var Z:RRaster; Filename:String);
    Procedure SetDynamicRData(Var Z:RRaster);
    Procedure SetzeroR(Var z:Rraster);
    Procedure DisposeDynamicRdata(Var Z:RRaster);

    Var 
      NROW,NCOL: integer;
      RES: double;
      //fixed resolution for plane proj and dx=dy
      MINX, MAXX, MINY, MAXY, MINZ, MAXZ : double;
      Raster_Projection: TRaster_Projection;
      ncolAR, nrowAR: array Of integer;
      // array waarin resp. nrow, ncol en
      resAR: array Of double;
      // res van elke ingelezen kaart wordt opgeslagen
      lengthAR: integer;

    Implementation

    //********************************************************************
    //De waarden van de buitenste cellen worden vervangen door de waarden van de
    //cellen die een laag meer naar het midden liggen
    //********************************************************************
    Procedure SetRasterBorders(Var Z:RRaster);

    Var 
      i,j       : integer;
    Begin
      Z[0,0] := Z[1,1];
      Z[0,(ncol+1)] := Z[1,ncol];
      Z[nrow+1,0] := Z[nrow,1];
      Z[nrow+1,ncol+1] := Z[nrow,ncol];
      For j := 1 To ncol Do
        Begin
          Z[0,j] := Z[1,j];
          Z[(nrow+1),j] := Z[nrow,j];
        End;
      For  i := 1 To nrow Do
        Begin
          Z[i,0] := Z[i,1];
          Z[i,ncol+1] := Z[i,ncol];
        End;
    End;

    //********************************************************************
    //In onderstaande regels wordt er geheugen vrij gemaakt voor de verschillende
    //arrays die de kaarten voorstellen
    //********************************************************************
    Procedure SetDynamicRData(Var Z:RRaster);
    Begin
      SetLength(Z,nrow+2, ncol+2);
    End;

    //***************************************************************************
    //Met deze procedure wordt het dynamisch toegekende geheugen weer vrijgegeven
    //***************************************************************************
    Procedure DisposeDynamicRdata(Var Z:RRaster);
    Begin
      Z := Nil;
    End;

    //********************************************************************
    //In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
    //en wordt de nodige informatie hieruit gehaald.
    //********************************************************************

    Procedure GetRFile(Var Z:RRaster; Filename:String);

    Var 
      i,j,hulpgetal: integer;
      docfileIMG : textfile;
      fileIMG : file Of single ;
      textfileIMG : textfile ;
      docnfileIMG,NfileIMG,dumstr : string;
      idrisi32,asciidatatype : boolean;
    Begin
      dumstr := ExtractFilename(filename);
      If ExtractFileExt(dumstr)='.img' Then
        idrisi32 := false
      Else idrisi32 := true;

      hulpgetal := length(dumstr)-2;
      delete(dumstr,hulpgetal,3);
      //De extensie wordt verwijderd
      If Idrisi32 Then //Voor Idrisi32 bestanden
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
      If (dumstr='integer') Or (dumstr='byte') Then
        Begin
          closefile(docfileIMG);
          Raise ERasterException.Create(
                 'Error in reading one of the rasters: data type must be real, please re-enter data'
          );
        End;
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      //Het filetype wordt opgeslagen
      If dumstr='binary' Then
        asciidatatype := false
      Else asciidatatype := true;
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      ncol := strtoint(dumstr);
      // INLEZEN NROWS
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      nrow := strtoint(dumstr);
      readln(docfileIMG, dumstr);
      delete(dumstr,1,14);
      If (dumstr='plane') Or (dumstr='') Then Raster_Projection := plane
      Else Raster_Projection := LATLONG;
      readln(docfileIMG);
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
      If (res=0.0) Then
        Begin
          Raise ERasterException.Create('Error in reading one of the rasters: Resolution is invalid'
          );
        End;

      // Inlezen gegevens

      //Er wordt geheugen vrijgemaakt voor de kaart (array) in kwestie
      SetDynamicRData(Z);

      //Het .rst bestand wordt ingelezen en opgeslaan
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
          assignfile(FileIMG, NfileIMG);
          reset (fileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              read(fileIMG, Z[i,j]);
          Closefile(Fileimg);
        End;
      Closefile(DocfileImg);

      //De buitenste waarden van het raster worden aangepast
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
    //Deze procedure geeft een nulwaarde aan elk element in een Rraster
    //*****************************************************************
    Procedure SetzeroR(Var z:Rraster);

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
