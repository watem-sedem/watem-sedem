
Unit RData_CN;


{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, LazFileUtils;

Type
  generic Traster<T> = array of array of T;
  Rraster = specialize Traster<single> ;
  TRaster_Projection = (plane,LATLONG);

  ERasterException = Class(Exception);

  THeader = record
    ncol, nrow: integer;
    res, minx, maxx, miny, maxy, nodata_value: double;
    datafile: string;
    datatype: string; // byte, integer in RDC
    raster_projection: TRaster_Projection;
    asciidatatype: boolean;
    end;

    Procedure GetRfile(Var Z:RRaster; Filename:String);
    Procedure SetDynamicRData(Var Z:RRaster);
    Procedure SetzeroR(Var z:Rraster);
    Procedure SetnodataR(Var z:Rraster);
    Procedure DisposeDynamicRdata(Var Z:RRaster);
    Procedure SetRasterBorders(Var Z:RRaster; value: single);
    Function ReadRDC(Filename: String): THeader;
    Function ReadSGRD(Filename: String): THeader;

    Var 
      NROW,NCOL: integer;
      //fixed resolution for plane proj and dx=dy
      RES, MINX, MAXX, MINY, MAXY : double;
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
    Procedure CopyRasterBorders(Var Z:RRaster);

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

    //*********************************************************************
    // Set raster borders to a specific value, overriding copyrasterborders
    //*********************************************************************
    Procedure SetRasterBorders(Var Z:RRaster; value: single);
    Var
      i,j       : integer;
    Begin
      For j := 0 To ncol Do
        Begin
          Z[0,j] :=value;
          Z[(nrow+1),j] := value;
        End;
      For  i := 0 To nrow Do
        Begin
          Z[i,0] := value;
          Z[i,ncol+1] := value;
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


    //**************************************************************************
    // This function reads an SGRDC file and returns the properties in a header object
    //**************************************************************************
    Function ReadSGRD(Filename: String): THeader;
    var
      header_filename, line, key, value: string;
      line_split: array of string;
      header_file: textfile;
    begin
     header_filename := ExtractFileNameWithoutExt(Filename) + '.sgrd';
     ReadSGRD.datafile:=ExtractFileNameWithoutExt(Filename) + '.sdat';
     Assignfile(header_file, header_filename);
     reset(header_file);
     while not eof(header_file) do
     begin
       readln(header_file, line);
       line_split:= line.Split('=');
       key := line_split[0].trim();
       if length(line_split) < 2 then
         continue;
       value := line_split[1].trim();
       case (key) of
         'DATAFORMAT': readsgrd.datatype:=value;
         'POSITION_XMIN': readsgrd.minx:=StrToFloat(value);
         'POSITION_YMIN': readsgrd.miny:=StrToFloat(value);
         'CELLSIZE': readsgrd.res:=StrToFloat(Value);
         'CELLCOUNT_X': readsgrd.ncol:=StrToInt(Value);
         'CELLCOUNT_Y': readsgrd.nrow:=StrToInt(Value);
         'NODATA_VALUE': readsgrd.nodata_value:=StrToFloat(Value);
       end;

     end;
       // idrisi and cnws reports middle of the cell, while saga uses
       // outer of the cell
       readsgrd.minx += -readsgrd.res/2;
       readsgrd.miny += -readsgrd.res/2;

       readsgrd.maxx:= readsgrd.minx + readsgrd.res * readsgrd.ncol;
       readsgrd.maxy:= readsgrd.miny + readsgrd.res * readsgrd.nrow;

     closefile(header_file);

    end;

    //**************************************************************************
    // This function reads an IMG/RDC file and returns the properties in a header object
    //**************************************************************************

    Function ReadRDC(Filename: String): THeader;
    var
      filename_noext, docNfileIMG, dumstr: string;
      idrisi32 : boolean;
      docfileIMG : textfile;
      i: integer;
    begin
      If ExtractFileExt(Filename)='.img' Then
        idrisi32 := false
      Else idrisi32 := true;

      filename_noext := ExtractFileNameWithoutExt(filename);

      If Idrisi32 Then //Voor Idrisi32 bestanden
        Begin
          docNfileIMG := filename_noext + '.rdc' ;
          ReadRDC.datafile := filename_noext + '.rst';
          // ==> De namen van de betreffende .rst en .rdc bestanden worden nagemaakt
        End
      Else //Voor .IMG bestanden
        Begin
          docNfileIMG := filename_noext + '.doc' ;
          ReadRDC.datafile := filename_noext + '.img';
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

      ReadRDC.datatype := dumstr;

      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      //Het filetype wordt opgeslagen
      readrdc.asciidatatype:= not (dumstr='binary');
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      ReadRDC.ncol := strtoint(dumstr);

      // INLEZEN NROWS
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      ReadRDC.nrow := strtoint(dumstr);
      readln(docfileIMG, dumstr);
      delete(dumstr,1,14);
      If (dumstr='plane') Or (dumstr='') Then ReadRDC.Raster_Projection := plane
      Else Raster_Projection := LATLONG;
      readln(docfileIMG);
      readln(docfileIMG);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      ReadRDC.MINX := strtofloat(dumstr);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      ReadRDC.MAXX := strtofloat(dumstr);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      ReadRDC.MINY := strtofloat(dumstr);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      ReadRDC.MAXY :=  strtofloat(dumstr);
      readln(docfileIMG);
      readln(docfileIMG, dumstr);
      delete(dumstr,1,14);
      ReadRDC.res := strtofloat(dumstr);
      closefile(docfileIMG);
      If (ReadRDC.res=0.0) Then
        Begin
          Raise ERasterException.Create('Error in reading one of the rasters: Resolution is invalid'
          );
        End;

    end;

    //********************************************************************
    //In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
    //en wordt de nodige informatie hieruit gehaald.
    //********************************************************************

    Procedure GetRFile(Var Z:RRaster; Filename:String);

    Var 
      i,j: integer;
      fileIMG : file Of single ;
      textfileIMG : textfile ;
      header: THeader;
    Begin

      header := readrdc(filename);

      If (header.datatype ='integer') Or (header.datatype='byte') Then
        Begin

          Raise ERasterException.Create(
                 'Error in reading one of the rasters: data type must be real, please re-enter data'
          );
        End;

      // Inlezen gegevens

      ncol := header.ncol;
      nrow := header.nrow;
      res := header.res;

      // it would make more sense to keep these in a header object for the future
      minx := header.minx;
      maxx := header.maxx;
      miny := header.miny;
      maxy := header.maxy;


      //Er wordt geheugen vrijgemaakt voor de kaart (array) in kwestie
      SetDynamicRData(Z);

      //Het .rst bestand wordt ingelezen en opgeslaan
      If header.asciidatatype Then
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
          assignfile(FileIMG, header.datafile);
          reset (fileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              read(fileIMG, Z[i,j]);
          Closefile(Fileimg);
        End;


      //De buitenste waarden van het raster worden aangepast
      CopyRasterBorders(Z);

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
      i: integer;
    Begin
        For i:=Low(Z) To High(Z) Do
            Filldword(z[i][0], ncol+2, 0);
    End;

    Procedure SetnodataR(Var z:Rraster);
    Var
      i: integer;
      val: single;
    Begin
      val := -9999;
        For i:=Low(Z) To High(Z) Do
            filldword(z[i][0], ncol+2,  dword(val));
    End;


  End.
