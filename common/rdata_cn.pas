
Unit RData_CN;


{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, LazFileUtils, strutils;

Type
  generic Traster<T> = class
       r: array of array of T;
       ncol, nrow: integer;
      constructor Create(nrow_c, ncol_c: integer);
      destructor Destroy;
      function getItem(row, col:integer): T;
      procedure setItem(row, col:integer; value: T);
      property item[row, col:Integer]:T read getItem write setItem; default;
      procedure CopyRasterBorders;
      procedure SetRasterBorders;
  end;

  Rraster = specialize Traster<single> ;
  TRaster_Projection = (plane,LATLONG);

  ERasterException = Class(Exception);

  THeader = record
    ncol, nrow: integer;
    res, minx, maxx, miny, maxy, nodata_value, minz, maxz: double;
    datafile: string;
    datatype: string; // byte, integer in RDC
    raster_projection: TRaster_Projection;
    asciidatatype: boolean;
    toptobottom: boolean;
    end;

    Procedure GetRfile(Var Z:RRaster; Filename:String);
    Procedure SetDynamicRData(Var Z:RRaster);
    Procedure SetzeroR(Var z:Rraster);
    Procedure SetnodataR(Var z:Rraster);
    Procedure DisposeDynamicRdata(Var Z:RRaster);
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

    const
      saga_extensions: array[0..1] of UnicodeString = ('.sdat', '.sgrd');
      idrisi_extensions: array[0..1] of UnicodeString = ('.rst', '.rdc');
      // note that in theory also .doc/.img exists for idrisi, but we don't use them
    Implementation

    constructor TRaster.Create(nrow_c, ncol_c: integer);
    begin
      nrow := nrow_c;
      ncol := ncol_c;
      // two extra lines are added around the grid
      setlength(self.r, nrow+2, ncol + 2);
    end;

    destructor TRaster.Destroy;
    begin
      setlength(self.r, 0);
      self.r:=nil;
    end;
    function TRaster.getItem(row, col: integer): T;
    begin
      getItem:= self.r[row, col];
    end;

    procedure TRaster.setItem(row, col:integer; value: T);
    begin
      self.r[row, col] := value;
    end;

    //********************************************************************
    //De waarden van de buitenste cellen worden vervangen door de waarden van de
    //cellen die een laag meer naar het midden liggen
    //********************************************************************
    Procedure TRaster.CopyRasterBorders;

    Var
      i,j       : integer;
    Begin
      r[0,0] := r[1,1];
      r[0,(ncol+1)] := r[1,ncol];
      r[nrow+1,0] := r[nrow,1];
      r[nrow+1,ncol+1] := r[nrow,ncol];
      For j := 1 To ncol Do
        Begin
          r[0,j] := r[1,j];
          r[(nrow+1),j] := r[nrow,j];
        End;
      For  i := 1 To nrow Do
        Begin
          r[i,0] := r[i,1];
          r[i,ncol+1] := r[i,ncol];
        End;
    End;


   // Set raster borders (outside actual grid domain) to zero

    Procedure TRaster.SetRasterBorders;

    Var
      i,j       : integer;
    Begin
      For j := 0 To ncol+1 Do
        Begin
          r[0,j] := 0;
          r[(nrow+1),j] := 0;
        End;
      For  i := 1 To nrow Do
        Begin
          r[i,0] := 0;
          r[i,ncol+1] := 0;
        End;
    End;



    //********************************************************************
    //In onderstaande regels wordt er geheugen vrij gemaakt voor de verschillende
    //arrays die de kaarten voorstellen
    //********************************************************************
    Procedure SetDynamicRData(Var Z:RRaster);
    Begin
      Z := RRaster.Create(nrow, ncol);
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
      header_filename, line, key, value, dataformat: string;
      line_split: array of string;
      header_file: textfile;
    begin
     header_filename := ExtractFileNameWithoutExt(Filename) + '.sgrd';
     ReadSGRD.datafile:=ExtractFileNameWithoutExt(Filename) + '.sdat';
     ReadSGRD.asciidatatype:=false;
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
         'DATAFORMAT': dataformat:=value;
         'POSITION_XMIN': readsgrd.minx:=StrToFloat(value);
         'POSITION_YMIN': readsgrd.miny:=StrToFloat(value);
         'CELLSIZE': readsgrd.res:=StrToFloat(Value);
         'CELLCOUNT_X': readsgrd.ncol:=StrToInt(Value);
         'CELLCOUNT_Y': readsgrd.nrow:=StrToInt(Value);
         'NODATA_VALUE': readsgrd.nodata_value:=StrToFloat(Value.split(';')[0]); // if a range of nodata values is present we take the lowest one
         'TOPTOBOTTOM': readsgrd.toptobottom:=StrToBool(Value);
       end;

     end;
      case (dataformat) of
          'SHORTINT': readsgrd.datatype := 'smallint';
          'INTEGER': readsgrd.datatype := 'integer';
          'FLOAT':  readsgrd.datatype := 'single';
      else raise Exception.Create('invalid datatype '+ dataformat + ' in ' + filename);
      end;
       // idrisi and cnws reports outer of the cell, while saga uses
       // middle of the cell
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
      filename_noext, docNfileIMG, dumstr, orig_datatype: string;
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

      orig_datatype := dumstr;
      case (orig_datatype) of
         'byte':      readrdc.datatype:= 'byte';
         'integer':   readrdc.datatype:= 'smallint';
         'real':     readrdc.datatype:= 'single';
      else  raise Exception.Create('invalid datatype '+ orig_datatype + ' in ' + filename);
      end;

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
      If (dumstr='latlong') or (dumstr='lat/long')
      then ReadRDC.Raster_Projection:= LATLONG
      Else Raster_Projection := PLANE;
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
      READRDC.toptobottom:=True;

    end;

    //********************************************************************
    //In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
    //en wordt de nodige informatie hieruit gehaald.
    //********************************************************************

    Procedure GetRFile(Var Z:RRaster; Filename:String);

    Var 
      i,j, irow: integer;
      fileIMG : file Of single ;
      textfileIMG : textfile ;
      header: THeader;

    Begin

     if  matchstr(ExtractFileExt(filename), saga_extensions) then
       header:= ReadSGRD(filename)
     else
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
              if header.toptobottom then irow:=i else irow:=nrow-i+1;
              read(textfileIMG, Z.r[irow,j]);
          Closefile(textfileimg);
        End
      Else
        Begin
          assignfile(FileIMG, header.datafile);
          reset (fileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              begin
                if header.toptobottom then irow:=i else irow:=nrow-i+1;
               read(fileIMG, Z.r[irow,j]);
              end;
          Closefile(Fileimg);
        End;


      //De buitenste waarden van het raster worden aangepast
      Z.CopyRasterBorders;

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
        For i:=Low(Z.r) To High(Z.r) Do
            Filldword(z.r[i][0], ncol+2, 0);
    End;

    Procedure SetnodataR(Var z:Rraster);
    Var
      i: integer;
      val: single;
    Begin
      val := -9999;
        For i:=Low(Z.r) To High(Z.r) Do
            filldword(z.r[i][0], ncol+2,  dword(val));
    End;


  End.
