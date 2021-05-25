
Unit Write_raster;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN, GData_CN, LazFileUtils, strutils;

Procedure writefloatfile(pncol,pnrow : Integer;filename:String;z: Rraster);
Procedure writeSmallintFile(pncol,pnrow : Integer;filename:String;z: Graster);
Procedure writeIdrisi32header(header: THeader);

Implementation

Procedure writeIdrisi32header(header: THeader);
Var
  dumstr: string;
  outputdoc: textfile;

Begin
  assignfile(outputdoc, ExtractFileNameWithoutExt(header.datafile)+'.rdc');
  // De .rdc naam wordt aangemaakt
  rewrite(outputdoc);
  writeln(outputdoc,'file format : IDRISI Raster A.1');
  writeln(outputdoc,'file title :');
  writeln(outputdoc,'data type   : ' + header.datatype);
  writeln(outputdoc,'filetype    : binary');
  dumstr := 'columns     : ' + inttostr(header.ncol);
  writeln(outputdoc, dumstr);
  dumstr := 'rows        : ' + inttostr(header.nrow);
  writeln(outputdoc, dumstr);
  writeln(outputdoc, 'ref. system : plane');
  writeln(outputdoc, 'ref. units  : m');
  writeln(outputdoc, 'unit dist.  : 1');
  dumstr := 'min. X      : ' + floattostr(header.minx);
  writeln(outputdoc, dumstr);
  dumstr := 'max. X      : ' + floattostr(header.maxx);
  writeln(outputdoc, dumstr);
  dumstr := 'min. Y      : ' + floattostr(header.miny);
  writeln(outputdoc, dumstr);
  dumstr := 'max. Y      : ' + floattostr(header.maxy);
  writeln(outputdoc, dumstr);
  writeln(outputdoc, 'posnn error : unknown');
  dumstr := 'resolution  : ' + floattostr(header.res);
  writeln(outputdoc, dumstr);
  dumstr := 'min. value  : ' + floattostr(header.minz);
  writeln(outputdoc, dumstr);
  dumstr := 'max. value  : ' + floattostr(header.maxz);
  writeln(outputdoc, dumstr);
  dumstr := 'display min : '+floattostr(header.minz);
  writeln(outputdoc,dumstr);
  dumstr := 'display max : '+floattostr(header.maxz);
  writeln(outputdoc,dumstr);
  writeln(outputdoc,'value units : unspecified');
  writeln(outputdoc,'value error : unknown');
  writeln(outputdoc,'flag value  : -9999');
  writeln(outputdoc,'flag def''n  : missing data');
  writeln(outputdoc,'legend cats : 0');
  Closefile(outputdoc);

end;

Procedure writeSGRDheader(header: THeader);
var
  outputdoc: textfile;
  dataformat: string;
begin
  assignfile(outputdoc, ExtractFileNameWithoutExt(header.datafile)+'.sgrd');
  rewrite(outputdoc);
  writeln(outputdoc,'NAME'#9'= ' + ExtractFileNameWithoutExt(header.datafile));
  writeln(outputdoc,'DESCRIPTION'#9'= ');
  writeln(outputdoc,'UNIT'#9'= ');
  writeln(outputdoc, 'DATAFILE_OFFSET'#9'= 0');
  if header.datatype='real' then  dataformat := 'FLOAT' else dataformat := 'SMALLINT';
  writeln(outputdoc, 'DATAFORMAT'#9'= ' + dataformat);
  writeln(outputdoc, 'BYTEORDER_BIG'#9'= FALSE');
  writeln(outputdoc,'POSITION_XMIN'#9'= '+ floattostr(header.minx + header.res/2));
  writeln(outputdoc,'POSITION_YMIN'#9'= '+ floattostr(header.miny + header.res/2));
  writeln(outputdoc,'CELLCOUNT_X'#9'= ' + inttostr(header.ncol));
  writeln(outputdoc,'CELLCOUNT_Y'#9'= ' + inttostr(header.nrow));
  writeln(outputdoc,'CELLSIZE'#9'= ' + floattostr(header.res));
  writeln(outputdoc,'ZFACTOR'#9'= 1');
  writeln(outputdoc,'NODATA_VALUE'#9'= -9999.00');
  writeln(outputdoc,'TOPTOBOTTOM'#9'= FALSE');
  Closefile(outputdoc);

  // write projection
  assignfile(outputdoc, ExtractFileNameWithoutExt(header.datafile)+'.prj');
  rewrite(outputdoc);
  Closefile(outputdoc);

end;

///**************************************************************
// Gets header values from the global scope
//***************************************************************

function global_header: THeader;
begin
  global_header.minx:=minx;
  global_header.miny:=miny;
  global_header.maxx:=maxx;
  global_header.maxy:=maxy;
  global_header.ncol:=ncol;
  global_header.nrow:=nrow;
  global_header.res:=res;
end;



//***************************************************************
//Procedure om Raster kaarten van het type float weg te schrijven
//***************************************************************
Procedure writefloatfile(pncol,pnrow : Integer;filename:String; z: Rraster);

Var
  i,j : integer;
  MAXZ,MINZ: real;
  outputf : file Of single;
  header: THeader;
Begin
  If matchstr(ExtractFileExt(filename), idrisi_extensions) Then
    filename := ExtractFileNameWithoutExt(filename)+'.rst'
  else
    filename:= ExtractFileNameWithoutExt(filename)+'.sdat';

  Assignfile(outputf,filename);
  rewrite(outputf);
  //Openen om in te schrijven
  MAXZ := -999999999.99;
  MINZ := 999999999.99;
  For i:=1 To pnrow Do
    // Hiermee worden de kleinste en grootste variabelewaarden
    For j:=1 To pncol Do
      // op de kaart bepaald
      Begin
        write(outputf, Z[i,j]);
        //wegschrijven waarde op kaart
        if Z[i,j] = -9999 then continue;
        If Z[i,j]>MAXZ Then
          MAXZ := Z[i,j];
        If Z[i,j]<MINZ Then
          MINZ := Z[i,j];

      End;
  Closefile(outputf);

  header:= global_header;
  header.minz:=minz;
  header.maxz:=maxz;
  header.datafile:=filename;
  header.datatype:='real';

  If matchstr(ExtractFileExt(filename), saga_extensions) Then
    writeSGRDheader(header)
  else
    writeIdrisi32header(header);
End;

//*****************************************************************
//Procedure om Raster kaarten van het type integer weg te schrijven
//*****************************************************************
Procedure writeSmallintFile(pncol,pnrow : Integer; filename:String; z: Graster);

Var
  i,j : integer;
  MAXZ,MINZ: real;
  outputf : file Of smallint;
  header: THeader;
Begin
  If ExtractFileExt(filename)='' Then filename := filename+'.rst';
  Assignfile(outputf,filename);
  rewrite(outputf);
  MAXZ := -9999999999.99;
  MINZ := 999999999.99;
  For i:=1 To pnrow Do
    For j:=1 To pncol Do
      Begin
        If Z[i,j]>MAXZ Then MAXZ := Z[i,j];
        If Z[i,j]<MINZ Then MINZ := Z[i,j];
        write(outputf, Z[i,j]);
      End;
  Closefile(outputf);

  header:= global_header;
  header.minz:=minz;
  header.maxz:=maxz;
  header.datafile:=filename;
  header.datatype:='integer';
  writeIdrisi32header(header);

End;

End.
