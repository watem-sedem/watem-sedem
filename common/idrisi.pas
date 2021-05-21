
Unit Idrisi;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN, GData_CN, LazFileUtils;

Procedure writeIdrisi32file(pncol,pnrow : Integer;filename:String;z: Rraster);
Procedure writeGIdrisi32file(pncol,pnrow : Integer;filename:String;z: Graster);

Implementation

Procedure writeIdrisi32header(header: THeader);
Var
  dumstr: string;
  i,j,hulpgetal : integer;
  MAXZ,MINZ: real;
  outputf : file Of single;
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

///**************************************************************
// Gets header values from the global scope
//***************************************************************

function global_header: THeader;
begin
  global_header.minx:=minx;
  global_header.miny:=miny;
  global_header.ncol:=ncol;
  global_header.nrow:=nrow;
  global_header.res:=res;
end;



//***************************************************************
//Procedure om Idrisi kaarten van het type float weg te schrijven
//***************************************************************
Procedure writeIdrisi32file(pncol,pnrow : Integer;filename:String; z: Rraster);

Var 
  dumstr: string;
  i,j,hulpgetal : integer;
  MAXZ,MINZ: real;
  outputf : file Of single;
  outputdoc: textfile;
  header: THeader;
Begin
  If ExtractFileExt(filename)='' Then
    filename := filename+'.rst';
  //De .rst naam wordt aangemaakt

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
  writeIdrisi32header(header);
End;

//*****************************************************************
//Procedure om Idrisi kaarten van het type integer weg te schrijven
//*****************************************************************
Procedure writeGIdrisi32file(pncol,pnrow : Integer; filename:String; z: Graster);

Var 
  dumstr: string;
  i,j,hulpgetal : integer;
  MAXZ,MINZ: real;
  outputf : file Of smallint;
  outputdoc: textfile;
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
