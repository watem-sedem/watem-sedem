
Unit Idrisi;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, RData_CN, GData_CN;

Procedure writeIdrisi32file(pncol,pnrow : Integer;filename:String;z: Rraster);
Procedure writeGIdrisi32file(pncol,pnrow : Integer;filename:String;z: Graster);

Implementation

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
Begin
  If ExtractFileExt(filename)='' Then
    filename := filename+'.rst';
  //De .rst naam wordt aangemaakt

  Assignfile(outputf,filename);
  rewrite(outputf);
  //Openen om in te schrijven
  MAXZ := -9999999999.99;
  MINZ := 999999999.99;
  For i:=1 To pnrow Do
    // Hiermee worden de kleinste en grootste variabelewaarden
    For j:=1 To pncol Do
      // op de kaart bepaald
      Begin
        If Z[i,j]>MAXZ Then
          MAXZ := Z[i,j];
        If Z[i,j]<MINZ Then
          MINZ := Z[i,j];
        write(outputf, Z[i,j]);
        //wegschrijven waarde op kaart
      End;
  Closefile(outputf);

  hulpgetal := length(filename)-2;
  delete(filename,hulpgetal,3);
  //De extensie wordt van de filename geknipt
  assignfile(outputdoc, filename+'rdc');
  // De .rdc naam wordt aangemaakt
  rewrite(outputdoc);
  writeln(outputdoc,'file format : IDRISI Raster A.1');
  writeln(outputdoc,'file title :');
  writeln(outputdoc,'data type   : real');
  writeln(outputdoc,'filetype    : binary');
  dumstr := 'columns     : ' + inttostr(pNCOL);
  writeln(outputdoc, dumstr);
  dumstr := 'rows        : ' + inttostr(pNROW);
  writeln(outputdoc, dumstr);
  writeln(outputdoc, 'ref. system : plane');
  writeln(outputdoc, 'ref. units  : m');
  writeln(outputdoc, 'unit dist.  : 1');
  dumstr := 'min. X      : ' + floattostr(MINX);
  // Waar komen de min en max waarden vandaan???
  writeln(outputdoc, dumstr);
  dumstr := 'max. X      : ' + floattostr(MAXX);
  writeln(outputdoc, dumstr);
  dumstr := 'min. Y      : ' + floattostr(MINY);
  writeln(outputdoc, dumstr);
  dumstr := 'max. Y      : ' + floattostr(MAXY);
  writeln(outputdoc, dumstr);
  writeln(outputdoc, 'posnn error : unknown');
  dumstr := 'resolution  : ' + floattostr(RES);
  writeln(outputdoc, dumstr);
  dumstr := 'min. value  : ' + floattostr(MINZ);
  writeln(outputdoc, dumstr);
  dumstr := 'max. value  : ' + floattostr(MAXZ);
  writeln(outputdoc, dumstr);
  dumstr := 'display min : '+floattostr(minz);
  writeln(outputdoc,dumstr);
  dumstr := 'display max : '+floattostr(maxz);
  writeln(outputdoc,dumstr);
  writeln(outputdoc,'value units : unspecified');
  writeln(outputdoc,'value error : unknown');
  writeln(outputdoc,'flag value  : none');
  writeln(outputdoc,'flag defnn  : none');
  writeln(outputdoc,'legend cats : 0');
  Closefile(outputdoc);
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
  hulpgetal := length(filename)-2;
  delete(filename,hulpgetal,3);
  assignfile(outputdoc, filename+'rdc');
  rewrite(outputdoc);
  writeln(outputdoc,'file format : IDRISI Raster A.1');
  writeln(outputdoc,'file title :');
  writeln(outputdoc,'data type   : integer');
  writeln(outputdoc,'filetype    : binary');
  dumstr := 'columns     : ' + inttostr(pNCOL);
  writeln(outputdoc, dumstr);
  dumstr := 'rows        : ' + inttostr(pNROW);
  writeln(outputdoc, dumstr);
  writeln(outputdoc, 'ref. system : plane');
  writeln(outputdoc, 'ref. units  : m');
  writeln(outputdoc, 'unit dist.  : 1');
  dumstr := 'min. X      : ' + floattostr(MINX);
  writeln(outputdoc, dumstr);
  dumstr := 'max. X      : ' + floattostr(MAXX);
  writeln(outputdoc, dumstr);
  dumstr := 'min. Y      : ' + floattostr(MINY);
  writeln(outputdoc, dumstr);
  dumstr := 'max. Y      : ' + floattostr(MAXY);
  writeln(outputdoc, dumstr);
  writeln(outputdoc, 'posnn error : unknown');
  dumstr := 'resolution  : ' + floattostr(RES);
  writeln(outputdoc, dumstr);
  dumstr := 'min. value  : ' + floattostr(MINZ);
  writeln(outputdoc, dumstr);
  dumstr := 'max. value  : ' + floattostr(MAXZ);
  writeln(outputdoc, dumstr);
  dumstr := 'display min : '+floattostr(minz);
  writeln(outputdoc,dumstr);
  dumstr := 'display max : '+floattostr(maxz);
  writeln(outputdoc,dumstr);
  writeln(outputdoc,'value units : unspecified');
  writeln(outputdoc,'value error : unknown');
  writeln(outputdoc,'flag value  : none');
  writeln(outputdoc,'flag defnn  : none');
  writeln(outputdoc,'legend cats : 0');
  Closefile(outputdoc);
End;

End.
