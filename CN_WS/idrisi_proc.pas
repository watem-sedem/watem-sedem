unit Idrisi_Proc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Rdata,Gdata;

procedure writeIdrisifile(pncol,pnrow : Integer;filename:string;z: Rraster);
procedure writeIdrisi32file(pncol,pnrow : Integer;filename:string;z: Rraster);
procedure writeIdrisi32asciifile(pncol,pnrow : Integer;filename:string;z: Rraster);
procedure writeGIdrisifile(pncol,pnrow : Integer;filename:string;z: Graster);
procedure writeSurferfile(pncol,pnrow:integer;filename:string;z:RRaster);
procedure Setzero(var z:Rraster);
procedure SetzeroG(var z:Graster);

implementation

procedure writeSurferfile(pncol,pnrow:integer;filename:string;z:RRaster);
var
i,j:integer;
MINZ,MAXZ,MINXs,MAXXs,MINYs,MAXYs:real;
outputf: textfile;
begin
MINXs:=MINX+RES/2;MAXXs:=MAXX-RES/2;
MINYs:=MINY+RES/2;MAXYs:=MAXY-RES/2;
if ExtractFileExt(filename)='' then filename:=filename+'.GRD';
Assignfile(outputf,filename);rewrite(outputf);
writeln(outputf,'DSAA');
writeln(outputf,pncol, ' ',pnrow);
writeln(outputf, MINXs,' ',MAXXs);
writeln(outputf, MINYs, ' ',MAXYs);
 MAXZ:=-9999999999.99;MINZ:=999999999.99;
 For i:=1 to pnrow do
 for j:=1 to pncol do
  begin
   If Z[i,j]>MAXZ then MAXZ:=Z[i,j];
   If Z[i,j]<MINZ then MINZ:=Z[i,j];
  end;
writeln(outputf,MINZ,' ',MAXZ);
for i:= pnrow downto 1 do
 for j := 1 to pncol do
  writeln(outputf, Z[i,j]);
Closefile(outputf);

end;

procedure Setzero(var z:Rraster);
var
i,j : integer;
begin
for i:= 1 to nrow do
 for j:= 1 to ncol do
   Z[i,j]:=0.0;
end;

procedure SetzeroG(var z:Graster);
var
i,j : integer;
begin
for i:= 1 to nrow do
 for j:= 1 to ncol do
   Z[i,j]:=0;
end;

procedure writeIdrisifile(pncol,pnrow:Integer;filename:string;z: Rraster);
var
dumstr:string;
i,j,hulpgetal : integer;
MAXZ,MINZ:real;
outputf : file of single;
begin
 if ExtractFileExt(filename)='' then filename:=filename+'.img';
 Assignfile(outputf,filename);rewrite(outputf);
 MAXZ:=-9999999999.99;MINZ:=999999999.99;
 For i:=1 to pnrow do
 for j:=1 to pncol do
  begin
   If Z[i,j]>MAXZ then MAXZ:=Z[i,j];
   If Z[i,j]<MINZ then MINZ:=Z[i,j];
  write(outputf, Z[i,j]);
  end;
 // MAXZ:=0.5;
 // MINZ:=-0.5;
 Closefile(outputf);
  hulpgetal := length(filename)-2;      delete(filename,hulpgetal,3);
 assignfile(output, filename+'DOC'); rewrite(output);
 writeln(output,'file title  :'); writeln(output,'data type   : real');
 writeln(output,'file type   : binary'); dumstr := 'columns     : ' + inttostr(pNCOL);
 writeln(output, dumstr); dumstr := 'rows        : ' + inttostr(pNROW);
 writeln(output, dumstr); writeln(output, 'ref. system : plane');
 writeln(output, 'ref. units  : m'); writeln(output, 'unit dist.  : 1');
 dumstr := 'min. X      : ' + floattostr(MINX); writeln(output, dumstr);
 dumstr := 'max. X      : ' + floattostr(MAXX); writeln(output, dumstr);
 dumstr := 'min. Y      : ' + floattostr(MINY); writeln(output, dumstr);
 dumstr := 'max. Y      : ' + floattostr(MAXY); writeln(output, dumstr);
 writeln(output, 'posnn error : unknown'); dumstr := 'resolution  : ' + floattostr(RES);
 writeln(output, dumstr); dumstr := 'min. value  : ' + floattostr(MINZ);
 writeln(output, dumstr); dumstr := 'max. value  : ' + floattostr(MAXZ);
 writeln(output, dumstr); writeln(output,'value units : unspecified');
 writeln(output,'value error : unknown'); writeln(output,'flag value  : none');
 writeln(output,'flag defnn  : none');   writeln(output,'legend cats : 0');
 Closefile(output);
end;

procedure writeIdrisi32file(pncol,pnrow : Integer;filename:string;z: Rraster);
var
dumstr:string;
i,j,hulpgetal : integer;
MAXZ,MINZ:real;
outputf : file of single;
outputdoc : textfile;
begin
 if ExtractFileExt(filename)='' then filename:=filename+'.rst';
 Assignfile(outputf,filename);rewrite(outputf);
 MAXZ:=-9999999999.99;MINZ:=999999999.99;
 For i:=1 to pnrow do
 for j:=1 to pncol do
  begin
   If Z[i,j]>MAXZ then MAXZ:=Z[i,j];
   If Z[i,j]<MINZ then MINZ:=Z[i,j];
  write(outputf, Z[i,j]);
  end;
 // MAXZ:=0.5;
 // MINZ:=-0.5;
 Closefile(outputf);
 hulpgetal := length(filename)-2;      delete(filename,hulpgetal,3);
 assignfile(outputdoc, filename+'rdc'); rewrite(outputdoc);
 writeln(outputdoc,'file format : IDRISI Raster A.1');
 writeln(outputdoc,'file title :'); writeln(outputdoc,'data type   : real');
 writeln(outputdoc,'filetype    : binary'); dumstr := 'columns     : ' + inttostr(pNCOL);
 writeln(outputdoc, dumstr); dumstr := 'rows        : ' + inttostr(pNROW);
 writeln(outputdoc, dumstr); writeln(outputdoc, 'ref. system : plane');
 writeln(outputdoc, 'ref. units  : m'); writeln(outputdoc, 'unit dist.  : 1');
 dumstr := 'min. X      : ' + floattostr(MINX); writeln(outputdoc, dumstr);
 dumstr := 'max. X      : ' + floattostr(MAXX); writeln(outputdoc, dumstr);
 dumstr := 'min. Y      : ' + floattostr(MINY); writeln(outputdoc, dumstr);
 dumstr := 'max. Y      : ' + floattostr(MAXY); writeln(outputdoc, dumstr);
 writeln(outputdoc, 'posnn error : unknown'); dumstr := 'resolution  : ' + floattostr(RES);
 writeln(outputdoc, dumstr); dumstr := 'min. value  : ' + floattostr(MINZ);
 writeln(outputdoc, dumstr); dumstr := 'max. value  : ' + floattostr(MAXZ);
 writeln(outputdoc, dumstr);
 dumstr:='display min : '+floattostr(minz);
 writeln(outputdoc,dumstr);
 dumstr:='display max : '+floattostr(maxz);
 writeln(outputdoc,dumstr);
 writeln(outputdoc,'value units : unspecified');
 writeln(outputdoc,'value error : unknown'); writeln(outputdoc,'flag value  : none');
 writeln(outputdoc,'flag defnn  : none');   writeln(outputdoc,'legend cats : 0');
 Closefile(outputdoc);
end;

procedure writeIdrisi32asciifile(pncol,pnrow : Integer;filename:string;z: Rraster);
var
dumstr:string;
i,j,hulpgetal : integer;
MAXZ,MINZ:real;
outputf : textfile;
outputdoc : textfile;
begin
 if ExtractFileExt(filename)='' then filename:=filename+'.rst';
 Assignfile(outputf,filename);rewrite(outputf);
 MAXZ:=-9999999999.99;MINZ:=999999999.99;
 For i:=1 to pnrow do
 for j:=1 to pncol do
  begin
   If Z[i,j]>MAXZ then MAXZ:=Z[i,j];
   If Z[i,j]<MINZ then MINZ:=Z[i,j];
  write(outputf, Z[i,j]);
  end;
 // MAXZ:=0.5;
 // MINZ:=-0.5;
 Closefile(outputf);
 hulpgetal := length(filename)-2;      delete(filename,hulpgetal,3);
 assignfile(outputdoc, filename+'rdc'); rewrite(outputdoc);
 writeln(outputdoc,'file format : IDRISI Raster A.1');
 writeln(outputdoc,'file title  :'); writeln(output,'datatype    : real');
 writeln(outputdoc,'filetype    : binary'); dumstr := 'columns     : ' + inttostr(pNCOL);
 writeln(outputdoc, dumstr); dumstr := 'rows        : ' + inttostr(pNROW);
 writeln(outputdoc, dumstr); writeln(outputdoc, 'ref. system : plane');
 writeln(outputdoc, 'ref. units  : m'); writeln(outputdoc, 'unit dist.  : 1');
 dumstr := 'min. X      : ' + floattostr(MINX); writeln(outputdoc, dumstr);
 dumstr := 'max. X      : ' + floattostr(MAXX); writeln(outputdoc, dumstr);
 dumstr := 'min. Y      : ' + floattostr(MINY); writeln(outputdoc, dumstr);
 dumstr := 'max. Y      : ' + floattostr(MAXY); writeln(outputdoc, dumstr);
 writeln(outputdoc, 'posnn error : unknown'); dumstr := 'resolution  : ' + floattostr(RES);
 writeln(outputdoc, dumstr); dumstr := 'min. value  : ' + floattostr(MINZ);
 writeln(outputdoc, dumstr); dumstr := 'max. value  : ' + floattostr(MAXZ);
 writeln(outputdoc, dumstr);
 dumstr:='display min : '+floattostr(minz);
 writeln(outputdoc,dumstr);
 dumstr:='display max : '+floattostr(maxz);
 writeln(outputdoc,dumstr);
 writeln(outputdoc,'value units : unspecified');
 writeln(outputdoc,'value error : unknown'); writeln(outputdoc,'flag value  : none');
 writeln(outputdoc,'flag defnn  : none');   writeln(outputdoc,'legend cats : 0');
 Closefile(outputdoc);
end;


procedure writeGIdrisifile(pncol,pnrow:Integer;filename:string;z: Graster);
var
dumstr:string;
i,j : integer;
outputdoc : textfile;
begin
 Assignfile(outputdoc,filename+'.IMG');rewrite(output);
 MAXZ:=-9999999999.99;MINZ:=999999999.99;
 For i:=1 to nrow do
 for j:=1 to ncol do
  begin
   If Z[i,j]>MAXZ then MAXZ:=Z[i,j];
   If Z[i,j]<MINZ then MINZ:=Z[i,j];
  writeln(outputdoc, Z[i,j]);
  end;
 Closefile(outputdoc);
 assignfile(outputdoc, filename+'.DOC'); rewrite(outputdoc);
 writeln(outputdoc,'file title  :'); writeln(outputdoc,'datatype    : real');
 writeln(outputdoc,'filetype    : ascii'); dumstr := 'columns     : ' + inttostr(pNCOL);
 writeln(outputdoc, dumstr); dumstr := 'rows        : ' + inttostr(pNROW);
 writeln(outputdoc, dumstr); writeln(outputdoc, 'ref. system : plane');
 writeln(outputdoc, 'ref. units  : m'); writeln(outputdoc, 'unit dist.  : 1');
 dumstr := 'min. X      : ' + floattostr(MINX); writeln(outputdoc, dumstr);
 dumstr := 'max. X      : ' + floattostr(MAXX); writeln(outputdoc, dumstr);
 dumstr := 'min. Y      : ' + floattostr(MINY); writeln(outputdoc, dumstr);
 dumstr := 'max. Y      : ' + floattostr(MAXY); writeln(outputdoc, dumstr);
 writeln(outputdoc, 'posnn error : unknown'); dumstr := 'resolution  : ' + floattostr(RES);
 writeln(outputdoc, dumstr); dumstr := 'min. value  : ' + floattostr(MINZ);
 writeln(outputdoc, dumstr); dumstr := 'max. value  : ' + floattostr(MAXZ);
 writeln(outputdoc, dumstr); writeln(outputdoc,'value units : unspecified');
 writeln(outputdoc,'value error : unknown'); writeln(outputdoc,'flag value  : none');
 writeln(outputdoc,'flag defnn  : none');   writeln(outputdoc,'legend cats : 0');
 Closefile(outputdoc);
end;


end.

