unit GData_CN;
{In deze unit worden kaarten van integers ingelezen}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

Type
  Graster = array of array of smallint;

Procedure GetGFile(var Z:GRaster; Filename:string);
Procedure SetDynamicGData(var Z:GRaster);
procedure SetzeroG(var z:Graster);
Procedure DisposeDynamicGdata(var Z:GRaster);

implementation

uses RData_CN;


//**************************************************************************
//De waarden van de buitenste cellen worden vervangen door de nullen
//WANT de buitenste cellen zijn steeds 0 wanneer het bestand wordt aangemaakt:
//in Lazarus is de eerste cel (0,0) terwijl deze in Idrisi (1,1) is!!!
//**************************************************************************

Procedure SetRasterBorders(var Z:GRaster);
var
i,j       : integer;
begin
Z[0,0]:= 0;
   Z[0,(ncol+1)] := 0;
   Z[nrow+1,0] := 0;
   Z[nrow+1,ncol+1] := 0;
   for j := 1 to ncol do
       begin
         Z[0,j] := 0;
         Z[(nrow+1),j]:=0;
       end;
   for  i := 1 to nrow do
        begin
          Z[i,0] := 0;
          Z[i,ncol+1] := 0;
        end;
end;

//**********************************************
//Er wordt geheugen vrijgemaakt voor de matrix Z
//**********************************************

Procedure SetDynamicGData(var Z:GRaster);
var
i       : integer;
begin
     SetLength(Z,nrow+2);
     for i := Low(Z) to high(Z) do
      Setlength(Z[i],ncol+2);
end;


//***************************************************************************
//Met deze procedure wordt het dynamisch toegekende geheugen weer vrijgegeven
//***************************************************************************
Procedure DisposeDynamicGdata(var Z:GRaster);
var
i       : integer;
begin
for i := Low(Z) to high(Z) do
      Z[i]:=NIL;
 Z:=NIL;
end;


//********************************************************************
//In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
//en wordt de nodige informatie hieruit gehaald.
//********************************************************************

Procedure GetGFile(var Z:GRaster; Filename:string);
var
i,j,hulpgetal: integer;
docfileIMG : textfile;
fileIMG : file of smallint;
textfileIMG : textfile ;
bytefileIMG : file of byte;
docnfileIMG,NfileIMG,dumstr : string;
idrisi32,asciidatatype,bytebinary :boolean;
bytedata : byte;
begin
  dumstr := extractfilename(filename);
      if ExtractFileExt(dumstr)='.img' then  idrisi32:=false else idrisi32:=true;
      hulpgetal := length(dumstr)-2;
      delete(dumstr,hulpgetal,3);
      if Idrisi32 then
      begin
      docNfileIMG := dumstr + 'rdc' ;     NfileIMG := dumstr + 'rst';
      // ==> De namen van de betreffende .rst en .rdc bestanden worden nagemaakt
      end
      else //Voor .IMG bestanden
       begin
        docNfileIMG := dumstr + 'doc' ;     NfileIMG := dumstr + 'img';
       end;

      // INLEZEN NCOLS
      Assignfile(docfileIMG, docNfileIMG); //Een 'filehandle' wordt toegewezen aan de bestanden
      reset(docfileIMG); //Het .rdc bestand wordt geopend om te lezen
      if Idrisi32 then
      for i := 1 to 3 do
       readln(docfileIMG, dumstr);
      delete (dumstr,1,14); //Na 14 tekens staat het data type
        if (dumstr='real')  then
         begin
         showmessage('Data type must be integer !!'+chr(13)+'Please Re-enter data');
         closefile(docfileIMG);
         exit;
         end;
         if dumstr='byte' then
            bytebinary:=true
            else bytebinary:=false;
        readln(docfileIMG, dumstr);
        delete (dumstr,1,14);
        if dumstr='binary' then
           asciidatatype:=false
           else asciidatatype:=true;
        readln(docfileIMG, dumstr);
        delete (dumstr,1,14);
        ncol := strtoint(dumstr); // Number of columns is saved

     // INLEZEN NROWS
     readln(docfileIMG, dumstr);
     delete (dumstr,1,14);
     nrow := strtoint(dumstr); // Number of rows is saved
     readln(docfileIMG, dumstr);
     delete(dumstr,1,14);
     if (dumstr='plane') OR (dumstr='') then Raster_Projection:=plane else Raster_Projection:=LATLONG;
     for i := 1 to 2 do // Er worden 2 lijnen gelezen
         readln(docfileIMG);
     readln(docfileIMG,dumstr);
     delete(dumstr,1,14);
          MINX := strtofloat(dumstr);
     readln(docfileIMG,dumstr);
     delete(dumstr,1,14);
     MAXX :=strtofloat(dumstr);
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
     SetDynamicGData(Z); //Er wordt geheugen vrijgemaakt voor de matrix Z
     if asciidatatype then
      begin
       assignfile(textFileIMG, NfileIMG);
       reset (textfileIMG);
        for i:= 1 to nrow do
        for j:= 1 to ncol do
        read(textfileIMG, Z[i,j]);
        Closefile(textfileimg);
       end
     else
      begin
        if bytebinary then
          begin
           assignfile(byteFileIMG, NfileIMG);
           reset (bytefileIMG);
            for i:= 1 to nrow do
            for j:= 1 to ncol do
              begin
               read(bytefileIMG, bytedata);
               Z[i,j]:=bytedata;
              end;
           Closefile(byteFileimg);
           end
          else
           begin
            assignfile(FileIMG, NfileIMG);
            reset (fileIMG);
            for i:= 1 to nrow do
            for j:= 1 to ncol do
            read(fileIMG,Z[i,j]);
            Closefile(fileimg);
            end;
           end;
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

end;

//*****************************************************************
//Deze procedure geeft een nulwaarde aan elk element in een Graster
//*****************************************************************
procedure SetzeroG(var z:Graster);
var
i,j:integer;
begin
for i:=Low(Z) to High(Z) do
 for j:=Low(Z[i]) to High(Z[i]) do
  begin
    Z[i,j]:=0
  end;
end;


end.

