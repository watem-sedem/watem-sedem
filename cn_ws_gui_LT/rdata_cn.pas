unit RData_CN;

//In de oorspronkelijk unit was er ook een formulier gedefinieerd.
//Dit is hier voorlopig uitgelaten!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

Type
 Rraster = array of array of single ;
 TRaster_Projection=(plane,LATLONG);

 IntegerArray = array of integer;
 FloatArray = array of Double;
 FloatArray2 = array of array of Double;

Procedure GetRfile(var Z:RRaster; Filename:string);
Procedure SetDynamicRData(var Z:RRaster);
procedure SetzeroR(var z:Rraster);
Procedure DisposeDynamicRdata(var Z:RRaster);

var
  NROW,NCOL: integer;
  RES: double;  //fixed resolution for plane proj and dx=dy
  MINX, MAXX, MINY, MAXY, MINZ, MAXZ : double;
  Raster_Projection: TRaster_Projection;
  ncolAR, nrowAR: array of integer;    // array waarin resp. nrow, ncol en
  resAR: array of double;              // res van elke ingelezen kaart wordt opgeslagen
  lengthAR: integer;

implementation

//********************************************************************
//De waarden van de buitenste cellen worden vervangen door de waarden van de
//cellen die een laag meer naar het midden liggen
//********************************************************************
Procedure SetRasterBorders(var Z:RRaster);
var
i,j       : integer;
begin
   Z[0,0]:= Z[1,1];
   Z[0,(ncol+1)] := Z[1,ncol];
   Z[nrow+1,0] := Z[nrow,1];
   Z[nrow+1,ncol+1] := Z[nrow,ncol];
   for j := 1 to ncol do
       begin
         Z[0,j] := Z[1,j];
         Z[(nrow+1),j]:=Z[nrow,j];
       end;
   for  i := 1 to nrow do
        begin
          Z[i,0] := Z[i,1];
          Z[i,ncol+1] := Z[i,ncol];
        end;
end;

//********************************************************************
//In onderstaande regels wordt er geheugen vrij gemaakt voor de verschillende
//arrays die de kaarten voorstellen
//********************************************************************
Procedure SetDynamicRData(var Z:RRaster);
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
Procedure DisposeDynamicRdata(var Z:RRaster);
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

Procedure GetRFile(var Z:RRaster; Filename:string);
var
i,j,hulpgetal: integer;
docfileIMG : textfile;
fileIMG : file of single ;
textfileIMG : textfile ;
docnfileIMG,NfileIMG,dumstr : string;
idrisi32,asciidatatype :boolean;
begin
     dumstr := ExtractFilename(filename);
     if ExtractFileExt(dumstr)='.img' then
       idrisi32:=false
       else idrisi32:=true;

      hulpgetal := length(dumstr)-2;
      delete(dumstr,hulpgetal,3); //De extensie wordt verwijderd
      if Idrisi32 then //Voor Idrisi32 bestanden
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
        if (dumstr='integer') or (dumstr='byte') then
           begin
               showmessage('Data type must be real !!'+chr(13)+'Please Re-enter data');
               closefile(docfileIMG);
               exit; // Exits the current subroutine
           end;
         readln(docfileIMG, dumstr);
         delete (dumstr,1,14);
//Het filetype wordt opgeslagen
            if dumstr='binary' then
               asciidatatype:=false
               else asciidatatype:=true;
        readln(docfileIMG, dumstr);
        delete (dumstr,1,14);
        ncol := strtoint(dumstr);
     // INLEZEN NROWS
     readln(docfileIMG, dumstr);
     delete (dumstr,1,14);
     nrow := strtoint(dumstr);
     readln(docfileIMG, dumstr);
     delete(dumstr,1,14);
     if (dumstr='plane') OR (dumstr='') then Raster_Projection:=plane else Raster_Projection:=LATLONG;
     readln(docfileIMG);
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
     if (res=0.0) then showmessage('Resolution is invalid');
        // Inlezen gegevens

//Er wordt geheugen vrijgemaakt voor de kaart (array) in kwestie
     SetDynamicRData(Z);

//Het .rst bestand wordt ingelezen en opgeslaan
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
         assignfile(FileIMG, NfileIMG);
         reset (fileIMG);
         for i:= 1 to nrow do
           for j:= 1 to ncol do
            read(fileIMG, Z[i,j]);
        Closefile(Fileimg);
       end;
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

end;


//*****************************************************************
//Deze procedure geeft een nulwaarde aan elk element in een Rraster
//*****************************************************************
procedure SetzeroR(var z:Rraster);
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

