unit SURFACE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Rdata, Gdata, vector,Idrisi_Proc, Math, ReadInParameters;

  // can local procedures and functions be declared in implementation ????////
  // I think we only need Topo_calculations & DistributeFlux & Is_Export_Cell & X_Resolution Y_Resolution

  Procedure Topo_Calculations(ra:TRoutingAlgorithm;Runoff_filename:string;Topo_threshold:double);
  procedure sortbis(var Z:Rvector;iLo,iHi:Integer);
  procedure sort(D:Rraster);
  Procedure DistributeFlux(i,j:integer;var Flux_IN,Flux_OUT: RRaster;FINISH:GRaster;ra:TRoutingalgorithm);
  procedure CalculateSlopeAspect;
  Procedure Calculate_UpstreamArea(ra:TRoutingAlgorithm; var UPAREA:RRaster);
  Procedure CalculateLS(var LS:RRaster;UPAREA:RRaster);
  function CalculateASPECT(i,j:integer):double;
  function CalculateSLOPE(i,j:integer):double;
  function ASPECTB(const resolutie,DTM1,DTM2,DTM3,DTM4:double) : double;
  function Max(a,b:double):double;
  function SlopeDir(dir:double;i,j:integer):double;
  function Is_Export_Cell(i,j:integer):boolean;
  Procedure Write_Resolution(datadir:string);
  function X_Resolution(i,j:integer):double;
  function Y_Resolution(i,j:integer):double;


implementation

var
  H : Rvector;
  const
  WEIGTH : array[1..8] of double=(0.353553,0.5,0.353553,0.5,0.5,0.353553,0.5,0.353553);
  Earth_DegToMeter = 111319.444444444;

function Is_Export_Cell(i,j:integer):boolean;
begin
if PRC[i,j]<1 then result:=true else result:=false;
end;

Procedure Sort_DEMTopology(ra:TRoutingAlgorithm;runoff_filename:string;Topo_Threshold:double);
var
i,j:integer;
begin
DTM_TOPCOR:=DTM;
GetRFile(RUNOFF,runoff_filename);
if ra=sdra then
begin
 for i:=1 to nrow do
   for j:=1 to ncol do
   RUNOFF[i,j]:=-1.0*(RUNOFF[i,j]);
 sort(RUNOFF);
end
else
begin
 for i:=1 to nrow do
   for j:=1 to ncol do
   DTM_TOPCOR[i,j]:=DTM[i,j]-1.0*(RUNOFF[i,j]/Topo_Threshold);
 sort(DTM_TOPCOR);
end;
 DisposeDynamicRdata(RUNOFF);
end;

function X_Resolution(i,j:integer):double;
var
Yresdeg,Xresdeg,longitude:double;
begin
if Raster_Projection=plane then
 begin
   result:=(MAXX-MINX)/ncol;
 end
  else
   begin
    Yresdeg:=(MAXY-MINY)/nrow;
    Xresdeg:=(MAXX-MINX)/ncol;
    longitude:= MAXY-i*Yresdeg; // rowcount starts left top corner of raster
    result:=Xresdeg*Earth_DegToMeter*cos(degtorad(longitude)); //
   end;
end;

function Y_Resolution (i,j:integer):double;
begin
 if Raster_Projection=plane then
 begin
   result:=(MAXY-MINY)/nrow;
 end
  else
   begin
     result:=(MAXY-MINY)/nrow*Earth_DegToMeter; //MAX & MIN in degrees  YRES in m
   end;
end;

Procedure Write_Resolution(datadir:string);
var
   Dumraster:RRaster;
   i,j:integer;
begin
 SetDynamicRData(Dumraster);
 for i:=1 to nrow do
 for j:=1 to ncol do
  begin
   Dumraster[i,j]:= X_Resolution(i,j);
  end;
  Writeidrisi32file(ncol,nrow,datadir+'xres.rst',Dumraster);
 DisposeDynamicRdata(Dumraster);
end;

Procedure CalculateLS(var LS:RRaster;UPAREA:RRaster);
var
i,j     : integer;
exp,Sfactor,Lfactor,adjust,B,locres : double;
begin


      for i:=1 to nrow do
      begin
      for j:=1 to ncol do
      begin  // begin matrix loop
      If PRC[i,j]<1 then continue;
      if Raster_projection=plane then locres:=RES
      else locres := (X_Resolution(i,j)+Y_Resolution(i,j))/2.0; //else fixed res is used
      ADJUST := (ABS(cos(aspect[i,j]))+ABS(sin(aspect[i,j])));

      B:=(sin(slope[i,j])/0.0896)/((3.0*power(sin(slope[i,j]),0.8))+0.56);
      EXP:=B/(B+1);

      Sfactor:=-1.5 + 17/(1+power(2.718281828,(2.3-6.1*sin(slope[i,j]))));
      Lfactor:=(POWER((Uparea[i,j]+sqr(locres)),EXP+1)-POWER(Uparea[i,j],EXP+1))/
             (POWER(ADJUST,EXP)*POWER(locres,EXP+2)*POWER(22.13,EXP));
      LS[i,j]:=Sfactor*Lfactor;
      end;    // end matrix loop
      end;

end;  // --------- end procedure CalculateLS------------------------------------

function SlopeDir(dir:double;i,j:integer):double; // nulrichting is grid noorden
var
G,H:double;
begin
  G:=(-DTM[i,j-1]+DTM[i,j+1])/(2*X_Resolution(i,j));
  H:=(DTM[i-1,j]-DTM[i+1,j])/(2*Y_Resolution(i,j));
  result:=(H*cos(dir)+G*sin(dir));
end;

function CalculateSLOPE(i,j:integer):double;   // Slope based on gridcells within same parcel PRC[]
var
DIFFX,DIFFY:double;
begin

If PRC[i-1,j]<>PRC[i,j] then
 DIFFX:=abs(DTM[i,j]-DTM[i+1,j])/(Y_Resolution(i,j))
else
begin
 If PRC[i+1,j]<>PRC[i,j] then
 DIFFX:=abs(DTM[i-1,j]-DTM[i,j])/(Y_Resolution(i,j))
else
 DIFFX:=abs(DTM[i-1,j]-DTM[i+1,j])/(2*Y_Resolution(i,j))
end;
If PRC[i,j-1]<>PRC[i,j] then
 DIFFY:=abs(DTM[i,j]-DTM[i,j+1])/(X_Resolution(i,j))
else
begin
 If PRC[i,j+1]<>PRC[i,j] then
  DIFFY:=abs(DTM[i,j-1]-DTM[i,j])/(X_Resolution(i,j))
else  DIFFY:=abs(DTM[i,j-1]-DTM[i,j+1])/(2*X_Resolution(i,j));
end;
 result:=arctan(sqrt(sqr(DIFFX)+sqr(DIFFY)));              //helling in radialen
end;

function CalculateASPECT(i,j:integer) : double;
var
DIFFX,DIFFY,SLOPEX,SLOPEY,ASP,GRAD : double;
BEGIN
  ASP := 0;
  DIFFX := DTM[i-1,j]-DTM[i+1,j]; DIFFY := DTM[i,j-1]-DTM[i,j+1];
  SLOPEX := DIFFX/Y_Resolution(i,j); SLOPEY := DIFFY/X_Resolution(i,j);
    if SLOPEX = 0.0 then  begin
        if diffy > 0.0 then ASP := 90.0;
        if diffy < 0.0 then ASP := 270.0;  end
     else begin
          if slopey = 0.0 then begin
          if diffx > 0.0 then ASP := 180.0;if diffx < 0.0 then ASP := 0.0;
          end else  begin
          GRAD := SLOPEX/SLOPEY; asp := -(arctan(GRAD)*(180.0/PI));
          if (diffx > 0.0) and (diffy > 0.0) then  ASP := ABS(ASP) + 90.0
          else begin if (diffx > 0.0) and (diffy < 0.0) then
             ASP := 270.0 - ASP  else  begin
                 if (diffx < 0.0) and (diffy<0.0) then ASP := 270.0 - ASP
                 else  ASP := 90.0 - ASP;  end;  end;    end;
      end;
result:=ASP*(PI/180.0);                                    // aspect in radialen
END;

procedure CalculateSlopeAspect;
var
i,j : integer;
begin
for i:=1 to nrow do
 for j:=1 to ncol do
  begin
   Slope[i,j]:= CalculateSlope(i,j);
   Aspect[i,j]:=CalculateAspect(i,j);
  end;
end;

procedure sort(D:Rraster);
var
number1,i,j: integer;
begin
Setlength(H,nrow*ncol+1);
Setlength(ROW,nrow*ncol+1);
Setlength(COLUMN,nrow*ncol+1);
number1:=0;
for i := 1 to nrow do
 for j := 1 to ncol do begin
 Inc(number1);
 H[number1]:= D[i,j];
 ROW[number1]:=i;
 COLUMN[number1]:=j;
 end;
sortbis(H,1,nrow*ncol);
H:=NIL;
end;

procedure sortbis(var Z:Rvector;iLo,iHi:Integer);
var
    helpcol, helprow,Lo, Hi: Integer;
    Mid,T : double;
begin
    Lo := iLo;
    Hi := iHi;
    Mid := Z[(Lo + Hi) div 2];
    repeat
      while Z[Lo] < Mid do Inc(Lo);
      while Z[Hi] > Mid do Dec(Hi);
      if Lo <= Hi then
      begin
        T := Z[Lo];
        Z[Lo] := Z[Hi];
        Z[Hi] := T;
        helprow := row[lo];
        row[lo]:=row[hi];
        row[hi]:=helprow;
        helpcol := column[lo];
        column[lo]:=column[hi];
        column[hi]:=helpcol;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then Sortbis(Z, iLo, Hi);
    if Lo < iHi then Sortbis(Z, Lo, iHi);
end;

Procedure DistributeFlux(i,j:integer;var Flux_IN,Flux_OUT: RRaster;FINISH:GRaster;ra:TRoutingalgorithm);
var
MINIMUM : double;
ROWMIN,COLMIN,K,L : integer;
PART : array[1..8] of extended;
sum,D,flux,local_slope:double;
number:integer;
begin
If ra=sdra then           //---------------------- STEEPEST DESCENT--------------
BEGIN
            ROWMIN:= 0;
            COLMIN:= 0;
            MINIMUM := 9999999.0;
	    for K := -1 to 1 do
	    for L := -1 to 1 do
            begin
	         IF ((K=0)AND(L=0)) then CONTINUE;
                 IF (abs(K)+abs(L))=1 THEN D:=1 ELSE D:=1.414;
                 local_slope:= (DTM[I+K,J+L]-DTM[I,J])/D;
//	         IF ((DTM[I+K,J+L]<MINIMUM)AND(DTM[I+K,J+L]<DTM[I,J])AND(FINISH[I+K,J+L]=0))THEN
	         IF ((local_slope<MINIMUM)AND(DTM[I+K,J+L]<=DTM[I,J])AND(FINISH[I+K,J+L]=0))THEN
                 begin
	              MINIMUM := local_slope;//DTM[I+K,J+L];
	              ROWMIN := K;
	              COLMIN := L;
	         end;
            end;

        IF ((ROWMIN <>0)OR(COLMIN<>0))then
        BEGIN
          IF (PRC[i+ROWMIN,j+COLMIN]=PRC[i,j]) THEN
            begin
              Flux_IN[i+ROWMIN,j+COLMIN]:=Flux_IN[i+ROWMIN,j+COLMIN]+Flux_OUT[i,j];
            end
             ELSE
              begin
              Flux_IN[i+ROWMIN,j+COLMIN]:=Flux_IN[i+ROWMIN,j+COLMIN]+Flux_OUT[i,j]*TFCA/100.0;
              end;
        END;

END; // end STEEPEST DESCENT
if ra=mfra then  //------------------------------ MULTIPLE FLOW-------------------
BEGIN


  number:=0; sum:=0.0;
  FOR k:=-1 to 1 do
  FOR l := -1 to 1 do begin
    IF (k=0) and (l=0) then continue;
    number:=number+1;
    IF DTM[i+k,j+l]<=DTM[i,j] then
    BEGIN
      IF (ABS(k)=1)and(ABS(l)=1) then D:=RES*SQRT(2.0) else D:=RES;   //check if this should be adapted for MF in case of LATLONG
      IF (PRC[i,j]=0) then PART[number]:=0.0 else
      begin
       IF FINISH[i+k,j+l]<>1 then
       begin
         PART[number]:=(DTM_TOPCOR[i,j]-DTM_TOPCOR[i+k,j+l])/D;
         SUM:=SUM+PART[number]*WEIGTH[number];
       end
       else part[number]:=0.0;
      end;
    END
    ELSE
    PART[number]:=0.0;
  END;
  number:=0;
  FOR k:=-1 to 1 do
  FOR l := -1 to 1 do begin
    IF (k=0) and (l=0) then continue;
    number:=number+1;
    IF (k=0) and (l=0) then continue;
    IF  (part[number]>0.0) then
    begin
      flux:=((FLUX_OUT[i,j])*PART[number]*WEIGTH[number])/SUM;
      IF PRC[i+k,j+l]<>PRC[i,j] then
         flux:=TFCA*flux/100;
      Flux_IN[i+k,j+l]:=Flux_IN[i+k,j+l]+flux;
    end;

  END;

END;                                           // END MULTIPLE FLOW-------------

end;

Procedure Calculate_UpstreamArea(ra:TRoutingAlgorithm; var UPAREA:RRaster);
var
teller,i,j : integer;
FINISH : GRaster;
Fluxout: RRaster;
begin
SetDynamicRdata(Fluxout);
SetDynamicGdata(FINISH);

SetzeroG(FINISH);
Setzero(UPAREA);

for teller:= ncol*nrow downto 1 do begin // begin lus
  i:=row[teller];
  j:=column[teller];
  Fluxout[i,j]:=UPAREA[i,j]+X_resolution(i,j)*Y_resolution(i,j);        // X_res * Y_res = oppervlakte 1 cel
  If PRC[i,j]=0 then continue;
  DistributeFlux(i,j,UPAREA,Fluxout,FINISH,ra);       // volgende cellen worden geïdentificeerd en uparea in deze cellen wordt berekend
  FINISH[I,J] := 1;
  UPAREA[i,j]:=UPAREA[i,j]+(X_Resolution(i,j)*Y_Resolution(i,j))/2.0;      // waarom nog halve cel bijtellen nadat flux is gepasseerd
 end; // end matrix loop

DisposeDynamicGdata(FINISH);DisposeDynamicRdata(Fluxout);
end;


function ASPECTB(const resolutie,DTM1,DTM2,DTM3,DTM4:double) : double;
var
DIFFX,DIFFY,SLOPEX,SLOPEY,ASP,GRAD : double;
BEGIN
  ASP := 0;
  DIFFX := DTM1-DTM3; DIFFY := DTM2-DTM4;
  SLOPEX := DIFFX/resolutie; SLOPEY := DIFFY/resolutie;
    if SLOPEX = 0.0 then  begin
        if diffy > 0.0 then ASP := 90.0;
        if diffy < 0.0 then ASP := 270.0;  end
     else begin
          if slopey = 0.0 then begin
          if diffx > 0.0 then ASP := 180.0;if diffx < 0.0 then ASP := 0.0;
          end else  begin
          GRAD := SLOPEX/SLOPEY; asp := -(arctan(GRAD)*(180/PI));
          if (diffx > 0.0) and (diffy > 0.0) then  ASP := ABS(ASP) + 90.0
          else begin if (diffx > 0.0) and (diffy < 0.0) then
             ASP := 270.0 - ASP  else  begin
                 if (diffx < 0.0) and (diffy<0.0) then ASP := 270.0 - ASP
                 else  ASP := 90.0 - ASP;  end;  end;    end;
      end;
result:=ASP*(PI/180.0);     // aspect in radialen
END;

function Max(a,b:double):double;
begin
If a>=b then result:=a else result:=b;
end;

Procedure Topo_Calculations(ra:TRoutingAlgorithm;Runoff_filename:string;Topo_threshold:double);
begin
CalculateSlopeAspect;
Sort_DEMTopology(ra,Runoff_filename,Topo_threshold);
Calculate_UpstreamArea(ra,UPAREA);
CalculateLS(LS,UPAREA);
end;


end.

