unit Raster_calculations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Math, GData_CN, RData_CN, Idrisi, ReadInParameters, Forms;


procedure sort(D:Rraster);
procedure sortbis(var Z:Rvector; iLo,iHi:Integer);
procedure CalculateSlopeAspect;
Function LogReg(i,j:integer):double;
function CalculateSLOPE(i,j:integer):double;
function CalculateASPECT(i,j:integer) : double;
function IsRiver(i,j: Integer): boolean;
Function SlopeDir(dir:double;i,j:integer;DTM:Rraster):double;
procedure Calculate_routing(var Routing: TRoutingArray);
Procedure DistributeRiver_Routing(i,j:integer; var FINISH:GRaster);
Procedure DistributeTilDirEvent_Routing(i,j:integer; var FINISH:GRaster; Topo:boolean);
function intArrayIsEqual (inputArray: array of Integer): boolean;
function doubArrayIsEqual (inputarray: array of double): boolean;
function X_Resolution(i,j:integer):double;
function Y_Resolution(i,j:integer):double;
Procedure Calculate_UpstreamArea(var UPAREA:RRaster);
Procedure CalculateLS(var LS:RRaster;UPAREA:RRaster);
Procedure DistributeFlux_LS(i,j:integer;var Flux_IN,Flux_OUT: RRaster);
Procedure DistributeFlux_Sediment(i,j:integer;var Flux_IN,Flux_OUT: RRaster);
Procedure Topo_Calculations;
//Procedure dtmcheck;



implementation

 //var

 const
 WEIGTH : array[1..8] of double=(0.353553,0.5,0.353553,0.5,0.5,0.353553,0.5,0.353553);
  Earth_DegToMeter = 111319.444444444;

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

//******************************************************************************
// This procedure calculates the routing of runoff though the landscape by examining
// to which cell(s) every cell drains. This way the routing only needs to be
// calculated once, at the beginning of the program (for the time-dependent version).
//******************************************************************************
procedure Calculate_routing(var Routing: TRoutingArray);
var
Teller: integer;
Finish: GRaster;   //A cell receives a value of 1 after it had been treated
begin
SetDynamicGdata(Finish);
SetzeroG(Finish);

//Dimensions of the Routing matrix are set so that they are equal to the input maps
SetLength(Routing,nrow+1);
for i := Low(Routing) to high(Routing) do
Setlength(Routing[i],ncol+1);
//The values in all records are filled for the routing matrix
for i:= 1 to nrow do
    for j:= 1 to ncol do
    begin
        Routing[i,j].Target1Row := -99;
        Routing[i,j].Target1Col := -99;
        Routing[i,j].Target2Row := -99;
        Routing[i,j].Target2Col := -99;
        Routing[i,j].Part1 := 0.0;
        Routing[i,j].Part2 := 0.0;
        Routing[i,j].Distance1 := 0.0;
        Routing[i,j].Distance2 := 0.0;
end;
//For every cell in the catchment the target cell(s) are determined
for teller:=nrow*ncol downto 1 do
begin
i:=row[teller]; //row contain the dtm rownumbers from low to high
j:=column[teller]; //Same for columns
If PRC[i,j]=0 then continue;
if IsRiver(i,j) then //Routing procedure for rivers (once water has entered a river it has to stay in the river)
 DistributeRiver_Routing(i, j, Finish)
 else //Routing procedure for all other cells
  DistributeTilDirEvent_Routing(i,j, FINISH, Topo);
 end;
end;

//******************************************************************************
// In the two scripts below the DTM is sorted from low to high
//******************************************************************************
procedure sort(D:Rraster);
var
number1,i,j: integer;
H: Rvector;
begin
Setlength(H,nrow*ncol+1);
Setlength(ROW,nrow*ncol+1);
Setlength(COLUMN,nrow*ncol+1);
number1:=0;
for i := 1 to nrow do         //The DTM is read row per row (from l to r), for each next cell that is
  for j := 1 to ncol do       //read number1 is increased with 1: this is the cell identifier
  begin
 Inc(number1);
 H[number1]:= D[i,j]; //H is a vector with the height of each cell
 ROW[number1]:=i;     //The row of each cell is stored
 COLUMN[number1]:=j;  //Same for column
 end;
sortbis(H,1,nrow*ncol); //Procedure with the actual sorting
H:=NIL;
end;

procedure sortbis(var Z:Rvector; iLo,iHi:Integer);
var
    helpcol, helprow,Lo, Hi: Integer;
    Mid,T : double;
begin
    Lo := iLo; //iLo is 1 at the start of this procedure
    Hi := iHi; //iHi is the number of elements in the DTM (nrow*ncol)
    Mid := Z[(Lo + Hi) div 2]; //Mid is the height of the central (middle) element in the DTM
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

//******************************************************************************
//In this procedure the procedures to calculate the slope and aspect are called
//******************************************************************************
procedure CalculateSlopeAspect;
var
i,j: integer;
begin
SetDynamicRdata(Slope);
SetDynamicRdata(Aspect);
for i:=1 to nrow do
 for j:=1 to ncol do
  begin
   Slope[i,j]:= CalculateSlope(i,j);
   Aspect[i,j]:=CalculateAspect(i,j);
  end;
end;

//Slope calculation
function CalculateSLOPE(i,j:integer):double;
var
DIFFX,DIFFY:double;
begin

//Codes versie Jeroen:

 {If PRC[i-1,j]<>PRC[i,j] then
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
else
DIFFY:=abs(DTM[i,j-1]-DTM[i,j+1])/(2*X_Resolution(i,j));
end;
 result:=arctan(sqrt(sqr(DIFFX)+sqr(DIFFY)));}               //slope in radians

//Codes versie Bastiaan:

DIFFX := abs(DTM[i-1,j] - DTM[i + 1,j]) / (2 * RES);
DIFFY := abs(DTM[i,j-1] - DTM[i,j + 1]) / (2 * RES);
result := arctan(sqrt(sqr(DIFFX) + sqr(DIFFY))); //helling in radialen

end;

//Aspect calculation
function CalculateASPECT(i,j:integer) : double;
var
Diffx,Diffy,Slopex,Slopey,Asp,Grad: double;
BEGIN
  ASP := 0;
  Diffx := DTM[i-1,j]-DTM[i+1,j];
  Diffy := DTM[i,j-1]-DTM[i,j+1];
  Slopex := Diffx/Y_Resolution(i,j);
  Slopey := Diffy/X_Resolution(i,j);
    if Slopex = 0.0 then
    begin
        if Diffy > 0.0 then Asp := 90.0;
        if Diffy < 0.0 then Asp := 270.0;
    end
     else
     begin
          if slopey = 0.0 then
          begin
            if diffx > 0.0 then ASP := 180.0;
            if diffx < 0.0 then ASP := 0.0;
          end
          else //If both slopex en slopey are not 0
          begin
          GRAD := SLOPEX/SLOPEY;
          asp := -(arctan(GRAD)*(180.0/PI));
              if (diffx > 0.0) and (diffy > 0.0) then
              ASP := ABS(ASP) + 90.0
              else
                begin
                  if (diffx > 0.0) and (diffy < 0.0) then
                  ASP := 270.0 - ASP
                  else
                    begin
                    if (diffx < 0.0) and (diffy<0.0) then
                    ASP := 270.0 - ASP
                    else  ASP := 90.0 - ASP;
                    end;
                end;
          end;
      end;
result:=ASP*(PI/180.0); // aspect in radians
END;

//******************************************************************************
//In this procedure it is detemined whether a pixel is a river or not
//******************************************************************************
function IsRiver(i,j: Integer): boolean;
begin
if PRC[i,j] = -1 then
IsRiver := true
else IsRiver := false;
end;

//******************************************************************************
//In this procedure it is determined for every cell if runoff will take place in
//the direction of steepest descent (if logit(p)>0.5) or in the direction of
//tillage (if logit(p)<0.5)
//Algorithm from Takken et al. (2000)
//******************************************************************************
Function LogReg(i,j:integer):double;
var
 logitp,ep,angle : double;
 angle2,Td,Ad : double;
begin
  if Topo then
     begin
     result:=1.0; //If topo = true (is an input from the 'Input' window, tillage
     end          //direction is not taken into account
  else
  begin
  Td:=Tildir[i,j];
  if tildir[i,j] = -1 then // Dit is wanneer de tildir niet bepaald kon worden
     result := 1
  else
    begin
  Ad:=radtodeg(ASPECT[i,j]);
  if Td>Ad then
  angle:=Td-Ad
  else angle:=Ad-Td;
  if (angle)>180.0 then
  angle:=angle-180.0;
  Td:=Td+180.0;
  If Td>360.0 then
  Td:=Td-360.0;
  if Td>Ad then
  angle2:=Td-Ad
  else angle2:=Ad-Td;
  if (angle2)>180.0 then
  angle2:=angle2-180.0;
  if angle2<angle then
  angle:=angle2;
  logitp:=-5.92 + 0.133*tan(SLOPE[i,j])*100.0+0.102*angle-0.417*Ro[i,j];
  ep:=exp(logitp);
  result := ep/(1+ep);
    end;
  end;
end;

//********************************
//Geen idee waar dit voor dient...
//********************************
Function SlopeDir(dir:double;i,j:integer;DTM:Rraster):double; // Zero direction is grid north
var
G,H:double;
begin
  G:=(-DTM[i,j-1]+DTM[i,j+1])/(2*RES);
  H:=(DTM[i-1,j]-DTM[i+1,j])/(2*RES);
  result:=(H*cos(dir)+G*sin(dir));
end;

//**************************************************************************
//Procedure that calculated the routing for river pixels: after runoff has
//entered a river it cannot leave it. This procedures writes the target cell(s)
//for every river pixel to a record.
//The original version of this procedure can be found in the time-independent
//version of this model (and in the WaTEM/SEDEM script)
//**************************************************************************
Procedure DistributeRiver_Routing(i,j:integer; var FINISH:GRaster);
var
max : double;
K,L,id,number,rowmin,colmin,W : integer;
OK,OK2,check : boolean;
r, t: integer;
begin
      FINISH[i,j]:=1; //Treated cells receive a value of 1
        id:=-1;
         OK:=false;
         Max:=-9999999999.99;
         for K := -1 to 1 do //A 3x3 kernel is build around every cell
          for L := -1 to 1 do
           begin
             IF ((K=0)AND(L=0)) then Continue; //The cell under consideration ([i,j]) is not examined
             IF (PRC[i+k,j+l]=id)AND(DTM[i+k,j+l]<DTM[i,j])AND(DTM[i+k,j+l]>Max)AND(FINISH[i+k,j+l]=0) then
                // If the cell under consideration is a river, has a lower height, has a height higher than -9999999999.99 and has not been treated yet
              begin
              OK:=true;
              ROWMIN := K;
	      COLMIN := L;
              Max := DTM[I+K,J+L]; //To determine the heigest lower lying neighbor
             end;
           end;

       if (OK) then //If OK = true one of the neighbors meets the conditions. If multiple neigbors meet the condition the highest lower lying
                    //neigbor is selected
       begin
         //Routing[i,j].One_Target:= True;
         Routing[i,j].Target1Row := I+ROWMIN;
         Routing[i,j].Target1Col := J+COLMIN;
         Routing[i,j].Part1 := 1.0; //All material will flow to 1 neighbor
       end
       else //if the conditions are not met
       begin
       OK2:=false;
       MAX := -9999999999.9;
       ROWMIN:=0;
       COLMIN:=0;
         for K := -1 to 1 do //Again, only neighbors of the cell under consideration are looked at
          for L := -1 to 1 do
           begin
            IF ((K=0)AND(L=0)) then CONTINUE;
            IF (PRC[i+k,j+l]=id)AND(DTM[i+k,j+l]>MAX)AND(FINISH[i+k,j+l]=0) then
			//Same conditions as above, only the neigbor should not have a lower height
             begin
             ROWMIN := K;
             COLMIN := L;
             MAX := DTM[I+K,J+L];
             OK2:=true;
             end;
           end;
         If OK2 then
         begin
          Routing[i,j].One_Target:= True;
          Routing[i,j].Target1Row := I+ROWMIN;
          Routing[i,j].Target1Col := J+COLMIN;
          Routing[i,j].Part1 := 1.0;
          end
         else
          begin
           W:=1;
           check:=false;
            REPEAT
              for k := -W to W do
               for l := -W to W do
                begin
                 if (abs(k)<>W) and (abs(l)<>W) then continue; //The cell itself is not looked at + only the outer cells of the kernel are looked at
                 if ((i+k)<0)or(i+k>nrow)or(j+l<0)or(j+l>ncol) then continue; //The cells at the border of the map are not looked at
                  IF (PRC[i+k,j+l]=id)AND(FINISH[i+k,j+l]=0) then //If the cell is a river and has not been treated yet
                   begin
                   if check then //If a target cell has been found
                   break; //If break the current loop is ended
                   check:=true;
                   Routing[i,j].One_Target:= True;
                   Routing[i,j].Target1Row := I+K;
                   Routing[i,j].Target1Col := J+L;
                   Routing[i,j].Part1 := 1.0;
                   end;
                end;
                Inc(W);
           UNTIL ((check)OR(W>100)); //100 is the maximum size of the kernel (thus the water is transported 30 cells further away)

            end;
       end;

//The distance from the source cell to the target cell is determined
//This is needed to calculate the amount of water that is displaced every timestep,
//as this is calculated as the displacement distance (x=v*t) divided by the distance
//between source and taget cell
// => Cardinal: distance = resolution
// => Diagonal: distance = sqrt(res² + res²)

        { if (Routing[i,j].Part1 > 0.0) and (Routing[i,j].part2 > 0.0) then
         Routing[i,j].Distance := res
         else}
         if Routing[i,j].Part1 > 0.9999 then
              begin
              r := abs(i-Routing[i,j].Target1row);
              t := abs(j-Routing[i,j].Target1col);
              if (r = 0) or (t = 0) then
              Routing[i,j].Distance1 := res
              else
              Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
              end
         else
         if Routing[i,j].Part2 > 0.9999 then
            begin
            r := abs(i-Routing[i,j].Target2row);
            t := abs(j-Routing[i,j].Target2col);
            if (r = 0) or (t = 0) then
            Routing[i,j].Distance2 := res
            else
            Routing[i,j].Distance2 := sqrt(sqr(res) + sqr(res));
            end;
 end;

//Onderstaande procedure zoekt de targetcellen van alle pixels die geen rivier zijn
//en slaat ze op in de record array "Routing"
//******************************************************************************
//In onderstaande procedure wordt het water door het landschap geleid:
//- Met het flux decomposition routing algoritme
//- Het TCPR model (Takken e.a. 2000) is erin verwerkt
//- Er wordt rekening gehouden met perceelsranden
//- Wanneer het water in een rivier beland blijft het er in
//- Er wordt geen rekening gehouden met pits
//******************************************************************************
Procedure DistributeTilDirEvent_Routing(i,j:integer; var FINISH:GRaster; Topo:boolean);
// i,j = rij en kolomnummer van de cel onder beschouwing
// Area = De hoeveelheid neerslag gevallen in die cel, vermenigvuldigd met de oppervlakte van die cel
// Input(raster) = UpArea (de geaccumuleerde hoeveelheid water per cel).
// Finish(raster) = een waarde 1 geeft aan dat de cel reeds behandeld is)
// Massbalance = ???
// Topo = wordt meegegeven vanuit CalculateUpareaOlivier
var
CSN,SN,MINIMUM,MINIMUM2,PART1,PART2,extremum : extended;
K1,K2,l1,L2,ROWMIN,COLMIN,ROWMIN2,COLMIN2,K,L,z ,Area, W : integer;
parequal,closeriver, check: boolean;
Direction : single;
teller,vlag, a, b, center_x, center_y, center_ID, TFSED : integer;
begin
closeriver:=false;

// Uit code WatemSedem
for K := -1 to 1 do
  for L := -1 to 1 do
    begin
     IF ((K=0)AND(L=0)) then CONTINUE; //The pixel itself (i,j) is not evaluated
      if (PRC[i+k,j+l]=-1)then
        begin
          if(DTM[i+k,j+l]<DTM[i,j]) then
            begin
            closeriver := true; // end if
            break;
            end;
          //else closeriver:=false;
        end;
    end;

//This is our code
{for K := -1 to 1 do
 for L := -1 to 1 do
 begin
  if (PRC[i+k,j+l] = -1) AND (DTM[i+k,j+l] < DTM[i,j]) then //Geldt voor pixels die grenzen aan een rivierpixel
  //Rivierpixels worden nooit geselecteerd want daarvoor is nooit voldaan aan de tweede conditie
  closeriver := true; // The cell has a neighbouring river pixel
 end;}

if closeriver then //Voor pixels die aan een rivierpixel grenzen
BEGIN
  extremum := 99999.9;
    for K := -1 to 1 do
     for L := -1 to 1 do
       begin
            IF ((K=0)AND(L=0)) then CONTINUE; //The pixel itself (i,j) is not evaluated
            IF (PRC[i+k,j+l]=-1)AND(DTM[i+k,j+l]<DTM[i,j])AND(DTM[i+k,j+l]<extremum) then
             begin
             ROWMIN := K;
             COLMIN := L;
             extremum := DTM[i+k,j+l];
             end;
       end;
  Routing[i,j].One_Target:= True; //All water and sediment flows into the river
  Routing[i,j].Target1Row := i+ROWMIN;
  Routing[i,j].Target1Col := j+COLMIN;
  Routing[i,j].Part1 := 1.0;
  FINISH[i,j]:=1; //An identifier 1 is put in the cell when it has been evaluated

END

ELSE   //If the cell under evaluation is not adjacent to a river

BEGIN

 PART1 := 0.0;
 PART2 := 0.0;
 k1:=0;
 l1:=0;
 k2:=0;
 l2:=0;
 if (PRC[i,j] > 0) and (LogReg(i,j)<0.5) and (not(Topo)) // If tillage direction
 then 		//if logReg<0.5 then flow direction is determined by tillage direction
   begin
    Direction:=degtorad(TilDir[i,j]);
    if SlopeDir(Direction,i,j,DTM)>0.0 then
    Direction:=Direction+Pi;
    if direction>Pi*2.0 then
    direction:=direction-Pi*2.0; //Direction wordt aangepast aan de ploegrichting
   end
      else
  Direction:=aspect[i,j];  //Direction wordt gelijkgesteld aan de steilste helling

  CSN:=(ABS(cos(Direction)))/(ABS(SIN(Direction))+ABS(COS(Direction))); //Explanation: Desmet and Govers (1996) p.315
  SN :=(ABS(sin(Direction)))/(ABS(SIN(Direction))+ABS(COS(Direction))); //Deze twee waarden geven de groottes aan van de 2 componenten
                                                                        //waarover de totale vector (=upstream area + area of the cell)
                                                                        //wordt verdeeld


   Area := 1;      // obv hiervan wordt de fractie berekend die naar elke buurcel vloeit
  //For every direction (per PI/2: kwadrant) a K1/2 and L1/2 value is choosen
  //This is done to determine the coordinates of two cardinal neighboring cells to which material will flow
  IF (Direction >= 0.0) AND (Direction <= (PI/2)) THEN //Eerste kwadrant
	BEGIN
        PART1 := Area*CSN; //PART1 = de absolute waarde van de upslope area die wordt verplaatst; OPGELET: noorden is boven dus de hoek gaat van boven naar rechts!!!!! (dus sin en cos worden omgewisseld tov de 'normale' berekening!!!)
	PART2 := Area*SN; //PART2 = de absolute waarde van de upslope area die wordt verplaatst
	K1 := -1; // Deze K en L waarden geven aan naar welke cellen het materiaal verplaatst wordt
	L1 := 0 ;
	K2 := 0 ;
	L2 := 1 ;
        Routing[i,j].Target1Row := i+K1;
        Routing[i,j].Target1Col := j+L1;
        Routing[i,j].Target2Row := i+K2;
        Routing[i,j].Target2Col := j+L2;
	END
  else
     BEGIN
      IF (Direction > (PI/2)) AND (Direction < PI) THEN //tweede kwadrant
		BEGIN
		PART1 := Area*SN;
		PART2 := Area*CSN;
		K1 := 0;
		L1 := 1;
		K2 := 1;
		L2 := 0;
                Routing[i,j].Target1Row := i+K1;
                Routing[i,j].Target1Col := j+L1;
                Routing[i,j].Target2Row := i+K2;
                Routing[i,j].Target2Col := j+L2;
		END
	  else
		BEGIN
        IF (Direction >= PI)AND (Direction<= (PI*1.5)) THEN //Derde kwadrant
			BEGIN
			PART1 := Area*CSN;
			PART2 := Area*SN;
			K1 := 1;
			L1 := 0;
			K2 := 0;
			L2 := -1;
                        Routing[i,j].Target1Row := i+K1;
                        Routing[i,j].Target1Col := j+L1;
                        Routing[i,j].Target2Row := i+K2;
                        Routing[i,j].Target2Col := j+L2;
			end
		else
			BEGIN
				IF (Direction>(PI*1.5))then //Vierde kwadrant
					begin
					PART1 := Area*SN;
					PART2 := Area*CSN;
					K1 := 0;
					L1 := -1;
					K2 := -1;
					L2 := 0;
                                        Routing[i,j].Target1Row := i+K1;
                                        Routing[i,j].Target1Col := j+L1;
                                        Routing[i,j].Target2Row := i+K2;
                                        Routing[i,j].Target2Col := j+L2;
					END;
			END;
	   END;
	END;

   IF FINISH[i+K1,j+L1]=1 then // check if receiving cell is already treated
   begin
   IF FINISH[i+K2,j+L2]=1 then
     begin
		part1 := 0.0;
		part2 :=0.0; //Beide cellen zijn al behandeld en ontvangen dus niets meer
	 end
   else
     begin
	 part2 := part2+part1;
	 part1 :=0; //Een cel is reeds behandeld, de andere cel ontvangt alles
     end;
   end

   else // If the first target cell has not been treated yet

   begin
    IF FINISH[i+K2,j+L2]=1 then
    begin
	part1:=part1+part2; //One cell has been treated, the other one receives everything
	part2:=0;
    end;
   end; // End of checking if the cells have already been treated

 //In het volgende deel worden part1 en part2 ingevuld of niet naargelang de ontvangende cellen hoger liggen of tot een ander perceel behoren
  IF DTM[i+k1,j+l1]>DTM[i,j] then //If the first target cell has a higher elevation
  begin
     IF DTM[i+k2,j+l2] > DTM[i,j] then //If both cells have a higher elevation
      begin
		part1 :=0.0;
		part2:=0.0; //twee ontvangende cellen liggen hoger dan huidige cel en krijgen dus niets meer
      end
     else // Only target cell 1 has a higher elevation
      begin
       IF PRC[i+k2,j+l2]<>PRC[i,j] then //indien deze ene cel tot een ander perceel behoort dan..
       begin
          if (PRC[i+k2,j+l2] = -6) AND (FINISH[i+K2,j+L2]=0) then // If the target cell is a grass buffer strip it receives everything
          begin
            part2:=part1+part2; // In the distributionflux_LS procesure this is corrected for parcel connectivity
            part1:=0;
          end
          else
          begin
               part2:=0.0;
               part1:=0.0;      //... ontvangt deze cel (voorlopig) niets (preferentiële afstroming langs perceelsgrenzen)
          end;
       end
       else //If the parcel of the target cell is the same as the source cell...
        begin
         if FINISH[i+K2,j+L2]=0 then
         begin
         PART2:=PART2+PART1; //...it receives everything
	 PART1:=0.0;
         end
         else
         begin
            part1:=0.0;
            part2:=0.0;
         end;
        end;
      end;
  end
  else //If the first target cell does not have a higher elevation
   begin
     IF DTM[i+k2,j+l2]>DTM[i,j] then    //cel 1 lagergelegen maar cel 2 hoger...
      begin
        IF PRC[i+k1,j+l1] <> PRC[i,j] then
         begin
           if (PRC[i+k1,j+l1] = -6) AND (FINISH[i+K1,j+L1]=0) then // If the target cell is a grass buffer strip it receives everything
          begin
            part1:= part1+part2; //(part1+part2)*(TFSED_forest/100);
            part2:=0;
          end
          else
          begin
            part1:=0.0;      // cellen ontvangen voorlopig niets (preferentiële afstroming langs perceelsgrenzen)
            part2:=0.0;
          end;
         end
        else                          // als tot zelfde perceel behoort../
         begin
         if FINISH[i+K1,j+L1]=0 then
         begin
          part1:=part1+part2;         // ... ontvangt ze alles
          part2:=0.0;
          end
         else
         begin
           part1:=0.0;
           part2:=0.0;
         end;
         end;
      end
      else //Beide targetcellen liggen lager dan de broncel
       begin
        IF PRC[i+k1,j+l1]<>PRC[i,j] then
         begin
          IF PRC[i+k2,j+l2]<>PRC[i,j] then // If both target cell have a different parcel ID
           begin
             if PRC[i+k1,j+l1] = -6 then // If the first one is a grass buffer strip
               begin
                 if PRC[i+k2,j+l2] = -6 then     // If both target cells are grass buffer strips
                   begin
                     PART1:=PART1;
                     PART2:=PART2;
                   end
                   else                 // if only the first target cell is a grass buffer strip
                   begin
                   if FINISH[i+K1,j+L1]=0 then
                   begin
                     PART1:= part1+part2;
                     PART2:=0.0;
                   end
                   else
                   begin
                     part1:=0.0;
                     part2:=0.0;
                   end;
                   end;
               end
               else
               begin
                if (PRC[i+k2,j+l2] = -6) AND (FINISH[i+K2,j+L2]=0) then               // if only the 2nd target cell is a grass buffer strip
                begin
                  PART2:= part1+part2;
                  PART1:=0.0;
                end
                else                 // If none of the target cells is a grass buffer strip
                begin
                  Part1:=0.0;
                  PART2:=0.0;
                end;
               end;
           end
          else // als targetcel 1 tot ander perceel behoort, maar targetcel 2 niet...
           begin
             if PRC[i+k1,j+l1] = -6 then   // Target cell 1 is a grass buffer strip
               begin
                PART1:=PART1;
                PART2:=PART2;
               end
             else            // Target cell 1 is not a grass buffer strip
             begin
               if FINISH[i+K2,j+L2]=0 then
               begin
                 PART2:=PART2+PART1;
                 PART1:=0.0;
               end
               else
               begin
                 part1:=0;
                 part2:=0;
               end;
             end;
           end;
        end // If target cell 1 has the same parcel ID
        else
          begin                 
		  IF PRC[i+k2,j+l2]<>PRC[i,j] then        // als enkel targetcel 2 tot ander perceel behoort...
            begin
              if PRC[i+k2,j+l2] = -6 then         // als targetcel 2 een grasbufferstrook of grasgang is...
                begin
                  PART1:=PART1;
                  PART2:= PART2;
                end
              else                 // targetcel 2 is geen grasbufferstrook of grasgang
              begin
               if FINISH[i+K1,j+L1]=0 then
               begin
                PART1:=PART1+PART2;
                PART2:=0.0;
               end
               else
               begin
                 part1:=0.0;
                 part2:=0.0;
               end;
              end;
           end;
          end;
       end;
  end;

  IF ((PART1=0.0)AND(PART2=0.0)) THEN // no cells were found (both have a higher(?) elevation or another parcel ID)
       BEGIN

            PART1:=Area;  // CODE JEROEN
            parequal:=false;
            ROWMIN:= 0;
	    ROWMIN2:= 0;
            COLMIN:= 0;
	    COLMIN2:= 0;
            MINIMUM := 99999999.9;
	    MINIMUM2:= 99999999.9;
            W := 1;
            check := false;

         // CODE BASTIAAN
          {PART1:=Area; // Area = 1 (at this point in the code)
          parequal := false;
          ROWMIN := 0;
          ROWMIN2 := 0;
          COLMIN := 0;
          COLMIN2 := 0;
          MINIMUM := 99999999.9;
          MINIMUM2 := 99999999.9;   }


          // CODE JEROEN
           REPEAT                   // if no neighbouring cells are found to be a suitable target cell,
                                     //the search window is gradually extended until target is found
              for k := -W to W do
               for l := -W to W do
                begin
                 if (abs(k)<>W) and (abs(l)<>W) then continue; //The cell itself is not looked at + only the outer cells of the kernel are looked at
                 if ((i+k)<0)or(i+k>nrow)or(j+l<0)or(j+l>ncol) then continue; //The cells at the border of the map are not looked at
                  IF ((DTM[I+K,J+L]<MINIMUM)AND(DTM[I+K,J+L]<DTM[I,J]) //Als de bestemmingscel lager gelegen is dan broncel
                 AND(FINISH[I+K,J+L]=0)AND(PRC[I+K,J+L]=PRC[I,J]))THEN //En de bestemminscel nog niet behandeld is EN binnen hetzelfde perceel ligt
                   begin
                   check:=true;
                   MINIMUM := DTM[I+K,J+L];
	           ROWMIN := K;
	           COLMIN := L;
                   parequal:=true;
                   end;
                  IF ((DTM[I+K,J+L]<MINIMUM2)AND(DTM[I+K,J+L]<DTM[I,J]) AND(FINISH[I+K,J+L]=0))THEN   // lager gelegen cel, ander perceel, nog niet behandeld
                 begin
                 check:=true;
                 MINIMUM2 := DTM[I+K,J+L];
	         ROWMIN2 := K;
	         COLMIN2 := L;
	         end;
                end;
                Inc(W);
           UNTIL ((check)OR(W>50)); //50 is the maximum size of the kernel (thus the water is transported 50 cells further away)

          {  if check = false then
               showmessage('threshold exceeded');
            }
          {  // CODE WATEMSEDEM2015
         for K := -1 to 1 do
            for L := -1 to 1 do  //loop van 3*3 cellen om te zoeken naar laagst gelegen buurcel
              begin
	        if ((K = 0) AND (L = 0)) then continue;
                if ((DTM[I+K,J+L] < MINIMUM) and (DTM[I+K,J+L] < DTM[I,J])
                  and (FINISH[I+K,J+L] = 0) and (PRC[I+K,J+L] = PRC[I,J])) then      //laagste buurcel is zelfde perceel
                    begin
	                   MINIMUM := DTM[I+K,J+L];
	                   ROWMIN := K;
	                   COLMIN := L;
                      parequal := true;
                    end; //second loop voor wanneer er geen lagere cel is in hetzelfde perceel
                //end if
                if ((DTM[I+K,J+L] < MINIMUM2) AND (DTM[I+K,J+L]<DTM[I,J])
                  AND(FINISH[I+K,J+L] = 0)) THEN     //lagere cel ligt in ander perceel maar binnen het bekken
                    begin
	                   MINIMUM2 := DTM[I+K,J+L];
	                   ROWMIN2 := K;
	                   COLMIN2 := L;
                    end;

                //end if
              end;
          //end for's

           }

        if parequal THEN    // If receiving cell is in same parcel
                BEGIN
                Routing[i,j].One_Target:= True;
                Routing[i,j].Target1Row := i+ROWMIN;
                Routing[i,j].Target1Col := j+COLMIN;
                Routing[i,j].Target2Row := 0;
                Routing[i,j].Target2Col := 0;
                Routing[i,j].Part1 := part1{ + part2};
                Routing[i,j].Part2 := 0;
                END
                ELSE           // if receiving cell belongs to a different parcel
                BEGIN
                // determine value of parcel connectivity based on receiving parcel

                Routing[i,j].One_Target:= True;
                Routing[i,j].Target1Row := i+ROWMIN2;
                Routing[i,j].Target1Col := j+COLMIN2;
                Routing[i,j].Target2Row := 0;
                Routing[i,j].Target2Col := 0;
                Routing[i,j].Part1 := part1{ + part2};
                Routing[i,j].Part2 := 0;
                END;


       END  // end if no cells were found
        ELSE
        BEGIN // normal case part1 or part2 <> 0
             Routing[i,j].Target1Row := i+k1;
             Routing[i,j].Target1Col := j+l1;
             Routing[i,j].Target2Row := i+k2;
             Routing[i,j].Target2Col := j+l2;
             Routing[i,j].Part1 := Part1;
             Routing[i,j].Part2 := Part2;
        END;

  FINISH[I,J] := 1;

  if Routing[i,j].Part1 = 0 then
  begin
   Routing[i,j].Target1Row := 0;
   Routing[i,j].Target1Col := 0;
  end;

END;  // end if cell is not adjacent to river

// The distance from source to target cells is calculated
// => Cardinal: distance = resolution
// => Diagonal: distance = sqrt(res² + res²)

if Routing[i,j].Part1 > 0.0 then
     begin
     a := abs(i-Routing[i,j].Target1row);
     b := abs(j-Routing[i,j].Target1col);
     if (a = 0) or (b = 0) then
     Routing[i,j].Distance1 := res // If the source cell is a cardinal cell
     else
     Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res)); // If the source cell is a diagonal cell
     end;

if Routing[i,j].Part2 > 0.0 then
   begin
   a := abs(i-Routing[i,j].Target2row);
   b := abs(j-Routing[i,j].Target2col);
   if (a = 0) or (b = 0) then
   Routing[i,j].Distance2 := res  // If the source cell is a cardinal cell
   else
   Routing[i,j].Distance2 := sqrt(sqr(res) + sqr(res)); // If the source cell is a diagonal cell
   end;

// if cell contains a pond, the routing information is adjusted (water is not allowed to leave)

if (PRC[i,j]=-5) then // If the parcel under consideration is open water water
begin
  Routing[i,j].One_Target := false;
  Routing[i,j].Target1Row := 0;
  Routing[i,j].Target1Col := 0;
  Routing[i,j].Target2Row := 0;
  Routing[i,j].Target2Col := 0;
  Routing[i,j].Part1 := 0;
  Routing[i,j].Part2 := 0;
  Routing[i,j].Distance1 := 0;
  Routing[i,j].Distance2 := 0;
end;


//In the lines below the routing algorithm is adjusted for buffers (opvangbekkens?):
// Target cell for each buffer pixel = center cell of the buffer it belongs to
// Target cell for each buffer center cell = the lowest lying neighbour
// All cells adjacent to buffer and higher then buffer => target cell = buffer cell

if (Include_buffer) AND (Buffermap[i,j] <> 0) then
begin
   for K := -1 to 1 do //a 3*3 kernel is build around cell to which we look
     for L := -1 to 1 do
     begin

     IF ((K=0)AND(L=0)) then CONTINUE;

     if DTM[i+k,j+l] > DTM[i,j] then //Cells with a higher altitude are assigned the buffer as only target
       begin
          Routing[i+k,j+l].Target1Row := i;
          Routing[i+k,j+l].Target1Col := j;
          Routing[i+k,j+l].Part1 := 1.0;
          Routing[i+k,j+l].Target2Row := 0;
          Routing[i+k,j+l].Target2Col := 0;
          Routing[i+k,j+l].Part2 := 0;
          Routing[i+k,j+l].One_Target := True;
          if (K=0) OR (L=0) then
             Routing[i+k,j+l].Distance1 := res
          else
             Routing[i+k,j+l].Distance1 := sqrt(sqr(res) + sqr(res));
          Routing[i+k,j+l].Distance2 := 0;
       end;
     end;

   if Buffermap[i,j] <= Number_of_Buffers then //Center cell     // center of the buffer drains to lowest neighbour
   begin
     w := 1;
     Minimum := 99999999.9;
     check := false;
     REPEAT
     for K := -w to w do //a 3*3 kernel is build around the center of the buffer
       for L := -w to w do
       begin
         if (abs(k)<>W) and (abs(l)<>W) then continue;
         if (DTM[i+k,j+l] < DTM[i,j]) and (DTM[i+k,j+l] < Minimum) then
         begin
           Minimum := DTM[i+k,j+l];
           Routing[i,j].Target1Row := i+k;
           Routing[i,j].Target1Col := j+l;
           Routing[i,j].Part1 := 1.0;
           Routing[i,j].Target2Row := 0;
           Routing[i,j].Target2Col := 0;
           Routing[i,j].Part2 := 0;
           Routing[i,j].One_Target := True;
           if (k=0) or (l=0) then
              Routing[i,j].Distance1 := res
           else
              Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));

           Routing[i,j].Distance2 := 0;

           if (buffermap[i+k,j+l] <> (buffermap[i,j]*100)) then //Check will be true when the target cell does not belong to the same buffer
              check := true;
         end;
       end;
        Inc(W);
       UNTIL ((check)OR(W>50)); //50 is the maximum size of the kernel (thus the water is transported 50 cells further away)


   end


   else    //Voor buffercellen die geen centercell zijn      // buffer pixel drains to buffer center cell
   begin
     // identify center cell
     center_ID := 0;
     for k := 1 to Number_of_Buffers do
     begin
       if Buffermap[i,j] = BufferData[k].ext_ID then
         center_ID := k;
     end;
     if center_ID =0 then
       begin
         showmessage('Error in buffer input data: center cell of buffer ID '+inttostr(Buffermap[i,j])+' not found in buffer database.');
         Exit;
       end;
     for k := 1 to nrow do            // determine coordinates of buffer center
      for l:= 1 to ncol do
      begin
        if Buffermap[k,l] = center_ID then
        begin
          center_x := k;
          center_y := l;
        end;
      end;
     Routing[i,j].Target1Row := center_x;
     Routing[i,j].Target1Col := center_y;
     Routing[i,j].Part1 := 1.0;
     Routing[i,j].Target2Row := 0;
     Routing[i,j].Target2Col := 0;
     Routing[i,j].Part2 := 0;
     Routing[i,j].One_Target := True;
     Routing[i,j].Distance1 := res;
     Routing[i,j].Distance2 := 0;
   end;
end;

if (Include_ditch) AND (Ditch_map[i,j] <> 0) then
begin
  case Ditch_map[i,j] of        // first target cell is determined by user input...
    1: begin
      Routing[i,j].Target1row := i-1;
      Routing[i,j].Target1col := j;
      Routing[i,j].Distance1 := res;
    end;
    2: begin
      Routing[i,j].Target1row := i-1;
      Routing[i,j].Target1col := j+1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
   3: begin
      Routing[i,j].Target1row := i;
      Routing[i,j].Target1col := j+1;
      Routing[i,j].Distance1 := res;
    end;
   4: begin
      Routing[i,j].Target1row := i+1;
      Routing[i,j].Target1col := j+1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
   5: begin
      Routing[i,j].Target1row := i+1;
      Routing[i,j].Target1col := j;
      Routing[i,j].Distance1 := res;
    end;
   6: begin
      Routing[i,j].Target1row := i+1;
      Routing[i,j].Target1col := j-1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
   7: begin
      Routing[i,j].Target1row := i;
      Routing[i,j].Target1col := j-1;
      Routing[i,j].Distance1 := res;
    end;
   8: begin
      Routing[i,j].Target1row := i-1;
      Routing[i,j].Target1col := j-1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
  end;

  Routing[i,j].Part1 := 1.0;              // in any case there is no second target cell
  Routing[i,j].Target2Row := 0;
  Routing[i,j].Target2Col := 0;
  Routing[i,j].Part2 := 0;
  Routing[i,j].Distance2 := 0;
  Routing[i,j].One_Target := True;
end;

if (Include_dam) AND (Dam_map[i,j] <> 0) then         // same as for ditches
begin
  case Dam_map[i,j] of
    1: begin
      Routing[i,j].Target1row := i-1;
      Routing[i,j].Target1col := j;
      Routing[i,j].Distance1 := res;
    end;
    2: begin
      Routing[i,j].Target1row := i-1;
      Routing[i,j].Target1col := j+1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
   3: begin
      Routing[i,j].Target1row := i;
      Routing[i,j].Target1col := j+1;
      Routing[i,j].Distance1 := res;
    end;
   4: begin
      Routing[i,j].Target1row := i+1;
      Routing[i,j].Target1col := j+1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
   5: begin
      Routing[i,j].Target1row := i+1;
      Routing[i,j].Target1col := j;
      Routing[i,j].Distance1 := res;
    end;
   6: begin
      Routing[i,j].Target1row := i+1;
      Routing[i,j].Target1col := j-1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
   7: begin
      Routing[i,j].Target1row := i;
      Routing[i,j].Target1col := j-1;
      Routing[i,j].Distance1 := res;
    end;
   8: begin
      Routing[i,j].Target1row := i-1;
      Routing[i,j].Target1col := j-1;
      Routing[i,j].Distance1 := sqrt(sqr(res) + sqr(res));
    end;
  end;

  Routing[i,j].Part1 := 1.0;
  Routing[i,j].Target2Row := 0;
  Routing[i,j].Target2Col := 0;
  Routing[i,j].Part2 := 0;
  Routing[i,j].Distance2 := 0;
  Routing[i,j].One_Target := True;
end;

// if cell contains a sewer, the target river cell is determined and the routing information is adjusted
If (Include_sewer) AND (SewerMap[i,j]<>0) then
begin
      ROWMIN:= 0;
      COLMIN:= 0;
      MINIMUM := 99999999.9;
      W := 1;
      check := false;
     REPEAT                   // find closest LOWER river pixel. If no neighbouring cells are found to be a suitable target cell,
                             //the search window is gradually extended until target is found
      for k := -W to W do
       for l := -W to W do
        begin
         if (abs(k)<>W) and (abs(l)<>W) then continue; //The cell itself is not looked at + only the outer cells of the kernel are looked at
         if ((i+k)<0)or(i+k>nrow)or(j+l<0)or(j+l>ncol) then continue; //The cells at the border of the map are not looked at
          IF ((DTM[I+K,J+L]<MINIMUM)AND(DTM[I+K,J+L]<DTM[I,J]) //Als de bestemmingscel een riviercel is, lager gelegen is dan broncel
         AND(FINISH[I+K,J+L]=0)AND(PRC[I+K,J+L]=-1))THEN //En de bestemminscel nog niet behandeld is
           begin
             check:=true;
             MINIMUM := DTM[I+K,J+L];
	     ROWMIN := K;
	     COLMIN := L;
           end;
        end;
        Inc(W);
    UNTIL (check);

    if Routing[i,j].Part1 < Routing[i,j].Part2 then               // make sure the "strongest" target cell is target 1 (because sewer will be target 2)
    begin
      Routing[i,j].Target1Row := Routing[i,j].Target2Row;
      Routing[i,j].Target1Col := Routing[i,j].Target2Col;
      Routing[i,j].Distance1 := Routing[i,j].Distance2;
    end;

    Routing[i,j].One_Target := false;
    Routing[i,j].Target2Row := i+ROWMIN;      // river pixel = target 2, target 1 remains unaltered
    Routing[i,j].Target2Col := j+COLMIN;
    Routing[i,j].Part1 := 1-SewerMap[i,j];    // SewerMap[i,j] = vangefficiëntie!
    Routing[i,j].Part2 := SewerMap[i,j];

    // distance to target cell is calculated
    if (K=0) OR (L=0) then
      Routing[i,j].Distance2 := res*(W-1)
    else
      Routing[i,j].Distance2 := sqrt(sqr(ROWMIN*res) + sqr(COLMIN*res));
end;

end; // end procedure DistributeTilDirEvent_Routing

// ***************************************************************************
// The following 2 functions check whether the elements in an array are equal or
// not.
// ***************************************************************************

function intArrayIsEqual (inputArray:array of Integer): boolean;
  var
  Val, i: Integer;

  begin
     intArrayIsEqual := True;
     Val := inputArray[1];
     for i := low(inputArray) to High(inputArray) do
     begin
        if inputArray[i] <> Val then
          intArrayIsEqual := false;
     end;
  end;

 function doubArrayIsEqual (inputArray:array of double): boolean;
  var
  Val: double;
  i: Integer;

  begin
     doubArrayIsEqual := True;
     Val := inputArray[1];
     for i := low(inputArray) to High(inputArray) do
     begin
        if inputArray[i] <> Val then
          doubArrayIsEqual := false;
     end;
  end;

 Procedure Calculate_UpstreamArea(var UPAREA:RRaster);
var
teller,i,j : integer;
Fluxout: RRaster;
oppcor: double;

begin
SetDynamicRdata(Fluxout);
SetzeroR(UPAREA);
for teller:= ncol*nrow downto 1 do
    begin // begin lus
          i:=row[teller];
          j:=column[teller];
          If PRC[i,j]=0 then continue;
          OPPCOR := (X_resolution(i,j)*Y_resolution(i,j)) * (1 - (PTEFmap[i,j] / 100)); //bijdrage van elke cel aan de uparea
          Fluxout[i,j]:=OPPCOR+UPAREA[i,j];        // X_res * Y_res = oppervlakte 1 cel
          DistributeFlux_LS(i,j,UPAREA,Fluxout);       // volgende cellen worden geïdentificeerd en uparea in deze cellen wordt berekend
          UPAREA[i,j]:=UPAREA[i,j]+oppcor; //(X_Resolution(i,j)*Y_Resolution(i,j))/2.0;
    end; // end matrix loop

DisposeDynamicRdata(Fluxout);
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
      If (PRC[i,j] = 0) OR (PRC[i,j] = -1) then continue;
      if Raster_projection=plane then locres:=RES
      else locres := (X_Resolution(i,j)+Y_Resolution(i,j))/2.0; //else fixed res is used
      ADJUST := (ABS(cos(aspect[i,j]))+ABS(sin(aspect[i,j])));

      B:=(sin(slope[i,j])/0.0896)/((3.0*power(sin(slope[i,j]),0.8))+0.56);
      //EXP:=B/(B+1);
      if UPAREA[i,j] < 10000 then
        EXP := 0.3+POWER((UPAREA[i,j]/10000),0.8)
      else
        EXP:=0.72;
      if EXP>0.72 then
         EXP:=0.72;

      Sfactor:=-1.5 + 17/(1+power(2.718281828,(2.3-6.1*sin(slope[i,j]))));
      Lfactor:=(POWER((Uparea[i,j]+sqr(locres)),EXP+1)-POWER(Uparea[i,j],EXP+1))/
             (POWER(ADJUST,EXP)*POWER(locres,EXP+2)*POWER(22.13,EXP));
      LS[i,j]:=Sfactor*Lfactor;
      end;    // end matrix loop
      end;

end;  // --------- end procedure CalculateLS------------------------------------

// The following procedure is used to route the LS through the landscape:
// it adapts the LS values for the parcel connectivities
Procedure DistributeFlux_LS(i,j:integer;var Flux_IN,Flux_OUT: RRaster);
var
flux:double;
begin                                 // flux decomposition algoritme

// Bij de overgang naar een ander perceel wordt de uparea verminders volgens de parcel connectivities
if (Routing[i,j].one_target = true) and (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] <> PRC[i,j]) then
   // Als er maar 1 targetcel is (dit is steeds wanneer er naar een ander perceel wordt gerout,
   // behalve wanneer de targetcel een grasbufferstrook is)
   // en wanneer die targetcel een andere perceelswaarde heeft
   begin
     flux := 0;

     if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] >= 1) then // Als de targetcel cropland is
        begin
         flux := FLUX_OUT[i,j]*(TFSED_crop/100);
        end;
     if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -3) // Als de targetcel bos,
     or (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -4) // weide
     or (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -6) then // grasbufferstrook
        begin
         flux := FLUX_OUT[i,j]*(TFSED_forest/100);
        end;

     if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -2) then // Target is verhard oppervlak
        flux := FLUX_OUT[i,j];

     if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -5) then // Target is a pond (open water parcel but not river)
        flux := FLUX_OUT[i,j];

     if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -1) then // Target is a river pixel
        flux := FLUX_OUT[i,j];

     if Include_buffer then
        begin
             if (Buffermap[i,j] > 0) AND (Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] > 0) then // Both source and target are buffers
             flux := FLUX_OUT[i,j];
        end;

     Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] + flux;

    {  if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -1) then
         begin
         flux := FLUX_OUT[i,j];
         Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] + flux;
         end;

      if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -2) then
         begin
         flux := FLUX_OUT[i,j];
         Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] + flux;
         end; }

     end

     else // If the target cells belong to the same parcel no parcel connectivities
          // have to be taken into account, except when the target cell is a grass buffer strip

     begin
     flux := 0;
     if Routing[i,j].Part1 > 0.0 then
        begin
         if (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -6) AND (PRC[i,j] <> -6) AND (Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = 0) then // If this target cell is a grass buffer strip
            // The parcel connectivity should only be applied when you go from a non-grassbuffer strip to a bufferstrip
            begin
                  flux := FLUX_OUT[i,j]*(TFSED_forest/100);
                  Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] + flux;
             end

         else // If this target cell is not a grass buffer strip (or you are within a grassbuffer strip)

             begin
                  flux := FLUX_OUT[i,j]*Routing[i,j].Part1;
                  Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] + flux;
             end;
         end;

     if Routing[i,j].Part2 > 0.0 then
        begin
             if (PRC[Routing[i,j].Target2Row,Routing[i,j].Target2Col] = -6) AND (PRC[i,j] <> -6) AND (Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = 0) then // If this target cell is a grass buffer strip
                 // The parcel connectivity should only be applied when you go from a non-grassbuffer strip to a bufferstrip
                begin
                     flux := FLUX_OUT[i,j]*(TFSED_forest/100);
                     Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] := Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] + flux;
                end

         else // If this target cell is not a grass buffer strip (or you are within a grassbuffer strip)

                begin
                   flux := FLUX_OUT[i,j]*Routing[i,j].Part2;
                   Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] := Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] + flux;
                end;
         end;

         end;

{if (routing[i,j].Part1 = 0.0) and (Routing[i,j].Part2 = 0.0) and (PRC[i,j] <> -5) then
  Showmessage('No target cells determined!' + 'row ' + inttostr(i) + '; ' + 'column: ' + inttostr(j));
 }
end;

// The following procedure is used to route the sediment through the landscape
// by distributing the sediment over 1 or 2 target cells according to the
// flux decomposition algorithm
Procedure DistributeFlux_Sediment(i,j:integer;var Flux_IN,Flux_OUT: RRaster);
var
flux:double;

begin                                 // flux decomposition algoritme
     if Routing[i,j].Part1 > 0.0 then
        begin
             flux := FLUX_OUT[i,j]*Routing[i,j].Part1; // m³
             Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] + flux;
             // FLUX_IN [m³]
        end;
     if Routing[i,j].Part2 > 0.0 then
        begin
             flux := FLUX_OUT[i,j]*Routing[i,j].Part2;
             Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] := Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] + flux;
             // FLUX_IN [m³]
        end;
end;

  Procedure Topo_Calculations;
begin

sort(DTM); //The DTM is sorted from low to high
//dtmcheck;   // searches for neighbouring cells of equal height in the DTM
CalculateSlopeAspect; //Slope and aspect are calculated
Calculate_routing(Routing); //The flow direction(s) is calulated for every gridcell + the relative contribution to every receiving neighbour (Marijn)
Calculate_UpstreamArea(UPAREA);
CalculateLS(LS,UPAREA);
end;

 { Procedure dtmcheck;
  var
    teller, i, j, k, l,flag: integer;
    dtmcheck_rst : GRaster;
  begin
  SetDynamicGData(dtmcheck_rst);
  flag := 0;
  for teller := ncol * nrow downto 1 do
     begin // begin lus
           i := row[teller];
           j := column[teller];
           // create 3*3 kernel around cell
           for k := -1 to 1 do
           for l := -1 to 1 do
           begin
             IF ((k=0)AND(l=0)) then Continue; //The cell under consideration ([i,j]) is not examined
             IF (DTM[i,j]=DTM[i+k,j+l]) then
             begin
                dtmcheck_rst[i,j] := 1;
                //DTM[i+k,j+l] := DTM[i+k,j+l]-0.1;
                flag := 1;
             end;
           end;
     end;
  if flag=1 then
  begin
    Showmessage('Cells of equal height occur in DTM. This should be corrected first. See dtmcheck.rst in output folder for error locations. Click OK to quit the model run.');
    WriteGIdrisi32file(ncol,nrow,File_output_dir+'dtmcheck'+'.rst', dtmcheck_rst);
    Application.Terminate;
  end;
  DisposedynamicGData(dtmcheck_rst);
  end;
  }
end.
