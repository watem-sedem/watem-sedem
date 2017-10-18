
Unit Raster_calculations;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, Dialogs, Math, GData_CN, RData_CN, ReadInParameters;

Type
  ECalculationError = Exception;

Procedure sort(D:Rraster);
Procedure sortbis(Var Z:Rvector; iLo,iHi:Integer);
Procedure CalculateSlopeAspect;
Function LogReg(i,j:integer): double;
Function CalculateSLOPE(i,j:integer): double;
Function CalculateASPECT(i,j:integer) : double;
Function IsRiver(i,j: Integer): boolean;
Function SlopeDir(dir:double;i,j:integer;DTM:Rraster): double;
Procedure Calculate_routing(Var Routing: TRoutingArray);
Procedure DistributeRiver_Routing(i,j:integer; Var FINISH:GRaster);
Procedure DistributeTilDirEvent_Routing(i,j:integer; Var FINISH:GRaster; Topo:boolean);
Function intArrayIsEqual (inputArray: Array Of Integer): boolean;
Function doubArrayIsEqual (inputarray: Array Of double): boolean;
Function X_Resolution(): double;
Function Y_Resolution(): double;
Procedure Calculate_UpstreamArea(Var UPAREA:RRaster);
Procedure CalculateLS(Var LS:RRaster;UPAREA:RRaster);
Procedure DistributeFlux_LS(i,j:integer;Var Flux_IN,Flux_OUT: RRaster);
Procedure DistributeFlux_Sediment(i,j:integer;Var Flux_IN,Flux_OUT: RRaster);
Procedure Topo_Calculations;



Implementation

Const
  Earth_DegToMeter = 111319.444444444;

Function X_Resolution(): double;

Var 
  Yresdeg,Xresdeg,longitude: double;
Begin
  If Raster_Projection=plane Then
    Begin
      result := (MAXX-MINX)/ncol;
    End
  Else
    Begin
      Yresdeg := (MAXY-MINY)/nrow;
      Xresdeg := (MAXX-MINX)/ncol;
      longitude := MAXY-i*Yresdeg;
      // rowcount starts left top corner of raster
      result := Xresdeg*Earth_DegToMeter*cos(degtorad(longitude));
      //
    End;
End;

Function Y_Resolution (): double;
Begin
  If Raster_Projection=plane Then
    Begin
      result := (MAXY-MINY)/nrow;
    End
  Else
    Begin
      result := (MAXY-MINY)/nrow*Earth_DegToMeter;
      //MAX & MIN in degrees  YRES in m
    End;
End;

//******************************************************************************
// This procedure calculates the routing of runoff though the landscape by examining
// to which cell(s) every cell drains. This way the routing only needs to be
// calculated once, at the beginning of the program (for the time-dependent version).
//******************************************************************************
Procedure Calculate_routing(Var Routing: TRoutingArray);

Var 
  Teller: integer;
  Finish: GRaster;
  //A cell receives a value of 1 after it had been treated
Begin
  SetDynamicGdata(Finish);
  SetzeroG(Finish);

  //Dimensions of the Routing matrix are set so that they are equal to the input maps
  SetLength(Routing,nrow+1);
  For i := Low(Routing) To high(Routing) Do
    Setlength(Routing[i],ncol+1);
  //The values in all records are filled for the routing matrix
  For i:= 1 To nrow Do
    For j:= 1 To ncol Do
      Begin
        Routing[i,j].Target1Row := -99;
        Routing[i,j].Target1Col := -99;
        Routing[i,j].Target2Row := -99;
        Routing[i,j].Target2Col := -99;
        Routing[i,j].Part1 := 0.0;
        Routing[i,j].Part2 := 0.0;
        Routing[i,j].Distance1 := 0.0;
        Routing[i,j].Distance2 := 0.0;
      End;
  //For every cell in the catchment the target cell(s) are determined
  For teller:=nrow*ncol Downto 1 Do
    Begin
      i := row[teller];
      //row contain the dtm rownumbers from low to high
      j := column[teller];
      //Same for columns
      If PRC[i,j]=0 Then continue;
      If IsRiver(i,j) Then
        //Routing procedure for rivers (once water has entered a river it has to stay in the river)
        DistributeRiver_Routing(i, j, Finish)
      Else //Routing procedure for all other cells
        DistributeTilDirEvent_Routing(i,j, FINISH, Topo);
      If (Routing[i,j].Target1Row > 0) Then Routing[i,j].Distance1 := res * sqrt(sqr(i - Routing[i,j
                                                                      ].Target1Row) + sqr(j -
                                                                      Routing[i,j].Target1Col));
      If (Routing[i,j].Target2Row > 0) Then Routing[i,j].Distance2 := res * sqrt(sqr(i - Routing[i,j
                                                                      ].Target2Row) + sqr(j -
                                                                      Routing[i,j].Target2Col));
    End;
End;

//******************************************************************************
// In the two scripts below the DTM is sorted from low to high
//******************************************************************************
Procedure sort(D:Rraster);

Var 
  number1,i,j: integer;
  H: Rvector;
Begin
  Setlength(H,nrow*ncol+1);
  Setlength(ROW,nrow*ncol+1);
  Setlength(COLUMN,nrow*ncol+1);
  number1 := 0;
  For i := 1 To nrow Do
    //The DTM is read row per row (from l to r), for each next cell that is
    For j := 1 To ncol Do
      //read number1 is increased with 1: this is the cell identifier
      Begin
        Inc(number1);
        H[number1] := D[i,j];
        //H is a vector with the height of each cell
        ROW[number1] := i;
        //The row of each cell is stored
        COLUMN[number1] := j;
        //Same for column
      End;
  sortbis(H,1,nrow*ncol);
  //Procedure with the actual sorting
  H := Nil;
End;

Procedure sortbis(Var Z:Rvector; iLo,iHi:Integer);

Var 
  helpcol, helprow,Lo, Hi: Integer;
  Mid,T : double;
Begin
  Lo := iLo;
  //iLo is 1 at the start of this procedure
  Hi := iHi;
  //iHi is the number of elements in the DTM (nrow*ncol)
  Mid := Z[(Lo + Hi) Div 2];
  //Mid is the height of the central (middle) element in the DTM
  Repeat
    While Z[Lo] < Mid Do
      Inc(Lo);
    While Z[Hi] > Mid Do
      Dec(Hi);
    If Lo <= Hi Then
      Begin
        T := Z[Lo];
        Z[Lo] := Z[Hi];
        Z[Hi] := T;
        helprow := row[lo];
        row[lo] := row[hi];
        row[hi] := helprow;
        helpcol := column[lo];
        column[lo] := column[hi];
        column[hi] := helpcol;
        Inc(Lo);
        Dec(Hi);
      End;
  Until Lo > Hi;
  If Hi > iLo Then Sortbis(Z, iLo, Hi);
  If Lo < iHi Then Sortbis(Z, Lo, iHi);
End;

//******************************************************************************
//In this procedure the procedures to calculate the slope and aspect are called
//******************************************************************************
Procedure CalculateSlopeAspect;

Var 
  i,j: integer;
Begin
  SetDynamicRdata(Slope);
  SetDynamicRdata(Aspect);
  For i:=1 To nrow Do
    For j:=1 To ncol Do
      Begin
        Slope[i,j] := CalculateSlope(i,j);
        Aspect[i,j] := CalculateAspect(i,j);
      End;
End;

//Slope calculation
Function CalculateSLOPE(i,j:integer): double;

Var 
  DIFFX,DIFFY: double;
Begin

  DIFFX := abs(DTM[i-1,j] - DTM[i + 1,j]) / (2 * RES);
  DIFFY := abs(DTM[i,j-1] - DTM[i,j + 1]) / (2 * RES);
  result := arctan(sqrt(sqr(DIFFX) + sqr(DIFFY)));
  //slope in radians


  // previous version

{
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
else
DIFFY:=abs(DTM[i,j-1]-DTM[i,j+1])/(2*X_Resolution(i,j));
end;
 result:=arctan(sqrt(sqr(DIFFX)+sqr(DIFFY)));
}
  //slope in radians
End;

//Aspect calculation
Function CalculateASPECT(i,j:integer) : double;

Var 
  Diffx,Diffy,Slopex,Slopey,Asp,Grad: double;
Begin
  ASP := 0;
  Diffx := DTM[i-1,j]-DTM[i+1,j];
  Diffy := DTM[i,j-1]-DTM[i,j+1];
  Slopex := Diffx/Y_Resolution();
  Slopey := Diffy/X_Resolution();
  If Slopex = 0.0 Then
    Begin
      If Diffy > 0.0 Then Asp := 90.0;
      If Diffy < 0.0 Then Asp := 270.0;
    End
  Else
    Begin
      If slopey = 0.0 Then
        Begin
          If diffx > 0.0 Then ASP := 180.0;
          If diffx < 0.0 Then ASP := 0.0;
        End
      Else //If both slopex en slopey are not 0
        Begin
          GRAD := SLOPEX/SLOPEY;
          asp := -(arctan(GRAD)*(180.0/PI));
          If (diffx > 0.0) And (diffy > 0.0) Then
            ASP := ABS(ASP) + 90.0
          Else
            Begin
              If (diffx > 0.0) And (diffy < 0.0) Then
                ASP := 270.0 - ASP
              Else
                Begin
                  If (diffx < 0.0) And (diffy<0.0) Then
                    ASP := 270.0 - ASP
                  Else  ASP := 90.0 - ASP;
                End;
            End;
        End;
    End;
  result := ASP*(PI/180.0);
  // aspect in radians
End;

//******************************************************************************
//In this procedure it is detemined whether a pixel is a river or not
//******************************************************************************
Function IsRiver(i,j: Integer): boolean;
Begin
  If PRC[i,j] = -1 Then
    IsRiver := true
  Else IsRiver := false;
End;

//******************************************************************************
//In this procedure it is determined for every cell if runoff will take place in
//the direction of steepest descent (if logit(p)>0.5) or in the direction of
//tillage (if logit(p)<0.5)
//Algorithm from Takken et al. (2000)
//******************************************************************************
Function LogReg(i,j:integer): double;

Var 
  logitp,ep,angle : double;
  angle2,Td,Ad : double;
Begin
  If Topo Then
    Begin
      result := 1.0;
      //If topo = true (is an input from the 'Input' window, tillage
    End
    //direction is not taken into account
  Else
    Begin
      Td := Tildir[i,j];
      If tildir[i,j] = -1 Then // Dit is wanneer de tildir niet bepaald kon worden
        result := 1
      Else
        Begin
          Ad := radtodeg(ASPECT[i,j]);
          If Td>Ad Then
            angle := Td-Ad
          Else angle := Ad-Td;
          If (angle)>180.0 Then
            angle := angle-180.0;
          Td := Td+180.0;
          If Td>360.0 Then
            Td := Td-360.0;
          If Td>Ad Then
            angle2 := Td-Ad
          Else angle2 := Ad-Td;
          If (angle2)>180.0 Then
            angle2 := angle2-180.0;
          If angle2<angle Then
            angle := angle2;
          logitp := -5.92 + 0.133*tan(SLOPE[i,j])*100.0+0.102*angle-0.417*Ro[i,j];
          ep := exp(logitp);
          result := ep/(1+ep);
        End;
    End;
End;

//********************************
//Geen idee waar dit voor dient...
//********************************
Function SlopeDir(dir:double;i,j:integer;DTM:Rraster): double;
// Zero direction is grid north

Var 
  G,H: double;
Begin
  G := (-DTM[i,j-1]+DTM[i,j+1])/(2*RES);
  H := (DTM[i-1,j]-DTM[i+1,j])/(2*RES);
  result := (H*cos(dir)+G*sin(dir));
End;

//**************************************************************************
//Procedure that calculated the routing for river pixels: after runoff has
//entered a river it cannot leave it. This procedures writes the target cell(s)
//for every river pixel to a record.
//The original version of this procedure can be found in the time-independent
//version of this model (and in the WaTEM/SEDEM script)
//**************************************************************************
Procedure DistributeRiver_Routing(i,j:integer; Var FINISH:GRaster);

Var 
  max : double;
  K,L,id,rowmin,colmin,W : integer;
  OK,OK2,check : boolean;
  r, t: integer;
Begin
  FINISH[i,j] := 1;
  //Treated cells receive a value of 1
  id := -1;
  OK := false;
  Max := -9999999999.99;
  For K := -1 To 1 Do
    //A 3x3 kernel is build around every cell
    For L := -1 To 1 Do
      Begin
        If ((K=0)And(L=0)) Then Continue;
        //The cell under consideration ([i,j]) is not examined
        If (PRC[i+k,j+l]=id)And(DTM[i+k,j+l]<DTM[i,j])And(DTM[i+k,j+l]>Max)And(FINISH[i+k,j+l]=0)
          Then

// If the cell under consideration is a river, has a lower height, has a height higher than -9999999999.99 and has not been treated yet
          Begin
            OK := true;
            ROWMIN := K;
            COLMIN := L;
            Max := DTM[I+K,J+L];
            //To determine the heigest lower lying neighbor
          End;
      End;

  If (OK) Then
//If OK = true one of the neighbors meets the conditions. If multiple neigbors meet the condition the highest lower lying
    //neigbor is selected
    Begin
      //Routing[i,j].One_Target:= True;
      Routing[i,j].Target1Row := I+ROWMIN;
      Routing[i,j].Target1Col := J+COLMIN;
      Routing[i,j].Part1 := 1.0;
      //All material will flow to 1 neighbor
    End
  Else //if the conditions are not met
    Begin
      OK2 := false;
      MAX := -9999999999.9;
      ROWMIN := 0;
      COLMIN := 0;
      For K := -1 To 1 Do
        //Again, only neighbors of the cell under consideration are looked at
        For L := -1 To 1 Do
          Begin
            If ((K=0)And(L=0)) Then CONTINUE;
            If (PRC[i+k,j+l]=id)And(DTM[i+k,j+l]>MAX)And(FINISH[i+k,j+l]=0) Then
              //Same conditions as above, only the neigbor should not have a lower height
              Begin
                ROWMIN := K;
                COLMIN := L;
                MAX := DTM[I+K,J+L];
                OK2 := true;
              End;
          End;
      If OK2 Then
        Begin
          Routing[i,j].One_Target := True;
          Routing[i,j].Target1Row := I+ROWMIN;
          Routing[i,j].Target1Col := J+COLMIN;
          Routing[i,j].Part1 := 1.0;
        End
      Else
        Begin
          W := 1;
          check := false;
          Repeat
            For k := -W To W Do
              For l := -W To W Do
                Begin
                  If (abs(k)<>W) And (abs(l)<>W) Then continue;

               //The cell itself is not looked at + only the outer cells of the kernel are looked at
                  If ((i+k)<0)Or(i+k>nrow)Or(j+l<0)Or(j+l>ncol) Then continue;
                  //The cells at the border of the map are not looked at
                  If (PRC[i+k,j+l]=id)And(FINISH[i+k,j+l]=0) Then
                    //If the cell is a river and has not been treated yet
                    Begin
                      If check Then //If a target cell has been found
                        break;
                      //If break the current loop is ended
                      check := true;
                      Routing[i,j].One_Target := True;
                      Routing[i,j].Target1Row := I+K;
                      Routing[i,j].Target1Col := J+L;
                      Routing[i,j].Part1 := 1.0;
                    End;
                End;
            Inc(W);
          Until ((check)Or(W>100));

       //100 is the maximum size of the kernel (thus the water is transported 30 cells further away)
        End;
    End;

  //The distance from the source cell to the target cell is determined
  //This is needed to calculate the amount of water that is displaced every timestep,
  //as this is calculated as the displacement distance (x=v*t) divided by the distance
  //between source and taget cell
  // => Cardinal: distance = resolution
  // => Diagonal: distance = sqrt(res² + res²)


  If Routing[i,j].Part1 > 0.9999 Then
    Begin
      r := abs(i-Routing[i,j].Target1row);
      t := abs(j-Routing[i,j].Target1col);
    End
  Else
    If Routing[i,j].Part2 > 0.9999 Then
      Begin
        r := abs(i-Routing[i,j].Target2row);
        t := abs(j-Routing[i,j].Target2col);
		//TODO: johan - controleren of die afatand hier wel klopt
        If (r = 0) Or (t = 0) Then
          Routing[i,j].Distance2 := res
        Else
          Routing[i,j].Distance2 := sqrt(sqr(res) + sqr(res));
      End;
End;

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

Procedure DistributeTilDirEvent_Routing(i,j:integer; Var FINISH:GRaster; Topo:boolean);
// i,j = rij en kolomnummer van de cel onder beschouwing

// Area = De hoeveelheid neerslag gevallen in die cel, vermenigvuldigd met de oppervlakte van die cel
// Input(raster) = UpArea (de geaccumuleerde hoeveelheid water per cel).
// Finish(raster) = een waarde 1 geeft aan dat de cel reeds behandeld is)
// Massbalance = ???
// Topo = wordt meegegeven vanuit CalculateUpareaOlivier

Var 
  CSN,SN,MINIMUM,MINIMUM2,PART1,PART2,extremum : extended;
  K1,K2,l1,L2,ROWMIN,COLMIN,ROWMIN2,COLMIN2,K,L, Area, W : integer;
  parequal,closeriver, check: boolean;
  Direction : single;
  center_x, center_y, center_ID: integer;
Begin
  closeriver := false;

  // Uit code WatemSedem
  For K := -1 To 1 Do
    For L := -1 To 1 Do
      Begin
        If ((K=0)And(L=0)) Then CONTINUE;
        //The pixel itself (i,j) is not evaluated
        If (PRC[i+k,j+l]=-1)Then
          Begin
            If (DTM[i+k,j+l]<DTM[i,j]) Then
              Begin
                closeriver := true;
                // end if
                break;
              End;
            //else closeriver:=false;
          End;
      End;

  //This is our code

{for K := -1 to 1 do
 for L := -1 to 1 do
 begin
  if (PRC[i+k,j+l] = -1) AND (DTM[i+k,j+l] < DTM[i,j]) then //Geldt voor pixels die grenzen aan een rivierpixel
  //Rivierpixels worden nooit geselecteerd want daarvoor is nooit voldaan aan de tweede conditie
  closeriver := true; // The cell has a neighbouring river pixel
 end;}

  If closeriver Then //Voor pixels die aan een rivierpixel grenzen: neem de laagste riviercel
    Begin
      extremum := 99999.9;
      For K := -1 To 1 Do
        For L := -1 To 1 Do
          Begin
            If ((K=0)And(L=0)) Then CONTINUE;
            //The pixel itself (i,j) is not evaluated
            If (PRC[i+k,j+l]=-1)And(DTM[i+k,j+l]<DTM[i,j])And(DTM[i+k,j+l]<extremum) Then
              Begin
                ROWMIN := K;
                COLMIN := L;
                extremum := DTM[i+k,j+l];
              End;
          End;
      Routing[i,j].One_Target := True;
      //All water and sediment flows into the river
      Routing[i,j].Target1Row := i+ROWMIN;
      Routing[i,j].Target1Col := j+COLMIN;
      Routing[i,j].Part1 := 1.0;
      FINISH[i,j] := 1;
      //An identifier 1 is put in the cell when it has been evaluated

    End

  Else   //If the cell under evaluation is not adjacent to a river

    Begin

      PART1 := 0.0;
      PART2 := 0.0;
      k1 := 0;
      l1 := 0;
      k2 := 0;
      l2 := 0;
      If (PRC[i,j] > 0) And (LogReg(i,j)<0.5) And (Not(Topo)) // If tillage direction
        Then   //if logReg<0.5 then flow direction is determined by tillage direction
        Begin
          Direction := degtorad(TilDir[i,j]);
          If SlopeDir(Direction,i,j,DTM)>0.0 Then
            Direction := Direction+Pi;
          If direction>Pi*2.0 Then
            direction := direction-Pi*2.0;
          //Direction wordt aangepast aan de ploegrichting
        End
      Else
        Direction := aspect[i,j];
      //Direction wordt gelijkgesteld aan de steilste helling

      CSN := (ABS(cos(Direction)))/(ABS(SIN(Direction))+ABS(COS(Direction)));
      //Explanation: Desmet and Govers (1996) p.315
      SN := (ABS(sin(Direction)))/(ABS(SIN(Direction))+ABS(COS(Direction)));
      //Deze twee waarden geven de groottes aan van de 2 componenten
      //waarover de totale vector (=upstream area + area of the cell)
      //wordt verdeeld


      Area := 1;
      // obv hiervan wordt de fractie berekend die naar elke buurcel vloeit
      //For every direction (per PI/2: kwadrant) a K1/2 and L1/2 value is choosen

//This is done to determine the coordinates of two cardinal neighboring cells to which material will flow
      If (Direction >= 0.0) And (Direction <= (PI/2)) Then //Eerste kwadrant
        Begin
          PART1 := Area*CSN;

//PART1 = de absolute waarde van de upslope area die wordt verplaatst; OPGELET: noorden is boven dus de hoek gaat van boven naar rechts!!!!! (dus sin en cos worden omgewisseld tov de 'normale' berekening!!!)
          PART2 := Area*SN;
          //PART2 = de absolute waarde van de upslope area die wordt verplaatst
          K1 := -1;
          // Deze K en L waarden geven aan naar welke cellen het materiaal verplaatst wordt
          L1 := 0 ;
          K2 := 0 ;
          L2 := 1 ;
          Routing[i,j].Target1Row := i+K1;
          Routing[i,j].Target1Col := j+L1;
          Routing[i,j].Target2Row := i+K2;
          Routing[i,j].Target2Col := j+L2;
        End
      Else
        Begin
          If (Direction > (PI/2)) And (Direction < PI) Then //tweede kwadrant
            Begin
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
            End
          Else
            Begin
              If (Direction >= PI)And (Direction<= (PI*1.5)) Then //Derde kwadrant
                Begin
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
                End
              Else
                Begin
                  If (Direction>(PI*1.5))Then //Vierde kwadrant
                    Begin
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
                    End;
                End;
            End;
        End;

      If FINISH[i+K1,j+L1]=1 Then // check if receiving cell is already treated
        Begin
          If FINISH[i+K2,j+L2]=1 Then
            Begin
              part1 := 0.0;
              part2 := 0.0;
              //Beide cellen zijn al behandeld en ontvangen dus niets meer
            End
          Else
            Begin
              part2 := part2+part1;
              part1 := 0;
              //Een cel is reeds behandeld, de andere cel ontvangt alles
            End;
        End

      Else // If the first target cell has not been treated yet

        Begin
          If FINISH[i+K2,j+L2]=1 Then
            Begin
              part1 := part1+part2;
              //One cell has been treated, the other one receives everything
              part2 := 0;
            End;
        End;
      // End of checking if the cells have already been treated


//In het volgende deel worden part1 en part2 ingevuld of niet naargelang de ontvangende cellen hoger liggen of tot een ander perceel behoren
      If DTM[i+k1,j+l1]>DTM[i,j] Then //If the first target cell has a higher elevation
        Begin
          If DTM[i+k2,j+l2] > DTM[i,j] Then //If both cells have a higher elevation
            Begin
              part1 := 0.0;
              part2 := 0.0;
              //twee ontvangende cellen liggen hoger dan huidige cel en krijgen dus niets meer
            End
          Else // Only target cell 1 has a higher elevation
            Begin
              If PRC[i+k2,j+l2]<>PRC[i,j] Then
                //indien deze ene cel tot een ander perceel behoort dan..
                Begin
                  If (PRC[i+k2,j+l2] = -6) And (FINISH[i+K2,j+L2]=0) Then
                    // If the target cell is a grass buffer strip it receives everything
                    Begin
                      part2 := part1+part2;

                   // In the distributionflux_LS procedure this is corrected for parcel connectivity
                      part1 := 0;
                    End
                  Else
                    Begin
                      part2 := 0.0;
                      part1 := 0.0;

         //... ontvangt deze cel (voorlopig) niets (preferentiële afstroming langs perceelsgrenzen)
                    End;
                End
              Else //If the parcel of the target cell is the same as the source cell...
                Begin
                  If FINISH[i+K2,j+L2]=0 Then
                    Begin
                      PART2 := PART2+PART1;
                      //...it receives everything
                      PART1 := 0.0;
                    End
                  Else
                    Begin
                      part1 := 0.0;
                      part2 := 0.0;
                    End;
                End;
            End;
        End
      Else //If the first target cell does not have a higher elevation
        Begin
          If DTM[i+k2,j+l2]>DTM[i,j] Then    //cel 1 lagergelegen maar cel 2 hoger...
            Begin
              If PRC[i+k1,j+l1] <> PRC[i,j] Then
                Begin
                  If (PRC[i+k1,j+l1] = -6) And (FINISH[i+K1,j+L1]=0) Then
                    // If the target cell is a grass buffer strip it receives everything
                    Begin
                      part1 := part1+part2;
                      //(part1+part2)*(TFSED_forest/100);
                      part2 := 0;
                    End
                  Else
                    Begin
                      part1 := 0.0;

               // cellen ontvangen voorlopig niets (preferentiële afstroming langs perceelsgrenzen)
                      part2 := 0.0;
                    End;
                End
              Else                          // als tot zelfde perceel behoort../
                Begin
                  If FINISH[i+K1,j+L1]=0 Then
                    Begin
                      part1 := part1+part2;
                      // ... ontvangt ze alles
                      part2 := 0.0;
                    End
                  Else
                    Begin
                      part1 := 0.0;
                      part2 := 0.0;
                    End;
                End;
            End
          Else //Beide targetcellen liggen lager dan de broncel
            Begin
              If PRC[i+k1,j+l1]<>PRC[i,j] Then
                Begin
                  If PRC[i+k2,j+l2]<>PRC[i,j] Then // If both target cell have a different parcel ID
                    Begin
                      If PRC[i+k1,j+l1] = -6 Then // If the first one is a grass buffer strip
                        Begin
                          If PRC[i+k2,j+l2] = -6 Then
                            // If both target cells are grass buffer strips
                            Begin
                              PART1 := PART1;
                              PART2 := PART2;
                            End
                          Else
                            // if only the first target cell is a grass buffer strip
                            Begin
                              If FINISH[i+K1,j+L1]=0 Then
                                Begin
                                  PART1 := part1+part2;
                                  PART2 := 0.0;
                                End
                              Else
                                Begin
                                  part1 := 0.0;
                                  part2 := 0.0;
                                End;
                            End;
                        End
                      Else
                        Begin
                          If (PRC[i+k2,j+l2] = -6) And (FINISH[i+K2,j+L2]=0) Then
                            // if only the 2nd target cell is a grass buffer strip
                            Begin
                              PART2 := part1+part2;
                              PART1 := 0.0;
                            End
                          Else
                            // If none of the target cells is a grass buffer strip
                            Begin
                              Part1 := 0.0;
                              PART2 := 0.0;
                            End;
                        End;
                    End
                  Else // als targetcel 1 tot ander perceel behoort, maar targetcel 2 niet...
                    Begin
                      If PRC[i+k1,j+l1] = -6 Then   // Target cell 1 is a grass buffer strip
                        Begin
                          PART1 := PART1;
                          PART2 := PART2;
                        End
                      Else            // Target cell 1 is not a grass buffer strip
                        Begin
                          If FINISH[i+K2,j+L2]=0 Then
                            Begin
                              PART2 := PART2+PART1;
                              PART1 := 0.0;
                            End
                          Else
                            Begin
                              part1 := 0;
                              part2 := 0;
                            End;
                        End;
                    End;
                End
                // If target cell 1 has the same parcel ID
              Else
                Begin
                  If PRC[i+k2,j+l2]<>PRC[i,j] Then
                    // als enkel targetcel 2 tot ander perceel behoort...
                    Begin
                      If PRC[i+k2,j+l2] = -6 Then
                        // als targetcel 2 een grasbufferstrook of grasgang is...
                        Begin
                          PART1 := PART1;
                          PART2 := PART2;
                        End
                      Else                 // targetcel 2 is geen grasbufferstrook of grasgang
                        Begin
                          If FINISH[i+K1,j+L1]=0 Then
                            Begin
                              PART1 := PART1+PART2;
                              PART2 := 0.0;
                            End
                          Else
                            Begin
                              part1 := 0.0;
                              part2 := 0.0;
                            End;
                        End;
                    End;
                End;
            End;
        End;

      If ((PART1=0.0)And(PART2=0.0)) Then
        // no cells were found (both have a higher or equal elevation or another parcel ID)
        Begin
          PART1 := Area;
          // CODE JEROEN
          parequal := false;
          ROWMIN := 0;
          ROWMIN2 := 0;
          COLMIN := 0;
          COLMIN2 := 0;
          MINIMUM := 99999999.9;
          MINIMUM2 := 99999999.9;
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
          MINIMUM2 := 99999999.9;
}

          // CODE JEROEN
          Repeat
            // if no neighbouring cells are found to be a suitable target cell,
            //the search window is gradually extended until target is found
            For k := -W To W Do
              For l := -W To W Do
                Begin
                  If (abs(k)<>W) And (abs(l)<>W) Then continue;

               //The cell itself is not looked at + only the outer cells of the kernel are looked at
                  If ((i+k)<0)Or(i+k>nrow)Or(j+l<0)Or(j+l>ncol) Then continue;
                  //The cells at the border of the map are not looked at
                  If ((DTM[I+K,J+L]<MINIMUM)And(DTM[I+K,J+L]<DTM[I,J])
                     //Als de bestemmingscel lager gelegen is dan broncel
                     And(FINISH[I+K,J+L]=0)And(PRC[I+K,J+L]=PRC[I,J]))Then
                    //En de bestemminscel nog niet behandeld is EN binnen hetzelfde perceel ligt
                    Begin
                      check := true;
                      MINIMUM := DTM[I+K,J+L];
                      ROWMIN := K;
                      COLMIN := L;
                      parequal := true;
                    End;
                  If ((DTM[I+K,J+L]<MINIMUM2)And(DTM[I+K,J+L]<DTM[I,J]) And(FINISH[I+K,J+L]=0))Then
                    // lager gelegen cel, ander perceel, nog niet behandeld
                    Begin
                      check := true;
                      MINIMUM2 := DTM[I+K,J+L];
                      ROWMIN2 := K;
                      COLMIN2 := L;
                    End;
                End;
            Inc(W);

          Until ((check)Or(W>50));

        //50 is the maximum size of the kernel (thus the water is transported 50 cells further away)

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

          If parequal Then    // If receiving cell is in same parcel
            Begin
              Routing[i,j].One_Target := True;
              Routing[i,j].Target1Row := i+ROWMIN;
              Routing[i,j].Target1Col := j+COLMIN;
              Routing[i,j].Target2Row := 0;
              Routing[i,j].Target2Col := 0;
              Routing[i,j].Part1 := part1{ + part2};
              //TODO johan -  bug?
              Routing[i,j].Part2 := 0;
            End
          Else           // if receiving cell belongs to a different parcel
            Begin
              // determine value of parcel connectivity based on receiving parcel

              Routing[i,j].One_Target := True;
              Routing[i,j].Target1Row := i+ROWMIN2;
              Routing[i,j].Target1Col := j+COLMIN2;
              Routing[i,j].Target2Row := 0;
              Routing[i,j].Target2Col := 0;
              Routing[i,j].Part1 := part1{ + part2};
              //TODO johan - bug?
              Routing[i,j].Part2 := 0;
            End;


        End
        // end if no cells were found
      Else
        Begin
          // normal case part1 or part2 <> 0
          Routing[i,j].Target1Row := i+k1;
          Routing[i,j].Target1Col := j+l1;
          Routing[i,j].Target2Row := i+k2;
          Routing[i,j].Target2Col := j+l2;
          Routing[i,j].Part1 := Part1;
          Routing[i,j].Part2 := Part2;
        End;

      FINISH[I,J] := 1;

      If Routing[i,j].Part1 = 0 Then
        Begin
          Routing[i,j].Target1Row := 0;
          Routing[i,j].Target1Col := 0;
        End;

    End;
  // end if cell is not adjacent to river

  // if cell contains a pond, the routing information is adjusted (water is not allowed to leave)

  If (PRC[i,j]=-5) Then // If the parcel under consideration is open water water
    Begin
      Routing[i,j].One_Target := false;
      Routing[i,j].Target1Row := 0;
      Routing[i,j].Target1Col := 0;
      Routing[i,j].Target2Row := 0;
      Routing[i,j].Target2Col := 0;
      Routing[i,j].Part1 := 0;
      Routing[i,j].Part2 := 0;
      Routing[i,j].Distance1 := 0;
      Routing[i,j].Distance2 := 0;
    End;


  // In the lines below the routing algorithm is adjusted for buffers (opvangbekkens?):
  // Target cell for each buffer pixel = center cell of the buffer it belongs to
  // Target cell for each buffer center cell = the lowest lying neighbour
  // All cells adjacent to buffer and higher then buffer => target cell = buffer cell

  If (Include_buffer) And (Buffermap[i,j] <> 0) Then
    Begin
      For K := -1 To 1 Do
        //a 3*3 kernel is build around cell to which we look
        For L := -1 To 1 Do
          Begin

            If ((K=0)And(L=0)) Then CONTINUE;

            If DTM[i+k,j+l] > DTM[i,j] Then
              //Cells with a higher altitude are assigned the buffer as only target
              Begin
                Routing[i+k,j+l].Target1Row := i;
                Routing[i+k,j+l].Target1Col := j;
                Routing[i+k,j+l].Part1 := 1.0;
                Routing[i+k,j+l].Target2Row := 0;
                Routing[i+k,j+l].Target2Col := 0;
                Routing[i+k,j+l].Part2 := 0;
                Routing[i+k,j+l].One_Target := True;
              End;
          End;

      If Buffermap[i,j] <= Number_of_Buffers Then
        // center of the buffer drains to lowest neighbour
        Begin
          w := 1;
          Minimum := 99999999.9;
          check := false;
          Repeat
            For K := -w To w Do
              //a 3*3 kernel is build around the center of the buffer
              For L := -w To w Do
                Begin
                  If (abs(k)<>W) And (abs(l)<>W) Then continue;
                  If (DTM[i+k,j+l] < DTM[i,j]) And (DTM[i+k,j+l] < Minimum) Then
                    Begin
                      Minimum := DTM[i+k,j+l];
                      Routing[i,j].Target1Row := i+k;
                      Routing[i,j].Target1Col := j+l;
                      Routing[i,j].Part1 := 1.0;
                      Routing[i,j].Target2Row := 0;
                      Routing[i,j].Target2Col := 0;
                      Routing[i,j].Part2 := 0;
                      Routing[i,j].One_Target := True;

                      If (buffermap[i+k,j+l] <> (buffermap[i,j]*100)) Then
                        //Check will be true when the target cell does not belong to the same buffer
                        check := true;
                    End;
                End;
            Inc(W);
          Until ((check)Or(W>50));

        //50 is the maximum size of the kernel (thus the water is transported 50 cells further away)
        End


      Else
        //Voor buffercellen die geen centercell zijn   // buffer pixel drains to buffer center cell
        Begin
          // identify center cell
          center_ID := 0;
          For k := 1 To Number_of_Buffers Do
            Begin
              If Buffermap[i,j] = BufferData[k].ext_ID Then
                center_ID := k;
            End;
          If center_ID =0 Then
            Begin
              raise ECalculationError.Create('Error in buffer input data: center cell of buffer ID '+inttostr(Buffermap
                          [i,j])+' not found in buffer database.');
              Exit;
            End;
          For k := 1 To nrow Do
            // determine coordinates of buffer center
            For l:= 1 To ncol Do
              Begin
                If Buffermap[k,l] = center_ID Then
                  Begin
                    center_x := k;
                    center_y := l;
                  End;
              End;
          Routing[i,j].Target1Row := center_x;
          Routing[i,j].Target1Col := center_y;
          Routing[i,j].Part1 := 1.0;
          Routing[i,j].Target2Row := 0;
          Routing[i,j].Target2Col := 0;
          Routing[i,j].Part2 := 0;
          Routing[i,j].One_Target := True;
        End;
    End;

  If (Include_ditch) And (Ditch_map[i,j] <> 0) Then
    Begin
      Case Ditch_map[i,j] Of 
        // first target cell is determined by user input...
        1:
           Begin
             Routing[i,j].Target1row := i-1;
             Routing[i,j].Target1col := j;
           End;
        2:
           Begin
             Routing[i,j].Target1row := i-1;
             Routing[i,j].Target1col := j+1;
           End;
        3:
           Begin
             Routing[i,j].Target1row := i;
             Routing[i,j].Target1col := j+1;
           End;
        4:
           Begin
             Routing[i,j].Target1row := i+1;
             Routing[i,j].Target1col := j+1;
           End;
        5:
           Begin
             Routing[i,j].Target1row := i+1;
             Routing[i,j].Target1col := j;
           End;
        6:
           Begin
             Routing[i,j].Target1row := i+1;
             Routing[i,j].Target1col := j-1;
           End;
        7:
           Begin
             Routing[i,j].Target1row := i;
             Routing[i,j].Target1col := j-1;
           End;
        8:
           Begin
             Routing[i,j].Target1row := i-1;
             Routing[i,j].Target1col := j-1;
           End;
      End;

      Routing[i,j].Part1 := 1.0;
      // in any case there is no second target cell
      Routing[i,j].Target2Row := 0;
      Routing[i,j].Target2Col := 0;
      Routing[i,j].Part2 := 0;
      Routing[i,j].Distance2 := 0;
      Routing[i,j].One_Target := True;
    End;

  If (Include_dam) And (Dam_map[i,j] <> 0) Then         // same as for ditches
    Begin
      Case Dam_map[i,j] Of 
        1:
           Begin
             Routing[i,j].Target1row := i-1;
             Routing[i,j].Target1col := j;
           End;
        2:
           Begin
             Routing[i,j].Target1row := i-1;
             Routing[i,j].Target1col := j+1;
           End;
        3:
           Begin
             Routing[i,j].Target1row := i;
             Routing[i,j].Target1col := j+1;
           End;
        4:
           Begin
             Routing[i,j].Target1row := i+1;
             Routing[i,j].Target1col := j+1;
           End;
        5:
           Begin
             Routing[i,j].Target1row := i+1;
             Routing[i,j].Target1col := j;
           End;
        6:
           Begin
             Routing[i,j].Target1row := i+1;
             Routing[i,j].Target1col := j-1;
           End;
        7:
           Begin
             Routing[i,j].Target1row := i;
             Routing[i,j].Target1col := j-1;
           End;
        8:
           Begin
             Routing[i,j].Target1row := i-1;
             Routing[i,j].Target1col := j-1;
           End;
      End;

      Routing[i,j].Part1 := 1.0;
      Routing[i,j].Target2Row := 0;
      Routing[i,j].Target2Col := 0;
      Routing[i,j].Part2 := 0;
      Routing[i,j].One_Target := True;
    End;


// if cell contains a sewer, the target river cell is determined and the routing information is adjusted
  If (Include_sewer) And (SewerMap[i,j]<>0) Then
    Begin
      ROWMIN := 0;
      COLMIN := 0;
      MINIMUM := 99999999.9;
      W := 1;
      check := false;
      Repeat

 // find closest LOWER river pixel. If no neighbouring cells are found to be a suitable target cell,
        //the search window is gradually extended until target is found
        For k := -W To W Do
          For l := -W To W Do
            Begin
              If (abs(k)<>W) And (abs(l)<>W) Then continue;
              //The cell itself is not looked at + only the outer cells of the kernel are looked at
              If ((i+k)<0)Or(i+k>nrow)Or(j+l<0)Or(j+l>ncol) Then continue;
              //The cells at the border of the map are not looked at
              If ((DTM[I+K,J+L]<MINIMUM)And(DTM[I+K,J+L]<DTM[I,J])
                 //Als de bestemmingscel een riviercel is, lager gelegen is dan broncel
                 And(FINISH[I+K,J+L]=0)And(PRC[I+K,J+L]=-1))Then
                //En de bestemminscel nog niet behandeld is
                Begin
                  check := true;
                  MINIMUM := DTM[I+K,J+L];
                  ROWMIN := K;
                  COLMIN := L;
                End;
            End;
        Inc(W);
      Until (check);

      If Routing[i,j].Part1 < Routing[i,j].Part2 Then
        // make sure the "strongest" target cell is target 1 (because sewer will be target 2)
        Begin
          Routing[i,j].Target1Row := Routing[i,j].Target2Row;
          Routing[i,j].Target1Col := Routing[i,j].Target2Col;
        End;

      Routing[i,j].One_Target := false;
      Routing[i,j].Target2Row := i+ROWMIN;
      // river pixel = target 2, target 1 remains unaltered
      Routing[i,j].Target2Col := j+COLMIN;
      Routing[i,j].Part1 := 1-SewerMap[i,j];
      // SewerMap[i,j] = vangefficiëntie!
      Routing[i,j].Part2 := SewerMap[i,j];

      // distance to target cell is calculated
      If (K=0) Or (L=0) Then
        Routing[i,j].Distance2 := res*(W-1)
      Else
        Routing[i,j].Distance2 := sqrt(sqr(ROWMIN*res) + sqr(COLMIN*res));
    End;

End;
// end procedure DistributeTilDirEvent_Routing

// ***************************************************************************
// The following 2 functions check whether the elements in an array are equal or
// not.
// ***************************************************************************

Function intArrayIsEqual (inputArray:Array Of Integer): boolean;

Var 
  Val, i: Integer;

Begin
  intArrayIsEqual := True;
  Val := inputArray[1];
  For i := low(inputArray) To High(inputArray) Do
    Begin
      If inputArray[i] <> Val Then
        intArrayIsEqual := false;
    End;
End;

Function doubArrayIsEqual (inputArray:Array Of double): boolean;

Var 
  Val: double;
  i: Integer;

Begin
  doubArrayIsEqual := True;
  Val := inputArray[1];
  For i := low(inputArray) To High(inputArray) Do
    Begin
      If inputArray[i] <> Val Then
        doubArrayIsEqual := false;
    End;
End;

Procedure Calculate_UpstreamArea(Var UPAREA:RRaster);

Var 
  teller,i,j : integer;
  Fluxout: RRaster;
  oppcor: double;
Begin
  SetDynamicRdata(Fluxout);
  SetzeroR(UPAREA);
  For teller:= ncol*nrow Downto 1 Do
    Begin
      // begin lus
      i := row[teller];
      j := column[teller];
      If PRC[i,j]=0 Then continue;
      OPPCOR := (X_resolution()*Y_resolution()) * (1 - (PTEFmap[i,j] / 100));
      //bijdrage van elke cel aan de uparea
      Fluxout[i,j] := OPPCOR+UPAREA[i,j];
      // X_res * Y_res = oppervlakte 1 cel
      DistributeFlux_LS(i,j,UPAREA,Fluxout);
      // volgende cellen worden geïdentificeerd en uparea in deze cellen wordt berekend
      UPAREA[i,j] := UPAREA[i,j]+oppcor;
      //(X_Resolution(i,j)*Y_Resolution(i,j))/2.0;
    End;
  // end matrix loop

  DisposeDynamicRdata(Fluxout);
End;


Procedure CalculateLS(Var LS:RRaster;UPAREA:RRaster);

Var 
  i,j     : integer;
  exp,Sfactor,Lfactor,adjust,B,locres : double;
Begin


  For i:=1 To nrow Do
    Begin
      For j:=1 To ncol Do
        Begin
          // begin matrix loop
          If (PRC[i,j] = 0) Or (PRC[i,j] = -1) Then continue;
          If Raster_projection=plane Then locres := RES
          Else locres := (X_Resolution()+Y_Resolution())/2.0;
          //else fixed res is used
          ADJUST := (ABS(cos(aspect[i,j]))+ABS(sin(aspect[i,j])));
          //todo johan: bepaald maar niet gebruikt!
          B := (sin(slope[i,j])/0.0896)/((3.0*power(sin(slope[i,j]),0.8))+0.56);
          //EXP:=B/(B+1);
          If UPAREA[i,j] < 10000 Then
            EXP := 0.3+POWER((UPAREA[i,j]/10000),0.8)
          Else
            EXP := 0.72;
          If EXP>0.72 Then
            EXP := 0.72;

          Sfactor := -1.5 + 17/(1+power(2.718281828,(2.3-6.1*sin(slope[i,j]))));
          Lfactor := (POWER((Uparea[i,j]+sqr(locres)),EXP+1)-POWER(Uparea[i,j],EXP+1))/
                     (POWER(ADJUST,EXP)*POWER(locres,EXP+2)*POWER(22.13,EXP));
          LS[i,j] := Sfactor*Lfactor;
        End;
      // end matrix loop
    End;

End;
// --------- end procedure CalculateLS------------------------------------

// The following procedure is used to route the LS through the landscape:
// it adapts the LS values for the parcel connectivities
Procedure DistributeFlux_LS(i,j:integer;Var Flux_IN,Flux_OUT: RRaster);

Var 
  flux: double;
Begin
  // flux decomposition algoritme


// Bij de overgang naar een ander perceel wordt de uparea verminders volgens de parcel connectivities
  If (Routing[i,j].one_target = true) And (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] <>
     PRC[i,j]) Then
    // Als er maar 1 targetcel is (dit is steeds wanneer er naar een ander perceel wordt gerout,
    // behalve wanneer de targetcel een grasbufferstrook is)
    // en wanneer die targetcel een andere perceelswaarde heeft
    Begin
      flux := 0;
      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] >= 1) Then
        // Als de targetcel cropland is
        Begin
          flux := FLUX_OUT[i,j]*(TFSED_crop/100);
        End;
      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -3) // Als de targetcel bos,
         Or (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -4) // weide
         Or (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -6) Then // grasbufferstrook
        Begin
          flux := FLUX_OUT[i,j]*(TFSED_forest/100);
        End;

      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -2) Then
        // Target is verhard oppervlak
        flux := FLUX_OUT[i,j];

      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -5) Then
        // Target is a pond (open water parcel but not river)
        flux := FLUX_OUT[i,j];

      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -1) Then // Target is a river pixel
        flux := FLUX_OUT[i,j];

      If Include_buffer Then
        Begin
          If (Buffermap[i,j] > 0) And (Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] >
             0) Then // Both source and target are buffers
            flux := FLUX_OUT[i,j];
        End;

      Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,
                                                                  Routing[i,j].Target1Col] + flux;


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

    End

  Else // If the target cells belong to the same parcel no parcel connectivities
    // have to be taken into account, except when the target cell is a grass buffer strip

    Begin
      flux := 0;
      If Routing[i,j].Part1 > 0.0 Then
        Begin
          If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -6) And (PRC[i,j] <> -6) And (
             Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = 0) Then
            // If this target cell is a grass buffer strip

// The parcel connectivity should only be applied when you go from a non-grassbuffer strip to a bufferstrip
            Begin
              flux := FLUX_OUT[i,j]*(TFSED_forest/100);
              Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].
                                                                          Target1Row,Routing[i,j].
                                                                          Target1Col] + flux;
            End

          Else
          // If this target cell is not a grass buffer strip (or you are within a grassbuffer strip)

            Begin
              flux := FLUX_OUT[i,j]*Routing[i,j].Part1;
              Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].
                                                                          Target1Row,Routing[i,j].
                                                                          Target1Col] + flux;
            End;
        End;

      If Routing[i,j].Part2 > 0.0 Then
        Begin
          If (PRC[Routing[i,j].Target2Row,Routing[i,j].Target2Col] = -6) And (PRC[i,j] <> -6) And (
             Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = 0) Then
            // If this target cell is a grass buffer strip

// The parcel connectivity should only be applied when you go from a non-grassbuffer strip to a bufferstrip
            Begin
              flux := FLUX_OUT[i,j]*(TFSED_forest/100);
              Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] := Flux_IN[Routing[i,j].
                                                                          Target2Row,Routing[i,j].
                                                                          Target2Col] + flux;
            End

          Else
          // If this target cell is not a grass buffer strip (or you are within a grassbuffer strip)

            Begin
              flux := FLUX_OUT[i,j]*Routing[i,j].Part2;
              Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] := Flux_IN[Routing[i,j].
                                                                          Target2Row,Routing[i,j].
                                                                          Target2Col] + flux;
            End;
        End;

    End;


{if (routing[i,j].Part1 = 0.0) and (Routing[i,j].Part2 = 0.0) and (PRC[i,j] <> -5) then
  Showmessage('No target cells determined!' + 'row ' + inttostr(i) + '; ' + 'column: ' + inttostr(j));
}
End;

// The following procedure is used to route the sediment through the landscape
// by distributing the sediment over 1 or 2 target cells according to the
// flux decomposition algorithm
Procedure DistributeFlux_Sediment(i,j:integer;Var Flux_IN,Flux_OUT: RRaster);

Var 
  flux: double;
Begin
  // flux decomposition algoritme
  If Routing[i,j].Part1 > 0.0 Then
    Begin
      flux := FLUX_OUT[i,j]*Routing[i,j].Part1;
      // m³
      Flux_IN[Routing[i,j].Target1Row,Routing[i,j].Target1Col] := Flux_IN[Routing[i,j].Target1Row,
                                                                  Routing[i,j].Target1Col] + flux;
    End;
  If Routing[i,j].Part2 > 0.0 Then
    Begin
      flux := FLUX_OUT[i,j]*Routing[i,j].Part2;
      Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] := Flux_IN[Routing[i,j].Target2Row,
                                                                  Routing[i,j].Target2Col] + flux;
    End;
End;

Procedure Topo_Calculations;
Begin
  sort(DTM);
  //The DTM is sorted from low to high
  CalculateSlopeAspect;
  //Slope and aspect are calculated
  Calculate_routing(Routing);

//The flow direction(s) is calulated for every gridcell + the relative contribution to every receiving neighbour (Marijn)
  Calculate_UpstreamArea(UPAREA);
  CalculateLS(LS,UPAREA);
End;

End.
