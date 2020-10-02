
Unit Raster_calculations;

{$mode objfpc}{$H+}

{$R+}

Interface

Uses
Classes, SysUtils, Math, GData_CN, RData_CN, ReadInParameters;

Type
  ECalculationError = Exception;
  EInvalidValue = Class(Exception);


Procedure CalculateSlopeAspect;
Function LogReg(i,j:integer): double;
Function CalculateSLOPE(i,j:integer): double;
Function CalculateASPECT(i,j:integer) : double;
Function IsRiver(i,j: Integer): boolean;
Procedure Follow_Direction(var routing: TRoutingArray; map: Graster; i, j:integer);
Function SlopeDir(dir:double;i,j:integer;DTM:Rraster): double;
Procedure Calculate_routing(Var Routing: TRoutingArray);
Function Invert_routing(Routing: TRoutingArray): TRoutingInvArray;
Procedure DistributeRiver_Routing(i,j:integer);
Procedure DistributeTilDirEvent_Routing(i,j:integer; Topo:boolean);

Function X_Resolution(): double;
Function Y_Resolution(): double;
Procedure Calculate_UpstreamArea(Var UPAREA:RRaster);
Procedure CalculateLS(Var LS:RRaster;UPAREA:RRaster);
Procedure DistributeUparea(i,j:integer;Var Uparea: RRaster);
Procedure DistributeFlux_Sediment(i,j:integer;Var Flux_IN: RRaster; Flux_OUT: single);
Procedure Topo_Calculations;
Procedure Routing_Slope(Var Routing: TRoutingArray; Var Slope: RRaster);
Procedure Apply_Routing;
Procedure Apply_Buffer(i, j: integer);
Procedure add_queue(var inv: TRoutingInvArray; var q_index, last_index: integer) ;
Function FindLower(i,j, max_kernel: integer): boolean;
procedure addInverse(var inv:TroutingInvArray; i,j,t_c, t_r: integer);

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
  //A cell receives a value of 1 after it had been treated
  k: integer;
Begin

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
      End;

  //For every cell in the catchment the target cell(s) are determined
  For i:= 1 To nrow Do
    For j:= 1 To ncol Do
      Begin
        //Same for columns
        If PRC[i,j]=0 Then continue;
        If IsRiver(i,j) Then
          //Routing procedure for rivers (once water has entered a river it has to stay in the river)
          begin
               if river_routing then
                  DistributeRiver_Routing(i, j);
          end
        Else //Routing procedure for all other cells
          DistributeTilDirEvent_Routing(i,j, Topo);
      End;

  if force_routing then
    begin
      for k:= low(forced_routing) to high(forced_routing) do
      Begin
        i := forced_routing[k].FromRow;
        j := forced_routing[k].FromCol;
        Routing[i,j].Target1Row := forced_routing[k].TargetRow;
        Routing[i,j].Target1Col := forced_routing[k].TargetCol;
        Routing[i,j].Target2Row := -99;
        Routing[i,j].Target2Col := -99;
        Routing[i,j].Part1 := 1;
        Routing[i,j].Part2 := 0.0;
        Routing[i,j].One_Target:=True;

      End;

    end;

  Apply_Routing;
End;

Function Invert_routing(Routing: TRoutingArray): TRoutingInvArray;
var
  inv: TRoutingInvArray;
  i,j: integer;
  t_c, t_r: integer;
  ring: boolean;
  delta1: float;
begin
  SetLength(inv,nrow+2, ncol+2);
  For i := 1 To nrow Do
    //The DTM is read row per row (from l to r), for each next cell that is
    For j := 1 To ncol Do
    begin
      setlength(inv[i, j].up_X, 2);
      setlength(inv[i, j].up_Y, 2);
      inv[i, j].size := 0;
    end;

  For i := 1 To nrow Do
    For j := 1 To ncol Do
     begin;

      if PRC[i,j]=0 then
        continue; // jump out of the loop if we look at cell outside the study area

      if (Routing[i,j].Part1 > 0) and (Routing[i,j].Target1Col > 0) then
        begin
          t_c :=  Routing[i,j].Target1Col ;
          t_r :=  Routing[i,j].Target1Row ;

          // don't add if this creates a pairwise ring and target is higher or equal
          ring :=  (j=routing[t_r,t_c].Target1Col) and (i = routing[t_r,t_c].Target1Row) or
                ((j=routing[t_r,t_c].Target2Col) and (i = routing[t_r,t_c].Target2Row) )   ;
          // add a tiny difference to make sure two equal cells
          // don't route to eachother
          delta1 := (t_r/nrow - i/nrow +t_c/ncol - j/ncol) /4000;
          if ring and (dtm[t_r, t_c] >dtm[i,j] + delta1) then
          begin
             Routing[i,j].Part1:=0;
             Routing[i,j].Target1Col:= -99;
             Routing[i,j].Target1Row:= -99;
             if (Routing[i,j].One_Target) then
             begin
               Routing[i,j].One_Target:= False;
             end;
          end

          else  // not ring
            addInverse(inv, i,j,t_c, t_r);

        end;

      if (Routing[i,j].Part2 > 0) and (Routing[i,j].Target2Col > 0) then
        begin
          t_c :=  Routing[i,j].Target2Col ;
          t_r :=  Routing[i,j].Target2Row ;

          // don't add if this creates a pairwise ring and target is higher or equal
          delta1 := (t_r/nrow - i/nrow +t_c/ncol - j/ncol) /4000;
          ring :=  (j=routing[t_r,t_c].Target1Col) and (i = routing[t_r,t_c].Target1Row) or
           ((j=routing[t_r,t_c].Target2Col) and (i = routing[t_r,t_c].Target2Row) )   ;
          if ring and ((dtm[t_r, t_c] ) >dtm[i,j] + delta1) then
          begin
               Routing[i,j].Target2Col:= -99;
               Routing[i,j].Target2Row:= -99;
               Routing[i,j].Part2:= 0;
               if (Routing[i,j].One_Target) then
               begin
                 Routing[i,j].One_Target := False;
               end;
          end
          else
            addInverse(inv, i,j,t_c, t_r);
        end;

      // check if routing has no target: find a lower cell in the neighborhood and route there
      if (Routing[i,j].target1col<1) and (Routing[i,j].target2col <1) and ((not outlet_select) or (outlet[i,j]=0)) and (PRC[i,j]<>-1) then
        begin
          if FindLower(i,j,max_kernel) then
            begin
                t_c :=  Routing[i,j].Target1Col ;
                t_r :=  Routing[i,j].Target1Row ;
                addInverse(inv, i,j,t_c, t_r);
            end;
        end;

     end;
  Invert_routing:= inv;
end;

procedure addInverse(var inv:TroutingInvArray; i,j,t_c, t_r: integer);
var
  pos:integer;
begin
    pos := inv[t_r, t_c].size;

    inv[t_r, t_c].size := pos+1;
    if pos > length(inv[t_r, t_c].up_X)-1 then
    begin
      setlength(inv[t_r, t_c].up_X, length(inv[t_r, t_c].up_X)*2);
      setlength(inv[t_r, t_c].up_Y, length(inv[t_r, t_c].up_Y)*2);
    end;

    inv[t_r, t_c].up_X[pos] := i;
    inv[t_r, t_c].up_y[pos] := j;

end;

procedure settreatedsize(var inv: TRoutingInvArray) ;
var
 i,j, k: integer;
begin
   For i := 1 To nrow Do
    //The DTM is read row per row (from l to r), for each next cell that is
    For j := 1 To ncol Do
       begin
         setlength(inv[i,j].treated, inv[i, j].size);
         for k:=0 to inv[i, j].size -1 do
           inv[i,j].treated[k] :=false;
       end;
end;

procedure setpointtreated(var inv: TRoutingInvArray; var last_index:integer; i,j,t_r, t_c: integer);
// Sets the status of the goal cel [t_r, t_c] to treated for this origin cell
// add it to the queue if all its upstream cells have been treated
var
 k: integer;
 all_treated: boolean;
begin
   for k:=0 to inv[t_r, t_c].size -1 do
    if (inv[t_r, t_c].up_X[k] = i) and (inv[t_r, t_c].up_Y[k] = j) then
      begin
           inv[t_r, t_c].treated[k]:=true;
      end;

   all_treated := true;

   for k:=0 to inv[t_r, t_c].size -1 do
    all_treated := all_treated and inv[t_r, t_c].treated[k];

   if all_treated then
     begin
        row[last_index] := t_r;
        column[last_index] := t_c;
        last_index+=1;
     end;
end;

procedure getstartingpoints(inv: TRoutingInvArray; var last_index: integer);
var
  i,j: integer;
begin

  For i := 1 To nrow Do
    //The DTM is read row per row (from l to r), for each next cell that is
    For j := 1 To ncol Do
       begin
         setlength(inv[i,j].treated, inv[i, j].size);

         If PRC[i,j]=0 Then continue;
         if inv[i,j].size=0 then
           begin
             row[last_index] := i;
             column[last_index] := j;
             last_index+=1;
           end;
       end;
end;

Procedure Missing_routes(inv: TRoutingInvArray);
var
  i,j,k: integer;
  routingfile: textfile;
  sep: char;
Begin
  // in case any circular routing was determined, it should be broken

  setcurrentDir(File_output_dir);
  sep := #9;
  assignfile(routingfile, 'routing_missing.txt');
  rewrite(routingfile);
  Writeln(routingfile,
          'col'+sep+'row'+sep+'target1col'+sep+'target1row'+sep+'part1'+sep+'distance1'+sep+'target2col'+sep+'target2row'+sep+'part2'+sep+'distance2');
    For i:=1 To nrow Do
        For j:=1 To ncol Do
            for k:= 0 to inv[i,j].size -1 do
                if not inv[i,j].treated[k] then
                  writeln(routingfile, inttostr(inv[i,j].up_Y[k]) + sep  + inttostr(inv[i,j].up_X[k]) +sep+inttostr( j)+sep+inttostr(i));

    closefile(routingfile);
end;

Procedure Apply_Routing;
Var
  inv: TRoutingInvArray;
  q_index, last_index: integer;
Begin
       // invert routing
  inv:= Invert_routing(Routing);
  settreatedsize(inv);

  // save the columns and rows in the order they are followed
  // used in upstream area, sediment distribution and cn

    SetLength(column, NROW*NCOL);
    SetLength(row, NROW*NCOL);

    last_index := 0;
    q_index := 0;


    getstartingpoints(inv, last_index);

    // as long as there are elements in the queue, continue
  // new elements are added when all their parents have been handled

  while (last_index >= q_index) do
  begin
    i := row[q_index];
    j := column[q_index];
    add_queue(inv, q_index, last_index);
  end;

  missing_routes(inv);
end;

procedure add_queue(var inv: TRoutingInvArray; var q_index, last_index: integer) ;
var
  t_r, t_c: integer;
begin
  If Routing[i,j].Part1 > 0.0 Then
    begin
      t_r := Routing[i, j].Target1Row;
      t_c := Routing[i, j].Target1Col;

      setpointtreated(inv, last_index, i,j,t_r, t_c);
    end;
    If Routing[i,j].Part2 > 0.0 Then
     begin
       t_r := Routing[i, j].Target2Row;
       t_c := Routing[i, j].Target2Col;

       setpointtreated(inv, last_index, i,j,t_r, t_c);
     end;
   q_index+=1;

end;

//******************************************************************************
//In this procedure the procedures to calculate the slope and aspect are called
//******************************************************************************
Procedure CalculateSlopeAspect;

Var
  i,j: integer;
Begin
  SetDynamicRData(Slope);
  SetDynamicRData(Aspect);
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

Procedure Follow_Direction(var routing: TRoutingArray; map: Graster; i, j:integer);
// Follows the direction set in map used in river_routing, ditches, dam
begin

Case map[i,j] Of
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
  else
       raise EInvalidValue.Create('Invalid value in river direction');
End;



Routing[i,j].Part1 := 1.0;
Routing[i,j].Target2Row := -99;
Routing[i,j].Target2Col := -99;
Routing[i,j].Part2 := 0;
Routing[i,j].One_Target := True;

end;

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
// Berekent the helling in een richting (dir)
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
Procedure DistributeRiver_Routing(i,j:integer);
var
  k, l, segment, nextsegment, rowmin, colmin: integer;
  OK, check: boolean;
  w: integer;
Begin
  segment := rivseg[i,j];

  OK := false;

  rowmin:=0;
  colmin:=0;


  if (river_routing_map[i,j]>0)  then
    begin
    Follow_Direction(routing, river_routing_map,i,j);
    OK:= true;
    end;

    If not OK Then

    begin
      // no more adjectant cells in the segment --> flow to next segment

      nextsegment := river_adjectant[segment];

      if (nextsegment = 0) then
        exit;

      w:=1;

      check := false;

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
                  If (rivseg[I+K,J+L] = nextsegment)   Then
                    Begin
                      check := true;
                      ROWMIN := K;
                      COLMIN := L;
                    End;
                End;
            Inc(W);

          Until ((check)Or(W>max_kernel));
          If (W>max_kernel) Then
          begin
              Routing[i,j].One_Target := False;
              Routing[i,j].Target1Row := -99;
              Routing[i,j].Target1Col := -99;
              Routing[i,j].Part1 := 0;
              exit;
          end;
    end;
    if ((rowmin<>0) or (colmin<>0)) then
    begin
      Routing[i,j].One_Target := true;
      Routing[i,j].Target1Row := I+ROWMIN;
      Routing[i,j].Target1Col := J+COLMIN;
      Routing[i,j].Part1 := 1.0;
    end
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
Procedure DistributeTilDirEvent_Routing(i,j:integer; Topo:boolean);
// i,j = rij en kolomnummer van de cel onder beschouwing

// Area = De hoeveelheid neerslag gevallen in die cel, vermenigvuldigd met de oppervlakte van die cel
// Input(raster) = UpArea (de geaccumuleerde hoeveelheid water per cel).
// Topo = wordt meegegeven vanuit CalculateUpareaOlivier

Var
  CSN,SN,PART1,PART2,extremum : extended;
  K1,K2,l1,L2,ROWMIN,COLMIN,K,L, Area : integer;
  closeriver, closeditchdam, criterium: boolean;
  Direction : single;
Begin
  closeriver := false;
  closeditchdam := false;
  colmin:=0;
  rowmin:=0;


   // In the lines below the routing algorithm is adjusted for buffers (opvangbekkens?):
  // Target cell for each buffer pixel = center cell of the buffer it belongs to
  // Target cell for each buffer center cell = the lowest lying neighbour
  // All cells adjacent to buffer and higher then buffer => target cell = buffer cell

  If (Include_buffer) And (Buffermap[i,j] <> 0) Then
    Begin
     Apply_Buffer(i,j);
     // we should process dam, ditch and buffer if it is the endpoint of the buffer,
     // otherwise we should skip further steps.
     if (Buffermap[i,j] > 16384) then
        exit;
   End;

    If (Include_ditch) And (Ditch_map[i,j] <> 0) Then
    Begin
      Follow_Direction(routing, ditch_map, i, j);
      exit;
    End;

  If (Include_dam) And (Dam_map[i,j] <> 0) Then         // same as for ditches
    Begin
      Follow_Direction(routing, dam_map, i, j);
      exit;
    End;


  For K := -1 To 1 Do
    For L := -1 To 1 Do
      Begin
        If ((K=0)And(L=0)) Then CONTINUE;
        //The pixel itself (i,j) is not evaluated
        If (PRC[i+k,j+l]=-1)Then
            closeriver := true;

        If  include_ditch and (Ditch_map[i+k,j+l]<>0) Then
            closeditchdam := true;

        If (Include_dam) And (Dam_map[i+k,j+l]<> 0) Then
            closeditchdam := true;

      End;

  If closeriver or closeditchdam Then

  begin
  //Voor pixels die aan een rivierpixel grenzen: neem de laagste riviercel

   If closeriver then
    Begin
      extremum := 99999.9;
      For K := -1 To 1 Do
        For L := -1 To 1 Do
          Begin
            If ((K=0)And(L=0)) Then CONTINUE;
            //The pixel itself (i,j) is not evaluated
            If (PRC[i+k,j+l]=-1)And(DTM[i+k,j+l]<extremum) Then
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
      //An identifier 1 is put in the cell when it has been evaluated
      exit;
    End; // end closeriver

   // for ditch or dam:
   If closeditchdam then
     begin
     extremum := 99999;
      For K := -1 To 1 Do
        For L := -1 To 1 Do
          Begin
            //The pixel itself (i,j) is not evaluated
            If ((K=0)And(L=0)) Then CONTINUE;

            criterium :=  (Include_dam and (Dam_map[i+k,j+l]<> 0)) or   (Include_ditch and (Ditch_map[i+k,j+l]<> 0));

            If criterium And(DTM[i+k,j+l]<extremum) Then
              Begin
                ROWMIN := K;
                COLMIN := L;
                extremum := DTM[i+k,j+l];
              End;

          End;
      if (rowmin<>0) or (colmin<>0) then
        begin
          Routing[i,j].One_Target := True;
          //All water and sediment flows into the ditch/dam
          Routing[i,j].Target1Row := i+ROWMIN;
          Routing[i,j].Target1Col := j+COLMIN;
          Routing[i,j].Part1 := 1.0;
          exit;
         end;
     end;

  end;


      PART1 := 0.0;
      PART2 := 0.0;
      k1 := 0;
      l1 := 0;
      k2 := 0;
      l2 := 0;
      If (Not(Topo)) and (PRC[i,j] > 0) And (LogReg(i,j)<0.5) // If tillage direction
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
                  If (PRC[i+k2,j+l2] = -6) Then
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
                      PART2 := PART2+PART1;
                      //...it receives everything
                      PART1 := 0.0;
                End;
            End;
        End
      Else //If the first target cell does not have a higher elevation
        Begin
          If DTM[i+k2,j+l2]>DTM[i,j] Then    //cel 1 lagergelegen maar cel 2 hoger...
            Begin
              If PRC[i+k1,j+l1] <> PRC[i,j] Then
                Begin
                  If (PRC[i+k1,j+l1] = -6) Then
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
                      part1 := part1+part2;
                      // ... ontvangt ze alles
                      part2 := 0.0;
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

                                  PART1 := part1+part2;
                                  PART2 := 0.0;
                            End;
                        End
                      Else
                        Begin
                          If (PRC[i+k2,j+l2] = -6) Then
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
                              PART2 := PART2+PART1;
                              PART1 := 0.0;
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
                              PART1 := PART1+PART2;
                              PART2 := 0.0;
                        End;
                    End;
                End;
            End;
        End;

      If ((PART1=0.0)And(PART2=0.0)) Then
        FindLower(i,j,  max_kernel)
      Else
        Begin
          // normal case part1 or part2 <> 0
          Routing[i,j].Target1Row := i+k1;
          Routing[i,j].Target1Col := j+l1;
          Routing[i,j].Target2Row := i+k2;
          Routing[i,j].Target2Col := j+l2;
          Routing[i,j].Part1 := Part1;
          Routing[i,j].Part2 := Part2;
          Routing[i,j].One_Target:=False;
        End;


      If Routing[i,j].Part1 = 0 Then
        Begin
          Routing[i,j].Target1Row := -99;
          Routing[i,j].Target1Col := -99;
        End;


End;
// end procedure DistributeTilDirEvent_Routing


// procedure which ir run when no neigbouring lower parcel is found
function FindLower(i,j, max_kernel: integer): boolean;
var
rowmin, rowmin2, colmin, colmin2, w, k,l : integer;
minimum, minimum2: float;
check, parequal: boolean;

  Begin
    parequal := false;
    ROWMIN := 0;
    ROWMIN2 := 0;
    COLMIN := 0;
    COLMIN2 := 0;
    MINIMUM := 99999999.9;
    MINIMUM2 := 99999999.9;
    W := 1;
    check := false;

    // CODE JEROEN
    Repeat
      // if no neighbouring cells are found to be a suitable target cell,
      // the search window is gradually extended until target is found
      // river cells are always considered lower than the original cell.
      For k := -W To W Do
        For l := -W To W Do
          Begin
            If (abs(k)<>W) And (abs(l)<>W) Then continue;

         //The cell itself is not looked at + only the outer cells of the kernel are looked at
            If ((i+k)<1)Or(i+k>=nrow)Or(j+l<1)Or(j+l>=ncol) Then continue;
            //The cells at the border of the map are not looked at
            If ((DTM[I+K,J+L]<MINIMUM)And(DTM[I+K,J+L]<DTM[I,J])
               //Als de bestemmingscel lager gelegen is dan broncel
               And(PRC[I+K,J+L]=PRC[I,J]))Then
              //En de bestemminscel nog niet behandeld is EN binnen hetzelfde perceel ligt
              Begin
                check := true;
                MINIMUM := DTM[I+K,J+L];
                ROWMIN := K;
                COLMIN := L;
                parequal := true;
              End;

            // als er een rivier in de zoekstraal is springen we naar
            // de laagste riviercel die in de buurt ligt
            If ((DTM[I+K,J+L]<MINIMUM2) AND (PRC[I+K,J+L]=-1) )Then
              Begin;
                check := true;
                MINIMUM2 := DTM[I+K,J+L];
                ROWMIN2 := K;
                COLMIN2 := L;
              end;
            If ((DTM[I+K,J+L]<MINIMUM2)And(DTM[I+K,J+L]<DTM[I,J]) and not check)Then
              // lager gelegen cel, ander perceel, nog niet behandeld, enkel indien geen rivier of cel in eigen peceel
              Begin
                check := true;
                MINIMUM2 := DTM[I+K,J+L];
                ROWMIN2 := K;
                COLMIN2 := L;
              End;

          End;
      Inc(W);

    Until ((check) Or(W>max_kernel));
    If (W>max_kernel) Then
    begin
        Routing[i,j].One_Target := False;
        Routing[i,j].Target1Row := -99;
        Routing[i,j].Target1Col := -99;
        Routing[i,j].Target2Row := -99;
        Routing[i,j].Target2Col := -99;
        Routing[i,j].Part1 := -99;
        Routing[i,j].Part2 := -99;
        findlower:= False;
        exit;

    end;


    If parequal Then    // If receiving cell is in same parcel
      Begin
        Routing[i,j].One_Target := True;
        Routing[i,j].Target1Row := i+ROWMIN;
        Routing[i,j].Target1Col := j+COLMIN;
        Routing[i,j].Target2Row := -99;
        Routing[i,j].Target2Col := -99;
        Routing[i,j].Part1 := 1;
        Routing[i,j].Part2 := 0;
      End
    Else           // if receiving cell belongs to a different parcel
      Begin
        Routing[i,j].One_Target := True;
        Routing[i,j].Target1Row := i+ROWMIN2;
        Routing[i,j].Target1Col := j+COLMIN2;
        Routing[i,j].Target2Row := -99;
        Routing[i,j].Target2Col := -99;
        Routing[i,j].Part1 := 1;
        Routing[i,j].Part2 := 0;
      End;

          findlower:= True;
end;

Procedure Calculate_UpstreamArea(Var UPAREA:RRaster);

Var
  teller,i,j : integer;
  oppcor: double;

Begin
  SetDynamicRData(UPAREA);  //allocate memory
  // SetnodataR(UPAREA);

  // set all valid cells to zero
  for teller:=0 to nrow*ncol-1 do
    begin
    // begin lus
    i := row[teller];
    j := column[teller];
    if (i=0) and (j=0) then
      continue;

    If PRC[i,j]=0 Then
        continue;

    uparea[i,j] := 0;
    end;

  for teller:=0 to nrow*ncol-1 do
      begin
      i := row[teller];
      j := column[teller];
      if (i=0) and (j=0) then
        break;

      If PRC[i,j]=0 Then
          continue;

      OPPCOR := (X_resolution()*Y_resolution()) * (1 - (PTEFmap[i,j] / 100));
      UPAREA[i,j] := UPAREA[i,j]+oppcor;
      DistributeUparea(i,j,UPAREA);
    End;
  DisposeDynamicGData(PTEFmap);
  // end matrix loop

End;


Procedure CalculateLS(Var LS:RRaster;UPAREA:RRaster);
Var
  i,j     : integer;
  exp,Sfactor,Lfactor,adjust,B,locres : double;
Begin
  SetDynamicRData(LS);  // allocate memory

  // adjust the slope to the slope according to the actual routing table
  if adjusted_slope then
    Routing_Slope(routing, slope);

  For i:=1 To nrow Do
    Begin
      For j:=1 To ncol Do
        Begin
          // begin matrix loop
          If (PRC[i,j] = 0) Or (PRC[i,j] = -1) or (uparea[i,j] = -9999) Then
            begin
              LS[i,j] := -9999;
              continue;
            end;
          If Raster_projection=plane Then locres := RES
          Else locres := (X_Resolution()+Y_Resolution())/2.0;
          //else fixed res is used
          ADJUST := ABS(cos(aspect[i,j]))+ABS(sin(aspect[i,j]));

          if LModel = TLModel.Desmet1996_Vanoost2003 Then
            Begin
              If UPAREA[i,j] < 10000 Then
                EXP := 0.3+POWER((UPAREA[i,j]/10000),0.8)
              Else
                EXP := 0.72;
              If EXP>0.72 Then
                EXP := 0.72;
            end;
          if LModel = TLModel.Desmet1996_McCool Then
            Begin
              B := (sin(slope[i,j])/0.0896)/((3.0*power(sin(slope[i,j]),0.8))+0.56);
              EXP:=B/(B+1);
            end;

          Lfactor := (POWER((Uparea[i,j]+sqr(locres)),EXP+1)-POWER(Uparea[i,j],EXP+1))/
                     (POWER(ADJUST,EXP)*POWER(locres,EXP+2)*POWER(22.13,EXP));

          if SModel = TSModel.Nearing1997 then
             Sfactor := -1.5 + 17/(1+power(2.718281828,(2.3-6.1*sin(slope[i,j]))));

          if SModel = TSModel.McCool1987 then
             begin
               If (ArcTan(slope[i,j])*100.0 < 9.0) Then
                     Sfactor := (10.8*sin(slope[i,j]))+0.03
               Else Sfactor := (16.8*sin(slope[i,j]))-0.5;
             end;

          LS[i,j] := (Sfactor*Lfactor)/LScor;
        End;
      // end matrix loop
    End;
End;
// --------- end procedure CalculateLS------------------------------------

// The following procedure is used to route the LS through the landscape:
// it adapts the LS values for the parcel connectivities
Procedure DistributeUparea(i,j:integer;Var UpArea: rraster);
Var
  flux, ptef, fluxout: double;
  BufferId: integer;
Begin
  fluxout:=UpArea[i,j];
  If Include_buffer and (Buffermap[i,j] > 16384) Then
    Begin
      flux := fluxout;

      if (Routing[i,j].Part1 > 0) and (Buffermap[Routing[i,j].Target1Row,Routing[i,j].Target1Col] >0) then
        begin
         BufferId := Buffermap[i,j] - 16384;
         ptef :=1-BufferData[BufferId].PTEF/100;
         flux := fluxout * ptef;
        end;

      if (Routing[i,j].Part1 > 0) then
        begin
        UpArea[Routing[i,j].Target1Row,Routing[i,j].Target1Col] += Routing[i,j].Part1 * flux;

        end;

      if (Routing[i,j].Part2 > 0) then
        UpArea[Routing[i,j].Target2Row,Routing[i,j].Target2Col] += Routing[i,j].Part2 * flux;

      exit;
    End;

  If Include_sewer and (SewerMap[i,j] > 0) Then
    Begin
     flux := fluxout;
      if (Routing[i,j].Part1 > 0) then
        begin
        UpArea[Routing[i,j].Target1Row,Routing[i,j].Target1Col] += Routing[i,j].Part1 * flux * (1-SewerMap[i,j]);
         end;

      if (Routing[i,j].Part2 > 0) then
        UpArea[Routing[i,j].Target2Row,Routing[i,j].Target2Col] += Routing[i,j].Part2 * flux * (1-SewerMap[i,j]);
      exit;

    end;
// flux decomposition algoritme


// Bij de overgang naar een ander perceel wordt de uparea verminderd volgens de parcel connectivities
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
          flux := fluxout*(TFSED_crop/100);
        End;
      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -3) // Als de targetcel bos,
         Or (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -4) // weide
         Or (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -6) Then // grasbufferstrook
        Begin
          flux := fluxout*(TFSED_forest/100);
        End;

      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -2) Then
        // Target is verhard oppervlak
        flux := fluxout;

      If (PRC[Routing[i,j].Target1Row,Routing[i,j].Target1Col] = -1) Then // Target is a river pixel
        flux := fluxout;



      UpArea[Routing[i,j].Target1Row,Routing[i,j].Target1Col] += flux;


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
              flux := fluxout*(TFSED_forest/100);
              UpArea[Routing[i,j].Target1Row,Routing[i,j].Target1Col] += flux;
            End

          Else
          // If this target cell is not a grass buffer strip (or you are within a grassbuffer strip)

            Begin
              flux := fluxout*Routing[i,j].Part1;
              UpArea[Routing[i,j].Target1Row,Routing[i,j].Target1Col] += flux;
            End;
        End;

      If Routing[i,j].Part2 > 0.0 Then
        Begin
          If (PRC[Routing[i,j].Target2Row,Routing[i,j].Target2Col] = -6) And (PRC[i,j] <> -6) And (
             Buffermap[Routing[i,j].Target2Row,Routing[i,j].Target2Col] = 0) Then
            // If this target cell is a grass buffer strip

// The parcel connectivity should only be applied when you go from a non-grassbuffer strip to a bufferstrip
            Begin
              flux := fluxout*(TFSED_forest/100);
              UpArea[Routing[i,j].Target2Row,Routing[i,j].Target2Col] += flux;
            End

          Else
          // If this target cell is not a grass buffer strip (or you are within a grassbuffer strip)

            Begin
              flux := fluxout*Routing[i,j].Part2;
              UpArea[Routing[i,j].Target2Row,Routing[i,j].Target2Col] +=flux;
            End;
        End;

    End;


End;

// The following procedure is used to route the sediment through the landscape
// by distributing the sediment over 1 or 2 target cells according to the
// flux decomposition algorithm
Procedure DistributeFlux_Sediment(i,j:integer;Var Flux_IN: RRaster;flux_out: single);

Var
  flux: double;
Begin
  // flux decomposition algoritme
  If Routing[i,j].Part1 > 0.0 Then
    Begin
      flux := FLUX_OUT*Routing[i,j].Part1;
      // m³
      Flux_IN[Routing[i,j].Target1Row, Routing[i,j].Target1Col] += flux;
    End;
  If Routing[i,j].Part2 > 0.0 Then
    Begin
      flux := FLUX_OUT*Routing[i,j].Part2;
      Flux_IN[Routing[i,j].Target2Row,Routing[i,j].Target2Col] += flux;

    End;
End;

Procedure Topo_Calculations;
Begin
  CalculateSlopeAspect;
  //Slope and aspect are calculated
  Calculate_routing(Routing);

  //The flow direction(s) is calulated for every gridcell + the relative contribution to every receiving neighbour (Marijn)
  Calculate_UpstreamArea(UPAREA);
  CalculateLS(LS,UPAREA);
End;

Procedure Routing_Slope(Var Routing: TRoutingArray; Var Slope: RRaster);
// This procedure overwrites the slope using the actual direction of the routing table
// if the routing is not using the standard split discharge.
Var
  i, j, target_row, target_col: integer;
  s1, s2: double;
Begin
  for i := nrow downto 1 do
    for j:= ncol downto 1 do
      begin
        if PRC[i,j] = 0 then
          begin
          slope[i,j] := -9999;
          continue;
          end;
        if  (Routing[i][j].Part1 > 0.0000001) and (Routing[i][j].Part2 > 0.0000001) then
          continue; // the original slope is used

        target_row := Routing[i,j].Target1Row;
        target_col := Routing[i,j].Target1Col;
        if Routing[i][j].Part1 > 0.0000001 then
           s1 := (DTM[i,j] - DTM[target_row, target_col]) / Distance1(Routing,i,j)
        else
           s1:= 0;
        target_row := Routing[i,j].Target2Row;
        target_col := Routing[i,j].Target2Col;
        if Routing[i][j].Part2 > 0.0000001 then
           s2 := (DTM[i,j] - DTM[target_row, target_col]) / Distance2(Routing, i,j)
        else
           s2 :=0;
        slope[i,j] := arctan(sqrt(sqr(s1) + sqr(s2)))
      end;

End;

Procedure Apply_Buffer(i, j:integer);
var
  w, k, l, center_id, center_x, center_y: integer;
  minimum: double;
  check: boolean;
Begin
  If Buffermap[i,j] <= Number_of_Buffers Then
    // center of the buffer drains to lowest neighbour
    Begin
      w := 1;
      Repeat
        Minimum := 99999999.9;
        check := false;

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

                  If (buffermap[i+k,j+l] <> buffermap[i,j]+16384) Then
                    //Check will be true when the target cell does not belong to the same buffer
                    check := true;
                End;
            End;
        Inc(W);
      Until ((check)Or(max_kernel>50));

    //max_kernel is the maximum size of the kernel (thus the water is transported 50 cells further away)
    End


  Else
    //Voor buffercellen die geen centercell zijn   // buffer pixel drains to buffer center cell
    Begin
      // identify center cell
      center_ID := 0;
      For k := 1 To Number_of_Buffers Do
        Begin
          If Buffermap[i,j] = BufferData[k].ext_ID Then
            begin
            center_ID := k;
            break;
            end;
        End;
      If center_ID =0 Then
        Begin
          raise ECalculationError.Create('Error in buffer input data: center cell of buffer ID '+inttostr(Buffermap
                      [i,j])+' not found in buffer database.');
          Exit;
        End;

      center_x :=BufferData[center_id].row;
      center_y :=BufferData[center_id].col;

      Routing[i,j].Target1Row := center_x;
      Routing[i,j].Target1Col := center_y;
      Routing[i,j].Part1 := 1.0;
      Routing[i,j].Target2Row := -99;
      Routing[i,j].Target2Col := -99;
      Routing[i,j].Part2 := 0;
      Routing[i,j].One_Target := True;
    End;
End;


End.
