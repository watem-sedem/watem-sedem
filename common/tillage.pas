
Unit tillage;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils,  RData_CN, Raster_calculations, ReadInParameters;

Procedure tillage_dif;

Implementation


Procedure tillage_dif;
// Based on Watem Tillage model

Var
  teller, i, j, o, p: integer;
  adjust, area, ploweros : double;
Begin
  // Create temp 2D maps
  SetDynamicRData(SEDTIL_IN);
  SetDynamicRData(SEDTIL_OUT);
  SetZeroR(SEDTIL_IN);
  SetZeroR(SEDTIL_OUT);

  SetDynamicRData(TILEROS);
  SetDynamicRData(TILEROS_kg);
  SetZeroR(TILEROS);
  SetZeroR(TILEROS_kg);

  If Raster_projection=plane Then area := sqr(res)
      Else  area := X_Resolution()*Y_Resolution();

  for teller:=0 to nrow*ncol-1 do
      begin
      // begin lus
      i := row[teller];
      j := column[teller];

      If PRC[i,j]<=0 Then
         BEgin
         SEDTIL_OUT[i,j] := 0; // skip cells outside catchment + cells which are no cropland
         End
      // If DTM[i,j] < DTM[lowOutletX, lowOutletY] Then SEDTIL_OUT[i,j] := 0; // skip cells that are lower than lowest outlet
      Else
          Begin
          // Calculate Sediment (OUTM) & Carbon (CSOUT) outflow for cell i,j
          ADJUST := (ABS(cos(aspect[i,j]))+ABS(sin(aspect[i,j])));
          SEDTIL_OUT[i,j] := ktil[i,j]*sin(slope[i,j])*RES*ADJUST/BD; //OUTFLOW in m3
          End ;

      If SEDI_OUT[i,j] > 0 Then DistributeFlux_Sediment(i, j, SEDTIL_IN, SEDTIL_OUT[i,j]);    // if sed_output_file leaves this pixel, the sed_output_file needs to be distributed over target cells
      end;


  For o := 1 To nrow Do
    // tweede matrixlus nadat alle in- en outfluxen berekend zijn
    For p := 1 To ncol Do
      Begin
        If PRC[o,p]<1 Then
          begin
            if PRC[o,p] = 0 then
            Begin
                 //set no data
               SEDTIL_OUT[o,p] := -9999;
               SEDTIL_IN[o,p] := -9999;
               TILEROS[o,p] := -9999;
            end;
            continue;
          end;
        ploweros := (SEDTIL_IN[o,p]-SEDTIL_OUT[o,p])/area;
        // unit: m
        TILEROS[o,p] := ploweros*1000;
        // unit: mm

        // Convert in and outgoing sediment to kg
        SEDTIL_IN[o,p] := SEDTIL_IN[o,p] * BD;
        SEDTIL_OUT[o,p] := SEDTIL_OUT[O,p] * BD;
        TILEROS_kg[o,p] := SEDTIL_IN[o,p]-SEDTIL_OUT[o,p]

      End;
  //Einde  tweede matrixlus
  //********************
End;
End.
