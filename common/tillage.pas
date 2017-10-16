
Unit tillage;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils,  RData_CN, Raster_calculations, ReadInParameters;

Procedure tillage_dif;

Implementation

Var 
  SEDI_OUT: RRaster;

Procedure tillage_dif;
// Based on Watem Tillage model

Var
  i,j,o,p: integer;
  CSN,SN,PART1,PART2 : double;
  asp : double;
  k1,l1,k2,l2: smallint;
  K3: double;
  SEDI_IN: Rraster;
  adjust,area, ploweros : double;
Begin
  // Create temp 2D maps
  SetDynamicRData(SEDI_IN);
  SetDynamicRData(SEDI_OUT);
  //************************

  CalculateSlopeAspect;

  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        // begin eerste matrix-lus

        If PRC[i,j]<=0 Then continue;
        // skip cells outside catchment + cells which are no cropland
        If DTM[i,j] < DTM[lowOutletX, lowOutletY] Then continue;
        // skip cells that are lower than lowest outlet
        K3 := ktil[i,j];

        If Raster_projection=plane Then area := sqr(res)
        Else  area := X_Resolution()*Y_Resolution();

        // Calculate Sediment (OUTM) & Carbon (CSOUT) outflow for cell i,j
        ADJUST := (ABS(cos(aspect[i,j]))+ABS(sin(aspect[i,j])));
        SEDI_OUT[i,j] := K3*sin(slope[i,j])*RES*ADJUST/BD;
        //OUTFLOW in m3
        //////


// Calculate distribution of outflow flux over neighbours using flux decomposition algorithm (Desmet & Govers, 1997)
        PART1 := 0.0;
        PART2 := 0.0;
        k1 := 0;
        l1 := 0;
        k2 := 0;
        l2 := 0;
        CSN := (ABS(cos(aspect[i,j])))/(ABS(SIN(aspect[i,j]))+ABS(COS(aspect[i,j])));
        SN := (ABS(sin(aspect[i,j])))/(ABS(SIN(aspect[i,j]))+ABS(COS(aspect[i,j])));
        asp := aspect[i,j]*(180/PI);
        //aspect from rad to degrees

        If (asp >= 0.0) And (asp <= 90.0) Then
          Begin
            PART1 := CSN;
            PART2 := SN;
            K1 := -1;
            L1 := 0 ;
            K2 := 0 ;
            L2 := 1 ;
          End
        Else
          Begin
            If (asp > 90.0) And (asp < 180.0) Then
              Begin
                PART1 := SN;
                PART2 := CSN;
                K1 := 0;
                L1 := 1;
                K2 := 1;
                L2 := 0;
              End
            Else
              Begin
                If (asp >= 180.0)And (asp<= 270.0) Then
                  Begin
                    PART1 := CSN;
                    PART2 := SN;
                    K1 := 1;
                    L1 := 0;
                    K2 := 0;
                    L2 := -1;
                  End
                Else
                  Begin
                    If (asp>270.0)Then
                      Begin
                        PART1 := SN;
                        PART2 := CSN;
                        K1 := 0;
                        L1 := -1;
                        K2 := -1;
                        L2 := 0;
                      End;
                  End;
              End;
          End;
        ///////////////

        // Assign outgoing flux to Incoming cells for sediment (SEDI_IN)
        If PRC[i+K1,j+L1]=PRC[i,j] Then
          SEDI_IN[i+K1,j+L1] := SEDI_IN[i+K1,j+L1]+PART1*SEDI_OUT[i,j]
        Else
          SEDI_OUT[i,j] := SEDI_OUT[i,j]-PART1*SEDI_OUT[i,j];

        If PRC[i+K2,j+L2]=PRC[i,j] Then
          SEDI_IN[i+K2,j+L2] := SEDI_IN[i+K2,j+L2]+PART2*SEDI_OUT[i,j]
        Else
          SEDI_OUT[i,j] := SEDI_OUT[i,j]-PART2*SEDI_OUT[i,j];

      End;
  //einde matrixlus

  For o := 1 To nrow Do
    // tweede matrixlus nadat alle in- en outfluxen berekend zijn
    For p := 1 To ncol Do
      Begin
        If PRC[o,p]<1 Then continue;
        If Raster_projection=plane Then area := sqr(res)
        Else  area := X_Resolution()*Y_Resolution();
        ploweros := (SEDI_IN[o,p]-SEDI_OUT[o,p])/area;
        // unit: m
        TILEROS[o,p] := ploweros*1000;
        // unit: mm

      End;
  //                                     Einde  tweede matrixlus

  // Dispose Temp 2D maps
  DisposeDynamicRdata(SEDI_IN);
  DisposeDynamicRdata(SEDI_OUT);
  //********************
End;




End.
