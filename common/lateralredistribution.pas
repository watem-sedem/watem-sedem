
Unit LateralRedistribution;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, FileUtil, RData_CN, ReadInParameters,
Raster_calculations, math, CN_calculations, GData_CN, write_output;

Procedure Water;
Procedure Distribute_sediment;
procedure Cumulative_sections;
procedure Cumulative_raster;
Function followriver(var i,j: integer): boolean;

Implementation

Var
  Waterero, sedprod, depprod: double;
  SedLoad, SedLoad_VHA, SedLoad_VHA_Cumulative: RVector;

Procedure Calculatewaterero(i, j: integer);
//waterero in meter: for the pixel that is being considered

Var                                         //rill,interrill en cap in m≥
  capacity, area, Distcorr, Ero_Potential: double;

Begin

  If (Include_dam) And (Dam_map[i, j] <> 0) Then
    // adjust C factor, P factor and kTc for dams
    Begin
      C_factor[i, j] := 0.01;
      P_factor[i, j] := 1;
      ktc[i,j] := ktc_low;
    End;

  If (Include_ditch) And (Ditch_map[i, j] <> 0) Then
    // adjust C factor, P factor and kTc for ditches
    Begin
      C_factor[i, j] := 0;
      P_factor[i, j] := 1;
      ktc[i,j] := 9999;
    End;

  //The next lines of code are not used in WS, so they are commented out:

// in oude versie W/S model werd een buffer rondom een rivier gecreëerd met een zeer hoge KTc => hier ook nodig?

{for k := -1 to 1 do
    for l := -1 to 1 do
    begin
      if (k = 0) and (l = 0) then
        continue; // Skip the cell under consideration
      if PRC[i + k, j + l] = -1 then // If a cell around the cell under consideration is a river
        kTc[i, j] := 9999;
      // The ktc value for the cell next to the river is set to 9999
    end;}

  Waterero := 0.0;
  If Raster_projection = plane Then
    Begin
      area := sqr(RES);
      Distcorr := (RES * (ABS(sin(aspect[i, j])) + ABS(cos(aspect[i, j]))));
      // Correction factor for conversion to grid cell dimension
    End
  Else
    Begin
      area := X_resolution() * Y_resolution();
      Distcorr := (Y_Resolution() * ABS(sin(aspect[i, j])) + X_Resolution()
      	* ABS(cos(aspect[i, j])));
      // CHECK !!!
    End;

  // Erosion equations

  if (P_factor[i, j] = 0) or (LS[i, j] = -9999) then
      RUSLE[i, j] := -9999
  else
      RUSLE[i, j] := RFactor * C_factor[i, j] * P_factor[i, j] * K_Factor[i, j] * LS[i, j];
  // kg/(m² * yr) (ok)
  // Erosion in kg/m²

  // Capacity := ktc[i,j] * RUSLE[i, j] * distcorr;   // in kg (ok) KTC ZOALS IN VERSIE JEROEN
  Capacity := ktc[i,j] * RFactor * K_Factor[i, j] * (LS[i, j] - 0.6 * 6.86 * power(tan(slope[i,j]),
              0.8));
  // [kg/m] VOLGENS CODES BASTIAAN;


  // Verwijderd want niet aanwezig in WatemSedem2015

{
  if (C_factor[i, j] = 0) then                     // correction for roads and ditches
    //Capacity := RFactor * K_Factor[i, j] * LS[i, j] * ktc[i, j] * distcorr;
    Capacity := ktc[i, j] * RFactor * K_Factor[i, j] * (LS[i, j] - 0.6 * 6.86 * power(tan(slope[i,j]), 0.8)); // [kg/m] VOLGENS CODES BASTIAAN;
  }
  //*******************************

  Ero_Potential := RUSLE[i, j] * area / BD;
  // in m³ (ok) CODE JEROEN
  If capacity < 0.0 Then capacity :=  ktc[i,j] *  RUSLE[i,j];

  capacity := capacity*distcorr;
  // [kg] VOLGENS CODES BASTIAAN
  capacity := capacity / BD;
  // in m³ (ok)

  If (SEDI_IN[i, j] + Ero_Potential) > capacity Then
    // If the amount of soil that enters the cell is
    // larger than the transport capacity
    Begin
      SEDI_OUT[i, j] := capacity;
      // SEDI_OUT [m³]
      // The amount leaving the cell = transport capacity
      Waterero := (SEDI_IN[i, j] - capacity) / area;
      // in m (ok)
    End
  Else
    Begin
      SEDI_OUT[i, j] := SEDI_IN[i, j] + Ero_Potential;
      // SEDI_OUT [m³]
      Waterero := -(Ero_Potential) / area;
      // in m (ok)
    End;
  WATEREROS[i, j] := Waterero;
  CAPAC[i,j] := capacity*BD;
  // in m (ok)
  // in m      if < 0 => erosion & if > 0 => sedimentation
End;

Procedure Water;

Var 
  teller, i, j, k, l, m, n, tc, tr: integer;
  area, sewer_out_sed, TEMP_river_sed_input, TEMP_outside_sed_input, TEMP_buffer_sed_input: double;
  skip: boolean;
  sed_output_file, sediment_VHA, cal_output_file: textfile;

Begin
    //Create maps
  SetDynamicRData(SEDI_IN);
  SetDynamicRData(SEDI_OUT);
  SetDynamicRData(SEDI_EXPORT);
  SetDynamicRData(WATEREROS);
  SetDynamicRData(WATEREROS_cubmeter);
  SetDynamicRData(WATEREROS_kg);
  SetDynamicRData(RUSLE);
  SetDynamicRData(CAPAC);
  if include_sewer Then
    begin
    SetDynamicRData(SEWER_IN);
    SetzeroR(SEWER_IN);
    sewer_out_sed := 0;
    end;

  SetzeroR(SEDI_IN);
  SetzeroR(SEDI_OUT);
  SetzeroR(SEDI_EXPORT);

  If VHA Then //If the user wants output per river segment
    Begin
      numRivSeg := calcRivSeg(RivSeg);
      // check that numRivSeg calculated from the raster is not smaller than the
      // one used in the input files
      for i:=low(river_adjectant.key) to high(river_adjectant.key) do
         if river_adjectant.key[i] > numRivSeg then
            raise EInputException.Create('Larger number of segments in adjectant segment file than in raster -check the input');

      setlength(sedload_VHA, numRivSeg + 1);
      for i :=1 to numRivSeg Do
        sedload_VHA[i] :=0;
      //The length of a vector per river segment (+1) is set
    End;

  TEMP_river_sed_input := 0;
  TEMP_outside_sed_input := 0;
  TEMP_buffer_sed_input := 0;

    for teller:=0 to nrow*ncol-1 do
      begin
      // begin lus
      i := row[teller];
      j := column[teller];

     skip:=false;
      If (PRC[i, j] = 0) Or (PRC[i, j] = -1) Then
        // if cell is outside area or a river cell or a water body => = all export cells
        // This means that also cells outside the study area and ponds are included in the calculation of sed_output_file leaving the catchment?
        Begin
          If (PRC[i, j] = -1) Then
            TEMP_river_sed_input := TEMP_river_sed_input + SEDI_IN[i, j];
          If (PRC[i, j] = 0) Then
            TEMP_outside_sed_input := TEMP_outside_sed_input + SEDI_IN[i, j];

          SEDI_EXPORT[i, j] := SEDI_IN[i, j];
          // assign export sed_output_file (in m³) value for export cells
          If (VHA) And (RivSeg[i, j] <> 0) Then
            sedload_VHA[RivSeg[i, j]] := sedload_VHA[RivSeg[i, j]] + SEDI_EXPORT[i, j];
          // totale hoeveelheid sed_output_file per rivier segment wordt opgeslagen
          skip :=true;
        End;

      If (Include_buffer) And (Buffermap[i, j] <> 0) And
         (Buffermap[i, j] <= Number_of_Buffers) Then    // only center cells of buffers
        Begin
          If Raster_projection = plane Then
            area := sqr(RES)
          Else
            area := X_resolution() * Y_resolution();
          SEDI_OUT[i, j] := SEDI_IN[i, j] * (1 - (Bufferdata[Buffermap[i, j]].PTEF / 100));
          If SEDI_OUT[i,j]<SEDI_IN[i,j] Then
            TEMP_buffer_sed_input := TEMP_buffer_sed_input + (SEDI_IN[i,j]-SEDI_OUT[i, j]);
          DistributeFlux_Sediment(i, j, SEDI_IN, SEDI_OUT[i,j]);
          WATEREROS[i, j] := (SEDI_IN[i, j] - SEDI_OUT[i, j]) / area;
          // in m
          WATEREROS[i, j] := WATEREROS[i, j] * 1000;
          // in mm
          skip:=true;
        End;

      if not skip then
        begin
          CalculateWaterEro(i, j);

          WATEREROS[i, j] := WATEREROS[i, j] * 1000;
          // [mm] (ok)
          area := sqr(RES);
          WATEREROS_cubmeter[i,j] := WATEREROS[i, j] * Area / 1000;
          WATEREROS_kg[i,j] := WATEREROS_cubmeter[i,j] * BD;

          If (PRC[i, j] <> 0) And (PRC[i, j] <> -1) Then
            Begin
              If SEDI_IN[i, j] - SEDI_OUT[i, j] < 0 Then
                sedprod := sedprod + ((SEDI_IN[i, j] - SEDI_OUT[i, j]) * BD) //BD [kg/m³]
                           // sedprod [kg]
              Else
                depprod := depprod + ((SEDI_IN[i, j] - SEDI_OUT[i, j]) * BD);
              // depprod [kg]
            End;

          If (Include_sewer) And (SewerMap[i, j] <> 0) Then
        // if pixel contains sewer, total amount of sed_output_file leaving the system through sewer is updated
            Begin
            // AND SEDI_IN is corrected because procedure Distribute_Flux doesn't take this into account
              SEWER_IN[i,j] := (SEDI_IN[i, j] * SewerMap[i, j]);
              sewer_out_sed := sewer_out_sed + SEWER_IN[i,j];
              SEDI_OUT[i,j] := SEDI_IN[i,j] - SEWER_IN[i,j]; // wat nog uit de pixel na vermindering komt en verdeeld moet worden over de target pixels

              if Routing[i, j].Target2Row > 0 then
                begin
                   tc :=  Routing[i, j].Target2Col;
                   tr := Routing[i, j].Target2Row ;
                   SEDI_IN[tr, tc] += - SEDI_OUT[i,j] * Routing[i,j].Part2;
                end;
              if Routing[i, j].Target1Row > 0 then
                begin
                   tc :=  Routing[i, j].Target1Col;
                   tr := Routing[i, j].Target1Row;
                   SEDI_IN[tr, tc] += - SEDI_OUT[i,j] * Routing[i,j].Part1;
                end;
            End;

          If SEDI_OUT[i,j] > 0 Then
            // if sed_output_file leaves this pixel, the sed_output_file needs to be distributed over target cells
            DistributeFlux_Sediment(i, j, SEDI_IN, SEDI_OUT[i,j]);
          // SEDI_IN [m³]
      end; //skip
    End;

  // releasing memory of some inputmaps
  if not Calibrate then
    Begin;
  DisposeDynamicGdata(K_Factor);
  DisposeDynamicRdata(C_factor);
  DisposeDynamicRdata(P_factor);
    end;

  If Write_Routing_CR then
   Write_Routing_Table_RC(column, row);

  // loop over the full grid and set the output to no-data if
  // it was not part of the routing
      For i := 1 To nrow Do
    //The DTM is read row per row (from l to r), for each next cell that is
         For j := 1 To ncol Do
            if (PRC[i,j] = 0) then
            begin;
              RUSLE[i, j] := -9999;
              SEDI_OUT[i, j] := -9999;
              SEDI_IN[i, j] := -9999;
              WATEREROS[i, j] := -9999;
              WATEREROS_cubmeter[i, j] := -9999;
              WATEREROS_kg[i, j] := -9999;
              UPAREA[i,j] := -9999;
              If (Include_sewer) then
                 begin
                  SEWER_IN[i,j] := -9999;
                 end;
            end;

  //***********

  // voor elke outletpixel wordt nu de totale sedimentvracht berekend die binnenkomt
  // voor rivierpixel = som van alle sed_output_file dat terecht komt in alle hogergelegen rivierpixels
  // voor pixel op land = SEDI_IN voor die pixel

  setlength(sedload, numOutlet + 1);
  For i := 1 To numOutlet Do
    Begin
      sedload[i] := 0;
      k := OutletArray[i, 0];
      //row of the outlet
      l := OutletArray[i, 1];
      // column of the outlet
      If PRC[k, l] = -1 Then // If the outlet is a river
        Begin
          For m := 1 To nrow Do
            For n := 1 To ncol Do
              Begin
                If (PRC[m, n] = -1) And (DTM[m, n] >= DTM[k, l]) Then
                  sedload[i] := sedload[i] + SEDI_EXPORT[m, n];
                // Here SEDI_EXPORT [m³]
                //      sedload [m³]
              End;
        End
      Else
        sedload[i] := SEDI_IN[k, l];
      // [m³]

      sedload[i] := sedload[i] * BD;
      // convert sedload to kg
    End;

  // voor elk riviersegment wordt sed_output_file omgezet naar kg
  If VHA Then
    Begin
      For i := 1 To numRivSeg Do
        sedload_VHA[i] := sedload_VHA[i] * BD;
    End;

  // sedload has to be written to a .txt file as output of the model

  if not Calibrate Then
    Begin
  setcurrentDir(File_output_dir);
  assignfile(sed_output_file, 'Total sediment.txt');
  rewrite(sed_output_file);
  Writeln(sed_output_file, 'Total erosion: ' + floattostr(round(sedprod*100)/100) + ' (kg)');
  Writeln(sed_output_file, 'Total deposition: ' + floattostr(round(depprod*100)/100) + ' (kg)');
  Writeln(sed_output_file, 'Sediment leaving the catchment, via the river: ' + floattostr(round((
          TEMP_river_sed_input * BD)*100)/100) + ' (kg)');
  Writeln(sed_output_file, 'Sediment leaving the catchment, not via the river: ' + floattostr(round((
          TEMP_outside_sed_input * BD)*100)/100) + ' (kg)');
  Writeln(sed_output_file, 'Sediment trapped in buffers: ' + floattostr(round((TEMP_buffer_sed_input * BD)*
  100)/100) + ' (kg)');
  If (Include_sewer) Then
    begin
    Writeln(sed_output_file, 'Sediment entering sewer system: ' + floattostr(round((sewer_out_sed * BD)*100)/100) + ' (kg)');
    end;
  Writeln(sed_output_file,'_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _');
  Writeln(sed_output_file,'');
  Writeln(sed_output_file, 'Total sediment passing at each outlet [kg]');
  // write title
  Write(sed_output_file, 'Outlet ID', chr(9), 'Sediment');
  // write column headings
  writeln(sed_output_file, '');
  // go to next line

  For i := 1 To numOutlet Do
    Begin
      Write(sed_output_file, IntToStr(i), chr(9), floattostr(sedload[i]));
      writeln(sed_output_file, '');
    End;
  closefile(sed_output_file);
  //The memory of sed_output_file is released
  End
  Else
    begin
    // also write to cal_output_file
      setcurrentDir(File_output_dir);
      assignfile(cal_output_file, 'calibration.txt');

      append(cal_output_file);
      Write(cal_output_file, Formatfloat('0.00', ktc_low) + ';' + Formatfloat('0.00', ktc_high) +';');
      Write(cal_output_file, Formatfloat('0.00', sedprod) + ';' +FormatFloat('0.00', depprod) +';');
      Write(cal_output_file, Formatfloat('0.00', TEMP_river_sed_input * BD) + ';');
      Write(cal_output_file, Formatfloat('0.00', TEMP_outside_sed_input * BD) + ';');
      Write(cal_output_file, Formatfloat('0.00', TEMP_buffer_sed_input * BD) + ';');

      // also write to every outlet
      For i := 1 To numOutlet Do
      Begin
        Write(cal_output_file, ';' + floattostr(sedload[i]));
      End;

      writeln(cal_output_file, '');
      closefile(cal_output_file);
    end;


  If VHA Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(Sediment_VHA, 'Total sediment VHA.txt');
      rewrite(Sediment_VHA);
      Writeln(Sediment_VHA, 'Total sediment flowing into each VHA river segment [kg]');
      // write title
      Write(Sediment_VHA, 'VHA segment', chr(9), 'Sediment');
      // write column headings
      writeln(Sediment_VHA, '');
      // go to next line

      For i := 1 To numRivSeg Do
        Begin
          Write(Sediment_VHA, IntToStr(i), chr(9), floattostr(sedload_VHA[i]));
          writeln(Sediment_VHA, '');
        End;
      closefile(Sediment_VHA);
      //The memory of sed_output_file is released
    End;

    If river_routing Then
    Begin
      // Calculate cumulative sediment for VHA sections
      Cumulative_sections;

      setcurrentDir(File_output_dir);
      assignfile(Sediment_VHA, 'Cumulative sediment VHA.txt');
      rewrite(Sediment_VHA);
      Writeln(Sediment_VHA, 'Cumulative sediment flowing into each VHA river segment [kg]');
      // write title
      Write(Sediment_VHA, 'VHA segment', chr(9), 'Sediment');
      // write column headings
      writeln(Sediment_VHA, '');
      // go to next line

      For i := 1 To numRivSeg Do
        Begin
          Write(Sediment_VHA, IntToStr(i), chr(9), floattostr(sedload_VHA_cumulative[i]));
          writeln(Sediment_VHA, '');
        End;
      closefile(Sediment_VHA);
      //The memory of sed_output_file is released
    End;

  //converting SEDI_IN, SEDI_OUT and SEDI_EXPORT to kg

  For m := 1 To nrow Do
    For n := 1 To ncol Do
      Begin
        if PRC[m,n]=0 then
          begin
            SEDI_IN[m,n] := -9999;
            SEDI_OUT[m,n] := -9999;
            SEDI_EXPORT[m,n] := -9999;
            if (Include_sewer) Then
              Begin
                SEWER_IN[m,n]:=-9999;
              end;
          end
        else
        begin
        SEDI_IN[m,n] := SEDI_IN[m,n] * BD;
          SEDI_OUT[m,n] := SEDI_OUT[m,n] * BD;
          //depprod2[m,n] := SEDI_IN[m,n]-SEDI_OUT[m,n];
          SEDI_EXPORT[m,n] := SEDI_EXPORT[m,n] * BD;
          if (include_sewer) Then
            Begin
              SEWER_IN[m,n]:=SEWER_IN[m,n]*BD;
            end;
        end;
      End;
  //********************
End;

//sediment dat toekomt in elke outlet lineair verdelen over hydrogram
// resultaat wegschrijven naar .txt file (zoals voor discharge)

Procedure distribute_sediment;

Var 
  i, j, NTimesteps: integer;
  ER_clay: double;
  clay_cont, clay_cont_VHA: floatArray;
  fraction_discharge, fraction_discharge_VHA, sediment_result,
  sediment_conc, sediment_conc_VHA, sediment_result_VHA: floatArray2;
  sediment, sediment_VHA, sed_conc, sed_conc_VHA, clay_txt, clay_VHA_txt: textfile;

Begin
  If convert_output Then
    NTimesteps := NTimesteps2
  Else
    NTimesteps := NumberofTimesteps;

  // sediment load is distributed over hydrogram and written to .txt file

  setlength(fraction_discharge, NTimesteps + 1, numOutlet + 1);
  setlength(sediment_result, NTimesteps + 1, numOutlet + 1);
  For i := 0 To NTimesteps Do
    For j := 1 To numOutlet Do
      Begin
        If Sum_discharge[j] > 0 Then
          fraction_discharge[i, j] := Result_Discharge[i, j] / Sum_discharge[j]
        Else
          fraction_discharge[i, j] := 0;

        sediment_result[i, j] := fraction_discharge[i, j] * sedload[j];
      End;

  // write sediment_result to .txt

  setcurrentDir(File_output_dir);
  assignfile(Sediment, 'Sediment.txt');
  rewrite(Sediment);
  If convert_output Then
    Begin
      Writeln(Sediment, 'Sediment passing at each outlet [kg] with ',
              Timestep_output, ' minutes timestep');
      // write title
      Write(Sediment, 'Time (min)', chr(9));
    End
  Else
    Begin
      Writeln(Sediment, 'Sediment passing at each outlet [kg] with ',
              Timestep_model, ' seconds timestep');
      // write title
      Write(Sediment, 'Time (sec)', chr(9));
    End;
  For j := 1 To numOutlet Do
    Write(Sediment, 'Outlet ', j, chr(9));
  // write column headings
  writeln(Sediment, '');
  // go to next line

  If convert_output Then
    Begin
      For i := 0 To NTimeSteps2 Do
        Begin
          Write(Sediment, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
          For j := 1 To numOutlet Do
            Write(Sediment, floattostr(sediment_result[i, j]), chr(9));
          //Amount of sediment per time step is written to the .txt file
          writeln(Sediment, '');
        End;
    End
  Else
    Begin
      For i := 0 To NumberOfTimesteps Do
        Begin
          Write(Sediment, IntToStr(RainData[i].Time), chr(9));
          For j := 1 To numOutlet Do
            Write(Sediment, floattostr(sediment_result[i, j]), chr(9));
          //Amount of sediment per time step is written to the .txt file
          writeln(Sediment, '');
        End;
    End;
  closefile(Sediment);
  //The memory of Sediment is released

  // sediment load for each VHA segment is distributed over hydrogram and written to .txt file

  If VHA Then
    Begin
      setlength(fraction_discharge_VHA, NTimesteps + 1, numRivSeg + 1);
      setlength(sediment_result_VHA, NTimesteps + 1, numRivSeg + 1);
      For i := 0 To NTimesteps Do
        For j := 1 To numRivSeg Do
          Begin
            If Sum_discharge_VHA[j] = 0 Then
              sediment_result_VHA[i, j] := 0
            Else
              Begin
                fraction_discharge_VHA[i, j] := Result_Discharge_VHA[i, j] / Sum_discharge_VHA[j];
                sediment_result_VHA[i, j] := fraction_discharge_VHA[i, j] * sedload_VHA[j];
              End;
          End;

      // write sediment_result_VHA to .txt
      assignfile(Sediment_VHA, 'Sediment_VHA.txt');
      rewrite(Sediment_VHA);
      If convert_output Then
        Begin
          Writeln(Sediment_VHA, 'Sediment flowing in each river segment [kg] with ',
                  Timestep_output, ' minutes timestep');
          // write title
          Write(Sediment_VHA, 'Time (min)', chr(9));
        End
      Else
        Begin
          Writeln(Sediment_VHA, 'Sediment passing in each river segment [kg] with ',
                  Timestep_model, ' seconds timestep');
          // write title
          Write(Sediment_VHA, 'Time (sec)', chr(9));
        End;
      For j := 1 To numRivSeg Do
        Write(Sediment_VHA, 'VHA segment ', j, chr(9));
      // write column headings
      writeln(Sediment_VHA, '');
      // go to next line

      If convert_output Then
        Begin
          For i := 0 To NTimeSteps2 Do
            Begin
              Write(Sediment_VHA, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sediment_VHA, floattostr(sediment_result_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sediment_VHA, '');
            End;
        End
      Else
        Begin
          For i := 0 To NumberOfTimesteps Do
            Begin
              Write(Sediment_VHA, IntToStr(RainData[i].Time), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sediment_VHA, floattostr(sediment_result_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sediment_VHA, '');
            End;
        End;
      closefile(Sediment_VHA);
      //The memory of Sediment is released
    End;


  // sediment concentrations are calculated and written to .txt file
  setlength(sediment_conc, NTimesteps + 1, numOutlet + 1);
  For i := 0 To NTimesteps Do
    For j := 1 To numOutlet Do
      Begin
        If Result_Discharge[i, j] = 0 Then
          sediment_conc[i, j] := 0
        Else
          sediment_conc[i, j] := (sediment_result[i, j] * 1000) / (Result_Discharge[i, j] * 1000);
        // in g/l
      End;

  setcurrentDir(File_output_dir);
  assignfile(Sed_conc, 'Sediment concentration.txt');
  rewrite(Sed_conc);
  Writeln(Sed_conc, 'Sediment concentration at each outlet [g/l]');
  // write title
  If convert_output Then
    Write(Sed_conc, 'Time (min)', chr(9))
  Else
    Write(Sed_conc, 'Time (sec)', chr(9));
  For j := 1 To numOutlet Do
    Write(Sed_conc, 'Outlet ', j, chr(9));
  // write column headings
  writeln(Sed_conc, '');
  // go to next line

  If convert_output Then
    Begin
      For i := 0 To NTimeSteps2 Do
        Begin
          Write(Sed_conc, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
          For j := 1 To numOutlet Do
            Write(Sed_conc, floattostr(sediment_conc[i, j]), chr(9));
          writeln(Sed_conc, '');
        End;
    End
  Else
    Begin
      For i := 0 To NumberOfTimesteps Do
        Begin
          Write(Sed_conc, IntToStr(RainData[i].Time), chr(9));
          For j := 1 To numOutlet Do
            Write(Sed_conc, floattostr(sediment_conc[i, j]), chr(9));
          writeln(Sed_conc, '');
        End;
    End;
  closefile(Sed_conc);


  If VHA Then
    Begin
      setlength(sediment_conc_VHA, NTimesteps + 1, numRivSeg + 1);
      For i := 0 To NTimesteps Do
        For j := 1 To numRivSeg Do
          Begin
            If Result_Discharge_VHA[i, j] = 0 Then
              sediment_conc_VHA[i, j] := 0
            Else
              sediment_conc_VHA[i, j] := 
                                         (sediment_result_VHA[i, j] * 1000) / (Result_Discharge_VHA[
                                         i, j] * 1000);
            // in g/l
          End;

      setcurrentDir(File_output_dir);
      assignfile(Sed_conc_VHA, 'Sediment concentration_VHA.txt');
      rewrite(Sed_conc_VHA);
      Writeln(Sed_conc_VHA, 'Sediment concentration for each VHA river segment [g/l]');
      // write title
      If convert_output Then
        Write(Sed_conc_VHA, 'Time (min)', chr(9))
      Else
        Write(Sed_conc_VHA, 'Time (sec)', chr(9));
      For j := 1 To numRivSeg Do
        Write(Sed_conc_VHA, 'VHA segment ', j, chr(9));
      // write column headings
      writeln(Sed_conc_VHA, '');
      // go to next line

      If convert_output Then
        Begin
          For i := 0 To NTimeSteps2 Do
            Begin
              Write(Sed_conc_VHA, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sed_conc_VHA, floattostr(sediment_conc_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sed_conc_VHA, '');
            End;
        End
      Else
        Begin
          For i := 0 To NumberOfTimesteps Do
            Begin
              Write(Sed_conc_VHA, IntToStr(RainData[i].Time), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sed_conc_VHA, floattostr(sediment_conc_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sed_conc_VHA, '');
            End;
        End;
      closefile(Sed_conc_VHA);
      //The memory of Sediment is released
    End;

  If est_clay Then
    // estimate the clay content of the sediment based on enrichment ratio
    Begin
      setlength(clay_cont, numOutlet + 1);
      For i := 1 To numOutlet Do
        Begin
          ER_clay := (0.7732 * exp(-0.0508 * sediment_conc[1, i])) + 1;
          // formula Wang et al 2010
          clay_cont[i] := ER_clay * clay_parent;
        End;

      // write result to .txt file
      setcurrentDir(File_output_dir);
      assignfile(clay_txt, 'Clay content sediment.txt');
      rewrite(clay_txt);
      Writeln(clay_txt, 'Clay content of sediment at each outlet [%]');
      // write title
      Write(clay_txt, 'Outlet ID', chr(9), 'Clay content (%)');
      // write column headings
      writeln(clay_txt, '');
      // go to next line
      For i := 1 To numOutlet Do
        Begin
          Write(clay_txt, IntToStr(i), chr(9), floattostr(clay_cont[i]));
          writeln(clay_txt, '');
        End;
      closefile(clay_txt);

      If VHA Then     // estimate clay content of sediment for each VHA river segment
        Begin
          setlength(clay_cont_VHA, numRivSeg + 1);
          For i := 1 To numRivSeg Do
            Begin
              ER_clay := (0.7732 * exp(-0.0508 * sediment_conc_VHA[1, i])) + 1;
              // formula Wang et al 2010
              clay_cont_VHA[i] := ER_clay * clay_parent;
            End;

          // write result to .txt file
          setcurrentDir(File_output_dir);
          assignfile(clay_VHA_txt, 'Clay content sediment VHA.txt');
          rewrite(clay_VHA_txt);
          Writeln(clay_VHA_txt,
                  'Clay content of sediment flowing in each river segment [%]');
          // write title
          Write(clay_VHA_txt, 'River segment', chr(9), 'Clay content (%)');
          // write column headings
          writeln(clay_VHA_txt, '');
          // go to next line
          For i := 1 To numRivSeg Do
            Begin
              Write(clay_VHA_txt, IntToStr(i), chr(9), floattostr(clay_cont_VHA[i]));
              writeln(clay_VHA_txt, '');
            End;
          closefile(clay_VHA_txt);
        End;
    End;


End;


procedure Cumulative_sections;
var
  i: integer;
Begin
  setlength(SedLoad_VHA_Cumulative, length(sedload_vha));
  for i:= 0 to length(sedload_vha)-1 do
     SedLoad_VHA_Cumulative[i] := sedload_vha[i];

  for i:= 0 to length(river_upstream.key)-1 do
  begin
     SedLoad_VHA_Cumulative[river_upstream.key[i]] += sedload_vha[river_upstream.value[i]] * river_upstream_proportion.value[i];

  end;
End;

// if river routing is enabled: make a grid with the cumulative sediment output in rivers.
procedure Cumulative_raster;
Var
 i,j, seg: integer ;
 temp: double;
 min_segment, min_col, min_row: TIntArray;  // slaat de laagste pixelwaarde op voor elk segment
 // dit wordt de startwaarde om cumulatieve som te nemen.
Begin
  setlength(min_segment, numRivSeg+1);
  setlength(min_col, numRivSeg+1);
  setlength(min_row, numRivSeg+1);
  for i:=0 to numRivSeg do
     begin
      min_segment[i] := maxSmallint;
     end;


   for i:= 0 to nrow-1 do
      for j:= 0 to ncol-1 do
         begin
           seg:= RivSeg[i,j];
           if (seg> 0) and (min_segment[seg] > river_routing_map[i,j]) and (river_routing_map[i,j] >0) then
             begin
             min_segment[seg] := river_routing_map[i,j];
             min_row[seg] := i;
             min_col[seg] := j;
             end;
         end;

   // follow the water in each segment
   for seg:=1 to numRivSeg do
   begin
     i:= min_row[seg];
     j:= min_col[seg];


     temp:=SedLoad_VHA_Cumulative[seg] - sedload_vha[seg] + SEDI_IN[i,j];
     cumulative[i,j]:=temp;
    if (i=0) and (j=0) then continue; // skip empty segments
     while followriver(i,j) do
     begin
       temp := temp + SEDI_IN[i,j];
       cumulative[i,j]:=temp;
     end;

   end;

end;

Function followriver(var i,j: integer): boolean;
var
 seg_start, k, l, rowmin, colmin, min, max: integer;
begin
  seg_start:= Rivseg[i,j];
  max := maxint;
  min := river_routing_map[i,j];
  rowmin:= 0;  colmin:=0;
    //A 3x3 kernel is build around every cell
  For K := -1 To 1 Do
    For L := -1 To 1 Do
      Begin
        //The cell under consideration ([i,j]) is not examined
        If ((K=0)And(L=0)) Then Continue;

        If (river_routing_map[i+k,j+l]<>-9999) and (rivseg[i+k,j+l]=seg_start) and (river_routing_map[i+k,j+l]>min) and (river_routing_map[i+k,j+l]<max)
          Then
          Begin
            ROWMIN := K;COLMIN := L;
            Max := river_routing_map[i+k,j+l];
          End;
      End;
  if ((rowmin<>0) or (colmin<>0)) then
  begin
    i:= i+ rowmin;
    j:= j+colmin;
    followriver:=True;
  end
  else
    followriver:=False;

end;


End.
