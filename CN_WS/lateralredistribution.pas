unit LateralRedistribution;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, RData_CN, GData_CN, ReadInParameters,
  Raster_calculations,math,
  CN_calculations, Idrisi;

procedure Water;
procedure Distribute_sediment;

implementation

var

  SEDI_OUT, CY_OUT, CO_OUT: RRaster;
  SEDI_IN, CY_IN, CO_IN: RRaster;
  Waterero, sedprod, depprod: double;
  SedLoad, SedLoad_VHA: RVector;
  Sediment: array of array of double;

procedure Checkerosionheight(i, j: integer; var A: RRaster);
var
  extremum, area: double;
  k, l: integer;
begin
  if Raster_projection = plane then
    area := sqr(RES)
  else
    area := X_Resolution(i, j) * Y_Resolution(i, j);
  // A = watereros value
  if A[i, j] < 0.0 then             //erosion: watereros < 0
  begin
    extremum := 99999.9;
    for k := -1 to 1 do                              //search for lowest neighbour
      for l := -1 to 1 do
      begin
        if (k = 0) and (l = 0) then
          continue;
        if DTM[i + k, j + l] < extremum then
          extremum := DTM[i + k, j + l];
      end;
    if extremum > (DTM[i, j] + A[i, j]) then
    begin
      A[i, j] := extremum - DTM[i, j];
      SEDI_OUT[i, j] := SEDI_IN[i, j] - (A[i, j] * area);
    end;
  end

  else                           //sedimentation

  begin
    if A[i, j] > 0.0 then
    begin
      extremum := 99999990.0;
      for k := -1 to 1 do
        for l := -1 to 1 do
        begin
          if ((k = 0) and (l = 0)) or (abs(k) + abs(l) < 2) then
            continue;
          if DTM[i, j] > extremum then
            extremum := DTM[i + k, j + l];
        end;
      if extremum < (DTM[i, j] + A[i, j]) then
      begin
        A[i, j] := extremum - DTM[i, j];
        SEDI_OUT[i, j] := SEDI_IN[i, j] - (A[i, j] * area);
      end;
    end;
  end;
end;//-----------------------------------------Einde procedure Checkerosionheight


procedure Calculatewaterero(i, j: integer);
//waterero in meter: for the pixel that is being considered
var                                         //rill,interrill en cap in m≥
  capacity, area, Distcorr, Ero_Potential: double;
  k, l: integer;

begin

  if (Include_dam) and (Dam_map[i, j] <> 0) then
    // adjust C factor, P factor and kTc for dams
  begin
    C_factor[i, j] := 0.01;
    P_factor[i, j] := 1;
    ktc[i,j] := 7;
  end;

  if (Include_ditch) and (Ditch_map[i, j] <> 0) then
    // adjust C factor, P factor and kTc for ditches
  begin
    C_factor[i, j] := 0;
    P_factor[i, j] := 1;
    ktc[i,j] := 9999;
  end;

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
  if Raster_projection = plane then
  begin
    area := sqr(RES);
    Distcorr := (RES * (ABS(sin(aspect[i, j])) + ABS(cos(aspect[i, j]))));
    // Correction factor for conversion to grid cell dimension
  end
  else
  begin
    area := X_resolution(i, j) * Y_resolution(i, j);
    Distcorr := (Y_Resolution(i, j) * ABS(sin(aspect[i, j])) + X_Resolution(i, j) * ABS(cos(aspect[i, j]))); // CHECK !!!
  end;

  // Erosion equations

  RUSLE[i, j] := RFactor * C_factor[i, j] * P_factor[i, j] * K_Factor[i, j] * LS[i, j];  //kg/(m² * yr) (ok)
  // Erosion in kg/m²

  // Capacity := ktc[i,j] * RUSLE[i, j] * distcorr;   // in kg (ok) KTC ZOALS IN VERSIE JEROEN
  Capacity := ktc[i,j] * RFactor * K_Factor[i, j] * (LS[i, j] - 0.6 * 6.86 * power(tan(slope[i,j]), 0.8)); // [kg/m] VOLGENS CODES BASTIAAN;

  {ShowMessage('Cfactor: ' + floattostr(C_factor[i,j]));
  ShowMessage('P_factor: ' + floattostr(P_factor[i,j]));
  ShowMessage('Kfactor:' + floattostr(K_Factor[i,j]));
  ShowMessage('LS: ' + floattostr(LS[i,j]));
   }
  // Verwijderd want niet aanwezig in WatemSedem2015
  {
  if (C_factor[i, j] = 0) then                     // correction for roads and ditches
    //Capacity := RFactor * K_Factor[i, j] * LS[i, j] * ktc[i, j] * distcorr;
    Capacity := ktc[i, j] * RFactor * K_Factor[i, j] * (LS[i, j] - 0.6 * 6.86 * power(tan(slope[i,j]), 0.8)); // [kg/m] VOLGENS CODES BASTIAAN;
  }

  //*******************************

  Ero_Potential := RUSLE[i, j] * area / BD; // in m³ (ok) CODE JEROEN

  if capacity < 0.0 then capacity :=  ktc[i,j] *  RUSLE[i,j];

  capacity := capacity*distcorr; // [kg] VOLGENS CODES BASTIAAN
  capacity := capacity / BD;                             // in m³ (ok)

  if (SEDI_IN[i, j] + Ero_Potential) > capacity then
    // If the amount of soil that enters the cell is
    // larger than the transport capacity
  begin
    SEDI_OUT[i, j] := capacity;  // SEDI_OUT [m³]
    // The amount leaving the cell = transport capacity
    Waterero := (SEDI_IN[i, j] - capacity) / area;          // in m (ok)
  end
  else
  begin
    SEDI_OUT[i, j] := SEDI_IN[i, j] + Ero_Potential; // SEDI_OUT [m³]
    Waterero := -(Ero_Potential) / area;                 // in m (ok)
  end;
  WATEREROS[i, j] := Waterero;     // in m (ok)
  // in m      if < 0 => erosion & if > 0 => sedimentation

end;

procedure Water;
var
  teller, i, j, k, l, m, n: integer;
  area, sewer_out_sed, TEMP_river_sed_input, TEMP_outside_sed_input, TEMP_buffer_sed_input, TEMP_pond_sed_input: double;
  sediment, sediment_VHA, sewer_out: textfile;

begin
  // Create temp 2D maps
  SetDynamicRData(SEDI_IN); // Raster with sediment input per gridcel?
  SetDynamicRData(SEDI_OUT);
  //************************

  SetzeroR(SEDI_IN);
  SetzeroR(SEDI_OUT);
  SetzeroR(SEDI_EXPORT);

  // SEDI_EXPORT is defined in 'Readinparameters.pas' (in allocate_memory)

  if Use_Rfactor then
    Rfactor := Rfactor / 10000;    // in MJ.mm/m².h.year

  if VHA then //If the user wants output per river segment
  begin
    numRivSeg := calcRivSeg(RivSeg);
    setlength(sedload_VHA, numRivSeg + 1);
     //The lengt of a vector per river segment (+1) is set
  end;

  if Include_sewer then // If sewers are included
    sewer_out_sed := 0;

  TEMP_river_sed_input := 0;
  TEMP_outside_sed_input := 0;
  TEMP_buffer_sed_input := 0;
  TEMP_pond_sed_input := 0;

  //** Calculate watererosion & Lateral sediment
  for teller := ncol * nrow downto 1 do
  begin // begin lus
    i := row[teller];
    j := column[teller]; // The catchment is looked at starting from the highest pixel
    if (PRC[i, j] = 0) or (PRC[i, j] = -1) or (PRC[i, j] = -5) then
      // if cell is outside area or a river cell or a water body => = all export cells
      //    This means that also cells outside the study area and ponds are included in the calculation of sediment leaving the catchment?
    begin
      if (PRC[i, j] = -1) then
        TEMP_river_sed_input := TEMP_river_sed_input + SEDI_IN[i, j];
      if (PRC[i, j] = 0) then
        TEMP_outside_sed_input := TEMP_outside_sed_input + SEDI_IN[i, j];
      if (PRC[i, j] = -5) then
        TEMP_pond_sed_input := TEMP_pond_sed_input + SEDI_IN[i, j];

      SEDI_EXPORT[i, j] := SEDI_IN[i, j];
      // assign export sediment (in m³) value for export cells
      if (VHA) and (RivSeg[i, j] <> 0) then
        sedload_VHA[RivSeg[i, j]] := sedload_VHA[RivSeg[i, j]] + SEDI_EXPORT[i, j];
      // totale hoeveelheid sediment per rivier segment wordt opgeslagen
      continue;
    end;

    if (Include_buffer) and (Buffermap[i, j] <> 0) and
      (Buffermap[i, j] <= Number_of_Buffers) then    // only center cells of buffers
    begin
      if Raster_projection = plane then
        area := sqr(RES)
      else
        area := X_resolution(i, j) * Y_resolution(i, j);
      SEDI_OUT[i, j] := SEDI_IN[i, j] * (1 - (Bufferdata[Buffermap[i, j]].PTEF / 100));
      if SEDI_OUT[i,j]<SEDI_IN[i,j] then
         TEMP_buffer_sed_input := TEMP_buffer_sed_input + (SEDI_IN[i,j]-SEDI_OUT[i, j]);
      DistributeFlux_Sediment(i, j, SEDI_IN, SEDI_OUT);
      WATEREROS[i, j] := (SEDI_IN[i, j] - SEDI_OUT[i, j]) / area;   // in m
      WATEREROS[i, j] := WATEREROS[i, j] * 1000;    // in mm
      continue;
    end;

    {row2[i,j] := i;
    col2[i,j] := j;    }

    CalculateWaterEro(i, j);
    //Checkerosionheight(i,j,WATEREROS);
    WATEREROS[i, j] := WATEREROS[i, j] * 1000;       // [mm] (ok)
    //ShowMessage('Watereros:' + floattostr(WATEREROS[i,j]));
    area := sqr(RES);
    WATEREROS_cubmeter[i,j] := WATEREROS[i, j] * Area / 1000;
    WATEREROS_kg[i,j] := WATEREROS_cubmeter[i,j] * BD;


    if (PRC[i, j] <> 0) and (PRC[i, j] <> -1) and (PRC[i, j] <> -5) then
    begin
      if SEDI_IN[i, j] - SEDI_OUT[i, j] < 0 then
        sedprod := sedprod + ((SEDI_IN[i, j] - SEDI_OUT[i, j]) * BD) //BD [kg/m³]
        // sedprod [kg]
      else
        depprod := depprod + ((SEDI_IN[i, j] - SEDI_OUT[i, j]) * BD);
        // depprod [kg]
    end;

    if SEDI_OUT[i,j] > 0 then         // if sediment leaves this pixel, the sediment needs to be distributed over target cells
      DistributeFlux_Sediment(i, j, SEDI_IN, SEDI_OUT); // SEDI_IN [m³]

    if (Include_sewer) and (SewerMap[i, j] <> 0) then
      // if pixel contains sewer, total amount of sediment leaving the system through sewer is updated
    begin
      // AND SEDI_IN is corrected because procedure Distribute_Flux doesn't take this into account
      sewer_out_sed := sewer_out_sed + (SEDI_OUT[i, j] * SewerMap[i, j] * (sewer_exit / 100));
      SEDI_IN[Routing[i, j].Target2Row, Routing[i, j].Target2Col] :=
        SEDI_IN[Routing[i, j].Target2Row, Routing[i, j].Target2Col] -
        SEDI_OUT[i, j] * Routing[i, j].Part2 + SEDI_OUT[i, j] * SewerMap[i, j] * (1 - (sewer_exit / 100));
    end;
  end;
  //***********

  // voor elke outletpixel wordt nu de totale sedimentvracht berekend die binnenkomt
  // voor rivierpixel = som van alle sediment dat terecht komt in alle hogergelegen rivierpixels
  // voor pixel op land = SEDI_IN voor die pixel

  setlength(sedload, numOutlet + 1);
  for i := 1 to numOutlet do
  begin
    k := OutletArray[i, 0]; //row of the outlet
    l := OutletArray[i, 1]; // column of the outlet
    if PRC[k, l] = -1 then // If the outlet is a river
    begin
      for m := 1 to nrow do
        for n := 1 to ncol do
        begin
          if (PRC[m, n] = -1) and (DTM[m, n] >= DTM[k, l]) then
            sedload[i] := sedload[i] + SEDI_EXPORT[m, n]; // Here SEDI_EXPORT [m³]
                                                          //      sedload [m³]
        end;
    end
    else
      sedload[i] := SEDI_IN[k, l]; // [m³]

    sedload[i] := sedload[i] * BD; // convert sedload to kg
  end;

  // voor elk riviersegment wordt sediment omgezet naar kg
  if VHA then
  begin
    for i := 1 to numRivSeg do
      sedload_VHA[i] := sedload_VHA[i] * BD;
  end;

  // sedload has to be written to a .txt file as output of the model

    setcurrentDir(File_output_dir);
    assignfile(Sediment, 'Total sediment.txt');
    rewrite(Sediment);
    Writeln(Sediment, 'Total erosion: ' + floattostr(round(sedprod*100)/100) + ' (kg)');
    Writeln(Sediment, 'Total deposition: ' + floattostr(round(depprod*100)/100) + ' (kg)');
    Writeln(Sediment, 'Sediment leaving the catchment, via the river: ' + floattostr(round((TEMP_river_sed_input * BD)*100)/100) + ' (kg)');
    Writeln(Sediment, 'Sediment leaving the catchment, not via the river: ' + floattostr(round((TEMP_outside_sed_input * BD)*100)/100) + ' (kg)');
    Writeln(Sediment, 'Sediment trapped in buffers: ' + floattostr(round((TEMP_buffer_sed_input * BD)*100)/100) + ' (kg)');
    Writeln(Sediment, 'Sediment trapped in open water: ' + floattostr(round((TEMP_pond_sed_input * BD)*100)/100) + ' (kg)');
    Writeln(Sediment,'_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _');
    Writeln(Sediment,'');
    Writeln(Sediment, 'Total sediment passing at each outlet [kg]');    // write title
    Write(Sediment, 'Outlet ID', chr(9), 'Sediment');     // write column headings
    writeln(Sediment, '');   // go to next line

    for i := 1 to numOutlet do
    begin
      Write(Sediment, IntToStr(i), chr(9), floattostr(sedload[i]));
      writeln(Sediment, '');
    end;
    closefile(Sediment); //The memory of Sediment is released

    if VHA then
    begin
      setcurrentDir(File_output_dir);
      assignfile(Sediment_VHA, 'Total sediment VHA.txt');
      rewrite(Sediment_VHA);
      Writeln(Sediment_VHA, 'Total sediment flowing into each VHA river segment [kg]');
      // write title
      Write(Sediment_VHA, 'VHA segment', chr(9), 'Sediment');     // write column headings
      writeln(Sediment_VHA, '');   // go to next line

      for i := 1 to numRivSeg do
      begin
        Write(Sediment_VHA, IntToStr(i), chr(9), floattostr(sedload_VHA[i]));
        writeln(Sediment_VHA, '');
      end;
      closefile(Sediment_VHA); //The memory of Sediment is released
    end;

  if (Include_sewer) then
    // total amount of sediment exported through sewer system is written to .txt file
  begin
    sewer_out_sed := sewer_out_sed * BD;   // convert to kg

    setcurrentDir(File_output_dir);
    assignfile(sewer_out, 'Sewer output sediment.txt');
    rewrite(sewer_out);
    Writeln(sewer_out, 'Total amount of sediment leaving the system through the sewers [kg]');
    // write title
    Writeln(sewer_out, floattostr(sewer_out_sed));
    closefile(sewer_out); //The memory of sewer_out is released
  end;

{for teller:= ncol*nrow downto 1 do
begin // begin lus
   i:=row[teller];  j:=column[teller];
 IF (PRC[i,j] <> 0) OR (PRC[i,j] <> -1) OR (PRC[i,j] <> -5) then
 begin
          if SEDI_IN[i,j] - SEDI_OUT[i,j] < 0 then
            sedprod := sedprod + SEDI_IN[i,j] - SEDI_OUT[i,j]
          else
            depprod := depprod + SEDI_IN[i,j] - SEDI_OUT[i,j];
        end;
 end;}

 for m := 1 to nrow do
 for n := 1 to ncol do
        begin
             SEDI_IN2[m,n] := SEDI_IN[m,n] * BD;
             SEDI_OUT2[m,n] := SEDI_OUT[m,n] * BD;
             //depprod2[m,n] := SEDI_IN2[m,n]-SEDI_OUT2[m,n];
             SEDI_EXPORT_kg[m,n] := SEDI_EXPORT[m,n] * BD;
        end;

  // Dispose Temp 2D maps
  DisposeDynamicRdata(SEDI_IN);
  DisposeDynamicRdata(SEDI_OUT);
  //********************
end;

//sediment dat toekomt in elke outlet lineair verdelen over hydrogram
// resultaat wegschrijven naar .txt file (zoals voor discharge)

procedure distribute_sediment;
var
  i, j, NTimesteps: integer;
  ER_clay: double;
  clay_cont, clay_cont_VHA: floatArray;
  fraction_discharge, fraction_discharge_VHA, sediment_result,
  sediment_conc, sediment_conc_VHA, sediment_result_VHA: floatArray2;
  sediment, sediment_VHA, sed_conc, sed_conc_VHA, clay_txt, clay_VHA_txt: textfile;

begin
  if convert_output then
    NTimesteps := NTimesteps2
  else
    NTimesteps := NumberofTimesteps;

  // sediment load is distributed over hydrogram and written to .txt file

  setlength(fraction_discharge, NTimesteps + 1, numOutlet + 1);
  setlength(sediment_result, NTimesteps + 1, numOutlet + 1);
  for i := 0 to NTimesteps do
    for j := 1 to numOutlet do
    begin
      if Sum_discharge[j] > 0 then
        fraction_discharge[i, j] := Result_Discharge[i, j] / Sum_discharge[j]
      else
        fraction_discharge[i, j] := 0;

      sediment_result[i, j] := fraction_discharge[i, j] * sedload[j];
    end;

  // write sediment_result to .txt

  setcurrentDir(File_output_dir);
  assignfile(Sediment, 'Sediment.txt');
  rewrite(Sediment);
  if convert_output then
  begin
    Writeln(Sediment, 'Sediment passing at each outlet [kg] with ',
      Timestep_output, ' minutes timestep');    // write title
    Write(Sediment, 'Time (min)', chr(9));
  end
  else
  begin
    Writeln(Sediment, 'Sediment passing at each outlet [kg] with ',
      Timestep_model, ' seconds timestep');    // write title
    Write(Sediment, 'Time (sec)', chr(9));
  end;
  for j := 1 to numOutlet do
    Write(Sediment, 'Outlet ', j, chr(9));     // write column headings
  writeln(Sediment, '');   // go to next line

  if convert_output then
  begin
    for i := 0 to NTimeSteps2 do
    begin
      Write(Sediment, IntToStr(TimeSeries_tmp_fin[i] div 60), chr(9));
      for j := 1 to numOutlet do
        Write(Sediment, floattostr(sediment_result[i, j]), chr(9));
      //Amount of sediment per time step is written to the .txt file
      writeln(Sediment, '');
    end;
  end
  else
  begin
    for i := 0 to NumberOfTimesteps do
    begin
      Write(Sediment, IntToStr(RainData[i].Time), chr(9));
      for j := 1 to numOutlet do
        Write(Sediment, floattostr(sediment_result[i, j]), chr(9));
      //Amount of sediment per time step is written to the .txt file
      writeln(Sediment, '');
    end;
  end;
  closefile(Sediment); //The memory of Sediment is released

  // sediment load for each VHA segment is distributed over hydrogram and written to .txt file

  if VHA then
  begin
    setlength(fraction_discharge_VHA, NTimesteps + 1, numRivSeg + 1);
    setlength(sediment_result_VHA, NTimesteps + 1, numRivSeg + 1);
    for i := 0 to NTimesteps do
      for j := 1 to numRivSeg do
      begin
        if Sum_discharge_VHA[j] = 0 then
          sediment_result_VHA[i, j] := 0
        else
        begin
          fraction_discharge_VHA[i, j] := Result_Discharge_VHA[i, j] / Sum_discharge_VHA[j];
          sediment_result_VHA[i, j] := fraction_discharge_VHA[i, j] * sedload_VHA[j];
        end;
      end;

    // write sediment_result_VHA to .txt
    assignfile(Sediment_VHA, 'Sediment_VHA.txt');
    rewrite(Sediment_VHA);
    if convert_output then
    begin
      Writeln(Sediment_VHA, 'Sediment flowing in each river segment [kg] with ',
        Timestep_output, ' minutes timestep');    // write title
      Write(Sediment_VHA, 'Time (min)', chr(9));
    end
    else
    begin
      Writeln(Sediment_VHA, 'Sediment passing in each river segment [kg] with ',
        Timestep_model, ' seconds timestep');    // write title
      Write(Sediment_VHA, 'Time (sec)', chr(9));
    end;
    for j := 1 to numRivSeg do
      Write(Sediment_VHA, 'VHA segment ', j, chr(9));     // write column headings
    writeln(Sediment_VHA, '');   // go to next line

    if convert_output then
    begin
      for i := 0 to NTimeSteps2 do
      begin
        Write(Sediment_VHA, IntToStr(TimeSeries_tmp_fin[i] div 60), chr(9));
        for j := 1 to numRivSeg do
          Write(Sediment_VHA, floattostr(sediment_result_VHA[i, j]), chr(9));
        //Amount of sediment per time step is written to the .txt file
        writeln(Sediment_VHA, '');
      end;
    end
    else
    begin
      for i := 0 to NumberOfTimesteps do
      begin
        Write(Sediment_VHA, IntToStr(RainData[i].Time), chr(9));
        for j := 1 to numRivSeg do
          Write(Sediment_VHA, floattostr(sediment_result_VHA[i, j]), chr(9));
        //Amount of sediment per time step is written to the .txt file
        writeln(Sediment_VHA, '');
      end;
    end;
    closefile(Sediment_VHA); //The memory of Sediment is released
  end;


  // sediment concentrations are calculated and written to .txt file
  setlength(sediment_conc, NTimesteps + 1, numOutlet + 1);
  for i := 0 to NTimesteps do
    for j := 1 to numOutlet do
    begin
      if Result_Discharge[i, j] = 0 then
        sediment_conc[i, j] := 0
      else
        sediment_conc[i, j] := (sediment_result[i, j] * 1000) / (Result_Discharge[i, j] * 1000);
      // in g/l
    end;

  setcurrentDir(File_output_dir);
  assignfile(Sed_conc, 'Sediment concentration.txt');
  rewrite(Sed_conc);
  Writeln(Sed_conc, 'Sediment concentration at each outlet [g/l]');    // write title
  if convert_output then
    Write(Sed_conc, 'Time (min)', chr(9))
  else
    Write(Sed_conc, 'Time (sec)', chr(9));
  for j := 1 to numOutlet do
    Write(Sed_conc, 'Outlet ', j, chr(9));     // write column headings
  writeln(Sed_conc, '');   // go to next line

  if convert_output then
  begin
    for i := 0 to NTimeSteps2 do
    begin
      Write(Sed_conc, IntToStr(TimeSeries_tmp_fin[i] div 60), chr(9));
      for j := 1 to numOutlet do
        Write(Sed_conc, floattostr(sediment_conc[i, j]), chr(9));
      writeln(Sed_conc, '');
    end;
  end
  else
  begin
    for i := 0 to NumberOfTimesteps do
    begin
      Write(Sed_conc, IntToStr(RainData[i].Time), chr(9));
      for j := 1 to numOutlet do
        Write(Sed_conc, floattostr(sediment_conc[i, j]), chr(9));
      writeln(Sed_conc, '');
    end;
  end;
  closefile(Sed_conc);


  if VHA then
  begin
    setlength(sediment_conc_VHA, NTimesteps + 1, numRivSeg + 1);
    for i := 0 to NTimesteps do
      for j := 1 to numRivSeg do
      begin
        if Result_Discharge_VHA[i, j] = 0 then
          sediment_conc_VHA[i, j] := 0
        else
          sediment_conc_VHA[i, j] :=
            (sediment_result_VHA[i, j] * 1000) / (Result_Discharge_VHA[i, j] * 1000);    // in g/l
      end;

    setcurrentDir(File_output_dir);
    assignfile(Sed_conc_VHA, 'Sediment concentration_VHA.txt');
    rewrite(Sed_conc_VHA);
    Writeln(Sed_conc_VHA, 'Sediment concentration for each VHA river segment [g/l]');
    // write title
    if convert_output then
      Write(Sed_conc_VHA, 'Time (min)', chr(9))
    else
      Write(Sed_conc_VHA, 'Time (sec)', chr(9));
    for j := 1 to numRivSeg do
      Write(Sed_conc_VHA, 'VHA segment ', j, chr(9));     // write column headings
    writeln(Sed_conc_VHA, '');   // go to next line

    if convert_output then
    begin
      for i := 0 to NTimeSteps2 do
      begin
        Write(Sed_conc_VHA, IntToStr(TimeSeries_tmp_fin[i] div 60), chr(9));
        for j := 1 to numRivSeg do
          Write(Sed_conc_VHA, floattostr(sediment_conc_VHA[i, j]), chr(9));
        //Amount of sediment per time step is written to the .txt file
        writeln(Sed_conc_VHA, '');
      end;
    end
    else
    begin
      for i := 0 to NumberOfTimesteps do
      begin
        Write(Sed_conc_VHA, IntToStr(RainData[i].Time), chr(9));
        for j := 1 to numRivSeg do
          Write(Sed_conc_VHA, floattostr(sediment_conc_VHA[i, j]), chr(9));
        //Amount of sediment per time step is written to the .txt file
        writeln(Sed_conc_VHA, '');
      end;
    end;
    closefile(Sed_conc_VHA); //The memory of Sediment is released
  end;

  if est_clay then
    // estimate the clay content of the sediment based on enrichment ratio
  begin
    setlength(clay_cont, numOutlet + 1);
    for i := 1 to numOutlet do
    begin
      ER_clay := (0.7732 * exp(-0.0508 * sediment_conc[1, i])) + 1;
      // formula Wang et al 2010
      clay_cont[i] := ER_clay * clay_parent;
    end;

    // write result to .txt file
    setcurrentDir(File_output_dir);
    assignfile(clay_txt, 'Clay content sediment.txt');
    rewrite(clay_txt);
    Writeln(clay_txt, 'Clay content of sediment at each outlet [%]');    // write title
    Write(clay_txt, 'Outlet ID', chr(9), 'Clay content (%)');     // write column headings
    writeln(clay_txt, '');   // go to next line
    for i := 1 to numOutlet do
    begin
      Write(clay_txt, IntToStr(i), chr(9), floattostr(clay_cont[i]));
      writeln(clay_txt, '');
    end;
    closefile(clay_txt);

    if VHA then     // estimate clay content of sediment for each VHA river segment
    begin
      setlength(clay_cont_VHA, numRivSeg + 1);
      for i := 1 to numRivSeg do
      begin
        ER_clay := (0.7732 * exp(-0.0508 * sediment_conc_VHA[1, i])) + 1;
        // formula Wang et al 2010
        clay_cont_VHA[i] := ER_clay * clay_parent;
      end;

      // write result to .txt file
      setcurrentDir(File_output_dir);
      assignfile(clay_VHA_txt, 'Clay content sediment VHA.txt');
      rewrite(clay_VHA_txt);
      Writeln(clay_VHA_txt,
        'Clay content of sediment flowing in each river segment [%]');
      // write title
      Write(clay_VHA_txt, 'River segment', chr(9), 'Clay content (%)');
      // write column headings
      writeln(clay_VHA_txt, '');   // go to next line
      for i := 1 to numRivSeg do
      begin
        Write(clay_VHA_txt, IntToStr(i), chr(9), floattostr(clay_cont_VHA[i]));
        writeln(clay_VHA_txt, '');
      end;
      closefile(clay_VHA_txt);
    end;
  end;

end;



end.
