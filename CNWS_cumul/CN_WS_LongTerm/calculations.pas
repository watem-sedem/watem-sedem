
Unit Calculations;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, Math, ReadInParameters, RData_CN, GData_CN, Dialogs;

Procedure DetectEvents;
Function sumPartArray (inputArray: FloatArray; start, number: integer): double;
Function sumPartIntArray(inputArray: integerArray; start, number : integer): integer;
Procedure DemarcateSeasons;
Function leapYear (year:integer): boolean;
Procedure identifyInput(eventID: integer);
Function calc_year(eventID:integer): integer;
Function calculate_AR5 (eventID: integer): double;
Function calc_month(eventID:integer): integer;
Function calc_season(eventID: integer): string;
Procedure Calc_numOutlet;
Procedure Calc_numVHA;
Procedure updateOutput(eventID:integer);
Procedure updateMap(eventMap: RRaster; Var totMap: RRaster);
Procedure updateText_sec(eventText: FloatArray2; Var totText : FloatArray2; eventID, numColumns,
                         timestep : integer);
Procedure updateText_min(eventText: FloatArray2; Var totText : FloatArray2; eventID, numColumns,
                         timestep : integer);

Implementation



// this procedure detects all events in the rainfall file and saves them in a multi dimensional array
// for each event, Tini, Tfin and NumberOfElements is saved in a record

Procedure DetectEvents;

Var 
  index, Tini, Tfin, NumElements, i : integer;
  rainVolume : FloatArray;
  rainInt : integerArray;
Begin

  numberOfEvents := 0;
  numElements := 0;

  // zoek geldig rain event en save in "Events" array
  index := 0;
  While index <= (length(TimeSeries_fin)-1) Do
    Begin
      If RainfallSeries_fin[index] <> 0 Then     // van zodra er regen valt...
        Begin
          Tini := index;
          While (sumPartArray(RainfallSeries_fin,index+1, (21600 Div Timestep_model)-1)<>0) And (
                index < length(TimeSeries_fin)-1) Do
            inc(index);
          // einde van event wordt bereikt van zodra in volgende 6u geen regen valt of van zodra
          // we aan einde van rainfall file komen...

          Tfin := index;
          rainVolume := Copy(RainfallSeries_fin, Tini, Tfin-Tini+1);
          // test of totaal rainvolume > 1.27 mm (indien niet, is dit geen erosive event en
          // wordt dit event niet meegenomen in modelberekeningen)
          If sum(rainVolume) < 1.27 Then
            Begin
              index := index+1;
              continue;
            End
          Else
            Begin
              numberOfEvents := numberOfEvents+1;
              If NumElements < length(rainVolume) Then
                NumElements := length(rainVolume);
              SetLength(Events, NumElements, numberOfEvents+1);
              For i := 0 To length(rainVolume)-1 Do
                Events[i,numberOfEvents] := rainVolume[i];
              SetLength(Event_meta,numberOfEvents+1);
              Event_meta[numberOfEvents].Tini := Tini;
              Event_meta[numberOfEvents].Tfin := Tfin;
              setLength(rainInt, length(rainVolume));
              Event_meta[numberOfEvents].NumberOfElements := length(rainInt);

     //showmessage('number of events: ' + inttostr(numberOfEvents) + '/ index: ' + inttostr(index));
            End;
        End;
      index := index+1;
    End;
End;

Function sumPartArray (inputArray: FloatArray; start, number: integer): double;

Var 
  i, endArray, endSum: integer;
Begin
  endArray := length(inputArray)-1;

  If start+number <= endArray Then
    endSum := start+number
  Else
    endSum := endArray;

  sumPartArray := inputArray[start];
  For i := start+1 To endSum Do
    sumPartArray := sumPartArray+inputArray[i];
End;

Function sumPartIntArray(inputArray: integerArray; start, number : integer): integer;

Var 
  i, endArray, endSum: integer;
Begin
  endArray := length(inputArray)-1;

  If start+number <= endArray Then
    endSum := start+number
  Else
    endSum := endArray;

  sumPartIntArray := inputArray[start];
  For i := start+1 To endSum Do
    sumPartIntArray := sumPartIntArray+inputArray[i];
End;

Procedure DemarcateSeasons;
Begin
  setlength(numDays,13);
  numDays[1] := 31;
  If leapYear(year) = True Then
    numDays[2] := 29
  Else
    numDays[2] := 28;
  numDays[3] := 31;
  numDays[4] := 30;
  numDays[5] := 31;
  numDays[6] := 30;
  numDays[7] := 31;
  numDays[8] := 31;
  numDays[9] := 30;
  numDays[10] := 31;
  numDays[11] := 30;
  numDays[12] := 31;

  Case First_month Of 
    1:
       Begin
         start_winter := sumPartIntArray(numDays,1,10);
         start_summer := sumPartIntArray(numDays,1,4);
         start_spring := sumPartIntArray(numDays,1,1);
         start_fall := sumPartIntArray(numDays,1,7);
         first_season := 'winter';
       End;
    2:
       Begin
         start_winter := sumPartIntArray(numDays,2,9);
         start_summer := sumPartIntArray(numDays,2,3);
         start_spring := numDays[2];
         start_fall := sumPartIntArray(numDays,2,6);
         first_season := 'winter';
       End;
    3:
       Begin
         start_winter := sumPartIntArray(numDays,3,8);
         start_summer := sumPartIntArray(numDays,3,2);
         start_spring := 999999;
         start_fall := sumPartIntArray(numDays,3,5);
         first_season := 'spring';
       End;
    4:
       Begin
         start_winter := sumPartIntArray(numDays,4,7);
         start_summer := sumPartIntArray(numDays,4,1);
         start_spring := sumPartIntArray(numDays,4,8) + sumPartIntArray(numDays,1,1);
         start_fall := sumPartIntArray(numDays,4,4);
         first_season := 'spring';
       End;
    5:
       Begin
         start_winter := sumPartIntArray(numDays,5,6);
         start_summer := numDays[5];
         start_spring := sumPartIntArray(numDays,5,7) + sumPartIntArray(numDays,1,1);
         start_fall := sumPartIntArray(numDays,5,3);
         first_season := 'spring';
       End;
    6:
       Begin
         start_winter := sumPartIntArray(numDays,6,5);
         start_summer := 999999;
         start_spring := sumPartIntArray(numDays,6,6) + sumPartIntArray(numDays,1,1);
         start_fall := sumPartIntArray(numDays,6,2);
         first_season := 'summer';
       End;
    7:
       Begin
         start_winter := sumPartIntArray(numDays,7,4);
         start_summer := sumPartIntArray(numDays,7,5) + sumPartIntArray(numDays,1,4);
         start_spring := sumPartIntArray(numDays,7,5) + sumPartIntArray(numDays,1,1);
         start_fall := sumPartIntArray(numDays,7,1);
         first_season := 'summer';
       End;
    8:
       Begin
         start_winter := sumPartIntArray(numDays,8,3);
         start_summer := sumPartIntArray(numDays,8,4) + sumPartIntArray(numDays,1,4);
         start_spring := sumPartIntArray(numDays,8,4) + sumPartIntArray(numDays,1,1);
         start_fall := numDays[8];
         first_season := 'summer';
       End;
    9:
       Begin
         start_winter := sumPartIntArray(numDays,9,2);
         start_summer := sumPartIntArray(numDays,9,3) + sumPartIntArray(numDays,1,4);
         start_spring := sumPartIntArray(numDays,9,3) + sumPartIntArray(numDays,1,1);
         start_fall := 999999;
         first_season := 'fall';
       End;
    10:
        Begin
          start_winter := sumPartIntArray(numDays,10,1);
          start_summer := sumPartIntArray(numDays,10,2) + sumPartIntArray(numDays,1,4);
          start_spring := sumPartIntArray(numDays,10,2) + sumPartIntArray(numDays,1,1);
          start_fall := sumPartIntArray(numDays,10,2) + sumPartIntArray(numDays,1,7);
          first_season := 'fall';
        End;
    11:
        Begin
          start_winter := numDays[11];
          start_summer := sumPartIntArray(numDays,11,1) + sumPartIntArray(numDays,1,4);
          start_spring := sumPartIntArray(numDays,11,1) + sumPartIntArray(numDays,1,1);
          start_fall := sumPartIntArray(numDays,11,1) + sumPartIntArray(numDays,1,7);
          first_season := 'fall';
        End;
    12:
        Begin
          start_winter := 999999;
          start_summer := numDays[12] + sumPartIntArray(numDays,1,4);
          start_spring := numDays[12] + sumPartIntArray(numDays,1,1);
          start_fall := numDays[12] + sumPartIntArray(numDays,1,7);
          first_season := 'winter';
        End;
  End;

{
// grenzen omzetten naar seconden
 start_winter := start_winter * 24 * 3600;
 start_summer := start_summer * 24 * 3600;
 start_spring := start_spring * 24 * 3600;
 start_fall := start_fall * 24 * 3600;
}
End;

Function leapYear (year:integer): boolean;
// check whether we are in a leap year...

Var 
  x: double;
Begin
  leapYear := False;
  If (First_month = 1) Or (First_month = 2) Then
    Begin
      x := year/100;
      x := frac(x)*100;
      If (year Mod 4 = 0) And (x <> 0) Then
        leapYear := True;
    End
  Else
    Begin
      x := (year+1)/100;
      x := frac(x)*100;
      If ((year+1) Mod 4 = 0) And (x <> 0) Then
        leapYear := True;
    End;
End;

Procedure identifyInput(eventID: integer);
Begin

  Use_Rfactor := False;

  If Not simplified Then
    Begin
      AR5_event := calculate_AR5(eventID);
      // see function below



      RivVel_event := RivVel[calc_month(eventID)];
      // month of event is determined after which the correct value is selected
      // (see function below) from the RivVel array

      Case calc_season(eventID) Of 
        // season of event is determined (see function below)
        'spring':
                  Begin
                    // and correct CN map is selected
                    If calc_year(eventID) = 1 Then
                      CN_event := CNmapSpring
                    Else
                      CN_event := CNmapSpring_2;
                  End;
        'summer':
                  Begin
                    If calc_year(eventID) = 1 Then
                      CN_event := CNmapSummer
                    Else
                      CN_event := CNmapSummer_2;
                  End;
        'fall':
                Begin
                  If calc_year(eventID) = 1 Then
                    CN_event := CNmapFall
                  Else
                    CN_event := CNmapFall_2;
                End;
        'winter':
                  Begin
                    If calc_year(eventID) = 1 Then
                      CN_event := CNmapWinter
                    Else
                      CN_event := CNmapWinter_2;
                  End;
      End;
    End;

  Case calc_season(eventID) Of 
    // season of event is determined (see function below)
    'spring':
              Begin
                // and correct Cfactor is selected
                If calc_year(eventID) = 1 Then
                  Cf_Data_event := Cf_Data_spring
                Else
                  Cf_Data_event := Cf_Data_spring_2;
              End;
    'summer':
              Begin
                If calc_year(eventID) = 1 Then
                  Cf_Data_event := Cf_Data_summer
                Else
                  Cf_Data_event := Cf_Data_summer_2;
              End;
    'fall':
            Begin
              If calc_year(eventID) = 1 Then
                Cf_Data_event := Cf_Data_fall
              Else
                Cf_Data_event := Cf_Data_fall_2;
            End;
    'winter':
              Begin
                If calc_year(eventID) = 1 Then
                  Cf_Data_event := Cf_Data_winter
                Else
                  Cf_Data_event := Cf_Data_winter_2;
              End;
  End;


// select appropriate parcel map, kTc and ktil => determine whether the event takes place in the first or second year
  // of the simulation (see function below)
  If calc_year(eventID) = 1 Then
    Begin
      PARCEL_filename := PARCEL_filename1;
      If Not Create_ktc Then
        ktc_filename := ktc_Data_Filename;
      If Not Create_ktil Then
        ktil_filename := ktil_Data_Filename;
    End
  Else
    Begin
      PARCEL_filename := PARCEL_filename2;
      If Not Create_ktc Then
        ktc_filename := ktc_Data_Filename2;
      If Not Create_ktil Then
        ktil_filename := ktil_Data_Filename2;
    End;

  If eventID > 1 Then                // the following output maps need to be written only once
    Begin
      Write_ASPECT := false;
      Write_LS := false;
      Write_SLOPE := false;
      Write_UPAREA := false;
      Write_TILEROS := false;
    End;
End;


Function  calc_year(eventID:integer): integer;

Var 
  month_event: integer;
Begin
  month_event := calc_month(eventID);
  If First_month = 1 Then
    calc_year := 1
  Else
    Begin
      If month_event < First_month Then
        calc_year := 2
      Else
        calc_year := 1;
    End;
End;

Function calculate_AR5 (eventID: integer): double;

Var 
  numberElements: integer;
Begin
  numberElements := 5*24*3600 Div Timestep_model;
  If 5*24*3600 > (Event_meta[eventID].Tini *Timestep_model) Then
    // if data for 5 days prior to event not available...
    numberElements := Event_meta[eventID].Tini;
  calculate_AR5 := sumPartArray(RainfallSeries_fin, Event_meta[eventID].Tini-numberElements,
                   numberElements-1);
End;

Function calc_month(eventID:integer): integer;

Var 
  days,dayTeller: double;
  month_index: integer;
  check: boolean;
Begin

  check := false;
  month_index := first_month;
  dayTeller := numDays[first_month];
  days := Timeseries_fin[Event_meta[eventID].Tini]/(24*3600);
  //= start of event expressed in days after start of simulation

  Repeat
    If days > dayTeller Then
      Begin
        month_index := month_index+1;
        dayTeller := dayTeller + numDays[month_index];
      End
    Else
      check := true;
  Until (check) Or (month_index = 12);

  If (month_index = 12) And (days > dayTeller) Then
    Begin
      month_index := 1;
      dayTeller := dayTeller + numDays[month_index];
      Repeat
        If days > dayTeller Then
          Begin
            month_index := month_index+1;
            dayTeller := dayTeller + numDays[month_index];
          End
        Else
          check := true;
      Until (check);
    End;
  calc_month := month_index;
End;

Function calc_season(eventID:integer): string;

Var 
  days: double;
Begin
  days := Timeseries_fin[Event_meta[eventID].Tini]/(24*3600);
  //= start of event expressed in days after start of simulation

  Case first_season Of 
    'spring':
              Begin
                If days > start_summer Then
                  Begin
                    If days > start_fall Then
                      Begin
                        If days > start_winter Then
                          Begin
                            If days > start_spring Then
                              calc_season := 'spring'
                            Else
                              calc_season := 'winter';
                          End
                        Else
                          calc_season := 'fall';
                      End
                    Else
                      calc_season := 'summer';
                  End
                Else
                  calc_season := 'spring';
              End;
    'summer':
              Begin
                If days > start_fall Then
                  Begin
                    If days > start_winter Then
                      Begin
                        If days > start_spring Then
                          Begin
                            If days > start_summer Then
                              calc_season := 'summer'
                            Else
                              calc_season := 'spring';
                          End
                        Else
                          calc_season := 'winter';
                      End
                    Else
                      calc_season := 'fall';
                  End
                Else
                  calc_season := 'summer';
              End;
    'fall':
            Begin
              If days > start_winter Then
                Begin
                  If days > start_spring Then
                    Begin
                      If days > start_summer Then
                        Begin
                          If days > start_fall Then
                            calc_season := 'fall'
                          Else
                            calc_season := 'summer';
                        End
                      Else
                        calc_season := 'spring';
                    End
                  Else
                    calc_season := 'winter';
                End
              Else
                calc_season := 'fall';
            End;
    'winter':
              Begin
                If days > start_spring Then
                  Begin
                    If days > start_summer Then
                      Begin
                        If days > start_fall Then
                          Begin
                            If days > start_winter Then
                              calc_season := 'winter'
                            Else
                              calc_season := 'fall';
                          End
                        Else
                          calc_season := 'summer';
                      End
                    Else
                      calc_season := 'spring';
                  End
                Else
                  calc_season := 'winter';
              End
  End;
End;

Procedure Calc_numOutlet;

Var 
  i,j: integer;
Begin
  numOutlet := 1;
  If outlet_Select Then
    Begin
      GetGfile(outletMap, datadir+OutletFilename);
      For i := 1 To nrow Do
        For j := 1 To ncol Do
          Begin
            If outletMap[i,j] <> 0 Then
              Begin
                If outletMap[i,j] > numOutlet Then
                  numoutlet := outletMap[i,j];
              End;
          End;
      DisposeDynamicGdata(outletMap);
    End;
End;

Procedure Calc_numVHA;

Var 
  i,j: integer;
Begin
  numVHA := 1;
  GetGfile(RivSegMap, datadir+riversegment_filename);
  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If RivSegMap[i,j] <> 0 Then
          Begin
            If RivSegMap[i,j] > numVHA Then
              numVHA := RivSegMap[i,j];
          End;
      End;
  DisposeDynamicGdata(RivSegMap);
End;

Procedure updateOutput(eventID: integer);

Var 
  i,j : integer;

Begin
  updateMap(SediExport_event, SediExport_tot);
  // update output maps
  updateMap(SediIn_event, SediIn_tot);
  updateMap(SediOut_event, SediOut_tot);
  updateMap(Watereros_event, Watereros_tot);
  updateMap(Watereros_kg_event, Watereros_kg_tot);
  updateMap(RUSLE_event, RUSLE_tot);

  If Not simplified Then
    Begin
      updateMap(TotRun_event, TotRun_tot);
      updateMap(ReMap_event, ReMap_tot);

      // update TotDischarge
      For i := 0 To numOutlet-1 Do
        TotDischarge_tot[i,1] := TotDischarge_tot[i,1] + TotDischarge_event[i,1];

      If Include_buffer Then     // update spillover
        Begin
          For i := 0 To Number_of_Buffers-1 Do
            Spillover_tot[i,1] := Spillover_tot[i,1] + Spillover_event[i,1];
        End;

      If convert_output Then      //output is given in minutes => use procedure updateText_min
        Begin
          // update discharge
          updateText_min(Discharge_event, Discharge_tot, eventID, numOutlet+1, timestep_output);
          // update sediment concentration
          updateText_min(Sedconc_event, Sedconc_tot, eventID, numOutlet+1, timestep_output);
          // update sediment
          updateText_min(Sediment_event, Sediment_tot, eventID, numOutlet+1, Timestep_output);
          If VHA Then
            Begin
              // update Discharge_VHA
              updateText_min(Discharge_VHA_event, Discharge_VHA_tot, eventID, numVHA+1,
                             Timestep_output);
              // update Sediment concentration VHA
              updateText_min(Sedconc_VHA_event, Sedconc_VHA_tot, eventID, numVHA+1, Timestep_output)
              ;
              // update Sediment_VHA
              updateText_min(Sediment_VHA_event, Sediment_VHA_tot, eventID, numVHA+1,
                             Timestep_output);
            End;

        End
      Else    // output is given in seconds => use procedure updateText_sec
        Begin
          updateText_sec(Discharge_event, Discharge_tot, eventID, numOutlet+1, Timestep_model);
          updateText_sec(Sedconc_event, Sedconc_tot, eventID, numOutlet+1, Timestep_model);
          updateText_sec(Sediment_event, Sediment_tot, eventID, numOutlet+1, Timestep_model);
          If VHA Then
            Begin
              updateText_sec(Discharge_VHA_event, Discharge_VHA_tot, eventID, numVHA+1,
                             Timestep_model);
              updateText_sec(Sedconc_VHA_event, Sedconc_VHA_tot, eventID, numVHA+1, Timestep_model);
              updateText_sec(Sediment_VHA_event, Sediment_VHA_tot, eventID, numVHA+1, Timestep_model
              );
            End;
        End;

      If include_sewer Then      // update sewer_out_water
        sewer_out_water_tot := sewer_out_water_tot + sewer_out_water_event;
    End;

  // update TotSediment
  TotalErosion_tot := TotalErosion_tot + TotalErosion_event;
  TotalDeposition_tot := TotalDeposition_tot + TotalDeposition_event;
  SedleavingRiv_tot := SedleavingRiv_tot + SedleavingRiv_event;
  SedLeaving_tot := SedLeaving_tot + SedLeaving_event;
  SedTrapBuffer_tot := SedTrapBuffer_tot + SedTrapBuffer_event;
  SedTrapWater_tot := SedTrapWater_tot + SedTrapWater_event;

  For i := 0 To numOutlet-1 Do
    Begin
      TotSediment_tot[i,1] := TotSediment_tot[i,1] + TotSediment_event[i,1];
      TotSediment_tot[i,0] := TotSediment_event[i,0];
    End;

  If VHA Then
    Begin
      // update TotSedimentVHA
      For i := 0 To numVHA-1 Do
        Begin
          TotSedimentVHA_tot[i,1] := TotSedimentVHA_tot[i,1] + TotSedimentVHA_event[i,1];
          TotSedimentVHA_tot[i,0] := TotSedimentVHA_event[i,0];
        End;
    End;

  If include_sewer Then             // update sewer_out_sediment
    sewer_out_sediment_tot := sewer_out_sediment_tot + sewer_out_sediment_event;
End;


Procedure updateMap(eventMap: RRaster; Var totMap: RRaster);

Var 
  i,j: integer;
Begin
  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        totMap[i,j] := totMap[i,j]+eventMap[i,j];
      End;
End;

Procedure updateText_sec(eventText: FloatArray2; Var totText : FloatArray2; eventID, numColumns,
                         timestep: integer);

Var 
  totLength, i, j, k, start_pos, newLength, fill, lastTime : integer;
Begin
  totLength := length(totText);
  lastTime := trunc(totText[totLength-1,0]);
  If Timeseries_fin[event_meta[eventID].Tini] <= lastTime Then
    // indien overlap tussen oude en nieuwe file
    Begin
      For i := 0 To totLength-1 Do
        Begin
          // startpositie bepalen
          If Timeseries_fin[event_meta[eventID].Tini] = totText[i,0] Then
            start_pos := i;
        End;
      newLength := totLength + ((length(eventText)-1) - (totLength - start_pos));
      // nieuwe lengte bepalen
      setLength(totText, newLength , numColumns);
    End
  Else      // if no overlap between two arrays
    Begin
      fill := ((Timeseries_fin[event_meta[eventID].Tini] - lastTime) Div timestep)-1;
      newLength := totLength + fill + (length(eventText)-1);
      start_pos := (totLength-1) + (fill+1);
      setLength(totText, newLength, numColumns);
      For j:= 1 To numColumns-1 Do
        // fill space between two series with zeros
        For i:= totLength To start_pos-1 Do
          totText[i,j] := 0;
      For i := totLength To start_pos-1 Do
        // complete timeseries (column 0) between two series
        totText[i,0] := totText[i-1,0]+timestep;
    End;

  // update array
  // complete timeseries (column 0)
  For i := start_pos To newLength-1 Do
    totText[i,0] := totText[i-1,0]+timestep;
  // update other columns
  For j := 1 To numColumns-1 Do
    Begin
      k := 1;
      For i := start_pos To newLength-1 Do
        Begin
          totText[i,j] := totText[i,j]+eventText[k,j];
          k := k+1;
        End;
    End;

  Dimension_result := newLength;
  // needed in procedure WriteOutput...
End;


// the goal of this procedure is similar to the procedure above, but this one is used in case the output is given in minutes instead of seconds

Procedure updateText_min(eventText: FloatArray2; Var totText : FloatArray2; eventID, numColumns,
                         timestep: integer);

Var 
  totLength, i, j, k, start_pos, newLength, fill, lastTime : integer;
  check: boolean;
  x: double;
Begin
  totLength := length(totText);
  lastTime := trunc(totText[totLength-1,0]) * 60;
  // in seconds
  If Timeseries_fin[event_meta[eventID].Tini] <= lastTime Then
    // indien overlap tussen oude en nieuwe file
    Begin
      check := false;
      For i := 0 To totLength-1 Do
        Begin
          // startpositie bepalen
          If (Timeseries_fin[event_meta[eventID].Tini] = (trunc(totText[i,0])*60)) Then
            Begin
              start_pos := i;
              check := true;
            End;
        End;
      If Not check Then
        Begin
          For i := 0 To totLength-1 Do
            Begin
              If (Timeseries_fin[event_meta[eventID].Tini] > (trunc(totText[i,0])*60)) And (
                 Timeseries_fin[event_meta[eventID].Tini] < (trunc(totText[i+1,0])*60)) Then
                start_pos := i+1;
            End;
        End;

      newLength := totLength + ((length(eventText)-1) - (totLength - start_pos));
      // nieuwe lengte bepalen
      setLength(totText, newLength , numColumns);
    End
  Else      // if no overlap between two arrays
    Begin
      If  ((Timeseries_fin[event_meta[eventID].Tini] - lastTime) Mod (timestep*60) = 0) Then
        fill := ((Timeseries_fin[event_meta[eventID].Tini] - lastTime) Div (timestep*60))-1
      Else
        Begin
          x := (Timeseries_fin[event_meta[eventID].Tini] - lastTime) / (timestep*60);
          fill := trunc(x);
        End;
      newLength := totLength + fill + (length(eventText)-1);
      start_pos := (totLength-1) + (fill+1);
      setLength(totText, newLength, numColumns);
      For j:= 1 To numColumns-1 Do
        // fill space between two series with zeros
        For i:= totLength To start_pos-1 Do
          totText[i,j] := 0;
      For i := totLength To start_pos-1 Do
        // complete timeseries (column 0) between two series
        totText[i,0] := totText[i-1,0]+timestep;
    End;

  // update array
  // complete timeseries (column 0)
  For i := start_pos To newLength-1 Do
    totText[i,0] := totText[i-1,0]+timestep;
  // update other columns
  For j := 1 To numColumns-1 Do
    Begin
      k := 1;
      For i := start_pos To newLength-1 Do
        Begin
          totText[i,j] := totText[i,j]+eventText[k,j];
          k := k+1;
        End;
    End;

  Dimension_result := newLength;
  // needed in procedure WriteOutput...
End;


End.
