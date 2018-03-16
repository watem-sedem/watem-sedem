
Unit CN_calculations;

// In this unit al the calculations concerning the curve number are codedd

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, GData_CN, RData_CN, idrisi, Math, ReadInParameters;

Type 
  IntegerArray = array Of integer;
  FloatArray = array Of Double;
  FloatArray2 = array Of array Of Double;

Function CalculateRe_singlevalue(Rainfall, CN, alpha, beta, I10, AR5, duration :double): double;
Procedure CalculateRe(Var Remap:Rraster; Perceelskaart:Rraster ; CNmap:Rraster; alpha,beta:double);
{Procedure CalculateRunoffAcc(var UpArea: RRaster; Remap: RRaster; PRC: GRaster);}
Procedure ReadRainfallFile (Var Raindata: TRainRecordArray; Rainfallfilename: String);
Procedure ReadText(filename:String; Var Z: FloatArray2; rowRain, colRain:integer);
Function interp(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray): FloatArray;
Function extrap(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray): FloatArray;
Procedure CalculateTimeDependentRunoff(Remap: Rraster; RainData: TRainRecordArray; Routing:
                                       TRoutingArray; PRC: Rraster);
Procedure CalculateRFactor;
Procedure calcOutlet;
Function  calcRivSeg(RivSeg:GRaster): integer;
Function is_outlet(i,j:integer): boolean;
Function sumPartArray (inputArray: FloatArray; start, number:integer): double;

Var 
  NumberOfTimesteps, NTimesteps2, StartTime_rain, EndTime_rain, EndTime, EndTime_modelSec,
  numOutlet, numRivSeg: integer;
  Rainfall, Duration, I10: double;
  TimeSeries, Timeseries_tmp_fin : integerArray;
  RainfallSeries, Sum_discharge, Sum_discharge_VHA: FloatArray;
  Result_Discharge, Result_Discharge_VHA : FloatArray2;


Implementation

//******************************************************************************
// In this procedure the rainfall file is read (and if necessary, converted to
// a different timestep). This is a .txt file (tab delimited) with the timestep
// (min) in column 1 and the amount of rainfall (mm) in column 2. In addition,
// several input variables for CN formula are calculated (Rainfall, I10 and duration).
//******************************************************************************
Procedure ReadRainfallFile (Var Raindata: TRainRecordArray; Rainfallfilename: String);

Var 
  TimeSeries_output, I10TimeSeries: IntegerArray;
  Cumul_Rain, Cumul_Interp, RainfallSeries_output,I10RainfallSeries, I10Series: FloatArray;
  teller, i, j, nr, Timestep_rain, Fill_num: integer;
  x: double;
  datafile: textfile;
  a: string;
  Z: FloatArray2;

Begin
  teller := 0;
  assignfile(datafile, RainfallFilename);
  reset(datafile);
  While Not eof(datafile) Do
    Begin
      //To determine the number of time steps and number of rows in the file
      inc(teller);
      Readln(datafile, a);
    End;
  Setlength(TimeSeries, teller);
  //Time in seconds
  Setlength(Rainfallseries, teller);
  //Regenval in mm (per timestep)
  NumberOfTimesteps := teller-1;

  ReadText(RainfallFilename,Z,teller,2);
  // see procedure below

  For 
      i:=0 To NumberOfTimesteps Do
    Begin
      TimeSeries[i] := Trunc(Z[i,0]);
      //Array containing the time steps
      RainfallSeries[i] := Z[i,1];
      //Array with the amount of rainfall (mm) per time step
    End;

  For i := 0 To NumberOfTimesteps Do
    // convert time min => sec
    Begin
      TimeSeries[i] := TimeSeries[i]*60;
    End;

  Timestep_rain := Timeseries[1]-TimeSeries[0];

  If (Simplified) Then   // if timestep rainfall is used as model timestep...
    Begin
      Timestep_model := Timestep_rain;
      TimeSeries_output := TimeSeries;
      RainfallSeries_output := RainfallSeries;
    End

  Else    // interpolation of rainfall data to desired timestep
    Begin
      Setlength(Cumul_rain, NumberOfTimesteps+1);
      // calculate cumulative rainfall series
      Cumul_rain[0] := RainfallSeries[0];
      For i := 1 To NumberOfTimesteps Do
        Begin
          Cumul_rain[i] := Cumul_rain[i-1] + RainfallSeries[i];
        End;

      // create new timeseries array
      NumberOfTimesteps := NumberOfTimesteps*(Timestep_rain Div Timestep_model);
      Setlength(TimeSeries_output, NumberOfTimesteps+1);
      TimeSeries_output[0] := TimeSeries[0];
      For i := 1 To NumberOfTimesteps Do
        Begin
          TimeSeries_output[i] := TimeSeries_output[i-1] + Timestep_model;
        End;

      // interpolation of cumulative rainfall series, see function below
      Cumul_Interp := interp(TimeSeries, TimeSeries_output, Cumul_rain);

      // cumulative rainfall series is converted to new rainfall series
      Setlength(RainfallSeries_output, NumberOfTimesteps+1);
      RainfallSeries_output[0] := Cumul_Interp[0];
      For i := 1 To NumberOfTimesteps Do
        Begin
          RainfallSeries_output[i] := Cumul_Interp[i]-Cumul_Interp[i-1];
        End;

      EndTime := TimeSeries_output[NumberOfTimesteps];
      // extra zeros are added to the rainfall series if necessary
      EndTime_modelSec := EndTime_model * 60;
      If EndTime_modelSec > EndTime Then
        Begin
          Fill_num := (EndTime_modelSec-EndTime) Div Timestep_model;
          If (EndTime_modelSec-Endtime) Mod Timestep_model <> 0 Then
            Begin
              Fill_num := Fill_num+1;
            End;
          NumberOfTimesteps := NumberOfTimesteps+Fill_num;
          setlength(TimeSeries_output, NumberOfTimesteps+1);
          setlength(RainfallSeries_output, NumberOfTimesteps+1);

          For i:= NumberOfTimesteps-Fill_num+1 To length(TimeSeries_output)-1 Do
            Begin
              TimeSeries_output[i] := TimeSeries_output[i-1]+Timestep_model;
              RainfallSeries_output[i] := 0;
            End;
        End;

    End;

  Rainfall := Sum(RainfallSeries_output);
  // calculate total amount of rainfall (mm)

  If Not Simplified Then
    Begin
      For i:=0 To NumberOfTimesteps Do
        // calculate start time of rain
        Begin
          If RainfallSeries_output[i] <> 0 Then
            Begin
              StartTime_rain := TimeSeries_output[i-1];
              Break;
            End;
        End;

      For i:= NumberOfTimesteps Downto 0 Do
        // calculate end time of rain
        Begin
          If RainfallSeries_output[i] <> 0 Then
            Begin
              EndTime_rain := TimeSeries_output[i];
              Break;
            End;
        End;

      Duration := (EndTime_rain - StartTime_rain)/60;
      // calculate duration of rain in min

      // calculation of I10: rainfall series is converted to a one-minute series, after which
      // I10 is calculated

      If Timestep_rain = 60 Then
        Begin
          I10TimeSeries := TimeSeries;
          I10RainfallSeries := RainfallSeries;
        End
      Else If Timestep_rain > 60 Then    // interpolation of the original series is needed
             Begin
               nr := (length(TimeSeries)-1)*(TimeStep_rain Div 60);
               setlength(I10TimeSeries, nr+1);
               setlength(I10RainfallSeries, nr+1);
               I10TimeSeries[0] := TimeSeries[0];
               For i := 1 To nr Do
                 I10TimeSeries[i] := I10TimeSeries[i-1]+60;
               I10RainfallSeries[0] := RainfallSeries[0];
               j := 1;
               For i := 1 To (length(TimeSeries)-1) Do
                 Begin
                   x := RainfallSeries[i]/(Timestep_rain Div 60);
                   While j <= i*(Timestep_rain Div 60) Do
                     Begin
                       I10RainfallSeries[j] := x;
                       Inc(j);
                     End;
                 End;
             End
      Else           // extrapolation of the original series is needed
        Begin
          nr := (length(TimeSeries)-1)Div(60 Div TimeStep_rain);
          setlength(I10TimeSeries, nr+1);
          I10TimeSeries[0] := TimeSeries[0];
          For i := 1 To nr Do
            Begin
              I10TimeSeries[i] := I10TimeSeries[i-1]+60;
            End;
          I10RainfallSeries := extrap(TimeSeries, I10TimeSeries, RainfallSeries);
        End;

      // for each minute, the intensity for the coming 10 minutes is calculated and
      // saved in array I10Series. Then the maximum value in this array is calculated.

      setlength(I10Series, length(I10TimeSeries)-1);
      For i := 1 To length(I10TimeSeries)-1 Do
        I10Series[i-1] := sumPartArray(I10RainfallSeries,i,9)/(1/6);

      I10 := I10Series[0];
      For i := 1 To length(I10Series)-1 Do
        Begin
          If I10Series[i] > I10 Then
            I10 := I10Series[i];
        End;
    End;

  //Time and Rainfall are written to a Record
  Setlength(Raindata,NumberOfTimesteps+1);
  For i := 0 To NumberOfTimesteps Do
    Begin
      RainData[i].ID := i+1;
      RainData[i].Time := TimeSeries_output[i];
      RainData[i].Rain := RainfallSeries_output[i];
      RainData[i].Rain_fraction := RainData[i].Rain / Rainfall;
    End;
End;
//*****************************************************************************
// This procedure reads a tab delimited text file and converts it to a 2D array
// (used in procedure ReadRainfallFile)
//*****************************************************************************

Procedure ReadText(filename:String; Var Z: FloatArray2; rowRain, colRain:integer);

Var 
  inputfile: textfile;
  i,j: integer;

Begin
  setlength(Z,rowRain,colRain);
  assignfile(inputfile, filename);
  reset (inputfile);
  For i:=0 To rowRain-1 Do
    For j:=0 To colRain-1 Do
      Begin
        Read(inputfile,Z[i,j]);
      End;
  closefile(inputfile);

End;

//*****************************************************************************
// This function is used for the interpolation of the rainfall data (see above)
//*****************************************************************************

Function interp(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray): FloatArray;

Var 
  i,j,k,l,t1,t2,step,x0,x1 : integer;
  y0,y1: double;

Begin
  setlength(interp, length(xNew));
  interp[0] := yOriginal[0];
  t1 := xOriginal[1];
  t2 := xNew[1];
  step := t1 Div t2;
  For i := 1 To length(xOriginal)-1 Do
    Begin
      j := (step*i);
      interp[j] := yOriginal[i];
    End;

  k := 1;

  repeat
    x0 := xNew[k-1];
    y0 := interp[k-1];
    x1 := xNew[k+step-1];
    y1 := interp[k+step-1];

    For l := 0 To step-2 Do
       Begin
         interp[k+l] := y0+((y1-y0)*((xNew[k+l]-x0)/(x1-x0)));
       End;

    k := k+(step);
  until k>=length(interp);

End;

//*****************************************************************************
// This function is used for the extrapolation of the rainfall data (see above)
//*****************************************************************************

Function extrap(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray): FloatArray;

Var 
  t1,t2,step,i,j: integer;

Begin
  setlength(extrap, length(xNew));
  extrap[0] := yOriginal[0];
  t1 := xOriginal[1];
  t2 := xNew[1];
  step := t2 Div t1;
  j := 1;
  For i := 1 To length(xNew)-1 Do
    Begin
      // memory outside the array is accessed hereif j+step-1 > length(yOriginal)-1
      // TODO: johan - we should check what the goal of this calculation is + correct
      extrap[i] := sum(yOriginal[j..j+step-1]);
      j := j+step;
    End;

End;

//******************************************************************************
// In this procedure the number of outlets is determined based on the outlet map
// provided by the user. In case no outlet map is provided, the location of the
// outlet is determined based on the DTM and the parcel map. The locations of the
// outlet(s) are written to an array.
//******************************************************************************
Procedure calcOutlet;

Var 
  i,j,k,l: integer;
  height, max_uparea: double;
  outlet_rst: GRaster;
Begin
  numOutlet := 1;
  k := 1;
  If outlet_Select Then
    Begin
      For i := 1 To nrow Do
        For j := 1 To ncol Do
          Begin
            If Outlet[i,j] <> 0 Then
              Begin
                setlength(OutletArray, k+1, 2);
                OutletArray[k,0] := i;
                OutletArray[k,1] := j;
                k := k+1;
                If Outlet[i,j] > numOutlet Then
                  numoutlet := Outlet[i,j];
              End;
          End;
      if ( k = 1 ) Then // if k is still 1 after looping through outlet map, no outlets were found
        raise EInputException.Create('Error in data input: no outlets present in outlet map');
      // lowest outlet is calculated
      lowOutletX := OutletArray[1,0];
      lowOutletY := OutletArray[1,1];
      If numOutlet > 1 Then
        Begin
          For l := 2 To numOutlet Do
            Begin
              If DTM[OutletArray[l,0],OutletArray[l,1]] < DTM[lowOutletX, lowOutletY] Then
                Begin
                  lowOutletX := OutletArray[l,0];
                  lowOutletY := OutletArray[l,1];
                End;
            End;
        End;
    End
  Else
    Begin
      // calculation of x and y coordinates of outlet... (= lowest river pixel)
      setlength(OutletArray, k+1, 2);
      height := 9999;
      For i := 1 To nrow Do
        For j := 1 To ncol Do
          Begin
            If (PRC[i,j] = -1) And (DTM[i,j]<height) Then
              Begin
                OutletArray[k,0] := i;
                OutletArray[k,1] := j;
                height := DTM[i,j];
              End;
          End;
      If height = 9999 Then
        // if no riverpixels present => select pixel with largest UPAREA as outlet
        Begin
          max_uparea := 0;
          For i := 1 To nrow Do
            For j := 1 To ncol Do
              Begin
                If UPAREA[i,j] > max_uparea Then
                  Begin
                    OutletArray[k,0] := i;
                    OutletArray[k,1] := j;
                    max_uparea := UPAREA[i,j];
                  End;
              End;
        End;
      lowOutletX := OutletArray[k,0];
      // lowest outlet = only outlet
      lowOutletY := OutletArray[k,1];

      // write location of outlet to Idrisi raster (Outlet.rst)
      SetDynamicGData(outlet_rst);
      SetzeroG(outlet_rst);
      outlet_rst[lowOutletX, lowOutletY] := 1;
      writeGIdrisi32file(ncol,nrow, Datadir+'Outlet'+'.rst',outlet_rst);
      DisposeDynamicGdata(outlet_rst);
    End;
End;

Function calcRivSeg(RivSeg:GRaster): integer;

Var i,j: integer;
Begin
  result := 1;
  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If (RivSeg[i,j] <> 0) And (result < RivSeg[i,j]) Then
          result := RivSeg[i,j];
      End;
End;

//TODO johan this function is called a lot -> better make this a hashmap
// or probably better use the Outlet raster
Function is_outlet(i,j:integer): boolean;
Var
  m : integer;
Begin
  result := false;

  For m := 1 To numOutlet Do
    Begin
      If (i = OutletArray[m,0]) And (j = OutletArray[m,1]) Then
        begin
           result := true;
           break;
        end;

    End;
End;

//******************************************************************************
//In this procedure the R factor of the RUSLE equation is calculated based on
//the rainfall input.
//******************************************************************************
Procedure CalculateRFactor;

Var 
  Timestep, i, j, nr, index, Tini, Tfin : integer;
  eventEnergy, maxI30, eventRFactor, x: double;
  newTimeSeries, minTimeSeries : integerArray;
  newRainfallSeries, minRainfallSeries, rainVolume, rainEnergy, I30: FloatArray;
Begin
  Timestep := Timeseries[1]-TimeSeries[0];
  // bereken timestep

  If Timestep = 600 Then    //(=10 min)
    Begin
      newTimeSeries := TimeSeries;
      newRainfallSeries := RainfallSeries;
    End
  Else
    Begin
      If Timestep > 60 Then     // eerst omzetten naar tijdstap van 1 min (makkelijker)
        Begin
          nr := (length(TimeSeries)-1)*(TimeStep Div 60);
          setlength(minTimeSeries, nr+1);
          setlength(minRainfallSeries, nr+1);
          minTimeSeries[0] := TimeSeries[0];
          For i := 1 To nr Do
            minTimeSeries[i] := minTimeSeries[i-1]+60;
          minRainfallSeries[0] := RainfallSeries[0];
          j := 1;
          For i := 1 To (length(TimeSeries)-1) Do
            Begin
              x := RainfallSeries[i]/(Timestep Div 60);
              While j <= i*(Timestep Div 60) Do
                Begin
                  minRainfallSeries[j] := x;
                  Inc(j);
                End;
            End;
        End
      Else If Timestep < 60 Then
             Begin
               nr := (length(TimeSeries)-1)Div(60 Div TimeStep);
               setlength(minTimeSeries, nr+1);
               minTimeSeries[0] := TimeSeries[0];
               For i := 1 To nr Do
                 Begin
                   minTimeSeries[i] := minTimeSeries[i-1]+60;
                 End;
               minRainfallSeries := extrap(TimeSeries, minTimeSeries, RainfallSeries);
             End
      Else
        Begin
          minTimeSeries := TimeSeries;
          minRainfallSeries := RainfallSeries;
        End;
      // vervolgens conversie naar tijdstap van 10 min
      nr := ((length(minTimeSeries)-1) Div 10) + 1;
      setlength(newTimeSeries, nr + 1);
      setlength(newRainfallSeries, nr);
      newTimeSeries[0] := TimeSeries[0];
      For i := 1 To nr Do
        Begin
          newTimeSeries[i] := newTimeSeries[i-1]+600;
        End;
      newRainfallSeries := extrap(minTimeSeries, newTimeSeries, minRainfallSeries);
    End;

  // zoek geldig event en baken het af (tini...tfin)
  index := 0;
  While index <= (length(newTimeSeries)-1) Do
    Begin
      If newRainfallSeries[index] <> 0 Then     // van zodra er regen valt...
        Begin
          Tini := index;
          While (sumPartArray(newRainfallSeries,index+1,35)<>0) And (index < length(newTimeSeries)-1
                ) Do
            inc(index);
          // einde van event wordt bereikt van zodra in volgende 6u geen regen valt of van zodra
          // we aan einde van rainfall file komen...

          Tfin := index;
          rainVolume := Copy(newRainfallSeries, Tini, Tfin-Tini+1);
          // test of totaal rainvolume > 1.27 mm (indien niet, is dit geen erosive event en
          // wordt dit event niet meegenomen bij berekening R factor)
          If sum(rainVolume) < 1.27 Then
            Begin
              index := index+1;
              continue;
            End;
          SetLength(rainEnergy, Tfin-Tini+1);
          eventEnergy := 0;
          For i := 0 To Tfin-Tini Do
            // bereken rain energy voor elke timestep binnen event
            Begin
              rainEnergy[i] := 11.12*power((rainVolume[i]/(1/6)),0.31);
              // in J/m².mm
              eventEnergy := eventEnergy+(rainEnergy[i]*rainVolume[i]);
              // in J/m²
            End;
          // bepaal max I30 van event
          If Tfin-Tini-1 < 1 Then
            maxI30 := sum(rainVolume) * 2
          Else
            Begin
              SetLength(I30, Tfin-Tini-1);
              For i := 0 To Tfin-Tini-2 Do
                I30[i] := (rainVolume[i]+rainVolume[i+1]+rainVolume[i+2])*2;
              // in mm/h

              maxI30 := I30[0];
              For i:=1 To Tfin-Tini-2 Do
                Begin
                  If I30[i] > maxI30 Then
                    maxI30 := I30[i];
                End;
            End;
          // bereken R factor van event
          eventRFactor := (eventEnergy*maxI30)/1000000;
          // in MJ.mm/m².h
          // bereken totale R factor
          RFactor := RFactor + eventRFactor;
          // in MJ.mm/m².h
        End;
      // verhoog index met 1
      index := index +1;
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


//******************************************************************************
// In this procedure the time dependent runoff is calculated
//******************************************************************************
Procedure CalculateTimeDependentRunoff(Remap: Rraster; RainData: TRainRecordArray; Routing:
                                       TRoutingArray; PRC: Rraster);

Var 
  RunoffMap, RunoffInputMap, RunoffInputMap_temp, RoutedMap_temp, RainfallMap,
  RunoffCummulMap, OutflowMap_temp: Rraster;
  i, j,  k , l, m, teller, targetX, targetY: integer;
  RunoffInput ,Part1_water, Part2_water, Speed, spill, sewer_out_water: double;
  Discharge, Discharge_tot, Discharge_VHA_txt, spillover_txt, sewer_out_txt: textfile;
  spillover, Discharge_result_tmp, Discharge_tmp_fin: FloatArray;
  Result, Discharge_VHA: FloatArray2;
  Timeseries_tmp: integerArray;

  Label SKIP;

Const 
  speed_sewer = 1;
  // assume 1 m/s

Begin
  //Runoff map is created (=Remap in which negative values are replaced by 0, and
  //runoff in (m³)
  //It is checked if the amount of runoff produced in a certain grid cell is smaller
  //or equal to the amount of rainfall
  //Startmap is created (= Remap with positive values replaced by zeros when runoff
  //takes places (this is added for every time step) and negative values where
  //re-infiltration takes place)

  SetDynamicRData(RunoffInputmap);
  //= map with total amount of runoff per
  SetDynamicRData(RunoffInputmap_Temp);
  //= map with runoff per pixel for every timestep
  SetdynamicRData(OutflowMap_temp);
  //= map with the removal of water (via routing) from every grid cell for every time step
  SetLength(Result,Numberoftimesteps+1,numOutlet+1);
  If Include_buffer Then
    Begin
      SetLength(spillover,number_of_buffers+1);
      //= data array containing the amount of spillover per buffer
      For i := 1 To number_of_buffers Do
        spillover[i] := 0;
    End;

  If Include_sewer Then
    sewer_out_water := 0;

  If VHA Then
    Begin
      numRivSeg := calcRivSeg(RivSeg);
      Setlength(Discharge_VHA, Numberoftimesteps+1, numRivSeg+1);
    End;
  SetDynamicRData(RainfallMap);
  SetDynamicRData(Runoffmap);
  //=map in which the amount of runoff per timestep is stored

  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If PRC[i,j] = 0 Then continue;
        // all cells outside catchment are skipped
        If Remap[i,j] < 0.0 Then
          Begin
            RunoffInputmap[i,j] := 0.0;
            Runoffmap[i,j] := (Remap[i,j]/1000)*sqr(res);
            // amount of water that still can infiltrate in m^3
          End
        Else
          Begin
            RunoffInputmap[i,j] := (Remap[i,j]/1000)*sqr(res);
            //runoff in m^3
            Runoffmap[i,j] := 0.0;
          End;
        RainfallMap[i,j] := (Rainfall/1000)*sqr(res);
        If RunoffInputmap[i,j] > RainfallMap[i,j] Then
          //Total runoff for a pixel can never be larger than total rainfall for that pixel
          Begin
            RunoffInputmap[i,j] := RainfallMap[i,j];
          End;
      End;

  //If buffers are included the maximum discharge and dead volume for every buffer are calculated
  If (Include_buffer) Then
    For i := 1 To Number_of_Buffers Do
      Begin
        BufferData[i].Qmax := BufferData[i].Cd * BufferData[i].Opening_area * sqrt(2*9.91*(
                              BufferData[i].height_dam - BufferData[i].Height_opening));
        BufferData[i].Volume_dead := (BufferData[i].Height_opening / BufferData[i].Height_dam) *
                                     BufferData[i].Volume;
        BufferData[i].area := BufferData[i].Volume/BufferData[i].Height_dam;
      End;

  //For every timestep the correct amount of runoff is added to RunoffMap and
  //is routed through the landscape
  SetDynamicRData(RoutedMap_temp);
  //Amount of transferred water during a particular time step
  SetDynamicRData(RunoffCummulMap);

//Total amount of water that reaches every grid cell (both as a result of direct rainfall input and as a result of routed runoff from upland gidcells)
  SetzeroR(RunoffCummulMap);

  For i := 0 To NumberOfTimeSteps Do
    //Every timestep is looked at
    Begin
      SetzeroR(RoutedMap_temp);
      //At he beginning of every timestep this map is set to zero
      SetzeroR(OutflowMap_temp);

      //De input per cel wordt bepaald voor de tijdstap
      For k := 1 To nrow Do
        For l := 1 To ncol Do
          //The input for every cell is determined
          Begin
            If PRC[k,l] = 0 Then continue;
            // all cells outside catchment are skipped
            RunoffInput := RunoffInputmap[k,l] * RainData[i].Rain_fraction;
            //The runoff for that timestep is determined by scaling the total runoff
            //for that grid cell with the fraction of rainfall that falls during this time step
            RunoffInputmap_Temp[k,l] := RunoffInput;
            RunoffMap[k,l] := RunoffMap[k,l] + RunoffInput;
            //The amount of runoff for this time step is added to the amount of runoff
            //that was already present at the beginning of the time step

            If is_outlet(k,l) Then
              Begin
                If outlet_Select Then
                  Result[i,Outlet[k,l]] := Result[i,Outlet[k,l]]+RunoffInput
                Else
                  Result[i,1] := Result[i,1]+RunoffInput;
              End;
            If (VHA) And (RivSeg[k,l] <> 0) Then
              Discharge_VHA[i,RivSeg[k,l]] := Discharge_VHA[i,RivSeg[k,l]] + RunoffInput;
          End;

      //The runoff is routed for this time step
      For teller:=nrow*ncol Downto 1 Do
        Begin
          k := row[teller];
          //row and col are vectors containing the row and column ID's from low to high
          l := column[teller];
          If PRC[k,l] = 0 Then continue;
          If RunoffMap[k,l] <= 0.0 Then continue;
          If PRC[k,l] = -1 Then
            speed := riv_vel
          Else
            Speed := 0.3;
          //Water velocity in m/s (based on Govers 1992)

          //If buffers are present:
          If (Include_buffer) And (Buffermap[k,l] <> 0) And (Buffermap[k,l] <= Number_of_Buffers)
             And (RunoffMap[k,l]>0) Then
     // discharge from buffer is only altered at buffer center (place where the opening is situated)
            Begin
              If RunoffMap[k,l] > BufferData[Buffermap[k,l]].Volume Then
//The maximum capacity of the buffer is reached: all the runoff that enters the buffer flows over it
                Begin
                  spill := BufferData[BufferMap[k,l]].Cd*
                           BufferData[BufferMap[k,l]].width_dam*
                           sqrt(9.81)*
                           Power(((RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume)/BufferData[BufferMap[k,l]].area),(3/2))* Timestep_model;

      // calculate maximum amount of water flowing over dam (formula from project Pieter Meert p.68)
                  If spill > RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume Then
             // water flowing over dam cannot exceed amount of water that does not fit in the buffer
                    spill := RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume;
                  Part1_Water := (BufferData[Buffermap[k,l]].Qmax * Timestep_model) + spill;

//Part1_water is composed of (1) the volume of water inside the buffer that flows through the opening and (2) the additional water (>volume of the buffer) that flows over the dam
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] := 
                                                                                     RoutedMap_temp[
                                                                                     Routing[k,l].
                                                                                     Target1row,
                                                                                     Routing[k,l].
                                                                                     Target1col] +
                                                                                     Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                  //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                  spillover[Buffermap[k,l]] := spillover[BufferMap[k,l]]+spill;
                End

              Else
                If RunoffMap[k,l] > BufferData[Buffermap[k,l]].Volume_dead Then
           //The volume of water in the buffer is larger than the dead volume: part of it will drain
                  Begin
                    Part1_water := (BufferData[Buffermap[k,l]].Qmax * sqrt(RunoffMap[k,l]/(
                                   BufferData[Buffermap[k,l]].Volume - BufferData[Buffermap[k,l]].
                                   Volume_dead))) * Timestep_model;
                    // Amount of discharge = [Qmax * sqrt(vol(t)/vol(max))] * timestep
                    RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] := 

                                                                                      RoutedMap_temp
                                                                                       [Routing[k,l]
                                                                                       .Target1row,
                                                                                       Routing[k,l].
                                                                                       Target1col] +
                                                                                       Part1_water;
                    RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                    //RunoffMap is updated
                    OutflowMap_temp[k,l] := Part1_water;
                  End

              Else
          //The volume of water in the buffer is smaller than the dead volume: nothing will flow out
                Begin
                  Part1_water := 0;
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] := 
                                                                                     RoutedMap_temp[
                                                                                     Routing[k,l].
                                                                                     Target1row,
                                                                                     Routing[k,l].
                                                                                     Target1col] +
                                                                                     Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                  //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                End;

              targetX := Routing[k,l].Target1row;
              targetY := Routing[k,l].Target1col;
              If is_outlet(targetX,targetY) Then
                // if target cell is outlet, the result is updated
                Begin
                  If outlet_Select Then
                    Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                         OutflowMap_temp[k,l]
                  Else
                    Result[i,1] := Result[i,1]+OutflowMap_temp[k,l];
                End;
              If VHA Then
                Begin
                  If RivSeg[targetX,targetY]<>0 Then
                    // if buffer drains directly into river, the result is updated
                    Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                targetY]]+OutflowMap_temp[k,l];
                End;
              Goto SKIP;
              //Proceed to next cell
            End;

          If Routing[k,l].Part1 > 0 Then
            Begin
              If Routing[k,l].Part2 > 0 Then //If the water is routed towards 2 lower cells
                Begin
                  If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                    // if cell contains entrance to sewer...
                    Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed_sewer*TimeStep_model)
                                   /Routing[k,l].Distance2)
                                   //Amount of water that is transfered through sewers
                  Else
                    Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed*TimeStep_model)/
                                   Routing[k,l].Distance2);
                  //Amount of water that is transfered to neighbor 2

                  Part1_water := (RunoffMap[k,l]*Routing[k,l].Part1)*((Speed*TimeStep_model)/Routing
                                 [k,l].Distance1);
                  //Amount of water that is transfered to neighbor 1
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] := 
                                                                                     RoutedMap_temp[
                                                                                     Routing[k,l].
                                                                                     Target1row,
                                                                                     Routing[k,l].
                                                                                     Target1col] +
                                                                                     Part1_water;
                  If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                    Begin
                      RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] := 

                                                                                      RoutedMap_temp
                                                                                         [Routing[k,
                                                                                         l].
                                                                                         Target2row,
                                                                                         Routing[k,l
                                                                                         ].
                                                                                         Target2col]
                                                                                         + (
                                                                                         Part2_water
                                                                                         * (1-(
                                                                                         sewer_exit/
                                                                                         100)));
                      sewer_out_water := sewer_out_water + (Part2_water * (sewer_exit/100));
                    End
                  Else
                    RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] := 

                                                                                      RoutedMap_temp
                                                                                       [Routing[k,l]
                                                                                       .Target2row,
                                                                                       Routing[k,l].
                                                                                       Target2col] +
                                                                                       Part2_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - (Part1_water + Part2_water);

//The total amount of runoff that leaves the grid cell during this time step is subtracted from the RunoffMap
                  OutflowMap_temp[k,l] := Part1_water + Part2_water;
                  //Amount of water leaving the grid cell during this time step
                  If RunoffMap[k,l] < 0 Then RunoffMap[k,l] := 0;
                  //No more water then is present can leave a cell


                  targetX := Routing[k,l].Target1row;
                  targetY := Routing[k,l].Target1col;
                  If is_outlet(targetX,targetY) Then
                    // if target cell 1 is outlet or river, the result is updated
                    Begin
                      If outlet_Select Then
                        Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                             Part1_water
                      Else
                        Result[i,1] := Result[i,1]+Part1_water;
                    End;
                  If VHA Then
                    Begin
                      If RivSeg[targetX,targetY]<>0 Then
                        Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                    targetY]]+Part1_water;
                    End;

                  targetX := Routing[k,l].Target2row;
                  targetY := Routing[k,l].Target2col;
                  If is_outlet(targetX,targetY) Then
                    // if target cell 2 is outlet or river, the result is updated
                    Begin
                      If outlet_Select Then
                        Begin
                          If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                            Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+(
                                                                 Part2_water*(1-(sewer_exit/100)))
                          Else
                            Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                                 Part2_water;
                        End
                      Else
                        Begin
                          If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                            Result[i,1] := Result[i,1]+(Part2_water*(1-(sewer_exit/100)))
                          Else
                            Result[i,1] := Result[i,1]+Part2_water;
                        End;
                    End;
                  If VHA Then
                    Begin
                      If RivSeg[targetX,targetY]<>0 Then
                        Begin
                          If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                            Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[
                                                                        targetX,targetY]]+(
                                                                        Part2_water*(1-(sewer_exit/
                                                                        100)))
                          Else
                            Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[
                                                                        targetX,targetY]]+
                                                                        Part2_water;
                        End;
                    End;
                End
              Else
                Begin
                  //If the runoff is transfered to only 1 neighbor (part 1 = 1)
                  Part1_water := (RunoffMap[k,l]*Routing[k,l].Part1)*((Speed*TimeStep_model)/Routing
                                 [k,l].Distance1);
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] := 
                     RoutedMap_temp[Routing[k,l].Target1row, Routing[k,l].Target1col] +  Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                  //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                  If RunoffMap[k,l] < 0 Then RunoffMap[k,l] := 0;

                  targetX := Routing[k,l].Target1row;
                  targetY := Routing[k,l].Target1col;
                  If is_outlet(targetX,targetY) Then
                    // if target cell 1 is outlet or river, the result is updated
                    Begin
                      If outlet_Select Then
                        Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                             Part1_water
                      Else
                        Result[i,1] := Result[i,1]+Part1_water;
                    End;
                  If VHA Then
                    Begin
                      If (RivSeg[targetX,targetY]<>0) And (RivSeg[k,l] = 0) Then
        // only if target cell is river AND source cell is no river, discharge_VHA should be updated
                        Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                    targetY]]+Part1_water;
                    End;
                End;

            End
          Else
            If Routing[k,l].Part2 > 0 Then
              Begin
                //If the runoff is transfered to only 1 neighbor (part 2 = 1)
                Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed*TimeStep_model)/Routing[k
                               ,l].Distance2);
                RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] := 
                                                                                   RoutedMap_temp[
                                                                                   Routing[k,l].
                                                                                   Target2row,
                                                                                   Routing[k,l].
                                                                                   Target2col] +
                                                                                   Part2_water;
                RunoffMap[k,l] := RunoffMap[k,l] - Part2_water;
                OutflowMap_temp[k,l] := Part2_water;
                If RunoffMap[k,l] < 0 Then RunoffMap[k,l] := 0;

                targetX := Routing[k,l].Target2row;
                targetY := Routing[k,l].Target2col;
                // if target cell 1 is outlet or river, the result is updated
                If is_outlet(targetX,targetY) Then
                  Begin
                    If outlet_Select Then
                      Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                           Part2_water
                    Else
                      Result[i,1] := Result[i,1]+Part2_water;
                  End;
                If VHA Then
                  Begin
                    If RivSeg[targetX,targetY]<>0 Then
                      Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                  targetY]]+Part2_water;
                  End;

              End;
          SKIP:
        End;

      //At the end of every time step the amount of displaced water is added to the runoff map

// + the amount of displaced water is stored for every grid cell to calculate a cummulative runoff map for the entire event
      For k := 1 To nrow Do
        For l := 1 To ncol Do
          // runoff cumul map = amount of water entering each cell
          Begin
            If PRC[k,l] = 0 Then continue;
            RunoffMap[k,l] := RunoffMap[k,l] + RoutedMap_temp[k,l];
            //RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + RoutedMap_temp[k,l];
            If RunoffMap[k,l] >= 0 Then

//RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + ((RunoffMap[k,l]*Speed*Timestep)/res) //Geeft totale hoeveelheid water (rekening houdend met snelheid en tijdstap)
              RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + RoutedMap_temp[k,l] +
                                      RunoffInputmap_Temp[k,l];
            //else RunoffCummulMap[k,l] := 0;
          End;
    End;


  // Runoff results are converted to a different timestep if requested by the user

  If convert_output Then
    Begin
      setlength(TimeSeries_tmp,Numberoftimesteps+1);
      For i := 0 To Numberoftimesteps Do
        TimeSeries_tmp[i] := Raindata[i].Time;
      setlength(discharge_result_tmp, Numberoftimesteps+1);

      NTimesteps2 := Numberoftimesteps Div ((Timestep_output*60) Div Timestep_model);
      setlength(Result_Discharge, Ntimesteps2+1,numOutlet+1);
      setlength(TimeSeries_tmp_fin, NTimesteps2+1);
      TimeSeries_tmp_fin[0] := TimeSeries_tmp[0];
      For i:=1 To length(TimeSeries_tmp_fin)-1 Do
        TimeSeries_tmp_fin[i] := TimeSeries_tmp_fin[i-1]+(Timestep_output*60);
      setlength(Discharge_tmp_fin, NTimesteps2+1);

      For j := 1 To numOutlet Do
        Begin
          For i:=0 To Numberoftimesteps Do
            Discharge_result_tmp[i] := Result[i,j];
          Discharge_tmp_fin := extrap(TimeSeries_tmp,TimeSeries_tmp_fin,Discharge_result_tmp);
          For i:=0 To NTimesteps2 Do
            Result_Discharge[i,j] := Discharge_tmp_fin[i];
        End;

      If VHA Then
        Begin
          setlength(Result_Discharge_VHA, Ntimesteps2+1, numRivSeg+1);
          For j := 1 To numRivSeg Do
            Begin
              For i:=0 To Numberoftimesteps Do
                Discharge_result_tmp[i] := Discharge_VHA[i,j];
              Discharge_tmp_fin := extrap(TimeSeries_tmp,TimeSeries_tmp_fin,Discharge_result_tmp);
              For i :=0 To NTimesteps2 Do
                Result_Discharge_VHA[i,j] := Discharge_tmp_fin[i];
            End;
        End;

    End
  Else
    Begin
      setlength(Result_Discharge, numberoftimesteps+1, numOutlet+1);
      Result_Discharge := Result;

      If VHA Then
        Begin
          setlength(Result_Discharge_VHA, numberoftimesteps+1, numRivSeg+1);
          Result_Discharge_VHA := Discharge_VHA;
        End;
    End;


  //The amount of runoff that leaves the catchment during every time step is written to a .txt file
  setcurrentDir(File_output_dir);
  assignfile(Discharge,'Discharge.txt');
  rewrite(Discharge);
  Writeln(Discharge, 'Discharge at each outlet [m³/s]');
  // write title

  If convert_output Then
    Write(Discharge, 'Time (min)',chr(9))
  Else
    Write(Discharge, 'Time (sec)',chr(9));

  For m := 1 To numOutlet Do
    write(Discharge, 'Outlet ', m, chr(9));
  // write column headings
  writeln(Discharge,'');
  // go to next line

  setlength(Sum_discharge,numOutlet+1);
  For m := 1 To numOutlet Do
    Sum_discharge[m] := 0;

  If convert_output Then
    Begin
      For i := 0 To NTimesteps2 Do
        Begin
          Write(Discharge, inttostr(Timeseries_tmp_fin[i] Div 60), chr(9));
          For m := 1 To numOutlet Do
            write (Discharge, floattostr(Result_Discharge[i,m]/(Timestep_output*60)), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(Discharge, '');
          For m := 1 To numOutlet Do
            Sum_discharge[m] := Sum_discharge[m] + Result_Discharge[i,m];

//Total amount of water leaving the catchment (this is dependent on the water velocity and length of the time step!)
        End;
    End
  Else
    Begin
      For i := 0 To Numberoftimesteps Do
        Begin
          Write(Discharge, inttostr(RainData[i].Time), chr(9));
          For m := 1 To numOutlet Do
            write (Discharge, floattostr(Result_Discharge[i,m]/Timestep_model), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(Discharge, '');
          For m := 1 To numOutlet Do
            Sum_discharge[m] := Sum_discharge[m] + Result_Discharge[i,m];

//Total amount of water leaving the catchment (this is dependent on the water velocity and length of the time step!)
        End;
    End;
  closefile(Discharge);
  //The memory of Discharge is released


  // The amount of water entering each VHA river segment is written to a .txt file

  If VHA Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(Discharge_VHA_txt,'Discharge_VHA.txt');
      rewrite(Discharge_VHA_txt);
      Writeln(Discharge_VHA_txt, 'Discharge to each river segment [m³/s]');
      // write title
      If convert_output Then
        Write(Discharge_VHA_txt, 'Time (min)',chr(9))
      Else
        Write(Discharge_VHA_txt, 'Time (sec)',chr(9));
      For m := 1 To numRivSeg Do
        write(Discharge_VHA_txt, 'VHA segment ', m, chr(9));
      // write column headings
      writeln(Discharge_VHA_txt,'');
      // go to next line

      setlength(Sum_discharge_VHA,numRivSeg+1);
      For m := 1 To numRivSeg Do
        Sum_discharge_VHA[m] := 0;

      If convert_output Then
        Begin
          For i := 0 To NTimesteps2 Do
            Begin
              Write(Discharge_VHA_txt, inttostr(Timeseries_tmp_fin[i] Div 60), chr(9));
              For m := 1 To numRivSeg Do
                write (Discharge_VHA_txt, floattostr(Result_Discharge_VHA[i,m]/(Timestep_output*60))
                , chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(Discharge_VHA_txt, '');
              For m := 1 To numRivSeg Do
                Sum_discharge_VHA[m] := Sum_discharge_VHA[m] + Result_Discharge_VHA[i,m];
              // total amount of water entering each river segment
            End;
        End
      Else
        Begin
          For i := 0 To Numberoftimesteps Do
            Begin
              Write(Discharge_VHA_txt, inttostr(RainData[i].Time), chr(9));
              For m := 1 To numRivSeg Do
                write (Discharge_VHA_txt, floattostr(Result_Discharge_VHA[i,m]/Timestep_model), chr(
                                                                                                   9
                ));
              //Amount of discharge per time step is written to the .txt file
              writeln(Discharge_VHA_txt, '');
              For m := 1 To numRivSeg Do
                Sum_discharge_VHA[m] := Sum_discharge_VHA[m] + Result_Discharge_VHA[i,m];
              // total amount of water entering each river segment
            End;
        End;
      closefile(Discharge_VHA_txt);
    End;



  //A map with the total amount of runoff for every grid cell for the entire event is created

//(= amount of runoff from precipitation + amount of routed runoff that arrives in a cell - amount of runoff that leaves the cel via routing)
  SetDynamicRData(RunoffTotMap);
  For k := 1 To nrow Do
    For l := 1 To ncol Do
      Begin
        If PRC[k,l] = 0 Then RunoffTotMap[k,l] := 0.0
        Else
          RunoffTotMap[k,l] := RunoffCummulMap[k,l];
      End;

{for m := 1 to numOutlet do
  Showmessage('The total discharge at outlet nr. ' + inttostr(m) + ' amounts to ' + floattostr(sum_discharge[m]) + ' m³.');
 }
  //The total amount of runoff that passes through each outlet is written to a .txt file
  setcurrentDir(File_output_dir);
  assignfile(Discharge_tot,'Total discharge.txt');
  rewrite(Discharge_tot);
  Writeln(Discharge_tot, 'Total discharge at each outlet [m³]');
  // write title
  Write(Discharge_tot, 'Outlet ID',chr(9),'Discharge');
  // write column headings
  writeln(Discharge_tot,'');
  // go to next line

  For i := 1 To numOutlet Do
    Begin
      Write(Discharge_tot, inttostr(i), chr(9), floattostr(sum_discharge[i]));
      writeln(Discharge_tot, '');
    End;
  closefile(Discharge_tot);
  //The memory of Discharge is released

  //The total amount of spillover (m³) for each buffer is written to a .txt file
  If Include_buffer Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(spillover_txt,'Spillover per buffer.txt');
      rewrite(spillover_txt);
      Writeln(spillover_txt, 'Amount of water flowing over dam for each buffer [m³]');
      // write title
      Write(spillover_txt, 'Buffer ID',chr(9),'Spillover');
      // write column headings
      writeln(spillover_txt,'');
      // go to next line

      For i := 1 To number_of_buffers Do
        Begin
          Write(spillover_txt, inttostr(i), chr(9), floattostr(spillover[i]));
          writeln(spillover_txt, '');
        End;
      closefile(spillover_txt);
      //The memory of spillover_txt is released
    End;


  // the total amount of water leaving the system through the sewers is written to a .txt file

  If Include_sewer Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(sewer_out_txt,'Sewer output water.txt');
      rewrite(sewer_out_txt);
      Writeln(sewer_out_txt,
              'Amount of water flowing out of the system through the sewer network [m³]');
      // write title
      Writeln(sewer_out_txt, floattostr(sewer_out_water));
      closefile(sewer_out_txt);
    End;


  //The memory of the created maps is released
  DisposedynamicRData(RunoffInputmap);
  DisposedynamicRData(RunoffInputmap_Temp);
  DisposedynamicRData(OutflowMap_temp);
  DisposedynamicRData(RainfallMap);
  DisposedynamicRData(Runoffmap);

End;

Function CalculateRe_singlevalue(Rainfall, CN, alpha, beta, I10, AR5, duration :double): double;
Var
  Ia,S, Re: double;
Begin
  S := 25400/CN - 254;
  Ia := 0.2*S;

  If Rainfall>Ia Then
    Begin
      Re := sqr(Rainfall-Ia)/((Rainfall-Ia)+S);
      Re := Re*power((I10/10),alpha)+AR5*Beta;
      // Volgens PhD KVO moet AR5 gedeeld worden door 10! (typfout in PhD?)
      // johanvdw: hangt uiteraard af van de eenheid die gebruikt wordt voor beta

    End
  Else
    Re := ((Rainfall-Ia)*(Duration/1440));

      // controle: runoff mag niet groter zijn dan totale regenval!
    If Re > Rainfall Then
      // johanvdw: de correctie van Van Oost hierboven kan dit opleveren indien power((I10/10),alpha) > 1
      Re:= Rainfall;

  Result := Re;
End;

//******************************************************************************
//This procedure calculates the amount of rainfall excess (=runoff) or rainfall
//deficit (= amount of water that can re-infiltrate in the grid cell)
//******************************************************************************
Procedure CalculateRe(Var Remap:Rraster; Perceelskaart:Rraster; CNmap:Rraster; alpha, beta:double);

Var 
  i,j, nrowPRC, ncolPRC : integer;
  Ia, S: double;

Begin
  nrowPRC := nrow;
  ncolPRC := ncol;
  SetLength(Remap,nrowPRC+1, ncolPRC+1);

  For i:= 1 To nrowPRC Do
    For j := 1 To ncolPRC Do
      Begin
        If (Perceelskaart[i,j]<>0) And (CNmap[i,j]<>0) Then
          Begin
            Remap[i,j] := CalculateRe_singlevalue(Rainfall, CNMap[i,j], alpha,beta,I10,AR5,duration);
          End
        Else Remap[i,j] := 0.0;

        If (Perceelskaart[i,j]=-1) Then
          Remap[i,j] := Rainfall;

        //Buffer and dam pixels are always assigned a CN value of 71
        If (Include_buffer) And (BufferMap[i,j] <> 0) or (Include_dam) And (Dam_map[i,j] <> 0) Then
          Begin
            Remap[i,j] := CalculateRe_singlevalue(Rainfall, 71, alpha,beta,I10,AR5,duration);
          End;

        // ditches are assigned a CN value of 98
        If (Include_ditch) And (Ditch_map[i,j] <> 0) Then
          Begin
            Remap[i,j] := CalculateRe_singlevalue(Rainfall, 98, alpha,beta,I10,AR5,duration);
          End;


      End;
End;


{
//******************************************************************************
//This procedure calculates the accumulated amount of runoff per gridcell. The
//water is routed with the Flux Decomposition algerithm with tillage direction
//(TCRP) and parcel borders taken into account.
//******************************************************************************
Procedure CalculateRunoffAcc(var UpArea: RRaster; Remap: RRaster; PRC: GRaster);
//!!!This unit is not used in the temporal distributed version of the model!!!
//In the original scrpits this unit was named "CalculateUpareaOlivier"
var
i, j, Teller, vlag: Integer;
Area: Double;      //Area is de hoeveelheid water (m³) per cel zoals berekend door de CN methode
Finish: GRaster;   //Een cel krijgt waarde 1 als een cel behandeld is
massbalance, totexport: double;

begin
SetDynamicRData(UpArea);
SetDynamicGdata(Finish);
SetzeroG(Finish);
SetzeroR(UpArea);

for teller:=nrow*ncol downto 1 do
begin
 i:=row[teller];
 j:=column[teller];
 If PRC[i,j]=0 then continue;
 Area:= UpArea[i,j]+(Remap[i,j]/1000)*sqr(Res);
 if Area < 0.0 then Area := 0.0; //Er kan geen negatieve hoeveelheid water worden doorgegeven aan de volgende cel
 if IsRiver(i,j) then
 //DistributeRiver(i, j, Area, UpArea, Finish)
 else
 //DistributeTildirEvent(i, j, Area, UpArea, Finish, massbalance, Topo);
 //!!! Bij input naar Distributetildirevent moet een booleaanse input (topo) worden gegeven.
 //    Bij true wordt er geen rekening gehouden met tildir, bij false wel. Deze input
 //    kan worden bepaald door in het 'input' venster de checkbox aan te klikken,
 //    en deze te linken aan deze booleaanse waarde.
 UpArea[i,j]:=UpArea[i,j] + (ReMap[i,j]/1000)*sqr(Res);
 if UpArea[i,j] < 0.0 then UpArea[i,j] := 0.0;
end;

//De cellen die zich in pits bevinden wordt de hoeveelheid water die hen bereikt toegekend
for i:= 1 to nrow do
 for j:= 1 to ncol do
  if Pit[i,j]<>0 then
  begin
  vlag:=Pit[i,j];
  UpArea[i,j]:=PitDat[vlag].input;
  end;

writeidrisi32file(ncol,nrow,'UpAreamap_20060821',UpArea);

//totexport := UpArea[97,353] + massbalance;
//showmessage('De totale hoeveelheid water dat het bekken verlaat is ' + floattostr(UpArea[36,105]));


DisposeDynamicGData(Finish);

end;     }

End.
