unit CN_calculations;

// In this unit al the calculations concerning the curve number are codedd

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, GData_CN, RData_CN, idrisi, Math, Raster_calculations,
  ReadInParameters;

Type
IntegerArray = array of integer;
FloatArray = array of Double;
FloatArray2 = array of array of Double;


procedure CalculateRe(var Remap:Rraster; Perceelskaart:Rraster ; CNmap:Rraster; alpha,beta:double);
{Procedure CalculateRunoffAcc(var UpArea: RRaster; Remap: RRaster; PRC: GRaster);}
Procedure ReadRainfallFile (var Raindata: TRainRecordArray; Rainfallfilename: string);
procedure ReadText(filename:string; var Z: FloatArray2; rowRain, colRain:integer);
function interp(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray):FloatArray;
function extrap(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray):FloatArray;
procedure CalculateTimeDependentRunoff(Remap: Rraster; RainData: TRainRecordArray; Routing: TRoutingArray; PRC: Rraster);
procedure CalculateRFactor;
procedure calcOutlet;
function  calcRivSeg(RivSeg:GRaster):integer;
function is_outlet(i,j:integer):boolean;
function sumPartArray (inputArray: FloatArray; start, number:integer): double;

var
NumberOfTimesteps, NTimesteps2, StartTime_rain, EndTime_rain, EndTime, EndTime_modelSec,
  numOutlet, numRivSeg: integer;
Rainfall, Duration, I10: double;
TimeSeries, Timeseries_tmp_fin :integerArray;
RainfallSeries, Sum_discharge, Sum_discharge_VHA:FloatArray;
Result_Discharge, Result_Discharge_VHA :FloatArray2;


implementation

//******************************************************************************
//In this procedure the rainfall file is read (and if necessary, converted to
//a different timestep). This is a .txt file (tab delimited) with the timestep
//(min) in column 1 and the amount of rainfall (mm) in column 2. In addition,
// several input variables for CN formula are calculated (Rainfall, I10 and duration).
//******************************************************************************
Procedure ReadRainfallFile (var Raindata: TRainRecordArray; Rainfallfilename: string);
var
  TimeSeries_output, I10TimeSeries: IntegerArray;
  Cumul_Rain, Cumul_Interp, RainfallSeries_output,I10RainfallSeries, I10Series: FloatArray;
  teller, i, j, nr, Timestep_rain, Fill_num: integer;
  x : double;
  datafile: textfile;
  a, b, c: string;
  Z:FloatArray2;

begin
  teller := 0;
  assignfile(datafile, RainfallFilename);
  reset(datafile);
  while not eof(datafile) do
  begin //To determine the number of time steps and number of rows in the file
   inc(teller);
   Readln(datafile, a);
   end;
  Setlength(TimeSeries, teller); //Time in seconds
  Setlength(Rainfallseries, teller); //Regenval in mm (per timestep)
  NumberOfTimesteps := teller-1;

  ReadText(RainfallFilename,Z,teller,2);   // see procedure below

  for
    i:=0 to NumberOfTimesteps do
    begin
     TimeSeries[i]:=Trunc(Z[i,0]);    //Array containing the time steps
     RainfallSeries[i]:=Z[i,1];     //Array with the amount of rainfall (mm) per time step
    end;

  for i := 0 to NumberOfTimesteps do         // convert time min => sec
      begin
        TimeSeries[i] := TimeSeries[i]*60;
      end;

  Timestep_rain := Timeseries[1]-TimeSeries[0];

  if (Simplified) then
  begin
    Timestep_model := Timestep_rain;
    TimeSeries_output := TimeSeries;
    RainfallSeries_output := RainfallSeries;
  end

  else    // interpolation of rainfall data to desired timestep
  begin
     Setlength(Cumul_rain, NumberOfTimesteps+1);    // calculate cumulative rainfall series
     Cumul_rain[0] := RainfallSeries[0];
     for i := 1 to NumberOfTimesteps do
       begin
          Cumul_rain[i] := Cumul_rain[i-1] + RainfallSeries[i];
       end;

     // create new timeseries array
     NumberOfTimesteps := NumberOfTimesteps*(Timestep_rain div Timestep_model);
     Setlength(TimeSeries_output, NumberOfTimesteps+1);
     TimeSeries_output[0] := TimeSeries[0];
     for i := 1 to NumberOfTimesteps do
       begin
       TimeSeries_output[i] := TimeSeries_output[i-1] + Timestep_model;
       end;

     // interpolation of cumulative rainfall series, see function below
     Cumul_Interp := interp(TimeSeries, TimeSeries_output, Cumul_rain);

     // cumulative rainfall series is converted to new rainfall series
     Setlength(RainfallSeries_output, NumberOfTimesteps+1);
     RainfallSeries_output[0]:=Cumul_Interp[0];
     for i := 1 to NumberOfTimesteps do
       begin
          RainfallSeries_output[i]:= Cumul_Interp[i]-Cumul_Interp[i-1];
       end;

  EndTime := TimeSeries_output[NumberOfTimesteps];  // extra zeros are added to the rainfall series if necessary
  EndTime_modelSec := EndTime_model * 60;
    if EndTime_modelSec > EndTime then
    begin
      Fill_num:= (EndTime_modelSec-EndTime) div Timestep_model;
      if (EndTime_modelSec-Endtime) mod Timestep_model <> 0 then
        begin
          Fill_num:=Fill_num+1;
        end;
      NumberOfTimesteps:=NumberOfTimesteps+Fill_num;
      setlength(TimeSeries_output, NumberOfTimesteps+1);
      setlength(RainfallSeries_output, NumberOfTimesteps+1);

      for i:= NumberOfTimesteps-Fill_num+1 to length(TimeSeries_output)-1 do
          begin
            TimeSeries_output[i]:=TimeSeries_output[i-1]+Timestep_model;
            RainfallSeries_output[i]:=0;
          end;
    end;

end;

  Rainfall := Sum(RainfallSeries_output);  // calculate total amount of rainfall (mm)

  if not Simplified then
    begin
        for i:=0 to NumberOfTimesteps do        // calculate start time of rain
            begin
              if RainfallSeries_output[i] <> 0 then
                begin
                  StartTime_rain := TimeSeries_output[i-1];
                  Break;
                end;
            end;

        for i:= NumberOfTimesteps downto 0 do    // calculate end time of rain
            begin
              if RainfallSeries_output[i] <> 0 then
                begin
                EndTime_rain := TimeSeries_output[i];
                Break;
                end;
            end;

        Duration := (EndTime_rain - StartTime_rain)/60;  // calculate duration of rain in min

        // calculation of I10: rainfall series is converted to a one-minute series, after which
        // I10 is calculated

        if Timestep_rain = 60 then
          begin
            I10TimeSeries := TimeSeries;
            I10RainfallSeries := RainfallSeries;
          end
        else if Timestep_rain > 60 then    // interpolation of the original series is needed
            begin
              nr := (length(TimeSeries)-1)*(TimeStep_rain div 60);
              setlength(I10TimeSeries, nr+1);
              setlength(I10RainfallSeries, nr+1);
              I10TimeSeries[0]:=TimeSeries[0];
              for i := 1 to nr do
                I10TimeSeries[i]:=I10TimeSeries[i-1]+60;
              I10RainfallSeries[0]:=RainfallSeries[0];
              j:=1;
              for i := 1 to (length(TimeSeries)-1) do
                begin
                  x := RainfallSeries[i]/(Timestep_rain div 60);
                  while j <= i*(Timestep_rain div 60) do
                  begin
                    I10RainfallSeries[j]:=x;
                    Inc(j);
                  end;
                end;
            end
        else           // extrapolation of the original series is needed
            begin
               nr := (length(TimeSeries)-1)div(60 div TimeStep_rain);
               setlength(I10TimeSeries, nr+1);
               I10TimeSeries[0]:=TimeSeries[0];
               for i := 1 to nr do
                    begin
                      I10TimeSeries[i]:=I10TimeSeries[i-1]+60;
                    end;
               I10RainfallSeries:= extrap(TimeSeries, I10TimeSeries, RainfallSeries);
            end;

        // for each minute, the intensity for the coming 10 minutes is calculated and
        // saved in array I10Series. Then the maximum value in this array is calculated.

        setlength(I10Series, length(I10TimeSeries)-1);
        for i := 1 to length(I10TimeSeries)-1 do
            I10Series[i-1]:= sumPartArray(I10RainfallSeries,i,9)/(1/6);

        I10 := I10Series[0];
        for i := 1 to length(I10Series)-1 do
          begin
            if I10Series[i] > I10 then
              I10 := I10Series[i];
          end;
    end;

  //Time and Rainfall are written to a Record
  Setlength(Raindata,NumberOfTimesteps+1);
  For i := 0 to NumberOfTimesteps do
  begin
  RainData[i].ID := i+1;
  RainData[i].Time := TimeSeries_output[i];
  RainData[i].Rain := RainfallSeries_output[i];
  RainData[i].Rain_fraction := RainData[i].Rain / Rainfall;
  end;
end;
//*****************************************************************************
// This procedure reads a tab delimited text file and converts it to a 2D array
// (used in procedure ReadRainfallFile)
//*****************************************************************************

procedure ReadText(filename:string; var Z: FloatArray2; rowRain, colRain:integer);
var
inputfile:textfile;
i,j:integer;

begin
setlength(Z,rowRain,colRain);
assignfile(inputfile, filename);
reset (inputfile);
for i:=0 to rowRain-1 do
  for j:=0 to colRain-1 do
  begin
 Read(inputfile,Z[i,j]);
  end;
closefile(inputfile);

end;

//*****************************************************************************
// This function is used for the interpolation of the rainfall data (see above)
//*****************************************************************************

  function interp(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray):FloatArray;
  var
  i,j,k,l,t1,t2,step,x0,x1 : integer;
  y0,y1: double;

  begin
    setlength(interp, length(xNew));
    interp[0] := yOriginal[0];
    t1:= xOriginal[1];
    t2:= xNew[1];
    step:=t1 div t2;
    for i := 1 to length(xOriginal)-1 do
    begin
      j := (step*i);
      interp[j] := yOriginal[i];
    end;

    k:=1;
    x0:=xNew[k-1];
    y0:=interp[k-1];
    x1:=xNew[k+step-1];
    y1:=interp[k+step-1];

    while k < length(interp)  do
      begin
        for l := 0 to step-2 do
        begin
          interp[k+l]:=y0+((y1-y0)*((xNew[k+l]-x0)/(x1-x0)));
        end;

        k:=k+(step);
        x0:=xNew[k-1];
        y0:=interp[k-1];
        x1:=xNew[k+step-1];
        y1:=interp[k+step-1];
      end;
  end;

  //*****************************************************************************
  // This function is used for the extrapolation of the rainfall data (see above)
  //*****************************************************************************

function extrap(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray):FloatArray;
  var
    t1,t2,step,i,j: integer;

  begin
    setlength(extrap, length(xNew));
    extrap[0] := yOriginal[0];
    t1:= xOriginal[1];
    t2:= xNew[1];
    step:=t2 div t1;
    j:=1;
    for i := 1 to length(xNew)-1 do
    begin
       extrap[i] := sum(yOriginal[j..j+step-1]);
       j:=j+step;
    end;

  end;

//******************************************************************************
//In this procedure the number of outlets is determined based on the outlet map
//provided by the user. In case no outlet map is provided, the location of the
//outlet is determined based on the DTM and the parcel map. The locations of the
// outlet(s) are written to an array.
//******************************************************************************
procedure calcOutlet;
var
  i,j,k,l:integer;
  height, max_uparea: double;
  outlet_rst : GRaster;
begin
numOutlet := 1;
k:=1;
  if outlet_Select then
    begin
      for i := 1 to nrow do
        for j := 1 to ncol do
        begin
            if Outlet[i,j] <> 0 then
              begin
                setlength(OutletArray, k+1, 2);
                OutletArray[k,0] := i;
                OutletArray[k,1] := j;
                k := k+1;
                if Outlet[i,j] > numOutlet then
                   numoutlet := Outlet[i,j];
              end;
        end;
      // lowest outlet is calculated
     lowOutletX := OutletArray[1,0];
     lowOutletY := OutletArray[1,1];
     if numOutlet > 1 then
       begin
       for l := 2 to numOutlet do
          begin
            if DTM[OutletArray[l,0],OutletArray[l,1]] < DTM[lowOutletX, lowOutletY] then
               begin
                 lowOutletX := OutletArray[l,0];
                 lowOutletY := OutletArray[l,1];
               end;
          end;
     end;
    end
  else
  begin
    // calculation of x and y coordinates of outlet... (= lowest river pixel)
  setlength(OutletArray, k+1, 2);
  height := 9999;
   for i := 1 to nrow do
     for j := 1 to ncol do
     begin
        if (PRC[i,j] = -1) AND (DTM[i,j]<height) then
          begin
            OutletArray[k,0] := i;
            OutletArray[k,1] := j;
            height := DTM[i,j];
          end;
     end;
   if height = 9999 then            // if no riverpixels present => select pixel with largest UPAREA as outlet
     begin
       max_uparea := 0;
       for i := 1 to nrow do
       for j := 1 to ncol do
       begin
          if UPAREA[i,j] > max_uparea then
            begin
             OutletArray[k,0] := i;
             OutletArray[k,1] := j;
             max_uparea := UPAREA[i,j];
            end;
       end;
     end;
   lowOutletX := OutletArray[k,0];             // lowest outlet = only outlet
   lowOutletY := OutletArray[k,1];

   // write location of outlet to Idrisi raster (Outlet.rst)
   SetDynamicGData(outlet_rst);
   SetzeroG(outlet_rst);
   outlet_rst[lowOutletX, lowOutletY] := 1;
   writeGIdrisi32file(ncol,nrow, Datadir+'Outlet'+'.rst',outlet_rst);
   DisposeDynamicGdata(outlet_rst);
  end;
end;

function calcRivSeg(RivSeg:GRaster):integer;
var i,j:integer;
begin
  result := 1;
  for i := 1 to nrow do
   for j := 1 to ncol do
   begin
   if (RivSeg[i,j] <> 0) AND (result < RivSeg[i,j]) then
     result := RivSeg[i,j];
   end;
end;

function is_outlet(i,j:integer):boolean;

var
m : integer;
  begin
    result := false;

    for m := 1 to numOutlet do
    begin
    if (i = OutletArray[m,0]) AND (j = OutletArray[m,1]) then
    result := true;
    end;
  end;

//******************************************************************************
//In this procedure the R factor of the RUSLE equation is calculated based on
//the rainfall input.
//******************************************************************************
procedure CalculateRFactor;
var
  Timestep, i, j, nr, index, Tini, Tfin :integer;
  eventEnergy, maxI30, eventRFactor, x: double;
  newTimeSeries, minTimeSeries : integerArray;
  newRainfallSeries, minRainfallSeries, rainVolume, rainEnergy, I30: FloatArray;
begin
  Timestep := Timeseries[1]-TimeSeries[0];  // bereken timestep

  if Timestep = 600 then    //(=10 min)
     begin
     newTimeSeries := TimeSeries;
     newRainfallSeries := RainfallSeries;
     end
  else
  begin
    if Timestep > 60 then     // eerst omzetten naar tijdstap van 1 min (makkelijker)
      begin
        nr := (length(TimeSeries)-1)*(TimeStep div 60);
        setlength(minTimeSeries, nr+1);
        setlength(minRainfallSeries, nr+1);
        minTimeSeries[0]:=TimeSeries[0];
        for i := 1 to nr do
          minTimeSeries[i]:=minTimeSeries[i-1]+60;
        minRainfallSeries[0]:=RainfallSeries[0];
        j:=1;
        for i := 1 to (length(TimeSeries)-1) do
          begin
            x := RainfallSeries[i]/(Timestep div 60);
            while j <= i*(Timestep div 60) do
            begin
              minRainfallSeries[j]:=x;
              Inc(j);
            end;
          end;
      end
      else if Timestep < 60 then
        begin
           nr := (length(TimeSeries)-1)div(60 div TimeStep);
           setlength(minTimeSeries, nr+1);
           minTimeSeries[0]:=TimeSeries[0];
           for i := 1 to nr do
                begin
                  minTimeSeries[i]:=minTimeSeries[i-1]+60;
                end;
           minRainfallSeries:= extrap(TimeSeries, minTimeSeries, RainfallSeries);
        end
      else
        begin
         minTimeSeries := TimeSeries;
         minRainfallSeries := RainfallSeries;
        end;
                                  // vervolgens conversie naar tijdstap van 10 min
    nr := ((length(minTimeSeries)-1) div 10) + 1;
    setlength(newTimeSeries, nr);
    setlength(newRainfallSeries, nr);
    newTimeSeries[0]:=TimeSeries[0];
      for i := 1 to nr do
        begin
          newTimeSeries[i]:=newTimeSeries[i-1]+600;
        end;
      newRainfallSeries:= extrap(minTimeSeries, newTimeSeries, minRainfallSeries);
  end;

// zoek geldig event en baken het af (tini...tfin)
  index := 0;
  while index <= (length(newTimeSeries)-1) do
    begin
     if newRainfallSeries[index] <> 0 then     // van zodra er regen valt...
        begin
          Tini := index;
          while (sumPartArray(newRainfallSeries,index+1,35)<>0) AND (index < length(newTimeSeries)-1) do
             inc(index);
// einde van event wordt bereikt van zodra in volgende 6u geen regen valt of van zodra
// we aan einde van rainfall file komen...

          Tfin := index;
          rainVolume := Copy(newRainfallSeries, Tini, Tfin-Tini+1);
          // test of totaal rainvolume > 1.27 mm (indien niet, is dit geen erosive event en
          // wordt dit event niet meegenomen bij berekening R factor)
          if sum(rainVolume) < 1.27 then
             begin
               index:=index+1;
               continue;
             end;
          SetLength(rainEnergy, Tfin-Tini+1);
          eventEnergy := 0;
          for i := 0 to Tfin-Tini do                   // bereken rain energy voor elke timestep binnen event
            begin
            rainEnergy[i] := 11.12*power((rainVolume[i]/(1/6)),0.31);      // in J/m².mm
            eventEnergy := eventEnergy+(rainEnergy[i]*rainVolume[i]);       // in J/m²
            end;
          // bepaal max I30 van event
          if Tfin-Tini-1 < 1 then
            maxI30 := sum(rainVolume) * 2
          else
          begin
          SetLength(I30, Tfin-Tini-1);
          for i := 0 to Tfin-Tini-2 do
            I30[i] := (rainVolume[i]+rainVolume[i+1]+rainVolume[i+2])*2;      // in mm/h

          maxI30 := I30[0];
          for i:=1 to Tfin-Tini-2 do
            begin
             if I30[i] > maxI30 then
                maxI30 := I30[i];
            end;
          end;
          // bereken R factor van event
          eventRFactor := (eventEnergy*maxI30)/1000000;          // in MJ.mm/m².h
          // bereken totale R factor
          RFactor := RFactor + eventRFactor;                  // in MJ.mm/m².h
        end;
     // verhoog index met 1
    index := index +1;
    end;
end;

function sumPartArray (inputArray: FloatArray; start, number: integer): double;
var
  i, endArray, endSum: integer;
begin
endArray := length(inputArray)-1;

if start+number <= endArray then
   endSum := start+number
else
   endSum := endArray;

sumPartArray:= inputArray[start];
for i := start+1 to endSum do
  sumPartArray := sumPartArray+inputArray[i];
end;


//******************************************************************************
//In this procedure the time dependent runoff is calculated
//******************************************************************************
procedure CalculateTimeDependentRunoff(Remap: Rraster; RainData: TRainRecordArray; Routing: TRoutingArray; PRC: Rraster);
var
RunoffMap, RiverInputMap, RunoffInputMap, RunoffInputMap_temp, RoutedMap_temp, RainfallMap, RunoffCummulMap, OutflowMap_temp: Rraster;
i, j,  k , l, m, o, teller, vlag, targetX, targetY: integer;
RunoffInput ,Part1_water, Part2_water, Speed, spill, sewer_out_water : double;
Discharge, Discharge_tot, Discharge_VHA_txt, spillover_txt, sewer_out_txt: textfile;
spillover, Discharge_result_tmp, Discharge_tmp_fin: FloatArray;
Result, Discharge_VHA: FloatArray2;
Timeseries_tmp: integerArray;
label SKIP;

const
  speed_sewer = 1;  // assume 1 m/s

begin
//Runoff map is created (=Remap in which negative values are replaced by 0, and
//runoff in (m³)
//It is checked if the amount of runoff produced in a certain grid cell is smaller
//or equal to the amount of rainfall
//Startmap is created (= Remap with positive values replaced by zeros when runoff
//takes places (this is added for every time step) and negative values where
//re-infiltration takes place)

SetDynamicRData(RunoffInputmap); //= map with total amount of runoff per
SetDynamicRData(RunoffInputmap_Temp); //= map with runoff per pixel for every timestep
SetdynamicRData(OutflowMap_temp); //= map with the removal of water (via routing) from every grid cell for every time step
SetLength(Result,Numberoftimesteps+1,numOutlet+1);
if Include_buffer then
begin
  SetLength(spillover,number_of_buffers+1);    //= data array containing the amount of spillover per buffer
  for i := 1 to number_of_buffers do
    spillover[i] := 0;
end;

if Include_sewer then
  sewer_out_water := 0;

if VHA then
begin
 numRivSeg := calcRivSeg(RivSeg);
 Setlength(Discharge_VHA, Numberoftimesteps+1, numRivSeg+1);
end;
SetDynamicRData(RainfallMap);
SetDynamicRData(Runoffmap); //=map in which the amount of runoff per timestep is stored


for i := 1 to nrow do
    for j := 1 to ncol do
        begin
        if PRC[i,j] = 0 then continue;   // all cells outside catchment are skipped
        if Remap[i,j] < 0.0 then
           begin
           RunoffInputmap[i,j] := 0.0;
           Runoffmap[i,j] := (Remap[i,j]/1000)*sqr(res); // amount of water that still can infiltrate in m^3
           end
           else
           begin
           RunoffInputmap[i,j] := (Remap[i,j]/1000)*sqr(res); //runoff in m^3
           Runoffmap[i,j] := 0.0;
           end;
        RainfallMap[i,j] := (Rainfall/1000)*sqr(res);
        if RunoffInputmap[i,j] > RainfallMap[i,j] then //Total runoff for a pixel can never be larger than total rainfall for that pixel
           begin
           RunoffInputmap[i,j] := RainfallMap[i,j];
           end;
        end;

//If buffers are included the maximum discharge and dead volume for every buffer are calculated
if (Include_buffer) then
   for i := 1 to Number_of_Buffers do
       begin
       BufferData[i].Qmax := BufferData[i].Cd * BufferData[i].Opening_area * sqrt(2*9.91*(BufferData[i].height_dam - BufferData[i].Height_opening));
       BufferData[i].Volume_dead := (BufferData[i].Height_opening / BufferData[i].Height_dam) * BufferData[i].Volume;
       BufferData[i].area := BufferData[i].Volume/BufferData[i].Height_dam;
       end;

//For every timestep the correct amount of runoff is added to RunoffMap and
//is routed through the landscape
SetDynamicRData(RoutedMap_temp); //Amount of transferred water during a particular time step
SetDynamicRData(RunoffCummulMap); //Total amount of water that reaches every grid cell (both as a result of direct rainfall input and as a result of routed runoff from upland gidcells)
SetzeroR(RunoffCummulMap);

for i := 0 to NumberOfTimeSteps do //Every timestep is looked at
begin
 SetzeroR(RoutedMap_temp); //At he beginning of every timestep this map is set to zero
 SetzeroR(OutflowMap_temp);

 //De input per cel wordt bepaald voor de tijdstap
 for k := 1 to nrow do
        for l := 1 to ncol do //The input for every cell is determined
        begin
        if PRC[k,l] = 0 then continue;      // all cells outside catchment are skipped
        RunoffInput := RunoffInputmap[k,l] * RainData[i].Rain_fraction; //The runoff for that timestep is determined by scaling the total runoff
                                                                       //for that grid cell with the fraction of rainfall that falls during this time step
        RunoffInputmap_Temp[k,l] := RunoffInput;
        RunoffMap[k,l] := RunoffMap[k,l] + RunoffInput; //The amount of runoff for this time step is added to the amount of runoff
                                                        //that was already present at the beginning of the time step

        if is_outlet(k,l) then
             begin
             if outlet_Select then
               Result[i,Outlet[k,l]] := Result[i,Outlet[k,l]]+RunoffInput
             else
                Result[i,1] := Result[i,1]+RunoffInput;
           end;
        if (VHA) AND (RivSeg[k,l] <> 0) then
          Discharge_VHA[i,RivSeg[k,l]] := Discharge_VHA[i,RivSeg[k,l]] + RunoffInput;
        end;

//The runoff is routed for this time step
for teller:=nrow*ncol downto 1 do
begin
  k:=row[teller];  //row and col are vectors containing the row and column ID's from low to high
 l:=column[teller];
 if PRC[k,l] = 0 then continue;
 if RunoffMap[k,l] <= 0.0 then continue;
 if PRC[k,l] = -1 then
   speed := riv_vel
 else
   Speed := 0.3; //Water velocity in m/s (based on Govers 1992)

 //If buffers are present:
 if (Include_buffer) and (Buffermap[k,l] <> 0) and (Buffermap[k,l] <= Number_of_Buffers) and (RunoffMap[k,l]>0) then      // discharge from buffer is only altered at buffer center (place where the opening is situated)
    begin

    if RunoffMap[k,l] > BufferData[Buffermap[k,l]].Volume then //The maximum capacity of the buffer is reached: all the runoff that enters the buffer flows over it
       begin
       spill := BufferData[BufferMap[k,l]].Cd*BufferData[BufferMap[k,l]].width_dam*sqrt(9.81)*Power(((RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume)/BufferData[BufferMap[k,l]].area),(3/2))* Timestep_model;
       // calculate maximum amount of water flowing over dam (formula from project Pieter Meert p.68)
       if spill > RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume then     // water flowing over dam cannot exceed amount of water that does not fit in the buffer
         spill := RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume;
       Part1_Water := (BufferData[Buffermap[k,l]].Qmax * Timestep_model) + spill;
       //Part1_water is composed of (1) the volume of water inside the buffer that flows through the opening and (2) the additional water (>volume of the buffer) that flows over the dam
       RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
            RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] + Part1_water;
       RunoffMap[k,l] := RunoffMap[k,l] - Part1_water; //RunoffMap is updated
       OutflowMap_temp[k,l] := Part1_water;
       spillover[Buffermap[k,l]]:= spillover[BufferMap[k,l]]+spill;
       end

    else
    if RunoffMap[k,l] > BufferData[Buffermap[k,l]].Volume_dead then //The volume of water in the buffer is larger than the dead volume: part of it will drain
    begin
    Part1_water := (BufferData[Buffermap[k,l]].Qmax * sqrt(RunoffMap[k,l]/(BufferData[Buffermap[k,l]].Volume - BufferData[Buffermap[k,l]].Volume_dead))) * Timestep_model; // Amount of discharge = [Qmax * sqrt(vol(t)/vol(max))] * timestep
    RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
            RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] + Part1_water;
    RunoffMap[k,l] := RunoffMap[k,l] - Part1_water; //RunoffMap is updated
    OutflowMap_temp[k,l] := Part1_water;
    end

    else //The volume of water in the buffer is smaller than the dead volume: nothing will flow out
    begin
    Part1_water := 0;
    RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
            RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] + Part1_water;
    RunoffMap[k,l] := RunoffMap[k,l] - Part1_water; //RunoffMap is updated
    OutflowMap_temp[k,l] := Part1_water;
    end;

    targetX := Routing[k,l].Target1row;
    targetY := Routing[k,l].Target1col;
    if is_outlet(targetX,targetY) then       // if target cell is outlet, the result is updated
      begin
       if outlet_Select then
         Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+OutflowMap_temp[k,l]
       else
         Result[i,1] := Result[i,1]+OutflowMap_temp[k,l];
     end;
    if VHA then
       begin
         if RivSeg[targetX,targetY]<>0 then    // if buffer drains directly into river, the result is updated
           Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,targetY]]+OutflowMap_temp[k,l];
       end;
    Goto SKIP; //Proceed to next cell
    end;

 if Routing[k,l].Part1 > 0 then
    begin
         if Routing[k,l].Part2 > 0 then //If the water is routed towards 2 lower cells
            begin
              if (Include_sewer) AND (SewerMap[k,l] <> 0) then     // if cell contains entrance to sewer...
                Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed_sewer*TimeStep_model)/Routing[k,l].Distance2) //Amount of water that is transfered through sewers
              else
                Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed*TimeStep_model)/Routing[k,l].Distance2); //Amount of water that is transfered to neighbor 2

               Part1_water := (RunoffMap[k,l]*Routing[k,l].Part1)*((Speed*TimeStep_model)/Routing[k,l].Distance1); //Amount of water that is transfered to neighbor 1
               RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] + Part1_water;
               if (Include_sewer) AND (SewerMap[k,l] <> 0) then
                begin
                  RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] :=
                  RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] + (Part2_water * (1-(sewer_exit/100)));
                  sewer_out_water := sewer_out_water + (Part2_water * (sewer_exit/100));
                end
               else
                  RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] :=
                  RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] + Part2_water;
               RunoffMap[k,l] := RunoffMap[k,l] - (Part1_water + Part2_water); //The total amount of runoff that leaves the grid cell during this time step is subtracted from the RunoffMap
               OutflowMap_temp[k,l] := Part1_water + Part2_water; //Amount of water leaving the grid cell during this time step
               if RunoffMap[k,l] < 0 then RunoffMap[k,l] := 0; //No more water then is present can leave a cell


               targetX := Routing[k,l].Target1row;
               targetY := Routing[k,l].Target1col;
               if is_outlet(targetX,targetY) then      // if target cell 1 is outlet or river, the result is updated
                 begin
                  if outlet_Select then
                    Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+Part1_water
                  else
                    Result[i,1] := Result[i,1]+Part1_water;
                  end;
               if VHA then
                 begin
                   if RivSeg[targetX,targetY]<>0 then
                     Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,targetY]]+Part1_water;
                 end;

               targetX := Routing[k,l].Target2row;
               targetY := Routing[k,l].Target2col;
               if is_outlet(targetX,targetY) then      // if target cell 2 is outlet or river, the result is updated
                  begin
                  if outlet_Select then
                    begin
                    if (Include_sewer) AND (SewerMap[k,l] <> 0) then
                      Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+(Part2_water*(1-(sewer_exit/100)))
                    else
                      Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+Part2_water;
                    end
                  else
                    begin
                    if (Include_sewer) AND (SewerMap[k,l] <> 0) then
                      Result[i,1] := Result[i,1]+(Part2_water*(1-(sewer_exit/100)))
                    else
                      Result[i,1] := Result[i,1]+Part2_water;
                   end;
                 end;
               if VHA then
                 begin
                   if RivSeg[targetX,targetY]<>0 then
                   begin
                     if (Include_sewer) AND (SewerMap[k,l] <> 0) then
                      Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,targetY]]+(Part2_water*(1-(sewer_exit/100)))
                     else
                      Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,targetY]]+Part2_water;
                   end;
                 end;
            end
            else
                begin //If the runoff is transfered to only 1 neighbor (part 1 = 1)
                  Part1_water := (RunoffMap[k,l]*Routing[k,l].Part1)*((Speed*TimeStep_model)/Routing[k,l].Distance1);
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
                     RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] + Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water; //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                  if RunoffMap[k,l] < 0 then RunoffMap[k,l] := 0;

                  targetX := Routing[k,l].Target1row;
                  targetY := Routing[k,l].Target1col;
                  if is_outlet(targetX,targetY) then      // if target cell 1 is outlet or river, the result is updated
                     begin
                     if outlet_Select then
                       Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+Part1_water
                     else
                       Result[i,1] := Result[i,1]+Part1_water;
                     end;
                  if VHA then
                    begin
                      if (RivSeg[targetX,targetY]<>0) AND (RivSeg[k,l] = 0) then     // only if target cell is river AND source cell is no river, discharge_VHA should be updated
                        Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,targetY]]+Part1_water;
                    end;
                end;

    end
    else
    if Routing[k,l].Part2 > 0 then
      begin //If the runoff is transfered to only 1 neighbor (part 2 = 1)
        Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed*TimeStep_model)/Routing[k,l].Distance2);
        RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] :=
             RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] + Part2_water;
        RunoffMap[k,l] := RunoffMap[k,l] - Part2_water;
        OutflowMap_temp[k,l] := Part2_water;
        if RunoffMap[k,l] < 0 then RunoffMap[k,l] := 0;

        targetX := Routing[k,l].Target2row;
        targetY := Routing[k,l].Target2col;      // if target cell 1 is outlet or river, the result is updated
        if is_outlet(targetX,targetY) then
             begin
            if outlet_Select then
              Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+Part2_water
            else
              Result[i,1] := Result[i,1]+Part2_water;
            end;
        if VHA then
           begin
           if RivSeg[targetX,targetY]<>0 then
             Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,targetY]]+Part2_water;
           end;

      end;
 SKIP:
end;

//At the end of every time step the amount of displaced water is added to the runoff map
// + the amount of displaced water is stored for every grid cell to calculate a cummulative runoff map for the entire event
for k := 1 to nrow do
        for l := 1 to ncol do                                          // runoff cumul map = amount of water entering each cell
        begin
        if PRC[k,l] = 0 then continue;
        RunoffMap[k,l] := RunoffMap[k,l] + RoutedMap_temp[k,l];
        //RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + RoutedMap_temp[k,l];
        if RunoffMap[k,l] >= 0 then
        //RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + ((RunoffMap[k,l]*Speed*Timestep)/res) //Geeft totale hoeveelheid water (rekening houdend met snelheid en tijdstap)
        RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + RoutedMap_temp[k,l] + RunoffInputmap_Temp[k,l];
        //else RunoffCummulMap[k,l] := 0;
        end;
end;


// Runoff results are converted to a different timestep if requested by the user

if convert_output then
begin
  setlength(TimeSeries_tmp,Numberoftimesteps+1);
  for i := 0 to Numberoftimesteps do
    TimeSeries_tmp[i] := Raindata[i].Time;
  setlength(discharge_result_tmp, Numberoftimesteps+1);

  NTimesteps2 := Numberoftimesteps div ((Timestep_output*60) div Timestep_model);
  setlength(Result_Discharge, Ntimesteps2+1,numOutlet+1);
  setlength(TimeSeries_tmp_fin, NTimesteps2+1);
  TimeSeries_tmp_fin[0] := TimeSeries_tmp[0];
  for i:=1 to length(TimeSeries_tmp_fin)-1 do
   TimeSeries_tmp_fin[i] := TimeSeries_tmp_fin[i-1]+(Timestep_output*60);
  setlength(Discharge_tmp_fin, NTimesteps2+1);

  for j := 1 to numOutlet do
  begin
    for i:=0 to Numberoftimesteps do
       Discharge_result_tmp[i]:=Result[i,j];
    Discharge_tmp_fin := extrap(TimeSeries_tmp,TimeSeries_tmp_fin,Discharge_result_tmp);
    for i:=0 to NTimesteps2 do
      Result_Discharge[i,j] := Discharge_tmp_fin[i];
  end;

  if VHA then
  begin
    setlength(Result_Discharge_VHA, Ntimesteps2+1, numRivSeg+1);
    for j := 1 to numRivSeg do
    begin
     for i:=0 to Numberoftimesteps do
        Discharge_result_tmp[i] := Discharge_VHA[i,j];
     Discharge_tmp_fin := extrap(TimeSeries_tmp,TimeSeries_tmp_fin,Discharge_result_tmp);
     for i :=0 to NTimesteps2 do
        Result_Discharge_VHA[i,j] := Discharge_tmp_fin[i];
     end;
  end;

end
else
begin
  setlength(Result_Discharge, numberoftimesteps+1, numOutlet+1);
  Result_Discharge := Result;

  if VHA then
  begin
    setlength(Result_Discharge_VHA, numberoftimesteps+1, numRivSeg+1);
    Result_Discharge_VHA := Discharge_VHA;
  end;
end;


//The amount of runoff that leaves the catchment during every time step is written to a .txt file
setcurrentDir(File_output_dir);
assignfile(Discharge,'Discharge.txt');
rewrite(Discharge);
Writeln(Discharge, 'Discharge at each outlet [m³/s]');    // write title

if convert_output then
  Write(Discharge, 'Time (min)',chr(9))
else
  Write(Discharge, 'Time (sec)',chr(9));

for m := 1 to numOutlet do
  write(Discharge, 'Outlet ', m, chr(9));     // write column headings
writeln(Discharge,'');   // go to next line

setlength(Sum_discharge,numOutlet+1);
for m := 1 to numOutlet do
  Sum_discharge[m] := 0;

if convert_output then
begin
  for i := 0 to NTimesteps2 do
  begin
  Write(Discharge, inttostr(Timeseries_tmp_fin[i] div 60), chr(9));
  for m := 1 to numOutlet do
    write (Discharge, floattostr(Result_Discharge[i,m]/(Timestep_output*60)), chr(9)); //Amount of discharge per time step is written to the .txt file
  writeln(Discharge, '');
  for m := 1 to numOutlet do
    Sum_discharge[m] := Sum_discharge[m] + Result_Discharge[i,m]; //Total amount of water leaving the catchment (this is dependent on the water velocity and length of the time step!)
  end;
end
else
begin
  for i := 0 to Numberoftimesteps do
  begin
  Write(Discharge, inttostr(RainData[i].Time), chr(9));
  for m := 1 to numOutlet do
    write (Discharge, floattostr(Result_Discharge[i,m]/Timestep_model), chr(9)); //Amount of discharge per time step is written to the .txt file
  writeln(Discharge, '');
  for m := 1 to numOutlet do
    Sum_discharge[m] := Sum_discharge[m] + Result_Discharge[i,m]; //Total amount of water leaving the catchment (this is dependent on the water velocity and length of the time step!)
  end;
end;
closefile(Discharge); //The memory of Discharge is released


// The amount of water entering each VHA river segment is written to a .txt file

if VHA then
begin
  setcurrentDir(File_output_dir);
  assignfile(Discharge_VHA_txt,'Discharge_VHA.txt');
  rewrite(Discharge_VHA_txt);
  Writeln(Discharge_VHA_txt, 'Discharge to each river segment [m³/s]');    // write title
  if convert_output then
    Write(Discharge_VHA_txt, 'Time (min)',chr(9))
  else
    Write(Discharge_VHA_txt, 'Time (sec)',chr(9));
  for m := 1 to numRivSeg do
    write(Discharge_VHA_txt, 'VHA segment ', m, chr(9));     // write column headings
  writeln(Discharge_VHA_txt,'');   // go to next line

  setlength(Sum_discharge_VHA,numRivSeg+1);
  for m := 1 to numRivSeg do
  Sum_discharge_VHA[m] := 0;

  if convert_output then
  begin
    for i := 0 to NTimesteps2 do
    begin
    Write(Discharge_VHA_txt, inttostr(Timeseries_tmp_fin[i] div 60), chr(9));
    for m := 1 to numRivSeg do
      write (Discharge_VHA_txt, floattostr(Result_Discharge_VHA[i,m]/(Timestep_output*60)), chr(9)); //Amount of discharge per time step is written to the .txt file
    writeln(Discharge_VHA_txt, '');
    for m := 1 to numRivSeg do
      Sum_discharge_VHA[m] := Sum_discharge_VHA[m] + Result_Discharge_VHA[i,m];   // total amount of water entering each river segment
    end;
  end
  else
  begin
    for i := 0 to Numberoftimesteps do
    begin
    Write(Discharge_VHA_txt, inttostr(RainData[i].Time), chr(9));
    for m := 1 to numRivSeg do
      write (Discharge_VHA_txt, floattostr(Result_Discharge_VHA[i,m]/Timestep_model), chr(9)); //Amount of discharge per time step is written to the .txt file
    writeln(Discharge_VHA_txt, '');
    for m := 1 to numRivSeg do
      Sum_discharge_VHA[m] := Sum_discharge_VHA[m] + Result_Discharge_VHA[i,m];   // total amount of water entering each river segment
    end;
  end;
  closefile(Discharge_VHA_txt);
end;



//A map with the total amount of runoff for every grid cell for the entire event is created
//(= amount of runoff from precipitation + amount of routed runoff that arrives in a cell - amount of runoff that leaves the cel via routing)
SetDynamicRData(RunoffTotMap);
for k := 1 to nrow do
        for l := 1 to ncol do
        begin
        if PRC[k,l] = 0 then RunoffTotMap[k,l] := 0.0
        else
        RunoffTotMap[k,l] := RunoffCummulMap[k,l];
        end;

{for m := 1 to numOutlet do
  Showmessage('The total discharge at outlet nr. ' + inttostr(m) + ' amounts to ' + floattostr(sum_discharge[m]) + ' m³.');
 }
//The total amount of runoff that passes through each outlet is written to a .txt file
setcurrentDir(File_output_dir);
assignfile(Discharge_tot,'Total discharge.txt');
rewrite(Discharge_tot);
Writeln(Discharge_tot, 'Total discharge at each outlet [m³]');    // write title
Write(Discharge_tot, 'Outlet ID',chr(9),'Discharge');     // write column headings
writeln(Discharge_tot,'');   // go to next line

for i := 1 to numOutlet do
begin
Write(Discharge_tot, inttostr(i), chr(9), floattostr(sum_discharge[i]));
writeln(Discharge_tot, '');
end;
closefile(Discharge_tot); //The memory of Discharge is released

//The total amount of spillover (m³) for each buffer is written to a .txt file
if Include_buffer then
begin
  setcurrentDir(File_output_dir);
  assignfile(spillover_txt,'Spillover per buffer.txt');
  rewrite(spillover_txt);
  Writeln(spillover_txt, 'Amount of water flowing over dam for each buffer [m³]');    // write title
  Write(spillover_txt, 'Buffer ID',chr(9),'Spillover');     // write column headings
  writeln(spillover_txt,'');   // go to next line

  for i := 1 to number_of_buffers do
  begin
  Write(spillover_txt, inttostr(i), chr(9), floattostr(spillover[i]));
  writeln(spillover_txt, '');
  end;
  closefile(spillover_txt); //The memory of spillover_txt is released
end;


// the total amount of water leaving the system through the sewers is written to a .txt file

if Include_sewer then
begin
  setcurrentDir(File_output_dir);
  assignfile(sewer_out_txt,'Sewer output water.txt');
  rewrite(sewer_out_txt);
  Writeln(sewer_out_txt, 'Amount of water flowing out of the system through the sewer network [m³]');    // write title
  Writeln(sewer_out_txt, floattostr(sewer_out_water));
  closefile(sewer_out_txt);
end;


//The memory of the created maps is released
DisposedynamicRData(RunoffInputmap);
DisposedynamicRData(RunoffInputmap_Temp);
DisposedynamicRData(OutflowMap_temp);
DisposedynamicRData(RainfallMap);
DisposedynamicRData(Runoffmap);

end;

//******************************************************************************
//This procedure calculates the amount of rainfall excess (=runoff) or rainfall
//deficit (= amount of water that can re-infiltrate in the grid cell)
//******************************************************************************
procedure CalculateRe(var Remap:Rraster; Perceelskaart:Rraster; CNmap:Rraster; alpha,beta:double);
var
  i,j,count, nrowPRC, ncolPRC : integer;
  Ia,S: double;

begin
nrowPRC := nrow;
ncolPRC:=ncol;
SetLength(Remap,nrowPRC+1, ncolPRC+1);

for i:= 1 to nrowPRC do
 for j := 1 to ncolPRC do
 begin
  if (Perceelskaart[i,j]<>0) AND (CNmap[i,j]<>0) then
   begin
    S := 25400/CNmap[i,j] - 254;
    Ia := 0.2*S;
    if Rainfall>Ia then
      begin
        Remap[i,j]:= sqr(Rainfall-Ia)/((Rainfall-Ia)+S);
        Remap[i,j]:=Remap[i,j]*power((I10/10),alpha)+AR5*Beta; //Volgens PhD KVO moet AR5 gedeeld worden door 10! (typfout in PhD?)
       end
       else Remap[i,j]:=((Rainfall-Ia)*(Duration/1440));
   end
   else Remap[i,j]:=0.0;

   if (Perceelskaart[i,j]=-1) then
    Remap[i,j]:=Rainfall;

   //Buffer and dam pixels are always assigned a CN value of 71
   If (Include_buffer) AND (BufferMap[i,j] <> 0) then
   begin
   S := 25400/71 - 254;
   Ia := 0.2*S;
   if Rainfall>Ia then
     begin
       Remap[i,j]:= sqr(Rainfall-Ia)/((Rainfall-Ia)+S);
       Remap[i,j]:=Remap[i,j]*power((I10/10),alpha)+AR5*Beta; //Volgens PhD KVO moet AR5 gedeeld worden door 10! (typfout in PhD?)
      end
   else Remap[i,j]:=((Rainfall-Ia)*(Duration/1440));
   end;

   If (Include_dam) AND (Dam_map[i,j] <> 0) then
   begin
   S := 25400/71 - 254;
   Ia := 0.2*S;
   if Rainfall>Ia then
     begin
       Remap[i,j]:= sqr(Rainfall-Ia)/((Rainfall-Ia)+S);
       Remap[i,j]:=Remap[i,j]*power((I10/10),alpha)+AR5*Beta; //Volgens PhD KVO moet AR5 gedeeld worden door 10! (typfout in PhD?)
      end
   else Remap[i,j]:=((Rainfall-Ia)*(Duration/1440));
   end;

   // ditches are assigned a CN value of 98
   If (Include_ditch) AND (Ditch_map[i,j] <> 0) then
    begin
     S := 25400/98 - 254;
     Ia := 0.2*S;
     if Rainfall>Ia then
       begin
         Remap[i,j]:= sqr(Rainfall-Ia)/((Rainfall-Ia)+S);
         Remap[i,j]:=Remap[i,j]*power((I10/10),alpha)+AR5*Beta; //Volgens PhD KVO moet AR5 gedeeld worden door 10! (typfout in PhD?)
        end
     else Remap[i,j]:=((Rainfall-Ia)*(Duration/1440));
    end;

   // controle: runoff mag niet groter zijn dan totale regenval!
   if Remap[i,j] > Rainfall then
    Remap[i,j] := Rainfall;
 end;

end;

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

end.

