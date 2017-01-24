unit Calculations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ReadInParameters, RData_CN, GData_CN, Dialogs;

Procedure DetectEvents;
function sumPartArray (inputArray: FloatArray; start, number: integer): double;
function sumPartIntArray(inputArray: integerArray; start, number : integer): integer;
Procedure DemarcateSeasons;
function leapYear (year:integer):boolean;
procedure identifyInput(eventID: integer);
function calc_year(eventID:integer): integer;
function calculate_AR5 (eventID: integer):double;
function calc_month(eventID:integer): integer;
function calc_season(eventID: integer): string;
procedure Calc_numOutlet;
procedure Calc_numVHA;
procedure updateOutput(eventID:integer);
procedure updateMap(eventMap: RRaster; var totMap: RRaster);
procedure updateText_sec(eventText: FloatArray2; var totText : FloatArray2; eventID, numColumns, timestep : integer);
procedure updateText_min(eventText: FloatArray2; var totText : FloatArray2; eventID, numColumns, timestep : integer);

implementation


// this procedure detects all events in the rainfall file and saves them in a multi dimensional array
// for each event, Tini, Tfin and NumberOfElements is saved in a record

Procedure DetectEvents;
var
  index, Tini, Tfin, NumElements, i : integer;
  rainVolume : FloatArray;
  rainInt : integerArray;
begin

numberOfEvents := 0;
numElements := 0;

// zoek geldig rain event en save in "Events" array
  index := 0;
  while index <= (length(TimeSeries_fin)-1) do
    begin
     if RainfallSeries_fin[index] <> 0 then     // van zodra er regen valt...
        begin
          Tini := index;
          while (sumPartArray(RainfallSeries_fin,index+1, (21600 div Timestep_model)-1)<>0) AND (index < length(TimeSeries_fin)-1) do
             inc(index);
// einde van event wordt bereikt van zodra in volgende 6u geen regen valt of van zodra
// we aan einde van rainfall file komen...

          Tfin := index;
          rainVolume := Copy(RainfallSeries_fin, Tini, Tfin-Tini+1);
          // test of totaal rainvolume > 1.27 mm (indien niet, is dit geen erosive event en
          // wordt dit event niet meegenomen in modelberekeningen)
          if sum(rainVolume) < 1.27 then
             begin
               index:=index+1;
               continue;
             end
            else
            begin
             numberOfEvents := numberOfEvents+1;
             if NumElements < length(rainVolume) then
                NumElements := length(rainVolume);
             SetLength(Events, NumElements, numberOfEvents+1);
             for i := 0 to length(rainVolume)-1 do
               Events[i,numberOfEvents] := rainVolume[i];
             SetLength(Event_meta,numberOfEvents+1);
             Event_meta[numberOfEvents].Tini :=Tini;
             Event_meta[numberOfEvents].Tfin:=Tfin;
             setLength(rainInt, length(rainVolume));
             Event_meta[numberOfEvents].NumberOfElements:=length(rainInt);
             //showmessage('number of events: ' + inttostr(numberOfEvents) + '/ index: ' + inttostr(index));
            end;
        end;
     index:=index+1;
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

  function sumPartIntArray(inputArray: integerArray; start, number : integer): integer;
var
 i, endArray, endSum: integer;
begin
endArray := length(inputArray)-1;

if start+number <= endArray then
   endSum := start+number
else
   endSum := endArray;

sumPartIntArray:= inputArray[start];
for i := start+1 to endSum do
  sumPartIntArray := sumPartIntArray+inputArray[i];
end;

Procedure DemarcateSeasons;
begin
setlength(numDays,13);
numDays[1]:=31;
if leapYear(year) = True then
  numDays[2]:=29
else
  numDays[2]:=28;
numDays[3]:=31;
numDays[4]:=30;
numDays[5]:=31;
numDays[6]:=30;
numDays[7]:=31;
numDays[8]:=31;
numDays[9]:=30;
numDays[10]:=31;
numDays[11]:=30;
numDays[12]:=31;

case First_month of
  1: begin
   start_winter := sumPartIntArray(numDays,1,10);
   start_summer := sumPartIntArray(numDays,1,4);
   start_spring := sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,1,7);
   first_season := 'winter';
    end;
  2: begin
   start_winter := sumPartIntArray(numDays,2,9);
   start_summer := sumPartIntArray(numDays,2,3);
   start_spring := numDays[2];
   start_fall := sumPartIntArray(numDays,2,6);
   first_season := 'winter';
    end;
  3: begin
   start_winter := sumPartIntArray(numDays,3,8);
   start_summer := sumPartIntArray(numDays,3,2);
   start_spring := 999999;
   start_fall := sumPartIntArray(numDays,3,5);
   first_season := 'spring';
    end;
  4: begin
   start_winter := sumPartIntArray(numDays,4,7);
   start_summer := sumPartIntArray(numDays,4,1);
   start_spring := sumPartIntArray(numDays,4,8) + sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,4,4);
   first_season := 'spring';
    end;
  5: begin
   start_winter := sumPartIntArray(numDays,5,6);
   start_summer := numDays[5];
   start_spring := sumPartIntArray(numDays,5,7) + sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,5,3);
   first_season := 'spring';
    end;
  6: begin
   start_winter := sumPartIntArray(numDays,6,5);
   start_summer := 999999;
   start_spring := sumPartIntArray(numDays,6,6) + sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,6,2);
   first_season := 'summer';
    end;
  7: begin
   start_winter := sumPartIntArray(numDays,7,4);
   start_summer := sumPartIntArray(numDays,7,5) + sumPartIntArray(numDays,1,4);
   start_spring := sumPartIntArray(numDays,7,5) + sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,7,1);
   first_season := 'summer';
    end;
  8: begin
   start_winter := sumPartIntArray(numDays,8,3);
   start_summer := sumPartIntArray(numDays,8,4) + sumPartIntArray(numDays,1,4);
   start_spring := sumPartIntArray(numDays,8,4) + sumPartIntArray(numDays,1,1);
   start_fall := numDays[8];
   first_season := 'summer';
    end;
  9: begin
   start_winter := sumPartIntArray(numDays,9,2);
   start_summer := sumPartIntArray(numDays,9,3) + sumPartIntArray(numDays,1,4);
   start_spring := sumPartIntArray(numDays,9,3) + sumPartIntArray(numDays,1,1);
   start_fall := 999999;
   first_season := 'fall';
    end;
  10:begin
   start_winter := sumPartIntArray(numDays,10,1);
   start_summer := sumPartIntArray(numDays,10,2) + sumPartIntArray(numDays,1,4);
   start_spring := sumPartIntArray(numDays,10,2) + sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,10,2) + sumPartIntArray(numDays,1,7);
   first_season := 'fall';
    end;
  11:begin
   start_winter := numDays[11];
   start_summer := sumPartIntArray(numDays,11,1) + sumPartIntArray(numDays,1,4);
   start_spring := sumPartIntArray(numDays,11,1) + sumPartIntArray(numDays,1,1);
   start_fall := sumPartIntArray(numDays,11,1) + sumPartIntArray(numDays,1,7);
   first_season := 'fall';
    end;
  12:begin
   start_winter := 999999;
   start_summer := numDays[12] + sumPartIntArray(numDays,1,4);
   start_spring := numDays[12] + sumPartIntArray(numDays,1,1);
   start_fall := numDays[12] + sumPartIntArray(numDays,1,7);
   first_season := 'winter';
    end;
  end;
{
// grenzen omzetten naar seconden
 start_winter := start_winter * 24 * 3600;
 start_summer := start_summer * 24 * 3600;
 start_spring := start_spring * 24 * 3600;
 start_fall := start_fall * 24 * 3600;
}
end;

function leapYear (year:integer):boolean;            // check whether we are in a leap year...
var
  x:double;
begin
  leapYear := False;
  if (First_month = 1) OR (First_month = 2) then
   begin
     x := year/100;
     x := frac(x)*100;
     if (year mod 4 = 0) AND (x <> 0) then
       leapYear := True;
   end
   else
   begin
    x := (year+1)/100;
    x := frac(x)*100;
    if ((year+1) mod 4 = 0) AND (x <> 0) then
      leapYear := True;
   end;
end;

procedure identifyInput(eventID: integer);
begin

 Use_Rfactor := False;

if not simplified then
 begin
 AR5_event := calculate_AR5(eventID);       // see function below



  RivVel_event := RivVel[calc_month(eventID)];  // month of event is determined after which the correct value is selected
                                                // (see function below) from the RivVel array

  case calc_season(eventID) of                 // season of event is determined (see function below)
  'spring': begin                              // and correct CN map is selected
     if calc_year(eventID) = 1 then
       CN_event := CNmapSpring
     else
       CN_event := CNmapSpring_2;
  end;
  'summer': begin
     if calc_year(eventID) = 1 then
      CN_event := CNmapSummer
     else
       CN_event := CNmapSummer_2;
   end;
  'fall': begin
     if calc_year(eventID) = 1 then
       CN_event := CNmapFall
     else
       CN_event := CNmapFall_2;
   end;
  'winter': begin
     if calc_year(eventID) = 1 then
       CN_event := CNmapWinter
     else
      CN_event := CNmapWinter_2;
   end;
  end;
end;

case calc_season(eventID) of                 // season of event is determined (see function below)
  'spring': begin                              // and correct Cfactor is selected
     if calc_year(eventID) = 1 then
       Cf_Data_event := Cf_Data_spring
     else
       Cf_Data_event := Cf_Data_spring_2;
  end;
  'summer': begin
     if calc_year(eventID) = 1 then
      Cf_Data_event := Cf_Data_summer
     else
       Cf_Data_event := Cf_Data_summer_2;
   end;
  'fall': begin
     if calc_year(eventID) = 1 then
       Cf_Data_event := Cf_Data_fall
     else
       Cf_Data_event := Cf_Data_fall_2;
   end;
  'winter': begin
     if calc_year(eventID) = 1 then
       Cf_Data_event := Cf_Data_winter
     else
       Cf_Data_event := Cf_Data_winter_2;
   end;
end;

  // select appropriate parcel map, kTc and ktil => determine whether the event takes place in the first or second year
  // of the simulation (see function below)
  if calc_year(eventID) = 1 then
  begin
   PARCEL_filename := PARCEL_filename1;
   if not Create_ktc then
     ktc_filename := ktc_Data_Filename;
   if not Create_ktil then
     ktil_filename := ktil_Data_Filename;
  end
  else
  begin
    PARCEL_filename := PARCEL_filename2;
    if not Create_ktc then
      ktc_filename := ktc_Data_Filename2;
    if not Create_ktil then
      ktil_filename := ktil_Data_Filename2;
  end;

  if eventID > 1 then                // the following output maps need to be written only once
   begin
      Write_ASPECT := false;
      Write_LS := false;
      Write_SLOPE := false;
      Write_UPAREA := false;
      Write_TILEROS := false;
   end;
end;


function  calc_year(eventID:integer): integer;
var
  month_event: integer;
begin
  month_event:=calc_month(eventID);
  if First_month = 1 then
   calc_year := 1
  else
    begin
      if month_event < First_month then
       calc_year := 2
      else
        calc_year := 1;
    end;
end;

function calculate_AR5 (eventID: integer):double;
var
  numberElements: integer;
begin
  numberElements := 5*24*3600 div Timestep_model;
  if 5*24*3600 > (Event_meta[eventID].Tini *Timestep_model) then                   // if data for 5 days prior to event not available...
   numberElements := Event_meta[eventID].Tini;
  calculate_AR5 := sumPartArray(RainfallSeries_fin, Event_meta[eventID].Tini-numberElements, numberElements-1);
end;

function calc_month(eventID:integer): integer;
var
  days,dayTeller: double;
  month_index: integer;
  check: boolean;
begin

 check:=false;
 month_index := first_month;
 dayTeller := numDays[first_month];
 days := Timeseries_fin[Event_meta[eventID].Tini]/(24*3600);    //= start of event expressed in days after start of simulation

 repeat
  if days > dayTeller then
   begin
    month_index := month_index+1;
    dayTeller := dayTeller + numDays[month_index];
   end
   else
    check:=true;
 until (check) or (month_index = 12);

 if (month_index = 12) AND (days > dayTeller) then
   begin
     month_index := 1;
     dayTeller := dayTeller + numDays[month_index];
     repeat
      if days > dayTeller then
        begin
          month_index := month_index+1;
          dayTeller:=dayTeller + numDays[month_index];
        end
        else
        check:=true;
     until (check);
   end;
 calc_month:= month_index;
end;

function calc_season(eventID:integer): string;
var
  days: double;
begin
 days := Timeseries_fin[Event_meta[eventID].Tini]/(24*3600);    //= start of event expressed in days after start of simulation

  case first_season of
  'spring':begin
     if days > start_summer then
       begin
         if days > start_fall then
           begin
             if days > start_winter then
               begin
                 if days > start_spring then
                   calc_season := 'spring'
                 else
                   calc_season := 'winter';
                 end
             else
               calc_season := 'fall';
             end
         else
           calc_season := 'summer';
         end
     else
       calc_season := 'spring';
     end;
  'summer':begin
     if days > start_fall then
       begin
         if days > start_winter then
           begin
             if days > start_spring then
               begin
                 if days > start_summer then
                   calc_season := 'summer'
                 else
                   calc_season := 'spring';
                 end
             else
               calc_season := 'winter';
             end
         else
           calc_season := 'fall';
         end
     else
       calc_season := 'summer';
     end;
    'fall':begin
     if days > start_winter then
       begin
         if days > start_spring then
           begin
             if days > start_summer then
               begin
                 if days > start_fall then
                   calc_season := 'fall'
                 else
                   calc_season := 'summer';
                 end
             else
               calc_season := 'spring';
             end
         else
           calc_season := 'winter';
         end
     else
       calc_season := 'fall';
     end;
    'winter':begin
     if days > start_spring then
       begin
         if days > start_summer then
           begin
             if days > start_fall then
               begin
                 if days > start_winter then
                   calc_season := 'winter'
                 else
                   calc_season := 'fall';
                 end
             else
               calc_season := 'summer';
             end
         else
           calc_season := 'spring';
         end
     else
       calc_season := 'winter';
     end
  end;
end;

procedure Calc_numOutlet;
var
  i,j: integer;
begin
  numOutlet := 1;
  if outlet_Select then
    begin
      GetGfile(outletMap, datadir+OutletFilename);
      for i := 1 to nrow do
        for j := 1 to ncol do
        begin
          if outletMap[i,j] <> 0 then
            begin
              if outletMap[i,j] > numOutlet then
                 numoutlet := outletMap[i,j];
            end;
        end;
      DisposeDynamicGdata(outletMap);
    end;
end;

procedure Calc_numVHA;
var
  i,j:integer;
begin
  numVHA := 1;
  GetGfile(RivSegMap, datadir+riversegment_filename);
  for i := 1 to nrow do
   for j := 1 to ncol do
     begin
       if RivSegMap[i,j] <> 0 then
        begin
          if RivSegMap[i,j] > numVHA then
             numVHA := RivSegMap[i,j];
        end;
      end;
  DisposeDynamicGdata(RivSegMap);
end;

procedure updateOutput(eventID: integer);
var
  i,j : integer;

begin
  updateMap(SediExport_event, SediExport_tot);         // update output maps
  updateMap(SediIn_event, SediIn_tot);
  updateMap(SediOut_event, SediOut_tot);
  updateMap(Watereros_event, Watereros_tot);
  updateMap(Watereros_kg_event, Watereros_kg_tot);
  updateMap(RUSLE_event, RUSLE_tot);

  if not simplified then
   begin
      updateMap(TotRun_event, TotRun_tot);
      updateMap(ReMap_event, ReMap_tot);

      // update TotDischarge
      for i := 0 to numOutlet-1 do
       TotDischarge_tot[i,1] := TotDischarge_tot[i,1] + TotDischarge_event[i,1];

      if Include_buffer then     // update spillover
       begin
         for i := 0 to Number_of_Buffers-1 do
           Spillover_tot[i,1] := Spillover_tot[i,1] + Spillover_event[i,1];
       end;

      if convert_output then      //output is given in minutes => use procedure updateText_min
       begin
         // update discharge
         updateText_min(Discharge_event, Discharge_tot, eventID, numOutlet+1, timestep_output);
         // update sediment concentration
         updateText_min(Sedconc_event, Sedconc_tot, eventID, numOutlet+1, timestep_output);
         // update sediment
         updateText_min(Sediment_event, Sediment_tot, eventID, numOutlet+1, Timestep_output);
         if VHA then
          begin
           // update Discharge_VHA
           updateText_min(Discharge_VHA_event, Discharge_VHA_tot, eventID, numVHA+1, Timestep_output);
           // update Sediment concentration VHA
           updateText_min(Sedconc_VHA_event, Sedconc_VHA_tot, eventID, numVHA+1, Timestep_output);
           // update Sediment_VHA
           updateText_min(Sediment_VHA_event, Sediment_VHA_tot, eventID, numVHA+1, Timestep_output);
          end;

       end
       else    // output is given in seconds => use procedure updateText_sec
       begin
          updateText_sec(Discharge_event, Discharge_tot, eventID, numOutlet+1, Timestep_model);
          updateText_sec(Sedconc_event, Sedconc_tot, eventID, numOutlet+1, Timestep_model);
          updateText_sec(Sediment_event, Sediment_tot, eventID, numOutlet+1, Timestep_model);
          if VHA then
           begin
             updateText_sec(Discharge_VHA_event, Discharge_VHA_tot, eventID, numVHA+1, Timestep_model);
             updateText_sec(Sedconc_VHA_event, Sedconc_VHA_tot, eventID, numVHA+1, Timestep_model);
             updateText_sec(Sediment_VHA_event, Sediment_VHA_tot, eventID, numVHA+1, Timestep_model);
           end;
       end;

       if include_sewer then      // update sewer_out_water
         sewer_out_water_tot := sewer_out_water_tot + sewer_out_water_event;
   end;

// update TotSediment
TotalErosion_tot := TotalErosion_tot + TotalErosion_event;
TotalDeposition_tot := TotalDeposition_tot + TotalDeposition_event;
SedleavingRiv_tot := SedleavingRiv_tot + SedleavingRiv_event;
SedLeaving_tot := SedLeaving_tot + SedLeaving_event;
SedTrapBuffer_tot := SedTrapBuffer_tot + SedTrapBuffer_event;
SedTrapWater_tot := SedTrapWater_tot + SedTrapWater_event;

for i := 0 to numOutlet-1 do
   begin
       TotSediment_tot[i,1] := TotSediment_tot[i,1] + TotSediment_event[i,1];
       TotSediment_tot[i,0] := TotSediment_event[i,0];
    end;

  if VHA then
       begin
 // update TotSedimentVHA
      for i := 0 to numVHA-1 do
       begin
         TotSedimentVHA_tot[i,1] := TotSedimentVHA_tot[i,1] + TotSedimentVHA_event[i,1];
         TotSedimentVHA_tot[i,0] := TotSedimentVHA_event[i,0];
       end;
       end;

  if include_sewer then             // update sewer_out_sediment
    sewer_out_sediment_tot := sewer_out_sediment_tot + sewer_out_sediment_event;
end;


procedure updateMap(eventMap: RRaster; var totMap: RRaster);
var
  i,j:integer;
begin
 for i := 1 to nrow do
  for j := 1 to ncol do
   begin
    totMap[i,j] := totMap[i,j]+eventMap[i,j];
   end;
end;

procedure updateText_sec(eventText: FloatArray2; var totText : FloatArray2; eventID, numColumns, timestep: integer);
var
  totLength, i, j, k, start_pos, newLength, fill, lastTime : integer;
begin
  totLength := length(totText);
  lastTime := trunc(totText[totLength-1,0]);
  if Timeseries_fin[event_meta[eventID].Tini] <= lastTime then               // indien overlap tussen oude en nieuwe file
   begin
     for i := 0 to totLength-1 do
      begin                        // startpositie bepalen
         if Timeseries_fin[event_meta[eventID].Tini] = totText[i,0] then
           start_pos := i;
      end;
     newLength := totLength + ((length(eventText)-1) - (totLength - start_pos));      // nieuwe lengte bepalen
     setLength(totText, newLength , numColumns);
    end
    else      // if no overlap between two arrays
    begin
      fill := ((Timeseries_fin[event_meta[eventID].Tini] - lastTime) div timestep)-1;
      newLength := totLength + fill + (length(eventText)-1);
      start_pos := (totLength-1) + (fill+1);
      setLength(totText, newLength, numColumns);
      for j:= 1 to numColumns-1 do                      // fill space between two series with zeros
        for i:= totLength to start_pos-1 do
          totText[i,j] := 0;
      for i := totLength to start_pos-1 do                  // complete timeseries (column 0) between two series
        totText[i,0] := totText[i-1,0]+timestep;
    end;

    // update array
    // complete timeseries (column 0)
    for i := start_pos to newLength-1 do
     totText[i,0]:=totText[i-1,0]+timestep;
    // update other columns
    for j := 1 to numColumns-1 do
      begin
       k := 1;
       for i := start_pos to newLength-1 do
        begin
         totText[i,j]:=totText[i,j]+eventText[k,j];
         k:=k+1;
         end;
       end;

     Dimension_result := newLength;     // needed in procedure WriteOutput...
end;

// the goal of this procedure is similar to the procedure above, but this one is used in case the output is given in minutes instead of seconds

procedure updateText_min(eventText: FloatArray2; var totText : FloatArray2; eventID, numColumns, timestep: integer);
var
  totLength, i, j, k, start_pos, newLength, fill, lastTime : integer;
  check:boolean;
  x:double;
begin
  totLength := length(totText);
  lastTime := trunc(totText[totLength-1,0]) * 60;      // in seconds
  if Timeseries_fin[event_meta[eventID].Tini] <= lastTime then               // indien overlap tussen oude en nieuwe file
   begin
     check:=false;
     for i := 0 to totLength-1 do
      begin                        // startpositie bepalen
         if (Timeseries_fin[event_meta[eventID].Tini] = (trunc(totText[i,0])*60)) then
          begin
            start_pos := i;
            check:=true;
          end;
      end;
     if not check then
      begin
        for i := 0 to totLength-1 do
         begin
          if (Timeseries_fin[event_meta[eventID].Tini] > (trunc(totText[i,0])*60)) AND (Timeseries_fin[event_meta[eventID].Tini] < (trunc(totText[i+1,0])*60)) then
            start_pos := i+1;
         end;
      end;

     newLength := totLength + ((length(eventText)-1) - (totLength - start_pos));      // nieuwe lengte bepalen
     setLength(totText, newLength , numColumns);
    end
    else      // if no overlap between two arrays
    begin
     if  ((Timeseries_fin[event_meta[eventID].Tini] - lastTime) mod (timestep*60) = 0) then
        fill := ((Timeseries_fin[event_meta[eventID].Tini] - lastTime) div (timestep*60))-1
      else
      begin
         x := (Timeseries_fin[event_meta[eventID].Tini] - lastTime) / (timestep*60);
         fill := trunc(x);
      end;
      newLength := totLength + fill + (length(eventText)-1);
      start_pos := (totLength-1) + (fill+1);
      setLength(totText, newLength, numColumns);
      for j:= 1 to numColumns-1 do                      // fill space between two series with zeros
        for i:= totLength to start_pos-1 do
          totText[i,j] := 0;
      for i := totLength to start_pos-1 do                  // complete timeseries (column 0) between two series
        totText[i,0] := totText[i-1,0]+timestep;
    end;

    // update array
    // complete timeseries (column 0)
    for i := start_pos to newLength-1 do
     totText[i,0]:=totText[i-1,0]+timestep;
    // update other columns
    for j := 1 to numColumns-1 do
      begin
       k := 1;
       for i := start_pos to newLength-1 do
        begin
         totText[i,j]:=totText[i,j]+eventText[k,j];
         k:=k+1;
         end;
       end;

     Dimension_result := newLength;     // needed in procedure WriteOutput...
end;


end.

