unit ReadInParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Inifiles, Math, Dialogs, RData_CN, GData_CN;

procedure Readsettings(INI_filename:string);
procedure ReadRainfallFile;
procedure ReadText(filename:string; var Z: FloatArray2; rowRain, colRain:integer);
function interp(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray):FloatArray;
function extrap(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray):FloatArray;
procedure ReadRivVelFile;
procedure ReadOutput;
procedure ReadOutputText (filename:string; var Z: FloatArray2; numColumns: integer);
procedure ActivateMemory;
procedure ReleaseMemory;
procedure ReleaseMemory2;

Type
    // buffer data
  TBufferData = record //Record containing inputdata for buffers
    Volume, Volume_dead, height_dam, height_opening, Opening_area, Qmax, Cd, area, width_dam, PTEF: double;
    row, col, ext_ID: integer;
  end;
  TBufferDataArray = array of TBufferData; //A vector is created

    // event data
  TEventData = record //Record containing metadata for each event
    Tini, Tfin, NumberOfElements, month: integer;
  end;
  TEventDataArray = array of TEventData; //A vector is created


var
  {Parameters to be read from ini-file------------------------------------------}
  {Working directories}
  datadir              : string;
  File_output_dir      : string;
  {Files}
  INIfilename          : string;
  ModelExe             : string;
  DTM_filename         : string;       {unit m}
  PARCEL_filename1     : string;       {unit id, 1 to 3200 for parcels, -1 for river, 0 for outside area, -2 for roads, -3 for forest, -4 for pasture, -5 for ponds}
  PARCEL_filename2     : string;       // parcel filename of year 2 (optional)
  PARCEL_filename      : string;       // the parcel filename which will be passed on to the CN_WS model
  Rainfallfilename     : string;
  RivVelFilename       : string;
  Sewerfilename        : string;
  CNmapSpring          : string;
  CNmapSummer          : string;
  CNmapFall            : string;
  CNmapWinter          : string;
  CNmapSpring_2        : string;
  CNmapSummer_2        : string;
  CNmapFall_2          : string;
  CNmapWinter_2        : string;
  TILDIRfilename       : string;
  Rofilename           : string;
  Bufferfilename       : string;
  K_Factor_filename    : string;
  Cf_Data_spring       : string;
  Cf_Data_summer       : string;
  Cf_Data_fall         : string;
  Cf_Data_winter       : string;
  Cf_Data_spring_2     : string;
  Cf_Data_summer_2     : string;
  Cf_Data_fall_2       : string;
  Cf_Data_winter_2     : string;
  Pf_data_Filename     : string;
  ktc_Data_Filename    : string;
  ktc_Data_Filename2   : string;
  ktc_Filename         : string;
  ktil_Data_Filename   : string;
  ktil_Data_Filename2  : string;
  ktil_Filename        : string;
  Ditch_filename       : string;
  Dam_filename         : string;
  Outletfilename       : string;
  riversegment_filename: string;
  {User Choices}
  Simplified           : boolean;
  Use_Rfactor          : boolean;
  Include_sewer        : boolean;
  Topo, Inc_tillage    : boolean;
  Include_buffer       : boolean;
  Include_ditch        : boolean;
  Include_dam          : boolean;
  Create_ktc           : boolean;
  Create_ktil          : boolean;
  est_clay             : boolean;
  Outlet_select        : boolean;
  Convert_output       : boolean;
  VHA                  : boolean;
  {Output maps}
  Write_ASPECT         : boolean;
  Write_LS             : boolean;
  Write_RE             : boolean;
  Write_RUSLE          : boolean;
  Write_Sediexport     : boolean;
  Write_SLOPE          : boolean;
  Write_TILEROS        : boolean;
  Write_TOTRUN         : boolean;
  Write_UPAREA         : boolean;
  Write_WATEREROS      : boolean;
  {Variables}
  BD                   : integer;
  First_month          : integer;
  year                 : integer;
  sewer_exit           : integer;
  alpha                : double;
  beta                 : double;
  Number_of_Buffers    : integer;
  ktc_low              : integer;
  ktc_high             : integer;
  ktc_limit            : double;
  ktil_Default         : integer;
  ktil_threshold       : double;
  clay_parent          : double;
  TFSED_crop           : integer;
  TFSED_forest         : integer;
  Timestep_model       : integer;
  EndTime_model        : integer;
  Timestep_output      : integer;
  PTefValueCropland    : integer;
  PTefValueForest      : integer;
  PTefValuePasture     : integer;
  {Buffers}
  BufferData: TBufferDataArray;
  {End Parameters to be read form ini-file--------------------------------------}

  TimeSeries, TimeSeries_fin, Timeseries_event, numDays: IntegerArray;
  RainfallSeries, RainfallSeries_fin, Rainfallseries_event, RivVel : FloatArray;
  NumberOfTimesteps, NumberOfEvents, Dimension_result, Timestep_rain : integer;
  Events : FloatArray2;
  Event_meta : TEventDataArray;
  Start_winter, Start_summer, Start_spring, Start_fall, numOutlet, numVHA: integer;
  first_season, CN_event, Cf_Data_event, CmdFilename: string;
  AR5_event, RivVel_event: double;
  outletMap, RivSegMap : GRaster;

  {Output to be read from each event}
  SediExport_event, SediIn_event, SediOut_event, TotRun_event, Watereros_event, Watereros_kg_event, ReMap_event, RUSLE_event : RRaster;
  Discharge_event, Sediment_event, Sedconc_event, Sedconc_VHA_event, Spillover_event, TotDischarge_event,
  Discharge_VHA_event, Sediment_VHA_event, TotSediment_event, TotSedimentVHA_event, clay_cont, clay_cont_VHA : FloatArray2;
  sewer_out_water_event, sewer_out_sediment_event, TotalErosion_event, TotalDeposition_event,
  SedleavingRiv_event, SedLeaving_event, SedTrapBuffer_event, SedTrapWater_event : double;

  {Output to be produced}
  SediExport_tot, SediIn_tot, SediOut_tot, TotRun_tot, Watereros_tot, Watereros_kg_tot, ReMap_tot, RUSLE_tot : RRaster;
  Discharge_tot, Sediment_tot, Sedconc_tot, Sedconc_VHA_tot, Spillover_tot, TotDischarge_tot,
  Discharge_VHA_tot, Sediment_VHA_tot, TotSediment_tot, TotSedimentVHA_tot: FloatArray2;
  Sewer_out_water_tot, sewer_out_sediment_tot, TotalErosion_tot, TotalDeposition_tot,
  SedleavingRiv_tot, SedLeaving_tot, SedTrapBuffer_tot, SedTrapWater_tot : double;

  Write_RE_tot, Write_RUSLE_tot, Write_Sediexport_tot, Write_TOTRUN_tot, Write_WATEREROS_tot : boolean;

implementation

//******************************************************************************
//This procedure reads the .ini file and assigns the file + location + values
//to the correct variables.
//******************************************************************************
procedure Readsettings(INI_filename:string);
var
  Inifile: Tinifile;
  Dummy_str, Buffername: string;
  i:integer;
begin
  Inifile:= Tinifile.create(INI_Filename);

  Datadir := Inifile.readstring('Working directories', 'Input directory', Dummy_str);
  File_output_dir := Inifile.readstring('Working directories', 'Output directory', Dummy_str);
    // If the last character in the output directory is not a "\" this is added
  if (File_output_dir[Length(File_output_dir)] <> '\') then
     File_output_dir := IncludeTrailingBackslash(File_output_dir);

  INIfilename := Inifile.Readstring('Files', '.INI filename', Dummy_str);
  ModelExe := Inifile.Readstring('Files', 'Model .exe file', Dummy_str);
  DTM_filename := Inifile.Readstring('Files', 'DTM filename', Dummy_str);
  PARCEL_filename1 := Inifile.Readstring('Files', 'Parcel filename 1', Dummy_str);
  PARCEL_filename2 := Inifile.Readstring('Files', 'Parcel filename 2', Dummy_str);
  Rainfallfilename := Inifile.Readstring('Files', 'Rainfall filename', Dummy_str);
  if not simplified then
    RivVelFilename := Inifile.Readstring('Files', 'Stream velocity filename', Dummy_str);
  Sewerfilename := Inifile.Readstring('Files', 'Sewer map filename', Dummy_str);
  TilDirfilename := Inifile.Readstring('Files', 'Tillage direction filename', Dummy_str);
  Rofilename := Inifile.Readstring('Files', 'Oriented roughness filename', Dummy_str);
  if not simplified then
    begin
      CNmapSpring := Inifile.Readstring('Files', 'CN map spring', Dummy_str);
      CNmapSpring_2 := Inifile.Readstring('Files', 'CN map spring 2', Dummy_str);
      CNmapSummer := Inifile.Readstring('Files', 'CN map summer', Dummy_str);
      CNmapSummer_2 := Inifile.Readstring('Files', 'CN map summer 2', Dummy_str);
      CNmapFall := Inifile.Readstring('Files', 'CN map fall', Dummy_str);
      CNmapFall_2 := Inifile.Readstring('Files', 'CN map fall 2', Dummy_str);
      CNmapWinter := Inifile.Readstring('Files', 'CN map winter', Dummy_str);
      CNmapWinter_2 := Inifile.Readstring('Files', 'CN map winter 2', Dummy_str);
    end;
  K_Factor_filename := inifile.readstring('Files', 'K factor filename', Dummy_str);
  Pf_Data_Filename := inifile.readstring('Files', 'P factor map filename', Dummy_str);
  ktc_Data_Filename := inifile.readstring('Files', 'ktc map filename', Dummy_str);
  ktc_Data_Filename2 := inifile.readstring('Files', 'ktc map filename 2', Dummy_str);
  ktil_Data_Filename := inifile.readstring('Files', 'ktil map filename', Dummy_str);
  ktil_Data_Filename2 := inifile.readstring('Files', 'ktil map filename 2', Dummy_str);
  Cf_Data_spring := inifile.readstring('Files', 'C factor map spring', Dummy_str);
  Cf_Data_summer := inifile.readstring('Files', 'C factor map summer', Dummy_str);
  Cf_Data_fall := inifile.readstring('Files', 'C factor map fall', Dummy_str);
  Cf_Data_winter := inifile.readstring('Files', 'C factor map winter', Dummy_str);
  Cf_Data_spring_2 := inifile.readstring('Files', 'C factor map spring 2', Dummy_str);
  Cf_Data_summer_2 := inifile.readstring('Files', 'C factor map summer 2', Dummy_str);
  Cf_Data_fall_2 := inifile.readstring('Files', 'C factor map fall 2', Dummy_str);
  Cf_Data_winter_2 := inifile.readstring('Files', 'C factor map winter 2', Dummy_str);
  BufferFilename := inifile.readstring('Files', 'Buffer map filename', Dummy_str);
  Ditch_filename := inifile.readstring('Files', 'Ditch map filename', Dummy_str);
  Dam_filename := inifile.readstring('Files', 'Dam map filename', Dummy_str);
  Outletfilename := inifile.readstring('Files', 'Outlet map filename', Dummy_str);
  Riversegment_filename := inifile.readstring('Files', 'River segment filename', Dummy_str);

  if (Inifile.ReadBool('User Choices','Simplified model version', false))=true then simplified:=true else simplified := false;
  if (Inifile.ReadBool('User Choices','Include sewers',false))=true then Include_sewer := true else Include_sewer:= false;
  if (Inifile.ReadBool('User Choices','Include tillage',false))=true then
  begin
    Inc_tillage:=true;
    topo :=false;
    end
  else
  begin
    Inc_tillage:=false;
    topo:=true;
  end;
  if (Inifile.ReadBool('User Choices','Include buffers',false))=true then Include_buffer:=true else Include_buffer:=false;
  if (Inifile.ReadBool('User Choices','Include ditches',false))=true then Include_ditch:=true else Include_ditch:=false;
  if (Inifile.ReadBool('User Choices','Include dams',false))=true then Include_dam:=true else Include_dam:=false;
  if (Inifile.ReadBool('User Choices','Create ktil map',false))=true then Create_ktil:=true else Create_ktil:=false;
  if (Inifile.ReadBool('User Choices','Create ktc map',false))=true then Create_ktc:=true else Create_ktc:=false;
  if (Inifile.ReadBool('User Choices','Estimate clay content',false))=true then est_clay:=true else est_clay:=false;
  if (Inifile.ReadBool('User Choices','Manual outlet selection',false))=true then Outlet_select:=true else Outlet_select:=false;
  if not simplified then
  begin
    if (Inifile.ReadBool('User Choices','Convert output',false))=true then Convert_output:=true else Convert_output:=false;
  end;
  if (Inifile.ReadBool('User Choices','Output per VHA river segment',false))=true then VHA:=true else VHA:=false;

  if (Inifile.ReadBool('Output maps','Write aspect',false))=true then Write_ASPECT:=true else Write_ASPECT:=false;
  if (Inifile.ReadBool('Output maps','Write LS factor',false))=true then Write_LS:=true else Write_LS:=false;
  if (Inifile.ReadBool('Output maps','Write rainfall excess',false))=true then Write_RE:=true else Write_RE:=false;
  if (Inifile.ReadBool('Output maps','Write RUSLE',false))=true then Write_RUSLE:=true else Write_RUSLE:=false;
  if (Inifile.ReadBool('Output maps','Write sediment export',false))=true then Write_Sediexport:=true else Write_Sediexport:=false;
  if (Inifile.ReadBool('Output maps','Write slope',false))=true then Write_SLOPE:=true else Write_SLOPE:=false;
  if (Inifile.ReadBool('Output maps','Write tillage erosion',false))=true then Write_TILEROS:=true else Write_TILEROS:=false;
  if (Inifile.ReadBool('Output maps','Write total runoff',false))=true then Write_TOTRUN:=true else Write_TOTRUN:=false;
  if (Inifile.ReadBool('Output maps','Write upstream area',false))=true then Write_UPAREA:=true else Write_UPAREA:=false;
  if (Inifile.ReadBool('Output maps','Write water erosion',false))=true then Write_WATEREROS:=true else Write_WATEREROS:=false;

  BD := StrToInt(Inifile.Readstring('Variables', 'Bulk density', Dummy_str));
  if Include_sewer then
    sewer_exit := StrToInt(Inifile.Readstring('Variables', 'Sewer exit', Dummy_str));
  First_month := StrToInt(Inifile.Readstring('Variables', 'First month', Dummy_str));
  year := StrToInt(Inifile.Readstring('Variables', 'Starting year', Dummy_str));
  if not simplified then
  begin
    alpha := StrToFloat(Inifile.Readstring('Variables', 'Alpha', Dummy_str));
    beta := StrToFloat(Inifile.Readstring('Variables', 'Beta', Dummy_str));
  end;
  Number_of_Buffers := StrToInt(inifile.readstring('Variables', 'Number of buffers', Dummy_str));
  if Create_ktc then
  begin
       ktc_low := StrToInt(Inifile.Readstring('Variables', 'ktc low', Dummy_str));
       ktc_high := StrToInt(Inifile.Readstring('Variables', 'ktc high', Dummy_str));
       ktc_limit := StrToFloat(Inifile.Readstring('Variables', 'ktc limit', Dummy_str));
  end;
  if Create_ktil then
  begin
    ktil_Default := StrToInt(Inifile.Readstring('Variables', 'ktil default', Dummy_str));
    ktil_threshold := StrToFloat(Inifile.Readstring('Variables', 'ktil threshold', Dummy_str));
  end;
  if est_clay then
    clay_parent := StrToFloat(Inifile.Readstring('Variables', 'Clay content parent material', Dummy_str));
  TFSED_crop := StrToInt(Inifile.Readstring('Variables', 'Parcel connectivity cropland', Dummy_str));
  TFSED_forest := StrToInt(Inifile.Readstring('Variables', 'Parcel connectivity forest', Dummy_str));
  PTEFValueCropland :=StrToInt(Inifile.ReadString ('Variables','Parcel trapping efficiency cropland',Dummy_str));
  PTEFValueForest :=StrToInt(Inifile.ReadString ('Variables','Parcel trapping efficiency forest',Dummy_str));
  PTEFValuePasture :=StrToInt(Inifile.ReadString ('Variables','Parcel trapping efficiency pasture',Dummy_str));

  if not simplified then
  begin
    Timestep_model := StrToInt(inifile.readstring('Variables', 'Desired timestep for model', Dummy_str));
    Endtime_model := StrToInt(inifile.readstring('Variables', 'Endtime model', Dummy_str));
    if Convert_output then
      Timestep_output := StrToInt(inifile.readstring('Variables', 'Final timestep output', Dummy_str));
  end
  else
  begin
    Timestep_model := 0;
    Endtime_model := 0;
    Timestep_output := 0;
  end;

  if Include_buffer then
  begin
    setlength(Bufferdata, Number_of_Buffers + 1);
    for i := 1 to Number_of_Buffers do
    begin
      Buffername := 'Buffer ' + IntToStr(i);
      Bufferdata[i].Volume := StrToFloat(inifile.readstring(Buffername, 'Volume', Dummy_str));
      Bufferdata[i].Height_dam := StrToFloat(inifile.readstring(Buffername, 'Height dam', Dummy_str));
      Bufferdata[i].Height_opening := StrToFloat(inifile.readstring(Buffername, 'Height opening', Dummy_str));
      Bufferdata[i].Opening_area := StrToFloat(inifile.readstring(Buffername, 'Opening area', Dummy_str));
      Bufferdata[i].Cd := StrToFloat(inifile.readstring(Buffername, 'Discharge coefficient', Dummy_str));
      Bufferdata[i].width_dam := StrToFloat(inifile.readstring(Buffername, 'Width dam', Dummy_str));
      Bufferdata[i].PTEF := StrToFloat(inifile.readstring(Buffername, 'Trapping efficiency', Dummy_str));
      Bufferdata[i].ext_ID := StrToInt(inifile.readstring(Buffername, 'Extension ID', Dummy_str));
      if Bufferdata[i].Height_opening > Bufferdata[i].Height_dam then
        begin
        showmessage('Error in buffer input: the height of the opening cannot be larger than the height of the dam. Please insert correct vallues.');
        Exit;
        end;
    end;
  end;

  Inifile.Destroy;
end;


//******************************************************************************
//In this procedure the rainfall file is read (and if necessary, converted to
//a different timestep). This is a .txt file (tab delimited) with the timestep
//(min) in column 1 and the amount of rainfall (mm) in column 2.
//******************************************************************************
Procedure ReadRainfallFile;
var
teller, i, j, nr: integer;
datafile: textfile;
a: string;
Z:FloatArray2;
minTimeSeries : integerArray;
Cumul_rain, Cumul_interp, minRainfallSeries : FloatArray;
x : double;

begin
teller := 0;
assignfile(datafile, RainfallFilename);
reset(datafile);
while not eof(datafile) do
begin //To determine the number of time steps and number of rows in the file
 inc(teller);
 Readln(datafile, a);
 end;
Setlength(TimeSeries, teller); //Time in minutes
Setlength(Rainfallseries, teller); //Regenval in mm (per timestep)
NumberOfTimesteps := teller-1;

ReadText(RainfallFilename,Z,teller,2);   // see procedure below

for
  i:=0 to NumberOfTimesteps do
  begin
   TimeSeries[i]:=Trunc(Z[i,0]);    //Array containing the time steps (min)
   RainfallSeries[i]:=Z[i,1];     //Array with the amount of rainfall (mm) per time step
   end;

for i := 0 to NumberOfTimesteps do         // convert time min => sec
    begin
      TimeSeries[i] := TimeSeries[i]*60;
    end;

Timestep_rain := Timeseries[1]-TimeSeries[0];

if (Simplified) then   // convert to 10 min rainfallseries
begin
  Timestep_model := 600;
  if Timestep_rain = 600 then    //(=10 min)
     begin
     TimeSeries_fin := TimeSeries;
     RainfallSeries_fin := RainfallSeries;
     end
  else
  begin
    if Timestep_rain > 60 then     // eerst omzetten naar tijdstap van 1 min (makkelijker)
      begin
        nr := (length(TimeSeries)-1)*(TimeStep_rain div 60);
        setlength(minTimeSeries, nr+1);
        setlength(minRainfallSeries, nr+1);
        minTimeSeries[0]:=TimeSeries[0];
        for i := 1 to nr do
          minTimeSeries[i]:=minTimeSeries[i-1]+60;
        minRainfallSeries[0]:=RainfallSeries[0];
        j:=1;
        for i := 1 to (length(TimeSeries)-1) do
          begin
            x := RainfallSeries[i]/(Timestep_rain div 60);
            while j <= i*(Timestep_rain div 60) do
            begin
              minRainfallSeries[j]:=x;
              Inc(j);
            end;
          end;
      end
      else if Timestep_rain < 60 then
        begin
           nr := (length(TimeSeries)-1)div(60 div TimeStep_rain);
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
    setlength(TimeSeries_fin, nr);
    setlength(RainfallSeries_fin, nr);
    TimeSeries_fin[0]:=TimeSeries[0];
      for i := 1 to nr do
        begin
          TimeSeries_fin[i]:=TimeSeries_fin[i-1]+600;
        end;
      RainfallSeries_fin:= extrap(minTimeSeries, TimeSeries_fin, minRainfallSeries);
  end;

end

else    // interpolation of rainfall data to desired model timestep
begin
   Setlength(Cumul_rain, NumberOfTimesteps+1);    // calculate cumulative rainfall series
   Cumul_rain[0] := RainfallSeries[0];
   for i := 1 to NumberOfTimesteps do
     begin
        Cumul_rain[i] := Cumul_rain[i-1] + RainfallSeries[i];
     end;

   // create new timeseries array
   NumberOfTimesteps := NumberOfTimesteps*(Timestep_rain div Timestep_model);
   Setlength(TimeSeries_fin, NumberOfTimesteps+1);
   TimeSeries_fin[0] := TimeSeries[0];
   for i := 1 to NumberOfTimesteps do
     begin
     TimeSeries_fin[i] := TimeSeries_fin[i-1] + Timestep_model;
     end;

   // interpolation of cumulative rainfall series, see function below
   Cumul_Interp := interp(TimeSeries, TimeSeries_fin, Cumul_rain);

   // cumulative rainfall series is converted to new rainfall series
   Setlength(RainfallSeries_fin, NumberOfTimesteps+1);
   RainfallSeries_fin[0]:=Cumul_Interp[0];
   for i := 1 to NumberOfTimesteps do
     begin
        RainfallSeries_fin[i]:= Cumul_Interp[i]-Cumul_Interp[i-1];
     end;
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


procedure ReadRivVelFile;        // file containing average stream velocity in m/s for each month
                                 // from January till December
var
i: integer;
datafile: textfile;

begin
setlength(RivVel,13);
assignfile(datafile, RivVelFilename);
reset (datafile);
for i:=1 to 12 do
 Read(datafile,RivVel[i]);
closefile(datafile);
end;

procedure ReadOutput;
var
 sewerfile, totsedfile: textfile;
 b, totsedfilename: string;
 lengthstring, teller,i,j: integer;
begin

  SetCurrentDir(File_output_dir);

// read output maps
  GetRfile(SediExport_event, File_output_dir + 'SediExport_kg.rst');
  GetRfile(SediIn_event, File_output_dir + 'SediIn_kg.rst');
  GetRfile(SediOut_event, File_output_dir + 'SediOut_kg.rst');
  GetRfile(Watereros_event, File_output_dir + 'WATEREROS (mm per gridcel).rst');
  GetRfile(Watereros_kg_event, File_output_dir + 'WATEREROS (kg per gridcel).rst');
  GetRfile(RUSLE_event, File_output_dir + 'RUSLE.rst');
  if not simplified then
  begin
    GetRfile(ReMap_event, File_output_dir + 'Remap.rst');
    GetRfile(TotRun_event, File_output_dir + 'Total runoff.rst');
  end;

// read output text files
if not simplified then
begin
  ReadOutputText(File_output_dir+'Discharge.txt', Discharge_event, numOutlet+1);
  if VHA then
    ReadOutputText(File_output_dir+'Discharge_VHA.txt', Discharge_VHA_event, numVHA+1);
  ReadOutputText(File_output_dir+'Sediment.txt', Sediment_event, numOutlet+1);
  if VHA then
    ReadOutputText(File_output_dir+'Sediment_VHA.txt', Sediment_VHA_event, numVHA+1);
  ReadOutputText(File_output_dir+'Sediment concentration.txt', Sedconc_event, numOutlet+1);
  if VHA then
    ReadOutputText(File_output_dir+'Sediment concentration_VHA.txt', Sedconc_VHA_event, numVHA+1);
  if Include_buffer then
    ReadOutputText(File_output_dir+'Spillover per buffer.txt', Spillover_event, 2);
  ReadOutputText(File_output_dir+'Total discharge.txt', TotDischarge_event, 2);
  if est_clay then
  begin
    ReadOutputText(File_output_dir+'Clay content sediment.txt', clay_cont, 2);
    if VHA then
      ReadOutputText(File_output_dir+'Clay content sediment VHA.txt', clay_cont_VHA, 2);
  end;
  if include_sewer then
  begin
    assignfile(sewerfile, File_output_dir+'Sewer output water.txt');
    reset(sewerfile);
    readln(sewerfile,b); // skip file title
    Read(sewerfile,sewer_out_water_event);
    closefile(sewerfile);
  end;
end;

//ReadOutputText(File_output_dir+'Total sediment.txt', TotSediment_event, 2);

// Read total sediment.txt
totsedfilename := File_output_dir+'Total sediment.txt';
assignfile(totsedfile, totsedfilename);
reset(totsedfile);
teller:=0;
while not eof(totsedfile) do
 begin //To determine the number of rows in the file
  inc(teller);
  Readln(totsedfile, b);
  end;
teller:=teller-10;
Setlength(TotSediment_event, teller, 2);
  for i := 0 to teller-1 do
   for j := 0 to 1 do
   begin
     TotSediment_event[i,j]:=0;
   end;
closefile(totsedfile);
assignfile(totsedfile,totsedfilename);
reset (totsedfile);
readln(totsedfile,b); // total erosion
delete(b,1,15);
lengthstring := length(b);
delete(b,lengthstring-4,5);
TotalErosion_event:=StrToFloat(b);
readln(totsedfile,b); // total deposition
delete(b,1,18);
lengthstring := length(b);
delete(b,lengthstring-4,5);
TotalDeposition_event:=StrToFloat(b);
readln(totsedfile,b); // Sediment leaving catchment, through river
delete(b,1,47);
lengthstring := length(b);
delete(b,lengthstring-4,5);
SedleavingRiv_event:=StrToFloat(b);
readln(totsedfile,b); // Sediment leaving catchment, not through river
delete(b,1,51);
lengthstring := length(b);
delete(b,lengthstring-4,5);
SedLeaving_event:=StrToFloat(b);
readln(totsedfile,b); // sediment trapped in buffers
delete(b,1,29);
lengthstring := length(b);
delete(b,lengthstring-4,5);
SedTrapBuffer_event:=StrToFloat(b);
readln(totsedfile,b); // sediment trapped in open water
delete(b,1,32);
lengthstring := length(b);
delete(b,lengthstring-4,5);
SedTrapWater_event:=StrToFloat(b);
readln(totsedfile,b); // space
readln(totsedfile,b); // space
readln(totsedfile,b); // skip title
readln(totsedfile,b); // skip column headings
for i:=0 to teller-1 do
    for j:=0 to 1 do
    begin
      Read(totsedfile,TotSediment_event[i,j]);
    end;
closefile(totsedfile);

// read Total sediment VHA.txt
if VHA then
  ReadOutputText(File_output_dir+'Total sediment VHA.txt', TotSedimentVHA_event, 2);

if include_sewer then
  begin
    assignfile(sewerfile, File_output_dir+'Sewer output sediment.txt');
    reset(sewerfile);
    readln(sewerfile,b); // skip file title
    Read(sewerfile,sewer_out_sediment_event);
    closefile(sewerfile);
  end;

end;

procedure ReadOutputText (filename:string; var Z: FloatArray2; numColumns: integer);
var
  teller, i, j: integer;
  datafile: textfile;
  temp: floatArray2;
  a: string;
begin
  teller := 0;
  assignfile(datafile, filename);
  reset(datafile);
  readln(datafile,a);     // skip file title
  readln(datafile,a);     // skip column headings
  while not eof(datafile) do
  begin //To determine the number of time steps and number of rows in the file
   inc(teller);
   Readln(datafile, a);
   end;
  Setlength(Z, teller, numColumns);
  for i := 0 to teller-1 do
   for j := 0 to numColumns-1 do
   begin
     Z[i,j]:=0;
   end;
  closefile(datafile);
  assignfile(datafile,filename);
  reset (datafile);
  readln(datafile,a);
  readln(datafile,a);
  for i:=0 to teller-1 do
    for j:=0 to numColumns-1 do
    begin
      Read(datafile,Z[i,j]);
    end;
  closefile(datafile);
end;

procedure ActivateMemory;
var
  i, j: integer;
begin
   SetDynamicRData(SediExport_tot);
   SetzeroR(SediExport_tot);
   SetDynamicRData(SediIn_tot);
   SetzeroR(SediIn_tot);
   SetDynamicRData(SediOut_tot);
   SetzeroR(SediOut_tot);
   SetDynamicRData(Watereros_tot);
   SetzeroR(Watereros_tot);
   SetDynamicRData(Watereros_kg_tot);
   SetzeroR(Watereros_kg_tot);
   SetDynamicRData(RUSLE_tot);
   SetzeroR(RUSLE_tot);

   if not Simplified then
   begin
      SetDynamicRData(TotRun_tot);
      SetzeroR(TotRun_tot);
      SetDynamicRData(ReMap_tot);
      SetzeroR(ReMap_tot);

       SetLength(Discharge_tot,1,numOutlet+1);
       for j:=0 to numOutlet do
         Discharge_tot[0,j]:=0;
       setLength(Sedconc_tot, 1, numOutlet+1);
        for j:=0 to numOutlet do
         Sedconc_tot[0,j]:=0;
       SetLength(Sediment_tot,1,numOutlet+1);
       for j:=0 to numOutlet do
         Sediment_tot[0,j]:=0;
       if Include_buffer then
       begin
         SetLength(Spillover_tot,Number_of_Buffers,2);
         for i:=0 to Number_of_Buffers-1 do
           Spillover_tot[i,0] := Spillover_event[i,0];
       end;
       SetLength(TotDischarge_tot,numOutlet,2);
       for i := 0 to numOutlet-1 do
         TotDischarge_tot[i,0] := TotDischarge_event[i,0];
       if VHA then
       begin
         SetLength(Discharge_VHA_tot,1,numVHA+1);
         for j:=0 to numVHA do
           Discharge_VHA_tot[0,j]:=0;
         setLength(Sedconc_VHA_tot, 1, numVHA+1);
        for j:=0 to numVHA do
         Sedconc_VHA_tot[0,j]:=0;
        setLength(Sediment_VHA_tot, 1, numVHA+1);
        for j:=0 to numVHA do
         Sediment_VHA_tot[0,j]:=0;
       end;
       if include_sewer then
         Sewer_out_water_tot:=0;
   end;

   SetLength(TotSediment_tot,numOutlet,2);
   if VHA then
     SetLength(TotSedimentVHA_tot,numVHA,2);

   if include_sewer then
     sewer_out_sediment_tot:=0;
end;

procedure ReleaseMemory;
begin
  DisposeDynamicRdata(SediExport_event);
  DisposeDynamicRdata(SediIn_event);
  DisposeDynamicRdata(SediOut_event);
  DisposeDynamicRdata(Watereros_event);
  DisposeDynamicRdata(Watereros_kg_event);
  DisposeDynamicRdata(RUSLE_event);
  if not simplified then
  begin
    DisposeDynamicRdata(TotRun_event);
    DisposeDynamicRdata(ReMap_event);
  end;
end;

procedure ReleaseMemory2;
begin
 DisposeDynamicRdata(SediExport_tot);
 DisposeDynamicRdata(SediIn_tot);
 DisposeDynamicRdata(SediOut_tot);
 DisposeDynamicRdata(Watereros_tot);
 DisposeDynamicRdata(Watereros_kg_tot);
 DisposeDynamicRdata(RUSLE_tot);
 if not simplified then
 begin
   DisposeDynamicRdata(TotRun_tot);
   DisposeDynamicRdata(ReMap_tot);
 end;
end;


end.

