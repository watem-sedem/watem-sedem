
Unit ReadInParameters;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, FileUtil, Inifiles, Math, Dialogs, RData_CN, GData_CN;

Procedure Readsettings(INI_filename:String);
Procedure ReadRainfallFile;
Procedure ReadText(filename:String; Var Z: FloatArray2; rowRain, colRain:integer);
Function interp(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray): FloatArray;
Function extrap(xOriginal:IntegerArray; xNew:IntegerArray; yOriginal:FloatArray): FloatArray;
Procedure ReadRivVelFile;
Procedure ReadOutput;
Procedure ReadOutputText (filename:String; Var Z: FloatArray2; numColumns: integer);
Procedure ActivateMemory;
Procedure ReleaseMemory;
Procedure ReleaseMemory2;

Type 
  // buffer data
  TBufferData = Record
    //Record containing inputdata for buffers
    Volume, Volume_dead, height_dam, height_opening, Opening_area, Qmax, Cd, area, width_dam, PTEF:
                                                                                              double
    ;
    row, col, ext_ID: integer;
  End;
  TBufferDataArray = array Of TBufferData;
  //A vector is created

  // event data
  TEventData = Record
    //Record containing metadata for each event
    Tini, Tfin, NumberOfElements, month: integer;
  End;
  TEventDataArray = array Of TEventData;
  //A vector is created


Var 
  {Parameters to be read from ini-file------------------------------------------}
  {Working directories}
  datadir              : string;
  File_output_dir      : string;
  {Files}
  INIfilename          : string;
  ModelExe             : string;
  DTM_filename         : string;       {unit m}
  PARCEL_filename1     : string;
{unit id, 1 to 3200 for parcels, -1 for river, 0 for outside area, -2 for roads, -3 for forest, -4 for pasture, -5 for ponds}
  PARCEL_filename2     : string;
  // parcel filename of year 2 (optional)
  PARCEL_filename      : string;
  // the parcel filename which will be passed on to the CN_WS model
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
  SediExport_event, SediIn_event, SediOut_event, TotRun_event, Watereros_event, Watereros_kg_event,
  ReMap_event, RUSLE_event : RRaster;
  Discharge_event, Sediment_event, Sedconc_event, Sedconc_VHA_event, Spillover_event,
  TotDischarge_event,
  Discharge_VHA_event, Sediment_VHA_event, TotSediment_event, TotSedimentVHA_event, clay_cont,
  clay_cont_VHA : FloatArray2;
  sewer_out_water_event, sewer_out_sediment_event, TotalErosion_event, TotalDeposition_event,
  SedleavingRiv_event, SedLeaving_event, SedTrapBuffer_event, SedTrapWater_event : double;

  {Output to be produced}
  SediExport_tot, SediIn_tot, SediOut_tot, TotRun_tot, Watereros_tot, Watereros_kg_tot, ReMap_tot,
  RUSLE_tot : RRaster;
  Discharge_tot, Sediment_tot, Sedconc_tot, Sedconc_VHA_tot, Spillover_tot, TotDischarge_tot,
  Discharge_VHA_tot, Sediment_VHA_tot, TotSediment_tot, TotSedimentVHA_tot: FloatArray2;
  Sewer_out_water_tot, sewer_out_sediment_tot, TotalErosion_tot, TotalDeposition_tot,
  SedleavingRiv_tot, SedLeaving_tot, SedTrapBuffer_tot, SedTrapWater_tot : double;

  Write_RE_tot, Write_RUSLE_tot, Write_Sediexport_tot, Write_TOTRUN_tot, Write_WATEREROS_tot :
                                                                                             boolean
  ;

Implementation

//******************************************************************************
//This procedure reads the .ini file and assigns the file + location + values
//to the correct variables.
//******************************************************************************
Procedure Readsettings(INI_filename:String);

Var 
  Inifile: Tinifile;
  Dummy_str, Buffername: string;
  i: integer;
Begin
  Inifile := Tinifile.create(INI_Filename);

  Datadir := Inifile.readstring('Working directories', 'Input directory', Dummy_str);
  File_output_dir := Inifile.readstring('Working directories', 'Output directory', Dummy_str);
  // If the last character in the output directory is not a "\" this is added
  If (File_output_dir[Length(File_output_dir)] <> '\') Then
    File_output_dir := IncludeTrailingBackslash(File_output_dir);

  INIfilename := Inifile.Readstring('Files', '.INI filename', Dummy_str);
  ModelExe := Inifile.Readstring('Files', 'Model .exe file', Dummy_str);
  DTM_filename := Inifile.Readstring('Files', 'DTM filename', Dummy_str);
  PARCEL_filename1 := Inifile.Readstring('Files', 'Parcel filename 1', Dummy_str);
  PARCEL_filename2 := Inifile.Readstring('Files', 'Parcel filename 2', Dummy_str);
  Rainfallfilename := Inifile.Readstring('Files', 'Rainfall filename', Dummy_str);
  If Not simplified Then
    RivVelFilename := Inifile.Readstring('Files', 'Stream velocity filename', Dummy_str);
  Sewerfilename := Inifile.Readstring('Files', 'Sewer map filename', Dummy_str);
  TilDirfilename := Inifile.Readstring('Files', 'Tillage direction filename', Dummy_str);
  Rofilename := Inifile.Readstring('Files', 'Oriented roughness filename', Dummy_str);
  If Not simplified Then
    Begin
      CNmapSpring := Inifile.Readstring('Files', 'CN map spring', Dummy_str);
      CNmapSpring_2 := Inifile.Readstring('Files', 'CN map spring 2', Dummy_str);
      CNmapSummer := Inifile.Readstring('Files', 'CN map summer', Dummy_str);
      CNmapSummer_2 := Inifile.Readstring('Files', 'CN map summer 2', Dummy_str);
      CNmapFall := Inifile.Readstring('Files', 'CN map fall', Dummy_str);
      CNmapFall_2 := Inifile.Readstring('Files', 'CN map fall 2', Dummy_str);
      CNmapWinter := Inifile.Readstring('Files', 'CN map winter', Dummy_str);
      CNmapWinter_2 := Inifile.Readstring('Files', 'CN map winter 2', Dummy_str);
    End;
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

  If (Inifile.ReadBool('User Choices','Simplified model version', false))=true Then simplified := 
                                                                                                true
  Else simplified := false;
  If (Inifile.ReadBool('User Choices','Include sewers',false))=true Then Include_sewer := true
  Else Include_sewer := false;
  If (Inifile.ReadBool('User Choices','Include tillage',false))=true Then
    Begin
      Inc_tillage := true;
      topo := false;
    End
  Else
    Begin
      Inc_tillage := false;
      topo := true;
    End;
  If (Inifile.ReadBool('User Choices','Include buffers',false))=true Then Include_buffer := true
  Else Include_buffer := false;
  If (Inifile.ReadBool('User Choices','Include ditches',false))=true Then Include_ditch := true
  Else Include_ditch := false;
  If (Inifile.ReadBool('User Choices','Include dams',false))=true Then Include_dam := true
  Else Include_dam := false;
  If (Inifile.ReadBool('User Choices','Create ktil map',false))=true Then Create_ktil := true
  Else Create_ktil := false;
  If (Inifile.ReadBool('User Choices','Create ktc map',false))=true Then Create_ktc := true
  Else Create_ktc := false;
  If (Inifile.ReadBool('User Choices','Estimate clay content',false))=true Then est_clay := true
  Else est_clay := false;
  If (Inifile.ReadBool('User Choices','Manual outlet selection',false))=true Then Outlet_select := 
                                                                                                true
  Else Outlet_select := false;
  If Not simplified Then
    Begin
      If (Inifile.ReadBool('User Choices','Convert output',false))=true Then Convert_output := true
      Else Convert_output := false;
    End;
  If (Inifile.ReadBool('User Choices','Output per VHA river segment',false))=true Then VHA := true
  Else VHA := false;

  If (Inifile.ReadBool('Output maps','Write aspect',false))=true Then Write_ASPECT := true
  Else Write_ASPECT := false;
  If (Inifile.ReadBool('Output maps','Write LS factor',false))=true Then Write_LS := true
  Else Write_LS := false;
  If (Inifile.ReadBool('Output maps','Write rainfall excess',false))=true Then Write_RE := true
  Else Write_RE := false;
  If (Inifile.ReadBool('Output maps','Write RUSLE',false))=true Then Write_RUSLE := true
  Else Write_RUSLE := false;
  If (Inifile.ReadBool('Output maps','Write sediment export',false))=true Then Write_Sediexport := 
                                                                                                true
  Else Write_Sediexport := false;
  If (Inifile.ReadBool('Output maps','Write slope',false))=true Then Write_SLOPE := true
  Else Write_SLOPE := false;
  If (Inifile.ReadBool('Output maps','Write tillage erosion',false))=true Then Write_TILEROS := true
  Else Write_TILEROS := false;
  If (Inifile.ReadBool('Output maps','Write total runoff',false))=true Then Write_TOTRUN := true
  Else Write_TOTRUN := false;
  If (Inifile.ReadBool('Output maps','Write upstream area',false))=true Then Write_UPAREA := true
  Else Write_UPAREA := false;
  If (Inifile.ReadBool('Output maps','Write water erosion',false))=true Then Write_WATEREROS := true
  Else Write_WATEREROS := false;

  BD := StrToInt(Inifile.Readstring('Variables', 'Bulk density', Dummy_str));
  If Include_sewer Then
    sewer_exit := StrToInt(Inifile.Readstring('Variables', 'Sewer exit', Dummy_str));
  First_month := StrToInt(Inifile.Readstring('Variables', 'First month', Dummy_str));
  year := StrToInt(Inifile.Readstring('Variables', 'Starting year', Dummy_str));
  If Not simplified Then
    Begin
      alpha := StrToFloat(Inifile.Readstring('Variables', 'Alpha', Dummy_str));
      beta := StrToFloat(Inifile.Readstring('Variables', 'Beta', Dummy_str));
    End;
  Number_of_Buffers := StrToInt(inifile.readstring('Variables', 'Number of buffers', Dummy_str));
  If Create_ktc Then
    Begin
      ktc_low := StrToInt(Inifile.Readstring('Variables', 'ktc low', Dummy_str));
      ktc_high := StrToInt(Inifile.Readstring('Variables', 'ktc high', Dummy_str));
      ktc_limit := StrToFloat(Inifile.Readstring('Variables', 'ktc limit', Dummy_str));
    End;
  If Create_ktil Then
    Begin
      ktil_Default := StrToInt(Inifile.Readstring('Variables', 'ktil default', Dummy_str));
      ktil_threshold := StrToFloat(Inifile.Readstring('Variables', 'ktil threshold', Dummy_str));
    End;
  If est_clay Then
    clay_parent := StrToFloat(Inifile.Readstring('Variables', 'Clay content parent material',
                   Dummy_str));
  TFSED_crop := StrToInt(Inifile.Readstring('Variables', 'Parcel connectivity cropland', Dummy_str))
  ;
  TFSED_forest := StrToInt(Inifile.Readstring('Variables', 'Parcel connectivity forest', Dummy_str))
  ;
  PTEFValueCropland := StrToInt(Inifile.ReadString ('Variables',
                       'Parcel trapping efficiency cropland',Dummy_str));
  PTEFValueForest := StrToInt(Inifile.ReadString ('Variables','Parcel trapping efficiency forest',
                     Dummy_str));
  PTEFValuePasture := StrToInt(Inifile.ReadString ('Variables','Parcel trapping efficiency pasture',
                      Dummy_str));

  If Not simplified Then
    Begin
      Timestep_model := StrToInt(inifile.readstring('Variables', 'Desired timestep for model',
                        Dummy_str));
      Endtime_model := StrToInt(inifile.readstring('Variables', 'Endtime model', Dummy_str));
      If Convert_output Then
        Timestep_output := StrToInt(inifile.readstring('Variables', 'Final timestep output',
                           Dummy_str));
    End
  Else
    Begin
      Timestep_model := 0;
      Endtime_model := 0;
      Timestep_output := 0;
    End;

  If Include_buffer Then
    Begin
      setlength(Bufferdata, Number_of_Buffers + 1);
      For i := 1 To Number_of_Buffers Do
        Begin
          Buffername := 'Buffer ' + IntToStr(i);
          Bufferdata[i].Volume := StrToFloat(inifile.readstring(Buffername, 'Volume', Dummy_str));
          Bufferdata[i].Height_dam := StrToFloat(inifile.readstring(Buffername, 'Height dam',
                                      Dummy_str));
          Bufferdata[i].Height_opening := StrToFloat(inifile.readstring(Buffername, 'Height opening'
                                          , Dummy_str));
          Bufferdata[i].Opening_area := StrToFloat(inifile.readstring(Buffername, 'Opening area',
                                        Dummy_str));
          Bufferdata[i].Cd := StrToFloat(inifile.readstring(Buffername, 'Discharge coefficient',
                              Dummy_str));
          Bufferdata[i].width_dam := StrToFloat(inifile.readstring(Buffername, 'Width dam',
                                     Dummy_str));
          Bufferdata[i].PTEF := StrToFloat(inifile.readstring(Buffername, 'Trapping efficiency',
                                Dummy_str));
          Bufferdata[i].ext_ID := StrToInt(inifile.readstring(Buffername, 'Extension ID', Dummy_str)
                                  );
          If Bufferdata[i].Height_opening > Bufferdata[i].Height_dam Then
            Begin
              showmessage(
'Error in buffer input: the height of the opening cannot be larger than the height of the dam. Please insert correct vallues.'
              );
              Exit;
            End;
        End;
    End;

  Inifile.Destroy;
End;


//******************************************************************************
//In this procedure the rainfall file is read (and if necessary, converted to
//a different timestep). This is a .txt file (tab delimited) with the timestep
//(min) in column 1 and the amount of rainfall (mm) in column 2.
//******************************************************************************
Procedure ReadRainfallFile;

Var 
  teller, i, j, nr: integer;
  datafile: textfile;
  a: string;
  Z: FloatArray2;
  minTimeSeries : integerArray;
  Cumul_rain, Cumul_interp, minRainfallSeries : FloatArray;
  x : double;

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
  //Time in minutes
  Setlength(Rainfallseries, teller);
  //Regenval in mm (per timestep)
  NumberOfTimesteps := teller-1;

  ReadText(RainfallFilename,Z,teller,2);
  // see procedure below

  For 
      i:=0 To NumberOfTimesteps Do
    Begin
      TimeSeries[i] := Trunc(Z[i,0]);
      //Array containing the time steps (min)
      RainfallSeries[i] := Z[i,1];
      //Array with the amount of rainfall (mm) per time step
    End;

  For i := 0 To NumberOfTimesteps Do
    // convert time min => sec
    Begin
      TimeSeries[i] := TimeSeries[i]*60;
    End;

  Timestep_rain := Timeseries[1]-TimeSeries[0];

  If (Simplified) Then   // convert to 10 min rainfallseries
    Begin
      Timestep_model := 600;
      If Timestep_rain = 600 Then    //(=10 min)
        Begin
          TimeSeries_fin := TimeSeries;
          RainfallSeries_fin := RainfallSeries;
        End
      Else
        Begin
          If Timestep_rain > 60 Then     // eerst omzetten naar tijdstap van 1 min (makkelijker)
            Begin
              nr := (length(TimeSeries)-1)*(TimeStep_rain Div 60);
              setlength(minTimeSeries, nr+1);
              setlength(minRainfallSeries, nr+1);
              minTimeSeries[0] := TimeSeries[0];
              For i := 1 To nr Do
                minTimeSeries[i] := minTimeSeries[i-1]+60;
              minRainfallSeries[0] := RainfallSeries[0];
              j := 1;
              For i := 1 To (length(TimeSeries)-1) Do
                Begin
                  x := RainfallSeries[i]/(Timestep_rain Div 60);
                  While j <= i*(Timestep_rain Div 60) Do
                    Begin
                      minRainfallSeries[j] := x;
                      Inc(j);
                    End;
                End;
            End
          Else If Timestep_rain < 60 Then
                 Begin
                   nr := (length(TimeSeries)-1)Div(60 Div TimeStep_rain);
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
          setlength(TimeSeries_fin, nr);
          setlength(RainfallSeries_fin, nr);
          TimeSeries_fin[0] := TimeSeries[0];
          For i := 1 To nr Do
            Begin
              TimeSeries_fin[i] := TimeSeries_fin[i-1]+600;
            End;
          RainfallSeries_fin := extrap(minTimeSeries, TimeSeries_fin, minRainfallSeries);
        End;

    End

  Else    // interpolation of rainfall data to desired model timestep
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
      Setlength(TimeSeries_fin, NumberOfTimesteps+1);
      TimeSeries_fin[0] := TimeSeries[0];
      For i := 1 To NumberOfTimesteps Do
        Begin
          TimeSeries_fin[i] := TimeSeries_fin[i-1] + Timestep_model;
        End;

      // interpolation of cumulative rainfall series, see function below
      Cumul_Interp := interp(TimeSeries, TimeSeries_fin, Cumul_rain);

      // cumulative rainfall series is converted to new rainfall series
      Setlength(RainfallSeries_fin, NumberOfTimesteps+1);
      RainfallSeries_fin[0] := Cumul_Interp[0];
      For i := 1 To NumberOfTimesteps Do
        Begin
          RainfallSeries_fin[i] := Cumul_Interp[i]-Cumul_Interp[i-1];
        End;
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
  x0 := xNew[k-1];
  y0 := interp[k-1];
  x1 := xNew[k+step-1];
  y1 := interp[k+step-1];

  While k < length(interp)  Do
    Begin
      For l := 0 To step-2 Do
        Begin
          interp[k+l] := y0+((y1-y0)*((xNew[k+l]-x0)/(x1-x0)));
        End;

      k := k+(step);
      x0 := xNew[k-1];
      y0 := interp[k-1];
      x1 := xNew[k+step-1];
      y1 := interp[k+step-1];
    End;
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
      extrap[i] := sum(yOriginal[j..j+step-1]);
      j := j+step;
    End;

End;


Procedure ReadRivVelFile;
// file containing average stream velocity in m/s for each month
// from January till December

Var 
  i: integer;
  datafile: textfile;

Begin
  setlength(RivVel,13);
  assignfile(datafile, RivVelFilename);
  reset (datafile);
  For i:=1 To 12 Do
    Read(datafile,RivVel[i]);
  closefile(datafile);
End;

Procedure ReadOutput;

Var 
  sewerfile, totsedfile: textfile;
  b, totsedfilename: string;
  lengthstring, teller,i,j: integer;
Begin

  SetCurrentDir(File_output_dir);

  // read output maps
  GetRfile(SediExport_event, File_output_dir + 'SediExport_kg.rst');
  GetRfile(SediIn_event, File_output_dir + 'SediIn_kg.rst');
  GetRfile(SediOut_event, File_output_dir + 'SediOut_kg.rst');
  GetRfile(Watereros_event, File_output_dir + 'WATEREROS (mm per gridcel).rst');
  GetRfile(Watereros_kg_event, File_output_dir + 'WATEREROS (kg per gridcel).rst');
  GetRfile(RUSLE_event, File_output_dir + 'RUSLE.rst');
  If Not simplified Then
    Begin
      GetRfile(ReMap_event, File_output_dir + 'Remap.rst');
      GetRfile(TotRun_event, File_output_dir + 'Total runoff.rst');
    End;

  // read output text files
  If Not simplified Then
    Begin
      ReadOutputText(File_output_dir+'Discharge.txt', Discharge_event, numOutlet+1);
      If VHA Then
        ReadOutputText(File_output_dir+'Discharge_VHA.txt', Discharge_VHA_event, numVHA+1);
      ReadOutputText(File_output_dir+'Sediment.txt', Sediment_event, numOutlet+1);
      If VHA Then
        ReadOutputText(File_output_dir+'Sediment_VHA.txt', Sediment_VHA_event, numVHA+1);
      ReadOutputText(File_output_dir+'Sediment concentration.txt', Sedconc_event, numOutlet+1);
      If VHA Then
        ReadOutputText(File_output_dir+'Sediment concentration_VHA.txt', Sedconc_VHA_event, numVHA+1
        );
      If Include_buffer Then
        ReadOutputText(File_output_dir+'Spillover per buffer.txt', Spillover_event, 2);
      ReadOutputText(File_output_dir+'Total discharge.txt', TotDischarge_event, 2);
      If est_clay Then
        Begin
          ReadOutputText(File_output_dir+'Clay content sediment.txt', clay_cont, 2);
          If VHA Then
            ReadOutputText(File_output_dir+'Clay content sediment VHA.txt', clay_cont_VHA, 2);
        End;
      If include_sewer Then
        Begin
          assignfile(sewerfile, File_output_dir+'Sewer output water.txt');
          reset(sewerfile);
          readln(sewerfile,b);
          // skip file title
          Read(sewerfile,sewer_out_water_event);
          closefile(sewerfile);
        End;
    End;

  //ReadOutputText(File_output_dir+'Total sediment.txt', TotSediment_event, 2);

  // Read total sediment.txt
  totsedfilename := File_output_dir+'Total sediment.txt';
  assignfile(totsedfile, totsedfilename);
  reset(totsedfile);
  teller := 0;
  While Not eof(totsedfile) Do
    Begin
      //To determine the number of rows in the file
      inc(teller);
      Readln(totsedfile, b);
    End;
  teller := teller-10;
  Setlength(TotSediment_event, teller, 2);
  For i := 0 To teller-1 Do
    For j := 0 To 1 Do
      Begin
        TotSediment_event[i,j] := 0;
      End;
  closefile(totsedfile);
  assignfile(totsedfile,totsedfilename);
  reset (totsedfile);
  readln(totsedfile,b);
  // total erosion
  delete(b,1,15);
  lengthstring := length(b);
  delete(b,lengthstring-4,5);
  TotalErosion_event := StrToFloat(b);
  readln(totsedfile,b);
  // total deposition
  delete(b,1,18);
  lengthstring := length(b);
  delete(b,lengthstring-4,5);
  TotalDeposition_event := StrToFloat(b);
  readln(totsedfile,b);
  // Sediment leaving catchment, through river
  delete(b,1,47);
  lengthstring := length(b);
  delete(b,lengthstring-4,5);
  SedleavingRiv_event := StrToFloat(b);
  readln(totsedfile,b);
  // Sediment leaving catchment, not through river
  delete(b,1,51);
  lengthstring := length(b);
  delete(b,lengthstring-4,5);
  SedLeaving_event := StrToFloat(b);
  readln(totsedfile,b);
  // sediment trapped in buffers
  delete(b,1,29);
  lengthstring := length(b);
  delete(b,lengthstring-4,5);
  SedTrapBuffer_event := StrToFloat(b);
  readln(totsedfile,b);
  // sediment trapped in open water
  delete(b,1,32);
  lengthstring := length(b);
  delete(b,lengthstring-4,5);
  SedTrapWater_event := StrToFloat(b);
  readln(totsedfile,b);
  // space
  readln(totsedfile,b);
  // space
  readln(totsedfile,b);
  // skip title
  readln(totsedfile,b);
  // skip column headings
  For i:=0 To teller-1 Do
    For j:=0 To 1 Do
      Begin
        Read(totsedfile,TotSediment_event[i,j]);
      End;
  closefile(totsedfile);

  // read Total sediment VHA.txt
  If VHA Then
    ReadOutputText(File_output_dir+'Total sediment VHA.txt', TotSedimentVHA_event, 2);

  If include_sewer Then
    Begin
      assignfile(sewerfile, File_output_dir+'Sewer output sediment.txt');
      reset(sewerfile);
      readln(sewerfile,b);
      // skip file title
      Read(sewerfile,sewer_out_sediment_event);
      closefile(sewerfile);
    End;

End;

Procedure ReadOutputText (filename:String; Var Z: FloatArray2; numColumns: integer);

Var 
  teller, i, j: integer;
  datafile: textfile;
  temp: floatArray2;
  a: string;
Begin
  teller := 0;
  assignfile(datafile, filename);
  reset(datafile);
  readln(datafile,a);
  // skip file title
  readln(datafile,a);
  // skip column headings
  While Not eof(datafile) Do
    Begin
      //To determine the number of time steps and number of rows in the file
      inc(teller);
      Readln(datafile, a);
    End;
  Setlength(Z, teller, numColumns);
  For i := 0 To teller-1 Do
    For j := 0 To numColumns-1 Do
      Begin
        Z[i,j] := 0;
      End;
  closefile(datafile);
  assignfile(datafile,filename);
  reset (datafile);
  readln(datafile,a);
  readln(datafile,a);
  For i:=0 To teller-1 Do
    For j:=0 To numColumns-1 Do
      Begin
        Read(datafile,Z[i,j]);
      End;
  closefile(datafile);
End;

Procedure ActivateMemory;

Var 
  i, j: integer;
Begin
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

  If Not Simplified Then
    Begin
      SetDynamicRData(TotRun_tot);
      SetzeroR(TotRun_tot);
      SetDynamicRData(ReMap_tot);
      SetzeroR(ReMap_tot);

      SetLength(Discharge_tot,1,numOutlet+1);
      For j:=0 To numOutlet Do
        Discharge_tot[0,j] := 0;
      setLength(Sedconc_tot, 1, numOutlet+1);
      For j:=0 To numOutlet Do
        Sedconc_tot[0,j] := 0;
      SetLength(Sediment_tot,1,numOutlet+1);
      For j:=0 To numOutlet Do
        Sediment_tot[0,j] := 0;
      If Include_buffer Then
        Begin
          SetLength(Spillover_tot,Number_of_Buffers,2);
          For i:=0 To Number_of_Buffers-1 Do
            Spillover_tot[i,0] := Spillover_event[i,0];
        End;
      SetLength(TotDischarge_tot,numOutlet,2);
      For i := 0 To numOutlet-1 Do
        TotDischarge_tot[i,0] := TotDischarge_event[i,0];
      If VHA Then
        Begin
          SetLength(Discharge_VHA_tot,1,numVHA+1);
          For j:=0 To numVHA Do
            Discharge_VHA_tot[0,j] := 0;
          setLength(Sedconc_VHA_tot, 1, numVHA+1);
          For j:=0 To numVHA Do
            Sedconc_VHA_tot[0,j] := 0;
          setLength(Sediment_VHA_tot, 1, numVHA+1);
          For j:=0 To numVHA Do
            Sediment_VHA_tot[0,j] := 0;
        End;
      If include_sewer Then
        Sewer_out_water_tot := 0;
    End;

  SetLength(TotSediment_tot,numOutlet,2);
  If VHA Then
    SetLength(TotSedimentVHA_tot,numVHA,2);

  If include_sewer Then
    sewer_out_sediment_tot := 0;
End;

Procedure ReleaseMemory;
Begin
  DisposeDynamicRdata(SediExport_event);
  DisposeDynamicRdata(SediIn_event);
  DisposeDynamicRdata(SediOut_event);
  DisposeDynamicRdata(Watereros_event);
  DisposeDynamicRdata(Watereros_kg_event);
  DisposeDynamicRdata(RUSLE_event);
  If Not simplified Then
    Begin
      DisposeDynamicRdata(TotRun_event);
      DisposeDynamicRdata(ReMap_event);
    End;
End;

Procedure ReleaseMemory2;
Begin
  DisposeDynamicRdata(SediExport_tot);
  DisposeDynamicRdata(SediIn_tot);
  DisposeDynamicRdata(SediOut_tot);
  DisposeDynamicRdata(Watereros_tot);
  DisposeDynamicRdata(Watereros_kg_tot);
  DisposeDynamicRdata(RUSLE_tot);
  If Not simplified Then
    Begin
      DisposeDynamicRdata(TotRun_tot);
      DisposeDynamicRdata(ReMap_tot);
    End;
End;


End.
