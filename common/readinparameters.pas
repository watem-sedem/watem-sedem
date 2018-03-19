
Unit ReadInParameters;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, RData_CN, GData_CN, Inifiles, Idrisi, Typinfo;

Procedure ReadInRasters;
Procedure Allocate_Memory;
Procedure Release_Memory;
Function SetFileFromIni(inifile: Tinifile; inivalue, datadir: string; obliged: boolean): string;
Procedure Readsettings(INI_filename:String);
Procedure Create_CN_map(Var CNmap: RRaster;Perceelskaart:RRaster; Filename:String);
Function CalculateCN(CNmax,Cc,Cr,c1,c2:integer): single;
Procedure Create_ktil_map(Var ktil: GRaster);
Procedure Create_ktc_map(Var ktc: GRaster);
Function intArrayIsEqual (inputArray: Array Of Integer): boolean;
Function doubArrayIsEqual (inputarray: Array Of double): boolean;

//Record for model variables

Type
  EInputException = Class(Exception);
  Gvector = array Of smallint;
  Rvector = array Of single;

  TSingleArray = array Of array Of single;
  TDoubleArray = array Of array Of double;
  TIntArray = array Of array Of integer;

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

  TRainRecord = Record
    //Record containing information about rainfall input
    ID: integer;
    //From 1 till number of time steps
    Time: Integer;
    //in seconds
    Rain: Double;
    // in mm
    Rain_fraction: double;
    //Fraction of rainfall that falls in each time step
  End;

  TRainRecordArray = array Of TRainRecord;
  //Record is converted to 2D matrix

  TRouting = Record
    //Record containing information about runoff routing for each cell
    One_Target: boolean;
    //Can I delete this???
    Target1Row, Target1Col, Target2Row, Target2Col: integer;
    Part1, Part2, Distance1, Distance2: double;
  End;

  TCalibration = Record
   KTcHigh_lower: integer;
   KTcHigh_upper: integer;
   KTcLow_lower: integer;
   KTcLow_upper: integer;
   steps: integer;
  end;

  TRoutingArray = array Of array Of TRouting;
  //Record is converted to 3D matrix

  TLModel = (Desmet1996_McCool, Desmet1996_Vanoost2003);
  TSModel = (Desmet1996, Nearing1997);

Var 
  //internal variables
  sedprod,depprod: double;
  WATEREROS     : RRaster;
  //water erosion (unit: mm)
  WATEREROS_cubmeter: RRaster;
  // water erosion (unit: m³)
  WATEREROS_kg: RRaster;
  // water erosion (unit: kg)
  RUSLE         : RRaster;
  // result of RUSLE equation in kg/m² = POTENTIAL soil loss
  TILEROS       : RRaster;
  //tillage erosion (unit: mm)
  SEDI_EXPORT   : RRaster;
  // sediment export in m³
  SEDI_EXPORT_kg   : RRaster;
  // sediment export in kg
  SEDI_IN2       : RRaster;
  // incoming sediment in kg
  SEDI_OUT2      : RRaster;
  // outgoing sediment in kg
  {Rasters to be read in--------------------------------------------------------}
  K_factor   : GRaster;    {RUSLE K-factor map kg m² h m-² MJ-1 mm-1}
  C_factor   : RRaster;
  P_factor   : RRaster;
  ktc        : GRaster;
  ktil       : GRaster;
  {End Rasters to be read in----------------------------------------------------}

  {Parameters to be read from ini-file------------------------------------------}
  {Working directories}
  datadir              : string;
  File_output_dir      : string;
  {Files}
  INIfilename          : string;
  DTM_filename         : string;       {unit m}
  PARCEL_filename      : string;

{unit id, 1 to 3200 for parcels, -1 for river, 0 for outside area, -2 for roads, -3 for forest, -4 for pasture, -5 for ponds}
  Rainfallfilename     : string;
  Sewerfilename        : string;
  {indicates location of sewers. Value = efficiency for capturing water/sediment (type: double)}
  CNmapfilename        : string;
  TILDIRfilename       : string;
  Rofilename           : string;
  Bufferfilename       : string;
  Ditch_filename       : string;
  Dam_filename         : string;
  K_Factor_filename    : string;
  Cf_Data_filename     : string;
  Pf_data_Filename     : string;
  ktc_Data_Filename    : string;
  ktil_Data_Filename   : string;
  Outletfilename       : string;
  riversegment_filename: string;
  {User Choices}
  Simplified           : boolean;
  Use_Rfactor          : boolean;
  Include_sewer        : boolean;
  Topo, Inc_tillage    : boolean;
  est_clay             : boolean;
  Include_buffer       : boolean;
  Include_ditch        : boolean;
  Include_dam          : boolean;
  Create_ktc           : boolean;
  Create_ktil          : boolean;
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
  AR5                  : double;
  RFactor              : double;
  BD                   : integer;
  riv_vel              : double;
  sewer_exit           : integer;
  alpha                : double;
  beta                 : double;
  clay_parent          : double;
  Number_of_Buffers    : integer;
  ktc_low              : integer;
  ktc_high             : integer;
  ktc_limit            : double;
  ktil_Default         : integer;
  ktil_threshold       : double;
  TFSED_crop           : integer;
  TFSED_forest         : integer;
  Timestep_model       : integer;
  EndTime_model        : integer;
  Timestep_output      : integer;
  PTefValueCropland    : integer;
  PTefValueForest      : integer;
  PTefValuePasture     : integer;
  max_kernel           : integer;
  max_kernel_river     : integer;
  calibrate            : Boolean;
  cal     : TCalibration;

  LModel: TLModel;
  SModel: TSModel;

  {Buffers}
  BufferData: TBufferDataArray;
  {End Parameters to be read form ini-file--------------------------------------}

  PRC, DTM, CNmap, LU, ReMap, RunoffTotMap, SewerMap: Rraster;
  TilDir, Ro, BufferMap, Outlet, RivSeg, Ditch_map, Dam_map, PTEFmap: GRaster;
  i, j, lowOutletX, lowOutletY: integer;

  ROW, COLUMN : Gvector;

  Slope,Aspect,Uparea,LS: Rraster;
  totsurface: double;

  Routing: TRoutingArray;
  OutletArray : TIntArray;
  RainData: TRainRecordArray;


Implementation

Procedure ReadInRasters;
Begin
  setCurrentDir(datadir);
  GetRFile(DTM,DTM_Filename);
  GetRFile(PRC,PARCEL_filename);

  If Include_sewer Then
    GetRFile(SewerMap,Sewerfilename);

  If Not Simplified Then
    Begin
      GetRfile(CNmap, CNmapfilename);
    End;
  If not topo Then // als topo = false wordt de ploegrichting in rekening gebracht
    Begin
      GetGFile(TilDir, TilDirFilename);
      GetGfile(Ro, RoFilename);
    End;

  GetGFile(K_factor, K_Factor_filename);
  GetRFile(C_factor, Cf_Data_filename);
  GetRFile(P_factor, Pf_Data_filename);

  If not calibrate Then
    If Create_ktc Then
      Create_ktc_map(ktc)
    Else
      GetGFile(ktc, ktc_Data_filename);

  If Create_ktil Then
    Create_ktil_map(ktil)
  Else
    GetGFile(ktil, ktil_Data_filename);

  //If buffers are taken into account the buffermap is loaded
  If Include_buffer = true Then
    Begin
      GetGfile(BufferMap,Bufferfilename);
      For i := 1 To nrow Do
        //The row and column of every buffer are stored in the record
        For j := 1 To ncol Do
          Begin
            If (Buffermap[i,j] <> 0) And (Buffermap[i,j] <= Number_of_Buffers) Then
              Begin
                BufferData[Buffermap[i,j]].row := i;
                BufferData[Buffermap[i,j]].col := j;
              End;
          End;
    End
  Else
    Begin
      // Buffermap should be map containing only zeros
      SetDynamicGData(Buffermap);
      SetzeroG(Buffermap);
    End;

  If Include_ditch = true Then
    GetGfile(Ditch_map,Ditch_filename);

  If Include_dam = true Then
    GetGfile(Dam_map,Dam_filename);

  If outlet_select Then
    GetGfile(Outlet, Outletfilename);

  If VHA Then
    GetGfile(RivSeg, Riversegment_filename);

  // PTEF map is created
  SetDynamicGdata(PTEFmap);
  setzeroG(PTEFmap);
  For i := 1 To nrow Do
    //The row and column of every buffer are stored in the record
    For j := 1 To ncol Do
      Begin
        Case round(PRC[i,j]) Of 
          //PRC = GRaster, dus integers.
          -6: PTEFmap[i,j] := PTEFValuePasture;
          -4: PTEFmap[i,j] := PTEFValuePasture;
          -3: PTEFmap[i,j] := PTEFValueForest;
          1..10000000000: PTEFmap[i,j] := PTEFValueCropland;
          Else PTEFmap[i,j] := 0;
        End;
      End;

  writeGidrisi32file(ncol,nrow,datadir+'PTEFmap'+'.rst', PTEFmap);

  //Check whether number of rows, number of columns and resolution are equal for all input maps
If Not intArrayIsEqual(nrowAR) Then
  raise EInputException.Create(
  'Error: The number of rows should be the same for all input maps. Please verify your input rasters.');

If Not intArrayIsEqual(ncolAR) Then
  raise EInputException.Create(
'Error: The number of columns should be the same for all input maps. Please verify your input rasters.');

If Not doubArrayIsEqual(resAR) Then
 raise EInputException.Create('Error: The resolution should be the same for all input maps. '+
 'Please verify input rasters.');

end;

Procedure Allocate_Memory;
Begin
  // Create procedure to read in maps & allocate memory to global maps

  // Assign internal & global 2D maps
  SetDynamicRData(LS);
  SetDynamicRData(UPAREA);
  SetDynamicRData(SLOPE);
  SetDynamicRData(ASPECT);

  SetDynamicRData(WATEREROS);
  SetDynamicRData(WATEREROS_cubmeter);
  SetDynamicRData(WATEREROS_kg);
  SetDynamicRData(RUSLE);
  SetDynamicRData(SEDI_EXPORT);
  SetDynamicRData(SEDI_EXPORT_kg);
  SetDynamicRData(SEDI_IN2);
  SetDynamicRData(SEDI_OUT2);
  SetDynamicRData(TILEROS);
  //************************

End;

Procedure Release_Memory;

Begin
  // Release memory for input rasters
  DisposeDynamicRdata(DTM);
  DisposeDynamicRdata(PRC);
  If Include_sewer Then
    DisposedynamicRData(SewerMap);

  If Not Simplified Then
    Begin
      DisposedynamicRData(CNmap);
    End;

  If topo = false Then // als topo = false wordt de ploegrichting in rekening gebracht
    Begin
      DisposedynamicGData(TilDir);
      DisposedynamicGData(Ro);
    End;

  DisposeDynamicGdata(K_Factor);
  DisposeDynamicRdata(C_factor);
  DisposeDynamicRdata(P_factor);
  DisposeDynamicGdata(ktil);
  DisposeDynamicGdata(ktc);
  DisposeDynamicGData(Buffermap);

  If Include_ditch = true Then
    DisposeDynamicGData(Ditch_map);

  If Include_dam = true Then
    DisposeDynamicGData(Dam_map);

  If outlet_select Then
    DisposeDynamicGData(Outlet);

  If VHA Then
    DisposeDynamicGData(RivSeg);

  // Release internal 2D rasters maps
  DisposeDynamicRdata(UPAREA);
  DisposeDynamicRdata(LS);
  DisposeDynamicRdata(SLOPE);
  DisposeDynamicRdata(ASPECT);
  DisposeDynamicRdata(WATEREROS);
  DisposeDynamicRdata(WATEREROS_cubmeter);
  DisposeDynamicRdata(WATEREROS_kg);
  DisposeDynamicRdata(RUSLE);
  DisposeDynamicRdata(SEDI_EXPORT);
  DisposeDynamicRdata(SEDI_EXPORT_kg);
  DisposeDynamicRdata(SEDI_IN2);
  DisposeDynamicRdata(SEDI_OUT2);
  DisposeDynamicRdata(TILEROS);

  If Not Simplified Then
    Begin
      DisposedynamicRData(RunoffTotMap);
      DisposedynamicRData(Remap);
    End;

End;

Function SetFileFromIni(inifile: Tinifile;inivalue,  datadir: string; obliged: boolean): string;
Var
 Dummy_str, filename: string;
Begin
  if obliged Then
    Begin
    filename := Inifile.Readstring('Files', inivalue, Dummy_str);
    if (filename = '') Then
      raise EInputException.Create('Error: '+ inivalue+' not defined in inifile');
    If (FileExists(datadir + filename)) Then SetFileFromIni := datadir  + filename
    Else If FileExists(filename) Then
       SetFileFromIni := filename
    Else
      raise EInputException.Create('Error in data input: ' + inivalue + ' not found in '+ datadir+ ' or path');
    End;
End;

//******************************************************************************
//This procedure reads the .ini file and assigns the file + location + values
//to the correct variables.
//******************************************************************************
Procedure Readsettings(INI_filename:String);

Var 
  Inifile: Tinifile;
  Dummy_str, Buffername, inistring: string;
  i: integer;
Begin

  If Not FileExists(INI_filename) Then
    raise Exception.create('Inifile does not exist');
  Inifile := Tinifile.create(INI_filename);

  Datadir := Inifile.readstring('Working directories', 'Input directory', Dummy_str);
  File_output_dir := Inifile.readstring('Working directories', 'Output directory', Dummy_str);
  If Not DirectoryExists(File_output_dir) Then CreateDir(File_output_dir);

  // Make sure that a trailing slash is added to the input/output dir
  File_output_dir := IncludeTrailingPathdelimiter(File_output_dir);
  Datadir := IncludeTrailingPathdelimiter(Datadir);

  {User choices}
  Simplified := Inifile.ReadBool('User Choices','Simplified model version',false);
  Include_sewer:= Inifile.ReadBool('User Choices','Include sewers',false);
  If Include_sewer And Not TryStrToInt(Inifile.Readstring('Variables', 'Sewer exit', Dummy_str),
   sewer_exit) Then
    Begin
      raise EInputException.Create('Error in data input: Sewer exit system value missing or wrong data format');
    End;

  Use_Rfactor:= Inifile.ReadBool('User Choices','Use R factor',false);
  Inc_tillage:=Inifile.ReadBool('User Choices','Include tillage',false);
  topo := not Inc_tillage;
  Include_buffer := Inifile.ReadBool('User Choices','Include buffers',false);
  Include_ditch := Inifile.ReadBool('User Choices','Include ditches',false);
  Include_dam := Inifile.ReadBool('User Choices','Include dams',false);
  Create_ktc := Inifile.ReadBool('User Choices','Create ktc map',false);
  Create_ktil := Inifile.ReadBool('User Choices','Create ktil map',false);
  VHA := Inifile.ReadBool('User Choices','Output per VHA river segment',false);
   max_kernel := Inifile.ReadInteger('User Choices', 'Max kernel', 50);
  max_kernel_river := Inifile.ReadInteger('User Choices', 'Max kernel river', 100);
  est_clay:= Inifile.ReadBool('User Choices','Estimate clay content',false);
  calibrate :=  inifile.ReadBool('Calibration', 'Calibrate', false);

  inistring:= Inifile.ReadString('User Choices', 'L model', 'Desmet1996_Vanoost2003');
  Lmodel := TLModel(GetEnumValue(Typeinfo(TLModel), inistring));
  if Lmodel > high(TLModel) then
    raise EInputException.Create('invalid L model: '+ inistring);

  inistring:= Inifile.ReadString('User Choices', 'S model', 'Nearing1997');
  Smodel := TSModel(GetEnumValue(Typeinfo(TSModel), inistring));
  if Smodel > high(TSModel) then
    raise EInputException.Create('invalid S model: '+ inistring);


  {Filenames}
  INIfilename := Inifile.Readstring('Files', '.INI filename', Dummy_str);
  DTM_filename := SetFileFromIni(Inifile, 'DTM filename', datadir, True);
  PARCEL_filename := SetFileFromIni(Inifile, 'Parcel filename', datadir, True);
  Rainfallfilename := SetFileFromIni(Inifile, 'Rainfall filename', datadir, not use_rfactor);
  Sewerfilename :=SetFileFromIni(Inifile, 'Sewer map filename', datadir, Include_sewer);
  CNmapfilename := SetFileFromIni(Inifile, 'CN map filename', datadir, not Simplified);
  TilDirfilename := SetFileFromIni(Inifile, 'Tillage direction filename', datadir, Inc_tillage);
  Rofilename := SetFileFromIni(Inifile, 'Oriented roughness filename', datadir, Inc_Tillage);
  BufferFilename := SetFileFromIni(Inifile, 'Buffer map filename', datadir, Include_buffer);
  Ditch_filename := SetFileFromIni(Inifile, 'Ditch map filename', datadir, Include_ditch);
  Dam_filename := SetFileFromIni(Inifile, 'Dam map filename', datadir, Include_dam);
  K_Factor_filename :=SetFileFromIni(Inifile, 'K factor filename', datadir, true);
  Cf_data_filename :=SetFileFromIni(Inifile, 'C factor map filename', datadir, true);
  Pf_data_filename :=SetFileFromIni(Inifile, 'P factor map filename', datadir, true);
  Riversegment_filename := SetFileFromIni(Inifile, 'River segment filename', datadir, VHA);
  if not calibrate then
    ktc_Data_Filename := SetFileFromIni(Inifile, 'ktc map filename', datadir, (not Create_ktc));
  ktil_Data_Filename := SetFileFromIni(Inifile, 'ktil map filename', datadir, not Create_ktil);
  Outletfilename := inifile.readstring('Files', 'Outlet map filename', Dummy_str);


  If (est_clay) And Not (TryStrToFloat(Inifile.Readstring('Variables',
     'Clay content parent material', Dummy_str), clay_parent)) Then
    Begin
      raise EInputException.Create('Error in data input: Clay content parent material value missing or wrong data format');
      ;
    End;
  If (Inifile.ReadBool('User Choices','Manual outlet selection',false))=true Then Outlet_select := 
                                                                                                true
  Else Outlet_select := false;
  If Outlet_select And Not (FileExists(Outletfilename)) Then
    Begin
      raise EInputException.Create('Error in data input: Outlet map file not found in '+datadir);
    End;
  If (Inifile.ReadBool('User Choices','Convert output',false))=true Then Convert_output := true
  Else Convert_output := false;

  {Output maps}
  If (Inifile.ReadBool('Output maps','Write aspect',false))=true Then Write_ASPECT := true
  Else Write_ASPECT := false;
  If (Inifile.ReadBool('Output maps','Write LS factor',false))=true Then Write_LS := true
  Else Write_LS := false;
  If (Inifile.ReadBool('Output maps','Write RUSLE',false))=true Then Write_RUSLE := true
  Else Write_RUSLE := false;
  If (Inifile.ReadBool('Output maps','Write sediment export',false))=true Then Write_Sediexport := 
                                                                                                true
  Else Write_Sediexport := false;
  If (Inifile.ReadBool('Output maps','Write slope',false))=true Then Write_SLOPE := true
  Else Write_SLOPE := false;
  If (Inifile.ReadBool('Output maps','Write tillage erosion',false))=true Then Write_TILEROS := true
  Else Write_TILEROS := false;
  If (Inifile.ReadBool('Output maps','Write upstream area',false))=true Then Write_UPAREA := true
  Else Write_UPAREA := false;
  If (Inifile.ReadBool('Output maps','Write water erosion',false))=true Then Write_WATEREROS := true
  Else Write_WATEREROS := false;
  If Simplified Then
    Begin
      Write_RE := false;
      Write_TOTRUN := false;
    End
  Else
    Begin
      If (Inifile.ReadBool('Output maps','Write rainfall excess',false))=true Then Write_RE := true
      Else Write_RE := false;
      If (Inifile.ReadBool('Output maps','Write total runoff',false))=true Then Write_TOTRUN := true
      Else Write_TOTRUN := false;
    End;


  {Variables}

  If Not Simplified Then
    Begin
      If Not Use_RFactor Then
          If Not TryStrToFloat(Inifile.Readstring('Variables', '5-day antecedent rainfall',
             Dummy_str), AR5) Then
              raise EInputException.Create('Error in data input: AR5 value missing or wrong data format');

      If Not TryStrToFloat(Inifile.Readstring('Variables', 'Stream velocity', Dummy_str), riv_vel)
        Then
          raise EInputException.Create('Error in data input: Stream velocity value missing or wrong data format');

      If Not TryStrToFloat(Inifile.Readstring('Variables', 'Alpha', Dummy_str), alpha) Then
          raise EInputException.Create('Error in data input: alpha value missing or wrong data format');

      If Not TryStrToFloat(Inifile.Readstring('Variables', 'Beta', Dummy_str), beta) Then
          raise EInputException.Create('Error in data input: beta value missing or wrong data format');

    End
  Else
    Begin
      If Not TryStrToFloat(Inifile.Readstring('Variables', 'R factor', Dummy_str),Rfactor) Then
          raise EInputException.Create('Error in data input: R factor value missing or wrong data format');
    End;

  If Not TryStrToInt(Inifile.Readstring('Variables', 'Bulk density', Dummy_str), BD) Then
      raise EInputException.Create('Error in data input: BD value missing or wrong data format');

  If (Include_buffer) And Not (TryStrToInt(inifile.readstring('Variables', 'Number of buffers',
     Dummy_str), Number_of_Buffers)) Then
      raise EInputException.Create('Error in data input: Number of buffers value missing or wrong data format');



  if not calibrate then
    begin
        If (create_ktc) And Not
           (TryStrToInt(Inifile.Readstring('Variables', 'ktc low', Dummy_str),ktc_low)) Then
             raise EInputException.Create('Error in data input: ktc low value missing or wrong data format');
        If (create_ktc) And Not
           (TryStrToInt(Inifile.Readstring('Variables', 'ktc high', Dummy_str), ktc_high)) Then
             raise EInputException.Create('Error in data input: ktc high value missing or wrong data format');
        If (create_ktc) And Not
            (TryStrToFloat(Inifile.Readstring('Variables', 'ktc limit', Dummy_str), ktc_limit)) Then
             raise EInputException.Create('Error in data input: ktc limit value missing or wrong data format');
    end;

  If (create_ktil) And Not (TryStrToInt(Inifile.Readstring('Variables', 'ktil default', Dummy_str),
     ktil_Default)) Then
    Begin
      raise EInputException.Create('Error in data input: ktil default value missing or wrong data format');
    End;
  If (create_ktil) And Not (TryStrToFloat(Inifile.Readstring('Variables', 'ktil threshold',
     Dummy_str), ktil_threshold)) Then
    Begin
     raise EInputException.Create('Error in data input: ktil threshold value missing or wrong data format');
    End;
  If Not TryStrToInt(Inifile.Readstring('Variables', 'Parcel connectivity cropland', Dummy_str),
     TFSED_crop) Then
    Begin
      raise EInputException.Create('Error in data input: Parcel connectivity cropland value missing or wrong data format')
      ;
    End;
  If Not TryStrToInt(Inifile.Readstring('Variables', 'Parcel connectivity forest', Dummy_str),
     TFSED_forest) Then
    Begin
      raise EInputException.Create(
        'Error in data input: Parcel connectivity forest/pasture value missing or wrong data format');
    End;
  If Not TryStrToInt(Inifile.Readstring('Variables', 'Parcel trapping efficiency cropland',
     Dummy_str), PTEFValueCropland) Then
      raise EInputException.Create(
      'Error in data input: Parcel trapping efficiency (cropland) value missing or wrong data format');
  If Not TryStrToInt(Inifile.Readstring('Variables', 'Parcel trapping efficiency forest', Dummy_str)
     , PTEFValueForest) Then
    raise EInputException.Create(
       'Error in data input: Parcel trapping efficiency (forest) value missing or wrong data format');
      ;

  If Not TryStrToInt(Inifile.Readstring('Variables', 'Parcel trapping efficiency pasture', Dummy_str
     ), PTEFValuePasture) Then
      raise EInputException.Create(
      'Error in data input: Parcel trapping efficiency (pasture) value missing or wrong data format')
      ;
  If Not Simplified Then
    Begin
      If Not TryStrToInt(inifile.readstring('Variables', 'Desired timestep for model', Dummy_str),
         Timestep_model) Then
          raise EInputException.Create('Error in data input: Desired timestep for model value missing or wrong data format')
          ;
      If (Convert_output) And Not TryStrToInt(inifile.readstring('Variables',
         'Final timestep output', Dummy_str), Timestep_output) Then
          raise EInputException.Create(
          'Error in data input: Final timestep output value missing or wrong data format')
          ;
    End;
  If Not TryStrToInt(inifile.readstring('Variables', 'Endtime model', Dummy_str), Endtime_model)
    Then
      raise EInputException.Create(
      'Error in data input: Endtime model value missing or wrong data format');

  If Include_buffer Then
    Begin
      setlength(Bufferdata, Number_of_Buffers + 1);
      For i := 1 To Number_of_Buffers Do
        Begin
          Buffername := 'Buffer ' + IntToStr(i);
          If Not TryStrToFloat(inifile.readstring(Buffername, 'Volume', Dummy_str), Bufferdata[i].
             Volume) Then
              raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                            ' volume value missing or wrong data format');
          If Not TryStrToFloat(inifile.readstring(Buffername, 'Height dam', Dummy_str),Bufferdata[i]
             .Height_dam) Then
            raise EInputException.Create(
                  'Error in data input: Buffer '+intToStr(i)+
                  ' height dam value missing or wrong data format');
          If Not TryStrToFloat(inifile.readstring(Buffername, 'Height opening', Dummy_str),
             Bufferdata[i].Height_opening) Then
              raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                            ' height opening value missing or wrong data format');
          If Not  TryStrToFloat(inifile.readstring(Buffername, 'Opening area', Dummy_str),Bufferdata
             [i].Opening_area) Then
              raise EInputException.Create(
              'Error in data input: Buffer '+intToStr(i)+
                            ' opening area value missing or wrong data format');
          If Not TryStrToFloat(inifile.readstring(Buffername, 'Discharge coefficient', Dummy_str),
             Bufferdata[i].Cd) Then
              raise EInputException.Create(
                'Error in data input: Buffer '+intToStr(i)+
                ' discharge coefficient value missing or wrong data format');
          If Not TryStrToFloat(inifile.readstring(Buffername, 'Width dam', Dummy_str), Bufferdata[i]
             .width_dam) Then
              raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                            ' width dam value missing or wrong data format');
          If Not TryStrToFloat(inifile.readstring(Buffername, 'Trapping efficiency', Dummy_str),
             Bufferdata[i].PTEF) Then
              raise EInputException.Create( 'Error in data input: Buffer '+intToStr(i)+
                            ' trapping efficiency value missing or wrong data format');
          If Not TryStrToInt(inifile.readstring(Buffername, 'Extension ID', Dummy_str), Bufferdata[i
             ].ext_ID) Then
              raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                            ' extension ID missing or wrong data format');
          If Bufferdata[i].Height_opening > Bufferdata[i].Height_dam Then
              raise EInputException.Create(
                'Error in buffer input: the height of the opening cannot be larger than' +
                ' the height of the dam. Please insert correct values.');
        End;
    End;

  {cal}

  If calibrate Then
    Begin

      cal.KTcHigh_lower:=inifile.ReadInteger('Calibration','KTcHigh_lower', 25);
      cal.KTcHigh_upper:=inifile.ReadInteger('Calibration','KTcHigh_upper', 250);
      cal.KTcLow_lower:=inifile.ReadInteger('Calibration','KTcLow_lower', 10);
      cal.KTcLow_upper:=inifile.ReadInteger('Calibration','KTcLow_upper', 100);
      cal.steps:=Inifile.ReadInteger('Calibration', 'steps', 10);
    end;

  Inifile.Destroy;
End;

//**************************************************************************
//This procedure is only ran when the user enters a land use map and table that
//contains factors to calculate the CN value for every land use
//In this procedure the CN map is calculated based on the formulas described in
//The PhD of Kristof Van Oost
//**************************************************************************
Procedure Create_CN_map(Var CNmap: RRaster;Perceelskaart:RRaster; Filename:String);

Var 
  Count, i, j, k, getal, NumberOfLU, nrowPRC, ncolPRC: integer;
  Table: textfile;
  TempName: String;
  M: GRaster;
  CN_waarden: array Of single;

  //The number of rows in the .txt file is counted. This way the user can define as
  //much LU classes as he wishes
  //!!The first row of the .txt file should contain headers!!
Begin
  If  FileExists(datadir+Filename) Then //Check if the .txt file exists
    Begin
      SetcurrentDir(datadir);
      TempName := Filename;
      // filename + extension
      Count := 0;
      Assignfile(Table,TempName);
      Reset(Table);
      While Not eof(Table) Do
        //eof = end of file
        Begin
          readln(Table);
          Count := Count+1;
          //The number of rows is counted
        End;
      Closefile(Table);
      NumberOfLU := Count-1;
      //'-1' because the first row contains headers
    End
  Else
    Begin
      EinputException.Create('De tabel met CN waarden werd niet herkend, het programma wordt gesloten.');
      //If the file does not extists the program is ended
    End;

  //The numbers from the .txt file are stored in a variable (matrix) M
  //All numbers in the .txt file should be integers!!!
  SetDynamicGData(M);
  Begin
    Assignfile(Table,TempName);
    Reset(Table);
    readln(Table);
    //The first row (headers) is read and is thus skipped below
    For i := 1 To NumberofLU Do
      For j := 1 To 6 Do

//6 because the .txt table alsways contains 6 columns (Parcel ID - CNmax - c1 - c2 - Crop cover - Crusting stage)
        Begin
          read(Table, getal);
          M[i,j] := getal;
        End;
  End;
  closefile(Table);

  //All posible CN values are calculated
  SetLength(CN_waarden,NumberofLU);
  For i:= 1 To NumberofLU Do
    Begin
      CN_Waarden[i] := calculateCN(M[i,2],M[i,5],M[i,6],M[i,3],M[i,4])
                       //CalcualteCN: procedure to calculate the CN value
    End;

  //Number of rows and colums is defined
  nrowPRC := nrow;
  //nrow and ncol are defined based on the .RDC files in RData_CN
  ncolPRC := ncol;

  //Based on the .txt table and the land use map (which is being read in the main unit
  //the CN map is created
  Setlength(CNmap,NrowPRC+1, NColPRC+1);
  //+1 because [0] is being used by Lazarus
  For i := 1 To nrowPRC Do
    For j := 1 To ncolPRC Do
      Begin
        If Perceelskaart[i,j] = 0 Then
          CNmap[i,j] := 0.0
        Else
          Begin
            For k := 1 To NumberofLU Do
              If Perceelskaart[i,j] = M[k,1] Then
                CNmap[i,j] := CN_Waarden[k]
          End
      End;

  //The CN map is stored as an Idrisi map
  writeidrisi32file(ncolPRC,nrowPRC,datadir+'\CNmap'+'.rst',CNmap);
  DisposeDynamicGData(M);
End;


//**********************************************************************
//This procedure calculates the CN value according to the formula of KVO
//**********************************************************************
Function CalculateCN(CNmax,Cc,Cr,c1,c2:integer): single;
Begin
  CalculateCN := CNmax - ((Cc/100)*c1) + ((Cr/5)*c2);
End;


Procedure Create_ktil_map(Var ktil: GRaster);

Var 
  i,j: integer;
Begin
  SetDynamicGData(ktil);
  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If C_factor[i,j] <= ktil_threshold Then
          ktil[i,j] := 0
        Else
          ktil[i,j] := ktil_Default;
      End;

  writeGidrisi32file(ncol,nrow,datadir+'ktilmap'+'.rst',ktil);
End;

Procedure Create_ktc_map(Var ktc: GRaster);

Var 
  i,j: integer;
Begin
  SetDynamicGData(ktc);
  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If C_factor[i,j] <= ktc_limit Then
          ktc[i,j] := ktc_low
        Else
          ktc[i,j] := ktc_high;
        // In WS, cells with PRC == -2 or -1 receive a Ktc value of 9999
        If (PRC[i,j] = -2) Or (PRC[i,j] = -1) Then
          ktc[i,j] := 9999;
        // kTc of open water = 0 because sediment can't go any further once it's there
        If PRC[i,j] = -5 Then
          ktc[i,j] := 0;
      End;

  writeGidrisi32file(ncol,nrow,datadir+'ktcmap'+'.rst',ktc);
End;

// ***************************************************************************
// The following 2 functions check whether the elements in an array are equal or
// not.
// ***************************************************************************

Function intArrayIsEqual (inputArray:Array Of Integer): boolean;

Var
  Val, i: Integer;

Begin
  intArrayIsEqual := True;
  Val := inputArray[1];
  For i := low(inputArray) To High(inputArray) Do
    Begin
      If inputArray[i] <> Val Then
        intArrayIsEqual := false;
    End;
End;

Function doubArrayIsEqual (inputArray:Array Of double): boolean;

Var
  Val: double;
  i: Integer;

Begin
  doubArrayIsEqual := True;
  Val := inputArray[1];
  For i := low(inputArray) To High(inputArray) Do
    Begin
      If inputArray[i] <> Val Then
        doubArrayIsEqual := false;
    End;
End;

End.
