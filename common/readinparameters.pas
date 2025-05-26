
Unit ReadInParameters;

{$mode objfpc}{$H+}
{$R+}
Interface

Uses
Classes, SysUtils, RData_CN, GData_CN, Inifiles, write_raster, Typinfo;

//Record for model variables

Type
  EInputException = Class(Exception);

  rvector = array of single;

  TSingleMatrix = array Of array Of single;
  TIntMatrix = array Of array Of integer;

  TIntArray = array of integer;

  generic TKVArray<T> = Class
   key: TIntArray;
   value: array of T;
   function getItem (k: integer): T;
   property item[k:integer]:T read getItem ; default;
  end;

   TIntKVArray = specialize TKVArray<integer>;
   TDoubleKVArray = specialize TKVArray<double>;

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
    Target1Row, Target1Col, Target2Row, Target2Col: integer;
    Part1, Part2: double;
  End;

  TInvRouting = Record
    size: integer;
    up_X: array of integer;
    up_Y: array of integer;
    treated: array of boolean;
  end;

  TForcedRouting = Record
    TargetRow, TargetCol, FromRow, FromCol: integer;
  end;

  TCalibration = Record
   KTcHigh_lower: double;
   KTcHigh_upper: double;
   KTcLow_lower: double;
   KTcLow_upper: double;
   steps: integer;
  end;

  TRoutingArray = array Of array Of TRouting;
  //Record is converted to 3D matrix

  TRoutingInvArray = array of array of TInvRouting;

  TLModel = (Desmet1996_McCool, Desmet1996_Vanoost2003);
  TSModel = (McCool1987, Nearing1997);
  TTCModel = (VanOost2000, Verstraeten2007);

Function Distance1(r: TRoutingArray;i,j: integer): double;
Function Distance2(r: TRoutingArray;i,j: integer): double;
Procedure ReadInRasters;
Procedure Release_Memory;
Function SetFileFromIni(inifile: Tinifile; inivalue, datadir: string; obliged: boolean): string;
Procedure Readsettings(INI_filename:String);
Procedure Create_CN_map(Var CNmap: RRaster;Perceelskaart:RRaster; Filename:String);
Function CalculateCN(CNmax,Cc,Cr,c1,c2:integer): single;
Procedure Create_ktil_map(Var ktil: GRaster);
Procedure Create_ktc_map(Var ktc: RRaster);
Function intArrayIsEqual (inputArray: Array Of Integer): boolean;
Function doubArrayIsEqual (inputarray: Array Of double): boolean;
Procedure ReadDownstreamSegments(filename: String; var kvArray: tintkvarray);
Procedure ReadUpstreamSegments(filename: String; var upstream: tintkvarray;var proportion: tdoublekvarray);


Var
  //internal variables
  sedprod,depprod: double;
  WATEREROS     : RRaster;
  CAPAC         : RRaster;
  //water erosion (unit: mm)
  WATEREROS_cubmeter: RRaster;
  // water erosion (unit: m³)
  WATEREROS_kg: RRaster;
  // water erosion (unit: kg)
  RUSLE         : RRaster;
  // result of RUSLE equation in kg/m² = POTENTIAL soil loss
  TILEROS       : RRaster;
  TILEROS_kg    : RRaster;
  SEDTIL_IN     : RRaster;
  SEDTIL_OUT    : RRaster;
  //tillage erosion (unit: mm)
  SEDI_EXPORT   : RRaster;
  // sediment export in kg
  SEDI_IN       : RRaster;
  // incoming sediment in kg
  SEDI_OUT      : RRaster;
  // outgoing sediment in kg
  SEWER_IN   : RRaster; //amount of sediment in kg entering sewersystem
  {Rasters to be read in--------------------------------------------------------}
  K_factor   : GRaster;    {RUSLE K-factor map kg m² h m-² MJ-1 mm-1}
  C_factor   : RRaster;
  P_factor   : RRaster;
  ktc        : RRaster;
  ktil       : GRaster;
  {End Rasters to be read in----------------------------------------------------}

  {Parameters to be read from ini-file------------------------------------------}
  {Working directories}
  datadir              : string;
  File_output_dir      : string;
  {Files}
  DTM_filename         : string;       {unit m}
  PARCEL_filename      : string;

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
  river_routing_filename: string;
  river_adjectant_filename: string;
  river_upstream_filename: string;
  {User Choices}
  OnlyRouting          : boolean;
  curve_number           : boolean;
  // Use_Rfactor          : boolean;
  Include_sewer        : boolean;
  Topo, Inc_tillage    : boolean;
  est_clay             : boolean;
  Include_buffer       : boolean;
  Include_ditch        : boolean;
  Include_dam          : boolean;
  Create_ktc           : boolean;
  Create_ktil          : boolean;
  Calc_tileros         : boolean;
  Outlet_select        : boolean;
  Convert_output       : boolean;
  segments             : boolean;
  adjusted_slope       : boolean;
  buffer_reduce_upstream_area: boolean;
  force_routing        : boolean;
  river_routing        : boolean;
  river_topology       : boolean;
  cardinal_routing_to_river: boolean;
  {Output maps}
  Saga_Grids           : boolean;
  Write_ASPECT         : boolean;
  Write_LS             : boolean;
  Write_RE             : boolean;
  Write_RUSLE          : boolean;
  Write_Sediexport     : boolean;
  Write_SLOPE          : boolean;
  Write_TOTRUN         : boolean;
  Write_UPAREA         : boolean;
  Write_WATEREROS      : boolean;
  Write_Routing        : boolean;
  Write_Routing_CR     : boolean;
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
  ktc_low              : double;
  ktc_high             : double;
  ktc_limit            : double;
  ktil_Default         : integer;
  ktil_threshold       : double;
  TFSED_crop           : integer;
  TFSED_forest         : integer;
  TFSED_grasstrip      : integer;
  Timestep_model       : integer;
  EndTime_model        : integer;
  Timestep_output      : integer;
  PTefValueCropland    : integer;
  PTefValueForest      : integer;
  PTefValuePasture     : integer;
  max_kernel           : integer;
  max_kernel_river     : integer;
  calibrate            : Boolean;
  LScor                : double;
  ext                  : string;
  cal     : TCalibration;
  forced_routing : array Of TForcedRouting;

  river_adjectant      : TIntKVArray;
  river_upstream      : TIntKVArray;
  river_upstream_proportion: TDoubleKVArray;

  LModel: TLModel;
  SModel: TSModel;
  TCModel: TTCModel;



  {Buffers}
  BufferData: TBufferDataArray;
  {End Parameters to be read form ini-file--------------------------------------}

  DTM, CNmap, LU, ReMap, RunoffTotMap, SewerMap, cumulative: Rraster;
  PRC, TilDir, Ro, BufferMap, Outlet, RivSeg, Ditch_map, Dam_map, PTEFmap, river_routing_map: GRaster;
  i, j, lowOutletX, lowOutletY: integer;

  ROW, COLUMN : TIntarray;

  Slope,Aspect,Uparea,LS: Rraster;
  totsurface: double;

  Routing: TRoutingArray;
  OutletArray : TIntMatrix;
  RainData: TRainRecordArray;

const
  { These values are used in the landuse grid. Parcels get a value >0 }
  OUTSIDE_STUDY_AREA = 0;
  RIVER = -1;
  INFRASTRUCTURE = -2;
  FOREST = -3;
  PASTURE = -4;
  OPEN_WATER = -5;
  GRASS_STRIP = -6;

Implementation


Function TKVArray.getItem(k: integer): T;
var
  i: integer;
begin
  for i:=low(Self.key) to high(Self.key) do
    begin
      if self.key[i] = k then
        getItem:= self.value[i];
    end;
  getItem:=0;
end;


Function Distance1(r: TRoutingArray;i,j: integer):double;
begin
  if ((r[i,j].Target1Row <> -99) and (r[i,j].Target1Col <> -99)) Then
     Distance1 := res * sqrt(sqr(i - r[i,j].Target1Row) + sqr(j -r[i,j].Target1Col))
  else Distance1 := 0;

end;

Function Distance2(r: TRoutingArray;i,j: integer):double;
begin
 if ((r[i,j].Target2Row <> -99) and (r[i,j].Target2Col <> -99)) Then
    Distance2 := res * sqrt(sqr(i - r[i,j].Target2Row) + sqr(j -r[i,j].Target2Col))
 else
    Distance2 := 0;
end;

Function CheckExtrema_RRaster(inrst: RRaster; minval,maxval: integer):boolean;
begin
   CheckExtrema_RRaster := True;
   For i := 1 To nrow Do
       For j := 1 To ncol Do
          Begin
            If (PRC[i,j] <> 0) And ((inrst[i,j] > maxval) Or (inrst[i,j] < minval)) Then
               CheckExtrema_RRaster:=False
          End;
end;

Function CheckExtrema_GRaster(inrst: GRaster; minval,maxval: integer):boolean;
begin
   CheckExtrema_GRaster := True;
   For i := 1 To nrow Do
       For j := 1 To ncol Do
          Begin
            If (PRC[i,j] <> 0) And ((inrst[i,j] > maxval) Or (inrst[i,j] < minval)) Then
               CheckExtrema_GRaster:=False
          End;
end;

Procedure ReadInRasters;
Begin
  setCurrentDir(datadir);
  GetRFile(DTM, DTM_Filename);
  GetGFile(PRC, PARCEL_filename);

  GetRFile(P_factor, Pf_Data_filename);

  If Include_sewer Then
    Begin
    GetRFile(SewerMap, Sewerfilename);
    if not CheckExtrema_RRaster(SewerMap, 0, 1) Then
       raise EInputException.Create('Error in data input: SewerMap contains values out of range (0-1)');
    end;

  If curve_number Then
    Begin
      GetRfile(CNmap, CNmapfilename);
      if not CheckExtrema_RRaster(CNmap, 0, 100) Then
         raise EInputException.Create('Error in data input: CN contains values out of range (0-100)');
    End;

  If not topo Then // als topo = false wordt de ploegrichting in rekening gebracht
    Begin
      GetGFile(TilDir, TilDirFilename);
      if not CheckExtrema_GRaster(TilDir, 0, 360) Then
         raise EInputException.Create('Error in data input: Tilage Direction contains values out of range (0-360)');
      GetGfile(Ro, RoFilename);
    End;

  If not OnlyRouting Then
    Begin
         GetGFile(K_factor, K_Factor_filename);
         GetRFile(C_factor, Cf_Data_filename);
         if not CheckExtrema_RRaster(C_factor, 0, 1) Then
           raise EInputException.Create('Error in data input: C-factor contains values out of range (0-1)');

         If not calibrate Then
          If Create_ktc Then
            Create_ktc_map(ktc)
          Else
            GetRFile(ktc, ktc_Data_filename);

         if Calc_tileros then
          begin
             If Create_ktil Then
                Create_ktil_map(ktil)
             Else
                GetGFile(ktil, ktil_Data_filename);
          end;

    end;

  //If buffers are taken into account the buffermap is loaded
  If Include_buffer Then
    Begin
      GetGfile(BufferMap, Bufferfilename);
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

  If Include_ditch Then
    GetGfile(Ditch_map,Ditch_filename);

  If Include_dam Then
    GetGfile(Dam_map,Dam_filename);

  If outlet_select Then
    GetGfile(Outlet, Outletfilename);

  If segments or river_routing Then
    GetGfile(RivSeg, Riversegment_filename);


  If river_routing then
    begin
    GetGfile(river_routing_map, river_routing_filename);
    SetDynamicRdata(cumulative);
    ReadDownstreamSegments(river_adjectant_filename, river_adjectant);
    ReadUpstreamSegments(river_upstream_filename, river_upstream, river_upstream_proportion);
    end;

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

  writeSmallintFile(ncol,nrow,datadir+'PTEFmap'+ext, PTEFmap);

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

Procedure Release_Memory;

Begin
  // Release memory for input rasters
  DisposeDynamicRdata(DTM);
  DisposeDynamicGdata(PRC);

  If Include_sewer Then
   Begin
    DisposedynamicRData(SewerMap);
      DisposeDynamicRdata(SEWER_IN);
   end;


  If curve_number Then
    Begin
      DisposedynamicRData(CNmap);
    End;

  If topo = false Then // als topo = false wordt de ploegrichting in rekening gebracht
    Begin
      DisposedynamicGData(TilDir);
      DisposedynamicGData(Ro);
    End;

  DisposeDynamicRdata(ktc);
  DisposeDynamicGData(Buffermap);

  If Include_ditch = true Then
    DisposeDynamicGData(Ditch_map);

  If Include_dam = true Then
    DisposeDynamicGData(Dam_map);

  DisposeDynamicGData(Outlet);

  If segments or river_routing Then
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
  DisposeDynamicRdata(SEDI_IN);
  DisposeDynamicRdata(SEDI_OUT);
  DisposeDynamicRdata(CAPAC);

  if Calc_tileros Then
   Begin
   DisposeDynamicRdata(SEDTIL_IN);
   DisposeDynamicRdata(SEDTIL_OUT);
    DisposeDynamicRdata(TILEROS);
    DisposeDynamicGdata(ktil);
   end;

  If curve_number Then
    Begin
      DisposedynamicRData(RunoffTotMap);
      DisposedynamicRData(Remap);
    End;

End;

Function SetFileFromIni(inifile: Tinifile;inivalue,  datadir: string; obliged: boolean): string;
Var
 Default, filename: string;
Begin
  Default:='';
  if obliged Then
    Begin
    filename := Inifile.Readstring('Files', inivalue, Default);
    if (filename = '') Then
      raise EInputException.Create('Error: '+ inivalue+' not defined in inifile');
    If (FileExists(datadir + filename)) Then SetFileFromIni := datadir  + filename
    Else If FileExists(filename) Then
       SetFileFromIni := filename
    Else
      raise EInputException.Create('Error in data input: ' + inivalue + ' not found in '+ datadir+ ' or path');
    End
  Else
     SetFileFromIni := '';
End;

Procedure ReadDownstreamSegments(filename: String; var kvArray: tintkvarray);
var
  table: TStrings;
  i: integer;
  split: TStringArray;
Begin
  table := TStringList.Create();
  table.LoadFromFile(filename);
  kvarray := tintkvarray.create();
  setlength(kvarray.key, table.Count);
  SetLength(kvarray.value, table.Count);

  for i:=1 to table.Count -1 do
    begin
      split := table[i].split(#9);  // this is the tab character
      kvarray.key[i] := StrToInt(trim(split[0])); // note we have to use 1-based indexing
      kvarray.value[i] :=  StrToInt(trim(split[1]));
    end;
End;

Procedure ReadUpstreamSegments(filename: String; var upstream: tintkvarray;var proportion: tdoublekvarray);
var
  table: TStrings;
  i: integer;
  split: TStringArray;
Begin
  table := TStringList.Create();
  table.LoadFromFile(filename);
  upstream := tintkvarray.create();
  proportion := Tdoublekvarray.create();
  setlength(upstream.key, table.Count);
  SetLength(upstream.value, table.Count);
  setlength(proportion.key, table.Count);
  SetLength(proportion.value, table.Count);

  for i:=1 to table.Count -1 do
    begin
      split := table[i].split(#9);  // this is the tab character
      upstream.key[i] := StrToInt(trim(split[0])); // note we have to use 1-based indexing
      upstream.value[i] :=  StrToInt(trim(split[1]));
      proportion.key[i] := StrToInt(trim(split[0])); // note we have to use 1-based indexing
      proportion.value[i] :=  StrToFloat(trim(split[2]));
    end;
End;


//******************************************************************************
//This procedure reads the .ini file and assigns the file + location + values
//to the correct variables.
//******************************************************************************
Procedure Readsettings(INI_filename:String);
Var
  Inifile: Tinifile;
  Default, Buffername, inistring, routingname: string;
  i: integer;
  number_of_forced_routing: integer;
Begin
  Default:='';

  INI_filename := ExpandFileName(INI_filename);
  If Not FileExists(INI_filename) Then
    raise Exception.create('Inifile does not exist');
  Inifile := Tinifile.create(INI_filename);

  Datadir := Inifile.readstring('Working directories', 'Input directory', Default);
  Datadir := expandfilename(datadir);

  If Not DirectoryExists(Datadir) Then
    raise EInputException.Create('Error: data directory not found: ' + Datadir);
  File_output_dir := Inifile.readstring('Working directories', 'Output directory', Default);
  File_output_dir:=ExpandFileName(File_output_dir);
  If Not DirectoryExists(File_output_dir) Then ForceDirectories(File_output_dir);

  // Make sure that a trailing slash is added to the input/output dir
  File_output_dir := IncludeTrailingPathdelimiter(File_output_dir);
  Datadir := IncludeTrailingPathdelimiter(Datadir);

  OnlyRouting := Inifile.ReadBool('Options', 'Only Routing', false);

  curve_number := OnlyRouting or Inifile.ReadBool('Extensions','Curve Number',false);

  Include_sewer:= Inifile.ReadBool('Extensions','Include sewers',false);

  If Include_sewer And Not TryStrToInt(Inifile.Readstring('Parameters extensions', 'Sewer exit', Default),
   sewer_exit) Then
    Begin
      raise EInputException.Create('Error in data input: Sewer exit system value missing or wrong data format');
    End;

  If Not OnlyRouting Then
    Begin
     // Use_Rfactor:= true;
     Create_ktc := Inifile.ReadBool('Extensions','Create ktc map', false);
     Calc_tileros := Inifile.ReadBool('Options', 'Calculate Tillage Erosion', false);
     if Calc_tileros then
        Create_ktil := Inifile.ReadBool('Extensions','Create ktil map',false);
     est_clay:= Inifile.ReadBool('Extensions','Estimate clay content',false);
     calibrate :=  inifile.ReadBool('Extensions', 'Calibrate', false);
    end
  Else
    Begin
      // Use_Rfactor := false;
      Create_ktc := false;
      Create_ktil := false;
      calibrate := false;
    End;


  Inc_tillage:=Inifile.ReadBool('Extensions','Include tillage direction',false);
  topo := not Inc_tillage;
  Include_buffer := Inifile.ReadBool('Extensions','Include buffers',false);
  Include_ditch := Inifile.ReadBool('Extensions','Include ditches',false);
  Include_dam := Inifile.ReadBool('Extensions','Include dams',false);

  segments := Inifile.ReadBool('Extensions','Output per river segment',false);
  max_kernel := Inifile.ReadInteger('Parameters', 'Max kernel', 50);
  max_kernel_river := Inifile.ReadInteger('Parameters', 'Max kernel river', 100);

  adjusted_slope := inifile.ReadBool('Extensions', 'Adjusted Slope', false);

  if include_buffer then
     buffer_reduce_upstream_area := inifile.ReadBool('Extensions', 'Buffer reduce Area', false);

  force_routing := inifile.ReadBool('Extensions', 'Force Routing', false);

  river_routing := inifile.ReadBool('Extensions', 'River Routing', false);
  if river_routing then
   segments := true;

  cardinal_routing_to_river := inifile.ReadBool('Extensions', 'Cardinal Routing River', true);


  inistring:= Inifile.ReadString('Options', 'L model', 'Desmet1996_Vanoost2003');
  Lmodel := TLModel(GetEnumValue(Typeinfo(TLModel), inistring));
  if Lmodel > high(TLModel) then
    raise EInputException.Create('invalid L model: '+ inistring);

  inistring:= Inifile.ReadString('Options', 'S model', 'Nearing1997');
  Smodel := TSModel(GetEnumValue(Typeinfo(TSModel), inistring));
  if Smodel > high(TSModel) then
    raise EInputException.Create('invalid S model: '+ inistring);

  inistring:= Inifile.ReadString('Options', 'TC model', 'VanOost2000');
  TCmodel := TTCModel(GetEnumValue(Typeinfo(TTCModel), inistring));
  if TCmodel > high(TTCModel) then
    raise EInputException.Create('invalid TC model: '+ inistring);

  Outlet_select:= Inifile.ReadBool('Extensions','Manual outlet selection',false);

  {Filenames}
  DTM_filename := SetFileFromIni(Inifile, 'DTM filename', datadir, True);
  PARCEL_filename := SetFileFromIni(Inifile, 'Parcel filename', datadir, True);
  Sewerfilename :=SetFileFromIni(Inifile, 'Sewer map filename', datadir, Include_sewer);
  TilDirfilename := SetFileFromIni(Inifile, 'Tillage direction filename', datadir, Inc_tillage);
  Rofilename := SetFileFromIni(Inifile, 'Oriented roughness filename', datadir, Inc_Tillage);
  BufferFilename := SetFileFromIni(Inifile, 'Buffer map filename', datadir, Include_buffer);
  Ditch_filename := SetFileFromIni(Inifile, 'Ditch map filename', datadir, Include_ditch);
  Dam_filename := SetFileFromIni(Inifile, 'Dam map filename', datadir, Include_dam);
  Pf_data_filename :=SetFileFromIni(Inifile, 'P factor map filename', datadir, true);
  Riversegment_filename := SetFileFromIni(Inifile, 'River segment filename', datadir, segments);
  Outletfilename := SetFileFromIni(Inifile, 'Outlet map filename', datadir, Outlet_select);
  river_adjectant_filename:=SetFileFromIni(Inifile, 'adjectant segments', datadir, river_routing);
  river_upstream_filename:=SetFileFromIni(Inifile, 'upstream segments', datadir, river_routing);
  river_routing_filename := SetFileFromIni(Inifile, 'river routing filename', datadir, river_routing);
  CNmapfilename := SetFileFromIni(Inifile, 'CN map filename', datadir, curve_number);

  if not OnlyRouting then
    Begin
    ktil_Data_Filename := SetFileFromIni(Inifile, 'ktil map filename', datadir, (not Create_ktil and Calc_tileros));
    Rainfallfilename := SetFileFromIni(Inifile, 'Rainfall filename', datadir, curve_number);
    K_Factor_filename := SetFileFromIni(Inifile, 'K factor filename', datadir, True);
    Cf_data_filename := SetFileFromIni(Inifile, 'C factor map filename', datadir, True);

    if not calibrate then
    ktc_Data_Filename := SetFileFromIni(Inifile, 'ktc map filename', datadir, (not Create_ktc));

    end;

  If (est_clay) And Not (TryStrToFloat(Inifile.Readstring('Parameters extensions',
     'Clay content parent material', Default), clay_parent)) Then
    Begin
      raise EInputException.Create('Error in data input: Clay content parent material value missing or wrong data format');
      ;
    End;

  If Outlet_select And Not (FileExists(Outletfilename)) Then
    Begin
      raise EInputException.Create('Error in data input: Outlet map file not found in '+datadir);
    End;
  Convert_output := Inifile.ReadBool('Extensions','Convert output',false);

  {Output maps}
  Saga_Grids := Inifile.ReadBool('Output', 'Export .sgrd grids', false);
  if saga_grids then ext:='.sdat' else ext:='.rst';
  Write_ASPECT := Inifile.ReadBool('Output','Write aspect',false);
  Write_LS := Inifile.ReadBool('Output','Write LS factor',false);
  Write_UPAREA := Inifile.ReadBool('Output','Write upstream area',false);
  Write_SLOPE := Inifile.ReadBool('Output','Write slope',false);
  Write_Routing := Inifile.ReadBool('Output','Write routing table',false);

  // Writes a file (tab seperated) with rows and cols of pixels in the order
  Write_Routing_CR := Inifile.ReadBool('Output', 'Write routing column/row', false);

  If OnlyRouting Then
    Begin
      Write_RUSLE := false;
      Write_WATEREROS := false;
    end
  Else
      Begin
        Write_RUSLE := Inifile.ReadBool('Output','Write RUSLE',false);
        Write_Sediexport := Inifile.ReadBool('Output','Write sediment export',false);
        Write_WATEREROS := Inifile.ReadBool('Output','Write water erosion',false);

        If not curve_number Then
          Begin
            Write_RE := false;
            Write_TOTRUN := false;
          End
        Else
          Begin
            Write_RE := Inifile.ReadBool('Output','Write rainfall excess',false);
            Write_TOTRUN := Inifile.ReadBool('Output','Write total runoff',false);
          End;
      End;

  {Variables}
  //if Use_Rfactor Then
  Begin
  If Not TryStrToFloat(Inifile.Readstring('Parameters', 'R factor', Default),Rfactor) Then
          raise EInputException.Create('Error in data input: R factor value missing or wrong data format');
      Rfactor := Rfactor / 10000;
  // in MJ.mm/m².h.year
  end;

  If curve_number Then
    Begin
      If Not TryStrToFloat(Inifile.Readstring('Parameters extensions', '5-day antecedent rainfall',
         Default), AR5) Then
          raise EInputException.Create('Error in data input: AR5 value missing or wrong data format');

      If Not TryStrToFloat(Inifile.Readstring('Parameters extensions', 'Stream velocity', Default), riv_vel)
        Then
          raise EInputException.Create('Error in data input: Stream velocity value missing or wrong data format');

      If Not TryStrToFloat(Inifile.Readstring('Parameters extensions', 'Alpha', Default), alpha) Then
          raise EInputException.Create('Error in data input: alpha value missing or wrong data format');

      If Not TryStrToFloat(Inifile.Readstring('Parameters extensions', 'Beta', Default), beta) Then
          raise EInputException.Create('Error in data input: beta value missing or wrong data format');

      If Not TryStrToInt(Inifile.Readstring('Parameters extensions', 'Bulk density', Default), BD) Then
          raise EInputException.Create('Error in data input: BD value missing or wrong data format');

    End
  Else
   If not OnlyRouting Then
    Begin
      If Not TryStrToInt(Inifile.Readstring('Parameters', 'Bulk density', Default), BD) Then
          raise EInputException.Create('Error in data input: BD value missing or wrong data format');
    End;

  If Not TryStrToFloat(Inifile.Readstring('Parameters extensions', 'LS correction', '1'), LSCor) Then
          raise EInputException.Create('Error in data input: LS correction factor missing or wrong data format');

  If (Include_buffer) And Not (TryStrToInt(inifile.readstring('Parameters extensions', 'Number of buffers',
     Default), Number_of_Buffers)) Then
      raise EInputException.Create('Error in data input: Number of buffers value missing or wrong data format');

  If (Force_routing) And Not (TryStrToInt(inifile.readstring('Parameters extensions', 'Number of Forced Routing',
     Default), number_of_forced_routing)) Then
      raise EInputException.Create('Error in data input: Number of Forced Routing value missing or wrong data format');

  If not OnlyRouting Then
   Begin
    if not calibrate then
      begin
          If (create_ktc) And Not
             (TryStrToFloat(Inifile.Readstring('Parameters extensions', 'ktc low', Default),ktc_low)) Then
               raise EInputException.Create('Error in data input: ktc low value missing or wrong data format');
          If (create_ktc) And Not
             (TryStrToFloat(Inifile.Readstring('Parameters extensions', 'ktc high', Default), ktc_high)) Then
               raise EInputException.Create('Error in data input: ktc high value missing or wrong data format');

      end;
    If (create_ktc or calibrate) And Not
              (TryStrToFloat(Inifile.Readstring('Parameters extensions', 'ktc limit', Default), ktc_limit)) Then
               raise EInputException.Create('Error in data input: ktc limit value missing or wrong data format');

    If (create_ktil) And Not (TryStrToInt(Inifile.Readstring('Parameters extensions', 'ktil default', Default),
       ktil_Default)) Then
      Begin
        raise EInputException.Create('Error in data input: ktil default value missing or wrong data format');
      End;
    If (create_ktil) And Not (TryStrToFloat(Inifile.Readstring('Parameters extensions', 'ktil threshold',
       Default), ktil_threshold)) Then
      Begin
       raise EInputException.Create('Error in data input: ktil threshold value missing or wrong data format');
      End;
    If Not TryStrToInt(Inifile.Readstring('Parameters', 'Parcel connectivity cropland', Default),
       TFSED_crop) Then
      Begin
        raise EInputException.Create('Error in data input: Parcel connectivity cropland value missing or wrong data format')
        ;
      End;
    If Not TryStrToInt(Inifile.Readstring('Parameters', 'Parcel connectivity grasstrips', '100'),
       TFSED_grasstrip) Then
      Begin
        raise EInputException.Create('Error in data input: Parcel connectivity grasstrips value missing or wrong data format')
        ;
      End;
   end;

  If Not TryStrToInt(Inifile.Readstring('Parameters', 'Parcel connectivity forest', Default),
     TFSED_forest) Then
    Begin
      raise EInputException.Create(
        'Error in data input: Parcel connectivity forest/pasture value missing or wrong data format');
    End;
  If Not TryStrToInt(Inifile.Readstring('Parameters', 'Parcel trapping efficiency cropland',
     Default), PTEFValueCropland) Then
      raise EInputException.Create(
      'Error in data input: Parcel trapping efficiency (cropland) value missing or wrong data format');
  If Not TryStrToInt(Inifile.Readstring('Parameters', 'Parcel trapping efficiency forest', Default)
     , PTEFValueForest) Then
    raise EInputException.Create(
       'Error in data input: Parcel trapping efficiency (forest) value missing or wrong data format');
      ;

  If Not TryStrToInt(Inifile.Readstring('Parameters', 'Parcel trapping efficiency pasture', Default
     ), PTEFValuePasture) Then
      raise EInputException.Create(
      'Error in data input: Parcel trapping efficiency (pasture) value missing or wrong data format')
      ;
  If curve_number Then
    Begin
      If Not TryStrToInt(inifile.readstring('Parameters extensions', 'Desired timestep for model', Default),
         Timestep_model) Then
          raise EInputException.Create('Error in data input: Desired timestep for model value missing or wrong data format')
          ;
      If (Convert_output) And Not TryStrToInt(inifile.readstring('Parameters extensions',
         'Final timestep output', Default), Timestep_output) Then
          raise EInputException.Create(
          'Error in data input: Final timestep output value missing or wrong data format')
          ;

      If Not TryStrToInt(inifile.readstring('Parameters extensions', 'Endtime model', Default), Endtime_model)
    Then
      raise EInputException.Create(
      'Error in data input: Endtime model value missing or wrong data format');
    End;



  If force_routing Then
    Begin
        setlength(forced_routing, number_of_forced_routing);
    for i:=0 to number_of_forced_routing-1 do
      begin;
        routingname:='Forced Routing ' + IntToStr(i+1);

        forced_routing[i].FromCol := inifile.ReadInteger(routingname, 'from col',-99);
        TryStrToInt(inifile.readstring(routingname, 'from col', Default), forced_routing[i].FromCol);
        TryStrToInt(inifile.readstring(routingname, 'from row', Default), forced_routing[i].FromRow);
        TryStrToInt(inifile.readstring(routingname, 'target col', Default), forced_routing[i].TargetCol);
        TryStrToInt(inifile.readstring(routingname, 'target row', Default), forced_routing[i].TargetRow);
      end;
    end;


  If Include_buffer Then
    Begin
      setlength(Bufferdata, Number_of_Buffers + 1);
      For i := 1 To Number_of_Buffers Do
        Begin
          Buffername := 'Buffer ' + IntToStr(i);

          If Not TryStrToFloat(inifile.readstring(Buffername, 'Trapping efficiency', Default),
             Bufferdata[i].PTEF) Then
              raise EInputException.Create( 'Error in data input: Buffer '+intToStr(i)+
                            ' trapping efficiency value missing or wrong data format');
          If Not TryStrToInt(inifile.readstring(Buffername, 'Extension ID', Default), Bufferdata[i
             ].ext_ID) Then
              raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                            ' extension ID missing or wrong data format');

          If curve_number Then
            Begin

            If Not TryStrToFloat(inifile.readstring(Buffername, 'Volume', Default), Bufferdata[i].
               Volume) Then
                raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                              ' volume value missing or wrong data format');

            If Not TryStrToFloat(inifile.readstring(Buffername, 'Height dam', Default),Bufferdata[i]
               .Height_dam) Then
              raise EInputException.Create(
                    'Error in data input: Buffer '+intToStr(i)+
                    ' height dam value missing or wrong data format');

            If Not TryStrToFloat(inifile.readstring(Buffername, 'Height opening', Default),
               Bufferdata[i].Height_opening) Then
                raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                              ' height opening value missing or wrong data format');

            If Not  TryStrToFloat(inifile.readstring(Buffername, 'Opening area', Default),Bufferdata
               [i].Opening_area) Then
                raise EInputException.Create(
                'Error in data input: Buffer '+intToStr(i)+
                              ' opening area value missing or wrong data format');

            If Not TryStrToFloat(inifile.readstring(Buffername, 'Discharge coefficient', Default),
               Bufferdata[i].Cd) Then
                raise EInputException.Create(
                  'Error in data input: Buffer '+intToStr(i)+
                  ' discharge coefficient value missing or wrong data format');

            If Not TryStrToFloat(inifile.readstring(Buffername, 'Width dam', Default), Bufferdata[i]
               .width_dam) Then
                raise EInputException.Create('Error in data input: Buffer '+intToStr(i)+
                              ' width dam value missing or wrong data format');

            If Bufferdata[i].Height_opening > Bufferdata[i].Height_dam Then
              raise EInputException.Create(
                'Error in buffer input: the height of the opening cannot be larger than' +
                ' the height of the dam. Please insert correct values.');

            End;
        End;
    End;

  {cal}

  If calibrate Then
    Begin

      cal.KTcHigh_lower:=inifile.ReadFloat('Calibration','KTcHigh_lower', 5);
      cal.KTcHigh_upper:=inifile.ReadFloat('Calibration','KTcHigh_upper', 40);
      cal.KTcLow_lower:=inifile.ReadFloat('Calibration','KTcLow_lower', 1);
      cal.KTcLow_upper:=inifile.ReadFloat('Calibration','KTcLow_upper', 20);
      cal.steps:=Inifile.ReadInteger('Calibration', 'steps', 12);
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
  {$push}{$warn 5091 off} // remove spurious warning about datatype not being initialized
  SetDynamicGData(M);
  {$pop}
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
  Setlength(CNmap.r,NrowPRC+1, NColPRC+1);
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
  writefloatfile(ncolPRC,nrowPRC,datadir+'\CNmap' + ext,CNmap);
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

  writeSmallintFile(ncol,nrow,datadir+'ktilmap'+ext,ktil);
End;

Procedure Create_ktc_map(Var ktc: RRaster);

Var
  i,j: integer;
Begin
  SetDynamicRData(ktc);
  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If C_factor[i,j] <= ktc_limit +0.000001 Then
          ktc[i,j] := ktc_low
        Else
          ktc[i,j] := ktc_high;
        // In WS, cells with PRC == -2 or -1 receive a Ktc value of 9999
        If (PRC[i,j] = -2) Or (PRC[i,j] = -1) Then
          ktc[i,j] := 9999;
        // kTc of open water = 0 because sediment can't go any further once it's there
        If PRC[i,j] = -5 Then
          ktc[i,j] := 9999;
      End;

  writefloatfile(ncol,nrow,datadir+'ktcmap'+ext,ktc);
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
