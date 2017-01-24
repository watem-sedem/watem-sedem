unit ReadInParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RData_CN, GData_CN, Inifiles, Dialogs, Idrisi;
Procedure ReadInRasters;
Procedure Allocate_Memory;
Procedure Release_Memory;
procedure Readsettings(INI_filename:string);
procedure Create_ktil_map(var ktil: GRaster);
procedure Create_ktc_map(var ktc: GRaster);


//Record for model variables
Type
Gvector = array of smallint;
Rvector = array of single;

  TSingleArray = array of array of single;
  TDoubleArray = array of array of double;
  TIntArray = array of array of integer;

  // buffer data
  TBufferData = record //Record containing inputdata for buffers
    Volume, Volume_dead, height_dam, height_opening, Opening_area, Qmax, Cd, area, width_dam, PTEF: double;
    row, col, ext_ID: integer;
  end;
  TBufferDataArray = array of TBufferData; //A vector is created

  TRainRecord = Record //Record containing information about rainfall input
    ID: integer; //From 1 till number of time steps
    Time: Integer; //in seconds
    Rain: Double; // in mm
    Rain_fraction: double; //Fraction of rainfall that falls in each time step
  end;

  TRainRecordArray = array of TRainRecord; //Record is converted to 2D matrix

TRouting = record //Record containing information about runoff routing for each cell
     One_Target: boolean; //Can I delete this???
     Target1Row, Target1Col, Target2Row, Target2Col: integer;
     Part1, Part2, Distance1, Distance2: double;
     end;
TRoutingArray = array of array of TRouting; //Record is converted to 3D matrix


var
  //internal variables
  sedprod,depprod: double;
  WATEREROS     : RRaster;                      //water erosion (unit: mm)
  WATEREROS_cubmeter: RRaster;                  // water erosion (unit: m³)
  WATEREROS_kg: RRaster;                  // water erosion (unit: kg)
  TILEROS       : RRaster;                      //tillage erosion (unit: mm)
  RUSLE         : RRaster;                      // result of RUSLE equation in kg/m² = POTENTIAL soil loss
  SEDI_EXPORT   : RRaster;            // sediment export in m³
  SEDI_EXPORT_kg   : RRaster;          // sediment export in kg
  SEDI_IN2       : RRaster;            // incoming sediment in kg
  SEDI_OUT2      :RRaster;             // outgoing sediment in kg
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
  PARCEL_filename      : string;       {unit id, 1 to 3200 for parcels, -1 for river, 0 for outside area, -2 for roads, -3 for forest, -4 for pasture, -5 for ponds}
  Rainfallfilename     : string;
  Sewerfilename        : string;       {indicates location of sewers. Value = efficiency for capturing water/sediment (type: double)}
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
  PTefValueCropland    : integer;
  PTefValueForest      : integer;
  PTefValuePasture     : integer;
  Timestep_model       : integer;
  EndTime_model        : integer;
  Timestep_output     : integer;
  {Buffers}
  BufferData: TBufferDataArray;
  {End Parameters to be read form ini-file--------------------------------------}


  PRC, DTM, CNmap, LU, ReMap, RunoffTotMap, SewerMap: Rraster;
  TilDir, Ro ,BufferMap, Outlet, RivSeg, Ditch_map, Dam_map, PTEFmap: GRaster;
  i, j, lowOutletX, lowOutletY: integer;


  ROW, COLUMN : Gvector;

  Slope,Aspect,Uparea,LS: Rraster;
  totsurface:double;

  Routing: TRoutingArray;
  OutletArray : TIntArray;
  RainData: TRainRecordArray;

  errorFlag : Boolean;
  errorDummy : string;


implementation

Procedure ReadInRasters;
begin
  SetCurrentDir(datadir);

  GetRFile(DTM,DTM_Filename);
  GetRFile(PRC,PARCEL_filename);

    if Include_sewer then
    GetRFile(SewerMap,Sewerfilename);

      if topo = false then // als topo = false wordt de ploegrichting in rekening gebracht
  begin
    GetGFile(TilDir, TilDirFilename);
    GetGfile(Ro, RoFilename);
  end;

  if not Simplified then
  begin
       GetRfile(CNmap, CNmapfilename);
  end;

  GetGFile(K_factor,K_Factor_filename);
  GetRFile(C_factor, Cf_Data_filename);
  GetRFile(P_factor, Pf_Data_filename);

  if Create_ktc then
    Create_ktc_map(ktc)
  else
    GetGFile(ktc, ktc_Data_filename);

  if Create_ktil then
    Create_ktil_map(ktil)
  else
    GetGFile(ktil, ktil_Data_filename);

  //If buffers are taken into account the buffermap is loaded
  if Include_buffer = true then
  begin
  GetGfile(BufferMap,Bufferfilename);
  for i := 1 to nrow do //The row and column of every buffer are stored in the record
  for j := 1 to ncol do
  begin
     if (Buffermap[i,j] <> 0) AND (Buffermap[i,j] <= Number_of_Buffers) then
     begin
       BufferData[Buffermap[i,j]].row := i;
       BufferData[Buffermap[i,j]].col := j;
     end;
  end;
  end
  else
  begin
     SetDynamicGData(Buffermap);
     SetzeroG(Buffermap);
  end;

  if Include_ditch = true then
    GetGfile(Ditch_map,Ditch_filename);

  if Include_dam = true then
    GetGfile(Dam_map,Dam_filename);

  if outlet_select then
  GetGfile(Outlet, Outletfilename);

  if VHA then
  GetGfile(RivSeg, Riversegment_filename);

  // PTEF map is created
SetDynamicGdata(PTEFmap);
setzeroG(PTEFmap);
for i := 1 to nrow do //The row and column of every buffer are stored in the record
  for j := 1 to ncol do
      begin
           case round(PRC[i,j]) of //PRC = GRaster, dus integers.
                 -6: PTEFmap[i,j] := PTEFValuePasture;
                 -4: PTEFmap[i,j] := PTEFValuePasture;
                 -3: PTEFmap[i,j] := PTEFValueForest;
                 1..10000000000: PTEFmap[i,j] := PTEFValueCropland;
                 else PTEFmap[i,j] := 0;
           end;
           end;

writeGidrisi32file(ncol,nrow,datadir+'PTEFmap'+'.rst', PTEFmap);

end;


Procedure Allocate_Memory;
var
z : integer;
begin
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

end;

Procedure Release_Memory;
var
i : integer;
begin
// Release memory for input rasters
DisposeDynamicRdata(DTM);
DisposeDynamicRdata(PRC);
if Include_sewer then
    DisposedynamicRData(SewerMap);

if not simplified then
begin
DisposedynamicRData(CNmap);
end;

  if topo = false then // als topo = false wordt de ploegrichting in rekening gebracht
     begin
        DisposedynamicGData(TilDir);
        DisposedynamicGData(Ro);
     end;

  DisposeDynamicGdata(K_Factor);
  DisposeDynamicRdata(C_factor);
  DisposeDynamicRdata(P_factor);
  DisposeDynamicGdata(ktc);
  DisposeDynamicGdata(ktil);
  DisposeDynamicGData(Buffermap);

  if Include_ditch = true then
     DisposeDynamicGData(Ditch_map);

    if Include_dam = true then
     DisposeDynamicGData(Dam_map);

     if outlet_select then
    DisposeDynamicGData(Outlet);

 if VHA then
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

if not Simplified then
  begin
    DisposedynamicRData(RunoffTotMap);
    DisposedynamicRData(Remap);
  end;

end;

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
  SetcurrentDir(datadir);
  File_output_dir := Inifile.readstring('Working directories', 'Output directory', Dummy_str);
  if not DirectoryExists(File_output_dir) then CreateDir(File_output_dir);
   // If the last character in the output directory is not a "\" this is added
  if (File_output_dir[Length(File_output_dir)] <> '\') then
     File_output_dir := IncludeTrailingBackslash(File_output_dir);

  {Filenames}
  INIfilename := Inifile.Readstring('Files', '.INI filename', Dummy_str);
  DTM_filename := Inifile.Readstring('Files', 'DTM filename', Dummy_str);
  if not (FileExists(DTM_filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: DTM file not found in '+datadir;
     end;
  PARCEL_filename := Inifile.Readstring('Files', 'Parcel filename', Dummy_str);
    if not (FileExists(PARCEL_filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Parcel file not found in '+datadir;
     end;
  Sewerfilename := Inifile.Readstring('Files', 'Sewer map filename', Dummy_str);
  CNmapfilename := Inifile.Readstring('Files', 'CN map filename', Dummy_str);
  TilDirfilename := Inifile.Readstring('Files', 'Tillage direction filename', Dummy_str);
  Rofilename := Inifile.Readstring('Files', 'Oriented roughness filename', Dummy_str);
  BufferFilename := inifile.readstring('Files', 'Buffer map filename', Dummy_str);
  Ditch_filename := inifile.readstring('Files', 'Ditch map filename', Dummy_str);
  Dam_filename := inifile.readstring('Files', 'Dam map filename', Dummy_str);
  K_Factor_filename := inifile.readstring('Files', 'K factor filename', Dummy_str);
   if not (FileExists(K_Factor_filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: K factor file not found in '+datadir;
     end;
  Cf_Data_Filename := inifile.readstring('Files', 'C factor map filename', Dummy_str);
   if not (FileExists(Cf_Data_Filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: C factor file not found in '+datadir;
     end;
  Pf_Data_Filename := inifile.readstring('Files', 'P factor map filename', Dummy_str);
  if not (FileExists(Pf_Data_Filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: P factor file not found in '+datadir;
     end;
  ktc_Data_Filename := inifile.readstring('Files', 'ktc map filename', Dummy_str);
  ktil_Data_Filename := inifile.readstring('Files', 'ktil map filename', Dummy_str);
  Outletfilename := inifile.readstring('Files', 'Outlet map filename', Dummy_str);
  Riversegment_filename := inifile.readstring('Files', 'River segment filename', Dummy_str);

  {User choices}
  if (Inifile.ReadBool('User Choices','Simplified model version',false))=true then Simplified := true else Simplified:= false;
  if not simplified then
    begin
      if not (FileExists(CNmapfilename)) then
         begin
           errorFlag := True;
           errorDummy := 'Error in data input: CN map file not found in '+datadir;
         end;
    end;
  if (Inifile.ReadBool('User Choices','Use R factor',false))=true then Use_Rfactor := true else Use_Rfactor:=false;
  if not Use_Rfactor then
    begin
      Rainfallfilename := Inifile.Readstring('Files', 'Rainfall filename', Dummy_str);
        if not (FileExists(Rainfallfilename)) then
         begin
           errorFlag := True;
           errorDummy := 'Error in data input: Rainfall file not found in '+datadir;
         end;
    end;
  if (Inifile.ReadBool('User Choices','Include sewers',false))=true then Include_sewer := true else Include_sewer:=false;
  if Include_sewer AND not (FileExists(Sewerfilename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Sewer map file not found in '+datadir;
     end;
  if Include_sewer AND not TryStrToInt(Inifile.Readstring('Variables', 'Sewer exit', Dummy_str), sewer_exit) then
    begin
      errorFlag := True;
      errorDummy := 'Error in data input: Sewer exit system value missing or wrong data format';
    end;
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
  if Inc_tillage AND not (FileExists(TilDirfilename))then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Tillage direction file not found in '+datadir;
     end;
  if Inc_tillage AND not (FileExists(Rofilename))then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Oriented roughness file not found in '+datadir;
     end;
  if (Inifile.ReadBool('User Choices','Include buffers',false))=true then Include_buffer:=true else Include_buffer:=false;
  if Include_buffer AND not (FileExists(BufferFilename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Buffer map file not found in '+datadir;
     end;
  if (Inifile.ReadBool('User Choices','Include ditches',false))=true then Include_ditch:=true else Include_ditch:=false;
   if Include_ditch AND not (FileExists(Ditch_filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Ditch map file not found in '+datadir;
     end;
  if (Inifile.ReadBool('User Choices','Include dams',false))=true then Include_dam:=true else Include_dam:=false;
   if Include_dam AND not (FileExists(Dam_filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Dam map file not found in '+datadir;
     end;
 if (Inifile.ReadBool('User Choices','Create ktc map',false))=true then Create_ktc:=true else Create_ktc:=false;
   if not (Create_ktc) AND not (FileExists(ktc_Data_Filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: ktc file not found in '+datadir;
     end;
  if (Inifile.ReadBool('User Choices','Create ktil map',false))=true then Create_ktil:=true else Create_ktil:=false;
  if not (Create_ktil) AND not (FileExists(ktil_Data_Filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: ktil file not found in '+datadir;
     end;
  if (Inifile.ReadBool('User Choices','Estimate clay content',false))=true then est_clay:=true else est_clay:=false;
  if (est_clay) AND not (TryStrToFloat(Inifile.Readstring('Variables', 'Clay content parent material', Dummy_str), clay_parent)) then
    begin
       errorFlag := True;
       errorDummy := 'Error in data input: Clay content parent material value missing or wrong data format';
     end;
  if (Inifile.ReadBool('User Choices','Manual outlet selection',false))=true then Outlet_select:=true else Outlet_select:=false;
  if Outlet_select AND not (FileExists(Outletfilename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Outlet map file not found in '+datadir;
     end;
  if (Inifile.ReadBool('User Choices','Convert output',false))=true then Convert_output:=true else Convert_output:=false;
  if (Inifile.ReadBool('User Choices','Output per VHA river segment',false))=true then VHA:=true else VHA:=false;
  if VHA AND not (FileExists(Riversegment_filename)) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: River segment map file not found in '+datadir;
     end;

  {Output maps}
  if (Inifile.ReadBool('Output maps','Write aspect',false))=true then Write_ASPECT:=true else Write_ASPECT:=false;
  if (Inifile.ReadBool('Output maps','Write LS factor',false))=true then Write_LS:=true else Write_LS:=false;
  if (Inifile.ReadBool('Output maps','Write RUSLE',false))=true then Write_RUSLE:=true else Write_RUSLE:=false;
  if (Inifile.ReadBool('Output maps','Write sediment export',false))=true then Write_Sediexport:=true else Write_Sediexport:=false;
  if (Inifile.ReadBool('Output maps','Write slope',false))=true then Write_SLOPE:=true else Write_SLOPE:=false;
  if (Inifile.ReadBool('Output maps','Write tillage erosion',false))=true then Write_TILEROS:=true else Write_TILEROS:=false;
  if (Inifile.ReadBool('Output maps','Write upstream area',false))=true then Write_UPAREA:=true else Write_UPAREA:=false;
  if (Inifile.ReadBool('Output maps','Write water erosion',false))=true then Write_WATEREROS:=true else Write_WATEREROS:=false;
  if Simplified then
    begin
      Write_RE := false;
      Write_TOTRUN := false;
    end
    else
    begin
       if (Inifile.ReadBool('Output maps','Write rainfall excess',false))=true then Write_RE:=true else Write_RE:=false;
       if (Inifile.ReadBool('Output maps','Write total runoff',false))=true then Write_TOTRUN:=true else Write_TOTRUN:=false;
    end;

  {Variables}
  if not Simplified then
    begin
      if not Use_RFactor then
            begin
          if not TryStrToFloat(Inifile.Readstring('Variables', '5-day antecedent rainfall', Dummy_str), AR5) then
             begin
               errorFlag := True;
               errorDummy := 'Error in data input: AR5 value missing or wrong data format';
             end;
        end;
       if not TryStrToFloat(Inifile.Readstring('Variables', 'Stream velocity', Dummy_str), riv_vel) then
        begin
           errorFlag := True;
           errorDummy := 'Error in data input: Stream velocity value missing or wrong data format';
         end;
      if not TryStrToFloat(Inifile.Readstring('Variables', 'Alpha', Dummy_str), alpha) then
         begin
           errorFlag := True;
           errorDummy := 'Error in data input: alpha value missing or wrong data format';
         end;
      if not TryStrToFloat(Inifile.Readstring('Variables', 'Beta', Dummy_str), beta) then
      begin
           errorFlag := True;
           errorDummy := 'Error in data input: beta value missing or wrong data format';
         end;
    end
    else
    begin
      if not TryStrToFloat(Inifile.Readstring('Variables', 'R factor', Dummy_str),Rfactor) then
      begin
        errorFlag := True;
        errorDummy := 'Error in data input: R factor value missing or wrong data format';
      end;
    end;

  if not TryStrToInt(Inifile.Readstring('Variables', 'Bulk density', Dummy_str), BD) then
    begin
       errorFlag := True;
       errorDummy := 'Error in data input: BD value missing or wrong data format';
     end;
 if (Include_buffer) AND not (TryStrToInt(inifile.readstring('Variables', 'Number of buffers', Dummy_str), Number_of_Buffers)) then
   begin
       errorFlag := True;
       errorDummy := 'Error in data input: Number of buffers value missing or wrong data format';
     end;
  if (create_ktc) AND not (TryStrToInt(Inifile.Readstring('Variables', 'ktc low', Dummy_str),ktc_low)) then
    begin
      errorFlag := True;
      errorDummy := 'Error in data input: ktc low value missing or wrong data format';
    end;
  if (create_ktc) AND not (TryStrToInt(Inifile.Readstring('Variables', 'ktc high', Dummy_str),ktc_high)) then
    begin
      errorFlag := True;
      errorDummy := 'Error in data input: ktc high value missing or wrong data format';
    end;
  if (create_ktc) AND not (TryStrToFloat(Inifile.Readstring('Variables', 'ktc limit', Dummy_str),ktc_limit)) then
    begin
      errorFlag := True;
      errorDummy := 'Error in data input: ktc limit value missing or wrong data format';
    end;
  if (create_ktil) AND not (TryStrToInt(Inifile.Readstring('Variables', 'ktil default', Dummy_str), ktil_Default)) then
    begin
       errorFlag := True;
       errorDummy := 'Error in data input: ktil default value missing or wrong data format';
     end;
  if (create_ktil) AND not (TryStrToFloat(Inifile.Readstring('Variables', 'ktil threshold', Dummy_str), ktil_threshold)) then
    begin
       errorFlag := True;
       errorDummy := 'Error in data input: ktil threshold value missing or wrong data format';
     end;
  if not TryStrToInt(Inifile.Readstring('Variables', 'Parcel connectivity cropland', Dummy_str), TFSED_crop) then
  begin
       errorFlag := True;
       errorDummy := 'Error in data input: Parcel connectivity cropland value missing or wrong data format';
     end;
  if not TryStrToInt(Inifile.Readstring('Variables', 'Parcel connectivity forest', Dummy_str), TFSED_forest) then
  begin
       errorFlag := True;
       errorDummy := 'Error in data input: Parcel connectivity forest/pasture value missing or wrong data format';
     end;
  if not TryStrToInt(Inifile.Readstring('Variables', 'Parcel trapping efficiency cropland', Dummy_str), PTEFValueCropland) then
  begin
       errorFlag := True;
       errorDummy := 'Error in data input: Parcel trapping efficiency (cropland) value missing or wrong data format';
     end;
  if not TryStrToInt(Inifile.Readstring('Variables', 'Parcel trapping efficiency forest', Dummy_str), PTEFValueForest) then
  begin
       errorFlag := True;
       errorDummy := 'Error in data input: Parcel trapping efficiency (forest) value missing or wrong data format';
     end;
  if not TryStrToInt(Inifile.Readstring('Variables', 'Parcel trapping efficiency pasture', Dummy_str), PTEFValuePasture) then
  begin
       errorFlag := True;
       errorDummy := 'Error in data input: Parcel trapping efficiency (pasture) value missing or wrong data format';
     end;
  if not Simplified then
  begin
    if not TryStrToInt(inifile.readstring('Variables', 'Desired timestep for model', Dummy_str), Timestep_model) then
       begin
         errorFlag := True;
         errorDummy := 'Error in data input: Desired timestep for model value missing or wrong data format';
       end;
    if (Convert_output) AND not TryStrToInt(inifile.readstring('Variables', 'Final timestep output', Dummy_str), Timestep_output) then
     begin
       errorFlag := True;
       errorDummy := 'Error in data input: Final timestep output value missing or wrong data format';
     end;
  end;
  if not TryStrToInt(inifile.readstring('Variables', 'Endtime model', Dummy_str), Endtime_model) then
    begin
       errorFlag := True;
       errorDummy := 'Error in data input: Endtime model value missing or wrong data format';
     end;

  if Include_buffer then
  begin
    setlength(Bufferdata, Number_of_Buffers + 1);
    for i := 1 to Number_of_Buffers do
    begin
      Buffername := 'Buffer ' + IntToStr(i);
      if not TryStrToFloat(inifile.readstring(Buffername, 'Volume', Dummy_str), Bufferdata[i].Volume) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' volume value missing or wrong data format';
        end;
      if not TryStrToFloat(inifile.readstring(Buffername, 'Height dam', Dummy_str),Bufferdata[i].Height_dam) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' height dam value missing or wrong data format';
        end;
      if not TryStrToFloat(inifile.readstring(Buffername, 'Height opening', Dummy_str),Bufferdata[i].Height_opening) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' height opening value missing or wrong data format';
        end;
       if not  TryStrToFloat(inifile.readstring(Buffername, 'Opening area', Dummy_str),Bufferdata[i].Opening_area) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' opening area value missing or wrong data format';
        end;
       if not TryStrToFloat(inifile.readstring(Buffername, 'Discharge coefficient', Dummy_str), Bufferdata[i].Cd) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' discharge coefficient value missing or wrong data format';
        end;
       if not TryStrToFloat(inifile.readstring(Buffername, 'Width dam', Dummy_str), Bufferdata[i].width_dam) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' width dam value missing or wrong data format';
        end;
       if not TryStrToFloat(inifile.readstring(Buffername, 'Trapping efficiency', Dummy_str), Bufferdata[i].PTEF) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' trapping efficiency value missing or wrong data format';
        end;
       if not TryStrToInt(inifile.readstring(Buffername, 'Extension ID', Dummy_str), Bufferdata[i].ext_ID) then
        begin
         errorFlag := True;
         errorDummy := 'Error in data input: Buffer '+intToStr(i)+' extension ID missing or wrong data format';
        end;
      if Bufferdata[i].Height_opening > Bufferdata[i].Height_dam then
        begin
         errorFlag := True;
         errorDummy := 'Error in buffer input: the height of the opening cannot be larger than the height of the dam. Please insert correct values.';
        end;
    end;
  end;

  Inifile.Destroy;
end;

procedure Create_ktil_map(var ktil: GRaster);
var
i,j: integer;
begin
  SetDynamicGData(ktil);
  for i := 1 to nrow do
   for j := 1 to ncol do
    begin
      if C_factor[i,j] <= ktil_threshold then
        ktil[i,j] := 0
      else
        ktil[i,j] := ktil_Default;
    end;

  writeGidrisi32file(ncol,nrow,datadir+'\ktilmap'+'.rst',ktil);
end;

procedure Create_ktc_map(var ktc: GRaster);
var
i,j: integer;
begin
  SetDynamicGData(ktc);
  for i := 1 to nrow do
   for j := 1 to ncol do
    begin
      if C_factor[i,j] <= ktc_limit then
        ktc[i,j] := ktc_low
      else
        ktc[i,j] := ktc_high;
      // In WS, cells with PRC == -2 or -1 receive a Ktc value of 9999
    if (PRC[i,j] = -2) or (PRC[i,j] = -1) then
       ktc[i,j] := 9999;
    // kTc of open water = 0 because sediment can't go any further once it's there
  if PRC[i,j] = -5 then
    ktc[i,j] := 0;
    end;

  writeGidrisi32file(ncol,nrow,datadir+'\ktcmap'+'.rst',ktc);
end;

end.

