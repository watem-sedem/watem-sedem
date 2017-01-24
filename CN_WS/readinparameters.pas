unit ReadInParameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RData_CN, GData_CN, Inifiles, Dialogs, Idrisi;
Procedure ReadInRasters;
Procedure Allocate_Memory;
Procedure Release_Memory;
procedure Readsettings(INI_filename:string);
procedure Create_CN_map(var CNmap: RRaster;Perceelskaart:RRaster; Filename:string);
function CalculateCN(CNmax,Cc,Cr,c1,c2:integer):single;
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
  RUSLE         : RRaster;                      // result of RUSLE equation in kg/m² = POTENTIAL soil loss
  TILEROS       : RRaster;                      //tillage erosion (unit: mm)
  SEDI_EXPORT   : RRaster;            // sediment export in m³
  SEDI_EXPORT_kg   : RRaster;          // sediment export in kg
  SEDI_IN2       : RRaster;            // incoming sediment in kg
  SEDI_OUT2      :RRaster;             // outgoing sediment in kg
  //depprod2        :RRaster;             // deposition in kg
  {row2            :GRaster;
  col2            :GRaster;   }
  {Rasters to be read in--------------------------------------------------------}
   K_factor   : GRaster;    {RUSLE K-factor map kg m² h m-² MJ-1 mm-1}
   C_factor   : RRaster;
   P_factor   : RRaster;
   ktil       : GRaster;
   ktc        : GRaster;
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
  AR5                  : double;
  BD                   : integer;
  riv_vel              : double;
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

  RFactor: double;
  PRC,DTM, CNmap, LU, ReMap, RunoffTotMap, SewerMap: Rraster;
  TilDir, Ro,BufferMap, Outlet, RivSeg, Ditch_map, Dam_map, PTEFmap: GRaster;
  i, j, lowOutletX, lowOutletY: integer;

  ROW, COLUMN : Gvector;

  Slope,Aspect,Uparea,LS: Rraster;
  totsurface:double;

  Routing: TRoutingArray;
  OutletArray : TIntArray;
  RainData: TRainRecordArray;


implementation

Procedure ReadInRasters;
begin
    GetRFile(DTM,DTM_Filename);

  GetRFile(PRC,PARCEL_filename);

  if Include_sewer then
    GetRFile(SewerMap,Sewerfilename);

  if not simplified then
  begin
      GetRfile(CNmap, CNmapfilename);
  end;

  if topo = false then // als topo = false wordt de ploegrichting in rekening gebracht
  begin
    GetGFile(TilDir, TilDirFilename);
    GetGfile(Ro, RoFilename);
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
     // Buffermap should be map containing only zeros
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
//SetDynamicRData(depprod2);
{SetDynamicGData(row2);
SetDynamicGData(col2);  }
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

if not Simplified then
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
 DisposeDynamicGdata(ktil);
 DisposeDynamicGdata(ktc);
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
//DisposeDynamicRdata(depprod2);
{DisposeDynamicGdata(row2);
DisposeDynamicGdata(col2);   }
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
  File_output_dir := Inifile.readstring('Working directories', 'Output directory', Dummy_str);
  INIfilename := Inifile.Readstring('Files', '.INI filename', Dummy_str);
  DTM_filename := Inifile.Readstring('Files', 'DTM filename', Dummy_str);
  PARCEL_filename := Inifile.Readstring('Files', 'Parcel filename', Dummy_str);
  Rainfallfilename := Inifile.Readstring('Files', 'Rainfall filename', Dummy_str);
  Sewerfilename := Inifile.Readstring('Files', 'Sewer map filename', Dummy_str);
  CNmapfilename := Inifile.Readstring('Files', 'CN map filename', Dummy_str);
  TilDirfilename := Inifile.Readstring('Files', 'Tillage direction filename', Dummy_str);
  Rofilename := Inifile.Readstring('Files', 'Oriented roughness filename', Dummy_str);
  BufferFilename := inifile.readstring('Files', 'Buffer map filename', Dummy_str);
  Ditch_filename := inifile.readstring('Files', 'Ditch map filename', Dummy_str);
  Dam_filename := inifile.readstring('Files', 'Dam map filename', Dummy_str);
  K_Factor_filename := inifile.readstring('Files', 'K factor filename', Dummy_str);
  Cf_Data_Filename := inifile.readstring('Files', 'C factor map filename', Dummy_str);
  Pf_Data_Filename := inifile.readstring('Files', 'P factor map filename', Dummy_str);
  ktc_Data_Filename := inifile.readstring('Files', 'ktc map filename', Dummy_str);
  ktil_Data_Filename := inifile.readstring('Files', 'ktil map filename', Dummy_str);
  Outletfilename := inifile.readstring('Files', 'Outlet map filename', Dummy_str);
  Riversegment_filename := inifile.readstring('Files', 'River segment filename', Dummy_str);

  // If the last character in the output directory is not a "\" this is added
  if (File_output_dir[Length(File_output_dir)] <> '\') then
     File_output_dir := IncludeTrailingBackslash(File_output_dir);

  if (Inifile.ReadBool('User Choices','Simplified model version',false))=true then Simplified := true else Simplified:= false;
  if (Inifile.ReadBool('User Choices','Use R factor',false))=true then Use_Rfactor := true else Use_Rfactor:= false;
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
  if (Inifile.ReadBool('User Choices','Create ktc map',false))=true then Create_ktc:=true else Create_ktc:=false;
  if (Inifile.ReadBool('User Choices','Create ktil map',false))=true then Create_ktil:=true else Create_ktil:=false;
  if (Inifile.ReadBool('User Choices','Estimate clay content',false))=true then est_clay:=true else est_clay:=false;
  if (Inifile.ReadBool('User Choices','Manual outlet selection',false))=true then Outlet_select:=true else Outlet_select:=false;
  if (Inifile.ReadBool('User Choices','Convert output',false))=true then Convert_output:=true else Convert_output:=false;
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

  if not Use_Rfactor then
    AR5 := StrToFloat(Inifile.Readstring('Variables', '5-day antecedent rainfall', Dummy_str))
  else
    RFactor := StrToFloat(Inifile.Readstring('Variables', 'R factor', Dummy_str));
  BD := StrToInt(Inifile.Readstring('Variables', 'Bulk density', Dummy_str));
  riv_vel := StrToFloat(Inifile.Readstring('Variables', 'Stream velocity', Dummy_str));
  if Include_sewer then
    sewer_exit := StrToInt(Inifile.Readstring('Variables', 'Sewer exit', Dummy_str));
  alpha := StrToFloat(Inifile.Readstring('Variables', 'Alpha', Dummy_str));
  beta := StrToFloat(Inifile.Readstring('Variables', 'Beta', Dummy_str));
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
  Timestep_model := StrToInt(inifile.readstring('Variables', 'Desired timestep for model', Dummy_str));
  Endtime_model := StrToInt(inifile.readstring('Variables', 'Endtime model', Dummy_str));
  if Convert_output then
    Timestep_output := StrToInt(inifile.readstring('Variables', 'Final timestep output', Dummy_str));

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

//**************************************************************************
//This procedure is only ran when the user enters a land use map and table that
//contains factors to calculate the CN value for every land use
//In this procedure the CN map is calculated based on the formulas described in
//The PhD of Kristof Van Oost
//**************************************************************************
procedure Create_CN_map(var CNmap: RRaster;Perceelskaart:RRaster; Filename:string);
var
Count, i, j, k, getal, NumberOfLU, nrowPRC, ncolPRC: integer;
Table: textfile;
TempName: String;
M: GRaster;
CN_waarden: array of single;

//The number of rows in the .txt file is counted. This way the user can define as
//much LU classes as he wishes
//!!The first row of the .txt file should contain headers!!
begin
  If  FileExists(datadir+Filename) then //Check if the .txt file exists
    BEGIN
      SetcurrentDir(datadir);
      TempName:=Filename; // filename + extension
      Count:=0;
      Assignfile(Table,TempName);
      Reset(Table);
      While not eof(Table) do //eof = end of file
      begin
        readln(Table);
        Count:=Count+1; //The number of rows is counted
      end;
      Closefile(Table);
      NumberOfLU:=Count-1; //'-1' because the first row contains headers
    end
  Else
  begin
  Showmessage('De tabel met CN waarden werd niet herkend, het proramma wordt gesloten.');
  Exit; //If the file does not extists the program is ended
  end;

//The numbers from the .txt file are stored in a variable (matrix) M
//All numbers in the .txt file should be integers!!!
SetDynamicGData(M);
begin
     Assignfile(Table,TempName);
     Reset(Table);
     readln(Table); //The first row (headers) is read and is thus skipped below
     for i := 1 to NumberofLU do
         for j := 1 to 6 do //6 because the .txt table alsways contains 6 columns (Parcel ID - CNmax - c1 - c2 - Crop cover - Crusting stage)
             begin
             read(Table, getal);
             M[i,j] := getal;
             end;
end;
closefile(Table);

//All posible CN values are calculated
SetLength(CN_waarden,NumberofLU);
for i:= 1 to NumberofLU do
    begin
        CN_Waarden[i]:=calculateCN(M[i,2],M[i,5],M[i,6],M[i,3],M[i,4]) //CalcualteCN: procedure to calculate the CN value
    end;

//Number of rows and colums is defined
nrowPRC := nrow; //nrow and ncol are defined based on the .RDC files in RData_CN
ncolPRC := ncol;

//Based on the .txt table and the land use map (which is being read in the main unit
//the CN map is created
Setlength(CNmap,NrowPRC+1, NColPRC+1); //+1 because [0] is being used by Lazarus
for i := 1 to nrowPRC do
    for j := 1 to ncolPRC do
        begin
        if Perceelskaart[i,j] = 0 then
          CNmap[i,j] := 0.0
        else
        begin
        for k := 1 to NumberofLU do
            if Perceelskaart[i,j] = M[k,1] then
            CNmap[i,j] := CN_Waarden[k]
        end
        end;

//The CN map is stored as an Idrisi map
writeidrisi32file(ncolPRC,nrowPRC,datadir+'\CNmap'+'.rst',CNmap);
DisposeDynamicGData(M);
end;


//**********************************************************************
//This procedure calculates the CN value according to the formula of KVO
//**********************************************************************
function CalculateCN(CNmax,Cc,Cr,c1,c2:integer):single;
begin
 CalculateCN := CNmax - ((Cc/100)*c1) + ((Cr/5)*c2);
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

