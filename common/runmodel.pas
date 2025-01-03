unit runmodel;

{$mode objfpc}{$H+}
{$R+}

interface

Uses
Classes, SysUtils, Dos, Crt,
ReadInParameters, Write_output,
CN_calculations, Raster_calculations, LateralRedistribution, tillage,
RData_CN, GData_CN;


Procedure Runmodel(inifilename: String);
implementation


Procedure Runmodel (inifilename: String);
var
   fs: TFormatSettings;
   cal_output_file: textfile;
   high_i, low_i: integer;

begin
// runs the complete model, using an inifile as input

fs := DefaultFormatSettings;
fs.DecimalSeparator := '.';
DefaultFormatSettings := fs;

Try
   ReadSettings(inifilename);
   ReadInRasters;
Except
  on E: Exception Do
        Begin
          TextColor(red);
          writeln(E.Message);
          writeln('Exception Address: ', BackTraceStrFunc(ExceptAddr));
          NormVideo();
          Halt(1);
          Exit
        End;

End;


If (curve_number) and (Timestep_model>=resAR[1]/0.3) Then
Begin
    writeln(
        'Error: Courant criterium for model stability violated. Please select a smaller timestep.');
    Halt(1);
    Exit;
End;

try
Topo_Calculations;
// Sort DTM, calculate slope and aspect, calculate routing, calculate UPAREA, calculate LS factor RUSLE

if not OnlyRouting Then
 Begin
     if Outlet_select then
      loadOutlet
     else
      calcOutlet;

     if curve_number Then //or (not use_rfactor) Then
      Begin
     ReadRainfallFile(Raindata, RainfallFilename); //The .txt file with rainfall per timestep is read and written to a variable
     End;

    // DEPRETACTED: Run CN per R-factor event.
    //If Not Use_Rfactor Then
    //Begin
    //  CalculateRFactor;
      // R factor is calculated from given rainfall record
    //  Write_RFactor;
    //End;

    if not calibrate then Water;
    // Water erosion calculations

    if calibrate Then
    Begin
      Writeln('Using calibration');
      setcurrentDir(File_output_dir);
      assignfile(cal_output_file, 'calibration.txt');
      rewrite(cal_output_file);
      write(cal_output_file, 'ktc_low;ktc_high;tot_erosion;tot_sedimentation;sed_river;sed_noriver;sed_buffer;sed_sewerin;sed_openwater');

      For i := 1 To numOutlet Do
      Begin
        Write(cal_output_file, ';outlet_'+inttostr(i));
      End;

      writeln(cal_output_file, '');
      closefile(cal_output_file);

        For low_i := 0 To cal.steps Do
          For high_i:=0 To cal.steps Do
            Begin
              ktc_low:=cal.KTcLow_lower + low_i * (cal.KTcLow_upper - cal.KTcLow_lower)/cal.steps;
              ktc_high:=cal.KTcHigh_lower + high_i* (cal.KTcHigh_upper - cal.KTcHigh_lower)/cal.steps;
              If ktc_high >= ktc_low Then
              Begin
                Create_ktc_map(ktc);
                Writeln('ktc_low: ' + Formatfloat('0.00', ktc_low) + '; ktc_high:' + Formatfloat('0.00', ktc_high));
                Water;
              end;
            end;
        DisposeDynamicGdata(K_Factor);
        DisposeDynamicRdata(C_factor);
        DisposeDynamicRdata(P_factor);
    End;

    If curve_number Then
    Begin
    CalculateRe(ReMap, PRC, CNmap, alpha, beta);
    //Amount of rainfall excess or deficit is calculated
    CalculateTimeDependentRunoff(Remap, RainData, Routing, PRC);
    //Amount of runoff per timestep is calculated
    Distribute_sediment;
    // Sediment is distributed over hydrogram
    End;

    if river_routing then
       Cumulative_raster;

    if Calc_tileros then
       Tillage_dif;
    // tillage erosion calculations
 end;
Except
on E: Exception Do
      Begin
        TextColor(red);
        writeln(E.Message);
        writeln('Exception Address: ', BackTraceStrFunc(ExceptAddr));
        NormVideo();
        Halt(1);
        Exit;
      End;

end;

write_maps;
// write output maps

If Write_routing Then
 Write_Routing_Table;

//The memory is released for all created maps
Release_Memory;

end;

end.

