
Program CN_WSmodel;

{$mode objfpc}{$H+}

{$R+} //enable range checking of arrays

Uses 
  {$IFDEF UNIX}{$IFDEF UseCThreads}
cthreads,
  {$ENDIF}{$ENDIF}
Classes, SysUtils, CustApp, Dos, Interfaces, Crt, ReadInParameters, Write_output,
CN_calculations, Raster_calculations, LateralRedistribution, Idrisi, tillage,
RData_CN, GData_CN
  { you can add units after this };


Type 

  TCN_WSApplication = Class(TCustomApplication)
    Protected 
      Procedure DoRun;
      override;
    Public 
      constructor Create(TheOwner:TComponent);
      override;
      destructor Destroy;
      override;
      Procedure WriteHelp;
      virtual;
  End;


Var 
  //execution var
  hr,mins,se,s1: word;

Procedure StartClock;
Begin
  GetTime(hr,mins,se,s1);
End;

Function StopClock: string;

Var 
  hr2,min2,se2 : word;
Begin
  GetTime(hr2,min2,se2,s1);
  result := floattostr(se2-se+(min2-mins)*60+(hr2-hr)*60*60+s1/100);
End;

Procedure TCN_WSApplication.DoRun;

Var 
  Time: String;
  filename: String;
  i : integer;
  high_i, low_i: integer;
  cal_output_file: textfile;

Begin
  StartClock;
  writeln;
  writeln;
  writeln('CN_WS model');
  writeln;
  filename := '';
  For i := 1 To ParamCount Do
    filename := filename+ParamStr(i);
  WriteLn('Inifile : ', filename);
  writeln;

  Try
     ReadSettings(filename);
     ReadInRasters;
  Except
    on E: Exception Do
          Begin
            TextColor(red);
            writeln(E.Message);
            Terminate(1);
            Exit
          End;

End;

If Not Use_Rfactor Then
  Begin
    ReadRainfallFile(Raindata, RainfallFilename);
    //The .txt file with rainfall per timestep is read and written to a variable
    CalculateRFactor;
    // R factor is calculated from given rainfall record
  End;

If (Not simplified) and (Timestep_model>=resAR[1]/0.3) Then
Begin
      writeln(
          'Error: Courant criterium for model stability violated. Please select a smaller timestep.');
      Terminate(1);
      Exit;
End;

Allocate_Memory;
// voor verschillende rasters

If Not Simplified Then
  CalculateRe(ReMap, PRC, CNmap, alpha, beta);
//Amount of rainfall excess or deficit is calculated

try

  Topo_Calculations;
  // Sort DTM, calculate slope and aspect, calculate routing, calculate UPAREA, calculate LS factor RUSLE

Except
  on E: Exception Do
        Begin
          writeln(E.Message);
          Terminate(1);
          Exit;
        End;

end;

try
// number and position of outlets is determined. Lowest outlet is also determined.
calcOutlet;
Except
  on E: Exception Do
     Begin
       Writeln(E.Message);
       Terminate(1);
       Exit;
     end;
end;

If Not simplified Then
  CalculateTimeDependentRunoff(Remap, RainData, Routing, PRC);
//Amount of runoff per timestep is calculated

if not calibrate then Water;
// Water erosion calculations

if calibrate Then
Begin
  Writeln('Using calibration');
  setcurrentDir(File_output_dir);
  assignfile(cal_output_file, 'calibration.txt');
  rewrite(cal_output_file);
  write(cal_output_file, 'ktc_low;ktc_high;tot_erosion;tot_sedimentation;sed_river;sed_noriver;sed_buffer;sed_openwater');

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
End;

If Not Simplified Then
  Distribute_sediment;
// Sediment is distributed over hydrogram

if river_routing then
   Cumulative_raster;

Tillage_dif;
// tillage erosion calculations

write_maps;
// write output maps

Write_Routing_Table;
//

//The memory is released for all created maps
Release_Memory;


Time := StopClock;
Writeln('Calculations completed. Program Execution Time: ',Time,' sec');
// Writeln('Press <Enter> to continue (in case of multiple model runs) or quit');
// readln;

Terminate;
End;


constructor TCN_WSApplication.Create(TheOwner: TComponent);
Begin
  inherited Create(TheOwner);
  StopOnException := True;
End;

destructor TCN_WSApplication.Destroy;
Begin
  inherited Destroy;
End;

Procedure TCN_WSApplication.WriteHelp;
Begin
    { add your help code here }
  writeln('Usage: ',ExeName,' -h');
End;

Var 
  Application: TCN_WSApplication;
Begin
  Application := TCN_WSApplication.Create(Nil);
  Application.Title := 'CN_WS model';
  Application.Run;
  Application.Free;
End.
