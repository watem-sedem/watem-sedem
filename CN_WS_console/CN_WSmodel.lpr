
Program CN_WSmodel;

{$mode objfpc}{$H+}

Uses 
  {$IFDEF UNIX}{$IFDEF UseCThreads}
cthreads,
  {$ENDIF}{$ENDIF}
Classes, SysUtils, CustApp, Dos, Interfaces, ReadInParameters, Write_output,
CN_calculations, Raster_calculations, LateralRedistribution, Idrisi, tillage,
RData_CN, GData_CN, radar_procedures
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
  result := inttostr(se2-se+(min2-mins)*60+(hr2-hr)*60*60);
End;

Procedure TCN_WSApplication.DoRun;

Var 
  Time: String;
  ErrorMsg: String;
  filename: String;
  i : integer;
Begin
  StartClock;
  writeln;
  writeln;
  writeln('CN_WS model');
  writeln;
  filename := '';
  For i := 1 To ParamCount Do
    filename := filename+ParamStr(i)+' ';
  WriteLn('Inifile : ', filename);
  writeln;

  Try
     ReadSettings(filename);
   Except
     on E:Exception Do
     Begin
       writeln(E.Message);
       readln;
       Terminate;
       Exit
     End;
   end;

  Try
    ReadInRasters;
  Except
    on E: Exception Do
          Begin
            writeln(E.Message);
            readln;
            Terminate;
            Exit
          End;

End;

If Use_Radar_Rain Then
   Begin
         PreparRadarRainfallData (Radar_dir, timestepRadar);
         // R factor is calculated from given rainfall record
         write ('test1', #13#10);
         CalculateRFactorRadar (TimeSeries, timestepRadar, RadarRaindataset_src);
         write ('test2', #13#10);
   End
Else
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
      Terminate;
      Exit;
End;

Allocate_Memory;
// voor verschillende rasters

//Amount of rainfall excess or deficit is calculated
If Use_Radar_Rain Then
   Begin
       CalculateReRadar(ReMap, RainfallSum, I10Radar, PRC, CNmap, alpha, beta);
       write ('test3', #13#10);
   End
Else
    Begin
         If Not Simplified Then
            Begin
                 CalculateRe(ReMap, PRC, CNmap, alpha, beta);
            End
    End;



try

  Topo_Calculations;
  // Sort DTM, calculate slope and aspect, calculate routing, calculate UPAREA, calculate LS factor RUSLE

Except
  on E: Exception Do
        Begin
          writeln(E.Message);
          readln;
          Terminate;
          Exit
        End;

end;

// number and position of outlets is determined. Lowest outlet is also determined.
calcOutlet;

//Amount of runoff per timestep is calculated
If Use_Radar_Rain Then
   Begin
       CalculateTimeDependentRunoffRadar(Remap, Routing, PRC);
   End
Else
    Begin
         If Not simplified Then
            Begin
                 CalculateTimeDependentRunoff(Remap, RainData, Routing, PRC);
            End
    End;

// Water erosion calculations
If Use_Radar_Rain Then
   Begin
       WaterRadar;
   End
Else
    Begin
         Water;
    End;

// Sediment is distributed over hydrogram
If Use_Radar_Rain Then
   Begin
       Distribute_sedimentRadar;
   End
Else
    Begin
        If Not Simplified Then
           Begin
                Distribute_sediment;
           End
    End;

Tillage_dif;
// tillage erosion calculations

write_maps;
// write output maps


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
