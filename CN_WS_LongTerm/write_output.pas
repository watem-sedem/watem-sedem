
Unit Write_output;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, Inifiles, ReadInParameters, Idrisi, RData_CN, Calculations, Dialogs;


Procedure Write_rainfall(eventID: integer);
Procedure Write_ini;
Procedure Write_cmd;
Procedure WriteOutput_event(eventID: integer);
Procedure WriteOutput;

Implementation


Procedure Write_rainfall(eventID: integer);

Var 
  Rain_event : textfile;
  i, time, numElements, difference, final_length: integer;
  Timeseries_temp, Timeseries_temp2: integerArray;
  Rainfall_temp, Rainfall_temp2: floatArray;

Begin

//The rainfall input for an event is written to a .txt file (this file is overwritten upon next event)

  If (Simplified) Then         // keep timestep of 10 min
    Begin
      numElements := Event_meta[eventID].NumberOfElements;
      setLength(Timeseries_temp, numElements+1);
      time := 10;
      Timeseries_temp[0] := 0;
      For i := 1 To numElements Do
        Begin
          Timeseries_temp[i] := Timeseries_temp[i-1]+time;
        End;
      setLength(Rainfall_temp, numElements+1);
      Rainfall_temp[0] := 0;
      For i := 1 To numElements Do
        Rainfall_temp[i] := Events[i-1,eventID];

// if endtime of rainfall data < endtime model (specified by user) => add extra zero's to rainfall data
      // to match the two
      If Timeseries_temp[numElements] < EndTime_model  Then
        Begin
          difference := EndTime_model - Timeseries_temp[numElements];
          final_length := length(Timeseries_temp) + (difference Div time) + 1;
          Setlength(Timeseries_event, final_length);
          Setlength(Rainfallseries_event, final_length);
          For i := 0 To numElements Do
            Begin
              Timeseries_event[i] := Timeseries_temp[i];
              Rainfallseries_event[i] := Rainfall_temp[i];
            End;
          For i := numElements+1 To final_length-1 Do
            Begin
              Timeseries_event[i] := Timeseries_event[i-1]+time;
              Rainfallseries_event[i] := 0
            End;
        End
      Else
        Begin
          Timeseries_event := Timeseries_temp;
          Rainfallseries_event := Rainfall_temp;
          final_length := numElements;
        End;
    End
  Else
    Begin
      If Timestep_model < 60 Then            // extrapolation to timestep of 1 min
        Begin
          numElements := Event_meta[eventID].NumberOfElements Div (60 Div Timestep_model);
          setLength(Timeseries_temp2, numElements+1);
          time := 60;
          Timeseries_temp2[0] := 0;
          For i := 1 To numElements Do
            Begin
              Timeseries_temp2[i] := Timeseries_temp2[i-1]+time;
            End;
          SetLength(Rainfall_temp, Event_meta[eventID].NumberOfElements+1);
          Rainfall_temp[0] := 0;
          For i := 1 To length(Rainfall_temp)-1 Do
            Rainfall_temp[i] := Events[i-1,eventID];
          SetLength(Timeseries_temp, Event_meta[eventID].NumberOfElements+1);
          time := Timestep_model;
          Timeseries_temp[0] := 0;
          For i := 1 To length(Timeseries_temp)-1 Do
            Begin
              Timeseries_temp[i] := Timeseries_temp[i-1]+time;
            End;
          Rainfall_temp2 := extrap(Timeseries_temp, Timeseries_temp2, Rainfall_temp);
          For i := 0 To numElements Do
            Timeseries_temp2[i] := Timeseries_temp2[i] Div 60;
          // convert time array to minutes

// if endtime of rainfall data < endtime model (specified by user) => add extra zero's to rainfall data
          // to match the two
          If Timeseries_temp2[numElements] < EndTime_model  Then
            Begin
              difference := EndTime_model - Timeseries_temp2[numElements];
              final_length := length(Timeseries_temp2) + difference + 1;
              Setlength(Timeseries_event, final_length);
              Setlength(Rainfallseries_event,final_length);
              For i := 0 To numElements Do
                Begin
                  Timeseries_event[i] := Timeseries_temp2[i];
                  Rainfallseries_event[i] := Rainfall_temp2[i];
                End;
              For i := numElements+1 To final_length-1 Do
                Begin
                  Timeseries_event[i] := Timeseries_event[i-1]+1;
                  Rainfallseries_event[i] := 0
                End;
            End
          Else
            Begin
              Timeseries_event := Timeseries_temp2;
              Rainfallseries_event := Rainfall_temp2;
              final_length := numElements;
            End;
        End;
    End;

  // write to .txt file
  setcurrentDir(datadir);
  assignfile(Rain_event,'Rain input event.txt');
  rewrite(Rain_event);
  For i := 0 To final_length -1 Do
    Writeln(Rain_event, inttostr(Timeseries_event[i]), chr(9), floattostr(Rainfallseries_event[i]));
  closefile(Rain_event);
End;

Procedure Write_ini;
// .ini file is created for each event (and overwritten by the next one)

Var 
  inifile: Tinifile;
  inifile_event, Buffername : string;
  i: integer;
Begin

  // new .ini file is created (or the existing one is overwritten) and filled

  inifile_event := datadir + 'CN_WS_event.ini';
  inifile := Tinifile.Create(inifile_event);
  //The .ini file is created

  //Writing a line: ('Section', 'Key','Value')
  inifile.WriteString('Working directories', 'Input directory', dataDir);
  inifile.WriteString('Working directories', 'Output directory', File_output_dir);
  inifile.writestring('Files', '.INI filename', inifile_event);
  inifile.writestring('Files', 'DTM filename', DTM_filename);
  inifile.writestring('Files', 'Parcel filename', PARCEL_filename);
  inifile.writestring('Files', 'Rainfall filename', 'Rain input event.txt');
  inifile.writestring('Files', 'Sewer map filename', Sewerfilename);
  inifile.writestring('Files', 'CN map filename', CN_event);
  inifile.writestring('Files', 'Tillage direction filename', TILDIRfilename);
  inifile.writestring('Files', 'Oriented roughness filename', Rofilename);
  inifile.Writestring('Files', 'K factor filename', K_Factor_filename);
  inifile.Writestring('Files', 'C factor map filename', Cf_Data_event);
  inifile.Writestring('Files', 'P factor map filename', Pf_Data_Filename);
  inifile.Writestring('Files', 'ktc map filename', kTC_Filename);
  inifile.Writestring('Files', 'ktil map filename', ktil_Filename);
  inifile.Writestring('Files', 'Buffer map filename', BufferFilename);
  inifile.Writestring('Files', 'Ditch map filename', Ditch_filename);
  inifile.Writestring('Files', 'Dam map filename', Dam_filename);
  inifile.Writestring('Files', 'Outlet map filename', Outletfilename);
  inifile.Writestring('Files', 'River segment filename', riversegment_filename);

  inifile.WriteBool('User Choices', 'Simplified model version', Simplified);
  Inifile.WriteBool('User Choices', 'Use R factor', Use_Rfactor);
  inifile.WriteBool('User Choices', 'Include sewers', Include_sewer);
  inifile.WriteBool('User Choices', 'Include tillage', Inc_tillage);
  inifile.WriteBool('User Choices', 'Create ktc map', Create_ktc);
  inifile.WriteBool('User Choices', 'Create ktil map', Create_ktil);
  inifile.WriteBool('User Choices', 'Estimate clay content', est_clay);
  inifile.WriteBool('User Choices', 'Include buffers', Include_buffer);
  inifile.WriteBool('User Choices', 'Include ditches', Include_ditch);
  inifile.WriteBool('User Choices', 'Include dams', Include_dam);
  inifile.WriteBool('User Choices', 'Manual outlet selection', Outlet_select);
  inifile.WriteBool('User Choices', 'Convert output', Convert_output);
  inifile.WriteBool('User Choices', 'Output per VHA river segment', VHA);

  inifile.WriteBool('Output maps', 'Write aspect', Write_ASPECT);
  inifile.WriteBool('Output maps', 'Write LS factor', Write_LS);
  inifile.WriteBool('Output maps', 'Write rainfall excess', Write_RE);
  inifile.WriteBool('Output maps', 'Write RUSLE', Write_RUSLE);
  inifile.WriteBool('Output maps', 'Write sediment export', Write_Sediexport);
  inifile.WriteBool('Output maps', 'Write slope', Write_SLOPE);
  inifile.WriteBool('Output maps', 'Write tillage erosion', Write_TILEROS);
  inifile.WriteBool('Output maps', 'Write total runoff', Write_TOTRUN);
  inifile.WriteBool('Output maps', 'Write upstream area', Write_UPAREA);
  inifile.WriteBool('Output maps', 'Write water erosion', Write_WATEREROS);

  Inifile.writestring('Variables', '5-day antecedent rainfall', FloatToStr(AR5_event));
  Inifile.writestring('Variables', 'R factor', FloatToStr(0));
  Inifile.Writestring('Variables', 'Bulk density', IntToStr(BD));
  Inifile.Writestring('Variables', 'Stream velocity', FloatToStr(RivVel_event));
  Inifile.Writestring('Variables', 'Sewer exit', IntToStr(sewer_exit));
  Inifile.writestring('Variables', 'Alpha', FloatToStr(alpha));
  Inifile.writestring('Variables', 'Beta', FloatToStr(beta));
  Inifile.Writestring('Variables', 'Number of buffers', IntToStr(Number_of_Buffers));
  Inifile.writestring('Variables', 'ktc low', IntToStr(ktc_low));
  Inifile.writestring('Variables', 'ktc high', IntToStr(ktc_high));
  Inifile.writestring('Variables', 'ktc limit', FloatToStr(ktc_limit));
  Inifile.writestring('Variables', 'ktil default', IntToStr(ktil_Default));
  Inifile.writestring('Variables', 'ktil threshold', FloatToStr(ktil_threshold));
  Inifile.writestring('Variables', 'Clay content parent material', FloatToStr(clay_parent));
  Inifile.writestring('Variables', 'Parcel connectivity cropland', IntToStr(TFSED_crop));
  Inifile.writestring('Variables', 'Parcel connectivity forest', IntToStr(TFSED_forest));
  Inifile.writestring('Variables', 'Parcel trapping efficiency cropland', IntToStr(PTEFValueCropland
  ));
  Inifile.writestring('Variables', 'Parcel trapping efficiency forest', IntToStr(PTEFValueForest));
  Inifile.writestring('Variables', 'Parcel trapping efficiency pasture', IntToStr(PTEFValuePasture))
  ;
  Inifile.Writestring('Variables', 'Desired timestep for model', IntToStr(Timestep_model));
  Inifile.writestring('Variables', 'Endtime model', IntToStr(Endtime_model));
  Inifile.Writestring('Variables', 'Final timestep output', IntToStr(Timestep_output));

  If (Include_buffer) Then
    Begin
      For i := 1 To Number_of_Buffers Do
        Begin
          Buffername := 'Buffer ' + IntToStr(i);
          inifile.Writestring(Buffername, 'Volume', floattostr(Bufferdata[i].Volume));
          inifile.Writestring(Buffername, 'Height dam', floattostr(Bufferdata[i].Height_dam));
          inifile.Writestring(Buffername, 'Height opening', floattostr(Bufferdata[i].Height_opening)
          );
          inifile.Writestring(Buffername, 'Opening area', floattostr(Bufferdata[i].Opening_area));
          inifile.Writestring(Buffername, 'Discharge coefficient', floattostr(Bufferdata[i].Cd));
          inifile.Writestring(Buffername, 'Width dam', floattostr(Bufferdata[i].width_dam));
          inifile.Writestring(Buffername, 'Trapping efficiency', floattostr(Bufferdata[i].PTEF));
          inifile.Writestring(Buffername, 'Extension ID', inttostr(Bufferdata[i].ext_ID));
        End;
    End;

  inifile.Destroy;
  // Het inifile wordt vrijgegeven

End;

Procedure Write_cmd;

Var 
  CmdFile: textfile;
Begin
  SetCurrentDir(datadir);
  assignfile(CmdFile,'Cmd file.cmd');
  rewrite(CmdFile);
  Writeln(CmdFile, '"' + ModelExe + '"' + ' "' + datadir + 'CN_WS_event.ini' + '"');
  closefile(CmdFile);

  CmdFilename := datadir + 'Cmd file.cmd';
End;

Procedure WriteOutput;

Var 
  datafile: textfile;
  i,j, timestep_fin : integer;
  s1,s2 : string;
Begin

  // write maps
  If Write_Sediexport_tot Then
    Begin
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_tot'+'.rst', SediExport_tot);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediIn_tot'+'.rst', SediIn_tot);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediOut_tot'+'.rst', SediOut_tot);
    End;

  If Write_TOTRUN_tot Then
    writeidrisi32file(ncol,nrow,File_output_dir+'Total runoff_tot'+'.rst',TotRun_tot);

  If Write_WATEREROS_tot Then
    Begin
      writeIdrisi32file(ncol,nrow,File_output_dir+'Watereros_mm_total'+'.rst', Watereros_tot);
      writeIdrisi32file(ncol,nrow,File_output_dir+'Watereros_kg_total'+'.rst', Watereros_kg_tot);
    End;

  If write_RE_tot Then
    writeidrisi32file(ncol,nrow,File_output_dir+'Remap_total'+'.rst',ReMap_tot);

  If write_RUSLE_tot Then
    writeidrisi32file(ncol,nrow,File_output_dir+'RUSLE_total'+'.rst',RUSLE_tot);

  If Not simplified Then
    Begin
      If convert_output Then
        Begin
          s1 := '(min)';
          s2 := 'minutes';
          timestep_fin := Timestep_output;
        End
      Else
        Begin
          s1 := '(sec)';
          s2 := 'seconds';
          timestep_fin := Timestep_model;
        End;

      // write text files

      // TotDischarge_tot
      setcurrentDir(File_output_dir);
      assignfile(datafile,'Total discharge_longterm.txt');
      rewrite(datafile);
      Writeln(datafile, 'Total discharge at each outlet [m³]');
      // write title
      Write(datafile, 'Outlet ID',chr(9),'Discharge');
      // write column headings
      writeln(datafile,'');
      // go to next line
      For i := 0 To numOutlet-1 Do
        Begin
          Write(datafile, floattostr(TotDischarge_tot[i,0]), chr(9), floattostr(TotDischarge_tot[i,1
                                                                                ]));
          writeln(datafile, '');
        End;
      closefile(datafile);

      // spillover_tot
      If Include_buffer Then
        Begin
          setcurrentDir(File_output_dir);
          assignfile(datafile,'Spillover per buffer_longterm.txt');
          rewrite(datafile);
          Writeln(datafile, 'Amount of water flowing over dam for each buffer [m³]');
          // write title
          Write(datafile, 'Buffer ID',chr(9),'Spillover');
          // write column headings
          writeln(datafile,'');
          // go to next line
          For i := 0 To number_of_buffers-1 Do
            Begin
              Write(datafile, floattostr(Spillover_tot[i,0]), chr(9), floattostr(Spillover_tot[i,1])
              );
              writeln(datafile, '');
            End;
          closefile(datafile);
          //The memory of Discharge is released
        End;

      // sewer_out_water_tot

      If include_sewer Then
        Begin
          setcurrentDir(File_output_dir);
          assignfile(datafile, 'Sewer output water_longterm.txt');
          rewrite(datafile);
          Writeln(datafile,
                  'Total amount of water flowing out of the system through the sewer network [m³]')
          ;
          Writeln(datafile, floattostr(sewer_out_water_tot));
          closefile(datafile);
        End;

      // Discharge_tot
      setcurrentDir(File_output_dir);
      assignfile(datafile,'Discharge_longterm.txt');
      rewrite(datafile);
      Writeln(datafile, 'Discharge at each outlet [m³/s]');
      // write title
      Write(datafile, 'Time ',s1,chr(9));
      For j := 1 To numOutlet Do
        write(datafile, 'Outlet ', inttostr(j), chr(9));
      // write column headings
      writeln(datafile,'');
      For i := 0 To Dimension_result-1 Do
        Begin
          write(datafile, inttostr(trunc(Discharge_tot[i,0])), chr(9));
          // first column containing time is written to the .txt file
          For j := 1 To numOutlet Do
            write(datafile, floattostr(Discharge_tot[i,j]), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(datafile, '');
        End;
      closefile(datafile);

      // Sedconc_tot
      setcurrentDir(File_output_dir);
      assignfile(datafile,'Sediment concentration_longterm.txt');
      rewrite(datafile);
      Writeln(datafile, 'Sediment concentration at each outlet [g/l]');
      // write title
      Write(datafile, 'Time ',s1,chr(9));
      For j := 1 To numOutlet Do
        write(datafile, 'Outlet ', inttostr(j), chr(9));
      // write column headings
      writeln(datafile,'');
      For i := 0 To Dimension_result-1 Do
        Begin
          write(datafile, inttostr(trunc(Sedconc_tot[i,0])), chr(9));
          // first column containing time is written to the .txt file
          For j := 1 To numOutlet Do
            write(datafile, floattostr(Sedconc_tot[i,j]), chr(9));
          //Sediment concentration per time step is written to the .txt file
          writeln(datafile, '');
        End;
      closefile(datafile);

      // Sediment_tot
      setcurrentDir(File_output_dir);
      assignfile(datafile,'Sediment_longterm.txt');
      rewrite(datafile);
      Writeln(datafile, 'Sediment passing at each outlet [kg] with ',timestep_Fin,' ',s2,' timestep'
      );
      // write title
      Write(datafile, 'Time ',s1,chr(9));
      For j := 1 To numOutlet Do
        write(datafile, 'Outlet ', inttostr(j), chr(9));
      // write column headings
      writeln(datafile,'');
      For i := 0 To Dimension_result-1 Do
        Begin
          write(datafile, inttostr(trunc(Sediment_tot[i,0])), chr(9));
          // first column containing time is written to the .txt file
          For j := 1 To numOutlet Do
            write(datafile, floattostr(Sediment_tot[i,j]), chr(9));
          //Amount of sediment per time step is written to the .txt file
          writeln(datafile, '');
        End;
      closefile(datafile);

      If VHA Then
        Begin
          // Discharge_VHA_tot
          setcurrentDir(File_output_dir);
          assignfile(datafile,'Discharge_VHA_longterm.txt');
          rewrite(datafile);
          Writeln(datafile, 'Discharge to each river segment [m³/s]');
          // write title
          Write(datafile, 'Time ',s1,chr(9));
          For j := 1 To numVHA Do
            write(datafile, 'VHA segment ', inttostr(j), chr(9));
          // write column headings
          writeln(datafile,'');
          For i := 0 To Dimension_result-1 Do
            Begin
              write(datafile, inttostr(trunc(Discharge_VHA_tot[i,0])), chr(9));
              // first column containing time is written to the .txt file
              For j := 1 To numVHA Do
                write(datafile, floattostr(Discharge_VHA_tot[i,j]), chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(datafile, '');
            End;
          closefile(datafile);

          // Sedconc_VHA_tot
          setcurrentDir(File_output_dir);
          assignfile(datafile,'Sediment concentration_VHA_longterm.txt');
          rewrite(datafile);
          Writeln(datafile, 'Sediment concentration for each river segment [g/l]');
          // write title
          Write(datafile, 'Time ',s1,chr(9));
          For j := 1 To numVHA Do
            write(datafile, 'VHA segment ', inttostr(j), chr(9));
          // write column headings
          writeln(datafile,'');
          For i := 0 To Dimension_result-1 Do
            Begin
              write(datafile, inttostr(trunc(Sedconc_VHA_tot[i,0])), chr(9));
              // first column containing time is written to the .txt file
              For j := 1 To numVHA Do
                write(datafile, floattostr(Sedconc_VHA_tot[i,j]), chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(datafile, '');
            End;
          closefile(datafile);

          // Sediment_VHA_tot
          setcurrentDir(File_output_dir);
          assignfile(datafile,'Sediment_VHA_longterm.txt');
          rewrite(datafile);
          Writeln(datafile, 'Sediment flowing in each river segment [kg] with',timestep_fin,' ',s2,
                  ' timestep');
          // write title
          Write(datafile, 'Time ',s1,chr(9));
          For j := 1 To numVHA Do
            write(datafile, 'VHA segment ', inttostr(j), chr(9));
          // write column headings
          writeln(datafile,'');
          For i := 0 To Dimension_result-1 Do
            Begin
              write(datafile, inttostr(trunc(Sediment_VHA_tot[i,0])), chr(9));
              // first column containing time is written to the .txt file
              For j := 1 To numVHA Do
                write(datafile, floattostr(Sediment_VHA_tot[i,j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(datafile, '');
            End;
          closefile(datafile);
        End;
    End
  Else
    Begin
      If VHA Then
        Begin
          // TotSedimentVHA_tot
          setcurrentDir(File_output_dir);
          assignfile(datafile,'Total sediment VHA_longterm.txt');
          rewrite(datafile);
          Writeln(datafile, 'Total sediment flowing into each VHA river segment [kg]');
          // write title
          Write(datafile, 'VHA segment',chr(9),'Sediment');
          // write column headings
          writeln(datafile,'');
          // go to next line
          For i := 0 To numVHA-1 Do
            Begin
              Write(datafile, floattostr(TotSedimentVHA_tot[i,0]), chr(9), floattostr(
                                                                                  TotSedimentVHA_tot
                                                                                      [i,1]));
              writeln(datafile, '');
            End;
          closefile(datafile);
        End;
    End;

  // TotSediment_tot
  setcurrentDir(File_output_dir);
  assignfile(datafile,'Total sediment_longterm.txt');
  rewrite(datafile);
  Writeln(datafile, 'Total erosion: ' + floattostr(TotalErosion_tot) + ' (kg)');
  Writeln(datafile, 'Total deposition: ' + floattostr(TotalDeposition_tot) + ' (kg)');
  Writeln(datafile, 'Sediment leaving the catchment, via the river: ' + floattostr(SedleavingRiv_tot
  ) + ' (kg)');
  Writeln(datafile, 'Sediment leaving the catchment, not via the river: ' + floattostr(
          SedLeaving_tot) + ' (kg)');
  Writeln(datafile, 'Sediment trapped in buffers: ' + floattostr(SedTrapBuffer_tot) + ' (kg)');
  Writeln(datafile, 'Sediment trapped in open water: ' + floattostr(SedTrapWater_tot) + ' (kg)');
  Writeln(datafile,'_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _');
  Writeln(datafile,'');
  Writeln(datafile, 'Total sediment passing at each outlet [kg]');
  // write title
  Write(datafile, 'Outlet ID',chr(9),'Sediment');
  // write column headings
  writeln(datafile,'');
  // go to next line
  For i := 0 To numOutlet-1 Do
    Begin
      Write(datafile, floattostr(TotSediment_tot[i,0]), chr(9), floattostr(TotSediment_tot[i,1]));
      writeln(datafile, '');
    End;
  closefile(datafile);

  // sewer_out_sediment_tot
  If include_sewer Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(datafile, 'Sewer output sediment_longterm.txt');
      rewrite(datafile);
      Writeln(datafile, 'Total amount of sediment leaving the system through the sewers [kg]');
      Writeln(datafile, floattostr(sewer_out_sediment_tot));
      closefile(datafile);
    End;

End;

Procedure WriteOutput_event (eventID: integer);

Var 
  outputEvent: textfile;
  year_sim, i, j, nr: integer;
  Rainfallseries_output, Rainfall_temp: floatArray;
  Timeseries_output, Timeseries_temp: integerArray;

Begin

  If calc_year(eventID) = 1 Then       // calculate year of simulation
    year_sim := year
  Else
    year_sim := year+1;

  // convert rainfall data to timestep of model output if necessary
  If Not simplified Then
    Begin
      If Convert_output Then           // write rainfall using output timestep
        Begin
          If Timestep_output > 1 Then
            Begin
              nr := ((length(Timeseries_event)-1) Div Timestep_output) + 1;
              setlength(TimeSeries_output, nr);
              setlength(RainfallSeries_output, nr);
              TimeSeries_output[0] := TimeSeries_event[0];
              For i := 1 To nr-1 Do
                Begin
                  TimeSeries_output[i] := TimeSeries_output[i-1]+Timestep_output;
                End;
              RainfallSeries_output := extrap(TimeSeries_event, TimeSeries_output,
                                       RainfallSeries_event);
            End
          Else
            Begin
              Timeseries_output := Timeseries_event;
              Rainfallseries_output := Rainfallseries_event;
            End;
        End
        // write rainfall file using model timestep
      Else
        Begin
          SetLength(Rainfallseries_output, Event_meta[eventID].NumberOfElements+1);
          Rainfallseries_output[0] := 0;
          For i := 1 To length(Rainfallseries_output)-1 Do
            Rainfallseries_output[i] := Events[i-1,eventID];
          SetLength(Timeseries_output, Event_meta[eventID].NumberOfElements+1);
          Timeseries_output[0] := 0;
          For i := 1 To length(Timeseries_output)-1 Do
            Timeseries_output[i] := Timeseries_output[i-1]+timestep_model;
        End;
    End;

  setcurrentDir(File_output_dir);
  assignfile(outputEvent,'Output event '+inttostr(eventID)+'.txt');
  rewrite(outputEvent);
  Writeln(outputEvent, 'Event', chr(9), inttostr(eventID));
  Writeln(outputEvent, 'Month', chr(9), inttostr(calc_month(eventID)));
  Writeln(outputEvent, 'Season', chr(9), calc_season(eventID));
  Writeln(outputEvent, 'Year', chr(9), inttostr(year_sim));
  Writeln(outputEvent, '');
  Writeln(outputEvent, 'SUMMARY OUTPUT');
  Writeln(outputEvent, '');
  Writeln(OutputEvent, 'Total erosion (kg)', chr(9), floattostr(TotalErosion_event));
  Writeln(OutputEvent, 'Total deposition (kg)', chr(9), floattostr(TotalDeposition_event));
  Writeln(OutputEvent, 'Sediment leaving catchment via river (kg)', chr(9), floattostr(
                                                                                 SedleavingRiv_event
  ));
  Writeln(OutputEvent, 'Sediment leaving catchment not via river (kg)', chr(9), floattostr(
                                                                                    SedLeaving_event
  ));
  Writeln(OutputEvent, 'Sediment trapped in buffers (kg)', chr(9), floattostr(SedTrapBuffer_event));
  Writeln(OutputEvent, 'Sediment trapped in open water (kg)', chr(9), floattostr(SedTrapWater_event)
  );
  Writeln(OutputEvent, '');
  Write(outputEvent, '', chr(9));
  For i := 1 To numOutlet Do
    Write(OutputEvent, 'Outlet ', inttostr(i),  chr(9));
  Writeln(OutputEvent, '');
  Write (OutputEvent, 'Total sediment load (kg)', chr(9));
  For i := 0 To numOutlet-1 Do
    Write(OutputEvent, floattostr(TotSediment_event[i,1]), chr(9));
  Writeln(OutputEvent, '');
  If Not simplified Then
    Begin
      Write (OutputEvent, 'Total runoff (m³)', chr(9));
      For i := 0 To numOutlet-1 Do
        Write(OutputEvent, floattostr(TotDischarge_event[i,1]), chr(9));
      Writeln(OutputEvent, '');
      Write (OutputEvent, 'Average sediment concentration (g/l)', chr(9));
      For i := 1 To numOutlet Do
        Write(OutputEvent, floattostr(sedconc_event[2,i]), chr(9));
      Writeln(OutputEvent, '');
      Write (OutputEvent, 'Average clay concentration of sediment (%)', chr(9));
      If est_clay Then
        Begin
          For i := 0 To numOutlet-1 Do
            Write(OutputEvent, floattostr(clay_cont[i,1]), chr(9));
        End;
      Writeln(OutputEvent, '');
      Writeln(OutputEvent, '');
      Write(OutputEvent, 'Sewer output water (m³)', chr(9));
      If include_sewer Then
        Write(floattostr(sewer_out_water_event));
      Writeln(OutputEvent, '');
    End;

  Writeln(OutputEvent, '');
  Write(OutputEvent, 'Sewer output sediment (kg)', chr(9));
  If include_sewer Then
    Write(floattostr(sewer_out_sediment_event));
  Writeln(OutputEvent, '');
  Writeln(OutputEvent, '');

  If Not simplified Then
    Begin
      If include_buffer Then
        Begin
          Write(outputEvent, '', chr(9));
          For i := 1 To Number_of_Buffers Do
            Write(OutputEvent, 'Buffer ', inttostr(i),  chr(9));
        End;
      Writeln(OutputEvent, '');
      Write(OutputEvent, 'Buffer spillover (m³)', chr(9));
      If include_buffer Then
        Begin
          For i := 0 To Number_of_Buffers-1 Do
            Write(OutputEvent, floattostr(Spillover_event[i,1]), chr(9));
        End;
      Writeln(OutputEvent, '');
      Writeln(OutputEvent, '');
      Writeln(OutputEvent, 'TIMESERIES');
      Writeln(OutputEvent, 'Rainfall (mm)');
      For i := 0 To length(Timeseries_output)-1 Do
        Writeln(OutputEvent, inttostr(TimeSeries_output[i]), chr(9), floattostr(
                                                                               Rainfallseries_output
                                                                                [i]));
      Writeln(OutputEvent, '');
      Write (OutputEvent, 'Discharge (m³/s)', chr(9));
      For i := 1 To numOutlet Do
        Write(OutputEvent, 'Outlet ', inttostr(i),  chr(9));
      Writeln(OutputEvent, '');
      For i := 0 To length(Discharge_event)-1 Do
        Begin
          Write(OutputEvent, inttostr(trunc(Discharge_event[i,0])), chr(9));
          For j := 1 To numOutlet Do
            write(OutputEvent, floattostr(Discharge_event[i,j]), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(OutputEvent, '');
        End;
      Writeln(OutputEvent, '');
      Write (OutputEvent, 'Sediment concentration (g/l)', chr(9));
      For i := 1 To numOutlet Do
        Write(OutputEvent, 'Outlet ', inttostr(i),  chr(9));
      Writeln(OutputEvent, '');
      For i := 0 To length(Sedconc_event)-1 Do
        Begin
          Write(OutputEvent, inttostr(trunc(Sedconc_event[i,0])), chr(9));
          For j := 1 To numOutlet Do
            write(OutputEvent, floattostr(Sedconc_event[i,j]), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(OutputEvent, '');
        End;
      Writeln(OutputEvent, '');
      Write (OutputEvent, 'Sediment load (kg)', chr(9));
      For i := 1 To numOutlet Do
        Write(OutputEvent, 'Outlet ', inttostr(i),  chr(9));
      Writeln(OutputEvent, '');
      For i := 0 To length(Sediment_event)-1 Do
        Begin
          Write(OutputEvent, inttostr(trunc(Sediment_event[i,0])), chr(9));
          For j := 1 To numOutlet Do
            write(OutputEvent, floattostr(Sediment_event[i,j]), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(OutputEvent, '');
        End;
      Writeln(OutputEvent, '');
      Writeln(OutputEvent, '');

      If VHA Then
        Begin
          Writeln(OutputEvent, 'RIVER SEGMENTS');
          Write (OutputEvent, 'Discharge (m³/s)', chr(9));
          For i := 1 To numVHA Do
            Write(OutputEvent, 'Segment ', inttostr(i),  chr(9));
          Writeln(OutputEvent, '');
          For i := 0 To length(Discharge_VHA_event)-1 Do
            Begin
              Write(OutputEvent, inttostr(trunc(Discharge_VHA_event[i,0])), chr(9));
              For j := 1 To numVHA Do
                write(OutputEvent, floattostr(Discharge_VHA_event[i,j]), chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(OutputEvent, '');
            End;
          Writeln(OutputEvent, '');
          Write (OutputEvent, 'Sediment concentration (g/l)', chr(9));
          For i := 1 To numVHA Do
            Write(OutputEvent, 'Segment ', inttostr(i),  chr(9));
          Writeln(OutputEvent, '');
          For i := 0 To length(Sedconc_VHA_event)-1 Do
            Begin
              Write(OutputEvent, inttostr(trunc(Sedconc_VHA_event[i,0])), chr(9));
              For j := 1 To numVHA Do
                write(OutputEvent, floattostr(Sedconc_VHA_event[i,j]), chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(OutputEvent, '');
            End;
          Writeln(OutputEvent, '');
          Write (OutputEvent, 'Sediment load (kg)', chr(9));
          For i := 1 To numVHA Do
            Write(OutputEvent, 'Segment ', inttostr(i),  chr(9));
          Writeln(OutputEvent, '');
          For i := 0 To length(Sediment_VHA_event)-1 Do
            Begin
              Write(OutputEvent, inttostr(trunc(Sediment_VHA_event[i,0])), chr(9));
              For j := 1 To numVHA Do
                write(OutputEvent, floattostr(Sediment_VHA_event[i,j]), chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(OutputEvent, '');
            End;
          Writeln(OutputEvent, '');

        End;

    End
  Else        // if simplified...
    Begin

      Writeln(OutputEvent, '');
      Writeln(OutputEvent, 'RIVER SEGMENTS');
      Write(outputEvent, '', chr(9));
      For i := 1 To numVHA Do
        Write(OutputEvent, 'Segment ', inttostr(i),  chr(9));
      Writeln(OutputEvent, '');
      Write (OutputEvent, 'Total sediment load (kg)', chr(9));
      For i := 0 To numVHA-1 Do
        Write(OutputEvent, floattostr(TotSedimentVHA_event[i,1]), chr(9));
      Writeln(OutputEvent, '');
      Writeln(OutputEvent, '');
      Writeln(OutputEvent, 'TIMESERIES');
      Writeln(OutputEvent, 'Rainfall (mm)');
      For i := 0 To length(Rainfallseries_event)-1 Do
        Writeln(OutputEvent, inttostr(TimeSeries_event[i]), chr(9), floattostr(Rainfallseries_event[
                                                                               i]));
      Writeln(OutputEvent, '');
    End;
  closefile(outputEvent);
End;

End.
