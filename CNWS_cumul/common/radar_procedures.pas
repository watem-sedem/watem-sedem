unit radar_procedures;


// This file contains copys of procedures from the standard model which are adapted to work with rainfall data.
// These adaptations are mostly related to the 3D nature of the radar rainfall data (x,y and time).
// At several points in these procedures are arrays wiped from memory, after this point the data in the arrays is not necessary anymore for the scripts.
// Wiping this data from the memory reduces the memory load using radar data in the model has and frees up RAM.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Math, Forms, Controls, Graphics, Dialogs, RData_CN, CN_calculations, LateralRedistribution, Raster_calculations, ReadInParameters;

type
  IntegerArray = array Of integer;
  FloatArray = array Of Double;
  RadarRaster = array of array of Double;
  TForm2 = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;
  
// after each of the procedures is indicated from which file they originate.
Procedure PreparRadarRainfallData (Var Radar_dir: String; timestepRadar: Integer); //LateralRedistribution
Procedure GetRfileRadar(Var Z:RRaster; Filename:String); //rdata_cn
Procedure SetDynamicRDataRadar(Var Z:RadarRaster); //rdata_cn
Procedure SetzeroRRadar(Var z:RadarRaster); //rdata_cn
Procedure InitializeRadar ( Var InitilizationDataset: Array Of RadarRaster; Number_Radarfiles: Integer; Mask: Array Of RRaster);
Procedure CalculateRFactorRadar (Var TimeSeries: integerArray; timestepRadar: integer; RadarRaindataset_src: Array Of RadarRaster); //cn_calculations
Procedure CalculateReRadar(Var Remap:Rraster; RainfallSum:RadarRaster; I10Radar:RadarRaster; Perceelskaart:Rraster ; CNmap:Rraster; alpha,beta:double); //cn_calculations
Procedure CalculateTimeDependentRunoffRadar(Var Remap: Rraster; Routing:
                                       TRoutingArray; PRC: Rraster); //cn_calculations
Procedure WaterRadar; //LateralRedistribution
Procedure Distribute_sedimentRadar; //LateralRedistribution

var
  Form2: TForm2;
  i: integer;
  j: integer;
  time: integer;
  k: integer;
  l: integer;
  x: double;
  nr: integer;
  Fill_num: integer;
  bestand: string;
  RadarRaindata: RadarRaster;
  RainfallSum: RadarRaster;
  StartTime_rain_radar: RadarRaster;
  EndTime_rain_radar: RadarRaster;
  Duration_rain_radar: RadarRaster;
  I10Radar: RadarRaster;
  TimeSeries_output: IntegerArray;
  I10TimeSeries: IntegerArray;
  RadarRaindataset_src: Array Of RadarRaster;
  RadarRaindataset_src_single: Array Of RRaster;
  RadarRaindataset_dst: Array Of RadarRaster;
  RfactorRadar: RadarRaster;
  RadarFiles: TStringList;
  Src_timeseries: IntegerArray;
  Cumul_rain_radar_array: FloatArray;
  Cumul_Interp_radar_array: FloatArray;
  I10RainfallSeries_Interp_radar_array: FloatArray;
  I10RainfallSeries_radar_array: FloatArray;
  I10RainfallSeries_radI10_array: FloatArray;
  I10RainfallSeriesRadar: Array Of RadarRaster;
  I10Series_radar: FloatArray;
  Cumul_rain_radar: Array Of RadarRaster;
  Cumul_Interp_radar: Array Of RadarRaster;
  Rain_fraction_radar: Array Of RadarRaster;

  SEDI_OUT: RRaster;
  SEDI_IN: RRaster;
  Waterero, sedprod, depprod: double;
  SedLoad, SedLoad_VHA: RVector;

implementation

Procedure ReadRadarRainfallFile (Var RadarRaindata : RRaster; bestand: String);
Begin

  GetRFileRadar(RadarRaindata,bestand);

End;
//********************************************************************
//De waarden van de buitenste cellen worden vervangen door de waarden van de
//cellen die een laag meer naar het midden liggen
//********************************************************************
Procedure SetRasterBorders(Var Z:RRaster);

Var
i,j       : integer;
Begin
Z[0,0] := Z[1,1];
Z[0,(ncol+1)] := Z[1,ncol];
Z[nrow+1,0] := Z[nrow,1];
Z[nrow+1,ncol+1] := Z[nrow,ncol];
For j := 1 To ncol Do
Begin
  Z[0,j] := Z[1,j];
  Z[(nrow+1),j] := Z[nrow,j];
End;
For  i := 1 To nrow Do
Begin
  Z[i,0] := Z[i,1];
  Z[i,ncol+1] := Z[i,ncol];
End;
End;
//********************************************************************
    //In onderstaande regels wordt het RDC bestand van elke .rst kaart gescand
    //en wordt de nodige informatie hieruit gehaald.
    //********************************************************************

    Procedure GetRFileRadar(Var Z:RRaster; Filename:String);

    Var
      i,j,hulpgetal: integer;
      docfileIMG : textfile;
      fileIMG : file Of single ;
      textfileIMG : textfile ;
      docnfileIMG,NfileIMG,dumstr : string;
      idrisi32,asciidatatype : boolean;
    Begin
      dumstr := ExtractFilename(filename);
      If ExtractFileExt(dumstr)='.img' Then
        idrisi32 := false
      Else idrisi32 := true;

      hulpgetal := length(dumstr)-2;
      delete(dumstr,hulpgetal,3);
      //De extensie wordt verwijderd
      If Idrisi32 Then //Voor Idrisi32 bestanden
        Begin
          docNfileIMG := dumstr + 'rdc' ;
          NfileIMG := dumstr + 'rst';
          // ==> De namen van de betreffende .rst en .rdc bestanden worden nagemaakt
        End
      Else //Voor .IMG bestanden
        Begin
          docNfileIMG := dumstr + 'doc' ;
          NfileIMG := dumstr + 'img';
        End;
      // INLEZEN NCOLS
      Assignfile(docfileIMG, docNfileIMG);
      //Een 'filehandle' wordt toegewezen aan de bestanden
      reset(docfileIMG);
      //Het .rdc bestand wordt geopend om te lezen
      If Idrisi32 Then
        For i := 1 To 3 Do
          readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      //Na 14 tekens staat het data type
      If (dumstr='integer') Or (dumstr='byte') Then
        Begin
          closefile(docfileIMG);
          Raise ERasterException.Create(
                 'Error in reading one of the rasters: data type must be real, please re-enter data'
          );
        End;
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      //Het filetype wordt opgeslagen
      If dumstr='binary' Then
        asciidatatype := false
      Else asciidatatype := true;
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      ncol := strtoint(dumstr);
      // INLEZEN NROWS
      readln(docfileIMG, dumstr);
      delete (dumstr,1,14);
      nrow := strtoint(dumstr);
      readln(docfileIMG, dumstr);
      delete(dumstr,1,14);
      If (dumstr='plane') Or (dumstr='') Then Raster_Projection := plane
      Else Raster_Projection := LATLONG;
      readln(docfileIMG);
      readln(docfileIMG);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      MINX := strtofloat(dumstr);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      MAXX := strtofloat(dumstr);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      MINY := strtofloat(dumstr);
      readln(docfileIMG,dumstr);
      delete(dumstr,1,14);
      MAXY :=  strtofloat(dumstr);
      readln(docfileIMG);
      readln(docfileIMG, dumstr);
      delete(dumstr,1,14);
      res := strtofloat(dumstr);
      If (res=0.0) Then
        Begin
          Raise ERasterException.Create('Error in reading one of the rasters: Resolution is invalid'
          );
        End;

      // Inlezen gegevens

      //Er wordt geheugen vrijgemaakt voor de kaart (array) in kwestie
      SetDynamicRData(Z);

      //Het .rst bestand wordt ingelezen en opgeslaan
      If asciidatatype Then
        Begin
          assignfile(textFileIMG, NfileIMG);
          reset (textfileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              read(textfileIMG, Z[i,j]);
          Closefile(textfileimg);
        End
      Else
        Begin
          assignfile(FileIMG, NfileIMG);
          reset (fileIMG);
          For i:= 1 To nrow Do
            For j:= 1 To ncol Do
              read(fileIMG, Z[i,j]);
          Closefile(Fileimg);
        End;
      Closefile(DocfileImg);

      //De buitenste waarden van het raster worden aangepast
      SetRasterBorders(Z);

      //ncol, nrow en res worden opgeslagen in array zodat achteraf kan worden nagegaan
      //of deze voor alle kaarten gelijk zijn
      lengthAR := lengthAR + 1;
      SetLength(nrowAR, lengthAR);
      SetLength(ncolAR, lengthAR);
      SetLength(resAR, lengthAR);
      nrowAR[lengthAR-1] := NROW;
      ncolAR[lengthAR-1] := NCOL;
      resAR[lengthAR-1] := RES;

    End;

    //********************************************************************
    //In onderstaande regels wordt er geheugen vrij gemaakt voor de verschillende
    //arrays die de kaarten voorstellen
    //********************************************************************
    Procedure SetDynamicRDataRadar(Var Z:RadarRaster);

    Var
      i       : integer;
    Begin
      SetLength(Z,nrow+2);
      For i := Low(Z) To high(Z) Do
        Setlength(Z[i],ncol+2);
    End;

    //*****************************************************************
    //Deze procedure geeft een nulwaarde aan elk element in een Rraster
    //*****************************************************************
    Procedure SetzeroRRadar(Var z:RadarRaster);

    Var
      i,j: integer;
    Begin
      For i:=Low(Z) To High(Z) Do
        For j:=Low(Z[i]) To High(Z[i]) Do
          Begin
            Z[i,j] := 0
          End;
    End;

Procedure PreparRadarRainfallData (Var Radar_dir: String; timestepRadar: Integer);
   Begin
     setCurrentDir(Radar_dir);
     RadarFiles:= FindAllFiles(Radar_dir, '*.rst', true);
     Setlength(RadarRaindataset_src_single,RadarFiles.count);
     Setlength(Src_timeseries,RadarFiles.count);
     Setlength(TimeSeries,RadarFiles.count);
     repeat
         j:= (i) * timestepRadar;
         bestand:= Radar_dir + IntToStr(j) + '.rst';
         if FileExists(bestand) then
            Begin
                 ReadRadarRainfallFile (RadarRaindataset_src_single[i], bestand);
                 Src_timeseries[i]:= j;
            End;


         i:= i + 1
     until Not FileExists(bestand);


     Setlength (RadarRaindataset_src, RadarFiles.count);
     InitializeRadar ( RadarRaindataset_src, RadarFiles.count, RadarRaindataset_src_single);


     For i := 0 To RadarFiles.count - 1 Do
             Begin
                  For k := Low(RadarRaindataset_src_single[i]) To High(RadarRaindataset_src_single[i]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src_single[i,k]) To High(RadarRaindataset_src_single[i,k]) - 1 Do
                               Begin
                                    RadarRaindataset_src[i,k,l] := RadarRaindataset_src_single[i,k,l];
                               End
                      End

             End;


     TimeSeries := Src_timeseries;


    If (Simplified) Then   // if timestep rainfall is used as model timestep...
       Begin
            Timestep_model := timestepRadar;
            TimeSeries_output := Src_timeseries;
            For i := 1 To RadarFiles.count - 1 Do
             Begin
                  For k := Low(RadarRaindataset_src[i]) To High(RadarRaindataset_src[i]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[i,k]) To High(RadarRaindataset_src[i,k]) - 1 Do
                               Begin
                                    RadarRaindataset_dst[i,k,l] := RadarRaindataset_src[i,k,l];
                               End
                      End

             End
       End

    Else    // interpolation of rainfall data to desired timestep
       Begin;

         Setlength (Cumul_rain_radar, RadarFiles.count);
         InitializeRadar ( Cumul_rain_radar, RadarFiles.count, RadarRaindataset_src_single);

         // calculate cumulatieve rainfall series and a correction for negative rain values on radar image
         For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                               Begin
                                    Cumul_rain_radar[0,k,l] := RadarRaindataset_src[0,k,l];
                               End
                      End;

         For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                               Begin
                                 If RadarRaindataset_src[0,k,l] < 0  Then
                                    Cumul_rain_radar[0,k,l]:= 0
                               End
                      End;

         For i := 1 To RadarFiles.count - 1 Do
             Begin
                  For k := Low(RadarRaindataset_src[i]) To High(RadarRaindataset_src[i]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[i,k]) To High(RadarRaindataset_src[i,k]) - 1 Do
                               Begin
                                 If RadarRaindataset_src[i,k,l] < 0  Then
                                    Cumul_rain_radar[i,k,l]:= Cumul_rain_radar[i-1,k,l]
                                 Else
                                    Cumul_rain_radar[i,k,l]:= Cumul_rain_radar[i-1,k,l] + RadarRaindataset_src[i,k,l];
                               End
                      End
             End;

       // create new timeseries array
      NumberOfTimesteps := (RadarFiles.count - 1)*(timestepRadar Div Timestep_model);
      Setlength(TimeSeries_output, NumberOfTimesteps+1);
      TimeSeries_output[0] := Src_timeseries[0];
      For i := 1 To NumberOfTimesteps Do
        Begin
          TimeSeries_output[i] := TimeSeries_output[i-1] + Timestep_model;
        End;



       // interpolation of cumulative rainfall series, see function below

       Setlength (Cumul_Interp_radar, NumberOfTimesteps+1);
       InitializeRadar ( Cumul_Interp_radar, NumberOfTimesteps+1, RadarRaindataset_src_single);

       SetLength (Cumul_rain_radar_array, NumberOfTimesteps+1);
       For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
           Begin
                For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                    Begin
                       For i := 0 To RadarFiles.count - 1 Do
                           Begin
                             Cumul_rain_radar_array[i]:= Cumul_rain_radar[i,k,l];
                           End;

                       Cumul_Interp_radar_array := interp(TimeSeries, TimeSeries_output, Cumul_rain_radar_array);

                       For i := 0 To NumberOfTimesteps Do
                           Begin
                             Cumul_Interp_radar[i,k,l]:= Cumul_Interp_radar_array[i];
                           End
                    End
           End;

       // cumulative rainfall series is converted to new rainfall series

       Setlength (RadarRaindataset_dst, NumberOfTimesteps+1);
       InitializeRadar ( RadarRaindataset_dst, NumberOfTimesteps+1, RadarRaindataset_src_single);

       RadarRaindataset_dst[0] := Cumul_Interp_radar[0];

       For i := 1 To NumberOfTimesteps Do
           Begin
             For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                 Begin
                      For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                          Begin
                               RadarRaindataset_dst[i,k,l] :=  Cumul_Interp_radar[i,k,l] - Cumul_Interp_radar[i-1,k,l];
                          End
                 End;
           End;

     EndTime := TimeSeries_output[NumberOfTimesteps];

     // extra zeros are added to the rainfall series if necessary
      EndTime_modelSec := EndTime_model * 60;
      If EndTime_modelSec > EndTime Then
        Begin
          Fill_num := (EndTime_modelSec-EndTime) Div Timestep_model;
          If (EndTime_modelSec-Endtime) Mod Timestep_model <> 0 Then
            Begin
              Fill_num := Fill_num+1;
            End;
          NumberOfTimesteps := NumberOfTimesteps+Fill_num;
          setlength(TimeSeries_output, NumberOfTimesteps+1);
          setlength(RadarRaindataset_dst, NumberOfTimesteps+1);

          For i:= NumberOfTimesteps-Fill_num+1 To length(TimeSeries_output)-1 Do
            Begin
              TimeSeries_output[i] := TimeSeries_output[i-1]+Timestep_model;
              For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                 Begin
                   Setlength (RadarRaindataset_dst[i], High(RadarRaindataset_src[0]));
                      For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                          Begin
                               Setlength (RadarRaindataset_dst[i,k], High(RadarRaindataset_src[0,k]));
                               RadarRaindataset_dst[i,k,l] :=0;
                          End
                 End
            End;
        End;
     End;


  // calculate total amount of rainfall per pixel (mm)

  Setlength (RainfallSum, High(RadarRaindataset_src[0]));

   For i:= 0 To High(RadarRaindataset_src[0])-1 Do
            Begin
              Setlength (RainfallSum[i], High(RadarRaindataset_src[0,i]));
            End;

   SetzeroRRadar (RainfallSum);

   For i:= 0 To length(TimeSeries_output)-1 Do
            Begin
              For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                 Begin
                   For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                          Begin
                               RainfallSum[k,l] := RainfallSum[k,l] + RadarRaindataset_dst[i,k,l];
                               //write ('k: ',k, ' l: ',l,' i: ',i, ' dstRain: ', RainfallSum[k,l])
                          End
                 End
            End;

   Setlength (StartTime_rain_radar, High(RadarRaindataset_src[0]));

   For i:= 0 To High(RadarRaindataset_src[0])-1 Do
            Begin
              Setlength (StartTime_rain_radar[i], High(RadarRaindataset_src[0,i]));
            End;

   SetzeroRRadar (StartTime_rain_radar);

   Setlength (EndTime_rain_radar, High(RadarRaindataset_src[0]));

   For i:= 0 To High(RadarRaindataset_src[0])-1 Do
            Begin
              Setlength (EndTime_rain_radar[i], High(RadarRaindataset_src[0,i]));
            End;

   SetzeroRRadar (EndTime_rain_radar);

   Setlength (Duration_rain_radar, High(RadarRaindataset_src[0]));

   For i:= 0 To High(RadarRaindataset_src[0])-1 Do
            Begin
              Setlength (Duration_rain_radar[i], High(RadarRaindataset_src[0,i]));
            End;

   SetzeroRRadar (Duration_rain_radar);

   If Not Simplified Then
    Begin
      // calculate start time of rain
      For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
          Begin
               For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                   Begin
                        For i:= 0 To NumberOfTimesteps Do
                            Begin
                                 If RadarRaindataset_dst[i,k,l] <> 0 Then
                                    Begin
                                         StartTime_rain_radar [k,l] := TimeSeries_output[i-1];
                                         Break;
                                    End;
                            End
                  End
          End;

      // calculate end time of rain
      For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
          Begin
               For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                   Begin
                     For i:= NumberOfTimesteps Downto 0 Do
                         Begin
                              If RadarRaindataset_dst[i,k,l] <> 0 Then
                                 Begin
                                      EndTime_rain_radar [k,l] := TimeSeries_output[i];
                                      Break;
                                 End;
                         End
                   End
          End;
      // calculate duration of rain in min

      For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
          Begin
               For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                   Begin
                        Duration_rain_radar [k,l] := (EndTime_rain_radar [k,l] - StartTime_rain_radar [k,l])/60;
                   End
          End;

      // calculation of I10: rainfall series is converted to a one-minute series, after which
      // I10 is calculated

      If timestepRadar = 60 Then
        Begin
          I10TimeSeries := Src_timeseries;
          For i := 1 To RadarFiles.count - 1 Do
             Begin
                  For k := Low(RadarRaindataset_src[i]) To High(RadarRaindataset_src[i]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[i,k]) To High(RadarRaindataset_src[i,k]) - 1 Do
                               Begin
                                    I10RainfallSeriesRadar[i,k,l] := RadarRaindataset_src[i,k,l];
                               End
                      End

             End
        End
      Else If timestepRadar > 60 Then    // interpolation of the original series is needed
             Begin
               nr := (length(Src_timeseries)-1)*(timestepRadar Div 60);
               setlength(I10TimeSeries, nr+1);
               setlength(I10RainfallSeriesRadar, nr+1);
               InitializeRadar ( I10RainfallSeriesRadar, nr+1, RadarRaindataset_src_single);
               I10TimeSeries[0] := Src_timeseries[0];
               For i := 1 To nr Do
                 I10TimeSeries[i] := I10TimeSeries[i-1] + 60;
                 For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                               Begin
                                    I10RainfallSeriesRadar[0,k,l] := RadarRaindataset_src[0,k,l];
                               End
                      End;
               For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                          Begin
                               For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                                   Begin
                                        j := 1;
                                        For i := 1 To (length(Src_timeseries)-1) Do
                                            Begin
                                               x := RadarRaindataset_src[i,k,l]/(timestepRadar Div 60);
                                            While j <= i*(timestepRadar Div 60) Do
                                                  Begin
                                                       I10RainfallSeriesRadar[j,k,l] := x;
                                                       Inc(j);
                                                  End;
                                            End
                                   End
                          End;

             End
      Else           // extrapolation of the original series is needed
        Begin
          nr := (length(Src_timeseries)-1)Div(60 Div timestepRadar);
          setlength(I10TimeSeries, nr+1);
          setlength(I10RainfallSeries_radar_array, RadarFiles.count);
          setlength(I10RainfallSeries_Interp_radar_array, nr+1);
          setlength(I10RainfallSeriesRadar, nr+1);
          InitializeRadar ( I10RainfallSeriesRadar, nr+1, RadarRaindataset_src_single);
          I10TimeSeries[0] := Src_timeseries[0];
          For i := 1 To nr Do
            Begin
              I10TimeSeries[i] := I10TimeSeries[i-1]+60;
            End;
          For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
           Begin
                For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                    Begin
                       For i := 0 To RadarFiles.count - 1 Do
                           Begin
                             I10RainfallSeries_radar_array[i]:= RadarRaindataset_src[i,k,l];
                           End;

                       I10RainfallSeries_Interp_radar_array := extrap(Src_timeseries, I10TimeSeries, I10RainfallSeries_radar_array);

                       For i := 0 To nr Do
                           Begin
                             I10RainfallSeriesRadar[i,k,l]:= I10RainfallSeries_Interp_radar_array[i];
                           End;
                    End
           End;
        End;

      // for each minute, the intensity for the coming 10 minutes is calculated and
      // saved in array I10Series. Then the maximum value in this array is calculated.

      setlength(I10Series_radar, length(I10TimeSeries)-1);
      Setlength(I10RainfallSeries_radI10_array, length(I10TimeSeries)-1);

      Setlength (I10Radar, High(RadarRaindataset_src[0]));

      For i:= 0 To High(RadarRaindataset_src[0])-1 Do
            Begin
              Setlength (I10Radar[i], High(RadarRaindataset_src[0,i]));
            End;

      SetzeroRRadar (I10Radar);

      For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
           Begin
                For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                    Begin
                       For i := 0 To length(I10TimeSeries)-1 Do
                           Begin
                             I10RainfallSeries_radI10_array[i]:= I10RainfallSeriesRadar[i,k,l];
                           End;

                       For i := 1 To length(I10TimeSeries)-1 Do
                           Begin
                              I10Series_radar[i-1] := sumPartArray(I10RainfallSeries_radI10_array,i,9)/(1/6);
                           End;

                       I10Radar [k,l] := I10Series_radar[0];
                       For i := 1 To length(I10Series_radar)-1 Do
                           Begin
                                If I10Series_radar[i] > I10Radar [k,l] Then
                                   I10Radar [k,l] := I10Series_radar[i];
                           End;
                    End;
           End;

      End;

    //Time and Rainfall are written to a Record

     Setlength (Rain_fraction_radar, NumberOfTimesteps+1);
     InitializeRadar ( Rain_fraction_radar, NumberOfTimesteps+1, RadarRaindataset_src_single);

     For i := 0 To NumberOfTimesteps Do
             Begin
                  For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                               Begin
                                    Rain_fraction_radar [i,k,l] := RadarRaindataset_dst [i,k,l]/RainfallSum[k,l];
                               End
                      End;
             End;
    End;



Procedure InitializeRadar ( Var InitilizationDataset: Array Of RadarRaster; Number_Radarfiles: Integer; Mask: Array Of RRaster);
          Begin
            For i:= 0 To Number_Radarfiles - 1 Do
             Begin
               For k := Low(Mask[0]) To High(Mask[0]) - 1 Do
                   Begin
                     Setlength (InitilizationDataset[i], High(Mask[0]));
                           For l:= Low(Mask[0,k]) To High(Mask[0,k]) - 1 Do
                               Begin
                                 Setlength (InitilizationDataset[i,k], High(Mask[0,k]));
                                 InitilizationDataset[i,k,l] := 0
                               End
                     end
             end
          end;


Procedure CalculateRFactorRadar (Var TimeSeries: integerArray; timestepRadar: integer; RadarRaindataset_src: Array Of RadarRaster);

Var
  newTimeSeries, minTimeSeries : integerArray;
  minRainfallSeries_extrap_array, minRainfallSeries_array, rainEnergy, newRainfallSeries_extrap_array, newRainfallSeries_array, newRainfallSeries_while_array, rainVolume, I30 : floatArray;
  newRainfallSeries, minRainfallSeries : Array Of RadarRaster;
  tel, nr, j, index, Tini, Tfin : integer;
  x, eventEnergy, maxI30, eventRFactor: double;
         Begin
          Setlength (newRainfallSeries, Length(RadarRaindataset_src));
          Setlength (newTimeSeries, Length(TimeSeries));
          InitializeRadar ( newRainfallSeries, Length(RadarRaindataset_src), RadarRaindataset_src_single);

          Setlength (RfactorRadar, High(RadarRaindataset_src[0]));

          For i:= 0 To High(RadarRaindataset_src[0])-1 Do
            Begin
              Setlength (RfactorRadar[i], High(RadarRaindataset_src[0,i]));
            End;

          SetzeroRRadar (RfactorRadar);
          // Correction for Negative values in source radarraindataset
          For i := 1 To Length(TimeSeries) - 1 Do
             Begin
                  For k := Low(RadarRaindataset_src[i]) To High(RadarRaindataset_src[i]) - 1 Do
                      Begin
                           For l:= Low(RadarRaindataset_src[i,k]) To High(RadarRaindataset_src[i,k]) - 1 Do
                               Begin
                                 If RadarRaindataset_src[i,k,l] < 0  Then
                                    RadarRaindataset_src[i,k,l]:= 0
                               end
                      end
             end;
          If timestepRadar = 600 Then    //(=10 min)
             Begin
                  newTimeSeries := TimeSeries;
                  For tel := 0 To Length(RadarRaindataset_src) - 1 Do
                     Begin
                          For k := Low(RadarRaindataset_src[tel]) To High(RadarRaindataset_src[tel]) - 1 Do
                              Begin
                                   For l:= Low(RadarRaindataset_src[tel,k]) To High(RadarRaindataset_src[tel,k]) - 1 Do
                                       Begin
                                            newRainfallSeries[tel,k,l] := RadarRaindataset_src[tel,k,l];
                                       End
                              End
                     End;
             End
          Else
             Begin
                  If  timestepRadar > 60 then      // eerst omzetten naar tijdstap van 1 min (makkelijker)
                     Begin
                          nr := (length(TimeSeries)-1)*(timestepRadar Div 60);
                          setlength(minTimeSeries, nr+1);
                          setlength(minRainfallSeries, nr+1);
                          InitializeRadar ( minRainfallSeries, nr+1, RadarRaindataset_src_single);
                          minTimeSeries[0] := TimeSeries[0];
                          For i := 1 To nr Do
                              minTimeSeries[i] := minTimeSeries[i-1]+60;
                              For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                                  Begin
                                       For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                                           Begin
                                                minRainfallSeries[0,k,l] := RadarRaindataset_src[0,k,l];
                                           End
                                  End;
                              j := 1;
                              For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                                  Begin
                                       For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                                           Begin
                                                j := 1;
                                                For i := 1 To (length(TimeSeries)-1) Do
                                                      Begin
                                                         x := RadarRaindataset_src[i,k,l]/(timestepRadar Div 60);
                                                         While j <= i*(timestepRadar Div 60) Do
                                                               Begin
                                                                    minRainfallSeries[j,k,l] := x;
                                                                    Inc(j);
                                                               End;
                                                         End
                                           End
                                  End
                     End
                  Else If timestepRadar < 60 Then
                         Begin
                            nr := (length(TimeSeries)-1)Div(60 Div timestepRadar);
                            setlength(minTimeSeries, nr+1);
                            setlength(minRainfallSeries_array, length(TimeSeries));
                            setlength(minRainfallSeries_extrap_array, nr+1);
                            setlength(minRainfallSeries, nr+1);
                            InitializeRadar ( minRainfallSeries, nr+1, RadarRaindataset_src_single);
                            minTimeSeries[0] := TimeSeries[0];
                            For i := 1 To nr Do
                                Begin
                                     minTimeSeries[i] := minTimeSeries[i-1]+60;
                                End;
                            For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                                Begin
                                     For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                                         Begin
                                              For i := 0 To length(TimeSeries) - 1 Do
                                                  Begin
                                                       minRainfallSeries_array[i]:= RadarRaindataset_src[i,k,l];
                                                  End;

                                              minRainfallSeries_extrap_array := extrap(TimeSeries, minTimeSeries, minRainfallSeries_array);

                                              For i := 0 To nr Do
                                                  Begin
                                                       minRainfallSeries[i,k,l]:= minRainfallSeries_extrap_array[i];
                                                  End;
                                         End
                                End
                            End
                  Else
                      Begin
                           Setlength (minRainfallSeries, Length(RadarRaindataset_src));
                           Setlength (minTimeSeries, Length(TimeSeries));
                           InitializeRadar ( minRainfallSeries, Length(RadarRaindataset_src), RadarRaindataset_src_single);

                           minTimeSeries := TimeSeries;
                           For tel := 0 To Length(RadarRaindataset_src) - 1 Do
                               Begin
                                  For k := Low(RadarRaindataset_src[tel]) To High(RadarRaindataset_src[tel]) - 1 Do
                                      Begin
                                           For l:= Low(RadarRaindataset_src[tel,k]) To High(RadarRaindataset_src[tel,k]) - 1 Do
                                               Begin
                                                    minRainfallSeries[tel,k,l] := RadarRaindataset_src[tel,k,l];
                                               End
                                      End
                               End
                      End;
                  // vervolgens conversie naar tijdstap van 10 min
                  nr := ((length(minTimeSeries)-1) Div 10) + 1;
                  setlength(newTimeSeries, nr);
                  setlength(newRainfallSeries_array,length(minTimeSeries));
                  setlength(newRainfallSeries_extrap_array, nr);
                  setlength(newRainfallSeries, nr);
                  InitializeRadar ( newRainfallSeries, nr, RadarRaindataset_src_single);
                  newTimeSeries[0] := TimeSeries[0];
                  For i := 1 To nr Do
                      Begin
                           newTimeSeries[i] := newTimeSeries[i-1]+600;
                      End;
                  For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                                Begin
                                     For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                                         Begin
                                              For i := 0 To length(minTimeSeries)-1 Do
                                                  Begin
                                                       newRainfallSeries_array[i]:= minRainfallSeries[i,k,l];
                                                  End;

                                              newRainfallSeries_extrap_array := extrap(minTimeSeries, newTimeSeries, newRainfallSeries_array);

                                              For i := 0 To nr-1 Do
                                                  Begin
                                                       newRainfallSeries[i,k,l]:= newRainfallSeries_extrap_array[i];
                                                  End;
                                         End
                                End

                  End;
             // zoek geldig event en baken het af (tini...tfin)
             setlength(newRainfallSeries_while_array, length(newTimeSeries));
             For k := Low(RadarRaindataset_src[0]) To High(RadarRaindataset_src[0]) - 1 Do
                 Begin
                      For l:= Low(RadarRaindataset_src[0,k]) To High(RadarRaindataset_src[0,k]) - 1 Do
                          Begin
                               index := 0;
                               While index <= (length(newTimeSeries)-1) Do
                                     Begin
                                          If newRainfallSeries[index,k,l] <> 0 Then     // van zodra er regen valt...
                                             Begin
                                                For i := 0 To length(newTimeSeries)-1 Do
                                                    Begin
                                                         newRainfallSeries_while_array[i]:= newRainfallSeries[i,k,l];
                                                    End;
                                                Tini := index;
                                                While (sumPartArray(newRainfallSeries_while_array,index+1,35)<>0) And (index < length(newTimeSeries)-1) Do
                                                      inc(index);
                                                // einde van event wordt bereikt van zodra in volgende 6u geen regen valt of van zodra
                                                // we aan einde van rainfall file komen...

                                                Tfin := index;
                                                rainVolume := Copy(newRainfallSeries_while_array, Tini, Tfin-Tini+1);
                                                // test of totaal rainvolume > 1.27 mm (indien niet, is dit geen erosive event en
                                                // wordt dit event niet meegenomen bij berekening R factor)
                                                If sum(rainVolume) < 1.27 Then
                                                   Begin
                                                        index := index+1;
                                                        continue;
                                                   End;
                                                SetLength(rainEnergy, Tfin-Tini+1);
                                                eventEnergy := 0;
                                                For i := 0 To Tfin-Tini Do
                                                // bereken rain energy voor elke timestep binnen event
                                                   Begin
                                                        rainEnergy[i] := 11.12*power((rainVolume[i]/(1/6)),0.31);
                                                        // in J/m².mm
                                                        eventEnergy := eventEnergy+(rainEnergy[i]*rainVolume[i]);
                                                        // in J/m²
                                                   End;
                                                // bepaal max I30 van event
                                                If Tfin-Tini-1 < 1 Then
                                                   maxI30 := sum(rainVolume) * 2
                                                Else
                                                    Begin
                                                    SetLength(I30, Tfin-Tini-1);
                                                    For i := 0 To Tfin-Tini-2 Do
                                                        I30[i] := (rainVolume[i]+rainVolume[i+1]+rainVolume[i+2])*2;
                                                    // in mm/h

                                                    maxI30 := I30[0];
                                                    For i:=1 To Tfin-Tini-2 Do
                                                        Begin
                                                             If I30[i] > maxI30 Then
                                                                maxI30 := I30[i];
                                                        End;
                                                    End;
                                                // bereken R factor van event
                                                eventRFactor := (eventEnergy*maxI30)/1000000;
                                                // in MJ.mm/m².h
                                                // bereken totale R factor
                                                RFactorRadar[k,l] := RFactorRadar[k,l] + eventRFactor;
                                                // in MJ.mm/m².h
                                             End;
                                          // verhoog index met 1
                                          index := index +1;
                                     End;
                               End;
                      End;
         End;

//******************************************************************************
//This procedure calculates the amount of rainfall excess (=runoff) or rainfall
//deficit (= amount of water that can re-infiltrate in the grid cell)
//******************************************************************************
Procedure CalculateReRadar(Var Remap:Rraster; RainfallSum:RadarRaster; I10Radar:RadarRaster; Perceelskaart:Rraster; CNmap:Rraster; alpha, beta:double);

Var
  i,j, nrowPRC, ncolPRC : integer;
  Ia, S: double;

Begin
  nrowPRC := nrow;
  ncolPRC := ncol;
  SetLength(Remap,nrowPRC+1, ncolPRC+1);

  For i:= 1 To nrowPRC Do
    For j := 1 To ncolPRC Do
      Begin
        If (Perceelskaart[i,j]<>0) And (CNmap[i,j]<>0) Then
          Begin
            Remap[i,j] := CalculateRe_singlevalue(RainfallSum[i,j], CNMap[i,j], alpha,beta, I10Radar[i,j],AR5,duration);
          End
        Else Remap[i,j] := 0.0;

        If (Perceelskaart[i,j]=-1) Then
          Remap[i,j] := RainfallSum[i,j];

        //Buffer and dam pixels are always assigned a CN value of 71
        If (Include_buffer) And (BufferMap[i,j] <> 0) or (Include_dam) And (Dam_map[i,j] <> 0) Then
          Begin
            Remap[i,j] := CalculateRe_singlevalue(RainfallSum[i,j], 71, alpha,beta,I10Radar[i,j],AR5,duration);
          End;

        // ditches are assigned a CN value of 98
        If (Include_ditch) And (Ditch_map[i,j] <> 0) Then
          Begin
            Remap[i,j] := CalculateRe_singlevalue(RainfallSum[i,j], 98, alpha,beta,I10Radar[i,j],AR5,duration);
          End;


      End;
End;

//******************************************************************************
// In this procedure the time dependent runoff is calculated
//******************************************************************************
Procedure CalculateTimeDependentRunoffRadar(Var Remap: Rraster; Routing:
                                       TRoutingArray; PRC: Rraster);

Var
  RunoffMap, RunoffInputMap, RunoffInputMap_temp, RoutedMap_temp, RainfallMap,
  RunoffCummulMap, OutflowMap_temp: Rraster;
  i, j,  k , l, m, teller, targetX, targetY: integer;
  RunoffInput ,Part1_water, Part2_water, Speed, spill, sewer_out_water: double;
  Discharge, Discharge_tot, Discharge_VHA_txt, spillover_txt, sewer_out_txt: textfile;
  spillover, Discharge_result_tmp, Discharge_tmp_fin: FloatArray;
  Result, Discharge_VHA: FloatArray2;
  Timeseries_tmp: integerArray;

  Label SKIP;

Const
  speed_sewer = 1;
  // assume 1 m/s

Begin
  //Runoff map is created (=Remap in which negative values are replaced by 0, and
  //runoff in (m³)
  //It is checked if the amount of runoff produced in a certain grid cell is smaller
  //or equal to the amount of rainfall
  //Startmap is created (= Remap with positive values replaced by zeros when runoff
  //takes places (this is added for every time step) and negative values where
  //re-infiltration takes place)

  SetDynamicRData(RunoffInputmap);
  //= map with total amount of runoff per
  SetDynamicRData(RunoffInputmap_Temp);
  //= map with runoff per pixel for every timestep
  SetdynamicRData(OutflowMap_temp);
  //= map with the removal of water (via routing) from every grid cell for every time step
  SetLength(Result,Numberoftimesteps+1,numOutlet+1);
  If Include_buffer Then
    Begin
      SetLength(spillover,number_of_buffers+1);
      //= data array containing the amount of spillover per buffer
      For i := 1 To number_of_buffers Do
        spillover[i] := 0;
    End;

  If Include_sewer Then
    sewer_out_water := 0;

  If VHA Then
    Begin
      numRivSeg := calcRivSeg(RivSeg);
      Setlength(Discharge_VHA, Numberoftimesteps+1, numRivSeg+1);
    End;
  SetDynamicRData(RainfallMap);
  SetDynamicRData(Runoffmap);
  //=map in which the amount of runoff per timestep is stored

  For i := 1 To nrow Do
    For j := 1 To ncol Do
      Begin
        If PRC[i,j] = 0 Then continue;
        // all cells outside catchment are skipped
        If Remap[i,j] < 0.0 Then
          Begin
            RunoffInputmap[i,j] := 0.0;
            Runoffmap[i,j] := (Remap[i,j]/1000)*sqr(res);
            // amount of water that still can infiltrate in m^3
          End
        Else
          Begin
            RunoffInputmap[i,j] := (Remap[i,j]/1000)*sqr(res);
            //runoff in m^3
            Runoffmap[i,j] := 0.0;
          End;
        RainfallMap[i,j] := (RainfallSum[i,j]/1000)*sqr(res);
        If RunoffInputmap[i,j] > RainfallMap[i,j] Then
          //Total runoff for a pixel can never be larger than total rainfall for that pixel
          Begin
            RunoffInputmap[i,j] := RainfallMap[i,j];
          End;
      End;

  //If buffers are included the maximum discharge and dead volume for every buffer are calculated
  If (Include_buffer) Then
    For i := 1 To Number_of_Buffers Do
      Begin
        BufferData[i].Qmax := BufferData[i].Cd * BufferData[i].Opening_area * sqrt(2*9.91*(
                              BufferData[i].height_dam - BufferData[i].Height_opening));
        BufferData[i].Volume_dead := (BufferData[i].Height_opening / BufferData[i].Height_dam) *
                                     BufferData[i].Volume;
        BufferData[i].area := BufferData[i].Volume/BufferData[i].Height_dam;
      End;

  //For every timestep the correct amount of runoff is added to RunoffMap and
  //is routed through the landscape
  SetDynamicRData(RoutedMap_temp);
  //Amount of transferred water during a particular time step
  SetDynamicRData(RunoffCummulMap);

//Total amount of water that reaches every grid cell (both as a result of direct rainfall input and as a result of routed runoff from upland gidcells)
  SetzeroR(RunoffCummulMap);

  For i := 0 To NumberOfTimeSteps Do
    //Every timestep is looked at
    Begin
      SetzeroR(RoutedMap_temp);
      //At he beginning of every timestep this map is set to zero
      SetzeroR(OutflowMap_temp);

      //De input per cel wordt bepaald voor de tijdstap
      For k := 1 To nrow Do
        For l := 1 To ncol Do
          //The input for every cell is determined
          Begin
            If PRC[k,l] = 0 Then continue;
            // all cells outside catchment are skipped
            RunoffInput := RunoffInputmap[k,l] * Rain_fraction_radar[i,k,l];
            //The runoff for that timestep is determined by scaling the total runoff
            //for that grid cell with the fraction of rainfall that falls during this time step
            RunoffInputmap_Temp[k,l] := RunoffInput;
            RunoffMap[k,l] := RunoffMap[k,l] + RunoffInput;
            //The amount of runoff for this time step is added to the amount of runoff
            //that was already present at the beginning of the time step

            If is_outlet(k,l) Then
              Begin
                If outlet_Select Then
                  Result[i,Outlet[k,l]] := Result[i,Outlet[k,l]]+RunoffInput
                Else
                  Result[i,1] := Result[i,1]+RunoffInput;
              End;
            If (VHA) And (RivSeg[k,l] <> 0) Then
              Discharge_VHA[i,RivSeg[k,l]] := Discharge_VHA[i,RivSeg[k,l]] + RunoffInput;
          End;

      //The runoff is routed for this time step
      For teller:=nrow*ncol Downto 1 Do
        Begin
          k := row[teller];
          //row and col are vectors containing the row and column ID's from low to high
          l := column[teller];
          If PRC[k,l] = 0 Then continue;
          If RunoffMap[k,l] <= 0.0 Then continue;
          If PRC[k,l] = -1 Then
            speed := riv_vel
          Else
            Speed := 0.3;
          //Water velocity in m/s (based on Govers 1992)

          //If buffers are present:
          If (Include_buffer) And (Buffermap[k,l] <> 0) And (Buffermap[k,l] <= Number_of_Buffers)
             And (RunoffMap[k,l]>0) Then
     // discharge from buffer is only altered at buffer center (place where the opening is situated)
            Begin
              If RunoffMap[k,l] > BufferData[Buffermap[k,l]].Volume Then
//The maximum capacity of the buffer is reached: all the runoff that enters the buffer flows over it
                Begin
                  spill := BufferData[BufferMap[k,l]].Cd*
                           BufferData[BufferMap[k,l]].width_dam*
                           sqrt(9.81)*
                           Power(((RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume)/BufferData[BufferMap[k,l]].area),(3/2))* Timestep_model;

      // calculate maximum amount of water flowing over dam (formula from project Pieter Meert p.68)
                  If spill > RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume Then
             // water flowing over dam cannot exceed amount of water that does not fit in the buffer
                    spill := RunoffMap[k,l]-BufferData[BufferMap[k,l]].Volume;
                  Part1_Water := (BufferData[Buffermap[k,l]].Qmax * Timestep_model) + spill;

//Part1_water is composed of (1) the volume of water inside the buffer that flows through the opening and (2) the additional water (>volume of the buffer) that flows over the dam
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
                                                                                     RoutedMap_temp[
                                                                                     Routing[k,l].
                                                                                     Target1row,
                                                                                     Routing[k,l].
                                                                                     Target1col] +
                                                                                     Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                  //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                  spillover[Buffermap[k,l]] := spillover[BufferMap[k,l]]+spill;
                End

              Else
                If RunoffMap[k,l] > BufferData[Buffermap[k,l]].Volume_dead Then
           //The volume of water in the buffer is larger than the dead volume: part of it will drain
                  Begin
                    Part1_water := (BufferData[Buffermap[k,l]].Qmax * sqrt(RunoffMap[k,l]/(
                                   BufferData[Buffermap[k,l]].Volume - BufferData[Buffermap[k,l]].
                                   Volume_dead))) * Timestep_model;
                    // Amount of discharge = [Qmax * sqrt(vol(t)/vol(max))] * timestep
                    RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=

                                                                                      RoutedMap_temp
                                                                                       [Routing[k,l]
                                                                                       .Target1row,
                                                                                       Routing[k,l].
                                                                                       Target1col] +
                                                                                       Part1_water;
                    RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                    //RunoffMap is updated
                    OutflowMap_temp[k,l] := Part1_water;
                  End

              Else
          //The volume of water in the buffer is smaller than the dead volume: nothing will flow out
                Begin
                  Part1_water := 0;
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
                                                                                     RoutedMap_temp[
                                                                                     Routing[k,l].
                                                                                     Target1row,
                                                                                     Routing[k,l].
                                                                                     Target1col] +
                                                                                     Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                  //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                End;

              targetX := Routing[k,l].Target1row;
              targetY := Routing[k,l].Target1col;
              If is_outlet(targetX,targetY) Then
                // if target cell is outlet, the result is updated
                Begin
                  If outlet_Select Then
                    Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                         OutflowMap_temp[k,l]
                  Else
                    Result[i,1] := Result[i,1]+OutflowMap_temp[k,l];
                End;
              If VHA Then
                Begin
                  If RivSeg[targetX,targetY]<>0 Then
                    // if buffer drains directly into river, the result is updated
                    Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                targetY]]+OutflowMap_temp[k,l];
                End;
              Goto SKIP;
              //Proceed to next cell
            End;

          If Routing[k,l].Part1 > 0 Then
            Begin
              If Routing[k,l].Part2 > 0 Then //If the water is routed towards 2 lower cells
                Begin
                  If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                    // if cell contains entrance to sewer...
                    Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed_sewer*TimeStep_model)
                                   /Routing[k,l].Distance2)
                                   //Amount of water that is transfered through sewers
                  Else
                    Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed*TimeStep_model)/
                                   Routing[k,l].Distance2);
                  //Amount of water that is transfered to neighbor 2

                  Part1_water := (RunoffMap[k,l]*Routing[k,l].Part1)*((Speed*TimeStep_model)/Routing
                                 [k,l].Distance1);
                  //Amount of water that is transfered to neighbor 1
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
                                                                                     RoutedMap_temp[
                                                                                     Routing[k,l].
                                                                                     Target1row,
                                                                                     Routing[k,l].
                                                                                     Target1col] +
                                                                                     Part1_water;
                  If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                    Begin
                      RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] :=

                                                                                      RoutedMap_temp
                                                                                         [Routing[k,
                                                                                         l].
                                                                                         Target2row,
                                                                                         Routing[k,l
                                                                                         ].
                                                                                         Target2col]
                                                                                         + (
                                                                                         Part2_water
                                                                                         * (1-(
                                                                                         sewer_exit/
                                                                                         100)));
                      sewer_out_water := sewer_out_water + (Part2_water * (sewer_exit/100));
                    End
                  Else
                    RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] :=

                                                                                      RoutedMap_temp
                                                                                       [Routing[k,l]
                                                                                       .Target2row,
                                                                                       Routing[k,l].
                                                                                       Target2col] +
                                                                                       Part2_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - (Part1_water + Part2_water);

//The total amount of runoff that leaves the grid cell during this time step is subtracted from the RunoffMap
                  OutflowMap_temp[k,l] := Part1_water + Part2_water;
                  //Amount of water leaving the grid cell during this time step
                  If RunoffMap[k,l] < 0 Then RunoffMap[k,l] := 0;
                  //No more water then is present can leave a cell


                  targetX := Routing[k,l].Target1row;
                  targetY := Routing[k,l].Target1col;
                  If is_outlet(targetX,targetY) Then
                    // if target cell 1 is outlet or river, the result is updated
                    Begin
                      If outlet_Select Then
                        Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                             Part1_water
                      Else
                        Result[i,1] := Result[i,1]+Part1_water;
                    End;
                  If VHA Then
                    Begin
                      If RivSeg[targetX,targetY]<>0 Then
                        Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                    targetY]]+Part1_water;
                    End;

                  targetX := Routing[k,l].Target2row;
                  targetY := Routing[k,l].Target2col;
                  If is_outlet(targetX,targetY) Then
                    // if target cell 2 is outlet or river, the result is updated
                    Begin
                      If outlet_Select Then
                        Begin
                          If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                            Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+(
                                                                 Part2_water*(1-(sewer_exit/100)))
                          Else
                            Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                                 Part2_water;
                        End
                      Else
                        Begin
                          If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                            Result[i,1] := Result[i,1]+(Part2_water*(1-(sewer_exit/100)))
                          Else
                            Result[i,1] := Result[i,1]+Part2_water;
                        End;
                    End;
                  If VHA Then
                    Begin
                      If RivSeg[targetX,targetY]<>0 Then
                        Begin
                          If (Include_sewer) And (SewerMap[k,l] <> 0) Then
                            Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[
                                                                        targetX,targetY]]+(
                                                                        Part2_water*(1-(sewer_exit/
                                                                        100)))
                          Else
                            Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[
                                                                        targetX,targetY]]+
                                                                        Part2_water;
                        End;
                    End;
                End
              Else
                Begin
                  //If the runoff is transfered to only 1 neighbor (part 1 = 1)
                  Part1_water := (RunoffMap[k,l]*Routing[k,l].Part1)*((Speed*TimeStep_model)/Routing
                                 [k,l].Distance1);
                  RoutedMap_temp[Routing[k,l].Target1row,Routing[k,l].Target1col] :=
                     RoutedMap_temp[Routing[k,l].Target1row, Routing[k,l].Target1col] +  Part1_water;
                  RunoffMap[k,l] := RunoffMap[k,l] - Part1_water;
                  //RunoffMap is updated
                  OutflowMap_temp[k,l] := Part1_water;
                  If RunoffMap[k,l] < 0 Then RunoffMap[k,l] := 0;

                  targetX := Routing[k,l].Target1row;
                  targetY := Routing[k,l].Target1col;
                  If is_outlet(targetX,targetY) Then
                    // if target cell 1 is outlet or river, the result is updated
                    Begin
                      If outlet_Select Then
                        Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                             Part1_water
                      Else
                        Result[i,1] := Result[i,1]+Part1_water;
                    End;
                  If VHA Then
                    Begin
                      If (RivSeg[targetX,targetY]<>0) And (RivSeg[k,l] = 0) Then
        // only if target cell is river AND source cell is no river, discharge_VHA should be updated
                        Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                    targetY]]+Part1_water;
                    End;
                End;

            End
          Else
            If Routing[k,l].Part2 > 0 Then
              Begin
                //If the runoff is transfered to only 1 neighbor (part 2 = 1)
                Part2_water := (RunoffMap[k,l]*Routing[k,l].Part2)*((Speed*TimeStep_model)/Routing[k
                               ,l].Distance2);
                RoutedMap_temp[Routing[k,l].Target2row,Routing[k,l].Target2col] :=
                                                                                   RoutedMap_temp[
                                                                                   Routing[k,l].
                                                                                   Target2row,
                                                                                   Routing[k,l].
                                                                                   Target2col] +
                                                                                   Part2_water;
                RunoffMap[k,l] := RunoffMap[k,l] - Part2_water;
                OutflowMap_temp[k,l] := Part2_water;
                If RunoffMap[k,l] < 0 Then RunoffMap[k,l] := 0;

                targetX := Routing[k,l].Target2row;
                targetY := Routing[k,l].Target2col;
                // if target cell 1 is outlet or river, the result is updated
                If is_outlet(targetX,targetY) Then
                  Begin
                    If outlet_Select Then
                      Result[i,Outlet[targetX,targetY]] := Result[i,Outlet[targetX,targetY]]+
                                                           Part2_water
                    Else
                      Result[i,1] := Result[i,1]+Part2_water;
                  End;
                If VHA Then
                  Begin
                    If RivSeg[targetX,targetY]<>0 Then
                      Discharge_VHA[i,RivSeg[targetX,targetY]] := Discharge_VHA[i,RivSeg[targetX,
                                                                  targetY]]+Part2_water;
                  End;

              End;
          SKIP:
        End;

      //At the end of every time step the amount of displaced water is added to the runoff map

// + the amount of displaced water is stored for every grid cell to calculate a cummulative runoff map for the entire event
      For k := 1 To nrow Do
        For l := 1 To ncol Do
          // runoff cumul map = amount of water entering each cell
          Begin
            If PRC[k,l] = 0 Then continue;
            RunoffMap[k,l] := RunoffMap[k,l] + RoutedMap_temp[k,l];
            //RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + RoutedMap_temp[k,l];
            If RunoffMap[k,l] >= 0 Then

//RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + ((RunoffMap[k,l]*Speed*Timestep)/res) //Geeft totale hoeveelheid water (rekening houdend met snelheid en tijdstap)
              RunoffCummulMap[k,l] := RunoffCummulMap[k,l] + RoutedMap_temp[k,l] +
                                      RunoffInputmap_Temp[k,l];
            //else RunoffCummulMap[k,l] := 0;
          End;
    End;


  // Runoff results are converted to a different timestep if requested by the user

  If convert_output Then
    Begin
      setlength(TimeSeries_tmp,Numberoftimesteps+1);
      For i := 0 To Numberoftimesteps Do
        TimeSeries_tmp[i] := TimeSeries_output[i];
      setlength(discharge_result_tmp, Numberoftimesteps+1);

      NTimesteps2 := Numberoftimesteps Div ((Timestep_output*60) Div Timestep_model);
      setlength(Result_Discharge, Ntimesteps2+1,numOutlet+1);
      setlength(TimeSeries_tmp_fin, NTimesteps2+1);
      TimeSeries_tmp_fin[0] := TimeSeries_tmp[0];
      For i:=1 To length(TimeSeries_tmp_fin)-1 Do
        TimeSeries_tmp_fin[i] := TimeSeries_tmp_fin[i-1]+(Timestep_output*60);
      setlength(Discharge_tmp_fin, NTimesteps2+1);

      For j := 1 To numOutlet Do
        Begin
          For i:=0 To Numberoftimesteps Do
            Discharge_result_tmp[i] := Result[i,j];
          Discharge_tmp_fin := extrap(TimeSeries_tmp,TimeSeries_tmp_fin,Discharge_result_tmp);
          For i:=0 To NTimesteps2 Do
            Result_Discharge[i,j] := Discharge_tmp_fin[i];
        End;

      If VHA Then
        Begin
          setlength(Result_Discharge_VHA, Ntimesteps2+1, numRivSeg+1);
          For j := 1 To numRivSeg Do
            Begin
              For i:=0 To Numberoftimesteps Do
                Discharge_result_tmp[i] := Discharge_VHA[i,j];
              Discharge_tmp_fin := extrap(TimeSeries_tmp,TimeSeries_tmp_fin,Discharge_result_tmp);
              For i :=0 To NTimesteps2 Do
                Result_Discharge_VHA[i,j] := Discharge_tmp_fin[i];
            End;
        End;

    End
  Else
    Begin
      setlength(Result_Discharge, numberoftimesteps+1, numOutlet+1);
      Result_Discharge := Result;

      If VHA Then
        Begin
          setlength(Result_Discharge_VHA, numberoftimesteps+1, numRivSeg+1);
          Result_Discharge_VHA := Discharge_VHA;
        End;
    End;


  //The amount of runoff that leaves the catchment during every time step is written to a .txt file
  setcurrentDir(File_output_dir);
  assignfile(Discharge,'Discharge.txt');
  rewrite(Discharge);
  Writeln(Discharge, 'Discharge at each outlet [m³/s]');
  // write title

  If convert_output Then
    Write(Discharge, 'Time (min)',chr(9))
  Else
    Write(Discharge, 'Time (sec)',chr(9));

  For m := 1 To numOutlet Do
    write(Discharge, 'Outlet ', m, chr(9));
  // write column headings
  writeln(Discharge,'');
  // go to next line

  setlength(Sum_discharge,numOutlet+1);
  For m := 1 To numOutlet Do
    Sum_discharge[m] := 0;

  If convert_output Then
    Begin
      For i := 0 To NTimesteps2 Do
        Begin
          Write(Discharge, inttostr(Timeseries_tmp_fin[i] Div 60), chr(9));
          For m := 1 To numOutlet Do
            write (Discharge, floattostr(Result_Discharge[i,m]/(Timestep_output*60)), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(Discharge, '');
          For m := 1 To numOutlet Do
            Sum_discharge[m] := Sum_discharge[m] + Result_Discharge[i,m];

//Total amount of water leaving the catchment (this is dependent on the water velocity and length of the time step!)
        End;
    End
  Else
    Begin
      For i := 0 To Numberoftimesteps Do
        Begin
          Write(Discharge, inttostr(TimeSeries_Output[i]), chr(9));
          For m := 1 To numOutlet Do
            write (Discharge, floattostr(Result_Discharge[i,m]/Timestep_model), chr(9));
          //Amount of discharge per time step is written to the .txt file
          writeln(Discharge, '');
          For m := 1 To numOutlet Do
            Sum_discharge[m] := Sum_discharge[m] + Result_Discharge[i,m];

//Total amount of water leaving the catchment (this is dependent on the water velocity and length of the time step!)
        End;
    End;
  closefile(Discharge);
  //The memory of Discharge is released


  // The amount of water entering each VHA river segment is written to a .txt file

  If VHA Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(Discharge_VHA_txt,'Discharge_VHA.txt');
      rewrite(Discharge_VHA_txt);
      Writeln(Discharge_VHA_txt, 'Discharge to each river segment [m³/s]');
      // write title
      If convert_output Then
        Write(Discharge_VHA_txt, 'Time (min)',chr(9))
      Else
        Write(Discharge_VHA_txt, 'Time (sec)',chr(9));
      For m := 1 To numRivSeg Do
        write(Discharge_VHA_txt, 'VHA segment ', m, chr(9));
      // write column headings
      writeln(Discharge_VHA_txt,'');
      // go to next line

      setlength(Sum_discharge_VHA,numRivSeg+1);
      For m := 1 To numRivSeg Do
        Sum_discharge_VHA[m] := 0;

      If convert_output Then
        Begin
          For i := 0 To NTimesteps2 Do
            Begin
              Write(Discharge_VHA_txt, inttostr(Timeseries_tmp_fin[i] Div 60), chr(9));
              For m := 1 To numRivSeg Do
                write (Discharge_VHA_txt, floattostr(Result_Discharge_VHA[i,m]/(Timestep_output*60))
                , chr(9));
              //Amount of discharge per time step is written to the .txt file
              writeln(Discharge_VHA_txt, '');
              For m := 1 To numRivSeg Do
                Sum_discharge_VHA[m] := Sum_discharge_VHA[m] + Result_Discharge_VHA[i,m];
              // total amount of water entering each river segment
            End;
        End
      Else
        Begin
          For i := 0 To Numberoftimesteps Do
            Begin
              Write(Discharge_VHA_txt, inttostr(TimeSeries_output[i]), chr(9));
              For m := 1 To numRivSeg Do
                write (Discharge_VHA_txt, floattostr(Result_Discharge_VHA[i,m]/Timestep_model), chr(
                                                                                                   9
                ));
              //Amount of discharge per time step is written to the .txt file
              writeln(Discharge_VHA_txt, '');
              For m := 1 To numRivSeg Do
                Sum_discharge_VHA[m] := Sum_discharge_VHA[m] + Result_Discharge_VHA[i,m];
              // total amount of water entering each river segment
            End;
        End;
      closefile(Discharge_VHA_txt);
    End;



  //A map with the total amount of runoff for every grid cell for the entire event is created

//(= amount of runoff from precipitation + amount of routed runoff that arrives in a cell - amount of runoff that leaves the cel via routing)
  SetDynamicRData(RunoffTotMap);
  For k := 1 To nrow Do
    For l := 1 To ncol Do
      Begin
        If PRC[k,l] = 0 Then RunoffTotMap[k,l] := 0.0
        Else
          RunoffTotMap[k,l] := RunoffCummulMap[k,l];
      End;

{for m := 1 to numOutlet do
  Showmessage('The total discharge at outlet nr. ' + inttostr(m) + ' amounts to ' + floattostr(sum_discharge[m]) + ' m³.');
 }
  //The total amount of runoff that passes through each outlet is written to a .txt file
  setcurrentDir(File_output_dir);
  assignfile(Discharge_tot,'Total discharge.txt');
  rewrite(Discharge_tot);
  Writeln(Discharge_tot, 'Total discharge at each outlet [m³]');
  // write title
  Write(Discharge_tot, 'Outlet ID',chr(9),'Discharge');
  // write column headings
  writeln(Discharge_tot,'');
  // go to next line

  For i := 1 To numOutlet Do
    Begin
      Write(Discharge_tot, inttostr(i), chr(9), floattostr(sum_discharge[i]));
      writeln(Discharge_tot, '');
    End;
  closefile(Discharge_tot);
  //The memory of Discharge is released

  //The total amount of spillover (m³) for each buffer is written to a .txt file
  If Include_buffer Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(spillover_txt,'Spillover per buffer.txt');
      rewrite(spillover_txt);
      Writeln(spillover_txt, 'Amount of water flowing over dam for each buffer [m³]');
      // write title
      Write(spillover_txt, 'Buffer ID',chr(9),'Spillover');
      // write column headings
      writeln(spillover_txt,'');
      // go to next line

      For i := 1 To number_of_buffers Do
        Begin
          Write(spillover_txt, inttostr(i), chr(9), floattostr(spillover[i]));
          writeln(spillover_txt, '');
        End;
      closefile(spillover_txt);
      //The memory of spillover_txt is released
    End;


  // the total amount of water leaving the system through the sewers is written to a .txt file

  If Include_sewer Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(sewer_out_txt,'Sewer output water.txt');
      rewrite(sewer_out_txt);
      Writeln(sewer_out_txt,
              'Amount of water flowing out of the system through the sewer network [m³]');
      // write title
      Writeln(sewer_out_txt, floattostr(sewer_out_water));
      closefile(sewer_out_txt);
    End;


  //The memory of the created maps is released
  DisposedynamicRData(RunoffInputmap);
  DisposedynamicRData(RunoffInputmap_Temp);
  DisposedynamicRData(OutflowMap_temp);
  DisposedynamicRData(RainfallMap);
  DisposedynamicRData(Runoffmap);

End;

Procedure CheckerosionheightRadar(i, j: integer; Var A: RRaster);

Var
  extremum, area: double;
  k, l: integer;
Begin
  If Raster_projection = plane Then
    area := sqr(RES)
  Else
    area := X_Resolution() * Y_Resolution();
  // A = watereros value
  If A[i, j] < 0.0 Then             //erosion: watereros < 0
    Begin
      extremum := 99999.9;
      For k := -1 To 1 Do
        //search for lowest neighbour
        For l := -1 To 1 Do
          Begin
            If (k = 0) And (l = 0) Then
              continue;
            If DTM[i + k, j + l] < extremum Then
              extremum := DTM[i + k, j + l];
          End;
      If extremum > (DTM[i, j] + A[i, j]) Then
        Begin
          A[i, j] := extremum - DTM[i, j];
          SEDI_OUT[i, j] := SEDI_IN[i, j] - (A[i, j] * area);
        End;
    End
  Else                           //sedimentation
    Begin
      If A[i, j] > 0.0 Then
        Begin
          extremum := 99999990.0;
          For k := -1 To 1 Do
            For l := -1 To 1 Do
              Begin
                If ((k = 0) And (l = 0)) Or (abs(k) + abs(l) < 2) Then
                  continue;
                If DTM[i, j] > extremum Then
                  extremum := DTM[i + k, j + l];
              End;
          If extremum < (DTM[i, j] + A[i, j]) Then
            Begin
              A[i, j] := extremum - DTM[i, j];
              SEDI_OUT[i, j] := SEDI_IN[i, j] - (A[i, j] * area);
            End;
        End;
    End;
End;
//-----------------------------------------Einde procedure Checkerosionheight


Procedure CalculatewatereroRadar(i, j: integer);
//waterero in meter: for the pixel that is being considered

Var                                         //rill,interrill en cap in m≥
  capacity, area, Distcorr, Ero_Potential: double;

Begin

  If (Include_dam) And (Dam_map[i, j] <> 0) Then
    // adjust C factor, P factor and kTc for dams
    Begin
      C_factor[i, j] := 0.01;
      P_factor[i, j] := 1;
      ktc[i,j] := 7;
    End;

  If (Include_ditch) And (Ditch_map[i, j] <> 0) Then
    // adjust C factor, P factor and kTc for ditches
    Begin
      C_factor[i, j] := 0;
      P_factor[i, j] := 1;
      ktc[i,j] := 9999;
    End;

  //The next lines of code are not used in WS, so they are commented out:

// in oude versie W/S model werd een buffer rondom een rivier gecreëerd met een zeer hoge KTc => hier ook nodig?

{for k := -1 to 1 do
    for l := -1 to 1 do
    begin
      if (k = 0) and (l = 0) then
        continue; // Skip the cell under consideration
      if PRC[i + k, j + l] = -1 then // If a cell around the cell under consideration is a river
        kTc[i, j] := 9999;
      // The ktc value for the cell next to the river is set to 9999
    end;}

  Waterero := 0.0;
  If Raster_projection = plane Then
    Begin
      area := sqr(RES);
      Distcorr := (RES * (ABS(sin(aspect[i, j])) + ABS(cos(aspect[i, j]))));
      // Correction factor for conversion to grid cell dimension
    End
  Else
    Begin
      area := X_resolution() * Y_resolution();
      Distcorr := (Y_Resolution() * ABS(sin(aspect[i, j])) + X_Resolution()
      	* ABS(cos(aspect[i, j])));
      // CHECK !!!
    End;

  // Erosion equations
  RUSLE[i, j] := RFactorRadar[i,j] * C_factor[i, j] * P_factor[i, j] * K_Factor[i, j] * LS[i, j];
  // kg/(m² * yr) (ok)
  // Erosion in kg/m²

  // Capacity := ktc[i,j] * RUSLE[i, j] * distcorr;   // in kg (ok) KTC ZOALS IN VERSIE JEROEN
  Capacity := ktc[i,j] * RFactorRadar[i,j] * K_Factor[i, j] * (LS[i, j] - 0.6 * 6.86 * power(tan(slope[i,j]),
              0.8));
  // [kg/m] VOLGENS CODES BASTIAAN;


  // Verwijderd want niet aanwezig in WatemSedem2015

{
  if (C_factor[i, j] = 0) then                     // correction for roads and ditches
    //Capacity := RFactorRadar[i,j] * K_Factor[i, j] * LS[i, j] * ktc[i, j] * distcorr;
    Capacity := ktc[i, j] * RFactorRadar[i,j] * K_Factor[i, j] * (LS[i, j] - 0.6 * 6.86 * power(tan(slope[i,j]), 0.8)); // [kg/m] VOLGENS CODES BASTIAAN;
  }
  //*******************************

  Ero_Potential := RUSLE[i, j] * area / BD;
  // in m³ (ok) CODE JEROEN
  If capacity < 0.0 Then capacity :=  ktc[i,j] *  RUSLE[i,j];

  capacity := capacity*distcorr;
  // [kg] VOLGENS CODES BASTIAAN
  capacity := capacity / BD;
  // in m³ (ok)

  If (SEDI_IN[i, j] + Ero_Potential) > capacity Then
    // If the amount of soil that enters the cell is
    // larger than the transport capacity
    Begin
      SEDI_OUT[i, j] := capacity;
      // SEDI_OUT [m³]
      // The amount leaving the cell = transport capacity
      Waterero := (SEDI_IN[i, j] - capacity) / area;
      // in m (ok)
    End
  Else
    Begin
      SEDI_OUT[i, j] := SEDI_IN[i, j] + Ero_Potential;
      // SEDI_OUT [m³]
      Waterero := -(Ero_Potential) / area;
      // in m (ok)
    End;
  WATEREROS[i, j] := Waterero;
  // in m (ok)
  // in m      if < 0 => erosion & if > 0 => sedimentation
End;

Procedure WaterRadar;

Var
  teller, i, j, k, l, m, n: integer;
  area, sewer_out_sed, TEMP_river_sed_input, TEMP_outside_sed_input, TEMP_buffer_sed_input,
  TEMP_pond_sed_input: double;
  sediment, sediment_VHA, sewer_out: textfile;

Begin
  // Create temp 2D maps
  SetDynamicRData(SEDI_IN);
  // Raster with sediment input per gridcel?
  SetDynamicRData(SEDI_OUT);
  //************************

  SetzeroR(SEDI_IN);
  SetzeroR(SEDI_OUT);
  SetzeroR(SEDI_EXPORT);

  // SEDI_EXPORT is defined in 'Readinparameters.pas' (in allocate_memory)


  If VHA Then //If the user wants output per river segment
    Begin
      numRivSeg := calcRivSeg(RivSeg);
      setlength(sedload_VHA, numRivSeg + 1);
      //The length of a vector per river segment (+1) is set
    End;

  If Include_sewer Then // If sewers are included
    sewer_out_sed := 0;

  TEMP_river_sed_input := 0;
  TEMP_outside_sed_input := 0;
  TEMP_buffer_sed_input := 0;
  TEMP_pond_sed_input := 0;

  //** Calculate watererosion & Lateral sediment
  For teller := ncol * nrow Downto 1 Do
    Begin
      // begin lus
      i := row[teller];
      j := column[teller];
      // The catchment is looked at starting from the highest pixel
      If (PRC[i, j] = 0) Or (PRC[i, j] = -1) Or (PRC[i, j] = -5) Then
        // if cell is outside area or a river cell or a water body => = all export cells

//    This means that also cells outside the study area and ponds are included in the calculation of sediment leaving the catchment?
        Begin
          If (PRC[i, j] = -1) Then
            TEMP_river_sed_input := TEMP_river_sed_input + SEDI_IN[i, j];
          If (PRC[i, j] = 0) Then
            TEMP_outside_sed_input := TEMP_outside_sed_input + SEDI_IN[i, j];
          If (PRC[i, j] = -5) Then
            TEMP_pond_sed_input := TEMP_pond_sed_input + SEDI_IN[i, j];

          SEDI_EXPORT[i, j] := SEDI_IN[i, j];
          // assign export sediment (in m³) value for export cells
          If (VHA) And (RivSeg[i, j] <> 0) Then
            sedload_VHA[RivSeg[i, j]] := sedload_VHA[RivSeg[i, j]] + SEDI_EXPORT[i, j];
          // totale hoeveelheid sediment per rivier segment wordt opgeslagen
          continue;
        End;

      If (Include_buffer) And (Buffermap[i, j] <> 0) And
         (Buffermap[i, j] <= Number_of_Buffers) Then    // only center cells of buffers
        Begin
          If Raster_projection = plane Then
            area := sqr(RES)
          Else
            area := X_resolution() * Y_resolution();
          SEDI_OUT[i, j] := SEDI_IN[i, j] * (1 - (Bufferdata[Buffermap[i, j]].PTEF / 100));
          If SEDI_OUT[i,j]<SEDI_IN[i,j] Then
            TEMP_buffer_sed_input := TEMP_buffer_sed_input + (SEDI_IN[i,j]-SEDI_OUT[i, j]);
          DistributeFlux_Sediment(i, j, SEDI_IN, SEDI_OUT);
          WATEREROS[i, j] := (SEDI_IN[i, j] - SEDI_OUT[i, j]) / area;
          // in m
          WATEREROS[i, j] := WATEREROS[i, j] * 1000;
          // in mm
          continue;
        End;

      CalculateWaterEroRadar(i, j);
      //CheckerosionheightRadar(i,j,WATEREROS);
      WATEREROS[i, j] := WATEREROS[i, j] * 1000;
      // [mm] (ok)
      //ShowMessage('Watereros:' + floattostr(WATEREROS[i,j]));
      area := sqr(RES);
      WATEREROS_cubmeter[i,j] := WATEREROS[i, j] * Area / 1000;
      WATEREROS_kg[i,j] := WATEREROS_cubmeter[i,j] * BD;


      If (PRC[i, j] <> 0) And (PRC[i, j] <> -1) And (PRC[i, j] <> -5) Then
        Begin
          If SEDI_IN[i, j] - SEDI_OUT[i, j] < 0 Then
            sedprod := sedprod + ((SEDI_IN[i, j] - SEDI_OUT[i, j]) * BD) //BD [kg/m³]
                       // sedprod [kg]
          Else
            depprod := depprod + ((SEDI_IN[i, j] - SEDI_OUT[i, j]) * BD);
          // depprod [kg]
        End;

      If SEDI_OUT[i,j] > 0 Then
        // if sediment leaves this pixel, the sediment needs to be distributed over target cells
        DistributeFlux_Sediment(i, j, SEDI_IN, SEDI_OUT);
      // SEDI_IN [m³]

      If (Include_sewer) And (SewerMap[i, j] <> 0) Then

    // if pixel contains sewer, total amount of sediment leaving the system through sewer is updated
        Begin

        // AND SEDI_IN is corrected because procedure Distribute_Flux doesn't take this into account
          sewer_out_sed := sewer_out_sed + (SEDI_OUT[i, j] * SewerMap[i, j] * (sewer_exit / 100));
          SEDI_IN[Routing[i, j].Target2Row, Routing[i, j].Target2Col] :=
                                                                         SEDI_IN[Routing[i, j].
                                                                         Target2Row, Routing[i, j].
                                                                         Target2Col] -
                                                                         SEDI_OUT[i, j] * Routing[i,
                                                                         j].Part2 + SEDI_OUT[i, j] *
                                                                         SewerMap[i, j] * (1 - (
                                                                         sewer_exit / 100));
        End;
    End;
  //***********

  // voor elke outletpixel wordt nu de totale sedimentvracht berekend die binnenkomt
  // voor rivierpixel = som van alle sediment dat terecht komt in alle hogergelegen rivierpixels
  // voor pixel op land = SEDI_IN voor die pixel

  setlength(sedload, numOutlet + 1);
  For i := 1 To numOutlet Do
    Begin
      k := OutletArray[i, 0];
      //row of the outlet
      l := OutletArray[i, 1];
      // column of the outlet
      If PRC[k, l] = -1 Then // If the outlet is a river
        Begin
          For m := 1 To nrow Do
            For n := 1 To ncol Do
              Begin
                If (PRC[m, n] = -1) And (DTM[m, n] >= DTM[k, l]) Then
                  sedload[i] := sedload[i] + SEDI_EXPORT[m, n];
                // Here SEDI_EXPORT [m³]
                //      sedload [m³]
              End;
        End
      Else
        sedload[i] := SEDI_IN[k, l];
      // [m³]

      sedload[i] := sedload[i] * BD;
      // convert sedload to kg
    End;

  // voor elk riviersegment wordt sediment omgezet naar kg
  If VHA Then
    Begin
      For i := 1 To numRivSeg Do
        sedload_VHA[i] := sedload_VHA[i] * BD;
    End;

  // sedload has to be written to a .txt file as output of the model

  setcurrentDir(File_output_dir);
  assignfile(Sediment, 'Total sediment.txt');
  rewrite(Sediment);
  Writeln(Sediment, 'Total erosion: ' + floattostr(round(sedprod*100)/100) + ' (kg)');
  Writeln(Sediment, 'Total deposition: ' + floattostr(round(depprod*100)/100) + ' (kg)');
  Writeln(Sediment, 'Sediment leaving the catchment, via the river: ' + floattostr(round((
          TEMP_river_sed_input * BD)*100)/100) + ' (kg)');
  Writeln(Sediment, 'Sediment leaving the catchment, not via the river: ' + floattostr(round((
          TEMP_outside_sed_input * BD)*100)/100) + ' (kg)');
  Writeln(Sediment, 'Sediment trapped in buffers: ' + floattostr(round((TEMP_buffer_sed_input * BD)*
  100)/100) + ' (kg)');
  Writeln(Sediment, 'Sediment trapped in open water: ' + floattostr(round((TEMP_pond_sed_input * BD)
  *100)/100) + ' (kg)');
  Writeln(Sediment,'_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _');
  Writeln(Sediment,'');
  Writeln(Sediment, 'Total sediment passing at each outlet [kg]');
  // write title
  Write(Sediment, 'Outlet ID', chr(9), 'Sediment');
  // write column headings
  writeln(Sediment, '');
  // go to next line

  For i := 1 To numOutlet Do
    Begin
      Write(Sediment, IntToStr(i), chr(9), floattostr(sedload[i]));
      writeln(Sediment, '');
    End;
  closefile(Sediment);
  //The memory of Sediment is released

  If VHA Then
    Begin
      setcurrentDir(File_output_dir);
      assignfile(Sediment_VHA, 'Total sediment VHA.txt');
      rewrite(Sediment_VHA);
      Writeln(Sediment_VHA, 'Total sediment flowing into each VHA river segment [kg]');
      // write title
      Write(Sediment_VHA, 'VHA segment', chr(9), 'Sediment');
      // write column headings
      writeln(Sediment_VHA, '');
      // go to next line

      For i := 1 To numRivSeg Do
        Begin
          Write(Sediment_VHA, IntToStr(i), chr(9), floattostr(sedload_VHA[i]));
          writeln(Sediment_VHA, '');
        End;
      closefile(Sediment_VHA);
      //The memory of Sediment is released
    End;

  If (Include_sewer) Then
    // total amount of sediment exported through sewer system is written to .txt file
    Begin
      sewer_out_sed := sewer_out_sed * BD;
      // convert to kg

      setcurrentDir(File_output_dir);
      assignfile(sewer_out, 'Sewer output sediment.txt');
      rewrite(sewer_out);
      Writeln(sewer_out, 'Total amount of sediment leaving the system through the sewers [kg]');
      // write title
      Writeln(sewer_out, floattostr(sewer_out_sed));
      closefile(sewer_out);
      //The memory of sewer_out is released
    End;


{for teller:= ncol*nrow downto 1 do
begin // begin lus
   i:=row[teller];  j:=column[teller];
 IF (PRC[i,j] <> 0) OR (PRC[i,j] <> -1) OR (PRC[i,j] <> -5) then
 begin
          if SEDI_IN[i,j] - SEDI_OUT[i,j] < 0 then
            sedprod := sedprod + SEDI_IN[i,j] - SEDI_OUT[i,j]
          else
            depprod := depprod + SEDI_IN[i,j] - SEDI_OUT[i,j];
        end;
 end;}

  For m := 1 To nrow Do
    For n := 1 To ncol Do
      Begin
        SEDI_IN2[m,n] := SEDI_IN[m,n] * BD;
        SEDI_OUT2[m,n] := SEDI_OUT[m,n] * BD;
        //depprod2[m,n] := SEDI_IN2[m,n]-SEDI_OUT2[m,n];
        SEDI_EXPORT_kg[m,n] := SEDI_EXPORT[m,n] * BD;
      End;
  // Dispose Temp 2D maps
  DisposeDynamicRdata(SEDI_IN);
  DisposeDynamicRdata(SEDI_OUT);
  //********************
End;


//sediment dat toekomt in elke outlet lineair verdelen over hydrogram
// resultaat wegschrijven naar .txt file (zoals voor discharge)

Procedure Distribute_sedimentRadar;

Var
  i, j, NTimesteps: integer;
  ER_clay: double;
  clay_cont, clay_cont_VHA: floatArray;
  fraction_discharge, fraction_discharge_VHA, sediment_result,
  sediment_conc, sediment_conc_VHA, sediment_result_VHA: floatArray2;
  sediment, sediment_VHA, sed_conc, sed_conc_VHA, clay_txt, clay_VHA_txt: textfile;

Begin
  If convert_output Then
    NTimesteps := NTimesteps2
  Else
    NTimesteps := NumberofTimesteps;

  // sediment load is distributed over hydrogram and written to .txt file

  setlength(fraction_discharge, NTimesteps + 1, numOutlet + 1);
  setlength(sediment_result, NTimesteps + 1, numOutlet + 1);
  For i := 0 To NTimesteps Do
    For j := 1 To numOutlet Do
      Begin
        If Sum_discharge[j] > 0 Then
          fraction_discharge[i, j] := Result_Discharge[i, j] / Sum_discharge[j]
        Else
          fraction_discharge[i, j] := 0;

        sediment_result[i, j] := fraction_discharge[i, j] * sedload[j];
      End;

  // write sediment_result to .txt

  setcurrentDir(File_output_dir);
  assignfile(Sediment, 'Sediment.txt');
  rewrite(Sediment);
  If convert_output Then
    Begin
      Writeln(Sediment, 'Sediment passing at each outlet [kg] with ',
              Timestep_output, ' minutes timestep');
      // write title
      Write(Sediment, 'Time (min)', chr(9));
    End
  Else
    Begin
      Writeln(Sediment, 'Sediment passing at each outlet [kg] with ',
              Timestep_model, ' seconds timestep');
      // write title
      Write(Sediment, 'Time (sec)', chr(9));
    End;
  For j := 1 To numOutlet Do
    Write(Sediment, 'Outlet ', j, chr(9));
  // write column headings
  writeln(Sediment, '');
  // go to next line

  If convert_output Then
    Begin
      For i := 0 To NTimeSteps2 Do
        Begin
          Write(Sediment, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
          For j := 1 To numOutlet Do
            Write(Sediment, floattostr(sediment_result[i, j]), chr(9));
          //Amount of sediment per time step is written to the .txt file
          writeln(Sediment, '');
        End;
    End
  Else
    Begin
      For i := 0 To NumberOfTimesteps Do
        Begin
          Write(Sediment, IntToStr(TimeSeries_output[i]), chr(9));
          For j := 1 To numOutlet Do
            Write(Sediment, floattostr(sediment_result[i, j]), chr(9));
          //Amount of sediment per time step is written to the .txt file
          writeln(Sediment, '');
        End;
    End;
  closefile(Sediment);
  //The memory of Sediment is released

  // sediment load for each VHA segment is distributed over hydrogram and written to .txt file

  If VHA Then
    Begin
      setlength(fraction_discharge_VHA, NTimesteps + 1, numRivSeg + 1);
      setlength(sediment_result_VHA, NTimesteps + 1, numRivSeg + 1);
      For i := 0 To NTimesteps Do
        For j := 1 To numRivSeg Do
          Begin
            If Sum_discharge_VHA[j] = 0 Then
              sediment_result_VHA[i, j] := 0
            Else
              Begin
                fraction_discharge_VHA[i, j] := Result_Discharge_VHA[i, j] / Sum_discharge_VHA[j];
                sediment_result_VHA[i, j] := fraction_discharge_VHA[i, j] * sedload_VHA[j];
              End;
          End;

      // write sediment_result_VHA to .txt
      assignfile(Sediment_VHA, 'Sediment_VHA.txt');
      rewrite(Sediment_VHA);
      If convert_output Then
        Begin
          Writeln(Sediment_VHA, 'Sediment flowing in each river segment [kg] with ',
                  Timestep_output, ' minutes timestep');
          // write title
          Write(Sediment_VHA, 'Time (min)', chr(9));
        End
      Else
        Begin
          Writeln(Sediment_VHA, 'Sediment passing in each river segment [kg] with ',
                  Timestep_model, ' seconds timestep');
          // write title
          Write(Sediment_VHA, 'Time (sec)', chr(9));
        End;
      For j := 1 To numRivSeg Do
        Write(Sediment_VHA, 'VHA segment ', j, chr(9));
      // write column headings
      writeln(Sediment_VHA, '');
      // go to next line

      If convert_output Then
        Begin
          For i := 0 To NTimeSteps2 Do
            Begin
              Write(Sediment_VHA, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sediment_VHA, floattostr(sediment_result_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sediment_VHA, '');
            End;
        End
      Else
        Begin
          For i := 0 To NumberOfTimesteps Do
            Begin
              Write(Sediment_VHA, IntToStr(TimeSeries_output[i]), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sediment_VHA, floattostr(sediment_result_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sediment_VHA, '');
            End;
        End;
      closefile(Sediment_VHA);
      //The memory of Sediment is released
    End;


  // sediment concentrations are calculated and written to .txt file
  setlength(sediment_conc, NTimesteps + 1, numOutlet + 1);
  For i := 0 To NTimesteps Do
    For j := 1 To numOutlet Do
      Begin
        If Result_Discharge[i, j] = 0 Then
          sediment_conc[i, j] := 0
        Else
          sediment_conc[i, j] := (sediment_result[i, j] * 1000) / (Result_Discharge[i, j] * 1000);
        // in g/l
      End;

  setcurrentDir(File_output_dir);
  assignfile(Sed_conc, 'Sediment concentration.txt');
  rewrite(Sed_conc);
  Writeln(Sed_conc, 'Sediment concentration at each outlet [g/l]');
  // write title
  If convert_output Then
    Write(Sed_conc, 'Time (min)', chr(9))
  Else
    Write(Sed_conc, 'Time (sec)', chr(9));
  For j := 1 To numOutlet Do
    Write(Sed_conc, 'Outlet ', j, chr(9));
  // write column headings
  writeln(Sed_conc, '');
  // go to next line

  If convert_output Then
    Begin
      For i := 0 To NTimeSteps2 Do
        Begin
          Write(Sed_conc, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
          For j := 1 To numOutlet Do
            Write(Sed_conc, floattostr(sediment_conc[i, j]), chr(9));
          writeln(Sed_conc, '');
        End;
    End
  Else
    Begin
      For i := 0 To NumberOfTimesteps Do
        Begin
          Write(Sed_conc, IntToStr(TimeSeries_output[i]), chr(9));
          For j := 1 To numOutlet Do
            Write(Sed_conc, floattostr(sediment_conc[i, j]), chr(9));
          writeln(Sed_conc, '');
        End;
    End;
  closefile(Sed_conc);


  If VHA Then
    Begin
      setlength(sediment_conc_VHA, NTimesteps + 1, numRivSeg + 1);
      For i := 0 To NTimesteps Do
        For j := 1 To numRivSeg Do
          Begin
            If Result_Discharge_VHA[i, j] = 0 Then
              sediment_conc_VHA[i, j] := 0
            Else
              sediment_conc_VHA[i, j] :=
                                         (sediment_result_VHA[i, j] * 1000) / (Result_Discharge_VHA[
                                         i, j] * 1000);
            // in g/l
          End;

      setcurrentDir(File_output_dir);
      assignfile(Sed_conc_VHA, 'Sediment concentration_VHA.txt');
      rewrite(Sed_conc_VHA);
      Writeln(Sed_conc_VHA, 'Sediment concentration for each VHA river segment [g/l]');
      // write title
      If convert_output Then
        Write(Sed_conc_VHA, 'Time (min)', chr(9))
      Else
        Write(Sed_conc_VHA, 'Time (sec)', chr(9));
      For j := 1 To numRivSeg Do
        Write(Sed_conc_VHA, 'VHA segment ', j, chr(9));
      // write column headings
      writeln(Sed_conc_VHA, '');
      // go to next line

      If convert_output Then
        Begin
          For i := 0 To NTimeSteps2 Do
            Begin
              Write(Sed_conc_VHA, IntToStr(TimeSeries_tmp_fin[i] Div 60), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sed_conc_VHA, floattostr(sediment_conc_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sed_conc_VHA, '');
            End;
        End
      Else
        Begin
          For i := 0 To NumberOfTimesteps Do
            Begin
              Write(Sed_conc_VHA, IntToStr(TimeSeries_output[i]), chr(9));
              For j := 1 To numRivSeg Do
                Write(Sed_conc_VHA, floattostr(sediment_conc_VHA[i, j]), chr(9));
              //Amount of sediment per time step is written to the .txt file
              writeln(Sed_conc_VHA, '');
            End;
        End;
      closefile(Sed_conc_VHA);
      //The memory of Sediment is released
    End;

  If est_clay Then
    // estimate the clay content of the sediment based on enrichment ratio
    Begin
      setlength(clay_cont, numOutlet + 1);
      For i := 1 To numOutlet Do
        Begin
          ER_clay := (0.7732 * exp(-0.0508 * sediment_conc[1, i])) + 1;
          // formula Wang et al 2010
          clay_cont[i] := ER_clay * clay_parent;
        End;

      // write result to .txt file
      setcurrentDir(File_output_dir);
      assignfile(clay_txt, 'Clay content sediment.txt');
      rewrite(clay_txt);
      Writeln(clay_txt, 'Clay content of sediment at each outlet [%]');
      // write title
      Write(clay_txt, 'Outlet ID', chr(9), 'Clay content (%)');
      // write column headings
      writeln(clay_txt, '');
      // go to next line
      For i := 1 To numOutlet Do
        Begin
          Write(clay_txt, IntToStr(i), chr(9), floattostr(clay_cont[i]));
          writeln(clay_txt, '');
        End;
      closefile(clay_txt);

      If VHA Then     // estimate clay content of sediment for each VHA river segment
        Begin
          setlength(clay_cont_VHA, numRivSeg + 1);
          For i := 1 To numRivSeg Do
            Begin
              ER_clay := (0.7732 * exp(-0.0508 * sediment_conc_VHA[1, i])) + 1;
              // formula Wang et al 2010
              clay_cont_VHA[i] := ER_clay * clay_parent;
            End;

          // write result to .txt file
          setcurrentDir(File_output_dir);
          assignfile(clay_VHA_txt, 'Clay content sediment VHA.txt');
          rewrite(clay_VHA_txt);
          Writeln(clay_VHA_txt,
                  'Clay content of sediment flowing in each river segment [%]');
          // write title
          Write(clay_VHA_txt, 'River segment', chr(9), 'Clay content (%)');
          // write column headings
          writeln(clay_VHA_txt, '');
          // go to next line
          For i := 1 To numRivSeg Do
            Begin
              Write(clay_VHA_txt, IntToStr(i), chr(9), floattostr(clay_cont_VHA[i]));
              writeln(clay_VHA_txt, '');
            End;
          closefile(clay_VHA_txt);
        End;
    End;

End;
End.


