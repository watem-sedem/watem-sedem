program CN_WSmodel;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Dos, Interfaces, ReadInParameters, Write_output,
  CN_calculations, Raster_calculations, LateralRedistribution, Idrisi, tillage,
  RData_CN, GData_CN
  { you can add units after this };


type

  TCN_WSApplication = class(TCustomApplication)
    protected
      procedure DoRun; override;
    public
      constructor Create(TheOwner:TComponent);override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
  end;


var
  //execution var
  hr,mins,se,s1: word;

procedure StartClock;
begin
  GetTime(hr,mins,se,s1);
end;

function StopClock:string;
var
  hr2,min2,se2 : word;
begin
  GetTime(hr2,min2,se2,s1);
  result:= inttostr(se2-se+(min2-mins)*60+(hr2-hr)*60*60);
end;

procedure TCN_WSApplication.DoRun;
var
    Time:String;
    ErrorMsg: String;
    filename: String;
    i : integer;
  begin
    StartClock;
    writeln;
    writeln;
    writeln('CN_WS model');
    writeln;
    filename:='';
    for i := 1 to ParamCount do
     filename:=filename+ParamStr(i)+' ';
    WriteLn('Inifile : ', filename);
    writeln;

    ReadSettings(filename);        // read .INI file

    if errorFlag then
    begin
       ErrorMsg := errorDummy;
       writeln(ErrorMsg);
       writeln;
       writeln('Press <Enter> to quit');
       readln;
       Terminate;
       Exit;
    end;

    try
       ReadInRasters;
    except
       on E: Exception do
       begin
          writeln(E.Message);
          readln;
          Terminate;
          Exit
       end;

    end;

    if errorFlag then
    begin
       ErrorMsg := errorDummy;
       writeln(ErrorMsg);
       writeln;
       writeln('Press <Enter> to quit');
       readln;
       Terminate;
       Exit;
    end;

  //Check whether number of rows, number of columns and resolution are equal for all input maps
   if not intArrayIsEqual(nrowAR) then
   begin
      ErrorMsg := 'Error: The number of rows should be the same for all input maps. Please verify your input rasters.';
      writeln(ErrorMsg);
      writeln;
      writeln('Press <Enter> to quit');
      readln;
      Terminate;
      Exit;
   end;
   if not intArrayIsEqual(ncolAR) then
   begin
      ErrorMsg := 'Error: The number of columns should be the same for all input maps. Please verify your input rasters.';
      writeln(ErrorMsg);
      writeln;
      writeln('Press <Enter> to quit');
      readln;
      Terminate;
      Exit;
   end;
   if not doubArrayIsEqual(resAR) then
   begin
      ErrorMsg:='Error: The resolution should be the same for all input maps. Please verify input rasters.';
      writeln(ErrorMsg);
      writeln;
      writeln('Press <Enter> to quit');
      readln;
      Terminate;
      Exit;
   end;

   if not Use_Rfactor then
   begin
     ReadRainfallFile(Raindata, RainfallFilename);  //The .txt file with rainfall per timestep is read and written to a variable
     CalculateRFactor;  // R factor is calculated from given rainfall record
   end;

   if not simplified then
   begin
     // Courant criterium is checked
     if Timestep_model>=resAR[1]/0.3 then
     begin
       ErrorMsg:='Error: Courant criterium for model stability violated. Please select a smaller timestep.';
       writeln(ErrorMsg);
        writeln;
        writeln('Press <Enter> to quit');
        readln;
        Terminate;
        Exit;
     end;
   end;

   Allocate_Memory;  // voor verschillende rasters

   if not Simplified then
     CalculateRe(ReMap, PRC, CNmap, alpha, beta);   //Amount of rainfall excess or deficit is calculated

   Topo_Calculations;       // Sort DTM, calculate slope and aspect, calculate routing, calculate UPAREA, calculate LS factor RUSLE

     if errorFlag then
    begin
       ErrorMsg := errorDummy;
       writeln(ErrorMsg);
       writeln;
       writeln('Press <Enter> to quit');
       readln;
       Terminate;
       Exit;
    end;

   // number and position of outlets is determined. Lowest outlet is also determined.
   calcOutlet;

   if not simplified then
     CalculateTimeDependentRunoff(Remap, RainData, Routing, PRC); //Amount of runoff per timestep is calculated

   Water;   // Water erosion calculations

   if not Simplified then
     Distribute_sediment;       // Sediment is distributed over hydrogram

   Tillage_dif;         // tillage erosion calculations

   write_maps;       // write output maps


//The memory is released for all created maps
   Release_Memory;


   Time:=StopClock;
    Writeln('Calculations completed. Program Execution Time: ',Time,' sec');
   // Writeln('Press <Enter> to continue (in case of multiple model runs) or quit');
   // readln;

  Terminate;
  end;


  constructor TCN_WSApplication.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException:=True;
  end;

  destructor TCN_WSApplication.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TCN_WSApplication.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ',ExeName,' -h');
  end;

  var
    Application: TCN_WSApplication;
  begin
    Application:=TCN_WSApplication.Create(nil);
    Application.Title:='CN_WS model';
    Application.Run;
    Application.Free;
end.

