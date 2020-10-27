
Program cn_ws;

{$mode objfpc}{$H+}

{$R+} //enable range checking of arrays

Uses 
  {$IFDEF UNIX}{$IFDEF UseCThreads}
cthreads,
  {$ENDIF}{$ENDIF}
Classes, SysUtils, CustApp, Dos, Crt, runmodel;


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
  End;


Function StopClock(hr, mins, se, s1: word): string;

Var 
  hr2,min2,se2 : word;
Begin
  {$push}{$warn 5057 off}
  GetTime(hr2,min2,se2,s1);
  {$pop}
  result := floattostr(se2-se+(min2-mins)*60+(hr2-hr)*60*60+s1/100);
End;

Procedure TCN_WSApplication.DoRun;

Var 
  Time: String;
  filename: String;
  i : integer;
  hr,mins,se,s1: word;

Begin
  {$push}{$warn 5057 off}
  GetTime(hr,mins,se,s1);
  {$pop}
  writeln;
  writeln;
  writeln('CN_WS model');
  writeln;
  filename := '';
  For i := 1 To ParamCount Do
    filename := filename+ParamStr(i);
  WriteLn('Inifile : ', filename);
  writeln;

  runmodel.runmodel(filename);

  Time := StopClock(hr,mins,se,s1);
  Writeln('Calculations completed. Program Execution Time: ',Time,' sec');

  Terminate(0);
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


Var 
  Application: TCN_WSApplication;
Begin
  Application := TCN_WSApplication.Create(Nil);
  Application.Title := 'cn_ws';
  Application.Run;
  Application.Free;
End.
