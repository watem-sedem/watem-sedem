program watem_sedem_gui_LT;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Hoofdscherm_CN, Input_kaarten_cn, RData_CN, GData_CN,
  Idrisi,
  ReadInParameters, Write_output, Calculations;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THoofdscherm_CN_form, Hoofdscherm_CN_form);
  Application.CreateForm(TInput_kaarten_cn_form, Input_kaarten_cn_form);
  Application.Run;
end.

