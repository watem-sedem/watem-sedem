program watem_sedem_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Hoofdscherm_CN, Input_kaarten_cn, RData_CN, GData_CN, CN_calculations,
  Idrisi, Raster_calculations, LateralRedistribution,
  tillage, ReadInParameters, Write_output;

{$R *.res}

begin
  Application.Title:='WaTEM/SEDEM model';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(THoofdscherm_CN_form, Hoofdscherm_CN_form);
  Application.CreateForm(TInput_kaarten_cn_form, Input_kaarten_cn_form);
  Application.Run;
end.

