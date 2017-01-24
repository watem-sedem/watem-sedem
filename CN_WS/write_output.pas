unit Write_output;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RData_CN, ReadInParameters, CN_calculations,
  Idrisi, Dialogs, LateralRedistribution;

Procedure Write_maps;

implementation

Procedure Write_maps;

begin

If Write_Sediexport then
begin
  //  writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_m3'+'.rst', SEDI_EXPORT);
     writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_kg'+'.rst', SEDI_EXPORT_kg);
     writeIdrisi32file(ncol,nrow,File_output_dir+'SediIn_kg'+'.rst', SEDI_IN2);
     writeIdrisi32file(ncol,nrow,File_output_dir+'SediOut_kg'+'.rst', SEDI_OUT2);
     //writeIdrisi32file(ncol,nrow,File_output_dir+'Dep_prod_kg'+'.rst', depprod2);
     {writeGIdrisi32file(ncol,nrow,File_output_dir+'row'+'.rst', row2);
     writeGIdrisi32file(ncol,nrow,File_output_dir+'col'+'.rst', col2);   }
end;
if Write_TILEROS then writeIdrisi32file(ncol,nrow,File_output_dir+'TILEROS'+'.rst', TILEROS);
if Write_WATEREROS then
begin
  writeIdrisi32file(ncol,nrow,File_output_dir+'WATEREROS (mm per gridcel)'+'.rst', WATEREROS);  // WATEREROS [mm]
  // writeIdrisi32file(ncol, nrow, File_output_dir + 'WATEREROS (m3 per gridcel)' + '.rst', watereros_cubmeter);
  writeIdrisi32file(ncol, nrow, File_output_dir + 'WATEREROS (kg per gridcel)' + '.rst', WATEREROS_kg);
end;
if Write_UPAREA then writeIdrisi32file(ncol,nrow,File_output_dir+'UPAREA'+'.rst', UPAREA);
if Write_LS then writeIdrisi32file(ncol,nrow,File_output_dir+'LS'+'.rst', LS);
if Write_SLOPE then
writeIdrisi32file(ncol,nrow,File_output_dir+'SLOPE'+'.rst', SLOPE);
if write_RUSLE then
writeidrisi32file(ncol,nrow,File_output_dir+'RUSLE'+'.rst',RUSLE);  // POTENTIAL soil erosion
if Write_ASPECT then
writeidrisi32file(ncol,nrow,File_output_dir+'AspectMap'+'.rst',Aspect); //Aspectmap (.RST) is created

if not simplified then
begin
  if Write_TOTRUN then
    writeidrisi32file(ncol,nrow,File_output_dir+'Total runoff'+'.rst',RunoffTotMap);  //Cumulative runoff for the entire event
  if write_RE then
    writeidrisi32file(ncol,nrow,File_output_dir+'Remap'+'.rst',Remap);  // rainfall excess map
end;

end;

end.

