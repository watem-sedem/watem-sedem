unit Write_output;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RData_CN, ReadInParameters, CN_calculations,
  Idrisi, Dialogs, LateralRedistribution;

Procedure Write_maps;
Procedure Write_Routing_Table;

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

Procedure Write_Routing_Table;
// writes the routing table to a textfile
var
  routingfile: textfile;
  k,l : integer;
  ny, nx : integer;

begin
  assignfile(routingfile, 'routing.txt');
  rewrite(routingfile);
  Writeln(routingfile, 'col;row;target1col;target1row;part1;distance1;target2col;target2row;part2;distance2');


  for k := 1 to nrow do
         for l := 1 to ncol do
            Writeln(routingfile,  IntToStr(k)+';'+ IntToStr(l) + ';'
               + IntToStr(Routing[k,l].Target1Col)  + ';' + IntToStr(Routing[k,l].Target1Row)+ ';' + floattostr(Routing[k,l].part1)+ ';' + floattostr(Routing[k,l].distance1) + ';'
               + IntToStr(Routing[k,l].Target2Col)  + ';' + IntToStr(Routing[k,l].Target2Row)+ ';' + floattostr(Routing[k,l].part2)+ ';' + floattostr(Routing[k,l].distance2)
            );

  closefile(routingfile);

end;



end.

