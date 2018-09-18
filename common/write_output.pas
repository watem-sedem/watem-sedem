
Unit Write_output;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN, ReadInParameters, CN_calculations,
Idrisi, Dialogs, LateralRedistribution;

Procedure Write_maps;
Procedure Write_Routing_Table;

Implementation

Procedure Write_maps;
Begin
  If Write_Sediexport Then
    Begin
      //  writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_m3'+'.rst', SEDI_EXPORT);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_kg'+'.rst', SEDI_EXPORT_kg);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediIn_kg'+'.rst', SEDI_IN2);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediOut_kg'+'.rst', SEDI_OUT2);
      //writeIdrisi32file(ncol,nrow,File_output_dir+'Dep_prod_kg'+'.rst', depprod2);

{writeGIdrisi32file(ncol,nrow,File_output_dir+'row'+'.rst', row2);
     writeGIdrisi32file(ncol,nrow,File_output_dir+'col'+'.rst', col2);   }
    End;
  If Write_TILEROS Then writeIdrisi32file(ncol,nrow,File_output_dir+'TILEROS'+'.rst', TILEROS);
  If Write_WATEREROS Then
    Begin
      writeIdrisi32file(ncol,nrow,File_output_dir+'WATEREROS (mm per gridcel)'+'.rst', WATEREROS);
      // WATEREROS [mm]

// writeIdrisi32file(ncol, nrow, File_output_dir + 'WATEREROS (m3 per gridcel)' + '.rst', watereros_cubmeter);
      writeIdrisi32file(ncol, nrow, File_output_dir + 'WATEREROS (kg per gridcel)' + '.rst',
                        WATEREROS_kg);
    End;
  If Write_UPAREA Then writeIdrisi32file(ncol,nrow,File_output_dir+'UPAREA'+'.rst', UPAREA);
  If Write_LS Then writeIdrisi32file(ncol,nrow,File_output_dir+'LS'+'.rst', LS);
  If Write_SLOPE Then
    writeIdrisi32file(ncol,nrow,File_output_dir+'SLOPE'+'.rst', SLOPE);
  If Write_RUSLE Then
    writeidrisi32file(ncol,nrow,File_output_dir+'RUSLE'+'.rst',RUSLE);
  // potential soil erosion
  If Write_ASPECT Then
    writeidrisi32file(ncol,nrow,File_output_dir+'AspectMap'+'.rst',Aspect);
  //Aspectmap (.RST) is created

  If Not simplified Then
    Begin
      If Write_TOTRUN Then
        writeidrisi32file(ncol,nrow,File_output_dir+'Total runoff'+'.rst',RunoffTotMap);
      //Cumulative runoff for the entire event
      If write_RE Then
        writeidrisi32file(ncol,nrow,File_output_dir+'Remap'+'.rst',Remap);
      // rainfall excess map
    End;
End;

Procedure Write_Routing_Table;
// writes the routing table to a textfile

Var 
  routingfile: textfile;
  k,l : integer;

Begin
  assignfile(routingfile, 'routing.txt');
  rewrite(routingfile);
  Writeln(routingfile,
          'col;row;target1col;target1row;part1;distance1;target2col;target2row;part2;distance2');


  For k := 1 To nrow Do
    For l := 1 To ncol Do
      Writeln(routingfile,  IntToStr(l)+';'+ IntToStr(k) + ';'
      + IntToStr(Routing[k,l].Target1Col)  + ';' + IntToStr(Routing[k,l].Target1Row)+ ';' +
      floattostr(Routing[k,l].part1)+ ';' + floattostr(Routing[k,l].distance1) + ';'
      + IntToStr(Routing[k,l].Target2Col)  + ';' + IntToStr(Routing[k,l].Target2Row)+ ';' +
      floattostr(Routing[k,l].part2)+ ';' + floattostr(Routing[k,l].distance2)
      );

  closefile(routingfile);

End;

End.
