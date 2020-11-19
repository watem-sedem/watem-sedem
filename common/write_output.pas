
Unit Write_output;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN, ReadInParameters, CN_calculations, Idrisi;

Procedure Write_maps;
Procedure Write_Routing_Table;
Procedure Write_Routing_Table_RC(routing_cols, routing_rows: Gvector);

Implementation

Procedure Write_maps;
Begin

     if not outlet_select then
       begin
        writeGIdrisi32file(ncol,nrow, Datadir+'Outlet'+'.rst',Outlet);
       end;


  If Write_Sediexport Then
    Begin
      //  writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_m3'+'.rst', SEDI_EXPORT);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediExport_kg'+'.rst', SEDI_EXPORT);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediIn_kg'+'.rst', SEDI_IN);
      writeIdrisi32file(ncol,nrow,File_output_dir+'SediOut_kg'+'.rst', SEDI_OUT);
      writeIdrisi32file(ncol,nrow,File_output_dir+'Capacity'+'.rst', CAPAC);
      //writeIdrisi32file(ncol,nrow,File_output_dir+'Dep_prod_kg'+'.rst', depprod2);

{writeGIdrisi32file(ncol,nrow,File_output_dir+'row'+'.rst', row2);
     writeGIdrisi32file(ncol,nrow,File_output_dir+'col'+'.rst', col2);   }
    End;
  If Calc_tileros Then
    Begin
    writeIdrisi32file(ncol,nrow,File_output_dir+'TILEROS'+'.rst', TILEROS);
    writeIdrisi32file(ncol,nrow,File_output_dir+'SEDTIL_IN'+'.rst', SEDTIL_IN);
    writeIdrisi32file(ncol,nrow,File_output_dir+'SEDTIL_OUT'+'.rst', SEDTIL_OUT);
    end;

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

  If river_routing Then
    Begin
    writeidrisi32file(ncol,nrow,File_output_dir+'cumulative'+'.rst',cumulative);
    End;

  If (include_sewer) Then
    Begin
    writeidrisi32file(ncol,nrow,File_output_dir+'sewer_in'+'.rst',SEWER_IN);
    end;

End;

Procedure Write_Routing_Table;
// writes the routing table to a textfile

Var 
  routingfile: textfile;
  k,l : integer;
  sep: char;

Begin
  setcurrentDir(File_output_dir);
  sep := #9;
  assignfile(routingfile, 'routing.txt');
  rewrite(routingfile);
  Writeln(routingfile,
          'col'+sep+'row'+sep+'target1col'+sep+'target1row'+sep+'part1'+sep+'distance1'+sep+'target2col'+sep+'target2row'+sep+'part2'+sep+'distance2');


  For k := 1 To nrow Do
    For l := 1 To ncol Do
      begin
      if (Routing[k,l].Target1Col <1) and (Routing[k,l].Target2Col <1) then continue; // skip empty rows
      Writeln(routingfile,  IntToStr(l)+sep+ IntToStr(k) + sep
      + IntToStr(Routing[k,l].Target1Col)  + sep + IntToStr(Routing[k,l].Target1Row)+ sep +
      floattostr(Routing[k,l].part1)+ sep + floattostr(Distance1(Routing,k,l)) + sep
      + IntToStr(Routing[k,l].Target2Col)  + sep + IntToStr(Routing[k,l].Target2Row)+ sep +
      floattostr(Routing[k,l].part2)+ sep + floattostr(Distance2(Routing,k,l))
      );

      end;

  closefile(routingfile);

End;


Procedure Write_Routing_Table_RC(routing_cols, routing_rows: Gvector);
// writes the routing table to a textfile

Var
 i: integer;
 sep: char;
   routingfile: textfile;
Begin
  if not length(routing_cols) = length(routing_rows) then
    raise EInputException.Create('row and col length not equal');


  setcurrentDir(File_output_dir);
  sep := #9;
  assignfile(routingfile, 'routing_rowcol.txt');
  rewrite(routingfile);
  Writeln(routingfile,
          'col'+sep+'row');

  for i:= 0 to length(routing_cols)-1 do
    begin

      if routing_cols[i]>0 then
        Writeln(routingfile,  IntToStr(routing_cols[i])+sep+ IntToStr(routing_rows[i] ));

    end;

  closefile(routingfile);
End;


End.

