
Unit Write_output;

{$mode objfpc}{$H+}
{$R+}

Interface

Uses 
Classes, SysUtils, RData_CN, ReadInParameters, CN_calculations, write_raster;

Procedure Write_maps;
Procedure Write_Routing_Table;
Procedure Write_Routing_Table_RC(routing_cols, routing_rows: TIntarray);
Procedure Write_RFactor;

Implementation

Procedure Write_maps;
Begin

     if not outlet_select then
       begin
        writeSmallintFile(ncol,nrow, Datadir+'Outlet'+ext,Outlet);
       end;


  If Write_Sediexport Then
    Begin
      //  writefloatfile(ncol,nrow,File_output_dir+'SediExport_m3'+ext, SEDI_EXPORT);
      writefloatfile(ncol,nrow,File_output_dir+'SediExport_kg'+ext, SEDI_EXPORT);
      writefloatfile(ncol,nrow,File_output_dir+'SediIn_kg'+ext, SEDI_IN);
      writefloatfile(ncol,nrow,File_output_dir+'SediOut_kg'+ext, SEDI_OUT);
      writefloatfile(ncol,nrow,File_output_dir+'Capacity'+ext, CAPAC);
      //writefloatfile(ncol,nrow,File_output_dir+'Dep_prod_kg'+ext, depprod2);

{writeSmallintFile(ncol,nrow,File_output_dir+'row'+ext, row2);
     writeSmallintFile(ncol,nrow,File_output_dir+'col'+ext, col2);   }
    End;
  If Calc_tileros Then
    Begin
    writefloatfile(ncol,nrow,File_output_dir+'TILEROS (mm per gridcel)'+ext, TILEROS);
    writefloatfile(ncol,nrow,File_output_dir+'TILEROS (kg per gridcel)'+ext, TILEROS_kg);
    writefloatfile(ncol,nrow,File_output_dir+'SEDTIL_IN'+ext, SEDTIL_IN);
    writefloatfile(ncol,nrow,File_output_dir+'SEDTIL_OUT'+ext, SEDTIL_OUT);
    end;

  If Write_WATEREROS Then
    Begin
      writefloatfile(ncol,nrow,File_output_dir+'WATEREROS (mm per gridcel)'+ext, WATEREROS);
      // WATEREROS [mm]

// writefloatfile(ncol, nrow, File_output_dir + 'WATEREROS (m3 per gridcel)' + ext, watereros_cubmeter);
      writefloatfile(ncol, nrow, File_output_dir + 'WATEREROS (kg per gridcel)' + ext,
                        WATEREROS_kg);
    End;
  If Write_UPAREA Then writefloatfile(ncol,nrow,File_output_dir+'UPAREA'+ext, UPAREA);
  If Write_LS Then writefloatfile(ncol,nrow,File_output_dir+'LS'+ext, LS);
  If Write_SLOPE Then
    writefloatfile(ncol,nrow,File_output_dir+'SLOPE'+ext, SLOPE);
  If Write_RUSLE Then
    writefloatfile(ncol,nrow,File_output_dir+'RUSLE'+ext,RUSLE);
  // potential soil erosion
  If Write_ASPECT Then
    writefloatfile(ncol,nrow,File_output_dir+'AspectMap'+ext,Aspect);
  //Aspectmap (.RST) is created

  If Not simplified Then
    Begin
      If Write_TOTRUN Then
        writefloatfile(ncol,nrow,File_output_dir+'Total runoff'+ext,RunoffTotMap);
      //Cumulative runoff for the entire event
      If write_RE Then
        writefloatfile(ncol,nrow,File_output_dir+'Remap'+ext,Remap);
      // rainfall excess map
    End;

  If river_routing Then
    Begin
    writefloatfile(ncol,nrow,File_output_dir+'cumulative'+ext,cumulative);
    End;

  If (include_sewer) Then
    Begin
    writefloatfile(ncol,nrow,File_output_dir+'sewer_in'+ext,SEWER_IN);
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

Procedure Write_Routing_Table_RC(routing_cols, routing_rows: TIntarray);
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

Procedure Write_RFactor;
// Writes the calculated R-Factor to a file
Var
   rfactorfile: textfile;
Begin
  setcurrentDir(File_output_dir);
  assignfile(rfactorfile, 'rfactor.txt');
  rewrite(rfactorfile);
  Writeln(rfactorfile, 'r-factor');
  Writeln(rfactorfile, RFactor*10000);  // write R-factor in same units as for the input R-factor
  closefile(rfactorfile);
end;

End.

