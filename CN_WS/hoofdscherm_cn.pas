
Unit Hoofdscherm_CN;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
Menus, ComCtrls, Inifiles, Input_kaarten_cn, ReadInParameters, LateralRedistribution,
tillage, Write_output, RData_CN;

Type 


{ THoofdscherm_CN_form }

  THoofdscherm_CN_form = Class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CnRun: TButton;
    Input: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure InputClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure CnRunClick(Sender: TObject);
    Private 
    { private declarations }
    Public 
    { public declarations }
  End;

Var 
  Hoofdscherm_CN_form: THoofdscherm_CN_form;
  inifile: Tinifile;
  resultfile : textfile ;
  i, j: integer;


Implementation

Uses 
CN_calculations,
Raster_calculations;

{$R *.lfm}


Procedure THoofdscherm_CN_form.CheckBox1Change(Sender: TObject);
Begin
  If CheckBox1.Checked Then
    Begin
      Simplified := True;
      CheckBox2.Checked := False;
      CheckBox2.Enabled := False;
    End
  Else
    Begin
      Simplified := False;
      CheckBox2.Enabled := True;
    End;
End;

Procedure THoofdscherm_CN_form.CheckBox2Change(Sender: TObject);
Begin
  If CheckBox2.Checked Then
    Begin
      Simplified := False;
      CheckBox1.Checked := False;
      CheckBox1.Enabled := False;
    End
  Else
    Begin
      CheckBox1.Enabled := True;
    End;
End;

Procedure THoofdscherm_CN_form.InputClick(Sender: TObject);

Var 
  Input_kaarten_cn_form: TInput_kaarten_cn_form;
Begin
  Input_kaarten_cn_form := TInput_kaarten_cn_form.Create(application);
  Input_kaarten_cn_form.Show;

  If FileExists(INIfilename) Then
    Begin
      Readsettings(INIfilename);
      //De namen van de bestanden worden aan de variabelen gekoppeld
      autoFill(Input_kaarten_cn_form);
      //inputformulier wordt ingevuld
    End;

End;

Procedure THoofdscherm_CN_form.FormClose(Sender: TObject;
                                         Var CloseAction: TCloseAction);
Begin
  Application.Terminate;
End;

Procedure THoofdscherm_CN_form.FormCreate(Sender: TObject);
Begin
  Simplified := False;
End;

//*****************************************************************************
//By clicking the 'CnRun' button the CNmodel is run with the input parameters
//*****************************************************************************
Procedure THoofdscherm_CN_form.CnRunClick(Sender: TObject);

Begin

  //Input maps are read and assigned a filename
  ReadInRasters;

  //Check whether number of rows, number of columns and resolution are equal for all input maps
  If Not intArrayIsEqual(nrowAR) Then
    Begin
      showmessage('The number of rows should be the same for all input maps!');
      Exit;
    End;
  If Not intArrayIsEqual(ncolAR) Then
    Begin
      showmessage('The number of columns should be the same for all input maps!');
      Exit;
    End;
  If Not doubArrayIsEqual(resAR) Then
    Begin
      showmessage('The resolution should be the same for all input maps!');
      Exit;
    End;


  If Not Simplified Then
    Begin
      // Courant criterium is checked
      If Timestep_model>=resAR[1]/0.3 Then
        Begin
          showmessage(
                 'Courant criterium for model stability violated! Please select a smaller timestep.'
          );
          Exit;
        End;
    End;

  If Not Use_Rfactor Then
    Begin
      ReadRainfallFile(Raindata, RainfallFilename);
      //The .txt file with rainfall per timestep is read and written to a variable
      CalculateRFactor;
      // R factor is calculated from given rainfall record
    End;

  Allocate_Memory;
  // voor verschillende rasters

  If Not Simplified Then
    CalculateRe(ReMap, PRC, CNmap, alpha, beta);
  //Amount of rainfall excess or deficit is calculated

  Topo_Calculations;

// Sort DTM, calculate slope and aspect, calculate routing, calculate UPAREA, calculate LS factor RUSLE

  // number and position of outlets is determined. Lowest outlet is also determined.
  calcOutlet;

  If Not Simplified Then
    CalculateTimeDependentRunoff(Remap, RainData, Routing, PRC);
  //Amount of runoff per timestep is calculated

//CalculateRunoffAcc(UpArea, Remap, PRC); //This procedure was used in the past to calculate the temporally lumped cummulative runoff map (not used anymore)

  Water;
  // water erosion calculations

  If Not Simplified Then
    Distribute_sediment;
  // sediment is distributed over hydrogram

  Tillage_dif;
  // tillage erosion calculations

  write_maps;
  // write output maps

  // write routing output

  Write_Routing_Table;



  //The memory is released for all created maps
  Release_Memory;


  statusbar1.Simpletext := 'Finished';
  showmessage('Model run completed successfully. Click OK to quit the program.');
  Application.Terminate;

End;
End.
