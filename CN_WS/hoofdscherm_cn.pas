unit Hoofdscherm_CN;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ComCtrls, Inifiles, Input_kaarten_cn, ReadInParameters, LateralRedistribution,
  tillage, Write_output, RData_CN;

Type


{ THoofdscherm_CN_form }

  THoofdscherm_CN_form = class(TForm)
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
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure InputClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CnRunClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Hoofdscherm_CN_form: THoofdscherm_CN_form;
  inifile: Tinifile;
  resultfile : textfile ;
  i, j: integer;


implementation

uses
  CN_calculations,
  Raster_calculations;

{$R *.lfm}


procedure THoofdscherm_CN_form.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Simplified := True;
    CheckBox2.Checked := False;
    CheckBox2.Enabled := False;
  end
  else
  begin
    Simplified := False;
    CheckBox2.Enabled := True;
  end;
end;

procedure THoofdscherm_CN_form.CheckBox2Change(Sender: TObject);
begin
   if CheckBox2.Checked then
  begin
    Simplified := False;
    CheckBox1.Checked := False;
    CheckBox1.Enabled := False;
  end
  else
  begin
    CheckBox1.Enabled := True;
  end;
end;

procedure THoofdscherm_CN_form.InputClick(Sender: TObject);
var
  Input_kaarten_cn_form: TInput_kaarten_cn_form;
begin
  Input_kaarten_cn_form := TInput_kaarten_cn_form.Create(application);
  Input_kaarten_cn_form.Show;

  if FileExists(INIfilename) then
  begin
    Readsettings(INIfilename); //De namen van de bestanden worden aan de variabelen gekoppeld
    autoFill(Input_kaarten_cn_form);    //inputformulier wordt ingevuld
  end;

end;

procedure THoofdscherm_CN_form.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure THoofdscherm_CN_form.FormCreate(Sender: TObject);
begin
  Simplified := False;
end;

//*****************************************************************************
//By clicking the 'CnRun' button the CNmodel is run with the input parameters
//*****************************************************************************
procedure THoofdscherm_CN_form.CnRunClick(Sender: TObject);

begin

  //Input maps are read and assigned a filename
ReadInRasters;

    //Check whether number of rows, number of columns and resolution are equal for all input maps
   if not intArrayIsEqual(nrowAR) then
   begin
      showmessage('The number of rows should be the same for all input maps!');
      Exit;
   end;
   if not intArrayIsEqual(ncolAR) then
   begin
      showmessage('The number of columns should be the same for all input maps!');
      Exit;
   end;
   if not doubArrayIsEqual(resAR) then
   begin
      showmessage('The resolution should be the same for all input maps!');
      Exit;
   end;


   if not Simplified then
   begin
     // Courant criterium is checked
     if Timestep_model>=resAR[1]/0.3 then
     begin
       showmessage('Courant criterium for model stability violated! Please select a smaller timestep.');
       Exit;
     end;
   end;

   if not Use_Rfactor then
   begin
     ReadRainfallFile(Raindata, RainfallFilename);  //The .txt file with rainfall per timestep is read and written to a variable
     CalculateRFactor;  // R factor is calculated from given rainfall record
   end;

   Allocate_Memory;  // voor verschillende rasters

   if not Simplified then
       CalculateRe(ReMap, PRC, CNmap, alpha, beta);   //Amount of rainfall excess or deficit is calculated

   Topo_Calculations;       // Sort DTM, calculate slope and aspect, calculate routing, calculate UPAREA, calculate LS factor RUSLE

  // number and position of outlets is determined. Lowest outlet is also determined.
  calcOutlet;

  if not Simplified then
    CalculateTimeDependentRunoff(Remap, RainData, Routing, PRC); //Amount of runoff per timestep is calculated
  //CalculateRunoffAcc(UpArea, Remap, PRC); //This procedure was used in the past to calculate the temporally lumped cummulative runoff map (not used anymore)

  Water;     // water erosion calculations

  if not Simplified then
  Distribute_sediment;          // sediment is distributed over hydrogram

  Tillage_dif;          // tillage erosion calculations

  write_maps;          // write output maps



//The memory is released for all created maps
Release_Memory;


statusbar1.Simpletext := 'Finished';
showmessage('Model run completed successfully. Click OK to quit the program.');
Application.Terminate;

end;
end.

