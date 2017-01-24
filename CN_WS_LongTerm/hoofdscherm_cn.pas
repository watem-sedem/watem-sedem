unit Hoofdscherm_CN;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ComCtrls, Inifiles, Input_kaarten_cn, ReadInParameters, Write_output,
  Calculations, RData_CN;

Type


{ THoofdscherm_CN_form }

  THoofdscherm_CN_form = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CnRun: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Input: TButton;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure InputClick(Sender: TObject);
    procedure CnRunClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Hoofdscherm_CN_form: THoofdscherm_CN_form;
  inifile: Tinifile;
  i, j: integer;

implementation

{$R *.lfm}

{ THoofdscherm_CN_form }

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

procedure THoofdscherm_CN_form.FormCreate(Sender: TObject);
begin
  Simplified:=False;
end;

procedure THoofdscherm_CN_form.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Application.Terminate;
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

//*****************************************************************************
//By clicking the 'CnRun' button the CNmodel is run with the input parameters
//*****************************************************************************
procedure THoofdscherm_CN_form.CnRunClick(Sender: TObject);
var
   i: integer;
   Event_model: TProcess;
begin
ReadRainfallFile;

if not simplified then
  ReadRivVelFile;

DetectEvents;

DemarcateSeasons;

calc_numOutlet;        // number of outlets is calculated to set dimensions of result arrays

if VHA then            // if needed, number of VHA segments is calculated to set dimensions of result arrays
  calc_numVHA;


for i := 1 to numberOfEvents do
begin
 Write_rainfall(i);
 identifyInput(i);
 Write_ini;
 if i = 1 then        // .cmd file needs to be written only once
   Write_cmd;

 // Run the console version of the CN_WS_event model using created cmd file
 Event_model:= TProcess.Create(nil);
 Event_model.Executable := CmdFilename;
 Event_model.Options:=Event_model.Options + [poWaitOnExit];
 Event_model.Execute;
 Event_model.Free;

 ReadOutput;
 if i = 1 then
   ActivateMemory;
 writeOutput_event(i);
 updateOutput(i);
 ReleaseMemory; //(event maps)
 end;
 WriteOutput;
 ReleaseMemory2; //(total maps)

statusbar1.Simpletext := 'Finished';
showmessage('Model run completed successfully. Click OK to quit the program.');
Application.Terminate;
end;

end.

