unit Input_kaarten_cn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, DBCtrls, EditBtn, Spin, ComCtrls, ExtCtrls, Inifiles, types, ReadInParameters;

type

  { TInput_kaarten_cn_form }

  TInput_kaarten_cn_form = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button2: TButton;
    Button20: TButton;
    Button23: TButton;
    Button26: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Button37: TButton;
    Button38: TButton;
    Button39: TButton;
    Button40: TButton;
    Button41: TButton;
    Button42: TButton;
    Button43: TButton;
    Button44: TButton;
    Button45: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
    DirectoryEdit2: TDirectoryEdit;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    Edit23: TEdit;
    Edit24: TEdit;
    Edit25: TEdit;
    Edit26: TEdit;
    Edit27: TEdit;
    Edit28: TEdit;
    Edit29: TEdit;
    Edit3: TEdit;
    Edit30: TEdit;
    Edit31: TEdit;
    Edit32: TEdit;
    Edit33: TEdit;
    Edit34: TEdit;
    Edit35: TEdit;
    Edit36: TEdit;
    Edit37: TEdit;
    Edit38: TEdit;
    Edit39: TEdit;
    Edit4: TEdit;
    Edit40: TEdit;
    Edit41: TEdit;
    Edit42: TEdit;
    Edit43: TEdit;
    Edit44: TEdit;
    Edit47: TEdit;
    Edit48: TEdit;
    Edit49: TEdit;
    Edit5: TEdit;
    Edit50: TEdit;
    Edit51: TEdit;
    Edit52: TEdit;
    Edit53: TEdit;
    Edit54: TEdit;
    Edit55: TEdit;
    Edit56: TEdit;
    Edit57: TEdit;
    Edit58: TEdit;
    Edit59: TEdit;
    Edit6: TEdit;
    Edit60: TEdit;
    Edit61: TEdit;
    Edit62: TEdit;
    Edit63: TEdit;
    Edit64: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox13: TGroupBox;
    GroupBox14: TGroupBox;
    GroupBox15: TGroupBox;
    GroupBox18: TGroupBox;
    GroupBox19: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox20: TGroupBox;
    GroupBox22: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label36: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenDialog3: TOpenDialog;
    OpenDialog4: TOpenDialog;
    PageControl1: TPageControl;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet7: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
   { procedure Button12Click(Sender: TObject);  }
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure Button35Click(Sender: TObject);
    procedure Button36Click(Sender: TObject);
    procedure Button37Click(Sender: TObject);
    procedure Button38Click(Sender: TObject);
    procedure Button39Click(Sender: TObject);
    procedure Button40Click(Sender: TObject);
    procedure Button41Click(Sender: TObject);
    procedure Button42Click(Sender: TObject);
    procedure Button43Click(Sender: TObject);
    procedure Button44Click(Sender: TObject);
    procedure Button45Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox10Change(Sender: TObject);
    procedure CheckBox11Change(Sender: TObject);
    procedure CheckBox15Change(Sender: TObject);
    procedure CheckBox16Change(Sender: TObject);
    procedure CheckBox17Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure CheckBox9Change(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
   { procedure CheckGroup2ItemClick(Sender: TObject; Index: integer);   }
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure DirectoryEdit2Change(Sender: TObject);
    procedure Edit10EditingDone(Sender: TObject);
    procedure Edit11EditingDone(Sender: TObject);
    procedure Edit12EditingDone(Sender: TObject);
    procedure Edit13EditingDone(Sender: TObject);
    procedure Edit14EditingDone(Sender: TObject);
    procedure Edit19EditingDone(Sender: TObject);
    procedure Edit20EditingDone(Sender: TObject);
    procedure Edit22EditingDone(Sender: TObject);
    procedure Edit23EditingDone(Sender: TObject);
    procedure Edit26EditingDone(Sender: TObject);
    procedure Edit27EditingDone(Sender: TObject);
    procedure Edit29EditingDone(Sender: TObject);
    procedure Edit30EditingDone(Sender: TObject);
    procedure Edit32EditingDone(Sender: TObject);
    procedure Edit34EditingDone(Sender: TObject);
    procedure Edit35EditingDone(Sender: TObject);
    procedure Edit36EditingDone(Sender: TObject);
    procedure Edit37EditingDone(Sender: TObject);
    procedure Edit40EditingDone(Sender: TObject);
    procedure Edit42EditingDone(Sender: TObject);
    procedure Edit43EditingDone(Sender: TObject);
    procedure Edit47EditingDone(Sender: TObject);
    procedure Edit49EditingDone(Sender: TObject);
    procedure Edit51EditingDone(Sender: TObject);
    procedure Edit53EditingDone(Sender: TObject);
    procedure Edit54EditingDone(Sender: TObject);
    procedure Edit55EditingDone(Sender: TObject);
    procedure Edit56EditingDone(Sender: TObject);
    procedure Edit58EditingDone(Sender: TObject);
    procedure Edit59EditingDone(Sender: TObject);
    procedure Edit60EditingDone(Sender: TObject);
    procedure Edit61EditingDone(Sender: TObject);
    procedure Edit62EditingDone(Sender: TObject);
    procedure Edit63EditingDone(Sender: TObject);
    procedure Edit64EditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure SpinEdit1EditingDone(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure autoFill (var Input_kaarten_cn_form:TInput_kaarten_cn_form);
  procedure buffer_default;

var
  Input_kaarten_cn_form: TInput_kaarten_cn_form;
  Ini_select: Boolean;

implementation

uses
  Hoofdscherm_CN;

{$R *.lfm}

{ TInput_kaarten_cn_form }

//******************************************************************************
//The lines below define how the input maps are selected in the 'Input' window
//******************************************************************************

//When the window is closed memory is released
procedure TInput_kaarten_cn_form.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := cafree;
end;

procedure TInput_kaarten_cn_form.FormCreate(Sender: TObject);
begin
  // fill drop down list with months
   ComboBox2.Items.Add('January');
   ComboBox2.Items.Add('February');
   ComboBox2.Items.Add('March');
   ComboBox2.Items.Add('April');
   ComboBox2.Items.Add('May');
   ComboBox2.Items.Add('June');
   ComboBox2.Items.Add('July');
   ComboBox2.Items.Add('August');
   ComboBox2.Items.Add('September');
   ComboBox2.Items.Add('October');
   ComboBox2.Items.Add('November');
   ComboBox2.Items.Add('December');

   if Simplified then
   begin
      Button19.Enabled:=False;
      Edit24.Enabled:=False;
      Label12.Enabled:=False;
      Edit19.Enabled:=False;
      Label15.Enabled:=False;
      Edit20.Enabled:=False;
      Button8.Enabled:=False;
      Edit8.Enabled:=False;
      Button20.Enabled:=False;
      Edit17.Enabled:=False;
      Button23.Enabled:=False;
      Edit41.Enabled:=False;
      Button26.Enabled:=False;
      Edit44.Enabled:=False;
      Label8.Enabled:=False;
      SpinEdit2.Enabled:=False;
      Checkbox9.Enabled:=False;
      SpinEdit3.Enabled:=False;
      Label18.Enabled:=False;
      Button45.Enabled:=False;
      Button44.Enabled:=False;
      Button43.Enabled:=False;
      Button42.Enabled:=False;
      Edit30.Enabled:=False;
      Edit60.Enabled:=False;
      Edit56.Enabled:=False;
      Edit61.Enabled:=False;
      Checkbox17.Enabled:=False;
      Label20.Enabled:=False;
      Label19.Enabled:=False;
      Edit62.Enabled:=False;
   end;
end;

procedure TInput_kaarten_cn_form.ListBox1Click(Sender: TObject);
begin

end;

//When clicking 'Abort' the window is closed an no (new) .ini file is created
procedure TInput_kaarten_cn_form.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

procedure TInput_kaarten_cn_form.Button10Click(Sender: TObject);
begin
   if opendialog3.Execute then
     INIfilename := opendialog3.filename;
   Readsettings(INIfilename);
   Setcurrentdir(datadir);
   if not DirectoryExists(File_output_dir) then CreateDir(File_output_dir);  //create output folder if not existing
   Close;
   Input_kaarten_cn_form := TInput_kaarten_cn_form.Create(application);
   Input_kaarten_cn_form.Show;
   autoFill(Input_kaarten_cn_form);
   Ini_select := true;

end;

procedure TInput_kaarten_cn_form.Button11Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Outletfilename := opendialog1.filename; //Dit is de locatie + bestandsnaam + extensie!!
  Outletfilename := extractfilename(Outletfilename);
  Edit16.Text := Outletfilename;
end;

procedure TInput_kaarten_cn_form.Button12Click(Sender: TObject);
begin
  if opendialog1.Execute then
    riversegment_filename := opendialog1.filename; //Dit is de locatie + bestandsnaam + extensie!!
  riversegment_filename := extractfilename(riversegment_filename);
  Edit21.Text := riversegment_filename;
end;

procedure TInput_kaarten_cn_form.Button13Click(Sender: TObject);
begin
    if opendialog1.Execute then
      K_Factor_filename := opendialog1.filename;
    K_Factor_filename := extractfilename(K_Factor_filename);
    Edit25.Text := K_Factor_filename;
end;

procedure TInput_kaarten_cn_form.Button14Click(Sender: TObject);
begin
    if opendialog4.Execute then
      ModelExe := opendialog4.filename; //Dit is de locatie + bestandsnaam + extensie!!
    Edit18.Text := ModelExe;
end;

procedure TInput_kaarten_cn_form.Button15Click(Sender: TObject);
begin
    if opendialog1.Execute then
      Pf_Data_Filename := opendialog1.filename;
    Pf_Data_Filename := extractfilename(Pf_Data_Filename);
    Edit28.Text := Pf_Data_Filename;
end;

procedure TInput_kaarten_cn_form.Button16Click(Sender: TObject);
begin
    if opendialog1.Execute then
      ktil_Data_Filename := opendialog1.filename;
    ktil_Data_Filename := extractfilename(ktil_Data_Filename);
    Edit31.Text := ktil_Data_Filename;
end;

procedure TInput_kaarten_cn_form.Button17Click(Sender: TObject);
begin
    if opendialog1.Execute then
      kTC_Data_Filename := opendialog1.filename;
    kTC_Data_Filename := extractfilename(kTC_Data_Filename);
    Edit33.Text := kTC_Data_Filename;
end;

procedure TInput_kaarten_cn_form.Button18Click(Sender: TObject);
begin
     if opendialog1.Execute then
       Sewerfilename := opendialog1.filename;
     Sewerfilename := extractfilename(Sewerfilename);
     Edit38.Text := Sewerfilename;
end;

procedure TInput_kaarten_cn_form.Button19Click(Sender: TObject);
begin
    if opendialog2.Execute then
    RivVelFilename := opendialog2.filename;
  RivVelFilename := extractfilename(RivVelFilename);
  Edit24.Text := RivVelFilename;
end;

procedure TInput_kaarten_cn_form.Button1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    DTM_filename := opendialog1.filename; //Dit is de locatie + bestandsnaam + extensie!!
  DTM_filename := extractfilename(DTM_filename);
  Edit1.Text := DTM_filename;
end;

procedure TInput_kaarten_cn_form.Button20Click(Sender: TObject);
begin
    if opendialog1.Execute then
    CNmapSummer := opendialog1.filename;
  CNmapSummer := extractfilename(CNmapSummer);
  Edit17.Text := CNmapSummer;
end;

procedure TInput_kaarten_cn_form.Button23Click(Sender: TObject);
begin
if opendialog1.Execute then
    CNmapFall := opendialog1.filename;
  CNmapFall := extractfilename(CNmapFall);
  Edit41.Text := CNmapFall;
end;

procedure TInput_kaarten_cn_form.Button26Click(Sender: TObject);
begin
  if opendialog1.Execute then
    CNmapWinter := opendialog1.filename;
  CNmapWinter := extractfilename(CNmapWinter);
  Edit44.Text := CNmapWinter;
end;

procedure TInput_kaarten_cn_form.Button29Click(Sender: TObject);
begin
  if opendialog1.Execute then
    PARCEL_filename2 := opendialog1.filename;
  PARCEL_filename2 := extractfilename(PARCEL_filename2);
  Edit34.Text := PARCEL_filename2;
end;

//The parcel file is selected = file with a unique ID for each field (like in WaTEM/SEDEM)
procedure TInput_kaarten_cn_form.Button2Click(Sender: TObject);
begin
  if opendialog1.Execute then
    PARCEL_filename1 := opendialog1.filename;
  PARCEL_filename1 := extractfilename(PARCEL_filename1);
  Edit2.Text := PARCEL_filename1;
end;

procedure TInput_kaarten_cn_form.Button30Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_spring := opendialog1.filename;
  Cf_Data_spring := extractfilename(Cf_Data_spring);
Edit48.Text := Cf_Data_spring;
end;

procedure TInput_kaarten_cn_form.Button31Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_summer := opendialog1.filename;
  Cf_Data_summer := extractfilename(Cf_Data_summer);
Edit50.Text := Cf_Data_summer;
end;

procedure TInput_kaarten_cn_form.Button32Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_fall := opendialog1.filename;
  Cf_Data_fall := extractfilename(Cf_Data_fall);
Edit52.Text := Cf_Data_fall;
end;

procedure TInput_kaarten_cn_form.Button33Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_winter := opendialog1.filename;
  Cf_Data_winter := extractfilename(Cf_Data_winter);
Edit57.Text := Cf_Data_winter;
end;

procedure TInput_kaarten_cn_form.Button34Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Ditch_filename := opendialog1.filename;
  Ditch_filename := extractfilename(Ditch_filename);
  Edit49.Text := Ditch_filename;
end;

procedure TInput_kaarten_cn_form.Button35Click(Sender: TObject);
begin
     if opendialog1.Execute then
    Dam_filename := opendialog1.filename;
  Dam_filename := extractfilename(Dam_filename);
  Edit51.Text := Dam_filename;
end;

procedure TInput_kaarten_cn_form.Button36Click(Sender: TObject);
begin
  if opendialog1.Execute then
      kTC_Data_Filename2 := opendialog1.filename;
    kTC_Data_Filename2 := extractfilename(kTC_Data_Filename2);
    Edit37.Text := kTC_Data_Filename2;
end;

procedure TInput_kaarten_cn_form.Button37Click(Sender: TObject);
begin
  if opendialog1.Execute then
      ktil_Data_Filename2 := opendialog1.filename;
    ktil_Data_Filename2 := extractfilename(ktil_Data_Filename2);
    Edit53.Text := ktil_Data_Filename2;
end;

procedure TInput_kaarten_cn_form.Button38Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_winter_2 := opendialog1.filename;
  Cf_Data_winter_2 := extractfilename(Cf_Data_winter_2);
Edit58.Text := Cf_Data_winter_2;
end;

procedure TInput_kaarten_cn_form.Button39Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_fall_2 := opendialog1.filename;
  Cf_Data_fall_2 := extractfilename(Cf_Data_fall_2);
Edit59.Text := Cf_Data_fall_2;
end;


procedure TInput_kaarten_cn_form.Button40Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_summer_2 := opendialog1.filename;
  Cf_Data_summer_2 := extractfilename(Cf_Data_summer_2);
Edit54.Text := Cf_Data_summer_2;
end;

procedure TInput_kaarten_cn_form.Button41Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Cf_Data_spring_2 := opendialog1.filename;
  Cf_Data_spring_2 := extractfilename(Cf_Data_spring_2);
Edit55.Text := Cf_Data_spring_2;
end;

procedure TInput_kaarten_cn_form.Button42Click(Sender: TObject);
begin
       if opendialog1.Execute then
    CNmapWinter_2 := opendialog1.filename;
  CNmapWinter_2 := extractfilename(CNmapWinter_2);
  Edit56.Text := CNmapWinter_2;
end;

procedure TInput_kaarten_cn_form.Button43Click(Sender: TObject);
begin
      if opendialog1.Execute then
    CNmapFall_2 := opendialog1.filename;
  CNmapFall_2 := extractfilename(CNmapFall_2);
  Edit60.Text := CNmapFall_2;
end;

procedure TInput_kaarten_cn_form.Button44Click(Sender: TObject);
begin
    if opendialog1.Execute then
    CNmapSummer_2 := opendialog1.filename;
  CNmapSummer_2 := extractfilename(CNmapSummer_2);
  Edit30.Text := CNmapSummer_2;
end;


procedure TInput_kaarten_cn_form.Button45Click(Sender: TObject);
begin
    if opendialog1.Execute then
    CNmapSpring_2 := opendialog1.filename;
  CNmapSpring_2 := extractfilename(CNmapSpring_2);
  Edit61.Text := CNmapSpring_2;
end;

//The Tillage direction file is selected
procedure TInput_kaarten_cn_form.Button5Click(Sender: TObject);
begin
  if opendialog1.Execute then
    TILDIRfilename := opendialog1.filename;
  TILDIRfilename := extractfilename(TILDIRfilename);
  Edit5.Text := TILDIRfilename;
end;

//The oriented roughness file is selected
procedure TInput_kaarten_cn_form.Button6Click(Sender: TObject);
begin
  if opendialog1.Execute then
    Rofilename := opendialog1.filename;
  Rofilename := extractfilename(Rofilename);
  Edit6.Text := Rofilename;
end;

procedure TInput_kaarten_cn_form.Button7Click(Sender: TObject);
begin
  if opendialog1.Execute then
    BufferFilename := opendialog1.filename;
  BufferFilename := extractfilename(BufferFilename);
  Edit7.Text := BufferFilename;
end;

//The CN map is selected
procedure TInput_kaarten_cn_form.Button8Click(Sender: TObject);
begin
  if opendialog1.Execute then
    CNmapSpring := opendialog1.filename;
  CNmapSpring := extractfilename(CNmapSpring);
  Edit8.Text := CNmapSpring;
end;

//The .txt file containing amount of rainfall (mm) per time step is selected
procedure TInput_kaarten_cn_form.Button9Click(Sender: TObject);
begin
  if opendialog2.Execute then
    Rainfallfilename := opendialog2.filename;
  Rainfallfilename := extractfilename(Rainfallfilename);
  Edit9.Text := Rainfallfilename;
end;

procedure TInput_kaarten_cn_form.CheckBox10Change(Sender: TObject);
begin
     if Checkbox10.Checked then
  begin
    Button16.Enabled := False;
    Edit31.Enabled := False;
    Edit31.Text := '';
    ktil_Data_Filename := '';
    Button37.Enabled := False;
    Edit53.Enabled := False;
    Edit53.Text := '';
    ktil_Data_Filename2 := '';
    Label22.Enabled := True;
    Edit32.Enabled := True;
    Create_ktil := True;
    Label24.Enabled:= True;
    Edit36.Enabled:= True;
    end
  else
  begin
    Button16.Enabled := True;
    Button37.Enabled := True;
    Edit53.Enabled := True;
    Label22.Enabled := False;
    Edit32.Enabled := False;
    Edit32.Text := '';
    ktil_Default := 0;
    Create_ktil := False;
    Label24.Enabled:= False;
    Edit36.Enabled:= False;
    Edit36.Text := '';
    ktil_threshold := 0;
  end;
end;

procedure TInput_kaarten_cn_form.CheckBox11Change(Sender: TObject);
begin
  if Checkbox11.Checked then
  begin
    Button18.Enabled := True;
    Include_sewer := True;
    Label26.Enabled := True;
    Edit64.Enabled := True;
    end
  else
  begin
   Button18.Enabled := False;
   Edit38.Enabled := False;
   Edit38.Text := '';
   Include_sewer := False;
   Label26.Enabled := False;
   Edit64.Enabled := False;
   Edit64.Text := '';
   Sewerfilename := '';
   sewer_exit := 0;
  end;
end;


procedure TInput_kaarten_cn_form.CheckBox15Change(Sender: TObject);
begin
  if Checkbox15.Checked then
  begin
    Include_ditch := True;
    Button34.Enabled := True;
  end
  else
  begin
    Include_ditch := False;
    Button34.Enabled := False;
    Edit49.Enabled := False;
    Edit49.Text := '';
    Ditch_filename := '';
  end;
end;

procedure TInput_kaarten_cn_form.CheckBox16Change(Sender: TObject);
begin
     if Checkbox16.Checked then
  begin
    Include_dam := True;
    Button35.Enabled := True;
  end
  else
  begin
    Include_dam := False;
    Button35.Enabled := False;
    Edit51.Enabled := False;
    Edit51.Text := '';
    Dam_filename := '';
  end;
end;

procedure TInput_kaarten_cn_form.CheckBox17Change(Sender: TObject);
begin
    if Checkbox17.Checked then
  begin
    Label21.Enabled := True;
    Edit63.Enabled := True;
    est_clay := True;
    end
  else
  begin
    Label21.Enabled := False;
    Edit63.Enabled := False;
    Edit63.Text := '';
    clay_parent := 0;
    est_clay := False;
  end;
end;

//The directory containing all input maps is selected
procedure TInput_kaarten_cn_form.DirectoryEdit1Change(Sender: TObject);
begin
  datadir := DirectoryEdit1.Directory;
  OpenDialog1.InitialDir := DirectoryEdit1.Directory;
end;

procedure TInput_kaarten_cn_form.DirectoryEdit2Change(Sender: TObject);
begin
  File_output_dir := DirectoryEdit2.Directory;
end;

//******************************************************************************
//The procedures below allow the user to provide information about buffers and
//store this information in a record
//******************************************************************************

//After the user specifies the number of buffers using the SpinEdit, the number
//of buffers and their names are added to the Combobox
procedure TInput_kaarten_cn_form.SpinEdit1EditingDone(Sender: TObject);
var
  i: integer;
  Buffername: string;
begin
  Setlength(BufferData, SpinEdit1.Value + 1);
  //The size of the record containing bufferdata is set
  ComboBox1.Clear;
  Number_of_Buffers := SpinEdit1.Value;
  for i := 1 to Number_of_Buffers do
  begin
    Buffername := 'Buffer ' + IntToStr(i);
    ComboBox1.Items.Add(Buffername);
  end;
  if Checkbox5.Checked then
    Buffer_default;
end;


//When the user selects a buffer from the ComboBox, the variables that have
//already been assigned to this buffer appear in the boxes
procedure TInput_kaarten_cn_form.ComboBox1Change(Sender: TObject);
begin
  if not simplified then
  begin
    Edit13.text := FloatToStr (Bufferdata[combobox1.Itemindex + 1].Height_opening);
    Edit10.text := FloatToStr (Bufferdata[Combobox1.ItemIndex + 1].Volume);
    Edit11.text := FloatToStr (Bufferdata[Combobox1.ItemIndex + 1].Height_dam);
    Edit12.Text := FloatToStr (Bufferdata[Combobox1.ItemIndex + 1].Opening_area);
    Edit14.Text := FloatToStr (Bufferdata[Combobox1.ItemIndex + 1].Cd);
    Edit23.Text := FloatToStr (Bufferdata[Combobox1.ItemIndex + 1].width_dam);
  end;
  Edit27.Text := FloatToStr (Bufferdata[Combobox1.ItemIndex + 1].PTEF);
  Edit29.Text := IntToStr (Bufferdata[Combobox1.ItemIndex + 1].ext_ID);
end;

procedure TInput_kaarten_cn_form.ComboBox2Change(Sender: TObject);
begin
  First_month := Combobox2.ItemIndex + 1;
end;


//The buffer variables are stored in the record once they are typed in the boxes
procedure TInput_kaarten_cn_form.Edit13EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit13.text, Bufferdata[Combobox1.ItemIndex + 1].Height_opening) then
   begin
     showmessage('This has to be a real number.');
     Edit13.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit10EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit10.text, Bufferdata[Combobox1.ItemIndex + 1].Volume) then
   begin
     showmessage('This has to be a real number.');
     Edit10.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit11EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit11.text, Bufferdata[Combobox1.ItemIndex + 1].Height_dam) then
   begin
     showmessage('This has to be a real number.');
     Edit11.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit12EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit12.text, Bufferdata[Combobox1.ItemIndex + 1].Opening_area) then
   begin
     showmessage('This has to be a real number.');
     Edit12.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit14EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit14.text, Bufferdata[Combobox1.ItemIndex + 1].Cd) then
   begin
     showmessage('This has to be a real number.');
     Edit14.text:='';
   end;
end;

//Bufferpart is ended
//******************************************************************************


procedure TInput_kaarten_cn_form.Edit19EditingDone(Sender: TObject);
begin
    if not TryStrToFloat(Edit19.text, alpha) then
   begin
     showmessage('This has to be a real number.');
     Edit19.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit20EditingDone(Sender: TObject);
begin
   if not TryStrToFloat(Edit20.text, beta) then
   begin
     showmessage('This has to be a real number.');
     Edit20.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit22EditingDone(Sender: TObject);
begin
  if not TryStrToInt(Edit22.text, BD) then
   begin
     showmessage('This has to be an integer.');
     Edit22.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit23EditingDone(Sender: TObject);
begin
    if not TryStrToFloat(Edit23.text, Bufferdata[Combobox1.ItemIndex + 1].width_dam) then
   begin
     showmessage('This has to be a real number.');
     Edit23.text:='';
   end;
end;


procedure TInput_kaarten_cn_form.Edit26EditingDone(Sender: TObject);
begin
   if not TryStrToInt(Edit26.text, year) then
   begin
     showmessage('This has to be an integer.');
     Edit26.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit27EditingDone(Sender: TObject);
begin
   if not TryStrToFloat(Edit27.text, Bufferdata[Combobox1.ItemIndex + 1].PTEF) then
  begin
    showmessage('This has to be a real number.');
    Edit27.text:='';
  end;
end;

procedure TInput_kaarten_cn_form.Edit29EditingDone(Sender: TObject);
begin
   if not TryStrToInt(Edit29.text, Bufferdata[Combobox1.ItemIndex + 1].ext_ID) then
  begin
    showmessage('This has to be an integer.');
    Edit29.text:='';
  end;
end;

procedure TInput_kaarten_cn_form.Edit30EditingDone(Sender: TObject);
begin
  if Edit30.Text = '' then
    CNmapSummer_2 := '';
end;


procedure TInput_kaarten_cn_form.Edit32EditingDone(Sender: TObject);
begin
if not TryStrToInt(Edit32.text, ktil_Default) then
   begin
     showmessage('This has to be an integer.');
     Edit32.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit34EditingDone(Sender: TObject);
begin
  if Edit34.Text = '' then
    PARCEL_filename2 := '';
end;

procedure TInput_kaarten_cn_form.Edit35EditingDone(Sender: TObject);
begin
    if not TryStrToInt(Edit35.text, TFSED_crop) then
   begin
     showmessage('This has to be an integer.');
     Edit35.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit36EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit36.text, ktil_threshold) then
   begin
     showmessage('This has to be a real number.');
     Edit36.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit37EditingDone(Sender: TObject);
begin
  if Edit37.Text = '' then
    kTC_Data_Filename2 := '';
end;

procedure TInput_kaarten_cn_form.Edit40EditingDone(Sender: TObject);
begin
  if not TryStrToInt(Edit40.Text, PTEFValueCropland) then
   begin
     showmessage('This has to be an integer.');
     Edit40.text := '';
   end;
end;

procedure TInput_kaarten_cn_form.Edit42EditingDone(Sender: TObject);
begin
   if not TryStrToInt(Edit42.Text, PTEFValuePasture) then
   begin
     showmessage('This has to be an integer.');
     Edit42.text := '';
   end;
end;

procedure TInput_kaarten_cn_form.Edit43EditingDone(Sender: TObject);
begin
    if not TryStrToInt(Edit43.Text, PTEFValueForest) then
   begin
     showmessage('This has to be an integer.');
     Edit43.text := '';
   end;
end;


procedure TInput_kaarten_cn_form.Edit47EditingDone(Sender: TObject);
begin
    if not TryStrToInt(Edit47.text, TFSED_forest) then
   begin
     showmessage('This has to be an integer.');
     Edit47.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit49EditingDone(Sender: TObject);
begin

end;

procedure TInput_kaarten_cn_form.Edit51EditingDone(Sender: TObject);
begin

end;


procedure TInput_kaarten_cn_form.Edit53EditingDone(Sender: TObject);
begin
  if Edit53.Text = '' then
    ktil_Data_Filename2 := '';
end;

procedure TInput_kaarten_cn_form.Edit54EditingDone(Sender: TObject);
begin
  if Edit54.Text = '' then
    Cf_Data_summer_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit55EditingDone(Sender: TObject);
begin
    if Edit55.Text = '' then
    Cf_Data_spring_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit56EditingDone(Sender: TObject);
begin
     if Edit56.Text = '' then
    CNmapWinter_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit58EditingDone(Sender: TObject);
begin
  if Edit58.Text = '' then
    Cf_Data_winter_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit59EditingDone(Sender: TObject);
begin
  if Edit59.Text = '' then
    Cf_Data_fall_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit60EditingDone(Sender: TObject);
begin
   if Edit60.Text = '' then
    CNmapFall_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit61EditingDone(Sender: TObject);
begin
    if Edit61.Text = '' then
    CNmapSpring_2 := '';
end;

procedure TInput_kaarten_cn_form.Edit62EditingDone(Sender: TObject);
begin
   if not TryStrToInt(Edit62.text, endtime_model) then
   begin
     showmessage('This has to be an integer.');
     Edit62.text:='';
   end;
end;

procedure TInput_kaarten_cn_form.Edit63EditingDone(Sender: TObject);
begin
  if not TryStrToFloat(Edit63.text, clay_parent) then
  begin
    showmessage('This has to be a real number.');
    Edit63.text:='';
  end;
end;

procedure TInput_kaarten_cn_form.Edit64EditingDone(Sender: TObject);
begin
   if not TryStrToInt(Edit64.Text, sewer_exit) then
   begin
     showmessage('This has to be an integer.');
     Edit64.text := '';
   end;
end;


//If the checkbox 'Include tillage direction?' is checked, a map with tillage direction
//and oriented roughness can be selected and tillage direction is taken into account
//during calculations
procedure TInput_kaarten_cn_form.CheckBox1Change(Sender: TObject);
begin
  if Checkbox1.Checked then
  begin
    Button5.Enabled := True; //TilDir
    Button6.Enabled := True; //Ro
    Topo := False;
    Inc_tillage := True;
  end
  else
  begin
    Button5.Enabled := False; //TilDir
    Edit5.Enabled := False;
    Edit5.Text := '';
    TILDIRfilename := '';
    Button6.Enabled := False; //Ro
    Edit6.Enabled := False;
    Edit6.Text := '';
    Rofilename := '';
    Topo := True;
    Inc_tillage := False;
  end;
end;

procedure TInput_kaarten_cn_form.CheckBox2Change(Sender: TObject);
begin
   if Checkbox2.Checked then
  begin
    Button17.Enabled := False;
    Button36.Enabled := False;
    Edit33.Enabled := False;
    Edit37.Enabled := False;
    Edit33.Text := '';
    Edit37.Text := '';
    ktc_Data_Filename:='';
    ktc_Data_Filename2:='';
    Label27.Enabled := True;
    Edit3.Enabled := True;
    Edit3.Text := '7';
    Create_ktc := True;
    Label28.Enabled:= True;
    Edit4.Enabled:= True;
    Edit4.Text := '21';
    Label29.Enabled := True;
    Edit39.Enabled := True;
    Edit39.Text := '0.1';
    end
  else
  begin
    Button17.Enabled := True;
    Button36.Enabled := True;
    Label27.Enabled := False;
    Edit3.Enabled := False;
    Edit3.Text := '';
    ktc_low := 0;
    Create_ktc := False;
    Label28.Enabled:= False;
    Edit4.Enabled:= False;
    Edit4.Text := '';
    ktc_high := 0;
    Label29.Enabled := False;
    Edit39.Enabled := False;
    Edit39.Text := '';
    ktc_limit := 0;
  end;
end;

//If the checkbox 'Include Buffers' is checked, the input boxes appear
procedure TInput_kaarten_cn_form.CheckBox3Change(Sender: TObject);
begin
  if Checkbox3.Checked then
  begin
    Button7.Enabled := True;
    SpinEdit1.Enabled := True;
    ComboBox1.Enabled := True;
    Label1.Enabled := True;
    Label13.Enabled := True;
    Label14.Enabled := True;
    Edit27.Enabled := True;
    Edit29.Enabled := True;
    Include_buffer := True;
    Checkbox5.Enabled := True;
    if not simplified then
    begin
      Label2.Enabled := True;
      Label3.Enabled := True;
      Label4.Enabled := True;
      Label5.Enabled := True;
      Label9.Enabled := True;
      Label6.Enabled := True;
      Edit10.Enabled := True;
      Edit11.Enabled := True;
      Edit12.Enabled := True;
      Edit13.Enabled := True;
      Edit14.Enabled := True;
      Edit23.Enabled := True;
    end;
  end
  else
  begin
    Button7.Enabled := False;
    Edit7.Enabled := False;
    Edit7.Text := '';
    Bufferfilename := '';
    SpinEdit1.Enabled := False;
    Number_of_Buffers := 0;
    ComboBox1.Enabled := False;
    Label1.Enabled := False;
    Label2.Enabled := False;
    Label3.Enabled := False;
    Label4.Enabled := False;
    Label5.Enabled := False;
    Label13.Enabled := False;
    Label14.Enabled := False;
    Edit10.Enabled := False;
    Edit11.Enabled := False;
    Edit12.Enabled := False;
    Edit13.Enabled := False;
    Label6.Enabled := False;
    Edit14.Enabled := False;
    Label9.Enabled := False;
    Edit23.Enabled := False;
    Edit27.Enabled := False;
    Edit29.Enabled := False;
    Include_buffer := False;
    Checkbox5.Enabled := False;
  end;
end;

// option to calculate the output per VHA river segment
procedure TInput_kaarten_cn_form.CheckBox4Change(Sender: TObject);
begin
  if CheckBox4.Checked then
  begin
    VHA := True;
    Button12.Enabled := True;
  end
  else
  begin
    VHA := False;
    Button12.Enabled := False;
    Edit21.Text := '';
    riversegment_filename := '';
    Edit21.Enabled := False;
  end;
end;

procedure TInput_kaarten_cn_form.CheckBox5Change(Sender: TObject);
var
  n: integer;
begin
  if Checkbox5.Checked then
    buffer_default
  else
  begin
    for n:=1 to Number_of_Buffers do
    begin
      if not simplified then
      begin
       Bufferdata[n].Volume := 0;
       Bufferdata[n].height_dam := 0;
       Bufferdata[n].height_opening := 0;
       Bufferdata[n].Opening_area := 0;
       Bufferdata[n].width_dam := 0;
       Bufferdata[n].Cd := 0;
      end;
       Bufferdata[n].PTEF := 0;
       Bufferdata[n].ext_ID:=0;
    end;
  end;
end;


// option to read all input parameters from existing .ini file
procedure TInput_kaarten_cn_form.CheckBox6Change(Sender: TObject);
begin
  if Checkbox6.Checked then
  begin
    PageControl1.Enabled := False;
    Button10.Enabled := True;
    Ini_select := True;
  end
  else
  begin
    PageControl1.Enabled := True;
    Button10.Enabled := False;
    Edit15.Enabled := False;
    Ini_select := False;
  end;
end;

// option to calculate output for user defined outlet(s)
procedure TInput_kaarten_cn_form.CheckBox7Change(Sender: TObject);
begin
  if Checkbox7.Checked then
  begin
     Button11.Enabled := False;
     Edit16.Enabled := False;
     Edit16.Text := '';
     Outletfilename := '';
     Label16.Enabled := False;
     Outlet_select := False;
  end
  else
  begin
      Button11.Enabled := True;
      Label16.Enabled := True;
      Outlet_select := True;
  end;
end;

procedure TInput_kaarten_cn_form.CheckBox8Change(Sender: TObject);
begin
  if Checkbox8.Checked then
    begin
      File_output_dir := datadir;
      DirectoryEdit2.Directory := File_output_dir;
      DirectoryEdit2.Enabled := False;
    end
  else
     DirectoryEdit2.Enabled := True;
end;

procedure TInput_kaarten_cn_form.CheckBox9Change(Sender: TObject);
begin
if Checkbox9.Checked then
  begin
    SpinEdit3.Enabled := True;
    Convert_output := True;
    end
  else
  begin
    SpinEdit3.Enabled := False;
    Convert_output := False;
    Timestep_output := 0;
  end;
end;

procedure TInput_kaarten_cn_form.CheckGroup1ItemClick(Sender: TObject;
  Index: integer);
begin
  if Index = 0 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_ASPECT := True else Write_ASPECT := False
  end;
  if Index = 1 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_LS := True else Write_LS := False
  end;
  if Index = 2 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_RE := True else Write_RE := False
  end;
  if Index = 3 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_RUSLE := True else Write_RUSLE := False
  end;
  if Index = 4 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_Sediexport := True else Write_Sediexport := False
  end;
  if Index = 5 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_SLOPE := True else Write_SLOPE := False
  end;
  if Index = 6 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_TILEROS := True else Write_TILEROS := False
  end;
  if Index = 7 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_TOTRUN := True else Write_TOTRUN := False
  end;
  if Index = 8 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_UPAREA := True else Write_UPAREA := False
  end;
  if Index = 9 then
  begin
      if CheckGroup1.Checked[Index] then
      Write_WATEREROS := True else Write_WATEREROS := False
  end;
end;

//******************************************************************************
//In the lines below the file locations of the selected maps and other input data
// are written to a .ini file, when clicking 'OK'
//******************************************************************************
procedure TInput_kaarten_cn_form.BitBtn1Click(Sender: TObject);
var
  Buffername: string;
  i:integer;
begin
  Close; // The 'Input' window is closed
  if datadir = '' then //When no data directory is defined
  begin
    ShowMessage('Input data has not been saved, please specify data path.');
    exit;
  end;

  PTEFValueCropland := strtoint(Input_kaarten_cn_form.edit40.Text);
  PTEFValuePasture := strtoint(Input_kaarten_cn_form.edit42.Text);
  PTEFValueForest := strtoint(Input_kaarten_cn_form.edit43.Text);
  TFSED_crop :=  strtoint(Input_kaarten_cn_form.edit35.Text);
  TFSED_forest := strtoint(Input_kaarten_cn_form.edit47.Text);

  if not Ini_select then
  begin
    datadir := datadir+'\';
    File_output_dir := File_output_dir+'\';
    INIfilename := datadir + 'CN_WS_longterm.ini';
  end;

  if not Include_sewer then
     sewer_exit := 0;

  if Include_buffer then
    Number_of_Buffers:=SpinEdit1.Value
  else
    Number_of_Buffers:=0;

   if not create_ktil then
  begin
    ktil_default := 0;
    ktil_threshold := 0;
  end
   else
   begin
    ktil_default := strtoint(Input_kaarten_cn_form.Edit32.Text);
    ktil_threshold := strtofloat(Input_kaarten_cn_form.Edit36.Text);
   end;

   if not create_ktc then
   begin
     ktc_low := 0;
     ktc_high := 0;
     ktc_limit := 0;
   end
   else
   begin
    ktc_low := strtoint(Input_kaarten_cn_form.Edit3.Text);
    ktc_high := strtoint(Input_kaarten_cn_form.Edit4.Text);
    ktc_limit := strtofloat(Input_kaarten_cn_form.Edit39.Text);
   end;

   if not est_clay then
     clay_parent := 0;

  Timestep_model:=SpinEdit2.Value;


  if convert_output then
    Timestep_output:=SpinEdit3.Value
  else
    Timestep_output:=0;

  if simplified then
    begin
      Timestep_model := 0;
      Endtime_model := 0;
      Timestep_output := 0;
    end;

  if checkbox1.checked = true then
     topo := false
     else topo := true;

  if checkbox7.checked = true then
    Outlet_select := false
    else Outlet_select := true;

    // if simplified version of model is run, all output maps related to runoff are not produced
if Simplified then
  begin
    Write_RE := False;
    Write_TOTRUN:= False;
  end;

  Write_RE_tot := Write_RE;
  Write_Sediexport_tot := Write_Sediexport;
  Write_TOTRUN_tot := Write_TOTRUN;
  Write_WATEREROS_tot := Write_WATEREROS;
  Write_RUSLE_tot := Write_RUSLE;

  // new .ini file is created (or the existing one is overwritten) and filled

  SetCurrentDir(datadir);
  inifile := Tinifile.Create(INIfilename); //The .ini file is created

  //Writing a line: ('Section', 'Key','Value')
  inifile.WriteString('Working directories', 'Input directory', dataDir);
  inifile.WriteString('Working directories', 'Output directory', File_output_dir);
  inifile.writestring('Files', '.INI filename', INIfilename);
  inifile.writestring('Files', 'Model .exe file', ModelExe);
  inifile.writestring('Files', 'DTM filename', DTM_filename);
  inifile.writestring('Files', 'Parcel filename 1', PARCEL_filename1);
  inifile.writestring('Files', 'Parcel filename 2', PARCEL_filename2);
  inifile.writestring('Files', 'Rainfall filename', Rainfallfilename);
  inifile.writestring('Files', 'Stream velocity filename', RivVelFilename);
  inifile.writestring('Files', 'Sewer map filename', Sewerfilename);
  inifile.writestring('Files', 'Tillage direction filename', TILDIRfilename);
  inifile.writestring('Files', 'Oriented roughness filename', Rofilename);
  inifile.writestring('Files', 'CN map spring', CNmapSpring);
  inifile.writestring('Files', 'CN map spring 2', CNmapSpring_2);
  inifile.writestring('Files', 'CN map summer', CNmapSummer);
  inifile.writestring('Files', 'CN table summer 2', CNmapSummer_2);
  inifile.writestring('Files', 'CN map fall', CNmapFall);
  inifile.writestring('Files', 'CN map fall 2', CNmapFall_2);
  inifile.writestring('Files', 'CN map winter', CNmapWinter);
  inifile.writestring('Files', 'CN map winter 2', CNmapWinter_2);
  inifile.Writestring('Files', 'K factor filename', K_Factor_filename);
  inifile.Writestring('Files', 'P factor map filename', Pf_Data_Filename);
  inifile.Writestring('Files', 'kTC map filename', kTC_Data_Filename);
  inifile.Writestring('Files', 'kTC map filename 2', kTC_Data_Filename2);
  inifile.Writestring('Files', 'ktil map filename', ktil_Data_Filename);
  inifile.Writestring('Files', 'ktil map filename 2', ktil_Data_Filename2);
  inifile.Writestring('Files', 'C factor map spring', Cf_Data_spring);
  inifile.Writestring('Files', 'C factor map spring 2', Cf_Data_spring_2);
  inifile.Writestring('Files', 'C factor map summer', Cf_Data_summer);
  inifile.Writestring('Files', 'C factor map summer 2', Cf_Data_summer_2);
  inifile.Writestring('Files', 'C factor map fall', Cf_Data_fall);
  inifile.Writestring('Files', 'C factor map fall 2', Cf_Data_fall_2);
  inifile.Writestring('Files', 'C factor map winter', Cf_Data_winter);
  inifile.Writestring('Files', 'C factor map winter 2', Cf_Data_winter_2);
  inifile.Writestring('Files', 'Buffer map filename', BufferFilename);
  inifile.Writestring('Files', 'Ditch map filename', Ditch_filename);
  inifile.Writestring('Files', 'Dam map filename', Dam_filename);
  inifile.Writestring('Files', 'Outlet map filename', Outletfilename);
  inifile.Writestring('Files', 'River segment filename', riversegment_filename);

  inifile.WriteBool('User Choices', 'Simplified model version', Simplified);
  inifile.WriteBool('User Choices', 'Include sewers', Include_sewer);
  inifile.WriteBool('User Choices', 'Include tillage', Inc_tillage);
  inifile.WriteBool('User Choices', 'Create ktil map', Create_ktil);
  inifile.WriteBool('User Choices', 'Estimate clay content', est_clay);
  inifile.WriteBool('User Choices', 'Include buffers', Include_buffer);
  inifile.WriteBool('User Choices', 'Include ditches', Include_ditch);
  inifile.WriteBool('User Choices', 'Include dams', Include_dam);
  inifile.WriteBool('User Choices', 'Manual outlet selection', Outlet_select);
  inifile.WriteBool('User Choices', 'Convert output', Convert_output);
  inifile.WriteBool('User Choices', 'Output per VHA river segment', VHA);

  inifile.WriteBool('Output maps', 'Write aspect', Write_ASPECT);
  inifile.WriteBool('Output maps', 'Write LS factor', Write_LS);
  inifile.WriteBool('Output maps', 'Write rainfall excess', Write_RE);
  inifile.WriteBool('Output maps', 'Write RUSLE', Write_RUSLE);
  inifile.WriteBool('Output maps', 'Write sediment export', Write_Sediexport);
  inifile.WriteBool('Output maps', 'Write slope', Write_SLOPE);
  inifile.WriteBool('Output maps', 'Write tillage erosion', Write_TILEROS);
  inifile.WriteBool('Output maps', 'Write total runoff', Write_TOTRUN);
  inifile.WriteBool('Output maps', 'Write upstream area', Write_UPAREA);
  inifile.WriteBool('Output maps', 'Write water erosion', Write_WATEREROS);

  Inifile.Writestring('Variables', 'Bulk density', IntToStr(BD));
  Inifile.Writestring('Variables', 'First month', IntToStr(First_month));
  Inifile.Writestring('Variables', 'Starting year', IntToStr(year));
  Inifile.Writestring('Variables', 'Sewer exit', IntToStr(sewer_exit));
  Inifile.writestring('Variables', 'Alpha', FloatToStr(alpha));
  Inifile.writestring('Variables', 'Beta', FloatToStr(beta));
  Inifile.Writestring('Variables', 'Number of buffers', IntToStr(Number_of_Buffers));
  Inifile.writestring('Variables', 'ktc low', IntToStr(ktc_low));
  Inifile.writestring('Variables', 'ktc high', IntToStr(ktc_high));
  Inifile.writestring('Variables', 'ktc limit', FloatToStr(ktc_limit));
  Inifile.writestring('Variables', 'ktil default', IntToStr(ktil_Default));
  Inifile.writestring('Variables', 'ktil threshold', FloatToStr(ktil_threshold));
  Inifile.writestring('Variables', 'Clay content parent material', FloatToStr(clay_parent));
  Inifile.writestring('Variables', 'Parcel connectivity cropland', IntToStr(TFSED_crop));
  Inifile.writestring('Variables', 'Parcel connectivity forest', IntToStr(TFSED_forest));
  Inifile.Writestring('Variables', 'Desired timestep for model', IntToStr(Timestep_model));
  Inifile.Writestring('Variables', 'Endtime model', IntToStr(Endtime_model));
  Inifile.Writestring('Variables', 'Final timestep output', IntToStr(Timestep_output));
  Inifile.Writestring('Variables', 'Parcel trapping efficiency cropland', IntToStr(PTEFValueCropland));
  Inifile.Writestring('Variables', 'Parcel trapping efficiency forest', IntToStr(PTEFValueForest));
  Inifile.Writestring('Variables', 'Parcel trapping efficiency pasture', IntToStr(PTEFValuePasture));

  if (Include_buffer) then
  begin
  for i := 1 to Number_of_Buffers do
  begin
    Buffername := 'Buffer ' + IntToStr(i);
    inifile.Writestring(Buffername, 'Volume', floattostr(Bufferdata[i].Volume));
    inifile.Writestring(Buffername, 'Height dam', floattostr(Bufferdata[i].Height_dam));
    inifile.Writestring(Buffername, 'Height opening', floattostr(Bufferdata[i].Height_opening));
    inifile.Writestring(Buffername, 'Opening area', floattostr(Bufferdata[i].Opening_area));
    inifile.Writestring(Buffername, 'Discharge coefficient', floattostr(Bufferdata[i].Cd));
    inifile.Writestring(Buffername, 'Width dam', floattostr(Bufferdata[i].width_dam));
    inifile.Writestring(Buffername, 'Trapping efficiency', floattostr(Bufferdata[i].PTEF));
    inifile.Writestring(Buffername, 'Extension ID', Inttostr(Bufferdata[i].ext_ID));
    if Bufferdata[i].Height_opening > Bufferdata[i].Height_dam then
    begin
    showmessage('The height of the opening can not be larger than the height of the dam. Please insert correct vallues.');
    close;
    end;
  end;
  end;

  inifile.Destroy; // Het inifile wordt vrijgegeven

  //******************************************************************************
  //When one of the necessary input maps/variables is missing an error is diplayed.
  //This happens when clicking 'OK' in the 'Input' window.
  //******************************************************************************
  {Tabsheet 1}

  if not (FileExists(ModelExe)) then
    ShowMessage('Model .exe file not found');
  SetcurrentDir(datadir);
  if not (FileExists(DTM_filename)) then
    ShowMessage('DTM file' + ' not found in ' + datadir);
  if not (FileExists(PARCEL_filename1)) then
    ShowMessage('Parcel file 1' + ' not found in ' + datadir);
  if not (FileExists(Rainfallfilename)) then
    ShowMessage('Rainfall file' + ' not found in ' + datadir);
  if Edit22.Text = '' then
    ShowMessage('Missing value: Bulk density');
  if Include_sewer = True then
  begin
    if not (FileExists(Sewerfilename)) then
      ShowMessage('Sewer file' + ' not found in ' + datadir);
    if  Edit64.Text = '' then
      ShowMessage('Missing value: Percentage of sewer input exiting system');
  end;
  if First_month = 0 then
    ShowMessage('Please select the first month of the simulation');
  if Edit26.Text = '' then
    ShowMessage('Missing value: Starting year of simulation');

  {Tabsheet 2}
  if Inc_tillage = True then //If tillage direction is taken into account
  begin
    if not (FileExists(TilDirfilename)) then
      ShowMessage('Tillage direction file' + ' not found in ' + datadir);
    if not (FileExists(Rofilename)) then
      ShowMessage('Ro (oriented rougness) file' + ' not found in ' + datadir);
  end;

    if Edit35.Text = '' then
    ShowMessage('Missing value: Parcel connectivity to cropland');
  if Edit47.Text = '' then
    ShowMessage('Missing value: Parcel connectivity to forest/pasture');

  {Tabsheet 3}

  if Edit40.Text = '' then
    ShowMessage('Missing value: Parcel trapping efficiency of cropland');
  if Edit42.Text = '' then
    ShowMessage('Missing value: Parcel trapping efficiency of pasture');
  if Edit43.Text = '' then
    ShowMessage('Missing value: Parcel trapping efficiency of forest');

  if not (FileExists(K_Factor_Filename)) then
      ShowMessage('K factor map' + ' not found in ' + datadir);
  if not (FileExists(Pf_Data_Filename)) then
    ShowMessage('P factor map' + ' not found in ' + datadir);
  if not (create_ktc) AND not (FileExists(kTC_Data_Filename)) then
    ShowMessage('kTC map year 1' + ' not found in ' + datadir);
  if (Create_ktc) AND (Edit3.Text = '') then
    ShowMessage('Missing value: kTc low');
  if (Create_ktc) AND (Edit4.Text = '') then
    ShowMessage('Missing value: kTc high');
  if (Create_ktc) AND (Edit39.Text = '') then
    ShowMessage('Missing value: kTc limit');
  if not (Create_ktil) AND not (FileExists(ktil_Data_Filename)) then
    ShowMessage('ktil map year 1' + ' not found in ' + datadir);
  if (Create_ktil) AND (Edit32.Text = '') then
    ShowMessage('Missing value: Default ktil');
   if (Create_ktil) AND (Edit36.Text = '') then
    ShowMessage('Missing value: Threshold ktil');

   if (est_clay) AND (Edit63.Text = '') then
    ShowMessage('Missing value: Clay content parent material');

    if not (FileExists(Cf_Data_spring)) then
    ShowMessage('C factor map spring' + ' not found in ' + datadir);

  if not (FileExists(Cf_Data_summer)) then
    ShowMessage('C factor map summer' + ' not found in ' + datadir);

if not (FileExists(Cf_Data_fall)) then
ShowMessage('C factor map fall' + ' not found in ' + datadir);

if not (FileExists(Cf_Data_winter)) then
ShowMessage('C factor map winter' + ' not found in ' + datadir);

  {Tabsheet 4}

    if Include_buffer = True then //If buffers are taken into account
  begin
    if not (FileExists(Bufferfilename)) then
      ShowMessage('Buffer map' + ' not found in ' + datadir);
  end;

    if Include_ditch = True then
  begin
    if not (FileExists(Ditch_filename)) then
      ShowMessage('Ditch map' + ' not found in ' + datadir);
  end;

  if Include_dam = True then
  begin
    if not (FileExists(Dam_filename)) then
      ShowMessage('Dam map' + ' not found in ' + datadir);
  end;

  {Tabsheet 5}

  if Outlet_select = true then
  begin
    if not (FileExists(Outletfilename)) then
      ShowMessage('Outlet location map' + ' not found in ' + datadir);
  end;
  if VHA = true then
  begin
    if not (FileExists(Riversegment_filename)) then
      ShowMessage('River segment map' + ' not found in ' + datadir);
  end;

  {Only in case of detailed model version}
  if not Simplified then
  begin
   if not (FileExists(RivVelFilename)) then
    ShowMessage('Stream velocity file' + ' not found in ' + datadir);

   if Edit19.Text = '' then
    ShowMessage('Missing value: alpha');
   if Edit20.Text = '' then
    ShowMessage('Missing value: beta');


    if not (FileExists(CNMapSpring)) then
          ShowMessage('CN map spring' + ' not found in ' + datadir);


        if not (FileExists(CNMapSummer)) then
          ShowMessage('CN map summer' + ' not found in ' + datadir);


        if not (FileExists(CNMapFall)) then
          ShowMessage('CN map fall' + ' not found in ' + datadir);


        if not (FileExists(CNMapWinter)) then
          ShowMessage('CN map winter' + ' not found in ' + datadir);

     if Edit62.Text = '' then
    ShowMessage('Missing value: end time of model run');
     end;
//******************************************************************************
//For each buffer it is checked whether height of dam is larger than height of opening
//******************************************************************************

if not Simplified then
begin
if Include_buffer then
  begin
    for i := 1 to Number_of_Buffers do
    begin
      if Bufferdata[i].Height_opening > Bufferdata[i].Height_dam then
        begin
        showmessage('Error in buffer input: the height of the opening cannot be larger than the height of the dam. Please insert correct vallues.');
        close;
        end;
      end;
    end;
end;
end;

//***************************************************************************
//  This procedure fills in the input window from an existing .ini file
//***************************************************************************

procedure autoFill(var Input_kaarten_cn_form:TInput_kaarten_cn_form);
var
  Buffername: string;
  i: integer;
begin
  {Tabsheet 1}
  Input_kaarten_cn_form.Checkbox6.Checked := True;
  Input_kaarten_cn_form.Button10.Enabled := True;
  Input_kaarten_cn_form.Edit15.Text := Extractfilename(INIfilename);
  Input_kaarten_cn_form.Pagecontrol1.Enabled := True;

  Input_kaarten_cn_form.DirectoryEdit1.Directory := datadir;
  Input_kaarten_cn_form.Opendialog1.InitialDir := datadir;
  Input_kaarten_cn_form.DirectoryEdit2.Directory := File_output_dir;
  Input_kaarten_cn_form.edit18.Text := ModelExe;
  Input_kaarten_cn_form.edit1.Text := Extractfilename(DTM_filename);
  Input_kaarten_cn_form.edit2.Text := Extractfilename(PARCEL_filename1);
  Input_kaarten_cn_form.edit34.Text := Extractfilename(PARCEL_filename2);
  Input_kaarten_cn_form.edit9.Text := Extractfilename(Rainfallfilename);
  Input_kaarten_cn_form.edit22.Text := IntToStr(BD);
  if not Simplified then
    Input_kaarten_cn_form.edit24.Text := Extractfilename(RivVelFilename);

  if Include_sewer then
  begin
      Input_kaarten_cn_form.checkbox11.Checked := True;
      Input_kaarten_cn_form.edit38.Text := Extractfilename(Sewerfilename);
      Input_kaarten_cn_form.edit64.Text := IntToStr(sewer_exit);
  end;


  case First_month of
  1: Input_kaarten_cn_form.ComboBox2.Text:='January';
  2: Input_kaarten_cn_form.ComboBox2.Text:='February';
  3: Input_kaarten_cn_form.ComboBox2.Text:='March';
  4: Input_kaarten_cn_form.ComboBox2.Text:='April';
  5: Input_kaarten_cn_form.ComboBox2.Text:='May';
  6: Input_kaarten_cn_form.ComboBox2.Text:='June';
  7: Input_kaarten_cn_form.ComboBox2.Text:='July';
  8: Input_kaarten_cn_form.ComboBox2.Text:='August';
  9: Input_kaarten_cn_form.ComboBox2.Text:='September';
  10: Input_kaarten_cn_form.ComboBox2.Text:='October';
  11: Input_kaarten_cn_form.ComboBox2.Text:='November';
  12: Input_kaarten_cn_form.ComboBox2.Text:='December';
  end;
  Input_kaarten_cn_form.edit26.Text := IntToStr(year);

  {Tabsheet 2}
  if not Simplified then
  begin
    Input_kaarten_cn_form.edit19.Text := FloatToStr(alpha);
    Input_kaarten_cn_form.edit20.Text := FloatToStr(beta);
  end;

  if Inc_tillage then
  begin
     Input_kaarten_cn_form.Checkbox1.Checked := True;
     Input_kaarten_cn_form.edit5.Text := Extractfilename(TilDirfilename);
     Input_kaarten_cn_form.edit6.Text := Extractfilename(Rofilename);
  end
   else
   begin
     Input_kaarten_cn_form.checkbox1.Checked := False;
   end;

    Input_kaarten_cn_form.edit35.Text := IntToStr(TFSED_crop);
    Input_kaarten_cn_form.edit47.Text := IntToStr(TFSED_forest);

  if not simplified then
  begin
      Input_kaarten_cn_form.edit8.Text := Extractfilename(CNmapSpring);

    Input_kaarten_cn_form.edit61.Text := Extractfilename(CNmapSpring_2);
    Input_kaarten_cn_form.edit17.Text := Extractfilename(CNmapSummer);
    Input_kaarten_cn_form.edit30.Text := Extractfilename(CNmapSummer_2);
    Input_kaarten_cn_form.edit41.Text := Extractfilename(CNmapFall);
    Input_kaarten_cn_form.edit60.Text := Extractfilename(CNmapFall_2);
    Input_kaarten_cn_form.edit44.Text := Extractfilename(CNmapWinter);
    Input_kaarten_cn_form.edit56.Text := Extractfilename(CNmapWinter_2);
  end;

  {Tabsheet 3}
  Input_kaarten_cn_form.edit25.Text := Extractfilename(K_Factor_Filename);
  Input_kaarten_cn_form.edit48.Text := Extractfilename(Cf_Data_spring);
  Input_kaarten_cn_form.edit50.Text := Extractfilename(Cf_Data_summer);
  Input_kaarten_cn_form.edit52.Text := Extractfilename(Cf_Data_fall);
  Input_kaarten_cn_form.edit57.Text := Extractfilename(Cf_Data_winter);
  Input_kaarten_cn_form.edit55.Text := Extractfilename(Cf_Data_spring_2);
  Input_kaarten_cn_form.edit54.Text := Extractfilename(Cf_Data_summer_2);
  Input_kaarten_cn_form.edit59.Text := Extractfilename(Cf_Data_fall_2);
  Input_kaarten_cn_form.edit58.Text := Extractfilename(Cf_Data_winter_2);
  Input_kaarten_cn_form.edit28.Text := Extractfilename(Pf_Data_Filename);

  if Create_ktc then
  begin
      Input_kaarten_cn_form.checkbox2.Checked := True;
      Input_kaarten_cn_form.label27.Enabled := True;
      Input_kaarten_cn_form.edit3.Enabled := True;
      Input_kaarten_cn_form.label28.Enabled := True;
      Input_kaarten_cn_form.edit4.Enabled := True;
      Input_kaarten_cn_form.label29.Enabled := True;
      Input_kaarten_cn_form.edit39.Enabled := True;
      Input_kaarten_cn_form.button17.Enabled := False;
      Input_kaarten_cn_form.button36.Enabled := False;
      Input_kaarten_cn_form.edit33.Enabled := False;
      Input_kaarten_cn_form.edit33.Text := '';
      ktc_Data_Filename := '';
      Input_kaarten_cn_form.edit37.Enabled := False;
      Input_kaarten_cn_form.edit37.Text := '';
      ktc_Data_Filename2 := '';
      Input_kaarten_cn_form.edit3.Text := IntToStr(ktc_low);
      Input_kaarten_cn_form.edit4.Text := IntToStr(ktc_high);
      Input_kaarten_cn_form.edit39.Text := FloatToStr(ktc_limit);
    end
    else
    begin
      Input_kaarten_cn_form.edit33.Text := Extractfilename(ktc_Data_Filename);
      Input_kaarten_cn_form.edit37.Text := Extractfilename(ktc_Data_Filename2);
    end;

  Input_kaarten_cn_form.edit3.Text := IntToStr(ktc_low);
  Input_kaarten_cn_form.edit4.Text := IntToStr(ktc_high);
  Input_kaarten_cn_form.edit39.Text := FloatToStr(ktc_limit);

  if Create_ktil then
    begin
      Input_kaarten_cn_form.checkbox10.Checked := True;
      Input_kaarten_cn_form.label22.Enabled := True;
      Input_kaarten_cn_form.edit32.Enabled := True;
      Input_kaarten_cn_form.label24.Enabled := True;
      Input_kaarten_cn_form.edit36.Enabled := True;
      Input_kaarten_cn_form.button16.Enabled := False;
      Input_kaarten_cn_form.edit31.Enabled := False;
      Input_kaarten_cn_form.edit31.Text := '';
      Input_kaarten_cn_form.edit53.Enabled := False;
      Input_kaarten_cn_form.edit53.Text := '';
      ktil_Data_Filename := '';
      Input_kaarten_cn_form.edit32.Text := IntToStr(ktil_Default);
      Input_kaarten_cn_form.edit36.Text := FloatToStr(ktil_threshold);
    end
    else
    begin
      Input_kaarten_cn_form.edit31.Text := Extractfilename(ktil_Data_Filename);
      Input_kaarten_cn_form.edit53.Text := Extractfilename(ktil_Data_Filename2);
    end;

    if est_clay then
     begin
       Input_kaarten_cn_form.checkbox17.Checked := True;
       Input_kaarten_cn_form.label21.Enabled := True;
       Input_kaarten_cn_form.edit63.Enabled := True;
       Input_kaarten_cn_form.edit63.Text := FloatToStr(clay_parent);
     end;

  Input_kaarten_cn_form.edit40.Text := IntToStr(PTEFValueCropland);
  Input_kaarten_cn_form.edit42.Text := IntToStr(PTEFValuePasture);
  Input_kaarten_cn_form.edit43.Text := IntToStr(PTEFValueForest);

  {Tabsheet 4}


  if Include_Buffer then
  begin
      Input_kaarten_cn_form.checkbox3.Checked := True;
      Input_kaarten_cn_form.edit7.Text := Extractfilename(BufferFilename);
      Input_kaarten_cn_form.SpinEdit1.Value := Number_of_Buffers;
      for i := 1 to Number_of_Buffers do
       begin
         Buffername := 'Buffer ' + IntToStr(i);
         Input_kaarten_cn_form.ComboBox1.Items.Add(Buffername);
       end;
  end
  else
  begin
      Input_kaarten_cn_form.checkbox3.Checked := False;
  end;

  if Include_ditch then
  begin
      Input_kaarten_cn_form.checkbox15.Checked := True;
      Input_kaarten_cn_form.edit49.Text := Extractfilename(Ditch_filename);
  end
  else
      Input_kaarten_cn_form.checkbox15.Checked := False;

  if Include_dam then
  begin
      Input_kaarten_cn_form.checkbox16.Checked := True;
      Input_kaarten_cn_form.edit51.Text := Extractfilename(Dam_filename);
  end
  else
      Input_kaarten_cn_form.checkbox16.Checked := False;

 {Tabsheet 5}
  if not Outlet_select then
  begin
      Input_kaarten_cn_form.checkbox7.Checked := True;
  end
    else
  begin
      Input_kaarten_cn_form.checkbox7.Checked := False;
      Input_kaarten_cn_form.Edit16.Text := Extractfilename(Outletfilename);
  end;

  if not simplified then
  begin

  Input_kaarten_cn_form.SpinEdit2.Value := Timestep_model;
  Input_kaarten_cn_form.Edit62.Text := IntToStr(EndTime_model);

  if Convert_output then
  begin
    Input_kaarten_cn_form.checkbox9.Checked := True;
    Input_kaarten_cn_form.SpinEdit3.Value := Timestep_output;
  end
  else
    Input_kaarten_cn_form.checkbox9.Checked := False;

  if Write_RE then
    Input_kaarten_cn_form.CheckGroup1.Checked[2] := True;
  if Write_TOTRUN then
    Input_kaarten_cn_form.CheckGroup1.Checked[7] := True;
 end;

  if VHA then
  begin
    Input_kaarten_cn_form.checkbox4.Checked := True;
    Input_kaarten_cn_form.Edit21.Text := Extractfilename(riversegment_filename);
  end
  else
    Input_kaarten_cn_form.checkbox4.Checked := False;

  if Write_ASPECT then
    Input_kaarten_cn_form.CheckGroup1.Checked[0] := True;
  if Write_LS then
    Input_kaarten_cn_form.CheckGroup1.Checked[1] := True;
  if Write_RUSLE then
    Input_kaarten_cn_form.CheckGroup1.Checked[3] := True;
  if Write_Sediexport then
    Input_kaarten_cn_form.CheckGroup1.Checked[4] := True;
  if Write_SLOPE then
    Input_kaarten_cn_form.CheckGroup1.Checked[5] := True;
  if Write_TILEROS then
    Input_kaarten_cn_form.CheckGroup1.Checked[6] := True;
  if Write_UPAREA then
    Input_kaarten_cn_form.CheckGroup1.Checked[8] := True;
  if Write_WATEREROS then
    Input_kaarten_cn_form.CheckGroup1.Checked[9] := True;

end;

procedure buffer_default;
var
  n:integer;
begin
  for n:=1 to Number_of_Buffers do
  begin
    if not simplified then
    begin
     Bufferdata[n].Volume := 1000;
     Bufferdata[n].height_dam := 1.5;
     Bufferdata[n].height_opening := 0;
     Bufferdata[n].Opening_area := 0.03;
     Bufferdata[n].width_dam := 7;
     Bufferdata[n].Cd := 0.6;
    end;
     Bufferdata[n].PTEF := 75;
     Bufferdata[n].ext_ID:=n*100;
    end;
  end;

end.
