{*******************************************************************************
  Sample.Form.Main
********************************************************************************
  Main Application Form demonstrating the usage of TSkFlowmotion.
  Features Demonstrated:
  - Initialization and configuration of the FlowMotion Gallery.
  - Loading images.
  - Dynamic layout changes and styling (Colors, Fonts, Borders).
  - Inter-Process Communication (WM_COPYDATA) for external control.
  - Parameter-based loading for standalone/slave execution modes.
  part of skFlowmotion
  written by: Lara Miriam Tamy Reschke
*******************************************************************************}
unit Sample.Form.Main;

interface

uses
  { Delphi RTL }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  System.IOUtils, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, ShellAPI, FMX.Layouts, FMX.StdCtrls, System.Skia, FMX.Skia,
  FMX.ImgList, FMX.ListBox, FMX.Colors, FMX.EditBox, FMX.SpinBox, Windows,
  Messages,
  { SkFlow Components }
  uSkFlowmotion, uSkFlowEffects, uSkFlowButtons, System.ImageList, FMX.Edit,
  FMX.Controls.Presentation;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    { --- Visual Components --- }
    saiAnimatedLogo: TSkAnimatedImage;
    fanFadeOutTransition: TFloatAnimation;
    lytContent: TLayout;
    lytcontrols: TLayout;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    CheckBox1: TCheckBox;
    Rectangle1: TRectangle;
    Button9: TButton;
    Button10: TButton;
    Panel1: TPanel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Button8: TButton;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Timer1: TTimer;
    ColorPicker1: TColorPicker;
    rbselfontcol: TRadioButton;
    rbfontcol: TRadioButton;
    rbselcapback: TRadioButton;
    rbcapback: TRadioButton;
    rbglowcol: TRadioButton;
    rbhotcol: TRadioButton;
    rbrotatedot: TRadioButton;
    CheckBox9: TCheckBox;
    SpinBox1: TSpinBox;
    rbAnimspeed: TRadioButton;
    rbGlowwidth: TRadioButton;
    rbHotwidth: TRadioButton;
    rbFontsize: TRadioButton;
    rbCaptionAlpha: TRadioButton;
    rbCaptionYOffset: TRadioButton;
    rbPagesize: TRadioButton;
    rbstartingAngle: TRadioButton;
    rbRoundEdges: TRadioButton;
    Panel2: TPanel;
    rbrotatedothot: TRadioButton;
    rbrotatedotdown: TRadioButton;
    CheckBox12: TCheckBox;
    rbrotateall: TRadioButton;
    CheckBox13: TCheckBox;
    rbtechbracketwidth: TRadioButton;
    Button11: TButton;
    rbRotateAllBy: TRadioButton;
    rbparticle: TRadioButton;
    Rectangle2: TRectangle;
    Button12: TButton;
    rbmaxinternal: TRadioButton;
    rbhotalpha: TRadioButton;
    rbalpha: TRadioButton;
    rbselectedalpha: TRadioButton;
    Button13: TButton;
    rbrotatesize: TRadioButton;
    rbsmlpcmrg: TRadioButton;
    rbmaxzoom: TRadioButton;
    Layout1: TLayout;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label4: TLabel;
    ComboBox4: TComboBox;
    rbhotzoom: TRadioButton;
    Label5: TLabel;
    ComboBox5: TComboBox;
    CheckBox16: TCheckBox;
    ComboBox6: TComboBox;
    CheckBox2: TCheckBox;
    rbifoarrow: TRadioButton;
    Panel3: TPanel;
    rbinfhot: TRadioButton;
    Button1: TButton;
    Button2: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Label6: TLabel;
    CheckBox19: TCheckBox;
    CheckBox18: TCheckBox;
    rbborder: TRadioButton;
    Button19: TButton;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    Button20: TButton;
    Timer2: TTimer;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox5: TCheckBox;
    rbfps: TRadioButton;
    Button14: TButton;
    Timer3: TTimer;
    procedure FormDestroy(Sender: TObject);
    { --- Event Handlers --- }
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox10Change(Sender: TObject);
    procedure CheckBox11Change(Sender: TObject);
    procedure CheckBox12Change(Sender: TObject);
    procedure CheckBox13Change(Sender: TObject);
    procedure CheckBox14Change(Sender: TObject);
    procedure CheckBox15Change(Sender: TObject);
    procedure CheckBox16Change(Sender: TObject);
    procedure CheckBox17Change(Sender: TObject);
    procedure CheckBox18Change(Sender: TObject);
    procedure CheckBox19Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox20Change(Sender: TObject);
    procedure CheckBox21Change(Sender: TObject);
    procedure CheckBox22Change(Sender: TObject);
    procedure CheckBox23Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure CheckBox9Change(Sender: TObject);
    procedure ColorPicker1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure fanFadeOutTransitionFinish(Sender: TObject);
    procedure saiAnimatedLogoAnimationFinished(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lytcontrolsResize(Sender: TObject);
    procedure rbPagesizeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Rectangle1DblClick(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
  private
    { Private declarations }
    Loadedwithparams: Boolean;
    ParamsTxtFile: string;
    FStreamFiles: TStringList;
    FStreamindex: Integer;
    { Components }
    FTestPanel: TFlowButtonPanel;
    skfmFlowGallery: TSkFlowmotion;
    { Initialization & Setup }
    procedure InitGallery;
    { TFlowButtonPanel Events }
    procedure OnTestButtonClick(Sender: TObject; Button: TFlowButton);
    { TSkFlowmotion Events }
    procedure Flowmotion1SelectedImageDblClick(Sender: TObject; ImageItem: TImageItem; Index: Integer);
    procedure Flowmotion1SelectedImageEnterZone(Sender: TObject; ImageItem: TImageItem; const ZoneName: string);
    procedure skfmFlowGalleryMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    { IPC (Inter-Process Communication) }
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    procedure SendSignalToPlayer(const FilePath: string);
    procedure SendNextToPlayer(const FilePath: string);
    { Utility }
    procedure LoadFromTxtFile(const TxtFilePath: string);
    function GetFirstImageInFolder(const Folder: string): string;
    procedure Onfullscreen(Sender: TObject; ImageItem: TImageItem; Index: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
{$R *.fmx}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FStreamFiles.Free;
end;

{ *******************************************************************************
  EVENT HANDLERS: BUTTON CLICKS
  ******************************************************************************* }

procedure TfrmMain.Button10Click(Sender: TObject);
begin
  { Clear gallery with specific animation targeting Panel1 }
  skfmFlowGallery.Clear(true, true, Panel1.BoundsRect.Round, Panel1.BoundsRect.Round, iesFromPoint);
end;

procedure TfrmMain.Button11Click(Sender: TObject);
begin
  { Set window to transparent overlay mode }
  skfmFlowGallery.SetBackgroundpicture('');
  Transparency := true;
  Borderstyle := TFmxFormBorderStyle.None;
  Canvas.Clear($00000000);
  skfmFlowGallery.BackgroundColor := TAlphaColors.Null;
end;

procedure TfrmMain.Button12Click(Sender: TObject);
begin
  { Trigger Zoom animation for the currently selected image }
  skfmFlowGallery.ZoomSelectedToFull;
end;

procedure TfrmMain.Button13Click(Sender: TObject);
begin
  { Show the Info Panel (Side panel with text) for selected image }
  if skfmFlowGallery.SelectedImage <> nil then
    skfmFlowGallery.ShowInfoPanel(skfmFlowGallery.SelectedImage);
end;

procedure TfrmMain.Button14Click(Sender: TObject);
begin
  if Timer3.Enabled then
  begin
    Timer3.Enabled := False;
    skfmFlowGallery.ExternalStreamImage := nil;
  end
  else

  { 1. Select the files }
    if OpenDialog1.Execute then
  begin
    if (skfmFlowGallery.SelectedImage = nil) then
      skfmFlowGallery.SelectNextImage;

    { 2. Fill our list }
    FStreamFiles.Clear;
    FStreamFiles.AddStrings(OpenDialog1.Files);

    { 3. Reset counter }
    FStreamindex := 0;

    { 4. Start Timer3 }
    Timer3.Enabled := True;
  end;
end;

procedure TfrmMain.Button15Click(Sender: TObject);
begin
  { Spatial Navigation: West }
  skfmFlowGallery.SelectWest;
end;

procedure TfrmMain.Button16Click(Sender: TObject);
begin
  { Spatial Navigation: East }
  skfmFlowGallery.SelectEast;
end;

procedure TfrmMain.Button17Click(Sender: TObject);
begin
  { Spatial Navigation: North }
  skfmFlowGallery.SelectNorth;
end;

procedure TfrmMain.Button18Click(Sender: TObject);
begin
  { Spatial Navigation: South }
  skfmFlowGallery.SelectSouth;
end;

procedure TfrmMain.Button19Click(Sender: TObject);
begin
  { Creates and displays a Radial Button Panel demonstration }
  // 1. Create Panel
  FTestPanel := TFlowButtonPanel.Create(Self);
  FTestPanel.Parent := Self; // IMPORTANT: Must be parented to Form!
  FTestPanel.SetBounds(0, 0, 400, 400);
  FTestPanel.Visible := False;
  // 2. Style Configuration
  FTestPanel.Layout := plRadial;       // Circle Layout
  FTestPanel.PanelStyle := psGlass;      // Glass background
  FTestPanel.PanelRotation := 0;
  FTestPanel.ButtonSize := 60;
  FTestPanel.Gap := 10;
  FTestPanel.Radius := 100;             // Distance from center
  // 3. Hook up Event
  FTestPanel.OnButtonClick := OnTestButtonClick;
  // 4. Add Buttons (Note: Replace paths with actual images for full effect)
  // For demonstration, we try to load 'btntest.png' from app directory
  FTestPanel.AddButton('Play', 'Start Playing', 'CMD_PLAY', TSkImage.MakeFromEncodedFile(ExtractFilePath(ParamStr(0)) + 'btntest.png'));
  FTestPanel.AddButton('Stop', 'Stop Playing', 'CMD_STOP', TSkImage.MakeFromEncodedFile(ExtractFilePath(ParamStr(0)) + 'btntest.png'));
  FTestPanel.AddButton('Exit', 'Close App', 'CMD_EXIT', TSkImage.MakeFromEncodedFile(ExtractFilePath(ParamStr(0)) + 'btntest.png'));
  FTestPanel.ShowAt(500, 500);
end;
{ *******************************************************************************
  EVENT HANDLERS: UI CONTROLS (CHECKBOXES, COMBOBOXES, SPINBOX)
  ******************************************************************************* }

procedure TfrmMain.CheckBox10Change(Sender: TObject);
begin
  skfmFlowGallery.HotTrackZoom := CheckBox10.isChecked;
end;

procedure TfrmMain.CheckBox11Change(Sender: TObject);
begin
  skfmFlowGallery.BreathingEnabled := CheckBox11.isChecked;
end;

procedure TfrmMain.CheckBox12Change(Sender: TObject);
begin
  skfmFlowGallery.ZoomSelectedtoCenter := Checkbox12.IsChecked;
end;

procedure TfrmMain.CheckBox13Change(Sender: TObject);
begin
  skfmFlowGallery.KeepSpaceforZoomed := CheckBox13.IsChecked;
end;

procedure TfrmMain.CheckBox14Change(Sender: TObject);
begin
  skfmFlowGallery.BreathRotationEnabled := CheckBox14.isChecked;
end;

procedure TfrmMain.CheckBox15Change(Sender: TObject);
begin
  skfmFlowGallery.HoverAlive := Checkbox15.IsChecked;
end;

procedure TfrmMain.CheckBox16Change(Sender: TObject);
begin
  skfmFlowGallery.ShowInfoIndicator := Checkbox16.IsChecked;
end;

procedure TfrmMain.CheckBox17Change(Sender: TObject);
begin
  skfmFlowGallery.HoverAliveOnFullscreen := Checkbox17.IsChecked;
end;

procedure TfrmMain.CheckBox18Change(Sender: TObject);
begin
  skfmFlowGallery.AlwaysShowInfo := CheckBox18.isChecked;
end;

procedure TfrmMain.CheckBox19Change(Sender: TObject);
begin
  skfmFlowGallery.DeleteClicked := CheckBox19.isChecked;
end;

procedure TfrmMain.CheckBox1Change(Sender: TObject);
begin
  { Toggle Picture Border Style }
  if not Checkbox1.IsChecked then
    skfmFlowGallery.PictureBorderType := btFull
  else
    skfmFlowGallery.PictureBorderType := btTech;
end;

procedure TfrmMain.CheckBox2Change(Sender: TObject);
begin
  skfmFlowGallery.AnimatedBackground := Checkbox2.IsChecked;
end;

procedure TfrmMain.CheckBox3Change(Sender: TObject);
begin
  skfmFlowGallery.SelectedMovable := CheckBox3.IsChecked;
end;

procedure TfrmMain.CheckBox4Change(Sender: TObject);
begin
  { Toggle Random Rotation vs Zero Rotation }
  if CheckBox4.IsChecked then
    skfmFlowGallery.StartingAngle := -1
  else
    skfmFlowGallery.StartingAngle := 0;
end;

procedure TfrmMain.CheckBox5Change(Sender: TObject);
begin
  skfmFlowGallery.RotationAllowed := CheckBox5.IsChecked;
end;

procedure TfrmMain.CheckBox6Change(Sender: TObject);
begin
  skfmFlowGallery.SmallPicVisible := CheckBox6.IsChecked;
end;

procedure TfrmMain.CheckBox7Change(Sender: TObject);
begin
  skfmFlowGallery.ShowCaptions := CheckBox7.IsChecked;
end;

procedure TfrmMain.CheckBox8Change(Sender: TObject);
begin
  skfmFlowGallery.CaptionOnHoverOnly := CheckBox8.IsChecked;
end;

procedure TfrmMain.CheckBox9Change(Sender: TObject);
begin
  skfmFlowGallery.ShowSmallPicOnlyOnHover := CheckBox9.IsChecked;
end;

procedure TfrmMain.ComboBox1Change(Sender: TObject);
begin
  { Change Layout Strategy }
  case Combobox1.ItemIndex of
    0:
      skfmFlowGallery.SetFlowLayout(flSorted);
    1:
      skfmFlowGallery.SetFlowLayout(flPlaceholder);
  end;
end;

procedure TfrmMain.ComboBox2Change(Sender: TObject);
begin
  { Change Info Panel Animation Style }
  case Combobox2.ItemIndex of
    0:
      skfmFlowGallery.InfoPanelAnimationStyle := iasBlurEdge;
    1:
      skfmFlowGallery.InfoPanelAnimationStyle := iasStatic;
    2:
      skfmFlowGallery.InfoPanelAnimationStyle := iasTransparent;
  end;
end;

procedure TfrmMain.ComboBox3Change(Sender: TObject);
begin
  { Change Fullscreen Rotation Mode }
  case Combobox3.ItemIndex of
    0:
      skfmFlowGallery.FullscreenAngle := fsa0;
    1:
      skfmFlowGallery.FullscreenAngle := fsa90;
    2:
      skfmFlowGallery.FullscreenAngle := fsa180;
    3:
      skfmFlowGallery.FullscreenAngle := fsa270;
  end;
end;

procedure TfrmMain.ComboBox4Change(Sender: TObject);
begin
  { Change Info Panel Position }
  case Combobox4.ItemIndex of
    0:
      skfmFlowGallery.InfoPanelDirection := ipdAuto;
    1:
      skfmFlowGallery.InfoPanelDirection := ipdTop;
    2:
      skfmFlowGallery.InfoPanelDirection := ipdBottom;
    3:
      skfmFlowGallery.InfoPanelDirection := ipdLeft;
    4:
      skfmFlowGallery.InfoPanelDirection := ipdRight;
  end;
end;

procedure TfrmMain.ComboBox5Change(Sender: TObject);
begin
  { Change Info Panel Width Percentage }
  case ComboBox5.ItemIndex of
    0:
      skfmFlowGallery.InfoPanelWidthPercent := 30 / 100;
    1:
      skfmFlowGallery.InfoPanelWidthPercent := 50 / 100;
    2:
      skfmFlowGallery.InfoPanelWidthPercent := 75 / 100;
  end;
end;

procedure TfrmMain.ComboBox6Change(Sender: TObject);
begin
  { Change Background Effect }
  case Combobox6.ItemIndex of
    0:
      skfmFlowGallery.BackgroundEffect := beRealMatrix;
    1:
      skfmFlowGallery.BackgroundEffect := beHolographic;
    2:
      skfmFlowGallery.BackgroundEffect := beFade;
  end;
end;

procedure TfrmMain.SpinBox1Change(Sender: TObject);
begin
  { Generic handler for multiple settings using Radio Buttons to select target }
  if not Assigned(skfmFlowGallery) then
    Exit;
  if rbAnimspeed.IsChecked then
    skfmFlowGallery.AnimationSpeed := Round(SpinBox1.Value)
  else if rbGlowwidth.IsChecked then
    skfmFlowGallery.GlowWidth := Round(SpinBox1.Value)
  else if rbHotwidth.IsChecked then
    skfmFlowGallery.HotTrackWidth := Round(SpinBox1.Value)
  else if rbFontsize.IsChecked then
    skfmFlowGallery.CaptionFont.Size := SpinBox1.Value
  else if rbCaptionAlpha.IsChecked then
    skfmFlowGallery.CaptionAlpha := Round(SpinBox1.Value)
  else if rbCaptionYOffset.IsChecked then
    skfmFlowGallery.CaptionOffsetY := Round(SpinBox1.Value)
  else if rbPagesize.IsChecked then
    skfmFlowGallery.PageSize := Round(SpinBox1.Value)
  else if rbstartingAngle.IsChecked then
    skfmFlowGallery.StartingAngle := Round(SpinBox1.Value)
  else if rbRoundEdges.IsChecked then
    skfmFlowGallery.RoundEdges := Round(SpinBox1.Value)
  else if rbRotateall.IsChecked then
    skfmFlowGallery.PutAllToAngle(SpinBox1.Value)
  else if rbtechbracketwidth.IsChecked then
    skfmFlowGallery.TechBracketWidth := trunc(SpinBox1.Value)
  else if rbRotateAllBy.IsChecked then
    skfmFlowGallery.RotateAllBy(SpinBox1.Value)
  else if rbmaxinternal.IsChecked then
    skfmFlowGallery.MaxInternalPicSize := Trunc(SpinBox1.Value)
  else if rbselectedalpha.IsChecked then
    skfmFlowGallery.AlphaHotSelected := Trunc(SpinBox1.Value)
  else if rbhotalpha.IsChecked then
    skfmFlowGallery.AlphaHotPhase := Trunc(SpinBox1.Value)
  else if rbalpha.IsChecked then
    skfmFlowGallery.AlphaStatic := Trunc(SpinBox1.Value)
  else if rbrotatesize.IsChecked then
    skfmFlowGallery.RotateHandleSize := Trunc(SpinBox1.Value)
  else if rbsmlpcmrg.IsChecked then
    skfmFlowGallery.SmallpicMargin := Trunc(SpinBox1.Value)
  else if rbmaxzoom.IsChecked then
    skfmFlowGallery.MaxZoomSize := Trunc(SpinBox1.Value)
  else if rbhotzoom.IsChecked then
    skfmFlowGallery.HotZoomMaxFactor := Trunc(SpinBox1.Value)
  else if rbfps.IsChecked then
    skfmFlowGallery.TargetFPS := Trunc(SpinBox1.Value);
end;

procedure TfrmMain.ColorPicker1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  { Applies the selected color to the property determined by the checked Radio Button }
  if not Assigned(skfmFlowGallery) then
    Exit;
  if rbselfontcol.IsChecked then
    skfmFlowGallery.SelectedCaptionColor := ColorPicker1.Color
  else if rbfontcol.IsChecked then
    skfmFlowGallery.CaptionColor := ColorPicker1.Color
  else if rbselcapback.IsChecked then
    skfmFlowGallery.SelectedCaptionBackground := ColorPicker1.Color
  else if rbcapback.IsChecked then
    skfmFlowGallery.CaptionBackground := ColorPicker1.Color
  else if rbglowcol.IsChecked then
    skfmFlowGallery.GlowColor := ColorPicker1.Color
  else if rbhotcol.IsChecked then
    skfmFlowGallery.HotTrackColor := ColorPicker1.Color
  else if rbrotatedot.IsChecked then
    skfmFlowGallery.RotateDotColor := ColorPicker1.Color
  else if rbrotatedothot.IsChecked then
    skfmFlowGallery.RotateDotHotColor := ColorPicker1.Color
  else if rbrotatedotDown.IsChecked then
    skfmFlowGallery.RotateDotDownColor := ColorPicker1.Color
  else if rbparticle.IsChecked then
    skfmFlowGallery.ParticleColor := ColorPicker1.Color
  else if rbifoarrow.IsChecked then
    skfmFlowGallery.InfoIndicatorColor := ColorPicker1.Color
  else if rbinfhot.IsChecked then
    skfmFlowGallery.InfoIndicatorHotColor := ColorPicker1.Color
  else if rbborder.IsChecked then
    skfmFlowGallery.ItemBorderColor := ColorPicker1.Color;
end;
{ *******************************************************************************
  EVENT HANDLERS: FORM LIFECYCLE
  ******************************************************************************* }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { Initial state setup }
  lytcontrols.Visible := False;
  Layout1.Visible := False;
  LoadedWithParams := False;
  FStreamFiles := TStringList.Create;
  FStreamindex := 0;
  { Check if started with parameters (Slave Mode) }
  if ParamCount >= 1 then
  begin
    borderstyle := TFmxFormBorderStyle.None;
    ParamsTxtFile := ParamStr(1);
    LoadedWithParams := True;
    saiAnimatedLogo.Visible := False;
    lytContent.Visible := True;
  end
  else
  begin
    { Normal Mode (Show Intro) }
    saiAnimatedLogo.Visible := True;
    saiAnimatedLogo.BringToFront;
    lytContent.Visible := False;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  { Start Timer to initialize gallery after form is fully shown }
  if Loadedwithparams then
    Timer1.Enabled := True;
end;

procedure TfrmMain.saiAnimatedLogoAnimationFinished(Sender: TObject);
begin
  { Called when intro animation ends }
  saiAnimatedLogo.Visible := False;
  ;
  lytcontrols.Visible := not Loadedwithparams;
  Layout1.Visible := not Loadedwithparams;
  lytContent.Visible := True;
  Fill.Color := TAlphaColors.Black;
  fanFadeOutTransition.Enabled := True;
  Timer1.Enabled := True;
end;

procedure TfrmMain.fanFadeOutTransitionFinish(Sender: TObject);
begin
  saiAnimatedLogo.Visible := False;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  { Initialize with either list file or default demo data }
  if LoadedWithParams then
    LoadFromTxtFile(ParamsTxtFile)
  else
    InitGallery;
end;

procedure TfrmMain.lytcontrolsResize(Sender: TObject);
begin
  { Adjust Max Zoom Size when control panel resizes }
  if visible and assigned(skfmFlowGallery) then
    skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 3);
end;

procedure TfrmMain.rbPagesizeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  { Enforce minimum page size to prevent errors }
  if SpinBox1.Value < 10 then
    SpinBox1.Value := 10;
end;

procedure TfrmMain.Rectangle1DblClick(Sender: TObject);
begin
  { Debug output }
  ShowMessage(inttostr(skfmFlowGallery.ImageCount));
end;
{ *******************************************************************************
  EVENT HANDLERS: CUSTOM CALLBACKS
  ******************************************************************************* }

procedure TfrmMain.OnTestButtonClick(Sender: TObject; Button: TFlowButton);
begin
  { Demonstrates TFlowButtonPanel event firing }
  ShowMessage('You clicked button with ID: ' + Button.TagString);
  FTestPanel.PanelHide;
end;

procedure TfrmMain.Flowmotion1SelectedImageEnterZone(Sender: TObject; ImageItem: TImageItem; const ZoneName: string);
begin
  { Demonstrates Activation Zones }
  if ZoneName = 'ActivationZone 1' then
    ShowMessage('Image entered ActivationZone 1! ');
end;

procedure TfrmMain.Onfullscreen(Sender: TObject; ImageItem: TImageItem; Index: Integer);
begin
  ShowMessage('Fullscreen arrived');
end;

procedure TfrmMain.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  { Map Mouse Wheel to Image Selection }
  if WheelDelta > 0 then
    skfmFlowGallery.SelectPreviousImage
  else if WheelDelta < 0 then
    skfmFlowGallery.SelectNextImage;
  Handled := True;
end;

procedure TfrmMain.Flowmotion1SelectedImageDblClick(Sender: TObject; ImageItem: TImageItem; Index: Integer);
var
  FolderPath: string;
begin
  if (ImageItem = nil) or (ImageItem.Path = '') then
    Exit;
  FolderPath := ImageItem.Path;
  { If launched as a slave, send signal back to Master }
  if Loadedwithparams then
  begin
    SendSignaltoPlayer(FolderPath);
    Exit;
  end;
  { Demo Logic: If path is dummy, just Zoom }
  if (FolderPath = '') or (FolderPath = 'Folder or whatever') then
  begin
    skfmFlowGallery.ZoomSelectedToFull;
    Exit;
  end;
  { If valid folder, open Explorer }
  if not DirectoryExists(FolderPath) then
    FolderPath := ExtractFilePath(FolderPath);
  if DirectoryExists(FolderPath) then
  begin
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,' + FolderPath), nil, 1);
    skfmFlowGallery.DeselectZoomedImage;
  end
  else
    ShowMessage('folder not found: ' + FolderPath);
end;
{ *******************************************************************************
  NAVIGATION & CONTROL
  ******************************************************************************* }

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  skfmFlowGallery.SelectPreviousImage;
end;

procedure TfrmMain.Button20Click(Sender: TObject);
begin
  Timer2.Enabled := not Timer2.Enabled;
  if TImer2.Enabled then
  begin
    Button20.text := 'Show Mode on';
    skfmFlowGallery.SelectNextImage;
  end
  else
    Button20.text := 'Show Mode off';
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  skfmFlowGallery.SelectNextImage;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  skfmFlowGallery.DeselectZoomedImage;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  { Adds a single random image from AppDir using 'Point' entry animation }
  skfmFlowGallery.ImageEntryStyle := iesFromPoint;
  skfmFlowGallery.EntryPoint := Point(Round(Panel3.Position.X), Round(Panel3.Position.Y));
  skfmFlowGallery.AddImageAsync(ExtractFilePath(ParamStr(0)) + inttostr(random(13) + 1) + '.jpg');
end;             //AddImage AddImageAsync

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  skfmFlowGallery.Clear(true);
end;

procedure TfrmMain.Button6Click(Sender: TObject);
var
  AppDir: string;
  i: Integer;
  IMList, Pathlist, Captionlist, Hintlist, InfosList: TStringList;
begin
  { Adds a batch of demo images with metadata }
  AppDir := ExtractFilePath(ParamStr(0));
  IMList := TStringList.create;
  Pathlist := TStringList.create;
  Captionlist := TStringList.create;
  Hintlist := TStringList.create;
  InfosList := TStringList.create;
  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 3);
  try
    { Create two loops of demo data }
    for i := 14 downto 0 do
    begin
      IMList.add(AppDir + inttostr(i) + '.jpg');
      Pathlist.add('Folder or whatever');
      Captionlist.add('Caption');
      Hintlist.Add('Hint');
      InfosList.Add('Movie infos or whatever you want to show here |.......... ......... ........... bit more to see if it works right... ...');
    end;
    for i := 0 to 14 do
    begin
      IMList.add(AppDir + inttostr(i) + '.jpg');
      Pathlist.add('Folder or whatever');
      Captionlist.add('Caption');
      Hintlist.Add('Hint');
      InfosList.Add('Movie infos or whatever you want to show here | ........... ......... ........... bit more to see if it works right.... ....');
    end;
    skfmFlowGallery.AddImagesAsync(IMList, Captionlist, Pathlist, Hintlist, InfosList);
  finally          //AddImages AddImagesAsync
    InfosList.Free;
    IMList.Free;
    Pathlist.Free;
    Captionlist.Free;
    Hintlist.Free;
  end;
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  { Set background image }
  if Opendialog1.Execute then
    skfmFlowGallery.SetBackgroundpicture(Opendialog1.FileName);
end;

procedure TfrmMain.Button8Click(Sender: TObject);
begin
  { Reset all image rotations to 0 }
  skfmFlowGallery.ResetAllRotations;
end;

procedure TfrmMain.Button9Click(Sender: TObject);
var
  i: Integer;
begin
  { 1. SET ENTRY POINT (Where images fly from) }
  skfmFlowGallery.EntryPoint := Point(Round(Panel3.Position.X), Round(Panel3.Position.Y));
  skfmFlowGallery.ImageEntryStyle := iesFromPoint;
  { 2. SHOW DIALOG (Ensure TOpenDialog1 has Options: [ofAllowMultiSelect]) }
  if OpenDialog1.Execute then
  begin
    { 3. ITERATE AND ADD IMAGES }
    if OpenDialog1.Files.Count > 0 then
    begin
      for i := 0 to OpenDialog1.Files.Count - 1 do
      begin
        skfmFlowGallery.AddImageAsync(OpenDialog1.Files[i]);
      end;
    end;
  end;
end;

procedure TfrmMain.CheckBox20Change(Sender: TObject);
begin
  skfmFlowGallery.ShowInfoIndicatoralways := CheckBox20.IsChecked;
end;

procedure TfrmMain.CheckBox21Change(Sender: TObject);
begin
  skfmFlowGallery.FreeFloat := CheckBox21.IsChecked;
end;

procedure TfrmMain.CheckBox22Change(Sender: TObject);
begin
  skfmFlowGallery.InfoIndicatorOnlyOnHover := CheckBox22.IsChecked;
end;

procedure TfrmMain.CheckBox23Change(Sender: TObject);
begin
  skfmFlowGallery.MitchellQuality := CheckBox23.IsChecked;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if visible and assigned(skfmFlowGallery) then
    skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 3);
end;

procedure TfrmMain.skfmFlowGalleryMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Item: TImageItem;
begin
  { Middle Click sends file to external player }
  if Button = TMouseButton.mbMiddle then
  begin
    Item := skfmFlowGallery.GetImageAtPoint(X, Y);
    if (Item <> nil) and (Item.Path <> '') then
    begin
      SendNextToPlayer(Item.Path);
      skfmFlowGallery.DeselectZoomedImage;
    end;
  end;
end;
{ *******************************************************************************
  INITIALIZATION
  ******************************************************************************* }

procedure TfrmMain.InitGallery;
var
  AppDir: string;
  FileName: string;
  i, rndX: Integer;
  IMList, Pathlist, Captionlist, Hintlist, Infoslist: TStringList;
  smallimgindex: TList;
begin
  AppDir := ExtractFilePath(ParamStr(0));
  { Ensure Gallery is created }
  if skfmFlowGallery = nil then
  begin
    skfmFlowGallery := TSkFlowmotion.Create(Self);
    skfmFlowGallery.Parent := lytContent;
    skfmFlowGallery.Align := TAlignLayout.Client;
  end;
  { Apply Basic Styles }
  skfmFlowGallery.BackgroundColor := TAlphaColors.Black;
  skfmFlowGallery.FlowLayout := TFlowLayout.flSorted;
  skfmFlowGallery.AnimationSpeed := 2;
  skfmFlowGallery.OnSelectedImageDblClick := Flowmotion1SelectedImageDblClick;
  skfmFlowGallery.Spacing := 15;
  skfmFlowGallery.PageSize := 80;
  skfmFlowGallery.OnMouseUp := skfmFlowGalleryMouseUp;
  skfmFlowGallery.ShowCaptions := True;
  skfmFlowGallery.SelectedMovable := True;
  skfmFlowGallery.KeepSpaceforZoomed := True;
  skfmFlowGallery.ShowHint := true;
  skfmFlowGallery.SmallPicVisible := True;
  skfmFlowGallery.SmallPicImageList := Imagelist1;
  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 3);
  skfmFlowGallery.OnSelectedImageEnterZone := Flowmotion1SelectedImageEnterZone;
  skfmFlowGallery.AddActivationZone('ActivationZone 1', Panel1.BoundsRect);
  skfmFlowGallery.OnFullscreenEnter := Onfullscreen;
  skfmFlowGallery.CaptionFont.Size := 14;
  skfmFlowGallery.CaptionFont.Family := 'Segoe UI';
  skfmFlowGallery.HotTrackZoom := True;
  FileName := AppDir + 'back.jpg';
  skfmFlowGallery.SetBackgroundpicture(FileName);
  skfmFlowGallery.Visible := True;
  skfmFlowGallery.BringToFront;
  { Populate Demo Data Lists }
  IMList := TStringList.create;
  Pathlist := TStringList.create;
  Captionlist := TStringList.create;
  Hintlist := TStringList.create;
  smallimgindex := TList.Create;
  Infoslist := TStringList.create;
  try
    for i := 0 to 14 do
    begin
      IMList.add(AppDir + inttostr(i) + '.jpg');
      Pathlist.add('Folder or whatever');
      Captionlist.add('Caption');
      Hintlist.Add('Hint');
      Infoslist.Add('Movie infos or whatever you want to show here |pipe for new line | |...........  bit more to see if it works right....  ....');
    end;
    for i := 14 downto 0 do
    begin
      rndX := random(13) + 1;
      IMList.add(ExtractFilePath(ParamStr(0)) + inttostr(rndX) + '.jpg');
      Pathlist.add('Folder or whatever');
      Captionlist.add('Caption');
      Hintlist.Add('Hint');
      Infoslist.Add('Movie infos or whatever you want to show here || pipe for new line |  ...........  |bit more to see if it works right.... ....');
      smallimgindex.Add(Pointer(rndX));
    end;
    skfmFlowGallery.AddImages(IMList, Captionlist, Pathlist, Hintlist, Infoslist, smallimgindex);
  finally
    Infoslist.Free;
    IMList.Free;
    Pathlist.Free;
    Captionlist.Free;
    Hintlist.Free;
  end;
end;

procedure TfrmMain.LoadFromTxtFile(const TxtFilePath: string);
var
  Lines: TStringList;
  i: Integer;
  Line, FilePath, Caption, CoverPath: string;
  PosPipe: Integer;
begin
  { Loads a playlist from a text file (Format: FilePath|Caption) }
  if skfmFlowGallery = nil then
  begin
    skfmFlowGallery := TSkFlowmotion.Create(Self);
    skfmFlowGallery.Parent := lytContent;
    skfmFlowGallery.Align := TAlignLayout.Client;
  end;
  { Setup basic gallery style for playlist mode }
  skfmFlowGallery.BackgroundColor := TAlphaColors.Black;
  skfmFlowGallery.FlowLayout := TFlowLayout.flSorted;
  skfmFlowGallery.AnimationSpeed := 2;
  skfmFlowGallery.OnSelectedImageDblClick := Flowmotion1SelectedImageDblClick;
  skfmFlowGallery.Spacing := 15;
  skfmFlowGallery.PageSize := 80;
  skfmFlowGallery.ShowCaptions := True;
  skfmFlowGallery.KeepSpaceforZoomed := False;
  skfmFlowGallery.ShowHint := true;
  skfmFlowGallery.SmallPicVisible := False;
  skfmFlowGallery.OnSelectedImageEnterZone := Flowmotion1SelectedImageEnterZone;
  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 3);
  skfmFlowGallery.SetBackgroundpicture(ExtractFilePath(ParamStr(0)) + 'back.jpg');
  skfmFlowGallery.CaptionFont.Size := 14;
  skfmFlowGallery.CaptionFont.Family := 'Segoe UI';
  skfmFlowGallery.HotTrackZoom := True;
  skfmFlowGallery.Visible := True;
  skfmFlowGallery.BringToFront;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(TxtFilePath);
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if Line = '' then
        Continue;
      { Parse Line: FilePath|Caption }
      PosPipe := Pos('|', Line);
      if PosPipe > 0 then
      begin
        FilePath := Trim(Copy(Line, 1, PosPipe - 1));
        Caption := Trim(Copy(Line, PosPipe + 1, MaxInt));
      end
      else
      begin
        FilePath := Line;
        Caption := ExtractFileName(FilePath);
      end;
      if not FileExists(FilePath) then
        Continue;
      { Try to find a cover image if path is a folder }
      CoverPath := GetFirstImageInFolder(ExtractFilePath(FilePath));
      if CoverPath <> '' then
        skfmFlowGallery.AddImageAsync(CoverPath, Caption, FilePath, '');
    end;
  finally
    Lines.Free;
  end;
end;
{ *******************************************************************************
  IPC (INTER-PROCESS COMMUNICATION)
  ******************************************************************************* }

procedure TfrmMain.SendSignalToPlayer(const FilePath: string);
var
  PlayerHandle: HWND;
  Data: AnsiString;
  CopyData: TCopyDataStruct;
begin
  { Sends a message to 'MEDIA Revolution Master' app to play/open a file }
  PlayerHandle := FindWindow('TMRMaster', 'MEDIA Revolution Master');
  if PlayerHandle = 0 then
    Exit;
  Data := AnsiString('MRXSKIAFLM' + FilePath);
  CopyData.dwData := 0;
  CopyData.cbData := Length(Data) + 1;
  CopyData.lpData := PAnsiChar(Data);
  SendMessage(PlayerHandle, WM_COPYDATA, 0, LPARAM(@CopyData));
  Close;
end;

procedure TfrmMain.SendNextToPlayer(const FilePath: string);
var
  PlayerHandle: HWND;
  Data: AnsiString;
  CopyData: TCopyDataStruct;
begin
  { Sends 'Next' command with path to Player }
  PlayerHandle := FindWindow('TMRMaster', 'MEDIA Revolution Master');
  if PlayerHandle = 0 then
    Exit;
  Data := AnsiString('MRXSKIAFLMNXT' + FilePath);
  CopyData.dwData := 0;
  CopyData.cbData := Length(Data) + 1;
  CopyData.lpData := PAnsiChar(Data);
  SendMessage(PlayerHandle, WM_COPYDATA, 0, LPARAM(@CopyData));
end;

procedure TfrmMain.WMCopyData(var Message: TWMCopyData);
var
  Data: string;
begin
  { Handles incoming messages (e.g., Load Playlist) }
  inherited;
  if Message.CopyDataStruct.cbData > 0 then
  begin
    SetLength(Data, Message.CopyDataStruct.cbData);
    Move(Message.CopyDataStruct.lpData^, Data[1], Message.CopyDataStruct.cbData);
    { Clean up null terminators }
    while (Length(Data) > 0) and (Data[Length(Data)] = #0) do
      SetLength(Data, Length(Data) - 1);
    Data := Trim(Data);
    { Check if file exists and load }
    if FileExists(Data) then
    begin
      skfmFlowGallery.Clear(True);
      LoadFromTxtFile(Data);
    end;
  end;
  Message.Result := 1;
end;
{ *******************************************************************************
  UTILITIES
  ******************************************************************************* }

function TfrmMain.GetFirstImageInFolder(const Folder: string): string;
var
  Files: TArray<string>;
begin
  Result := '';
  { Searches for JPG then PNG }
  Files := TDirectory.GetFiles(Folder, '*.jpg', TSearchOption.soTopDirectoryOnly);
  if Length(Files) > 0 then
    Exit(Files[0]);
  Files := TDirectory.GetFiles(Folder, '*.png', TSearchOption.soTopDirectoryOnly);
  if Length(Files) > 0 then
    Exit(Files[0]);
end;

procedure TfrmMain.Timer2Timer(Sender: TObject);
begin
  if skfmFlowGallery.CurrentSelectedIndex <= skfmFlowGallery.ImageCount - 1 then
    skfmFlowGallery.SelectNextImage
  else
  begin
    skfmFlowGallery.DeselectZoomedImage;
  end;
end;

procedure TfrmMain.Timer3Timer(Sender: TObject);
var
  NextFile: string;
  NewImage: ISkImage;
begin
  { 1. Check if list is empty }
  if (FStreamFiles.Count = 0) or (skfmFlowGallery.SelectedImage = nil) then
  begin
    Timer3.Enabled := False;
    skfmFlowGallery.ExternalStreamImage := nil;
    Exit;
  end;

  { 2. Get next file }
  NextFile := FStreamFiles[FStreamindex];

  { 3. Load Image }
  NewImage := TSkImage.MakeFromEncodedFile(NextFile);

  { 4. Push to Pipeline }
  if Assigned(NewImage) then
    skfmFlowGallery.ExternalStreamImage := NewImage;

  { 5. Advance Index (Loop) }
  Inc(FStreamindex);
  if FStreamindex >= FStreamFiles.Count then
    FStreamindex := 0;
end;

end.

