unit Sample.Form.Main;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types, System.IOUtils,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani, FMX.Objects, ShellAPI,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects, System.Win.Registry,
  System.Skia, FMX.Skia, System.ImageList, FMX.ImgList, FMX.ListBox, FMX.Colors,
  FMX.Edit, FMX.EditBox, FMX.SpinBox, windows, messages,
  //-----
  uSkFlowmotion, FMX.Menus;

const
  HKEY_CLASSES_ROOT = $80000000;
  HKEY_CURRENT_USER  = $80000001;
  HKEY_LOCAL_MACHINE = $80000002;
  HKEY_USERS         = $80000003;
  HKEY_CURRENT_CONFIG = $80000005;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    saiAnimatedLogo: TSkAnimatedImage;
    fanFadeOutTransition: TFloatAnimation;
    lytContent: TLayout;
    lytcontrols: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Rectangle1: TRectangle;
    CheckBox2: TCheckBox;
    Button9: TButton;
    Button10: TButton;
    Panel1: TPanel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
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
    ComboBox1: TComboBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
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
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure CheckBox9Change(Sender: TObject);
    procedure ColorPicker1MouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Single);
    procedure ComboBox1Change(Sender: TObject);
    procedure fanFadeOutTransitionFinish(Sender: TObject);
    procedure saiAnimatedLogoAnimationFinished(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lytcontrolsResize(Sender: TObject);
    procedure rbPagesizeMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Single);
    procedure SpinBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Loadedwithparams : Boolean;
    ParamsTxtFile: string;
    skfmFlowGallery: TSkFlowmotion;
    procedure InitGallery;
    procedure SendNextToPlayer(const FilePath: string);
    procedure skfmFlowGalleryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
    procedure SendSignalToPlayer(const FilePath: string);
    procedure LoadFromTxtFile(const TxtFilePath: string);
    function GetFirstImageInFolder(const Folder: string): string;
    procedure Flowmotion1SelectedImageDblClick(Sender: TObject; ImageItem: TImageItem; Index: Integer);
    procedure LoadInstalledPrograms;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Flowmotion1SelectedImageEnterZone(Sender: TObject; ImageItem: TImageItem; const ZoneName: string);
     protected
     procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.Button10Click(Sender: TObject);
begin
   skfmFlowGallery.Clear(true, true, Panel1.BoundsRect.Round, Panel1.BoundsRect.Round, iesFromPoint, true);
end;

procedure TfrmMain.Button11Click(Sender: TObject);
begin
  LoadInstalledPrograms;
end;

procedure TfrmMain.Button12Click(Sender: TObject);
begin
  skfmFlowGallery.ZoomSelectedToFull;
end;

procedure TfrmMain.Flowmotion1SelectedImageEnterZone(Sender: TObject; ImageItem: TImageItem; const ZoneName: string);
begin
  // Show a message or perform an action based on the zone name, here u can do...whatever u like then
  if ZoneName = 'ActivationZone 1' then
    ShowMessage('Image entered ActivationZone 1! ');
end;

procedure TfrmMain.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if WheelDelta > 0 then
    skfmFlowGallery.SelectPreviousImage
  else if WheelDelta < 0 then
    skfmFlowGallery.SelectNextImage;
  Handled := True;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
 skfmFlowGallery.SelectPreviousImage;
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
  //skfmFlowGallery.ImageEntryStyle := iesFromLeft;
  //skfmFlowGallery.AddImageAsync(ExtractFilePath(ParamStr(0)) + inttostr(random(13)+1) + '.jpg');

  skfmFlowGallery.ImageEntryStyle := iesFromPoint;
  skfmFlowGallery.EntryPoint := Point(Round(Button5.Position.X), Round(Button5.Position.Y));
  skfmFlowGallery.AddImageAsync(ExtractFilePath(ParamStr(0)) + inttostr(random(13)+1) + '.jpg');
end;

procedure TfrmMain.Button9Click(Sender: TObject);
var
  i: Integer;
begin
  // 1. SET ENTRY POINT (Where images fly from)
  // We use the button's screen center position as the target
  skfmFlowGallery.EntryPoint := Point(Round(Button9.Position.X), Round(Button9.Position.Y));
  skfmFlowGallery.ImageEntryStyle := iesFromPoint;
  // 2. SHOW DIALOG
  // Ensure TOpenDialog1 has Options: [ofAllowMultiSelect] set in Object Inspector
  if OpenDialog1.Execute then
  begin
    // 3. ITERATE AND ADD IMAGES
    // TOpenDialog1.Files contains a list of all selected filenames (TStrings)
    if OpenDialog1.Files.Count > 0 then
    begin
      for i := 0 to OpenDialog1.Files.Count - 1 do
      begin
        // Add each image asynchronously.
        // The Control generates Captions/Paths automatically from the filename.
        skfmFlowGallery.AddImageAsync(OpenDialog1.Files[i]);
      end;
    end;
  end;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  skfmFlowGallery.Clear(true);
end;


procedure TfrmMain.Button6Click(Sender: TObject);
var
  AppDir: string;
  i: Integer;
  IMList,Pathlist, Captionlist, Hintlist: TStringList;
begin
  AppDir := ExtractFilePath(ParamStr(0));
  IMList:= TStringList.create;
  Pathlist:= TStringList.create;
  Captionlist:= TStringList.create;
  Hintlist:= TStringList.create;
  try
   for i := 14 downto 0 do begin
     IMList.add(AppDir + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
     Hintlist.Add('Hint');
   end;
   for i := 0 to 14 do begin
     IMList.add(AppDir + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
     Hintlist.Add('Hint');
   end;

  skfmFlowGallery.MaxZoomSize := trunc(Clientwidth / 4);
  skfmFlowGallery.AddImagesAsync(IMList,Captionlist,Pathlist, Hintlist);
  finally
   IMList.Free;
   Pathlist.Free;
   Captionlist.Free;
   Hintlist.Free;
  end;
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  if Opendialog1.Execute then skfmFlowGallery.SetBackgroundpicture(Opendialog1.FileName);
end;

procedure TfrmMain.Button8Click(Sender: TObject);
begin
  skfmFlowGallery.ResetAllRotations;
end;



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

procedure TfrmMain.CheckBox1Change(Sender: TObject);
begin
   if not Checkbox1.IsChecked then skfmFlowGallery.PictureBorderType := btFull
   else skfmFlowGallery.PictureBorderType := btTech;
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
if CheckBox4.IsChecked then
   skfmFlowGallery.StartingAngle := -1 else skfmFlowGallery.StartingAngle := 0;
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

procedure TfrmMain.ColorPicker1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Single);
begin
if not Assigned(skfmFlowGallery) then Exit;

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
    skfmFlowGallery.ParticleColor := ColorPicker1.Color;
end;

procedure TfrmMain.ComboBox1Change(Sender: TObject);
begin
   case Combobox1.ItemIndex of
     0: skfmFlowGallery.SetFlowLayout(flSorted);
     1: skfmFlowGallery.SetFlowLayout(flFreeFloat);
   end;
end;

procedure TfrmMain.LoadInstalledPrograms;
var
  Reg: TRegistry;
  Keys: TStringList;
  i: Integer;
  AppName, ExePath, IconPath: string;
begin
  Reg := TRegistry.Create;
  Keys := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall') then
    begin
      Reg.GetKeyNames(Keys);
      Reg.CloseKey;
    end;
    // Zusätzlich 32-Bit-Apps auf 64-Bit Windows
    if Reg.OpenKeyReadOnly('SOFTWARE\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall') then
    begin
      Reg.GetKeyNames(Keys);
      Reg.CloseKey;
    end;
    skfmFlowGallery.Clear(True);
    for i := 0 to Keys.Count - 1 do
    begin
      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' + Keys[i]) then
      begin
        AppName := Reg.ReadString('DisplayName');
        IconPath := Reg.ReadString('DisplayIcon');
        ExePath := Reg.ReadString('InstallLocation');
        if (AppName <> '') and (IconPath <> '') then
        begin
          if Pos(',', IconPath) > 0 then
            IconPath := Copy(IconPath, 1, Pos(',', IconPath) - 1);
          if FileExists(IconPath) then
          begin
            skfmFlowGallery.AddImageAsync(IconPath, AppName, ExePath, AppName);
          end;
        end;
        Reg.CloseKey;
      end;
    end;
  finally
    Keys.Free;
    Reg.Free;
  end;
end;

procedure TfrmMain.skfmFlowGalleryMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Item: TImageItem;
begin
  if Button = TMouseButton.mbMiddle then  // Mittelklick
  begin
    Item := skfmFlowGallery.GetImageAtPoint(X, Y);
    if (Item <> nil) and (Item.Path <> '') then
    begin
      SendNextToPlayer(Item.Path);
      skfmFlowGallery.DeselectZoomedImage; // Optional
      // Optional: Visuelles Feedback (z. B. Farbwechsel oder Message)
      ShowMessage('Als Nächstes gesendet: ' + Item.Caption);
    end;
  end;
end;

procedure TfrmMain.SendNextToPlayer(const FilePath: string);
var
  PlayerHandle: HWND;
  Data: AnsiString;
  CopyData: TCopyDataStruct;
begin
  PlayerHandle := FindWindow('TMRMaster', 'MEDIA Revolution Master');
  if PlayerHandle = 0 then Exit;

  // Prefix für "Next" (wie du willst: MRXSKIAFLMNXT)
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
  inherited;

  if Message.CopyDataStruct.cbData > 0 then
  begin
    SetLength(Data, Message.CopyDataStruct.cbData);
    Move(Message.CopyDataStruct.lpData^, Data[1], Message.CopyDataStruct.cbData);

    while (Length(Data) > 0) and (Data[Length(Data)] = #0) do
      SetLength(Data, Length(Data) - 1);

    Data := Trim(Data);

    if FileExists(Data)  then
    begin
      skfmFlowGallery.Clear(True);
      LoadFromTxtFile(Data);
    end;
  end;

  Message.Result := 1;
end;

procedure TfrmMain.SendSignalToPlayer(const FilePath: string);
var
  PlayerHandle: HWND;
  Data: AnsiString;
  CopyData: TCopyDataStruct;
begin
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

procedure TfrmMain.Flowmotion1SelectedImageDblClick(Sender: TObject; ImageItem: TImageItem; Index: Integer);
var
  FolderPath: string;
begin
  if (ImageItem = nil) or (ImageItem.Path = '') then
    Exit;
  FolderPath := ImageItem.Path;
  if Loadedwithparams then begin
     SendSignaltoPlayer(FolderPath);
     Exit;
  end;
  if (FolderPath = '') or (FolderPath = 'Folder or whatever') then begin
    //ShowMessage('dblclicked selected');
    skfmFlowGallery.ZoomSelectedToFull;
    Exit;
  end;

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

procedure TfrmMain.InitGallery;
var
  AppDir: string;
  FileName: string;
  i: Integer;
  IMList,Pathlist, Captionlist, Hintlist: TStringList;
  smallimgindex: TList;
begin
  AppDir := ExtractFilePath(ParamStr(0));
  if skfmFlowGallery = nil then
  begin
    skfmFlowGallery := TSkFlowmotion.Create(Self);
    skfmFlowGallery.Parent := lytContent;
    skfmFlowGallery.Align := TAlignLayout.Client;
  end;
  skfmFlowGallery.BackgroundColor := TAlphaColors.Black;
  skfmFlowGallery.FlowLayout := TFlowLayout.flSorted;
  skfmFlowGallery.AnimationSpeed := 3;
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
  skfmFlowGallery.OnSelectedImageEnterZone := Flowmotion1SelectedImageEnterZone;
  skfmFlowGallery.AddActivationZone('ActivationZone 1', Panel1.BoundsRect);

  IMList:= TStringList.create;
  Pathlist:= TStringList.create;
  Captionlist:= TStringList.create;
  Hintlist:= TStringList.create;
  smallimgindex := TList.Create;
  try
   for i := 14 downto 0 do begin
     IMList.add(AppDir + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
     Hintlist.Add('Hint');
     smallimgindex.Add(Pointer(i));
   end;
   for i := 0 to 14 do begin
     IMList.add(AppDir + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
     Hintlist.Add('Hint');
     smallimgindex.Add(Pointer(i));
   end;

  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 2);
  skfmFlowGallery.AddImages(IMList,Captionlist,Pathlist, Hintlist, smallimgindex);
  finally
   IMList.Free;
   Pathlist.Free;
   Captionlist.Free;
   Hintlist.Free;
  end;
  FileName := AppDir + 'back.jpg';
  skfmFlowGallery.SetBackgroundpicture(FileName);
  skfmFlowGallery.CaptionFont.Size := 14;
  skfmFlowGallery.CaptionFont.Family := 'Segoe UI';
  skfmFlowGallery.HotTrackZoom := True;
  skfmFlowGallery.Visible := True;
  skfmFlowGallery.BringToFront;
end;



procedure TfrmMain.fanFadeOutTransitionFinish(Sender: TObject);
begin
  saiAnimatedLogo.Visible := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  lytcontrols.Visible := False;
  LoadedWithParams := False;
  if ParamCount >= 1 then
  begin
    borderstyle := TFmxFormBorderStyle.None;
    ParamsTxtFile := ParamStr(1);
    LoadedWithParams := True;
    saiAnimatedLogo.Visible := False;
    lytContent.Visible := True;
  end
  else begin
    saiAnimatedLogo.Visible := True;
    saiAnimatedLogo.BringToFront;
    lytContent.Visible := False;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
 if Loadedwithparams then Timer1.Enabled := True;
end;

procedure TfrmMain.LoadFromTxtFile(const TxtFilePath: string);
var
  Lines: TStringList;
  i: Integer;
  Line, FilePath, Caption, CoverPath: string;
  PosPipe: Integer;
begin
  if skfmFlowGallery = nil then
  begin
    skfmFlowGallery := TSkFlowmotion.Create(Self);
    skfmFlowGallery.Parent := lytContent;
    skfmFlowGallery.Align := TAlignLayout.Client;
  end;
  skfmFlowGallery.BackgroundColor := TAlphaColors.Black;
  skfmFlowGallery.FlowLayout := TFlowLayout.flSorted;
  skfmFlowGallery.AnimationSpeed := 3;
  skfmFlowGallery.OnSelectedImageDblClick := Flowmotion1SelectedImageDblClick;
  skfmFlowGallery.Spacing := 15;
  skfmFlowGallery.PageSize := 80;
  skfmFlowGallery.ShowCaptions := True;
  skfmFlowGallery.KeepSpaceforZoomed := False;
  skfmFlowGallery.ShowHint := true;
  skfmFlowGallery.SmallPicVisible := False;
  //skfmFlowGallery.SmallPicImageList := Imagelist1;
  skfmFlowGallery.OnSelectedImageEnterZone := Flowmotion1SelectedImageEnterZone;
  //skfmFlowGallery.AddActivationZone('ActivationZone 1', Panel1.BoundsRect);
  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 2);
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
      if Line = '' then Continue;

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
      if not FileExists(FilePath) then Continue;
      CoverPath := GetFirstImageInFolder(ExtractFilePath(FilePath));
      if CoverPath <> '' then
        skfmFlowGallery.AddImageAsync(CoverPath, Caption, FilePath, '');
    end;
  finally
    Lines.Free;
  end;
end;

function TfrmMain.GetFirstImageInFolder(const Folder: string): string;
var
  Files: TArray<string>;
begin
  Result := '';
  Files := TDirectory.GetFiles(Folder, '*.jpg', TSearchOption.soTopDirectoryOnly);
  if Length(Files) > 0 then Exit(Files[0]);

  Files := TDirectory.GetFiles(Folder, '*.png', TSearchOption.soTopDirectoryOnly);
  if Length(Files) > 0 then Exit(Files[0]);
end;



procedure TfrmMain.lytcontrolsResize(Sender: TObject);
begin
 if visible and assigned(skfmFlowGallery) then
  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 2);
end;

procedure TfrmMain.rbPagesizeMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Single);
begin
  if SpinBox1.Value < 50 then SpinBox1.Value := 50;

end;

procedure TfrmMain.saiAnimatedLogoAnimationFinished(Sender: TObject);
begin
  lytcontrols.Visible := not Loadedwithparams;
  lytContent.Visible := True;
  Fill.Color := $FFEBEEF1;
  fanFadeOutTransition.Enabled := True;
  Timer1.Enabled := True;
end;

procedure TfrmMain.SpinBox1Change(Sender: TObject);
begin
  if not Assigned(skfmFlowGallery) then Exit;
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
    skfmFlowGallery.AlphaStatic := Trunc(SpinBox1.Value);
end;



procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if LoadedWithParams then
    LoadFromTxtFile(ParamsTxtFile)
    else
  InitGallery;
end;



end.
