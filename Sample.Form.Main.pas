unit Sample.Form.Main;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects,
  uSkFlowmotion,
  { Skia }
  System.Skia, FMX.Skia, System.ImageList, FMX.ImgList, FMX.ListBox;

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
    ComboBox1: TComboBox;
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
    procedure Button10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure fanFadeOutTransitionFinish(Sender: TObject);
    procedure saiAnimatedLogoAnimationFinished(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lytcontrolsResize(Sender: TObject);
  private
    skfmFlowGallery: TSkFlowmotion;
    procedure InitGallery;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
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
  skfmFlowGallery.ImageEntryStyle := iesFromLeft;
  skfmFlowGallery.AddImageAsync(ExtractFilePath(ParamStr(0)) + inttostr(random(13)+1) + '.jpg');
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
   for i := 0 to 14 do begin
     IMList.add(AppDir + inttostr(i) + '.jpg');
     Pathlist.add('Folder or whatever');
     Captionlist.add('Caption');
     Hintlist.Add('Hint');
   end;
   for i := 14 downto 0 do begin
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

procedure TfrmMain.Button9Click(Sender: TObject);
begin
  skfmFlowGallery.ImageEntryStyle := iesFromLeft;
   if Opendialog1.Execute then begin
    skfmFlowGallery.AddImageAsync(Opendialog1.FileName);
   end;
end;

procedure TfrmMain.CheckBox1Change(Sender: TObject);
begin
   if Checkbox1.IsChecked then skfmFlowGallery.PictureBorderType := btFull
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
   skfmFlowGallery.StartingAngle := 0 else skfmFlowGallery.StartingAngle := -1;
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

procedure TfrmMain.ComboBox1Change(Sender: TObject);
begin
   case Combobox1.ItemIndex of
     0: skfmFlowGallery.SetFlowLayout(flSorted);
     1: skfmFlowGallery.SetFlowLayout(flFreeFloat);
   end;
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
  skfmFlowGallery.Spacing := 15;
  skfmFlowGallery.PageSize := 80;
  skfmFlowGallery.ShowCaptions := True;
  skfmFlowGallery.ShowHint := true;
  skfmFlowGallery.SmallPicVisible := True;
  skfmFlowGallery.SmallPicImageList := Imagelist1;
  IMList:= TStringList.create;
  Pathlist:= TStringList.create;
  Captionlist:= TStringList.create;
  Hintlist:= TStringList.create;
  smallimgindex := TList.Create;
  try
   for i := 0 to 14 do begin
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
   for i := 14 downto 0 do begin
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
  saiAnimatedLogo.Visible := True;
  saiAnimatedLogo.BringToFront;
  lytContent.Visible := False;
end;

procedure TfrmMain.lytcontrolsResize(Sender: TObject);
begin
 if visible and assigned(skfmFlowGallery) then
  skfmFlowGallery.MaxZoomSize := trunc(ClientHeight / 2);
end;

procedure TfrmMain.saiAnimatedLogoAnimationFinished(Sender: TObject);
begin
  lytcontrols.Visible := True;
  lytContent.Visible := True;
  Fill.Color := $FFEBEEF1;
  fanFadeOutTransition.Enabled := True;
  InitGallery;
end;

end.
