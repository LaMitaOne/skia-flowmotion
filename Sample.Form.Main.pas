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
    Button8: TButton;
    Label1: TLabel;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure fanFadeOutTransitionFinish(Sender: TObject);
    procedure saiAnimatedLogoAnimationFinished(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    skfmFlowGallery: TSkFlowmotion;
    procedure InitGallery;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

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
  skfmFlowGallery.AddImageAsync(ExtractFilePath(ParamStr(0)) + inttostr(random(11)+1) + '.jpg');
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
   for i := 1 to 12 do begin
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
  skfmFlowGallery.SelectedMovable := not skfmFlowGallery.SelectedMovable;
  if skfmFlowGallery.SelectedMovable then Button8.Text := 'Drag selected on'
   else Button8.Text := 'Drag selected off';
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
  skfmFlowGallery.Spacing := 4;
  skfmFlowGallery.ShowCaptions := True;
  skfmFlowGallery.ShowHint := True;
  skfmFlowGallery.SmallPicVisible := True;
  skfmFlowGallery.SmallPicImageList := Imagelist1;
  IMList:= TStringList.create;
  Pathlist:= TStringList.create;
  Captionlist:= TStringList.create;
  Hintlist:= TStringList.create;
  try
   for i := 1 to 12 do begin
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
  FileName := AppDir + 'back.jpg';
  skfmFlowGallery.SetBackgroundpicture(FileName);
  skfmFlowGallery.CaptionFont.Size := 14;
  skfmFlowGallery.CaptionFont.Family := 'Segoe UI';
  skfmFlowGallery.ShowCaptions := True;
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
  saiAnimatedLogo.Visible := True;
  saiAnimatedLogo.BringToFront;
  lytContent.Visible := False;
end;

procedure TfrmMain.saiAnimatedLogoAnimationFinished(Sender: TObject);
begin
  lytContent.Visible := True;
  Fill.Color := $FFEBEEF1;
  fanFadeOutTransition.Enabled := True;
  InitGallery;
end;

end.
