unit Unit9;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  System.IOUtils,
  uSkFlowmotion;

type
  TForm9 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FlowGallery: TSkFlowmotion;
    lytMain: TLayout;
    lytButtons: TLayout;
    btnAddRandom: TButton;
    btnAddList: TButton;
    btnClear: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    btnDeselect: TButton;
    btnSetBackground: TButton;

    procedure CreateControls;
    procedure AddSampleImageList;
    procedure AddRandomImage;

    procedure btnAddRandomClick(Sender: TObject);
    procedure btnAddListClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnDeselectClick(Sender: TObject);
    procedure btnSetBackgroundClick(Sender: TObject);
  public
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

procedure TForm9.FormCreate(Sender: TObject);
begin
  Caption := 'Skia-FlowMotion Test (Android + Windows)';
  CreateControls;
end;

procedure TForm9.CreateControls;
begin
  lytMain := TLayout.Create(Self);
  lytMain.Parent := Self;
  lytMain.Align := TAlignLayout.Client;

  FlowGallery := TSkFlowmotion.Create(Self);
  FlowGallery.Parent := lytMain;
  FlowGallery.Align := TAlignLayout.Client;

  FlowGallery.BackgroundColor := TAlphaColors.Black;
  FlowGallery.FlowLayout := flSorted;
  FlowGallery.AnimationSpeed := 3;
  FlowGallery.Spacing := 4;
  FlowGallery.ShowCaptions := True;
  FlowGallery.CaptionFont.Size := 16;
  FlowGallery.CaptionFont.Family := 'Roboto';
  FlowGallery.HotTrackZoom := True;
  FlowGallery.BreathingEnabled := True;
  FlowGallery.ZoomSelectedtoCenter := True;
  FlowGallery.HitTest := True;

  lytButtons := TLayout.Create(Self);
  lytButtons.Parent := lytMain;
  lytButtons.Align := TAlignLayout.Bottom;
  lytButtons.Height := 80;

  btnPrev := TButton.Create(Self);
  btnPrev.Parent := lytButtons;
  btnPrev.Text := '◄ Prev';
  btnPrev.Position.X := 20;
  btnPrev.Position.Y := 10;
  btnPrev.Width := 100;
  btnPrev.Height := 60;
  btnPrev.OnClick := btnPrevClick;

  btnNext := TButton.Create(Self);
  btnNext.Parent := lytButtons;
  btnNext.Text := 'Next ►';
  btnNext.Position.X := 130;
  btnNext.Position.Y := 10;
  btnNext.Width := 100;
  btnNext.Height := 60;
  btnNext.OnClick := btnNextClick;

  btnDeselect := TButton.Create(Self);
  btnDeselect.Parent := lytButtons;
  btnDeselect.Text := 'Deselect';
  btnDeselect.Position.X := 240;
  btnDeselect.Position.Y := 10;
  btnDeselect.Width := 120;
  btnDeselect.Height := 60;
  btnDeselect.OnClick := btnDeselectClick;

  btnAddRandom := TButton.Create(Self);
  btnAddRandom.Parent := lytButtons;
  btnAddRandom.Text := '+ Random';
  btnAddRandom.Position.X := 370;
  btnAddRandom.Position.Y := 10;
  btnAddRandom.Width := 120;
  btnAddRandom.Height := 60;
  btnAddRandom.OnClick := btnAddRandomClick;

  btnAddList := TButton.Create(Self);
  btnAddList.Parent := lytButtons;
  btnAddList.Text := 'Add All (1-12)';
  btnAddList.Position.X := 500;
  btnAddList.Position.Y := 10;
  btnAddList.Width := 150;
  btnAddList.Height := 60;
  btnAddList.OnClick := btnAddListClick;

  btnClear := TButton.Create(Self);
  btnClear.Parent := lytButtons;
  btnClear.Text := 'Clear';
  btnClear.Position.X := 660;
  btnClear.Position.Y := 10;
  btnClear.Width := 100;
  btnClear.Height := 60;
  btnClear.OnClick := btnClearClick;

  btnSetBackground := TButton.Create(Self);
  btnSetBackground.Parent := lytButtons;
  btnSetBackground.Text := 'Set BG';
  btnSetBackground.Position.X := 770;
  btnSetBackground.Position.Y := 10;
  btnSetBackground.Width := 100;
  btnSetBackground.Height := 60;
  btnSetBackground.OnClick := btnSetBackgroundClick;
end;

procedure TForm9.AddSampleImageList;
var
  AssetsPath: string;
  i: Integer;
  IMList, PathList, CaptionList, HintList: TStringList;
begin
  {$IF Defined(ANDROID)}
  AssetsPath := TPath.GetDocumentsPath;
  {$ELSE}
  AssetsPath := ExtractFilePath(ParamStr(0));
  {$ENDIF}

  IMList := TStringList.Create;
  PathList := TStringList.Create;
  CaptionList := TStringList.Create;
  HintList := TStringList.Create;
  try
    for i := 1 to 12 do
    begin
      IMList.Add(TPath.Combine(AssetsPath, IntToStr(i) + '.jpg'));
      PathList.Add('Folder or whatever');
      CaptionList.Add('Caption ' + IntToStr(i));
      HintList.Add('Hint for image ' + IntToStr(i));
    end;

    FlowGallery.MaxZoomSize := 200;
    FlowGallery.AddImages(IMList, CaptionList, PathList, HintList);

  finally
    IMList.Free;
    PathList.Free;
    CaptionList.Free;
    HintList.Free;
  end;
  var BgFile := TPath.Combine(AssetsPath, 'back.jpg');
  if TFile.Exists(BgFile) then
    FlowGallery.SetBackgroundpicture(BgFile);
end;

procedure TForm9.AddRandomImage;
var
  AssetsPath, FileName: string;
begin
  {$IF Defined(ANDROID)}
  AssetsPath := TPath.GetDocumentsPath;
  {$ELSE}
  AssetsPath := ExtractFilePath(ParamStr(0));
  {$ENDIF}

  FileName := TPath.Combine(AssetsPath, IntToStr(Random(12) + 1) + '.jpg');
  if TFile.Exists(FileName) then
    FlowGallery.AddImageAsync(FileName, 'Random Bild', 'Random Path', 'Random Hint');
end;

procedure TForm9.btnAddRandomClick(Sender: TObject);
begin
  AddRandomImage;
end;

procedure TForm9.btnAddListClick(Sender: TObject);
begin
  AddSampleImageList;
end;

procedure TForm9.btnClearClick(Sender: TObject);
begin
  FlowGallery.Clear(True);
end;

procedure TForm9.btnPrevClick(Sender: TObject);
begin
  FlowGallery.SelectPreviousImage;
end;

procedure TForm9.btnNextClick(Sender: TObject);
begin
  FlowGallery.SelectNextImage;
end;

procedure TForm9.btnDeselectClick(Sender: TObject);
begin
  FlowGallery.DeselectZoomedImage;
end;

procedure TForm9.btnSetBackgroundClick(Sender: TObject);
var
  AssetsPath, BgFile: string;
begin
  {$IF Defined(ANDROID)}
  AssetsPath := TPath.GetDocumentsPath;
  {$ELSE}
  AssetsPath := ExtractFilePath(ParamStr(0));
  {$ENDIF}

  BgFile := TPath.Combine(AssetsPath, 'back.jpg');
  if TFile.Exists(BgFile) then
    FlowGallery.SetBackgroundpicture(BgFile);
end;

procedure TForm9.FormShow(Sender: TObject);
begin
  AddSampleImageList;
end;

end.
