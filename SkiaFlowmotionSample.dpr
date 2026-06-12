program SkiaFlowmotionSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Sample.Form.Main in 'Sample.Form.Main.pas' {frmMain},
  uSkFlowmotion in 'uSkFlowmotion.pas',
  uSkFlowEffects in 'uSkFlowEffects.pas',
  uSkiaAliveHighlighter in 'uSkiaAliveHighlighter.pas';

{$R *.res}

begin
  GlobalUseMetal := True;
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable := False;
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
