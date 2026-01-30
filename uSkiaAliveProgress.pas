unit uSkiaAliveProgress;

interface

uses
  { System }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections,
  { FMX }
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.ImgList, FMX.Media,
  { Skia }
  System.Skia, FMX.Skia;

type
  { TEntranceStyle: Determines how the snakes appear when animation starts }
  TEntranceStyle = (enFade,   { Simply fades in }
    enGrow,   { Grows from scale 0 }
    enImplode){ Implodes from outside };
  { TExitStyle: Determines how the snakes disappear when animation stops }
  TExitStyle = (exFade,      { Simply fades out }
    exExplode,   { Explodes outward }
    exBlackHole); { Implodes into the center rapidly }

  { TOrientation: The visual shape of the track }

  TOrientation = (orHorizontal, { Left to Right }
    orVertical,   { Top to Bottom }
    orCircle);    { Circular / Radial }

  { TProgressMode: Behavior of the animation }

  TProgressMode = (pmIndeterminate, { Continuous looping (Loader) }
    pmDeterminate);  { Represents actual value 0-100 (ProgressBar) }

  { TSnake: A single "particle" or "snake" in the animation swarm }

  TSnake = record
    { Position & Movement }
    Offset: Single;       // Current position on the track (0.0 to 1.0)
    Speed: Single;        // Current speed of movement
    BaseSpeed: Single;    // Original speed set at initialization
    Phase: Single;        // Used for sine-wave calculations (swaying/breathing)

    { Appearance }
    Scale: Single;        // Current size scale (0.0 to 1.0+)
    Alpha: Single;        // Current opacity (0.0 to 1.0)
    AlphaTarget: Single;  // The opacity we are trying to reach (for smoothing)

    { --- Relative Sizing Ratios --- }
    { These allow the snake to scale relatively to the component size }
    ThicknessRatio: Single;    // e.g., 1.0 = Standard, 2.0 = Thick, 0.5 = Thin
    LengthRatio: Single;       // e.g., 0.15 = 15% of the component's total size
    RadiusOffsetRatio: Single; // Distance from the center track (e.g., 0.05)

    { Physics for "Black Hole" exit mode }
    X, Y: Single; // Cartesian coordinates when leaving the track
  end;
  { TSkiaAliveProgress: The main visual component }
  TSkiaAliveProgress = class(TSkCustomControl)
  private
    FSnakes: TArray<TSnake>;   // Array of particles
    FTimer: TTimer;            // The animation loop driver (~60FPS)

    { Settings }
    FOrientation: TOrientation;
    FMode: TProgressMode;
    FEntranceStyle: TEntranceStyle;
    FExitStyle: TExitStyle;

    { Visuals }
    FSwarmColor: TAlphaColor;    // Color of the normal background particles
    FProgressColor: TAlphaColor; // Color of the progress indicator (if used)
    FLineThickness: Single;     // Base thickness of the lines
    FGlowAmount: Single;        // Blur amount for the neon glow effect
    FRebelColor: TAlphaColor;   // Color of the "Rebel" (the unique particle)

    { Logic }
    FXPosition: Single;          // 0..100 for Determinate mode
    FState: (stIdle, stActive, stExiting);

    { Internal Helpers }
    procedure UpdateSnakes;
    function GetCircleAngle(const Percent: Single): Single;
    procedure DrawSnake(const Canvas: ISkCanvas; const Snake: TSnake; const R: TRectF);
    function GetTrackPoint(const Percent: Single; const R: TRectF): TPointF;
    procedure TimerLoop(Sender: TObject);
  protected
    { Paint override }
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { API Methods }
    procedure Start;      // Begins the animation
    procedure Stop;       // Stops the animation immediately
    procedure AnimateIn;  // Triggers the entrance animation
    procedure AnimateOut; // Triggers the exit animation

    property XPosition: Single read FXPosition write FXPosition;
  published
    { Standard FMX Properties }
    property Align;
    property HitTest default False;

    { Custom Properties }
    property Orientation: TOrientation read FOrientation write FOrientation default orCircle;
    property ProgressMode: TProgressMode read FMode write FMode default pmIndeterminate;

    property SwarmColor: TAlphaColor read FSwarmColor write FSwarmColor default $FF00BCD4; // Cyan
    property ProgressColor: TAlphaColor read FProgressColor write FProgressColor default $FF2979FF; // Blue
    property LineThickness: Single read FLineThickness write FLineThickness;
    property GlowAmount: Single read FGlowAmount write FGlowAmount;

    property EntranceStyle: TEntranceStyle read FEntranceStyle write FEntranceStyle default enImplode;
    property ExitStyle: TExitStyle read FExitStyle write FExitStyle default exBlackHole;
  end;

implementation
{ TSkiaAliveProgress }

constructor TSkiaAliveProgress.Create(AOwner: TComponent);
begin
  inherited;

  HitTest := False;

  // --- Default Configuration ---
  FOrientation := orCircle;      // Circular is the default cool look
  FMode := pmIndeterminate;      // Default is a loader (endless)
  FSwarmColor := $FF00BCD4;      // Cyan
  FProgressColor := $FF2979FF;   // Blue
  FRebelColor := TAlphaColors.Orange; // The "Rebel" particle is Orange
  FLineThickness := 3.0;
  FGlowAmount := 4.0;
  FEntranceStyle := enGrow;      // Entrance animation
  FExitStyle := exBlackHole;     // Exit animation
  FXPosition := 0;
  FState := stIdle;

  // --- Timer Setup ---
  // We create a standard TTimer for the animation loop (~60 FPS -> 16ms)
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 32;
  FTimer.OnTimer := TimerLoop;

  // --- Swarm Initialization ---
  // Let's start with 8 snakes (1 Rebel + 7 Swarm)
  SetLength(FSnakes, 8);
end;

destructor TSkiaAliveProgress.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TSkiaAliveProgress.TimerLoop(Sender: TObject);
begin
  // Check if we are in a state where we might need to stop the engine
  if (FState = stExiting) or (FState = stIdle) then
  begin
    // If the first snake is effectively invisible/dead, we assume the animation is done
    if (Length(FSnakes) > 0) and (FSnakes[0].Alpha <= 0.01) and (FSnakes[0].Scale <= 0.01) then
    begin
      FTimer.Enabled := False; // STOP THE ENGINE to save resources
      FState := stIdle;
      Redraw; // Force one final clear redraw
      Exit;
    end;
  end;

  // Update physics positions
  UpdateSnakes;
  // Request a repaint
  Redraw;
end;

procedure TSkiaAliveProgress.Start;
begin
  if FState = stIdle then
    AnimateIn // If stopped, start the entrance animation
  else
    FState := stActive; // If just running, ensure active state

  FTimer.Enabled := True;
end;

procedure TSkiaAliveProgress.Stop;
begin
  FTimer.Enabled := False;
  FState := stIdle;
end;

procedure TSkiaAliveProgress.AnimateIn;
begin
  FTimer.Enabled := True;
  FState := stActive;

  var NumSnakes := Length(FSnakes);
  var InnerBandWidth := 0.12; // The width of the track area where snakes live
  var SlotWidth := 0.0;

  // Calculate how much space each swarm particle gets
  if NumSnakes > 1 then
    SlotWidth := InnerBandWidth / (NumSnakes - 1);

  for var I := 0 to High(FSnakes) do
  begin
    // --- SETUP SPEED ---
    if I = 0 then
      FSnakes[I].BaseSpeed := -0.003 // Index 0 is "The Rebel" (moves backwards)
    else
      FSnakes[I].BaseSpeed := 0.002 + Random * 0.003; // Swarm moves forward randomly

    FSnakes[I].Speed := FSnakes[I].BaseSpeed;
    FSnakes[I].Offset := Random; // Start at a random position
    FSnakes[I].Phase := Random * 2 * Pi; // Random sway start

    // --- SETUP ENTRANCE VISUALS ---
    FSnakes[I].Scale := 0;  // Start small
    FSnakes[I].Alpha := 0;  // Start invisible
    FSnakes[I].AlphaTarget := 1.0; // Target full visibility

    // --- SETUP RELATIVE SIZES ---
    FSnakes[I].ThicknessRatio := 0.5 + Random * 1.5; // Random thickness
    FSnakes[I].LengthRatio := 0.40 + Random * 0.80;  // Random length

    // --- SETUP POSITIONING ---
    if I = 0 then
    begin
      // --- THE REBEL ---
      // Give it a bit more distance from the center (or inside)
      FSnakes[I].RadiusOffsetRatio := -0.03 + (Random * 0.05);
    end
    else
    begin
      // --- THE SWARM ---
      // Distribute them along the band width
      var SlotIndex := I - 1;
      var BaseOffset := SlotIndex * SlotWidth;
      var Jitter := (Random - 0.5) * (SlotWidth * 0.2); // Add slight randomness so they aren't perfectly lined up
      FSnakes[I].RadiusOffsetRatio := BaseOffset + Jitter;
    end;

    // --- HANDLE SPECIFIC ENTRANCE STYLES ---
    case FEntranceStyle of
      enFade:
        FSnakes[I].Alpha := 1.0; // Instant visibility
      enImplode:
        ; // Logic handled in UpdateSnakes via Scale/Alpha smoothing
      enGrow:
        FSnakes[I].Scale := 0;   // Starts at 0, grows in UpdateSnakes
    end;
  end;
end;

procedure TSkiaAliveProgress.AnimateOut;
begin
  FState := stExiting;

  for var I := 0 to High(FSnakes) do
  begin
    // 1. Set Alpha Target to 0 (start fading out)
    FSnakes[I].AlphaTarget := 0.0;

    // 2. Special logic for Black Hole exit
    if FExitStyle = exBlackHole then
    begin
      // We let the UpdateSnakes handle the shrinking and moving to center
      // We don't force Scale to 0 here instantly, or the movement visual won't happen
    end
    else
    begin
      // For other styles, we could force parameters here if needed,
      // but smoothing handles fadeouts nicely.
    end;
  end;
end;

procedure TSkiaAliveProgress.UpdateSnakes;
var
  LCenter: TPointF;
  LRect: TRectF;
  LProgressOffset: Single;
begin
  LRect := LocalRect;
  LCenter := LRect.CenterPoint;

  // Calculate the normalized progress (0.0 to 1.0)
  LProgressOffset := FXPosition / 100.0;

  for var I := 0 to High(FSnakes) do
  begin
    // Copy record to local variable for modification
    var LSnake := FSnakes[I];

    { ------------------------------------------------------------
      1. Handle Alpha (Opacity) Smoothing
      Uses Linear Interpolation (Lerp) towards Target
      ------------------------------------------------------------ }
    if Abs(LSnake.Alpha - LSnake.AlphaTarget) > 0.01 then
      LSnake.Alpha := LSnake.Alpha + (LSnake.AlphaTarget - LSnake.Alpha) * 0.1
    else
      LSnake.Alpha := LSnake.AlphaTarget;

    { ------------------------------------------------------------
      2. Handle Scale (Size) Smoothing
      ------------------------------------------------------------ }
    if FState = stActive then
    begin
      // Grow towards 1.0 (Normal Size)
      if Abs(LSnake.Scale - 1.0) > 0.01 then
        LSnake.Scale := LSnake.Scale + (1.0 - LSnake.Scale) * 0.1;
    end
    else if FState = stExiting then
    begin
      // If exiting, shrink rapidly if it's the Black Hole style
      if FExitStyle = exBlackHole then
        LSnake.Scale := LSnake.Scale * 0.9; // Exponential decay
    end;

    { ------------------------------------------------------------
      3. Movement Logic
      ------------------------------------------------------------ }
    if FState = stActive then
    begin
      // --- LOGIC: DETERMINATE MODE (Locked Drone) ---
      // If we are showing real progress and this is the special "Rebel" snake (Index 0)
      if (FMode = pmDeterminate) and (I = 0) then
      begin
        // The rebel represents the progress. It moves exactly to the progress percentage.
        // Since it moves backwards (BaseSpeed < 0), we invert the logic so 0% is start.
        LSnake.Offset := 1.0 - LProgressOffset;

        // Update phase for breathing/sway effect
        LSnake.Phase := LSnake.Phase + 0.05;
      end
      else
      begin
        // --- LOGIC: INDETERMINATE MODE (Normal Swarm) ---

        // Move along the track
        LSnake.Offset := LSnake.Offset + LSnake.Speed;

        // Handle Wrapping (Looping around the track)
        if LSnake.Offset > 1 then
          LSnake.Offset := LSnake.Offset - 1
        else if LSnake.Offset < 0 then
          LSnake.Offset := LSnake.Offset + 1;

        // Update phase for breathing/sway effect
        LSnake.Phase := LSnake.Phase + 0.1;
      end;
    end
    else if FState = stExiting then
    begin
      // --- LOGIC: BLACK HOLE EXIT ---
      if FExitStyle = exBlackHole then
      begin
        // Calculate current position on the track
        var PCurr := GetTrackPoint(LSnake.Offset, LRect);

        // Calculate vector to center
        var DX := LCenter.X - PCurr.X;
        var DY := LCenter.Y - PCurr.Y;

        // Move the snake's drawing coordinates towards the center (Lerp)
        LSnake.X := PCurr.X + DX * 0.1;
        LSnake.Y := PCurr.Y + DY * 0.1;
      end;
    end;

    // Save updated values back to the array
    FSnakes[I] := LSnake;
  end;
end;

function TSkiaAliveProgress.GetTrackPoint(const Percent: Single; const R: TRectF): TPointF;
var
  Angle: Single;
begin
  case FOrientation of
    orHorizontal:
      // Linear interpolation from Left to Right
      Result := TPointF.Create(R.Left + (R.Width * Percent), (R.Top + R.Bottom) / 2);

    orVertical:
      // Linear interpolation from Top to Bottom
      Result := TPointF.Create((R.Left + R.Right) / 2, R.Top + (R.Height * Percent));

    orCircle:
      begin
        // Convert percentage (0-1) to Angle (Radians)
        // Subtract Pi/2 to start at 12 o'clock instead of 3 o'clock
        Angle := Percent * 2 * Pi - (Pi / 2);

        // Calculate radius (slightly smaller than half the min dimension)
        var Radius := Min(R.Width, R.Height) / 2 * 0.8;

        // Polar to Cartesian conversion
        Result := TPointF.Create((R.Left + R.Right) / 2 + Cos(Angle) * Radius, (R.Top + R.Bottom) / 2 + Sin(Angle) * Radius);
      end;
  end;
end;

procedure TSkiaAliveProgress.DrawSnake(const Canvas: ISkCanvas; const Snake: TSnake; const R: TRectF);
var
  PathBuilder: ISkPathBuilder;
  Path: ISkPath;
  Paint: ISkPaint;
  { Variables for Appearance Calculations }
  Sway: Single;
  Center: TPointF;
  LColor: TAlphaColor;
  LLength: Single;
  LThickness: Single;
  LRadius: Single;
  LAngle: Single;
  LSweepAngle: Single;
  { Variables for Gradient }
  HeadColor, TailColor: TAlphaColor;
  HeadPoint, TailPoint: TPointF;
  GradientPoints: TArray<TPointF>;
  GradientColors: TArray<TAlphaColor>;
  { Variables for Dynamic Sizing }
  BaseSize: Single;
  DynamicThickness: Single;
  DynamicBlur: Single;
  SizeRatio: Single;
  LIsRebel: Boolean;
begin
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.AntiAlias := True;
  Paint.StrokeCap := TSkStrokeCap.Round;

  // 1. Determine Color
  // Check if this is the "Rebel" snake (defined by negative BaseSpeed)
  LIsRebel := (Snake.BaseSpeed < 0);

  if LIsRebel then
    LColor := FRebelColor
  else if (FMode = pmDeterminate) and (Snake.Offset < (FXPosition / 100)) then
    LColor := FProgressColor
  else
    LColor := FSwarmColor;

  // 2. Set Opacity
  Paint.AlphaF := Snake.Alpha;

  // 3. Calculate Relative Sizing
  // We normalize sizes so the component looks good at 300px or 30px
  BaseSize := Min(R.Width, R.Height);
  SizeRatio := BaseSize / 300.0;

  // Calculate Thickness based on snake's individual ratio and global scaling
  DynamicThickness := FLineThickness * Snake.ThicknessRatio * SizeRatio;
  // Clamp thickness to keep it visible but not huge
  if DynamicThickness < 1.0 then
    DynamicThickness := 1.0;
  if DynamicThickness > 4.0 then
    DynamicThickness := 4.0;

  // Apply the snake's current Scale (grow/shrink animation)
  Paint.StrokeWidth := DynamicThickness * Snake.Scale;

  // Calculate Blur (Glow)
  DynamicBlur := FGlowAmount * SizeRatio;
  if DynamicBlur > FGlowAmount then
    DynamicBlur := FGlowAmount;

  if DynamicBlur > 0.5 then
    Paint.ImageFilter := TSkImageFilter.MakeBlur(DynamicBlur, DynamicBlur)
  else
    Paint.ImageFilter := nil;

  // Length of the snake segment
  LLength := Snake.LengthRatio * BaseSize * Snake.Scale;

  Center := R.CenterPoint;
  // Calculate Sway (Sine wave offset)
  Sway := Sin(Snake.Phase) * 0.5 * Snake.Scale;

  PathBuilder := TSkPathBuilder.Create;
  try
    if FOrientation = orCircle then
    begin
      // --- DRAWING: CIRCULAR ARC ---

      // Calculate Radius: Base Radius + Offset + Sway
      LRadius := (BaseSize / 2 * 0.75) + (Snake.RadiusOffsetRatio * BaseSize);
      LRadius := LRadius + Sway;
      if LRadius < 1.0 then
        LRadius := 1.0;

      // Calculate Center Angle
      LAngle := (Snake.Offset * 2 * Pi) - (Pi / 2);

      // Calculate how much of the circle the arc covers (in radians)
      LSweepAngle := LLength / LRadius;

      // Convert to Degrees for Skia's AddArc (Skia usually expects degrees)
      var DegAngle := RadToDeg(LAngle);
      var DegSweep := RadToDeg(LSweepAngle);

      // If Rebel, we draw the arc BACKWARDS (Negative Sweep) so the head is leading
      if LIsRebel then
        DegSweep := -DegSweep;

      // --- CREATE GRADIENT POINTS ---
      var RadHead := DegToRad(DegAngle + (DegSweep / 2));
      var RadTail := DegToRad(DegAngle - (DegSweep / 2));

      HeadPoint := TPointF.Create(Center.X + Cos(RadHead) * LRadius, Center.Y + Sin(RadHead) * LRadius);
      TailPoint := TPointF.Create(Center.X + Cos(RadTail) * LRadius, Center.Y + Sin(RadTail) * LRadius);

      // Setup Gradient Colors (Color -> Transparent)
      HeadColor := LColor;
      TailColor := LColor;
      TailColor := TailColor and $00FFFFFF; // Set Alpha to 0

      // Handle Rebel Gradient Direction
      if LIsRebel then
      begin
        // Rebel moves backward, so head is actually at the "Tail" geometric position
        GradientPoints := [HeadPoint, TailPoint];
        GradientColors := [HeadColor, TailColor];
      end
      else
      begin
        GradientPoints := [TailPoint, HeadPoint];
        GradientColors := [TailColor, HeadColor];
      end;

      Paint.Shader := TSkShader.MakeGradientLinear(GradientPoints[0], GradientPoints[1], GradientColors, nil, TSkTileMode.Clamp);

      // --- DRAW THE ARC ---
      // AddArc takes: BoundingRect, StartAngle, SweepAngle
      PathBuilder.AddArc(TRectF.Create(Center.X - LRadius, Center.Y - LRadius, Center.X + LRadius, Center.Y + LRadius), DegAngle - (DegSweep / 2), DegSweep);
    end
    else
    begin
      // --- DRAWING: STRAIGHT LINES ---

      // Calculate Sway offset (more pronounced for lines)
      Sway := Sin(Snake.Phase) * 10.0 * Snake.Scale;
      var Pos: TPointF;

      // If exiting (Black Hole), use the computed Cartesian physics coordinates
      if FState = stExiting then
      begin
        Pos := TPointF.Create(Snake.X, Snake.Y);
      end
      else
      begin
        // Otherwise, get position on the track
        Pos := GetTrackPoint(Snake.Offset, R);

        // Apply Sway perpendicular to direction
        if FOrientation = orHorizontal then
          Pos.Y := Pos.Y + Sway
        else if FOrientation = orVertical then
          Pos.X := Pos.X + Sway;
      end;

      // Setup Gradient Colors
      HeadColor := LColor;
      TailColor := LColor;
      TailColor := TailColor and $00FFFFFF; // Fade out tail

      if FOrientation = orHorizontal then
      begin
        HeadPoint := Pos;
        HeadPoint.X := Pos.X + LLength; // Head is to the right
        TailPoint := Pos;
        TailPoint.X := Pos.X - LLength; // Tail is to the left

        PathBuilder.MoveTo(TailPoint.X, Pos.Y);
        PathBuilder.LineTo(HeadPoint.X, Pos.Y);
      end
      else if FOrientation = orVertical then
      begin
        HeadPoint := Pos;
        HeadPoint.Y := Pos.Y + LLength; // Head is below
        TailPoint := Pos;
        TailPoint.Y := Pos.Y - LLength; // Tail is above

        PathBuilder.MoveTo(Pos.X, TailPoint.Y);
        PathBuilder.LineTo(Pos.X, HeadPoint.Y);
      end;

      GradientPoints := [TailPoint, HeadPoint];
      GradientColors := [TailColor, HeadColor];
      Paint.Shader := TSkShader.MakeGradientLinear(GradientPoints[0], GradientPoints[1], GradientColors, nil, TSkTileMode.Clamp);
    end;

    // Finalize Path and Draw
    Path := PathBuilder.Snapshot;
    Canvas.DrawPath(Path, Paint);
  finally
    // Interfaces are automatically reference-counted, no manual Free needed
  end;
end;

function TSkiaAliveProgress.GetCircleAngle(const Percent: Single): Single;
begin
  // Helper to convert percentage to angle starting from top (12 o'clock)
  Result := (Percent * 2 * Pi) - (Pi / 2);
end;

procedure TSkiaAliveProgress.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  I: Integer;
begin
  inherited; // Call parent draw (handles background caching etc)

  // Clear the canvas (Transparent)
  ACanvas.Clear(TAlphaColors.Null);

  // Iterate through all snakes and draw them
  for I := 0 to High(FSnakes) do
  begin
    // Optimization: Don't draw if invisible
    if FSnakes[I].Alpha > 0.01 then
      DrawSnake(ACanvas, FSnakes[I], ADest);
  end;
end;

end.

