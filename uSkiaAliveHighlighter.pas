{*******************************************************************************
  SkiaAliveHighlighter

  part of skFlowmotion
  written by: Lara Miriam Tamy Reschke
*******************************************************************************}

unit uSkiaAliveHighlighter;

interface

uses
  { System }
  System.SysUtils, System.Types, System.Math, System.Generics.Collections,
  System.UITypes,
  { Skia }
  System.Skia, FMX.Skia;

type
  { Internal States for the Snake Logic }
  TSnakeState = (ssIdle, ssApproaching, ssOrbiting, ssExiting);

  { Simple record for a generic obstacle (Any Rect) }
  TObstacle = record
    Rect: TRectF;
  end;

  { TAliveStyle: Visual flavor }
  TAliveStyle = (asSnake, asEnergyBeam, asFirefly);

  { TAliveSnake: The entity state }
  TAliveSnake = record
    HeadPos: TPointF;         // Current Head Position
    Velocity: TPointF;        // Current Velocity Vector
    TargetRect: TRectF;       // The FINAL Goal (e.g., center of target image)
    CurrentWayPoint: TPointF; // Short term goal (to go around obstacles)

    { The Trail }
    PathHistory: TList<TPointF>;
    MaxHistory: Integer;

    { Appearance }
    Scale: Single;
    Alpha: Single;
    Phase: Single;           // For the Sway animation
    Color: TAlphaColor;
    Thickness: Single;
    GlowAmount: Single;
  end;

  { TAliveHighlighter: The standalone controller }
  TAliveHighlighter = class
  private
    FSnake: TAliveSnake;
    FState: TSnakeState;
    FStyle: TAliveStyle;
    FActive : Boolean;
    FSelfDestruct: Boolean; // Flag to trigger instant reset

    { Internal Physics & AI Helpers }
    procedure UpdatePhysics(DeltaTime: Single);
    procedure UpdateAI(const Obstacles: TArray<TObstacle>);
    function CheckCollision(const P: TPointF; const Obstacles: TArray<TObstacle>): TRectF;
    function GetCenter(const R: TRectF): TPointF; inline;
    procedure ResetToIdle;

  public
    constructor Create;
    destructor Destroy; override;

    { --- API --- }

    { Start moving to a specific point }
    procedure SendToPoint(const P: TPointF);

    { Start moving to the center of a specific Rect (e.g., an Image) }
    procedure SendToRect(const R: TRectF);

    { Stop and hide immediately }
    procedure Stop;

    { Fly outside of screen and reset instantly }
    procedure ExitToVoid;

    { The Main Loop (Call this from your Flowmotion AnimationThread) }
    procedure Update(DeltaTime: Single; const Obstacles: TArray<TObstacle>);

    { The Draw Loop (Call this from Flowmotion Draw) }
    procedure Draw(const Canvas: ISkCanvas);

    { Properties }
    property Active: Boolean read FActive write FActive;
    property Style: TAliveStyle read FStyle write FStyle;
    property Color: TAlphaColor read FSnake.Color write FSnake.Color;
  end;

implementation

{ TAliveHighlighter }

constructor TAliveHighlighter.Create;
begin
  inherited;
  FState := ssIdle;
  FSelfDestruct := False;
  FStyle := asSnake;

  // Init Snake
  FSnake.PathHistory := TList<TPointF>.Create;
  FSnake.MaxHistory := 20; // Length of the tail
  FSnake.Phase := 0;
  FSnake.Scale := 1.0;
  FSnake.Alpha := 0.0; // Starts invisible
  FSnake.Color := TAlphaColors.Cyan;
  FSnake.Thickness := 3.0;
  FSnake.GlowAmount := 5.0;
  FSnake.Velocity := TPointF.Create(0, 0);
end;

destructor TAliveHighlighter.Destroy;
begin
  FSnake.PathHistory.Free;
  inherited;
end;

procedure TAliveHighlighter.ResetToIdle;
begin
  FState := ssIdle;
  FSelfDestruct := False;
  FSnake.Alpha := 0.0;
  FSnake.Velocity := TPointF.Create(0, 0);
  FSnake.PathHistory.Clear;
end;

function TAliveHighlighter.GetCenter(const R: TRectF): TPointF;
begin
  Result.X := (R.Left + R.Right) / 2;
  Result.Y := (R.Top + R.Bottom) / 2;
end;

procedure TAliveHighlighter.SendToPoint(const P: TPointF);
begin
  if FState = ssExiting then Exit; // Don't interrupt an exit

  ResetToIdle;
  FState := ssApproaching;
  FActive := True;

  // Create a tiny 1x1 Rect around the point
  // The snake will orbit this tiny dot
  FSnake.TargetRect := TRectF.Create(P.X, P.Y, P.X + 1, P.Y + 1);

  FSnake.CurrentWayPoint := P;
  FSnake.Alpha := 1.0;
end;

procedure TAliveHighlighter.SendToRect(const R: TRectF);
begin
  if FState = ssExiting then Exit; // Don't interrupt an exit

  ResetToIdle;
  FState := ssApproaching;
  FActive := True;
  FSnake.TargetRect := R; // Just save the box!

  // Set initial WayPoint to center just to get it moving
  FSnake.CurrentWayPoint.X := (R.Left + R.Right) / 2;
  FSnake.CurrentWayPoint.Y := (R.Top + R.Bottom) / 2;

  FSnake.Alpha := 1.0;
end;

procedure TAliveHighlighter.Stop;
begin
  ResetToIdle;
end;

function TAliveHighlighter.CheckCollision(const P: TPointF; const Obstacles: TArray<TObstacle>): TRectF;
var
  I: Integer;
begin
  Result := RectF(0, 0, 0, 0);
  // Simple Point-in-Rect check
  for I := 0 to High(Obstacles) do
  begin
    if Obstacles[I].Rect.Contains(P) then
    begin
      Result := Obstacles[I].Rect;
      Exit; // Found first collision
    end;
  end;
end;

procedure TAliveHighlighter.ExitToVoid;
begin
  if FState = ssIdle then Exit;

  FState := ssExiting;
  FSelfDestruct := True; // "Finish it already" flag

  // 1. Set Target to -100, -100 (Always off-screen)
  FSnake.TargetRect := TRectF.Create(-100, -100, -99, -99);

  // 2. Give it a huge speed boost towards that direction
  // We want it to zip away, not crawl
  FSnake.Velocity.X := -20.0;
  FSnake.Velocity.Y := -20.0;
end;

procedure TAliveHighlighter.UpdateAI(const Obstacles: TArray<TObstacle>);
var
  LookAheadPos: TPointF;
  ObstacleRect: TRectF;
  DirToTarget: TPointF;
  DistToTarget: Single;
  LookAheadDist: Single;
  TargetCenter: TPointF;
begin
  if not FActive then Exit;

  // 1. Get the REAL Target Center from the Rect
  TargetCenter.X := (FSnake.TargetRect.Left + FSnake.TargetRect.Right) / 2;
  TargetCenter.Y := (FSnake.TargetRect.Top + FSnake.TargetRect.Bottom) / 2;

  // 2. Calculate vector to Target Center
  DirToTarget.X := TargetCenter.X - FSnake.HeadPos.X;
  DirToTarget.Y := TargetCenter.Y - FSnake.HeadPos.Y;
  DistToTarget := Sqrt(Sqr(DirToTarget.X) + Sqr(DirToTarget.Y));

  // 3. EXIT MODE CHECK
  // If we are exiting, we ignore obstacles and just fly to the target
  if FState = ssExiting then
  begin
    // If we are "close enough" (e.g. 10px) to the exit point, kill it
    if (DistToTarget < 10.0) and FSelfDestruct then
    begin
      ResetToIdle;
      Exit;
    end;
    // Don't do obstacle avoidance while exiting
    Exit;
  end;

  // 4. ARRIVAL CHECK (Switch to Orbit)
  var TargetRadius := Max(FSnake.TargetRect.Width, FSnake.TargetRect.Height) / 2 + 20;

  if DistToTarget < TargetRadius then
  begin
    FState := ssOrbiting;
    // Physics handles the rest
    Exit;
  end;

  // 5. THE SENSOR: Look ahead (Pathfinding)
  LookAheadDist := 60.0;

  if (FSnake.Velocity.X = 0) and (FSnake.Velocity.Y = 0) then
    LookAheadPos := TPointF.Create(FSnake.HeadPos.X + DirToTarget.X, FSnake.HeadPos.Y + DirToTarget.Y)
  else
    LookAheadPos := TPointF.Create(FSnake.HeadPos.X + FSnake.Velocity.X * 5, FSnake.HeadPos.Y + FSnake.Velocity.Y * 5);

  // 6. Check for Obstacles
  ObstacleRect := CheckCollision(LookAheadPos, Obstacles);

  if not ObstacleRect.IsEmpty then
  begin
    // OBSTACLE DETECTED!
    var ObstacleCenter: TPointF;
    ObstacleCenter.X := (ObstacleRect.Left + ObstacleRect.Right) / 2;
    ObstacleCenter.Y := (ObstacleRect.Top + ObstacleRect.Bottom) / 2;

    // Set WayPoint to Obstacle Center to fly around it
    FSnake.CurrentWayPoint := ObstacleCenter;
  end
  else
  begin
    // PATH CLEAR
    // Aim directly at Target Center
    FSnake.CurrentWayPoint := TargetCenter;
  end;
end;

procedure TAliveHighlighter.UpdatePhysics(DeltaTime: Single);
var
  TargetCenter: TPointF;
  DirX, DirY, Dist: Single;
  SpeedFactor: Single;
begin
  if not FActive then Exit;

  // 1. Get Center
  TargetCenter.X := (FSnake.TargetRect.Left + FSnake.TargetRect.Right) / 2;
  TargetCenter.Y := ( FSnake.TargetRect.Top + FSnake.TargetRect.Bottom) / 2;

  // 2. Vector to Center
  DirX := TargetCenter.X - FSnake.HeadPos.X;
  DirY := TargetCenter.Y - FSnake.HeadPos.Y;
  Dist := Sqrt(Sqr(DirX) + Sqr(DirY));

  // Normalize
  if Dist > 0 then
  begin
    DirX := DirX / Dist;
    DirY := DirY / Dist;
  end;

  // ========================================================================
  // PHYSICS LOGIC
  // ========================================================================

  // --- EXIT MODE: Direct Flight ---
  if FState = ssExiting then
  begin
    // Just keep flying. We already set velocity in ExitToVoid
    // Maybe add a little steering towards -100,-100 if it misses
    FSnake.Velocity.X := FSnake.Velocity.X + (DirX * 2.0);
    FSnake.Velocity.Y := FSnake.Velocity.Y + (DirY * 2.0);
  end
  else
  begin
    // --- ORBIT / APPROACH MODE ---

    // --- A. INSIDE THE BOX? PUSH OUT ---
    if FSnake.TargetRect.Contains(FSnake.HeadPos) then
    begin
      FSnake.Velocity.X := FSnake.Velocity.X - (DirX * 1.0);
      FSnake.Velocity.Y := FSnake.Velocity.Y - (DirY * 1.0);
    end

    // --- B. TOO FAR OUTSIDE? PULL IN ---
    else if Dist > 100 then
    begin
      FSnake.Velocity.X := FSnake.Velocity.X + (DirX * 0.2);
      FSnake.Velocity.Y := FSnake.Velocity.Y + (DirY * 0.2);
    end;

    // --- C. THE ORBIT FORCE ---
    FSnake.Velocity.X := FSnake.Velocity.X - (DirY * 0.25);
    FSnake.Velocity.Y := FSnake.Velocity.Y + (DirX * 0.25);

    // --- D. STAY IN SCREEN (Keep > 0) ---
    // If we hit the edge (0,0), gently push back in
    if FSnake.HeadPos.X < 10 then FSnake.Velocity.X := FSnake.Velocity.X + 0.5;
    if FSnake.HeadPos.Y < 10 then FSnake.Velocity.Y := FSnake.Velocity.Y + 0.5;
  end;

  // --- E. APPLY PHYSICS ---
  FSnake.HeadPos.X := FSnake.HeadPos.X + FSnake.Velocity.X;
  FSnake.HeadPos.Y := FSnake.HeadPos.Y + FSnake.Velocity.Y;

  // Friction (Drag)
  FSnake.Velocity.X := FSnake.Velocity.X * 0.95;
  FSnake.Velocity.Y := FSnake.Velocity.Y * 0.95;

  // Tail
  FSnake.PathHistory.Add(FSnake.HeadPos);
  if FSnake.PathHistory.Count > 40 then
    FSnake.PathHistory.Delete(0);

  FSnake.Phase := FSnake.Phase + 0.1;
end;

procedure TAliveHighlighter.Update(DeltaTime: Single; const Obstacles: TArray<TObstacle>);
begin
  // 1. Run the Brain (Pathfinding & State Management)
  UpdateAI(Obstacles);

  // 2. Run the Body (Movement)
  UpdatePhysics(DeltaTime);
end;

procedure TAliveHighlighter.Draw(const Canvas: ISkCanvas);
var
  I: Integer;
  PathBuilder: ISkPathBuilder;
  Path: ISkPath;
  Paint: ISkPaint;
  P1, P2: TPointF;
  SwayX, SwayY: Single;
begin
  if (FSnake.Alpha < 0.01) then Exit;

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.AntiAlias := True;
  Paint.StrokeCap := TSkStrokeCap.Round;
  Paint.Color := FSnake.Color;
  Paint.AlphaF := FSnake.Alpha;
  Paint.StrokeWidth := FSnake.Thickness;

  if FSnake.GlowAmount > 0 then
    Paint.ImageFilter := TSkImageFilter.MakeBlur(FSnake.GlowAmount, FSnake.GlowAmount);

  if FSnake.PathHistory.Count < 2 then Exit;

  PathBuilder := TSkPathBuilder.Create;

  // Start from the TAIL (Oldest point)
  P1 := FSnake.PathHistory[0];
  PathBuilder.MoveTo(P1.X, P1.Y);

  // Draw the Body to the HEAD
  for I := 1 to FSnake.PathHistory.Count - 1 do
  begin
    P2 := FSnake.PathHistory[I];

    // Calculate Sway (Organic movement)
    SwayX := Sin(FSnake.Phase + (I * 0.2)) * 3.0 * FSnake.Scale;
    SwayY := Cos(FSnake.Phase + (I * 0.2)) * 3.0 * FSnake.Scale;

    PathBuilder.LineTo(P2.X + SwayX, P2.Y + SwayY);
  end;

  Path := PathBuilder.Snapshot;
  Canvas.DrawPath(Path, Paint);
end;

end.
