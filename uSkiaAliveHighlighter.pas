{*******************************************************************************
  SkiaAliveHighlighter
  part of skFlowmotion
  written by: Lara Miriam Tamy Reschke

  Description:
  A dynamic, organic highlighting controller for Delphi (FMX) using the Skia4Delphi library.
  It simulates an entity (Snake, Energy Beam, or Firefly) that travels to specific
  screen coordinates, interacts with the mouse cursor, and avoids obstacles.

  Features:
  - 3 Visual Styles: Snake (organic sway), Energy Beam (jittery laser), Firefly (particle trail).
  - AI States: Approaching, Orbiting, Stalking (chasing mouse), Biting, Retreating, Enraged, Exiting.
  - Physics: Inertia, friction, wall collision, and "look-ahead" obstacle avoidance.
  - FX: Dynamic scaling, thickness modulation, shadows, and glow.
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
  { Internal States for the Entity Logic }
  TSnakeState = (ssIdle, ssApproaching, ssOrbiting, ssStalking, ssBiting, ssRetreating, ssEnraged, ssExiting);

  { Simple record defining a rectangular obstacle for collision detection }
  TObstacle = record
    Rect: TRectF;
  end;

  { Visual flavor presets }
  TAliveStyle = (asSnake, asEnergyBeam, asFirefly);

  { TAliveSnake: The physical state and properties of the entity }
  TAliveSnake = record
    HeadPos: TPointF;         // Current Head Position (World Coordinates)
    Velocity: TPointF;        // Current Velocity Vector (Direction * Speed)
    TargetRect: TRectF;       // The FINAL Goal (e.g., center of target image)
    CurrentWayPoint: TPointF; // Short term goal (used for pathfinding around obstacles)

    { The Trail }
    PathHistory: TList<TPointF>; // Stores previous positions to draw the tail
    MaxHistory: Integer;         // Length of the tail

    { Appearance & Animation }
    Scale: Single;           // Current Zoom level (1.0 = Normal)
    ScaleTarget: Single;     // Desired Zoom level (for smooth transitions)
    Thickness: Single;       // Current stroke/size width
    ThicknessTarget: Single; // Desired thickness
    Alpha: Single;           // Opacity (0.0 to 1.0)
    Phase: Single;           // Animation ticker (for sine wave swaying)
    SnakeColor: TAlphaColor;  // Primary tint color

    { Visual FX }
    GlowAmount: Single;      // Blur radius for the glow effect
    ShadowColor: TAlphaColor;
    ShadowBlur: Single;
    ShadowOffset: TPointF;

    { Interaction }
    MousePos: TPointF;       // Current Mouse Position (passed from Form)
    AggroTimer: Single;      // Randomized timer to trigger "Stalking" behavior
    BiteTimer: Single;       // Duration of the "Bite" attack animation
    AngerCounter: Integer;   // Counts how many times he got angry
    RageTimer: Single;       // Duration of the Enraged state
  end;

  { TAliveHighlighter: Main controller class }
  TAliveHighlighter = class
  private
    FSnake: TAliveSnake;
    FState: TSnakeState;
    FStyle: TAliveStyle;
    FActive: Boolean;
    FActiveCountdown: Single; // Auto-dismiss timer (seconds)
    FSelfDestruct: Boolean;  // Flag indicating the entity should leave the screen
    FBounds: TRectF;          // Screen boundaries (Width/Height)
    FSavedNormalColor: TAlphaColor; // Stores the user's set color persistently

    { Physics & AI Core }
    procedure UpdatePhysics(DeltaTime: Single);
    procedure UpdateAI(const Obstacles: TArray<TObstacle>);

    { Helpers }
    function CheckCollision(const P: TPointF; const Obstacles: TArray<TObstacle>): TRectF;
    function GetCenter(const R: TRectF): TPointF; inline;
    procedure ResetToIdle;
    procedure DrawCrackle(const Canvas: ISkCanvas; const Center: TPointF);
    procedure SetSnakeColor(const Value: TAlphaColor);

  public
    constructor Create;
    destructor Destroy; override;

    { --- Public API --- }

    /// <summary> Sends the entity to a specific point. </summary>
    procedure SendToPoint(const P: TPointF);

    /// <summary> Sends the entity to a specific rectangle (it will orbit the center). </summary>
    procedure SendToRect(const R: TRectF);

    /// <summary> Stops the entity immediately. </summary>
    procedure Stop;

    /// <summary> Triggers an exit animation off-screen. </summary>
    procedure ExitToVoid;

    /// <summary> Updates the internal mouse position for interaction logic. </summary>
    procedure UpdateMousePos(X, Y: Single);

    { --- Main Loop --- }

    /// <summary> Call this every frame to update logic. </summary>
    procedure Update(DeltaTime: Single; const Obstacles: TArray<TObstacle>);

    /// <summary> Call this inside the OnPaint event to render. </summary>
    procedure Draw(const Canvas: ISkCanvas);

    { --- Properties --- }
    property Active: Boolean read FActive write FActive;
    property Style: TAliveStyle read FStyle write FStyle;
    property Color: TAlphaColor read FSnake.SnakeColor write SetSnakeColor;

    // Shadow Configuration
    property ShadowColor: TAlphaColor read FSnake.ShadowColor write FSnake.ShadowColor;
    property ShadowBlur: Single read FSnake.ShadowBlur write FSnake.ShadowBlur;
    property ShadowOffset: TPointF read FSnake.ShadowOffset write FSnake.ShadowOffset;

    // Glow Configuration
    property GlowAmount: Single read FSnake.GlowAmount write FSnake.GlowAmount;

    // Screen Bounds
    procedure SetBounds(AWidth, AHeight: Single);
  end;

implementation

{ TAliveHighlighter }

constructor TAliveHighlighter.Create;
begin
  inherited;

  // Initialize Default State
  FState := ssIdle;
  FSelfDestruct := False;
  FStyle := asSnake;
  FActiveCountdown := 0;
  FSavedNormalColor := TAlphaColors.Cyan; // Default persistent color

  // Initialize Bounds (Defaults to 1080p, updated by Main Form)
  FBounds := TRectF.Create(0, 0, 1920, 1080);

  // Initialize Snake Entity
  FSnake.PathHistory := TList<TPointF>.Create;
  FSnake.MaxHistory := 20;
  FSnake.Phase := 0;
  FSnake.Scale := 1.0;
  FSnake.ScaleTarget := 1.0;
  FSnake.Thickness := 5.0;
  FSnake.ThicknessTarget := 5.0;
  FSnake.Alpha := 0.0; // Starts invisible
  FSnake.SnakeColor := FSavedNormalColor; // Use the persistent color
  FSnake.Velocity := TPointF.Create(0, 0);

  // Initialize Interaction Logic
  FSnake.MousePos := TPointF.Create(-1000, -1000); // Off-screen initially
  FSnake.AggroTimer := Random * 5.0;
  FSnake.BiteTimer := 0;

  // Anger System
  FSnake.AngerCounter := 0;
  FSnake.RageTimer := 0;

  // Initialize Default Visual FX
  FSnake.GlowAmount := 5.0;
  FSnake.ShadowColor := TAlphaColors.Black;
  FSnake.ShadowBlur := 4.0;
  FSnake.ShadowOffset := TPointF.Create(6.0, 6.0);
end;

destructor TAliveHighlighter.destroy;
begin
  FSnake.PathHistory.Free;
  inherited;
end;

procedure TAliveHighlighter.SetBounds(AWidth, AHeight: Single);
begin
  // Update internal screen boundaries when form resizes
  FBounds.Width := AWidth;
  FBounds.Height := AHeight;
end;

procedure TAliveHighlighter.SetSnakeColor(const Value: TAlphaColor);
begin
  // 1. Update the internal field directly
  FSnake.SnakeColor := Value;

  // 2. UPDATE THE PERSISTENT COLOR
  // This ensures "normal" color changes too, so when he exits rage, he uses the NEW color.
  FSavedNormalColor := Value;

  // 3. If we are NOT in Rage, apply it immediately
  if (FState <> ssEnraged) then
    FSnake.SnakeColor := Value;
end;

procedure TAliveHighlighter.ResetToIdle;
begin
  FState := ssIdle;
  FActive := False;
  FSelfDestruct := False;

  // Reset physical properties
  FSnake.Alpha := 0.0;
  FSnake.Velocity := TPointF.Create(0, 0);

  if Assigned(FSnake.PathHistory) then
    FSnake.PathHistory.Clear;

  // Reset visual targets to default
  FSnake.ScaleTarget := 1.0;
  FSnake.ThicknessTarget := 5.0;

  // Color reset - use the persistent saved color
  FSnake.SnakeColor := FSavedNormalColor;
end;

function TAliveHighlighter.GetCenter(const R: TRectF): TPointF;
begin
  // Helper to calculate the geometric center of a rectangle
  Result.X := (R.Left + R.Right) / 2;
  Result.Y := (R.Top + R.Bottom) / 2;
end;

procedure TAliveHighlighter.SendToPoint(const P: TPointF);
begin
  if FState = ssExiting then
    Exit;

  ResetToIdle;
  FState := ssApproaching;
  FActive := True;
  FActiveCountdown := 10.0;

  // Set Target
  FSnake.TargetRect := TRectF.Create(P.X, P.Y, P.X + 1, P.Y + 1);
  FSnake.CurrentWayPoint := P;

  // Visual Reset
  FSnake.Alpha := 1.0;
  FSnake.ScaleTarget := 1.0;
  FSnake.ThicknessTarget := 5.0;
end;

procedure TAliveHighlighter.SendToRect(const R: TRectF);
begin
  if FState = ssExiting then
    Exit;

  // Note: We do not fully reset here to preserve the tail if moving between targets quickly
  FState := ssApproaching;
  FActive := True;
  FActiveCountdown := 10.0;

  // Set Target to the center of the rect
  FSnake.TargetRect := R;
  FSnake.CurrentWayPoint.X := (R.Left + R.Right) / 2;
  FSnake.CurrentWayPoint.Y := (R.Top + R.Bottom) / 2;

  // Visual Reset
  FSnake.Alpha := 1.0;
  FSnake.ScaleTarget := 1.0;
  FSnake.ThicknessTarget := 5.0;
end;

procedure TAliveHighlighter.UpdateMousePos(X, Y: Single);
begin
  // Update internal mouse tracker
  FSnake.MousePos.X := X;
  FSnake.MousePos.Y := Y;
end;

procedure TAliveHighlighter.Stop;
begin
  ResetToIdle;
end;

function TAliveHighlighter.CheckCollision(const P: TPointF; const Obstacles: TArray<TObstacle>): TRectF;
var
  I: Integer;
begin
  // Simple point-in-rect collision check
  Result := RectF(0, 0, 0, 0);
  for I := 0 to High(Obstacles) do
  begin
    if Obstacles[I].Rect.Contains(P) then
    begin
      Result := Obstacles[I].Rect;
      Exit;
    end;
  end;
end;

procedure TAliveHighlighter.ExitToVoid;
var
  DistLeft, DistRight, DistTop, DistBottom: Single;
  TargetX, TargetY: Single;
begin
  if FState = ssIdle then
    Exit;

  FState := ssExiting;
  FSelfDestruct := True;

  // 1. Calculate distance to all 4 screen edges
  DistLeft := Abs(FSnake.HeadPos.X - FBounds.Left);
  DistRight := Abs(FSnake.HeadPos.X - FBounds.Right);
  DistTop := Abs(FSnake.HeadPos.Y - FBounds.Top);
  DistBottom := Abs(FSnake.HeadPos.Y - FBounds.Bottom);

  // 2. Find the CLOSEST edge (Minimum Distance) to ensure quick exit
  // Start assuming Left is closest
  TargetX := FBounds.Left - 200;
  TargetY := FSnake.HeadPos.Y;

  if (DistRight <= DistLeft) and (DistRight <= DistTop) and (DistRight <= DistBottom) then
  begin
    // Right edge is closest
    TargetX := FBounds.Right + 200;
    TargetY := FSnake.HeadPos.Y;
  end
  else if (DistTop <= DistLeft) and (DistTop <= DistRight) and (DistTop <= DistBottom) then
  begin
    // Top edge is closest
    TargetX := FSnake.HeadPos.X;
    TargetY := FBounds.Top - 200;
  end
  else if (DistBottom <= DistLeft) and (DistBottom <= DistRight) and (DistBottom <= DistTop) then
  begin
    // Bottom edge is closest
    TargetX := FSnake.HeadPos.X;
    TargetY := FBounds.Bottom + 200;
  end;
  // Else: Left edge is closest (already set above)

  // 3. Set the Target point off-screen
  FSnake.TargetRect := TRectF.Create(TargetX, TargetY, TargetX + 1, TargetY + 1);
  FSnake.ScaleTarget := 1.0;
  FSnake.ThicknessTarget := 5.0;

  // 4. Apply Initial Velocity towards target to kickstart the exit
  FSnake.Velocity.X := (TargetX - FSnake.HeadPos.X) * 0.1;
  FSnake.Velocity.Y := (TargetY - FSnake.HeadPos.Y) * 0.1;
end;

procedure TAliveHighlighter.UpdateAI(const Obstacles: TArray<TObstacle>);
var
  LookAheadPos: TPointF;
  ObstacleRect: TRectF;
  DirToTarget: TPointF;
  DistToTarget: Single;
  TargetCenter: TPointF;
  MouseVec: TPointF;
  DistToMouseHead: Single;
begin
  if not FActive then
    Exit;

  // ==========================================================
  // 0. ENRAGED STATE (The Rage Mode)
  // ==========================================================
  if FState = ssEnraged then
  begin
    // Tick down the rage timer
    FSnake.RageTimer := FSnake.RageTimer - 0.016;

    // Behavior: Relentless chase
    MouseVec := TPointF.Create(FSnake.MousePos.X - FSnake.HeadPos.X, FSnake.MousePos.Y - FSnake.HeadPos.Y);
    DistToMouseHead := Hypot(MouseVec.X, MouseVec.Y);

    if DistToMouseHead > 0 then
    begin
      MouseVec.X := MouseVec.X / DistToMouseHead;
      MouseVec.Y := MouseVec.Y / DistToMouseHead;

      // Very aggressive acceleration
      FSnake.Velocity.X := FSnake.Velocity.X + (MouseVec.X * 0.8);
      FSnake.Velocity.Y := FSnake.Velocity.Y + (MouseVec.Y * 0.8);
    end;

    // If rage timer expires, calm down
    if FSnake.RageTimer <= 0 then
    begin
      FState := ssOrbiting;

      // Return to the PERSISTENT saved color (synced with main form)
      FSnake.SnakeColor := FSavedNormalColor;

      FSnake.ScaleTarget := 1.0;
      FSnake.ThicknessTarget := 5.0;
      FSnake.AngerCounter := 0; // Reset anger counter
    end;

    // Rage logic overrides everything below
    Exit;
  end;

  // ==========================================================
  // 1. AGGRO LOGIC (Proximity Trigger)
  // ==========================================================
  if (FState = ssOrbiting) or (FState = ssApproaching) then
  begin
    DistToMouseHead := Hypot(FSnake.HeadPos.X - FSnake.MousePos.X, FSnake.HeadPos.Y - FSnake.MousePos.Y);

    // If mouse is close, randomly decide to Chase or Flee
    if DistToMouseHead < 100.0 then
    begin
      if Random(50) = 1 then
      begin
        // Increment Anger Counter
        Inc(FSnake.AngerCounter);

        // CHECK FOR RAGE TRIGGER
        if FSnake.AngerCounter >= (5 + Random(10)) then
        begin
          FState := ssEnraged;
          FSnake.RageTimer := 1+ Random (3); // few seconds of pure rage
          FSnake.SnakeColor := TAlphacolors.Red; // Turn Red
          FSnake.ScaleTarget := 2.5;
          FSnake.ThicknessTarget := 20.0;
          // Scream sound effect could go here
        end
        else if Random(2) = 1 then
          FState := ssStalking  // Chase Mouse
        else
          FState := ssRetreating; // Run from Mouse

        // Get "Excited" (Visual feedback)
        FSnake.ScaleTarget := 2.0;
        FSnake.ThicknessTarget := 14.0;
      end
      else
      begin
        FState := ssApproaching;
      end;
    end
    else
    begin
      FState := ssApproaching;
    end;
  end;

  // ==========================================================
  // 2. BITING LOGIC (Attack State)
  // ==========================================================
  if FState = ssStalking then
  begin
    DistToMouseHead := Hypot(FSnake.HeadPos.X - FSnake.MousePos.X, FSnake.HeadPos.Y - FSnake.MousePos.Y);

    // If very close, Lunge/Bite
    if DistToMouseHead < 30.0 then
    begin
      FState := ssBiting;
      FSnake.BiteTimer := 0.3; // Duration of bite
      FSnake.ScaleTarget := 3.0;     // Zoom in
      FSnake.ThicknessTarget := 22.0; // Get fat
      FSnake.Velocity := TPointF.Create(0, 0); // Stop movement for impact
    end;
  end;

  if FState = ssBiting then
  begin
    FSnake.BiteTimer := FSnake.BiteTimer - 0.016;

    // Lunge towards mouse rapidly
    MouseVec := TPointF.Create(FSnake.MousePos.X - FSnake.HeadPos.X, FSnake.MousePos.Y - FSnake.HeadPos.Y);
    var Dist := Hypot(MouseVec.X, MouseVec.Y);
    if Dist > 0 then
    begin
      MouseVec.X := MouseVec.X / Dist;
      MouseVec.Y := MouseVec.Y / Dist;
      FSnake.Velocity.X := FSnake.Velocity.X + (MouseVec.X * 0.8);
      FSnake.Velocity.Y := FSnake.Velocity.Y + (MouseVec.Y * 0.8);
    end;

    if FSnake.BiteTimer <= 0 then
    begin
      // Calm down and return to orbiting
      FState := ssOrbiting;
      FSnake.ScaleTarget := 1.0;
      FSnake.ThicknessTarget := 5.0;
    end;
  end;

  // ==========================================================
  // 3. RETREATING LOGIC (Fleeing State)
  // ==========================================================
  if FState = ssRetreating then
  begin
    DistToMouseHead := Hypot(FSnake.HeadPos.X - FSnake.MousePos.X, FSnake.HeadPos.Y - FSnake.MousePos.Y);

    // If far enough away, return to normal
    if DistToMouseHead > 200.0 then
    begin
      FState := ssOrbiting;
      FSnake.ScaleTarget := 1.0;
      FSnake.ThicknessTarget := 5.0;
    end
    else
    begin
      // 1. Calculate raw "Run Away" vector
      var RunVec := TPointF.Create(FSnake.HeadPos.X - FSnake.MousePos.X, FSnake.HeadPos.Y - FSnake.MousePos.Y);

      if Hypot(RunVec.X, RunVec.Y) > 0 then
      begin
        RunVec.X := RunVec.X / Hypot(RunVec.X, RunVec.Y);
        RunVec.Y := RunVec.Y / Hypot(RunVec.X, RunVec.Y);

        // 2. *** WALL AVOIDANCE WHILE RETREATING ***
        // If running away means hitting a wall, we steer him ALONG the wall instead.
        const Margin = 60.0;

        // Check Left Wall
        if (FSnake.HeadPos.X < FBounds.Left + Margin) and (RunVec.X < 0) then
          RunVec.X := 0.5 // Force movement Right (along wall)

        // Check Right Wall
        else if (FSnake.HeadPos.X > FBounds.Right - Margin) and (RunVec.X > 0) then
          RunVec.X := -0.5 // Force movement Left (along wall)

        // Check Top Wall
        else if (FSnake.HeadPos.Y < FBounds.Top + Margin) and (RunVec.Y < 0) then
          RunVec.Y := 0.5 // Force movement Down (along wall)

        // Check Bottom Wall
        else if (FSnake.HeadPos.Y > FBounds.Bottom - Margin) and (RunVec.Y > 0) then
          RunVec.Y := -0.5; // Force movement Up (along wall);
        // ******************************************************

        // 3. Accelerate using corrected Vector
        FSnake.Velocity.X := FSnake.Velocity.X + (RunVec.X * 2.0);
        FSnake.Velocity.Y := FSnake.Velocity.Y + (RunVec.Y * 2.0);
      end;
    end;
  end;

  // ==========================================================
  // 4. MAIN PATHFINDING & OBSTACLE AVOIDANCE
  // ==========================================================
  if (FState = ssOrbiting) or (FState = ssApproaching) then
  begin
    TargetCenter.X := (FSnake.TargetRect.Left + FSnake.TargetRect.Right) / 2;
    TargetCenter.Y := (FSnake.TargetRect.Top + FSnake.TargetRect.Bottom) / 2;

    DirToTarget.X := TargetCenter.X - FSnake.HeadPos.X;
    DirToTarget.Y := TargetCenter.Y - FSnake.HeadPos.Y;
    DistToTarget := Sqrt(Sqr(DirToTarget.X) + Sqr(DirToTarget.Y));

    // Switch to Orbiting if close enough to target
    var TargetRadius := Max(FSnake.TargetRect.Width, FSnake.TargetRect.Height) / 2 + 20;
    if DistToTarget < TargetRadius then
    begin
      FState := ssOrbiting;
    end;

    // Look-Ahead Logic: Check if we are about to hit something
    var LookAheadDist := 60.0;

    // Determine where we will be in the next few frames
    if (FSnake.Velocity.X = 0) and (FSnake.Velocity.Y = 0) then
      LookAheadPos := TPointF.Create(FSnake.HeadPos.X + DirToTarget.X, FSnake.HeadPos.Y + DirToTarget.Y)
    else
      LookAheadPos := TPointF.Create(FSnake.HeadPos.X + FSnake.Velocity.X * 5, FSnake.HeadPos.Y + FSnake.Velocity.Y * 5);

    // Check collision at the look-ahead point
    ObstacleRect := CheckCollision(LookAheadPos, Obstacles);

    if not ObstacleRect.IsEmpty then
    begin
      // Collision detected: steer towards the center of the obstacle to go around it
      var ObstacleCenter: TPointF;
      ObstacleCenter.X := (ObstacleRect.Left + ObstacleRect.Right) / 2;
      ObstacleCenter.Y := (ObstacleRect.Top + ObstacleRect.Bottom) / 2;
      FSnake.CurrentWayPoint := ObstacleCenter;
    end
    else
    begin
      // Path clear: head to main target
      FSnake.CurrentWayPoint := TargetCenter;
    end;
  end;

  // ==========================================================
  // 5. EXITING CHECK (Cleanup)
  // ==========================================================
  if FState = ssExiting then
  begin
    // Check if we are fully OUTSIDE the screen bounds (with margin)
    if (FSnake.HeadPos.X < FBounds.Left - 50) or (FSnake.HeadPos.X > FBounds.Right + 50) or (FSnake.HeadPos.Y < FBounds.Top - 50) or (FSnake.HeadPos.Y > FBounds.Bottom + 50) then
    begin
      // Safe to Reset
      ResetToIdle;
      Exit;
    end;

    // Still inside? Keep moving.
    // We skip standard logic below.
    Exit;
  end;
end;

procedure TAliveHighlighter.UpdatePhysics(DeltaTime: Single);
var
  TargetCenter: TPointF;
  DirX, DirY, Dist: Single;
begin
  if not FActive then
    Exit;

  if FState = ssIdle then
    Exit;

  // ==========================================================
  // Z-AXIS INTERPOLATION (Smooth Transitions)
  // ==========================================================
  // Smoothly update Scale
  if Abs(FSnake.Scale - FSnake.ScaleTarget) > 0.01 then
    FSnake.Scale := FSnake.Scale + (FSnake.ScaleTarget - FSnake.Scale) * 5.0 * DeltaTime
  else
    FSnake.Scale := FSnake.ScaleTarget;

  // Smoothly update Thickness
  if Abs(FSnake.Thickness - FSnake.ThicknessTarget) > 0.1 then
    FSnake.Thickness := FSnake.Thickness + (FSnake.ThicknessTarget - FSnake.Thickness) * 5.0 * DeltaTime
  else
    FSnake.Thickness := FSnake.ThicknessTarget;

  // ==========================================================
  // MOVEMENT PHYSICS
  // ==========================================================
  TargetCenter.X := (FSnake.TargetRect.Left + FSnake.TargetRect.Right) / 2;
  TargetCenter.Y := (FSnake.TargetRect.Top + FSnake.TargetRect.Bottom) / 2;

  DirX := TargetCenter.X - FSnake.HeadPos.X;
  DirY := TargetCenter.Y - FSnake.HeadPos.Y;
  Dist := Sqrt(Sqr(DirX) + Sqr(DirY));

  if Dist > 0 then
  begin
    DirX := DirX / Dist;
    DirY := DirY / Dist;
  end;

  // --- EXIT MODE ---
  if FState = ssExiting then
  begin
    // Accelerate towards the off-screen target
    FSnake.Velocity.X := FSnake.Velocity.X + (DirX * 2.0);
    FSnake.Velocity.Y := FSnake.Velocity.Y + (DirY * 2.0);
  end
  // --- ENRAGED MODE (Handled in UpdateAI, just apply friction/limits here if needed) ---
  else if FState = ssEnraged then
  begin
    // Velocity is handled in UpdateAI to lock onto mouse
    // We just apply standard physics below (Position update, Friction)
  end
  else
  begin
    // --- STALKING MODE (Chasing Mouse) ---
    if FState = ssStalking then
    begin
      var MouseDirX := FSnake.MousePos.X - FSnake.HeadPos.X;
      var MouseDirY := FSnake.MousePos.Y - FSnake.HeadPos.Y;
      var MouseDist := Sqrt(Sqr(MouseDirX) + Sqr(MouseDirY));
      if MouseDist > 0 then
      begin
        MouseDirX := MouseDirX / MouseDist;
        MouseDirY := MouseDirY / MouseDist;
        FSnake.Velocity.X := FSnake.Velocity.X + (MouseDirX * 2.0);
        FSnake.Velocity.Y := FSnake.Velocity.Y + (MouseDirY * 2.0);
      end;
    end
    // --- RETREATING MODE (Running from Mouse) ---
    else if FState = ssRetreating then
    begin
      // Physics handled in UpdateAI via velocity manipulation
      // Just apply friction here (handled globally at the end of this proc)
    end
    else
    begin
      // --- ORBIT / APPROACH MODE (Normal Movement) ---
      if FSnake.TargetRect.Contains(FSnake.HeadPos) then
      begin
        // Inside target: Apply "braking" force to stay in bounds
        FSnake.Velocity.X := FSnake.Velocity.X - (DirX * 1.0);
        FSnake.Velocity.Y := FSnake.Velocity.Y - (DirY * 1.0);
      end
      else if Dist > 100 then
      begin
        // Far away: Accelerate towards target
        FSnake.Velocity.X := FSnake.Velocity.X + (DirX * 0.2);
        FSnake.Velocity.Y := FSnake.Velocity.Y + (DirY * 0.2);
      end;

      // --- ORBIT FORCE (Tangential) ---
      // Adds a perpendicular force to create a circular orbit
      FSnake.Velocity.X := FSnake.Velocity.X - (DirY * 0.25);
      FSnake.Velocity.Y := FSnake.Velocity.Y + (DirX * 0.25);
    end;
  end;

  // ==========================================================
  // GLOBAL WALL BOUNDARIES
  // ==========================================================
  // Apply wall repulsion to all states EXCEPT Exiting (and maybe Enraged, to keep him trapped?)
  // Let's keep Enraged inside too so he rages against the walls!
  if (FState <> ssExiting) then
  begin
    if FSnake.HeadPos.X < FBounds.Left + 10 then
      FSnake.Velocity.X := FSnake.Velocity.X + 0.5;
    if FSnake.HeadPos.Y < FBounds.Top + 10 then
      FSnake.Velocity.Y := FSnake.Velocity.Y + 0.5;
    if FSnake.HeadPos.X > FBounds.Right - 10 then
      FSnake.Velocity.X := FSnake.Velocity.X - 0.5;
    if FSnake.HeadPos.Y > FBounds.Bottom - 10 then
      FSnake.Velocity.Y := FSnake.Velocity.Y - 0.5;
  end;

  // --- APPLY PHYSICS TO POSITION ---
  FSnake.HeadPos.X := FSnake.HeadPos.X + FSnake.Velocity.X;
  FSnake.HeadPos.Y := FSnake.HeadPos.Y + FSnake.Velocity.Y;

  // --- FRICTION ---
  FSnake.Velocity.X := FSnake.Velocity.X * 0.95;
  FSnake.Velocity.Y := FSnake.Velocity.Y * 0.95;

  // --- TAIL MANAGEMENT ---
  FSnake.PathHistory.Add(FSnake.HeadPos);
  if FSnake.PathHistory.Count > 40 then
    FSnake.PathHistory.Delete(0);

  // --- ANIMATION TICKER ---
  FSnake.Phase := FSnake.Phase + 0.1;
end;

procedure TAliveHighlighter.Update(DeltaTime: Single; const Obstacles: TArray<TObstacle>);
begin
  // Run AI decision making first
  UpdateAI(Obstacles);
  // Then calculate physics
  UpdatePhysics(DeltaTime);
end;

procedure TAliveHighlighter.DrawCrackle(const Canvas: ISkCanvas; const Center: TPointF);
var
  I: Integer;
  Paint: ISkPaint;
  PathBuilder: ISkPathBuilder;
  P1, P2, Mid: TPointF;
begin
  // Draws a jagged lightning effect around the head
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.Color := TAlphaColors.Black;
  Paint.StrokeWidth := 1.0;
  Paint.Alpha := 100;

  for I := 0 to 6 do
  begin
    P1 := Center;
    P2.X := Center.X + (Random - 0.5) * 200;
    P2.Y := Center.Y + (Random - 0.5) * 200;

    Mid.X := (P1.X + P2.X) / 2 + (Random - 0.5) * 80;
    Mid.Y := (P1.Y + P2.Y) / 2 + (Random - 0.5) * 80;

    PathBuilder := TSkPathBuilder.Create;
    PathBuilder.MoveTo(P1.X, P1.Y);
    PathBuilder.LineTo(Mid.X, Mid.Y);
    PathBuilder.LineTo(P2.X, P2.Y);

    Canvas.DrawPath(PathBuilder.Snapshot, Paint);
  end;
end;

procedure TAliveHighlighter.Draw(const Canvas: ISkCanvas);
var
  I: Integer;
  PathBuilder: ISkPathBuilder;
  Path: ISkPath;
  Paint, CorePaint: ISkPaint;
  P1, P2: TPointF;
  SwayX, SwayY: Single;
  Filter: ISkImageFilter;
  HeadRadius: Single;
  Pulse: Single;
  AlphaRatio: Single;
  RectF: TRectF;
begin
  if (FSnake.Alpha < 0.01) then
    Exit;

  // Initialize Main Paint
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := FSnake.SnakeColor;
  Paint.AlphaF := FSnake.Alpha;

  // --- SETUP FX CHAIN ---
  // 1. Shadow
  if FSnake.ShadowBlur > 0 then
    Filter := TSkImageFilter.MakeDropShadow(FSnake.ShadowOffset.X, FSnake.ShadowOffset.Y, FSnake.ShadowBlur, FSnake.ShadowBlur, FSnake.ShadowColor);

  // 2. Glow (Compose with Shadow if exists)
  if FSnake.GlowAmount > 0 then
  begin
    var LGlowFilter := TSkImageFilter.MakeBlur(FSnake.GlowAmount, FSnake.GlowAmount);
    if Assigned(Filter) then
      Filter := TSkImageFilter.MakeCompose(Filter, LGlowFilter)
    else
      Filter := LGlowFilter;
  end;

  // Apply filters
  if Assigned(Filter) then
    Paint.ImageFilter := Filter;

  if FSnake.PathHistory.Count < 2 then
    Exit;

  // ========================================================================
  // STYLE: SNAKE (Organic Trail)
  // ========================================================================
  if FStyle = asSnake then
  begin
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeCap := TSkStrokeCap.Round;
    Paint.StrokeWidth := FSnake.Thickness;

    PathBuilder := TSkPathBuilder.Create;

    // Start from the TAIL (Oldest history point)
    P1 := FSnake.PathHistory[0];
    PathBuilder.MoveTo(P1.X, P1.Y);

    // Draw the Body to the HEAD
    for I := 1 to FSnake.PathHistory.Count - 1 do
    begin
      P2 := FSnake.PathHistory[I];

      // Calculate organic Sway offset using Sine waves
      SwayX := Sin(FSnake.Phase + (I * 0.2)) * 3.0 * FSnake.Scale;
      SwayY := Cos(FSnake.Phase + (I * 0.2)) * 3.0 * FSnake.Scale;

      PathBuilder.LineTo(P2.X + SwayX, P2.Y + SwayY);
    end;

    Path := PathBuilder.Snapshot;
    Canvas.DrawPath(Path, Paint);

    // Draw Lightning effect if attacking or Enraged
    if (FState = ssBiting) or (FState = ssEnraged) then
      DrawCrackle(Canvas, FSnake.HeadPos);
  end

  // ========================================================================
  // STYLE: ENERGY BEAM (Jittery Laser)
  // ========================================================================
  else if FStyle = asEnergyBeam then
  begin
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.StrokeCap := TSkStrokeCap.Butt; // Flat caps for laser look
    Paint.StrokeWidth := FSnake.Thickness;

    PathBuilder := TSkPathBuilder.Create;

    P1 := FSnake.PathHistory[0];
    PathBuilder.MoveTo(P1.X, P1.Y);

    for I := 1 to FSnake.PathHistory.Count - 1 do
    begin
      P2 := FSnake.PathHistory[I];
      // Add jitter for "electric" feel
      PathBuilder.LineTo(P2.X + (Random - 0.5), P2.Y + (Random - 0.5));
    end;

    Path := PathBuilder.Snapshot;

    // 1. Draw Outer Glow (Color)
    Canvas.DrawPath(Path, Paint);

    // 2. Draw White Core (Inner bright line)
    CorePaint := TSkPaint.Create;
    CorePaint.Style := TSkPaintStyle.Stroke;
    CorePaint.StrokeCap := TSkStrokeCap.Butt;
    CorePaint.StrokeWidth := FSnake.Thickness * 0.4; // Thinner core
    CorePaint.Color := TAlphaColors.White;
    CorePaint.AlphaF := FSnake.Alpha;
    // No blur on core to keep it sharp
    CorePaint.ImageFilter := nil;

    Canvas.DrawPath(Path, CorePaint);
  end

  // ========================================================================
  // STYLE: FIREFLY (Particle Trail)
  // ========================================================================
  else if FStyle = asFirefly then
  begin
    Paint.Style := TSkPaintStyle.Fill;

    // 1. Draw Trail (Fading particles)
    for I := 0 to FSnake.PathHistory.Count - 1 do
    begin
      // Calculate fade ratio (0.0 at tail, 1.0 at head)
      AlphaRatio := I / FSnake.PathHistory.Count;

      // Set Alpha based on ratio
      Paint.AlphaF := FSnake.Alpha * AlphaRatio;

      // Size decreases towards tail
      HeadRadius := (FSnake.Thickness * 0.8) * AlphaRatio;
      if HeadRadius < 1.0 then
        HeadRadius := 1.0;

      // Draw Circle
      RectF := TRectF.Create(FSnake.PathHistory[I].X - HeadRadius, FSnake.PathHistory[I].Y - HeadRadius, FSnake.PathHistory[I].X + HeadRadius, FSnake.PathHistory[I].Y + HeadRadius);
      Canvas.DrawOval(RectF, Paint);
    end;

    // 2. Draw Head (Pulsing)
    Pulse := 1.0 + (Sin(FSnake.Phase * 0.2) * 0.3); // Pulse size by +/- 30%
    HeadRadius := (FSnake.Thickness * 1.5) * Pulse;

    Paint.AlphaF := FSnake.Alpha;
    RectF := TRectF.Create(FSnake.HeadPos.X - HeadRadius, FSnake.HeadPos.Y - HeadRadius, FSnake.HeadPos.X + HeadRadius, FSnake.HeadPos.Y + HeadRadius);
    Canvas.DrawOval(RectF, Paint);
  end;
end;

end.

