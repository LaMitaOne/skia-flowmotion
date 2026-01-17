{*******************************************************************************
  uSkFlowButtons
********************************************************************************
  An advanced animated button panel system for Delphi FMX utilizing Skia4Delphi.

  Features:
  - Customizable button animations (Scale, Fade, Rotate, Slide, Elastic).
  - Multiple layout modes (Grid, Radial, Horizontal, Vertical).
  - Background thread driving animations to keep the UI responsive.
  - Global animation manager handling multiple instances of the panel.
  - Skia-accelerated rendering with hardware acceleration.

  part of skFlowmotion

  written by: Lara Miriam Tamy Reschke
*******************************************************************************}

unit uSkFlowButtons;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math,
  System.Generics.Collections, System.SyncObjs, FMX.Types, FMX.Controls,
  FMX.Graphics, FMX.Forms, System.Skia, FMX.Skia, System.UITypes;

const
  { Target frames per second for the animation loop }
  BUTTON_ANIM_FPS = 60;
  { Time in milliseconds per frame (16ms for 60FPS) }
  BUTTON_FRAME_TIME = 1000 div BUTTON_ANIM_FPS;

type
  { Available transition effects for button state changes }
  TButtonEffect = (beNone, bebtnFade, beScale, beScaleElastic, beSlideLeft, beSlideRight, beRotateIn);

  { Visual states of a button determining the current animation target }
  TButtonState = (bsIdle, bsHover, bsDown, bsEntering, bsExiting);

  { Layout strategies for arranging buttons within the panel }
  TPanelLayout = (plGrid, plRadial, plHorizontal, plVertical);

  { Visual styles for the panel background }
  TPanelStyle = (psNone, psDark, psGlass);

  { Forward declarations }
  TFlowButton = class;

  TFlowButtonPanel = class;

  { Event type triggered when a button is clicked }
  TOnFlowButtonClick = procedure(Sender: TObject; Button: TFlowButton) of object;

  { ------------------------------------------------------------------
    TFlowButton
    ------------------------------------------------------------------ }
  TFlowButton = class
  private
    FOwner: TFlowButtonPanel;
    FID: Integer;
    FPicture: ISkImage;
    FCaption: string;
    FHint: string;
    FTagString: string;
    FEnabled: Boolean;
    FCurrentState: TButtonState;
    FTargetState: TButtonState;
    FAnimProgress: Single;
    FCurrentX: Single;
    FCurrentY: Single;
    FCurrentScale: Single;
    FCurrentAlpha: Single;
    FCurrentRotation: Single;
    FTargetX: Single;
    FTargetY: Single;
    FTargetScale: Single;
    FTargetAlpha: Single;
    FTargetRotation: Single;
    FIdleEffect: TButtonEffect;
    FHoverEffect: TButtonEffect;
    FDownEffect: TButtonEffect;
    function GetEffectForState(State: TButtonState): TButtonEffect;
  public
    { Creates a new button instance }
    constructor Create(AOwner: TFlowButtonPanel; AnID: Integer);
    destructor Destroy; override;

    { Updates internal animation physics (called by the Animation Thread) }
    procedure UpdateAnimation(const DeltaTime: Double);

    { Renders the button onto the provided Skia Canvas }
    procedure Draw(ACanvas: ISkCanvas; const GlobalRect: TRectF; GlobalRotation: Single);

    { Sets the target state, triggering specific animations }
    procedure SetState(AState: TButtonState);

    { Properties }
    property ID: Integer read FID;
    property Caption: string read FCaption write FCaption;
    property Hint: string read FHint write FHint;
    property TagString: string read FTagString write FTagString;
    property Picture: ISkImage read FPicture write FPicture;
    property Enabled: Boolean read FEnabled write FEnabled;
    property IdleEffect: TButtonEffect read FIdleEffect write FIdleEffect;
    property HoverEffect: TButtonEffect read FHoverEffect write FHoverEffect;
    property DownEffect: TButtonEffect read FDownEffect write FDownEffect;
  end;

  { ------------------------------------------------------------------
    TFlowButtonAnimThread
    ------------------------------------------------------------------ }
  TFlowButtonAnimThread = class(TThread)
  private
    FOwner: TComponent;
    FActivePanels: TList<TFlowButtonPanel>;
    FLastTick: Cardinal;
    FStopRequested: Boolean;
    FCritical: TCriticalSection;
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    { Signals the thread to halt the loop }
    procedure Stop;

    { Registers a panel to receive animation updates }
    procedure RegisterPanel(Panel: TFlowButtonPanel);

    { Removes a panel from animation updates }
    procedure UnregisterPanel(Panel: TFlowButtonPanel);
  end;

  { ------------------------------------------------------------------
    TFlowButtonPanel
    ------------------------------------------------------------------ }
  TFlowButtonPanel = class(TSkCustomControl)
  private
    FButtons: TList<TFlowButton>;
    FNextID: Integer;
    FLayout: TPanelLayout;
    FPanelStyle: TPanelStyle;
    FPanelRotation: Single;
    FButtonSize: Single;
    FGap: Single;
    FRadius: Single;
    FHoveredButton: TFlowButton;
    FDownedButton: TFlowButton;
    FOnButtonClick: TOnFlowButtonClick;
    FVisible: Boolean;
    procedure SetFlowVisible(const Value: Boolean);
    function GetButtonCount: Integer;
    function GetButtonAt(X, Y: Single): TFlowButton;
    procedure CalculateLayoutTargets;
    procedure DrawBackground(ACanvas: ISkCanvas; const R: TRectF);
  protected
    { Skia Paint Override }
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;

    { Mouse Interaction }
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Adds a new button to the panel with an optional image }
    function AddButton(const ACaption, AHint, ATagString: string; APic: ISkImage = nil): TFlowButton;

    { Shows the panel centered at specific coordinates }
    procedure ShowAt(X, Y: Single);

    { Hides the panel with an exit animation }
    procedure PanelHide;

    { Configuration Properties }
    property Layout: TPanelLayout read FLayout write FLayout;
    property PanelStyle: TPanelStyle read FPanelStyle write FPanelStyle;
    property PanelRotation: Single read FPanelRotation write FPanelRotation;
    property ButtonSize: Single read FButtonSize write FButtonSize;
    property Gap: Single read FGap write FGap;
    property Radius: Single read FRadius write FRadius;
    property Visible: Boolean read FVisible write SetFlowVisible;
    property OnButtonClick: TOnFlowButtonClick read FOnButtonClick write FOnButtonClick;
    property ButtonCount: Integer read GetButtonCount;
  end;

implementation

var
  { Global singleton instance for the animation thread }
  GAnimThread: TFlowButtonAnimThread = nil;

{ Helper: Convert Degrees to Radians }
function DegToRad(Deg: Single): Single;
begin
  Result := Deg * (PI / 180);
end;

{ ========================================================================
  TFlowButtonAnimThread
  ======================================================================== }

constructor TFlowButtonAnimThread.Create(AOwner: TComponent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FActivePanels := TList<TFlowButtonPanel>.Create;
  FCritical := TCriticalSection.Create;
  FLastTick := TThread.GetTickCount;
  FStopRequested := False;
  Suspended := False; { Start immediately }
end;

destructor TFlowButtonAnimThread.Destroy;
begin
  FStopRequested := True;
  if not Suspended then
    WaitFor
  else
    Terminate;
  FActivePanels.Free;
  FCritical.Free;
  inherited;
end;

procedure TFlowButtonAnimThread.Stop;
begin
  FStopRequested := True;
end;

procedure TFlowButtonAnimThread.RegisterPanel(Panel: TFlowButtonPanel);
begin
  FCritical.Enter;
  try
    if FActivePanels.IndexOf(Panel) = -1 then
      FActivePanels.Add(Panel);
  finally
    FCritical.Leave;
  end;
end;

procedure TFlowButtonAnimThread.UnregisterPanel(Panel: TFlowButtonPanel);
begin
  FCritical.Enter;
  try
    FActivePanels.Remove(Panel);
  finally
    FCritical.Leave;
  end;
end;

procedure TFlowButtonAnimThread.Execute;
var
  NowTick, LastTick, ElapsedMS, SleepTime: Cardinal;
  DeltaTime: Double;
  LList: TList<TFlowButtonPanel>;
begin
  LastTick := GetTickCount;

  while not Terminated and not FStopRequested do
  begin
    NowTick := GetTickCount;
    ElapsedMS := NowTick - LastTick;
    LastTick := NowTick;

    { Calculate sleep time to maintain ~60 FPS }
    SleepTime := 0;
    if ElapsedMS < BUTTON_FRAME_TIME then
      SleepTime := 16 + (BUTTON_FRAME_TIME - ElapsedMS);

    if FStopRequested then
      Break;

    if SleepTime > 0 then
      Sleep(SleepTime);

    { SAFE SNAPSHOT PATTERN
      1. Lock the list and create a local copy.
      2. Release the lock immediately.
      3. Iterate over the local copy.
      This prevents deadlocks if TThread.Synchronize tries to access the panel list. }
    FCritical.Enter;
    try
      if FActivePanels <> nil then
      begin
        LList := TList<TFlowButtonPanel>.Create;
        LList.AddRange(FActivePanels);
      end
      else
        LList := nil;
    finally
      FCritical.Leave;
    end;

    { 2. Process animations WITHOUT holding the lock }
    if LList <> nil then
    begin
      try
        DeltaTime := ElapsedMS / 1000.0;
        for var Panel in LList do
        begin
          if Assigned(Panel) and Panel.Visible then
          begin
            { Update Animations }
            if Assigned(Panel.FButtons) then
            begin
              for var B in Panel.FButtons do
                B.UpdateAnimation(DeltaTime);
            end;

            { Repaint (Synchronize is safe here because we released the Lock) }
            TThread.Synchronize(nil,
              procedure
              begin
                if Assigned(Panel) and Panel.Visible then
                  Panel.Repaint;
              end);
          end;
        end;
      finally
        LList.Free;
      end;
    end;

    { 3. Process standard Windows messages (CheckSynchronize) }
    CheckSynchronize(10);
  end;
end;

{ ========================================================================
  TFlowButton
  ======================================================================== }

constructor TFlowButton.Create(AOwner: TFlowButtonPanel; AnID: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FID := AnID;
  FCurrentState := bsIdle;
  FTargetState := bsIdle;
  FEnabled := True;

  { Default Transform Values }
  FCurrentScale := 1.0;
  FTargetScale := 1.0;
  FCurrentAlpha := 1.0;
  FTargetAlpha := 1.0;

  { Default Effects }
  FIdleEffect := beNone;
  FHoverEffect := beScale;
  FDownEffect := beScaleElastic;

  { Start in the center of the owner }
  if FOwner.Width > 0 then
    FCurrentX := FOwner.Width / 2
  else
    FCurrentX := 200;
  if FOwner.Height > 0 then
    FCurrentY := FOwner.Height / 2
  else
    FCurrentY := 200;
end;

destructor TFlowButton.Destroy;
begin
  FPicture := nil;
  inherited;
end;

function TFlowButton.GetEffectForState(State: TButtonState): TButtonEffect;
begin
  case State of
    bsIdle:
      Result := FIdleEffect;
    bsHover:
      Result := FHoverEffect;
    bsDown:
      Result := FDownEffect;
    bsEntering:
      Result := beScaleElastic; { Default entry animation }
    bsExiting:
      Result := bebtnFade;         { Default exit animation }
  else
    Result := beNone;
  end;
end;

procedure TFlowButton.SetState(AState: TButtonState);
begin
  if FTargetState = AState then
    Exit;
  FTargetState := AState;
  FAnimProgress := 0.0; { Reset animation timer }
end;

procedure TFlowButton.UpdateAnimation(const DeltaTime: Double);
var
  Speed: Single;
  Eff: TButtonEffect;
begin
  { Increment animation progress }
  if FAnimProgress < 1.0 then
  begin
    FAnimProgress := FAnimProgress + (DeltaTime * 5.0);
    if FAnimProgress > 1.0 then
      FAnimProgress := 1.0;
  end;

  Eff := GetEffectForState(FTargetState);

  { Determine Target Values based on State }
  case FTargetState of
    bsIdle, bsHover, bsDown:
      begin
        FTargetAlpha := 1.0;
        FTargetScale := 1.0;
        FTargetRotation := 0.0;
        if FTargetState = bsHover then
          FTargetScale := 1.2; { Grow slightly on hover }
        if FTargetState = bsDown then
          FTargetScale := 0.9; { Shrink slightly on click }
      end;
    bsEntering:
      begin
        FTargetAlpha := 1.0;
        FTargetScale := 1.0;
        FTargetRotation := 0.0;
        { Effect-specific overrides }
        if Eff = bebtnFade then
          FCurrentAlpha := FAnimProgress; { Manual override for alpha lerp }
        if Eff = beScale then
          FTargetScale := FAnimProgress;
        if Eff = beRotateIn then
          FTargetRotation := (1.0 - FAnimProgress) * 360;
      end;
    bsExiting:
      begin
        FTargetAlpha := 0.0;
        FTargetScale := 1.0;
        FTargetRotation := 0.0;
        if Eff = bebtnFade then
          FCurrentAlpha := 1.0 - FAnimProgress;
        if Eff = beScale then
          FTargetScale := 1.0 - FAnimProgress;
      end;
  end;

  { Linear Interpolation (Lerp) towards targets }
  Speed := DeltaTime * 10.0;
  FCurrentX := FCurrentX + (FTargetX - FCurrentX) * Speed;
  FCurrentY := FCurrentY + (FTargetY - FCurrentY) * Speed;
  FCurrentScale := FCurrentScale + (FTargetScale - FCurrentScale) * Speed;
  FCurrentAlpha := FCurrentAlpha + (FTargetAlpha - FCurrentAlpha) * Speed;
  FCurrentRotation := FCurrentRotation + (FTargetRotation - FCurrentRotation) * Speed;
end;

procedure TFlowButton.Draw(ACanvas: ISkCanvas; const GlobalRect: TRectF; GlobalRotation: Single);
var
  Paint: ISkPaint;
  R: TRectF;
  CX, CY: Single;
begin
  if FCurrentAlpha <= 0.01 then
    Exit;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Alpha := Round(255 * FCurrentAlpha);

  R := TRectF.Create(FCurrentX, FCurrentY, FCurrentX + FOwner.ButtonSize, FCurrentY + FOwner.ButtonSize);
  CX := (R.Left + R.Right) / 2;
  CY := (R.Top + R.Bottom) / 2;

  ACanvas.Save;

  { 1. Apply Scale (Centered) }
  if Abs(FCurrentScale - 1.0) > 0.001 then
  begin
    ACanvas.Translate(CX, CY);
    ACanvas.Scale(FCurrentScale, FCurrentScale);
    ACanvas.Translate(-CX, -CY);
  end;

  { 2. Apply Rotation (Centered) }
  if Abs(FCurrentRotation) > 0.1 then
  begin
    ACanvas.Translate(CX, CY);
    ACanvas.Rotate(FCurrentRotation);
    ACanvas.Translate(-CX, -CY);
  end;

  { 3. Draw Button Background }
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := $FF222222; { Default Dark Grey }
  if FOwner.FHoveredButton = Self then
    Paint.Color := $FF444444; { Lighter on Hover }
  if FOwner.FDownedButton = Self then
    Paint.Color := $FF666666; { Lighter on Down }

  ACanvas.DrawRoundRect(R, 8.0, 8.0, Paint);

  { 4. Draw Icon/Image }
  if Assigned(FPicture) then
  begin
    Paint.Style := TSkPaintStyle.Fill;
    ACanvas.DrawImageRect(FPicture, R, TSkSamplingOptions.High, Paint);
  end;

  ACanvas.Restore;
end;

{ ========================================================================
  TFlowButtonPanel
  ======================================================================== }

constructor TFlowButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := TList<TFlowButton>.Create;
  FLayout := plGrid;
  FPanelStyle := psDark;
  FPanelRotation := 0;
  FButtonSize := 64;
  FGap := 10;
  FRadius := 150;

  { Initialize Global Animation Thread if not exists }
  if not Assigned(GAnimThread) then
    GAnimThread := TFlowButtonAnimThread.Create(Self);
  GAnimThread.RegisterPanel(Self);

  HitTest := True;
end;

destructor TFlowButtonPanel.Destroy;
begin
  if Assigned(GAnimThread) then
    GAnimThread.UnregisterPanel(Self);
  FButtons.Free;
  inherited;
end;

procedure TFlowButtonPanel.SetFlowVisible(const Value: Boolean);
begin
  if FVisible = Value then
    Exit;
  FVisible := Value;

  if Value then
  begin
    CalculateLayoutTargets;
    { Trigger entering animation for all buttons }
    for var B in FButtons do
      B.SetState(bsEntering);
  end
  else
  begin
    { Trigger exiting animation }
    for var B in FButtons do
      B.SetState(bsExiting);
  end;
end;

function TFlowButtonPanel.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TFlowButtonPanel.AddButton(const ACaption, AHint, ATagString: string; APic: ISkImage): TFlowButton;
begin
  Result := TFlowButton.Create(Self, FNextID);
  Inc(FNextID);
  Result.FCaption := ACaption;
  Result.FHint := AHint;
  Result.FTagString := ATagString;
  Result.FPicture := APic;

  { Initialize position at center (will animate to layout target) }
  if Width > 0 then
    Result.FCurrentX := Width / 2
  else
    Result.FCurrentX := 200;
  if Height > 0 then
    Result.FCurrentY := Height / 2
  else
    Result.FCurrentY := 200;

  FButtons.Add(Result);

  if Visible then
    CalculateLayoutTargets;
end;

procedure TFlowButtonPanel.ShowAt(X, Y: Single);
var
  W, H: Single;
begin
  W := Width;
  H := Height;
  { Default size if not set }
  if W < 10 then
    W := 400;
  if H < 10 then
    H := 400;

  SetBounds(Round(X - (W / 2)), Round(Y - (H / 2)), Round(W), Round(H));
  Visible := True;
end;

procedure TFlowButtonPanel.PanelHide;
begin
  Visible := False;
end;

procedure TFlowButtonPanel.CalculateLayoutTargets;
var
  Cx, Cy, Angle, Step: Single;
  i, Row, Col: Integer;
  B: TFlowButton;
  GridW, StartX, StartY, TotalW, TotalH: Single;
begin
  if FButtons.Count = 0 then
    Exit;
  if Width <= 0 then
    Exit;
  if Height <= 0 then
    Exit;

  Cx := Width / 2;
  Cy := Height / 2;

  case FLayout of
    plRadial:
      begin
        Angle := 0;
        Step := 360 / FButtons.Count;
        for i := 0 to FButtons.Count - 1 do
        begin
          B := FButtons[i];
          { Polar to Cartesian conversion }
          B.FTargetX := Cx + Cos(DegToRad(Angle)) * FRadius - (FButtonSize / 2);
          B.FTargetY := Cy + Sin(DegToRad(Angle)) * FRadius - (FButtonSize / 2);
          Angle := Angle + Step;
        end;
      end;

    plGrid:
      begin
        Col := 0;
        Row := 0;
        GridW := (Ceil(Sqrt(FButtons.Count)) * (FButtonSize + FGap));
        StartX := Cx - (GridW / 2);
        for i := 0 to FButtons.Count - 1 do
        begin
          B := FButtons[i];
          B.FTargetX := StartX + Col * (FButtonSize + FGap);
          B.FTargetY := (Cy - (FButtonSize + FGap)) + Row * (FButtonSize + FGap);
          Inc(Col);
          if Col >= 4 then { Hardcoded 4 columns for grid }
          begin
            Col := 0;
            Inc(Row);
          end;
        end;
      end;

    plHorizontal:
      begin
        TotalW := FButtons.Count * (FButtonSize + FGap) - FGap;
        StartX := Cx - (TotalW / 2);
        for i := 0 to FButtons.Count - 1 do
        begin
          B := FButtons[i];
          B.FTargetX := StartX + i * (FButtonSize + FGap);
          B.FTargetY := Cy - (FButtonSize / 2);
        end;
      end;

    plVertical:
      begin
        TotalH := FButtons.Count * (FButtonSize + FGap) - FGap;
        StartY := Cy - (TotalH / 2);
        for i := 0 to FButtons.Count - 1 do
        begin
          B := FButtons[i];
          B.FTargetX := Cx - (FButtonSize / 2);
          B.FTargetY := StartY + i * (FButtonSize + FGap);
        end;
      end;
  end;
end;

procedure TFlowButtonPanel.DrawBackground(ACanvas: ISkCanvas; const R: TRectF);
var
  Paint: ISkPaint;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Alpha := 200;

  case FPanelStyle of
    psNone:
      ; { No background }
    psDark:
      begin
        Paint.Color := $AA000000;
        ACanvas.DrawRoundRect(R, 10, 10, Paint);
      end;
    psGlass:
      begin
        Paint.Color := $AA222222;
        ACanvas.DrawRoundRect(R, 10, 10, Paint);
        Paint.Style := TSkPaintStyle.Stroke;
        Paint.StrokeWidth := 2;
        Paint.Color := $FFFFFFFF;
        Paint.Alpha := 100;
        ACanvas.DrawRoundRect(R, 10, 10, Paint);
      end;
  end;
end;

procedure TFlowButtonPanel.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  Cx, Cy: Single;
  RenderList: TList<TFlowButton>;
begin
  ACanvas.Clear(TAlphaColors.Null);
  if not Visible then
    Exit;

  Cx := Width / 2;
  Cy := Height / 2;

  { Draw Background (Optional Rotation) }
  if FPanelStyle <> psNone then
  begin
    ACanvas.Save;
    if FPanelRotation <> 0 then
    begin
      ACanvas.Translate(Cx, Cy);
      ACanvas.Rotate(FPanelRotation);
      ACanvas.Translate(-Cx, -Cy);
    end;
    DrawBackground(ACanvas, RectF(0, 0, Width, Height));
    ACanvas.Restore;
  end;

  { Draw Buttons }
  ACanvas.Save;
  if FPanelRotation <> 0 then
  begin
    { Apply same rotation to buttons as background }
    ACanvas.Translate(Cx, Cy);
    ACanvas.Rotate(FPanelRotation);
    ACanvas.Translate(-Cx, -Cy);
  end;

  { Iterate over a copy of the list for thread safety during render }
  RenderList := TList<TFlowButton>.Create;
  try
    RenderList.AddRange(FButtons);
    for var B in RenderList do
      B.Draw(ACanvas, RectF(0, 0, Width, Height), FPanelRotation);
  finally
    RenderList.Free;
  end;
  ACanvas.Restore;
end;

function TFlowButtonPanel.GetButtonAt(X, Y: Single): TFlowButton;
var
  P: TPointF;
  R: TRectF;
  Cx, Cy, Rad, DX, DY: Single;
begin
  Result := nil;
  P := TPointF.Create(X, Y);
  Cx := Width / 2;
  Cy := Height / 2;

  { If the panel is rotated, we must "un-rotate" the mouse coordinates
    to match the un-rotated button rectangles. }
  if FPanelRotation <> 0 then
  begin
    Rad := DegToRad(-FPanelRotation); { Negative for inverse rotation }
    DX := P.X - Cx;
    DY := P.Y - Cy;
    { 2D Rotation Matrix application }
    P.X := Cx + (DX * Cos(Rad) - DY * Sin(Rad));
    P.Y := Cy + (DX * Sin(Rad) + DY * Cos(Rad));
  end;

  { Check collision with button rects }
  for var B in FButtons do
  begin
    R := RectF(B.FCurrentX, B.FCurrentY, B.FCurrentX + FButtonSize, B.FCurrentY + FButtonSize);
    if R.Contains(P) then
    begin
      Result := B;
      Break;
    end;
  end;
end;

procedure TFlowButtonPanel.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Btn: TFlowButton;
begin
  inherited;
  Btn := GetButtonAt(X, Y);

  { State Change Handling }
  if Btn <> FHoveredButton then
  begin
    { Reset old button }
    if Assigned(FHoveredButton) then
      FHoveredButton.SetState(bsIdle);

    { Set new button }
    FHoveredButton := Btn;
    if Assigned(FHoveredButton) and FHoveredButton.Enabled then
      FHoveredButton.SetState(bsHover);

    { Hint Handling }
    if Assigned(FHoveredButton) and (FHoveredButton.Hint <> '') then
    begin
      Application.Hint := FHoveredButton.Hint;
      Application.ShowHint := True;
    end
    else
      Application.CancelHint;
  end;
end;

procedure TFlowButtonPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Btn: TFlowButton;
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    Btn := GetButtonAt(X, Y);
    if Assigned(Btn) and Btn.Enabled then
    begin
      FDownedButton := Btn;
      Btn.SetState(bsDown);
    end;
  end;
end;

procedure TFlowButtonPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Btn: TFlowButton;
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    Btn := GetButtonAt(X, Y);

    { Check if we released the mouse on the same button we pressed (Click Event) }
    if (Btn = FDownedButton) and Assigned(FDownedButton) then
    begin
      FDownedButton.SetState(bsHover);
      if Assigned(FOnButtonClick) then
        FOnButtonClick(Self, FDownedButton);
    end
    else if Assigned(FDownedButton) then
    begin
      { Released outside, revert to idle }
      FDownedButton.SetState(bsIdle);
    end;
    FDownedButton := nil;
  end;
end;

procedure TFlowButtonPanel.DoMouseLeave;
begin
  inherited;
  if Assigned(FHoveredButton) then
    FHoveredButton.SetState(bsIdle);
  FHoveredButton := nil;
  Application.CancelHint;
  FDownedButton := nil;
end;

initialization

finalization
  { Cleanup the global animation thread }
  if Assigned(GAnimThread) then
  begin
    GAnimThread.Stop;
    GAnimThread.WaitFor;
    FreeAndNil(GAnimThread);
  end;

end.

