{*******************************************************************************
  SkFlowEffects
  - Part of the skFlowmotion library
  - Provides a collection of visual effects optimized for FMX & Skia
  - Backgrounds: Holographic, Matrix Rain, Fading Transitions, Neural Networks
  - UI Elements: Tech Brackets, Selection Glows
  - Particle Systems: Simple Squares, Image-based Debris
  - Post-FX: Cyberpunk Glitch / Chromatic Aberration

  written by: Lara Miriam Tamy Reschke
*******************************************************************************}

unit uSkFlowEffects;

interface

uses
  System.Types, System.UITypes, System.Math, System.SysUtils, System.Classes,
  System.Generics.Collections, FMX.Graphics, FMX.Controls, System.Skia, FMX.Skia,
  System.SyncObjs;

const
  MAX_BG_FADE_STEPS = 2000; // Number of frames for a full background crossfade

type
  { Enumeration of available background styles }
  TBackgroundEffect = (beHolographic, beRealMatrix, beFade, beNeuralLinks);

  { Forward declaration }
  TSkFlowControl = class(TComponent);

  { ==========================================
    DATA STRUCTURES
    ========================================== }

  { Record representing a single falling column of text (Matrix Rain) }
  TMatrixColumn = record
    X: Single;                 // Screen X position
    Y: Single;                 // Screen Y position (Top of the column)
    Speed: Single;             // Fall speed factor
    Length: Integer;           // Number of characters in the column
    Chars: string;             // The actual characters to render
    TargetImageIndex: Integer; // ID of the item this column represents
  end;

  { Simple square particle for explosions/sparks }
  TParticle = record
    X, Y: Single;
    VX, VY: Single;
    Life: Single;              // 1.0 = Born, 0.0 = Dead
    Color: TAlphaColor;
    Size: Single;
  end;

  { Advanced particle that renders an actual image/icon }
  TSmallPicParticle = record
    Index: Integer;            // Index to retrieve the image
    X, Y: Single;
    VX, VY: Single;
    Life: Single;              // 1.0 to 0.0
    Angle: Single;             // Current rotation
    VAngle: Single;            // Angular velocity
  end;

  { Connection line for the Neural Network effect }
  TNeuralLink = record
    StartX, StartY: Single;
    EndX, EndY: Single;
    Alpha: Single;
    LineColor: TAlphaColor;
  end;

  { Callback: Retrieves text data for a specific Matrix column }
  TGetMatrixDataFunc = reference to function(Index: Integer): string;

  { Callback: Retrieves a Skia image for a particle }
  TGetSmallPicSkImageFunc = reference to function(Index: Integer): ISkImage;

  { ==========================================
    PROCEDURE SIGNATURES
    ========================================== }

  { --- MATRIX RAIN EFFECT --- }
procedure InitMatrix(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, AMatrixFontSize: Single; ImageCount: Integer; GetDataFunc: TGetMatrixDataFunc);

procedure UpdateMatrixPhysics(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, MatrixSpeed, MatrixFontSize: Single; ImageCount: Integer; Animated: Boolean; GetDataFunc: TGetMatrixDataFunc; DeltaTime: Double);

procedure DrawMatrixBackground(ACanvas: ISkCanvas; const MatrixCols: TArray<TMatrixColumn>; MatrixFont: TFont; MatrixColor, MatrixHeadColor: TAlphaColor; MatrixFontSize, MatrixSpeed: Single; AHeight: Single; Animated: Boolean);

  { --- BACKGROUND MANAGEMENT --- }
function DrawFadeBackground(ACanvas: ISkCanvas; FBackgroundSkImage: ISkImage; FBackgroundNEWSkImage: ISkImage; FBackgroundFadestage: Integer; FBackgroundColor: TAlphaColor; const ADest: TRectF; const Paint: ISkPaint; owner: TSkCustomControl): Integer;

function GetDominantColor(const Img: ISkImage): TAlphaColor;

procedure DrawHolographicBackground(ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint; BackgroundSkImage: ISkImage; var GridOffsetY: Single; Animated: Boolean);

  { --- UI ELEMENTS --- }
procedure DrawTechBrackets(ACanvas: ISkCanvas; const VisualRect: TRectF; Paint: ISkPaint; BracketWidth: Integer; ImageAlpha: Byte; IsSelected: Boolean; GlowColor, HotTrackColor: TAlphaColor; GlowWidth, HotTrackWidth: Integer);

  { --- SIMPLE PARTICLES --- }
procedure UpdateSimpleParticles(var List: TList<TParticle>; DeltaTime: Double);

procedure DrawSimpleParticles(ACanvas: ISkCanvas; var List: TList<TParticle>);

procedure SpawnParticles(var ParticlesList: TList<TParticle>; X, Y: Single; Count: Integer; Color: TAlphaColor);

  { --- IMAGE PARTICLES --- }
procedure UpdateSmallPicParticles(var List: TList<TSmallPicParticle>; DeltaTime: Double; GetBmp: TGetSmallPicSkImageFunc);

procedure DrawSmallPicParticles(ACanvas: ISkCanvas; var List: TList<TSmallPicParticle>; GetBmp: TGetSmallPicSkImageFunc);

  { --- POST PROCESSING --- }
procedure DrawCyberpunkGlitch(ACanvas: ISkCanvas; const Img: ISkImage; const Rect: TRectF; Intensity: Single; BaseSampling: TSkSamplingOptions);

  { --- NEURAL LINKS --- }
procedure DrawNeuralLinks(ACanvas: ISkCanvas; Center: TPointF; Links: TList<TNeuralLink>; AFont: ISkFont; MaxLineWidth: Single);

var
  GNeuralLinksLock: TCriticalSection; // Thread safety for the Neural Links list

implementation

var
  GFontCache: TDictionary<string, ISkFont>; // Cache for optimized Skia font creation
  GFontCacheLock: TCriticalSection;
  GDominantColorSurface: ISkSurface;
  GDominantColorLock: TCriticalSection;

{ ========================================================================
  GLOBAL INITIALIZATION & HELPERS
  ======================================================================== }

procedure InitGlobals;
begin
  GFontCache := TDictionary<string, ISkFont>.Create;
  GFontCacheLock := TCriticalSection.Create;
  GDominantColorLock := TCriticalSection.Create;
end;

procedure FreeGlobals;
begin
  GFontCache.Free;
  GFontCacheLock.Free;
  GDominantColorLock.Free;
end;

{ Helper: Retrieves a cached Skia font to avoid repeated creation overhead }
function GetCachedSkFont(AFont: TFont; ASize: Single): ISkFont;
var
  CacheKey: string;
  LTypeface: ISkTypeface;
begin
  Result := nil;
  if AFont = nil then
    Exit;

  CacheKey := AFont.Family + '_' + FloatToStr(ASize);

  GFontCacheLock.Enter;
  try
    if not GFontCache.TryGetValue(CacheKey, Result) then
    begin
      LTypeface := TSkTypeface.MakeFromName(AFont.Family, TSkFontStyle.Normal);
      if Assigned(LTypeface) then
        Result := TSkFont.Create(LTypeface, ASize);

      if Assigned(Result) then
        GFontCache.Add(CacheKey, Result);
    end;
  finally
    GFontCacheLock.Leave;
  end;
end;

{ ========================================================================
  MATRIX RAIN EFFECT IMPLEMENTATION
  ======================================================================== }

procedure InitMatrix(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, AMatrixFontSize: Single; ImageCount: Integer; GetDataFunc: TGetMatrixDataFunc);
var
  i, ColsCount: Integer;
  SourceData: string;
begin
  if (AWidth <= 0) or (ImageCount = 0) then
    Exit;

  // Calculate column count based on font width
  if AMatrixFontSize <= 0 then
    ColsCount := Trunc(AWidth / 10)
  else
    ColsCount := Trunc(AWidth / (AMatrixFontSize * 0.6));

  // Safety limits
  if ColsCount < 1 then
    ColsCount := 1;
  if ColsCount > 300 then
    ColsCount := 300;

  SetLength(MatrixCols, ColsCount);

  for i := 0 to ColsCount - 1 do
  begin
    MatrixCols[i].X := i * (AMatrixFontSize * 0.6);
    // Start at random Y positions above screen for rain effect
    MatrixCols[i].Y := -Random(Trunc(AHeight * 2));
    MatrixCols[i].Speed := 5.0 + Random(15);

    // Assign random target image index for this column
    if ImageCount > 1 then
      MatrixCols[i].TargetImageIndex := Random(ImageCount - 1)
    else
      MatrixCols[i].TargetImageIndex := 0;

    // Retrieve data string via callback
    if Assigned(GetDataFunc) then
      SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex)
    else
      SourceData := 'NO_DATA';

    MatrixCols[i].Length := Length(SourceData);
    if MatrixCols[i].Length > 30 then
      MatrixCols[i].Length := 30;

    MatrixCols[i].chars := Copy(SourceData, 1, MatrixCols[i].Length);
  end;
end;

procedure UpdateMatrixPhysics(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, MatrixSpeed, MatrixFontSize: Single; ImageCount: Integer; Animated: Boolean; GetDataFunc: TGetMatrixDataFunc; DeltaTime: Double);
var
  i: Integer;
  SourceData: string;
begin
  if (Length(MatrixCols) = 0) or (ImageCount = 0) then
    Exit;
  if not Animated then
    Exit;

  for i := 0 to High(MatrixCols) do
  begin
    // Move column down
    MatrixCols[i].Y := MatrixCols[i].Y + (MatrixCols[i].Speed * DeltaTime * 3.0 * MatrixSpeed * 0.5);

    // Reset column if it goes off screen
    if MatrixCols[i].Y > (AHeight + (MatrixCols[i].Length * MatrixFontSize)) then
    begin
      MatrixCols[i].Y := -Random(Trunc(AHeight * 2));
      MatrixCols[i].Speed := 5.0 + Random(15);

      // Re-randomize target
      if (MatrixCols[i].TargetImageIndex < 0) or (MatrixCols[i].TargetImageIndex >= ImageCount) then
      begin
        if ImageCount > 1 then
          MatrixCols[i].TargetImageIndex := Random(ImageCount - 1)
        else
          MatrixCols[i].TargetImageIndex := 0;
      end;

      // Refresh text data
      if Assigned(GetDataFunc) then
      begin
        SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex);
        MatrixCols[i].Chars := Copy(SourceData, 1, 30);
        MatrixCols[i].Length := Length(MatrixCols[i].Chars);
      end;
    end;

    // Randomly change characters mid-fall for "decoding" effect
    if Assigned(GetDataFunc) and (MatrixCols[i].TargetImageIndex >= 0) and (MatrixCols[i].TargetImageIndex < ImageCount) then
    begin
      if Random(100) < 10 then
      begin
        SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex);
        MatrixCols[i].Chars := Copy(SourceData, 1, 30);
        MatrixCols[i].Length := Length(MatrixCols[i].Chars);
      end;
    end;
  end;
end;

procedure DrawMatrixBackground(ACanvas: ISkCanvas; const MatrixCols: TArray<TMatrixColumn>; MatrixFont: TFont; MatrixColor, MatrixHeadColor: TAlphaColor; MatrixFontSize, MatrixSpeed: Single; AHeight: Single; Animated: Boolean);
var
  i, j: Integer;
  LPaint: ISkPaint;
  LSkFont: ISkFont;
  CharY: Single;
  CurrentChar: string;
begin
  if Length(MatrixCols) = 0 then
    Exit;

  // Retrieve cached font for performance
  LSkFont := GetCachedSkFont(MatrixFont, MatrixFontSize);
  if not Assigned(LSkFont) then
    Exit;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Fill;

  { PASS 1: DRAW TRAIL (The fading tail) }
  LPaint.Color := MatrixColor;
  LPaint.AlphaF := 0.6;

  for i := 0 to High(MatrixCols) do
  begin
    for j := 1 to MatrixCols[i].Length - 1 do
    begin
      CharY := MatrixCols[i].Y + ((j - 1) * MatrixFontSize);
      // Culling: Only draw if within vertical bounds
      if (CharY > -MatrixFontSize) and (CharY < AHeight + MatrixFontSize) then
      begin
        if j <= Length(MatrixCols[i].Chars) then
          CurrentChar := MatrixCols[i].Chars[j]
        else
          CurrentChar := ' ';
        ACanvas.DrawSimpleText(CurrentChar, MatrixCols[i].X, CharY, LSkFont, LPaint);
      end;
    end;
  end;

  { PASS 2: DRAW HEADS (The bright leading character) }
  LPaint.Color := MatrixHeadColor;
  LPaint.AlphaF := 1.0;

  for i := 0 to High(MatrixCols) do
  begin
    if MatrixCols[i].Length > 0 then
    begin
      j := MatrixCols[i].Length;
      CharY := MatrixCols[i].Y + ((j - 1) * MatrixFontSize);
      if (CharY > -MatrixFontSize) and (CharY < AHeight + MatrixFontSize) then
      begin
        if j <= Length(MatrixCols[i].Chars) then
          CurrentChar := MatrixCols[i].Chars[j]
        else
          CurrentChar := ' ';
        ACanvas.DrawSimpleText(CurrentChar, MatrixCols[i].X, CharY, LSkFont, LPaint);
      end;
    end;
  end;
end;

{ ========================================================================
  HOLOGRAPHIC EFFECT IMPLEMENTATION
  ======================================================================== }

procedure DrawHolographicBackground(ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint; BackgroundSkImage: ISkImage; var GridOffsetY: Single; Animated: Boolean);
var
  WaveX, WaveY: Single;
  RRect: TRectF;
  LLinearSampling: TSkSamplingOptions;
begin
  if not Assigned(BackgroundSkImage) then
    Exit;

  LLinearSampling := TSkSamplingOptions.Create(TSkFilterMode.Linear, TSkMipmapMode.None);

  if Animated then
  begin
    Paint.Style := TSkPaintStyle.Fill;
    Paint.ImageFilter := nil;
    Paint.Alpha := 255;

    // Animate wave offset
    GridOffsetY := GridOffsetY + 1.0;
    if GridOffsetY > 1000 then
      GridOffsetY := 0;

    // Calculate sine wave offsets for RGB splitting effect
    WaveX := Sin(GridOffsetY * 0.02) * 10;
    WaveY := Sin(GridOffsetY * 0.03) * 10;

    { 1. Draw Base Image (Crisp) }
    ACanvas.DrawImageRect(BackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);

    { 2. Draw Ghost Copy 1 (Offset) }
    Paint.Alpha := 100;
    RRect := ADest;
    OffsetRect(RRect, WaveX, WaveY);
    ACanvas.DrawImageRect(BackgroundSkImage, RRect, LLinearSampling, Paint);

    { 3. Draw Ghost Copy 2 (Offset Opposite) }
    Paint.Alpha := 40;
    RRect := ADest;
    OffsetRect(RRect, -WaveX, -WaveY);
    ACanvas.DrawImageRect(BackgroundSkImage, RRect, LLinearSampling, Paint);
  end
  else
  begin
    // Static draw
    ACanvas.DrawImageRect(BackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);
  end;
end;

{ ========================================================================
  FADE EFFECT IMPLEMENTATION
  ======================================================================== }

function DrawFadeBackground(ACanvas: ISkCanvas; FBackgroundSkImage: ISkImage; FBackgroundNEWSkImage: ISkImage; FBackgroundFadestage: Integer; FBackgroundColor: TAlphaColor; const ADest: TRectF; const Paint: ISkPaint; owner: TSkCustomControl): Integer;
var
  CurrentPaint: ISkPaint;
begin
  Result := FBackgroundFadestage;

  // Phase 1: Crossfade
  if (FBackgroundFadestage < MAX_BG_FADE_STEPS) then
  begin
    if Assigned(FBackgroundNEWSkImage) then
    begin
      CurrentPaint := TSkPaint.Create;

      // Draw Old Image
      if Assigned(FBackgroundSkImage) then
      begin
        CurrentPaint.AlphaF := 255;
        ACanvas.DrawImageRect(FBackgroundSkImage, RectF(0, 0, owner.Width, owner.Height), TSkSamplingOptions.High, CurrentPaint);
      end;

      // Draw New Image with increasing Alpha
      CurrentPaint.AlphaF := FBackgroundFadestage / MAX_BG_FADE_STEPS;
      ACanvas.DrawImageRect(FBackgroundNEWSkImage, RectF(0, 0, owner.Width, owner.Height), TSkSamplingOptions.High, CurrentPaint);

      Inc(FBackgroundFadestage);
      Result := FBackgroundFadestage;
    end;
  end
  else
  begin
    // Phase 2: Final State (New Image)
    if Assigned(FBackgroundNEWSkImage) then
    begin
      CurrentPaint := TSkPaint.Create;
      CurrentPaint.AlphaF := 1.0;
      ACanvas.DrawImageRect(FBackgroundNEWSkImage, RectF(0, 0, owner.Width, owner.Height), TSkSamplingOptions.High, CurrentPaint);
    end
    else if Assigned(FBackgroundSkImage) then
    begin
      // Fallback if new image is missing
      CurrentPaint := TSkPaint.Create;
      CurrentPaint.AlphaF := 1.0;
      ACanvas.DrawImageRect(FBackgroundSkImage, RectF(0, 0, owner.Width, owner.Height), TSkSamplingOptions.High, CurrentPaint);
    end;
  end;
end;

{ Helper: Calculates the average color of an image }
function GetDominantColor(const Img: ISkImage): TAlphaColor;
var
  Info: TSkImageInfo;
  P: PByte;
begin
  Result := TAlphaColors.Black;
  if not Assigned(Img) then
    Exit;

  GDominantColorLock.Enter;
  try
    if not Assigned(GDominantColorSurface) then
    begin
      // Create a 1x1 pixel surface to downsample the image into a single color
      Info := TSkImageInfo.Create(1, 1, TSkColorType.RGBA8888, TSkAlphaType.Premul);
      GDominantColorSurface := TSkSurface.MakeRaster(Info);
    end;

    if Assigned(GDominantColorSurface) then
    begin
      GDominantColorSurface.Canvas.Clear(TAlphaColors.Black);
      // Draw full image into 1x1 pixel
      GDominantColorSurface.Canvas.DrawImageRect(Img, RectF(0, 0, Img.Width, Img.Height), RectF(0, 0, 1, 1), TSkSamplingOptions.High);

      // Read the pixel data
      P := GDominantColorSurface.PeekPixels.Pixels;
      if Assigned(P) then
        Result := (P[3] shl 24) or (P[2] shl 16) or (P[1] shl 8) or P[0];
    end;
  finally
    GDominantColorLock.Leave;
  end;
end;

{ ========================================================================
  TECH BRACKET IMPLEMENTATION
  ======================================================================== }

procedure DrawTechBrackets(ACanvas: ISkCanvas; const VisualRect: TRectF; Paint: ISkPaint; BracketWidth: Integer; ImageAlpha: Byte; IsSelected: Boolean; GlowColor, HotTrackColor: TAlphaColor; GlowWidth, HotTrackWidth: Integer);
var
  Len: Single;
  L, T, Rgt, B: Single;
begin
  if IsRectEmpty(VisualRect) then
    Exit;

  Len := BracketWidth;
  L := VisualRect.Left;
  T := VisualRect.Top;
  Rgt := VisualRect.Right;
  B := VisualRect.Bottom;

  Paint.Style := TSkPaintStyle.Stroke;
  Paint.ImageFilter := nil;
  Paint.AntiAlias := True;
  Paint.Alpha := ImageAlpha;

  // Set style based on selection state
  if IsSelected then
  begin
    Paint.Color := GlowColor;
    Paint.StrokeWidth := GlowWidth;
  end
  else
  begin
    Paint.Color := HotTrackColor;
    Paint.StrokeWidth := HotTrackWidth;
  end;

  // Draw Corners (Top-Left, Top-Right, Bottom-Left, Bottom-Right)
  ACanvas.DrawLine(L, T, L + Len, T, Paint);
  ACanvas.DrawLine(L, T, L, T + Len, Paint);

  ACanvas.DrawLine(Rgt - Len, T, Rgt, T, Paint);
  ACanvas.DrawLine(Rgt, T, Rgt, T + Len, Paint);

  ACanvas.DrawLine(L, B - Len, L, B, Paint);
  ACanvas.DrawLine(L, B, L + Len, B, Paint);

  ACanvas.DrawLine(Rgt - Len, B, Rgt, B, Paint);
  ACanvas.DrawLine(Rgt, B, Rgt, B - Len, Paint);
end;

{ ========================================================================
  SIMPLE PARTICLE SYSTEM IMPLEMENTATION
  ======================================================================== }

procedure UpdateSimpleParticles(var List: TList<TParticle>; DeltaTime: Double);
var
  i: Integer;
  P: TParticle;
begin
  if List.Count = 0 then
    Exit;

  for i := List.Count - 1 downto 0 do
  begin
    P := List[i];
    // Move
    P.X := P.X + (P.VX * DeltaTime * 10.0);
    P.Y := P.Y + (P.VY * DeltaTime * 10.0);
    // Age
    P.Life := P.Life - (DeltaTime * 2.0);

    if P.Life <= 0 then
      List.Delete(i)
    else
      List[i] := P;
  end;
end;

procedure DrawSimpleParticles(ACanvas: ISkCanvas; var List: TList<TParticle>);
var
  i: Integer;
  P: TParticle;
  PPaint: ISkPaint;
begin
  if List.Count = 0 then
    Exit;

  PPaint := TSkPaint.Create;
  PPaint.Style := TSkPaintStyle.Fill;

  for i := 0 to List.Count - 1 do
  begin
    P := List[i];
    PPaint.Color := P.Color;
    PPaint.AlphaF := Max(0, P.Life);
    ACanvas.DrawRect(TRectF.Create(P.X, P.Y, P.X + P.Size, P.Y + P.Size), PPaint);
  end;
end;

procedure SpawnParticles(var ParticlesList: TList<TParticle>; X, Y: Single; Count: Integer; Color: TAlphaColor);
var
  i: Integer;
  P: TParticle;
  Angle: Single;
begin
  for i := 0 to Count - 1 do
  begin
    Angle := Random * 2 * Pi;
    P.X := X;
    P.Y := Y;
    P.VX := Cos(Angle) * (2 + Random * 3);
    P.VY := Sin(Angle) * (2 + Random * 3);
    P.Life := 1.0;
    P.Color := Color;
    P.Size := 0.8 + Random * 1.5;
    ParticlesList.Add(P);
  end;
end;

{ ========================================================================
  IMAGE PARTICLE SYSTEM IMPLEMENTATION
  ======================================================================== }

procedure UpdateSmallPicParticles(var List: TList<TSmallPicParticle>; DeltaTime: Double; GetBmp: TGetSmallPicSkImageFunc);
var
  i: Integer;
  SP: TSmallPicParticle;
  TmpImg: ISkImage;
begin
  if List.Count = 0 then
    Exit;
  if not Assigned(GetBmp) then
    Exit;

  for i := List.Count - 1 downto 0 do
  begin
    SP := List[i];
    // Physics
    SP.X := SP.X + (SP.VX * DeltaTime * 10.0);
    SP.Y := SP.Y + (SP.VY * DeltaTime * 10.0);
    SP.Angle := SP.Angle + (SP.VAngle * DeltaTime * 10.0);
    SP.Life := SP.Life - (DeltaTime * 0.8);

    // Validate Image
    if (SP.Index < 0) then
      SP.Life := 0
    else if Assigned(GetBmp) then
    begin
      TmpImg := GetBmp(SP.Index);
      if not Assigned(TmpImg) then
        SP.Life := 0;
    end;

    if SP.Life <= 0 then
      List.Delete(i)
    else
      List[i] := SP;
  end;
end;

procedure DrawSmallPicParticles(ACanvas: ISkCanvas; var List: TList<TSmallPicParticle>; GetBmp: TGetSmallPicSkImageFunc);
var
  i: Integer;
  SP: TSmallPicParticle;
  IconSkImg: ISkImage;
  Paint: ISkPaint;
  ScaleW, ScaleH: Single;
  DestRect: TRectF;
begin
  if List.Count = 0 then
    Exit;
  if not Assigned(GetBmp) then
    Exit;

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.AntiAlias := True;

  for i := 0 to List.Count - 1 do
  begin
    SP := List[i];
    if (SP.Index >= 0) then
    begin
      IconSkImg := GetBmp(SP.Index);
      if Assigned(IconSkImg) then
      begin
        Paint.AlphaF := Max(0, SP.Life) * 0.5;

        { Optimization: Fast path for static particles }
        if (IsZero(SP.Angle) and IsZero(SP.VAngle) and (SP.Life > 0.95)) then
        begin
          DestRect := TRectF.Create(SP.X, SP.Y, SP.X + IconSkImg.Width, SP.Y + IconSkImg.Height);
          ACanvas.DrawImageRect(IconSkImg, DestRect, Paint);
        end
        else
        begin
          { Complex path: Rotation and Scaling }
          ScaleW := Max(0.2, SP.Life);
          ScaleH := Max(0.2, SP.Life);

          ACanvas.Save;
          ACanvas.Translate(SP.X, SP.Y);
          ACanvas.Translate(IconSkImg.Width / 2, IconSkImg.Height / 2);
          ACanvas.Scale(ScaleW, ScaleH);
          ACanvas.Rotate(SP.Angle);
          ACanvas.Translate(-IconSkImg.Width / 2, -IconSkImg.Height / 2);
          ACanvas.DrawImageRect(IconSkImg, TRectF.Create(0, 0, IconSkImg.Width, IconSkImg.Height), Paint);
          ACanvas.Restore;
        end;
      end;
    end;
  end;
end;

{ ========================================================================
  CYBERPUNK GLITCH EFFECT IMPLEMENTATION
  ======================================================================== }

procedure DrawCyberpunkGlitch(ACanvas: ISkCanvas; const Img: ISkImage; const Rect: TRectF; Intensity: Single; BaseSampling: TSkSamplingOptions);
var
  LPaint: ISkPaint;
  OffsetX, OffsetY: Single;
  TmpRect: TRectF;
  LColorFilter: ISkColorFilter;
begin
  if not Assigned(Img) then
    Exit;
  if Intensity <= 0.01 then
    Exit;

  // 1. CALCULATE RANDOM OFFSETS
  OffsetX := (10.0 * Intensity) * (0.5 + Random);
  OffsetY := (Random - 0.5) * (10.0 * Intensity);

  LPaint := TSkPaint.Create;
  LPaint.Style := TSkPaintStyle.Fill;
  LPaint.AntiAlias := True;

  // 2. RED CHANNEL (Shifted Left)
  LColorFilter := TSkColorFilter.MakeBlend(TAlphaColors.Red, TSkBlendMode.SrcIn);
  LPaint.ColorFilter := LColorFilter;
  LPaint.Alpha := Round(200 * Intensity);

  TmpRect := Rect;
  TmpRect.Offset(-OffsetX, OffsetY);
  ACanvas.DrawImageRect(Img, TmpRect, BaseSampling, LPaint);

  // 3. BLUE CHANNEL (Shifted Right)
  LColorFilter := TSkColorFilter.MakeBlend(TAlphaColors.Blue, TSkBlendMode.SrcIn);
  LPaint.ColorFilter := LColorFilter;
  LPaint.Alpha := Round(100 * Intensity);

  TmpRect := Rect;
  TmpRect.Offset(OffsetX, -OffsetY);
  ACanvas.DrawImageRect(Img, TmpRect, BaseSampling, LPaint);

  // 4. MAIN IMAGE (Centered - Bleeds through)
  LPaint.ColorFilter := nil;
  LPaint.Color := TAlphaColors.NULL;
  LPaint.Alpha := 255 - Round(100 * Intensity);

  ACanvas.DrawImageRect(Img, Rect, BaseSampling, LPaint);
end;

{ ========================================================================
  NEURAL LINKS IMPLEMENTATION
  ======================================================================== }

procedure DrawNeuralLinks(ACanvas: ISkCanvas; Center: TPointF; Links: TList<TNeuralLink>; AFont: ISkFont; MaxLineWidth: Single);
var
  i: Integer;
  Paint: ISkPaint;
  Link: TNeuralLink;
  MidX, MidY: Single;
begin
  if (Links = nil) or (Links.Count = 0) then
    Exit;

  GNeuralLinksLock.Enter;
  try
    Paint := TSkPaint.Create;
    try
      Paint.AntiAlias := True;
      Paint.StrokeWidth := MaxLineWidth;

      for i := 0 to Links.Count - 1 do
      begin
        Link := Links[i];

        Paint.Style := TSkPaintStyle.Stroke;
        Paint.StrokeWidth := 1;
        Paint.Color := Link.LineColor;
        Paint.Alpha := 70;

        // Draw Line
        ACanvas.DrawLine(TPointF.Create(Link.StartX, Link.StartY), TPointF.Create(Link.EndX, Link.EndY), Paint);

        // Draw Node at center
        MidX := (Link.StartX + Link.EndX) / 2;
        MidY := (Link.StartY + Link.EndY) / 2;

        Paint.Style := TSkPaintStyle.Fill;
        Paint.Color := Link.LineColor;

        ACanvas.DrawOval(TRectF.Create(MidX - 2, MidY - 2, MidX + 2, MidY + 2), Paint);
      end;
    finally
      // Paint is reference counted, no need to free manually
    end;
  finally
    GNeuralLinksLock.Leave;
  end;
end;

initialization
  InitGlobals;
  GNeuralLinksLock := TCriticalSection.Create;


finalization
  FreeGlobals;
  if Assigned(GNeuralLinksLock) then
    FreeAndNil(GNeuralLinksLock);

end.

