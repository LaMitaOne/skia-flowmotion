{*******************************************************************************
  SkFlowEffects
********************************************************************************
  A collection of visual effects utilizing the Skia4Delphi library.
  Features include:
  - Matrix Rain (Digital Rain) effect with data binding.
  - Holographic distortion effects.
  - Cross-fade background transitions.
  - Particle systems (Simple and Image-based).
  - Technical UI bracket rendering.

  part of skFlowmotion

  written by: Lara Miriam Tamy Reschke
*******************************************************************************}

unit uSkFlowEffects;

interface

uses
  System.Types, System.UITypes, System.Math, System.SysUtils, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls,
  System.Skia, FMX.Skia;

const
  { Maximum number of steps for the background cross-fade animation }
  MAX_BG_FADE_STEPS = 2000;

type
  { Available background effect types }
  TBackgroundEffect = (beHolographic, beRealMatrix, beFade);

  { Base control class for Skia flow effects (Reserved for future expansion) }
  TSkFlowControl = class(TComponent);

  { Record representing a single column in the Matrix effect }
  TMatrixColumn = record
    X: Single;              { Horizontal position }
    Y: Single;              { Vertical position (head of the stream) }
    Speed: Single;          { Fall speed }
    Length: Integer;        { Number of characters in this column }
    Chars: string;          { The actual character data string }
    TargetImageIndex: Integer; { Index associated with the data source }
  end;

  { === PARTICLE TYPES === }

  { Simple rectangular particle }
  TParticle = record
    X, Y: Single;       { Position }
    VX, VY: Single;     { Velocity vector }
    Life: Single;       { Life remaining: 1.0 (new) down to 0.0 (dead) }
    Color: TAlphaColor; { Particle color }
    Size: Single;       { Width/Height of the particle }
  end;

  { Image-based particle (sprite) }
  TSmallPicParticle = record
    Index: Integer;    { Index of the image in the source collection }
    X, Y: Single;      { Position }
    VX, VY: Single;    { Velocity vector }
    Life: Single;      { Life remaining: 1.0 (new) down to 0.0 (dead) }
    Angle: Single;     { Current rotation angle in degrees }
    VAngle: Single;    { Rotation velocity }
  end;

  { === CALLBACKS === }

  { Delegate to retrieve string data for a specific matrix column index }
  TGetMatrixDataFunc = reference to function(Index: Integer): string;

  { Delegate to retrieve an ISkImage for a specific particle index }
  TGetSmallPicSkImageFunc = reference to function(Index: Integer): ISkImage;

  { === MATRIX EFFECT FUNCTIONS === }

  { Initializes the matrix columns based on screen dimensions. }
  procedure InitMatrix(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, AMatrixFontSize: Single; ImageCount: Integer; GetDataFunc: TGetMatrixDataFunc);

  { Updates the position and content of matrix columns. }
  procedure UpdateMatrixPhysics(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, MatrixSpeed, MatrixFontSize: Single; ImageCount: Integer; Animated: Boolean; GetDataFunc: TGetMatrixDataFunc; DeltaTime: Double);

  { Renders the matrix effect to the canvas. }
  procedure DrawMatrixBackground(ACanvas: ISkCanvas; const MatrixCols: TArray<TMatrixColumn>; MatrixFont: TFont; MatrixColor, MatrixHeadColor: TAlphaColor; MatrixFontSize, MatrixSpeed: Single; AHeight: Single; Animated: Boolean);

  { === FADE EFFECT FUNCTIONS === }

  { Handles cross-fading between two background images. }
  function DrawFadeBackground(ACanvas: ISkCanvas; FBackgroundSkImage : ISkImage; FBackgroundNEWSkImage : ISkImage; FBackgroundFadestage : Integer; FBackgroundColor : TAlphaColor; const ADest: TRectF; const Paint: ISkPaint; owner : TSkCustomControl) : Integer;

  { Calculates the dominant color of an image by downscaling it to 1x1 pixel. }
  function GetDominantColor(const Img: ISkImage): TAlphaColor;

  { === HOLOGRAPHIC EFFECT FUNCTIONS === }

  { Renders a holographic distortion effect over a background image. }
  procedure DrawHolographicBackground(ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint; BackgroundSkImage: ISkImage; var GridOffsetY: Single; Animated: Boolean);

  { === TECH BRACKET === }

  { Draws sci-fi style corner brackets around a rectangle. }
  procedure DrawTechBrackets(ACanvas: ISkCanvas; const VisualRect: TRectF; Paint: ISkPaint; BracketWidth: Integer; ImageAlpha: Byte; IsSelected: Boolean; GlowColor, HotTrackColor: TAlphaColor; GlowWidth, HotTrackWidth: Integer);

  { === PARTICLE SYSTEM FUNCTIONS === }

  { Updates physics for simple square particles. }
  procedure UpdateSimpleParticles(var List: TList<TParticle>; DeltaTime: Double);

  { Renders simple square particles. }
  procedure DrawSimpleParticles(ACanvas: ISkCanvas; var List: TList<TParticle>);

  { Updates physics for image-based particles. }
  procedure UpdateSmallPicParticles(var List: TList<TSmallPicParticle>; DeltaTime: Double; GetBmp: TGetSmallPicSkImageFunc);

  { Renders image-based particles with rotation and scaling. }
  procedure DrawSmallPicParticles(ACanvas: ISkCanvas; var List: TList<TSmallPicParticle>; GetBmp: TGetSmallPicSkImageFunc);

  { Spawns a burst of simple particles at a specific location. }
  procedure SpawnParticles(var ParticlesList: TList<TParticle>; X, Y: Single; Count: Integer; Color: TAlphaColor);

implementation

{ ==========================================
  MATRIX EFFECT IMPLEMENTATION
  ========================================== }

procedure InitMatrix(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, AMatrixFontSize: Single; ImageCount: Integer; GetDataFunc: TGetMatrixDataFunc);
var
  i, j, ColsCount: Integer;
  SourceData: string;
begin
  { Basic validation }
  if (AWidth <= 0) or (ImageCount = 0) then
    Exit;

  { Calculate number of columns based on font width (approx 0.6 aspect ratio for monospace) }
  if AMatrixFontSize <= 0 then
    ColsCount := Trunc(AWidth / 10)
  else
    ColsCount := Trunc(AWidth / (AMatrixFontSize * 0.6));

  { Limit column count to prevent performance issues }
  if ColsCount < 1 then
    ColsCount := 1;
  if ColsCount > 300 then
    ColsCount := 300;

  SetLength(MatrixCols, ColsCount);

  { Initialize each column }
  for i := 0 to ColsCount - 1 do
  begin
    { Calculate X position }
    MatrixCols[i].X := i * (AMatrixFontSize * 0.6);

    { Randomize Y start position (start above screen) }
    MatrixCols[i].Y := -Random(Trunc(AHeight * 2));

    { Randomize speed: 5.0 to 20.0 units per tick (adjusted by DeltaTime later) }
    MatrixCols[i].Speed := 5.0 + Random(15);

    { Assign a random image/data index }
    if ImageCount > 1 then
      MatrixCols[i].TargetImageIndex := Random(ImageCount - 1)
    else
      MatrixCols[i].TargetImageIndex := 0;

    { Retrieve data content }
    if Assigned(GetDataFunc) then
      SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex)
    else
      SourceData := 'NO_DATA';

    { Truncate length to prevent excessively long strings }
    MatrixCols[i].Length := Length(SourceData);
    if MatrixCols[i].Length > 30 then
      MatrixCols[i].Length := 30;

    { Build character array }
    MatrixCols[i].Chars := '';
    for j := 1 to MatrixCols[i].Length do
    begin
      if j <= Length(SourceData) then
        MatrixCols[i].Chars := MatrixCols[i].Chars + SourceData[j]
      else
        MatrixCols[i].Chars := MatrixCols[i].Chars + ' ';
    end;
  end;
end;

procedure UpdateMatrixPhysics(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, MatrixSpeed, MatrixFontSize: Single; ImageCount: Integer; Animated: Boolean; GetDataFunc: TGetMatrixDataFunc; DeltaTime: Double);
var
  i: Integer;
  SourceData: string;
begin
  if (Length(MatrixCols) = 0) or (ImageCount = 0) then
    Exit;

  if Animated then
  begin
    for i := 0 to High(MatrixCols) do
    begin
      { 1. MOVE DOWN }
      { DeltaTime adjustment ensures smooth movement regardless of framerate }
      MatrixCols[i].Y := MatrixCols[i].Y + (MatrixCols[i].Speed * DeltaTime * 3.0 * MatrixSpeed * 0.5);

      { 2. RESET IF OFF SCREEN }
      { If the tail of the column goes below the screen, reset to top }
      if MatrixCols[i].Y > (AHeight + (MatrixCols[i].Length * MatrixFontSize)) then
      begin
        MatrixCols[i].Y := -Random(Trunc(AHeight * 2));
        MatrixCols[i].Speed := 5.0 + Random(15);

        { Sanity check for image index bounds }
        if (MatrixCols[i].TargetImageIndex < 0) or (MatrixCols[i].TargetImageIndex >= ImageCount) then
        begin
          if ImageCount > 1 then
            MatrixCols[i].TargetImageIndex := Random(ImageCount - 1)
          else
            MatrixCols[i].TargetImageIndex := 0;
        end;

        { Refresh data on reset }
        if Assigned(GetDataFunc) then
        begin
          SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex);
          MatrixCols[i].Chars := SourceData;
          MatrixCols[i].Length := Length(SourceData);
          if MatrixCols[i].Length > 30 then
            MatrixCols[i].Length := 30;
        end;
      end;

      { 3. LIVE DATA UPDATES }
      { Occasionally update the characters while falling to simulate data changes }
      if Assigned(GetDataFunc) and (MatrixCols[i].TargetImageIndex >= 0) and (MatrixCols[i].TargetImageIndex < ImageCount) then
      begin
        if Random(100) < 15 then { 15% chance per frame to refresh text }
        begin
          SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex);
          MatrixCols[i].Chars := SourceData;
          if Length(MatrixCols[i].Chars) > 30 then
            MatrixCols[i].Chars := Copy(MatrixCols[i].Chars, 1, 30);
        end;
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

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Fill;

  { Create font object }
  LSkFont := TSkFont.Create(TSkTypeface.MakeFromName(MatrixFont.Family, TSkFontStyle.Normal), MatrixFontSize);

  try
    for i := 0 to High(MatrixCols) do
    begin
      { Iterate through characters in the column }
      for j := 1 to Length(MatrixCols[i].Chars) do
      begin
        CharY := MatrixCols[i].Y + ((j - 1) * MatrixFontSize);

        { Optimization: Only draw if vertically visible }
        if (CharY > -MatrixFontSize) and (CharY < AHeight + MatrixFontSize) then
        begin
          CurrentChar := MatrixCols[i].Chars[j];

          { The last character is the "head" of the stream, drawn white/bright }
          if j = Length(MatrixCols[i].Chars) then
          begin
            LPaint.Color := MatrixHeadColor;
            LPaint.AlphaF := 1.0;
          end
          else
          begin
            LPaint.Color := MatrixColor;
            LPaint.AlphaF := 0.6; { Trail transparency }
          end;

          { Draw the character }
          ACanvas.DrawSimpleText(CurrentChar, MatrixCols[i].X, CharY, LSkFont, LPaint);
        end;
      end;
    end;
  except
    { Silent fail to prevent UI crashes during rendering errors }
  end;
end;

{ ==========================================
  HOLOGRAPHIC EFFECT IMPLEMENTATION
  ========================================== }

procedure DrawHolographicBackground(ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint; BackgroundSkImage: ISkImage; var GridOffsetY: Single; Animated: Boolean);
var
  WaveX, WaveY: Single;
  RRect: TRectF;
begin
  if not Assigned(BackgroundSkImage) then
    Exit;

  if Animated then
  begin
    Paint.Style := TSkPaintStyle.Fill;
    Paint.ImageFilter := nil;
    Paint.Alpha := 255;

    { Animate the wave offset }
    GridOffsetY := GridOffsetY + 1.0;
    if GridOffsetY > 1000 then
      GridOffsetY := 0;

    { Calculate Sine wave offset for "jitter" effect }
    WaveX := Sin(GridOffsetY * 0.02) * 10;
    WaveY := Sin(GridOffsetY * 0.03) * 10;

    { 1. Draw Base Image }
    ACanvas.DrawImageRect(BackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);

    { 2. Draw Offset Copy 1 (Ghost effect) }
    Paint.Alpha := 100;
    RRect := ADest;
    OffsetRect(RRect, WaveX, WaveY);
    ACanvas.DrawImageRect(BackgroundSkImage, RRect, TSkSamplingOptions.High, Paint);

    { 3. Draw Offset Copy 2 (Ghost effect opposite direction) }
    Paint.Alpha := 40;
    RRect := ADest;
    OffsetRect(RRect, -WaveX, -WaveY);
    ACanvas.DrawImageRect(BackgroundSkImage, RRect, TSkSamplingOptions.High, Paint);
  end
  else
  begin
    { Static rendering }
    ACanvas.DrawImageRect(BackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);
  end;
end;

{ ==========================================
  FADE EFFECT IMPLEMENTATION
  ========================================== }

function DrawFadeBackground(ACanvas: ISkCanvas; FBackgroundSkImage: ISkImage; FBackgroundNEWSkImage: ISkImage; FBackgroundFadestage: Integer; FBackgroundColor: TAlphaColor; const ADest: TRectF; const Paint: ISkPaint; owner: TSkCustomControl): Integer;
var
  CurrentPaint: ISkPaint;
begin
  Result := FBackgroundFadestage;

  { 1. Check if fading is in progress (< MAX_STEPS) }
  if (FBackgroundFadestage < MAX_BG_FADE_STEPS) then
  begin
    if Assigned(FBackgroundNEWSkImage) then
    begin
      CurrentPaint := TSkPaint.Create;

      { 2. Draw OLD image (Opaque) as the base }
      if Assigned(FBackgroundSkImage) then
      begin
        CurrentPaint.AlphaF := 255;
        ACanvas.DrawImageRect(FBackgroundSkImage, RectF(0, 0, Owner.Width, Owner.Height), TSkSamplingOptions.High, CurrentPaint);
      end;

      { 3. Draw NEW image (Fading IN) on top }
      { Calculate Alpha based on current stage (0 to 1) }
      CurrentPaint.AlphaF := FBackgroundFadestage / MAX_BG_FADE_STEPS;
      ACanvas.DrawImageRect(FBackgroundNEWSkImage, RectF(0, 0, Owner.Width, Owner.Height), TSkSamplingOptions.High, CurrentPaint);

      { Increment stage for next frame }
      Inc(FBackgroundFadestage);
      Result := FBackgroundFadestage;
    end;
  end
  else
  begin
    { 4. Fading is finished (Stage >= MAX_STEPS), draw NEW image statically }
    if Assigned(FBackgroundNEWSkImage) then
    begin
      CurrentPaint := TSkPaint.Create;
      CurrentPaint.AlphaF := 1.0;
      ACanvas.DrawImageRect(FBackgroundNEWSkImage, RectF(0, 0, Owner.Width, Owner.Height), TSkSamplingOptions.High, CurrentPaint);
    end
    else if Assigned(FBackgroundSkImage) then
    begin
      { Fallback if New Image is missing }
      CurrentPaint := TSkPaint.Create;
      CurrentPaint.AlphaF := 1.0;
      ACanvas.DrawImageRect(FBackgroundSkImage, RectF(0, 0, Owner.Width, Owner.Height), TSkSamplingOptions.High, CurrentPaint);
    end;
  end;
end;

function GetDominantColor(const Img: ISkImage): TAlphaColor;
var
  Surface: ISkSurface;
  Pixmap: ISkPixmap;
  Info: TSkImageInfo;
  P: PByte;
begin
  { Default fallback color }
  Result := TAlphaColors.Black;
  if not Assigned(Img) then
    Exit;

  try
    { 1. Create Image Info for a single pixel (RGBA8888, Premultiplied Alpha) }
    Info := TSkImageInfo.Create(1, 1, TSkColorType.RGBA8888, TSkAlphaType.Premul);

    { 2. Create a 1x1 pixel raster surface }
    Surface := TSkSurface.MakeRaster(Info);
    if not Assigned(Surface) then
      Exit;

    { 3. Draw the full source image onto the 1x1 surface.
        Skia's high-quality sampling automatically averages the pixels
        resulting in the average color. }
    Surface.Canvas.Clear(TAlphaColors.Black);
    Surface.Canvas.DrawImageRect(Img, RectF(0, 0, Img.Width, Img.Height), RectF(0, 0, 1, 1), TSkSamplingOptions.High);

    { 4. Read the single pixel data back }
    Pixmap := Surface.PeekPixels;
    if Assigned(Pixmap) then
    begin
      P := Pixmap.Pixels;
      if Assigned(P) then
      begin
        { Map bytes (R,G,B,A) to TAlphaColor (AABBGGRR) }
        Result := (P[3] shl 24) or (P[2] shl 16) or (P[1] shl 8) or P[0];
      end;
    end;
  except
    { Fallback to black on any rendering or read errors }
    Result := TAlphaColors.Black;
  end;
end;

{ ==========================================
  TECH BRACKET IMPLEMENTATION
  ========================================== }

procedure DrawTechBrackets(ACanvas: ISkCanvas; const VisualRect: TRectF; Paint: ISkPaint; BracketWidth: Integer; ImageAlpha: Byte; IsSelected: Boolean; GlowColor, HotTrackColor: TAlphaColor; GlowWidth, HotTrackWidth: Integer);
var
  Len: Single;
  L, T, Rgt, B: Single;
begin
  if IsRectEmpty(VisualRect) then
    Exit;

  { Setup local variables for coordinates }
  Len := BracketWidth;
  L := VisualRect.Left;
  T := VisualRect.Top;
  Rgt := VisualRect.Right;
  B := VisualRect.Bottom;

  { Configure Paint Style }
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.ImageFilter := nil;
  Paint.AntiAlias := True;
  Paint.Alpha := ImageAlpha;

  { Determine color and width based on selection state }
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

  { Draw Corners }
  // Top Left
  ACanvas.DrawLine(L, T, L + Len, T, Paint);
  ACanvas.DrawLine(L, T, L, T + Len, Paint);

  // Top Right
  ACanvas.DrawLine(Rgt - Len, T, Rgt, T, Paint);
  ACanvas.DrawLine(Rgt, T, Rgt, T + Len, Paint);

  // Bottom Left
  ACanvas.DrawLine(L, B - Len, L, B, Paint);
  ACanvas.DrawLine(L, B, L + Len, B, Paint);

  // Bottom Right
  ACanvas.DrawLine(Rgt - Len, B, Rgt, B, Paint);
  ACanvas.DrawLine(Rgt, B, Rgt, B - Len, Paint);
end;

{ ==========================================
  SIMPLE PARTICLE SYSTEM IMPLEMENTATION
  ========================================== }

procedure UpdateSimpleParticles(var List: TList<TParticle>; DeltaTime: Double);
var
  i: Integer;
  P: TParticle;
begin
  if List.Count = 0 then
    Exit;

  { Iterate backwards to allow safe deletion }
  for i := List.Count - 1 downto 0 do
  begin
    P := List[i];

    { Apply velocity }
    P.X := P.X + (P.VX * DeltaTime * 10.0);
    P.Y := P.Y + (P.VY * DeltaTime * 10.0);

    { Decay life }
    P.Life := P.Life - (DeltaTime * 2.0);

    { Remove dead particles }
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

    { Apply life to alpha for fade out effect }
    PPaint.Color := P.Color;
    PPaint.AlphaF := Max(0, P.Life);

    { Draw particle as a rectangle }
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
    { Random direction }
    Angle := Random * 2 * Pi;

    { Initialize Position }
    P.X := X;
    P.Y := Y;

    { Calculate Velocity Vector with random speed }
    P.VX := Cos(Angle) * (2 + Random * 3);
    P.VY := Sin(Angle) * (2 + Random * 3);

    { Initialize Properties }
    P.Life := 1.0;
    P.Color := Color;
    P.Size := 0.8 + Random * 1.5;

    ParticlesList.Add(P);
  end;
end;

{ ==========================================
  IMAGE PARTICLE SYSTEM IMPLEMENTATION
  ========================================== }

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

  { Iterate backwards to allow safe deletion }
  for i := List.Count - 1 downto 0 do
  begin
    SP := List[i];

    { Update Position }
    SP.X := SP.X + (SP.VX * DeltaTime * 10.0);
    SP.Y := SP.Y + (SP.VY * DeltaTime * 10.0);

    { Update Rotation }
    SP.Angle := SP.Angle + (SP.VAngle * DeltaTime * 10.0);

    { Update Life }
    SP.Life := SP.Life - (DeltaTime * 0.8);

    { Validate image existence; kill particle if image is missing }
    if (SP.Index < 0) then
      SP.Life := 0
    else if Assigned(GetBmp) then
    begin
      TmpImg := GetBmp(SP.Index);
      if not Assigned(TmpImg) then
        SP.Life := 0;
    end;

    { Update or Remove }
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
        { Set opacity based on life (fade out) }
        Paint.AlphaF := Max(0, SP.Life) * 0.5;

        { Save Canvas state to apply local transformations }
        ACanvas.Save;

        { 1. Move to particle position }
        ACanvas.Translate(SP.X, SP.Y);

        { 2. Scale based on life (shrink effect) }
        ScaleW := Max(0.2, SP.Life);
        ScaleH := Max(0.2, SP.Life);

        { 3. Center the scaling/rotation on the image center }
        ACanvas.Translate(IconSkImg.Width / 2, IconSkImg.Height / 2);
        ACanvas.Scale(ScaleW, ScaleH);
        ACanvas.Rotate(SP.Angle);
        ACanvas.Translate(-IconSkImg.Width / 2, -IconSkImg.Height / 2);

        { 4. Draw the image }
        ACanvas.DrawImageRect(IconSkImg, TRectF.Create(0, 0, IconSkImg.Width, IconSkImg.Height), TSkSamplingOptions.High, Paint);

        { Restore Canvas state }
        ACanvas.Restore;
      end;
    end;
  end;
end;

end.
