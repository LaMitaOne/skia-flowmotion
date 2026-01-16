{ Skia-Flowmotion v0.48 alpha                                                  }
{                                                                              }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{ larate@gmx.net                                                               }
{ https://lamita.jimdosite.com                                                 }
{                                                                              }
{------------------------------------------------------------------------------}

unit uSkFlowEffects;

interface

uses
  System.Types, System.UITypes, System.Math, System.SysUtils,
  System.Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls,
  System.Skia, FMX.Skia;

type
  TBackgroundEffect = (beHolographic, beRealMatrix);

  TMatrixColumn = record
    X: Single;
    Y: Single;
    Speed: Single;
    Length: Integer;
    Chars: string;
    TargetImageIndex: Integer;
  end;
  // === PARTICLE TYPES ===

  TParticle = record
    X, Y: Single;
    VX, VY: Single;
    Life: Single; // 1.0 down to 0
    Color: TAlphaColor;
    Size: Single;
  end;

  TSmallPicParticle = record
    Index: Integer;   // Index in ImageList
    X, Y: Single;   // Position
    VX, VY: Single;   // Velocity
    Life: Single;    // 1.0 down to 0.0
    Angle: Single;    // Rotation
    VAngle: Single;   // Rotation Velocity
  end;
  // === CALLBACKS ===

  TGetMatrixDataFunc = reference to function(Index: Integer): string;

  TGetSmallPicSkImageFunc = reference to function(Index: Integer): ISkImage;
  // === MATRIX EFFECT FUNCTIONS ===

procedure InitMatrix(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, AMatrixFontSize: Single; ImageCount: Integer; GetDataFunc: TGetMatrixDataFunc);

procedure UpdateMatrixPhysics(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, MatrixSpeed, MatrixFontSize: Single; ImageCount: Integer; Animated: Boolean; GetDataFunc: TGetMatrixDataFunc; DeltaTime: Double);

procedure DrawMatrixBackground(ACanvas: ISkCanvas; const MatrixCols: TArray<TMatrixColumn>; MatrixFont: TFont; MatrixColor, MatrixHeadColor: TAlphaColor; MatrixFontSize, MatrixSpeed: Single; AHeight: Single; Animated: Boolean);
  // === HOLOGRAPHIC EFFECT FUNCTIONS ===

procedure DrawHolographicBackground(ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint; BackgroundSkImage: ISkImage; var GridOffsetY: Single; Animated: Boolean);
  // === TECH BRACKET ===

procedure DrawTechBrackets(ACanvas: ISkCanvas; const VisualRect: TRectF; Paint: ISkPaint; BracketWidth: Integer; ImageAlpha: Byte; IsSelected: Boolean; GlowColor, HotTrackColor: TAlphaColor; GlowWidth, HotTrackWidth: Integer);
  // === PARTICLE SYSTEM FUNCTIONS ===

procedure UpdateSimpleParticles(var List: TList<TParticle>; DeltaTime: Double);

procedure DrawSimpleParticles(ACanvas: ISkCanvas; var List: TList<TParticle>);

procedure UpdateSmallPicParticles(var List: TList<TSmallPicParticle>; DeltaTime: Double; GetBmp: TGetSmallPicSkImageFunc);

procedure DrawSmallPicParticles(ACanvas: ISkCanvas; var List: TList<TSmallPicParticle>; GetBmp: TGetSmallPicSkImageFunc);

procedure SpawnParticles(var ParticlesList: TList<TParticle>; X, Y: Single; Count: Integer; Color: TAlphaColor);

implementation
{ ==========================================
   INIT MATRIX
   ========================================== }

procedure InitMatrix(var MatrixCols: TArray<TMatrixColumn>; AWidth, AHeight, AMatrixFontSize: Single; ImageCount: Integer; GetDataFunc: TGetMatrixDataFunc);
var
  i, j, ColsCount: Integer;
  SourceData: string;
begin
  if (AWidth <= 0) or (ImageCount = 0) then
    Exit;
  if AMatrixFontSize <= 0 then
    ColsCount := Trunc(AWidth / 10)
  else
    ColsCount := Trunc(AWidth / (AMatrixFontSize * 0.6));
  if ColsCount < 1 then
    ColsCount := 1;
  if ColsCount > 300 then
    ColsCount := 300;
  SetLength(MatrixCols, ColsCount);
  for i := 0 to ColsCount - 1 do
  begin
    MatrixCols[i].X := i * (AMatrixFontSize * 0.6);
    MatrixCols[i].Y := -Random(Trunc(AHeight * 2));
    MatrixCols[i].Speed := 5.0 + Random(15);
    if ImageCount > 1 then
      MatrixCols[i].TargetImageIndex := Random(ImageCount - 1)
    else
      MatrixCols[i].TargetImageIndex := 0;
    if Assigned(GetDataFunc) then
      SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex)
    else
      SourceData := 'NO_DATA';
    MatrixCols[i].Length := Length(SourceData);
    if MatrixCols[i].Length > 30 then
      MatrixCols[i].Length := 30;
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
{ ==========================================
   UPDATE MATRIX PHYSICS
   ========================================== }

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
      // 1. MOVE DOWN
      MatrixCols[i].Y := MatrixCols[i].Y + (MatrixCols[i].Speed * DeltaTime * 10.0 * MatrixSpeed * 0.5);
      // 2. RESET IF OFF SCREEN
      if MatrixCols[i].Y > (AHeight + (MatrixCols[i].Length * MatrixFontSize)) then
      begin
        MatrixCols[i].Y := -Random(Trunc(AHeight * 2));
        MatrixCols[i].Speed := 5.0 + Random(15);
        if (MatrixCols[i].TargetImageIndex < 0) or (MatrixCols[i].TargetImageIndex >= ImageCount) then
        begin
          if ImageCount > 1 then
            MatrixCols[i].TargetImageIndex := Random(ImageCount - 1)
          else
            MatrixCols[i].TargetImageIndex := 0;
        end;
        if Assigned(GetDataFunc) then
        begin
          SourceData := GetDataFunc(MatrixCols[i].TargetImageIndex);
          MatrixCols[i].Chars := SourceData;
          MatrixCols[i].Length := Length(SourceData);
          if MatrixCols[i].Length > 30 then
            MatrixCols[i].Length := 30;
        end;
      end;
      // 3. LIVE DATA UPDATES
      if Assigned(GetDataFunc) and (MatrixCols[i].TargetImageIndex >= 0) and (MatrixCols[i].TargetImageIndex < ImageCount) then
      begin
        if Random(100) < 15 then
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
{ ==========================================
   DRAW MATRIX BACKGROUND
   ========================================== }

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
  LSkFont := TSkFont.Create(TSkTypeface.MakeFromName(MatrixFont.Family, TSkFontStyle.Normal), MatrixFontSize);
  try
    for i := 0 to High(MatrixCols) do
    begin
      for j := 1 to Length(MatrixCols[i].Chars) do
      begin
        CharY := MatrixCols[i].Y + ((j - 1) * MatrixFontSize);
        if (CharY > -MatrixFontSize) and (CharY < AHeight + MatrixFontSize) then
        begin
          CurrentChar := MatrixCols[i].Chars[j];
          if j = Length(MatrixCols[i].Chars) then
          begin
            LPaint.Color := MatrixHeadColor;
            LPaint.AlphaF := 1.0;
          end
          else
          begin
            LPaint.Color := MatrixColor;
            LPaint.AlphaF := 0.6;
          end;
          ACanvas.DrawSimpleText(CurrentChar, MatrixCols[i].X, CharY, LSkFont, LPaint);
        end;
      end;
    end;
  except
  end;
end;
{ ==========================================
   DRAW HOLOGRAPHIC BACKGROUND
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
    GridOffsetY := GridOffsetY + 1.0;
    if GridOffsetY > 1000 then
      GridOffsetY := 0;
    WaveX := Sin(GridOffsetY * 0.02) * 10;
    WaveY := Sin(GridOffsetY * 0.03) * 10;
    ACanvas.DrawImageRect(BackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);
    Paint.Alpha := 100;
    RRect := ADest;
    OffsetRect(RRect, WaveX, WaveY);
    ACanvas.DrawImageRect(BackgroundSkImage, RRect, TSkSamplingOptions.High, Paint);
    Paint.Alpha := 40;
    RRect := ADest;
    OffsetRect(RRect, -WaveX, -WaveY);
    ACanvas.DrawImageRect(BackgroundSkImage, RRect, TSkSamplingOptions.High, Paint);
  end
  else
  begin
    ACanvas.DrawImageRect(BackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);
  end;
end;
{ ==========================================
   DRAW TECH BRACKETS
   ========================================== }

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
  if IsSelected then
    Paint.Color := GlowColor
  else
    Paint.Color := HotTrackColor;
  if IsSelected then
    Paint.StrokeWidth := GlowWidth
  else
    Paint.StrokeWidth := HotTrackWidth;
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
   SIMPLE PARTICLE UPDATE
   ========================================== }

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
    P.X := P.X + (P.VX * DeltaTime * 10.0);
    P.Y := P.Y + (P.VY * DeltaTime * 10.0);
    P.Life := P.Life - (DeltaTime * 2.0);
    if P.Life <= 0 then
      List.Delete(i)
    else
      List[i] := P;
  end;
end;
{ ==========================================
   SIMPLE PARTICLE DRAW
   ========================================== }

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
{ ==========================================
   SMALL PIC PARTICLE UPDATE
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
  for i := List.Count - 1 downto 0 do
  begin
    SP := List[i];
    SP.X := SP.X + (SP.VX * DeltaTime * 10.0);
    SP.Y := SP.Y + (SP.VY * DeltaTime * 10.0);
    SP.Angle := SP.Angle + (SP.VAngle * DeltaTime * 10.0);
    SP.Life := SP.Life - (DeltaTime * 0.8);
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
{ ==========================================
   SMALL PIC PARTICLE DRAW
   ========================================== }

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
        Paint.AlphaF := Max(0, SP.Life) * 0.5;
        ACanvas.Save;
        ACanvas.Translate(SP.X, SP.Y);
        ScaleW := Max(0.2, SP.Life);
        ScaleH := Max(0.2, SP.Life);
        ACanvas.Translate(IconSkImg.Width / 2, IconSkImg.Height / 2);
        ACanvas.Scale(ScaleW, ScaleH);
        ACanvas.Translate(-IconSkImg.Width / 2, -IconSkImg.Height / 2);
        ACanvas.Translate(IconSkImg.Width / 2, IconSkImg.Height / 2);
        ACanvas.Rotate(SP.Angle);
        ACanvas.Translate(-IconSkImg.Width / 2, -IconSkImg.Height / 2);
        ACanvas.DrawImageRect(IconSkImg, TRectF.Create(0, 0, IconSkImg.Width, IconSkImg.Height), TSkSamplingOptions.High, Paint);
        ACanvas.Restore;
      end;
    end;
  end;
end;


{ ==========================================
   SPAWN PARTICLES
   ========================================== }
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

end.

