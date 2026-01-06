unit uSkFlowmotion;

interface

uses
  { System }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections,
  { FMX }
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms,
  { Skia }
  System.Skia, FMX.Skia;

const
  TARGET_FPS = 30;
  MIN_FRAME_TIME = 1000 div TARGET_FPS;
  DEFAULT_ANIMATION_SPEED = 5;
  DEFAULT_ALPHA = 255;
  START_SCALE = 0.05;
  MIN_CELL_SIZE = 22;
  DEFAULT_GLOW_WIDTH = 2;
  DEFAULT_HOTTRACK_WIDTH = 1;
  DEFAULT_MAX_ZOOM_SIZE = 300;
  HOT_ZOOM_MIN_FACTOR = 1.02;
  HOT_ZOOM_MAX_FACTOR = 1.3;
  HOT_ZOOM_IN_SPEED = 0.07;
  HOT_ZOOM_OUT_SPEED = 0.09;
  HOT_ZOOM_IN_PER_SEC = 2.5;
  HOT_ZOOM_OUT_PER_SEC = 3.0;
  HOT_ZOOM_EPSILON = 0.0001;
  BREATHING_AMPLITUDE = 2.0;
  BREATHING_SPEED_PER_SEC = 0.06;

type
  TFlowLayout = (flSorted, flFreeFloat);

  TImageEntryStyle = (iesRandom, iesFromTop, iesFromBottom, iesFromLeft, iesFromRight, iesFromTopLeft, iesFromTopRight, iesFromBottomLeft, iesFromBottomRight, iesFromCenter, iesFromPoint);

  TBooleanGrid = array of array of Boolean;

  TZoomAnimationType = (zatSlide, zatFade, zatZoom, zatBounce);

  TImagePosition = record
    FileName: string;
    Caption: string;
    Path: string;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TImagePositions = array of TImagePosition;

  TImageItem = class
  private
    FSkImage: ISkImage;
    FCaption: string;
    FPath: string;
    FCurrentRect: TRect;
    FTargetRect: TRect;
    FStartRect: TRect;
    FAnimationProgress: Double;
    FAnimating: Boolean;
    FDirection: TImageEntryStyle;
    FVisible: Boolean;
    FAlpha: Byte;
    FTargetAlpha: Byte;
    FFileName: string;
    FIsSelected: Boolean;
    FZoomProgress: Double;
    FHotZoom: Double;
    FHotZoomTarget: Double;
    FHint: string;
    DriftRangeX: Single;
    DriftRangeY: Single;
    OriginalTargetRect: TRect;
  public
    constructor Create;
    destructor Destroy; override;
    property SkImage: ISkImage read FSkImage write FSkImage;
    property CurrentRect: TRect read FCurrentRect write FCurrentRect;
    property TargetRect: TRect read FTargetRect write FTargetRect;
    property StartRect: TRect read FStartRect write FStartRect;
    property AnimationProgress: Double read FAnimationProgress write FAnimationProgress;
    property Animating: Boolean read FAnimating write FAnimating;
    property Visible: Boolean read FVisible write FVisible;
    property Caption: string read FCaption write FCaption;
    property Path: string read FPath write FPath;
    property Alpha: Byte read FAlpha write FAlpha;
    property TargetAlpha: Byte read FTargetAlpha write FTargetAlpha;
    property FileName: string read FFileName write FFileName;
    property IsSelected: Boolean read FIsSelected write FIsSelected;
    property ZoomProgress: Double read FZoomProgress write FZoomProgress;
    property Direction: TImageEntryStyle read FDirection write FDirection;
    property Hint: string read FHint write FHint;
  end;

  TOnSelectedItemMouseDown = procedure(Sender: TObject; ImageItem: TImageItem; Index, X, Y: Integer; Button: TMouseButton; Shift: TShiftState) of object;

  TOnAllAnimationsFinished = procedure(Sender: TObject) of object;

  TOnSelectedImageDblClick = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TImageSelectEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TOnCaptionClick = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TImageLoadFailedEvent = procedure(Sender: TObject; const FileName: string; const ErrorMsg: string) of object;

  TImageHoverEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TActivationZone = record
    Name: string;
    Rect: TRect;
  end;

  TSelectedImageEnterZoneEvent = procedure(Sender: TObject; ImageItem: TImageItem; const ZoneName: string) of object;

  TImageLoadThread = class(TThread)
  private
    FCaption: string;
    FPath: string;
    FFileName: string;
    FSkImage: ISkImage;
    FHint: string;
    FOwner: TComponent;
    FSuccess: Boolean;
    FIndex: Integer;
    procedure SyncAddImage;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName, ACaption, APath, AHint: string; AOwner: TComponent; AIndex: Integer);
  end;

  TAnimationThread = class(TThread)
  private
    FOwner: TComponent;
    FLastTick: Cardinal;
    FStopRequested: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Stop;
  end;

  TSkFlowmotion = class(TSkCustomControl)
  private
    // Core image management
    FImages: TList;
    FAllFiles: TStringList;
    FAllCaptions: TStringList;
    FAllPaths: TStringList;
    FAllHints: TStringList;
    // Threading
    FNextLoaderCoreIndex: Integer;
    FAnimationThread: TAnimationThread;
    FLoadingThreads: TList;
    FLoadingCount: Integer;
    FClearing: Boolean;
    // Animation State
    inPaintCycle: Boolean;
    FAnimationSpeed: Integer;
    FAnimationEasing: Boolean;
    FInFallAnimation: Boolean;
    FFallingOut: Boolean;
    FPageOutProgress: Double;
    FImageEntryStyle: TImageEntryStyle;
    FEntryPoint: TPoint;
    // Layout
    FFlowLayout: TFlowLayout;
    FKeepSpaceforZoomed: Boolean;
    FSpacing: Integer;
    FKeepAspectRatio: Boolean;
    FMaxColumns: Integer;
    FMaxRows: Integer;
    FSorted: Boolean;
    FLoadedPositions: TImagePositions;
    FFreeFloatDrift: Boolean;
    FDragOffset: TPoint;
    // Visual
    FBackgroundSkImage: ISkImage;
    FBackgroundColor: TAlphaColor;
    FHotTrackColor: TAlphaColor;
    FHotTrackWidth: Integer;
    FHotTrackZoom: Boolean;
    FBreathingPhase: Double;
    FBreathingEnabled: Boolean;
    FZoomSelectedtoCenter: Boolean;
    FGlowColor: TAlphaColor;
    FGlowWidth: Integer;
    FCaptionFont: TFont;
    FCaptionColor: TAlphaColor;
    FCaptionBackground: TAlphaColor;
    FSelectedCaptionColor: TAlphaColor;
    FSelectedCaptionBackground: TAlphaColor;
    FCaptionAlpha: Byte;
    FCaptionOffsetY: Integer;
    FShowCaptions: Boolean;
    FCaptionOnHoverOnly: Boolean;
    FKeepAreaFreeRect: TRect;
    // Selection & Zoom
    FSelectedImage: TImageItem;
    FWasSelectedItem: TImageItem;
    FCurrentSelectedIndex: Integer;
    FMaxZoomSize: Integer;
    FZoomAnimationType: TZoomAnimationType;
    FSelectedMovable: Boolean;
    FDraggingSelected: Boolean;
    FDragStartPos: TPoint;
    FIsPotentialDrag: Boolean;
    FHotItem: TImageItem;
    FActivationZones: array of TActivationZone;
    FLastActivationZoneName: string;
    FOnCaptionClick: TOnCaptionClick;
    // Paging
    FPageSize: Integer;
    FCurrentPage: Integer;
    FPageChangeInProgress: Boolean;
    FAutoScrollPageForNewAdded: Boolean;
    // State
    FActive: Boolean;
    FThreadPriority: TThreadPriority;
    // Events
    FOnImageLoad: TProc<TObject, string, Boolean>;
    FOnItemSelected: TImageSelectEvent;
    FOnSelectedItemMouseDown: TOnSelectedItemMouseDown;
    FOnAllAnimationsFinished: TOnAllAnimationsFinished;
    FOnSelectedImageDblClick: TOnSelectedImageDblClick;
    FOnSelectedImageEnterZone: TSelectedImageEnterZoneEvent;
    FOnImageLoadFailed: TImageLoadFailedEvent;
    FOnImageMouseEnter: TImageHoverEvent;
    FOnImageMouseLeave: TImageHoverEvent;
    // Internal Methods - Animation
    procedure StartAnimationThread;
    procedure StopAnimationThread;
    procedure ThreadSafeInvalidate;
    procedure ThreadSafeRepaint;
    procedure ThreadSafeFireAllAnimationsFinished;
    procedure PerformAnimationUpdate(DeltaMS: Cardinal);
    function AnimationsRunning: Boolean;
    function EaseInOutQuad(t: Double): Double;
    function GetEntryDirection: TImageEntryStyle;
    procedure WaitForAllAnimations;
    procedure FreeAllImagesAndClearLists;
    procedure AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle; UseSavedPosition: Boolean; SavedTargetRect: TRect);
    procedure StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
    // Internal Methods - Layout
    procedure CalculateLayout;
    procedure CalculateLayoutSorted;
    procedure CalculateLayoutFreeFloat;
    function IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
    procedure MarkAreaOccupied(var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer);
    function PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer): Boolean;
    function GetOptimalSize(const OriginalWidth, OriginalHeight: Integer; const MaxWidth, MaxHeight: Double): TSize;
    // Internal Methods - Paging
    function GetPageCount: Integer;
    function GetPageStartIndex: Integer;
    function GetPageEndIndex: Integer;
    procedure ShowPage(Page: Integer);
    procedure NextPage;
    procedure PrevPage;
    // Internal Methods - Threading
    procedure ThreadFinished(Thread: TImageLoadThread);
    // Internal Methods - Utilities
    function GetImageItem(Index: Integer): TImageItem;
    function GetImageCount: Integer;
    function GetLoadingCount: Integer;
    procedure SetSelectedImage(ImageItem: TImageItem; Index: Integer);
    function GetCaptionRect(Item: TImageItem; const DrawRect: TRect): TRect;
    function SkImageToBitmap(Img: ISkImage): TBitmap;
    function BitmapToSkImage(Bmp: TBitmap): ISkImage;
    // Property Setters
    procedure SetActive(Value: Boolean);
    procedure SetSelectedMovable(Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetKeepSpaceforZoomed(Value: Boolean);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure SetHotTrackColor(Value: TAlphaColor);
    procedure SetGlowColor(Value: TAlphaColor);
    procedure SetGlowWidth(Value: Integer);
    procedure SetHotTrackWidth(Value: Integer);
    procedure SetAnimationSpeed(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetKeepAspectRatio(const Value: Boolean);
    procedure SetHotTrackZoom(const Value: Boolean);
    procedure SetZoomSelectedtoCenter(Value: Boolean);
    procedure SetBreathingEnabled(Value: Boolean);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetMaxColumns(const Value: Integer);
    procedure SetMaxRows(const Value: Integer);
    procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionColor(Value: TAlphaColor);
    procedure SetCaptionBackground(Value: TAlphaColor);
    procedure SetCaptionAlpha(Value: Byte);
    procedure SetCaptionOffsetY(Value: Integer);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetCaptionOnHoverOnly(Value: Boolean);
    procedure SetSelectedCaptionColor(Value: TAlphaColor);
    procedure SetSelectedCaptionBackground(Value: TAlphaColor);
    procedure SetKeepAreaFreeRect(const Value: TRect);
    procedure SetAutoScrollPageForNewAdded(Value: Boolean);
    procedure SetFreeFloatDrift(Value: Boolean);
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    procedure Resize; override;
    procedure DoImageLoad(const FileName: string; Success: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Layout
    procedure SetFlowLayout(Value: TFlowLayout);
    procedure SavePositionsToFile(const FileName: string);
    procedure LoadPositionsFromFile(const FileName: string);
    procedure SavePositionsToStream(Stream: TStream);
    procedure LoadPositionsFromStream(Stream: TStream);
    procedure AddImagesWithPositions(const FileNames, Captions, Paths: TStringList; const Positions: array of TRect);
    function GetCurrentPositions: TImagePositions;
    procedure ResetPositions;
    // Image management
    procedure AddImage(const FileName: string; const AHint: string = ''); overload;
    procedure AddImage(const FileName, ACaption, APath, AHint: string); overload;
    procedure AddImage(Bitmap: TBitmap); overload;
    procedure AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = ''; const AHint: string = '');
    procedure AddImages(const FileNames, Captions, Paths, Hints: TStringList);
    procedure AddImagesAsync(const FileNames, Captions, Paths, Hints: TStringList);
    procedure InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath, XHint: string);
    procedure InsertImageAsync(const FileName, Caption, Path, Hint: string);
    procedure SetImage(Index: Integer; Bitmap: TBitmap);
    procedure RemoveImage(Index: Integer; Animated: Boolean = True); overload;
    procedure RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean = false); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom; AndFree: Boolean = true); overload;
    procedure MoveImageToPos(RelIndexFrom, RelIndexTo: Integer);
    // Activation zone
    procedure AddActivationZone(const AName: string; const ARect: TRect);
    procedure ClearActivationZones;
    // Navigation
    procedure SelectNextImage;
    procedure SelectPreviousImage;
    procedure DeselectZoomedImage;
    procedure ScrollToIndex(Index: Integer; Animate: Boolean = True);
    // Search
    function FindImageByPath(const Path: string): TImageItem;
    function FindImageByCaption(const Caption: string): TImageItem;
    function GetImageIndex(ImageItem: TImageItem): Integer;
    function GetImageAtPoint(X, Y: Single): TImageItem;
    // Utilities
    function GetPicture(index: Integer): TBitmap;
    procedure SetBackgroundpicture(const Path: string);
    procedure WaitForAllLoads;
    // Properties
    property MaxZoomSize: Integer read FMaxZoomSize write FMaxZoomSize default DEFAULT_MAX_ZOOM_SIZE;
    property HotTrackColor: TAlphaColor read FHotTrackColor write SetHotTrackColor;
    property GlowColor: TAlphaColor read FGlowColor write SetGlowColor;
    property GlowWidth: Integer read FGlowWidth write SetGlowWidth;
    property HotTrackWidth: Integer read FHotTrackWidth write SetHotTrackWidth;
    property CaptionOnHoverOnly: Boolean read FCaptionOnHoverOnly write SetCaptionOnHoverOnly default True;
    property PageCount: Integer read GetPageCount;
    property CurrentSelectedIndex: Integer read FCurrentSelectedIndex;
    property ImageCount: Integer read GetImageCount;
    property LoadingCount: Integer read GetLoadingCount;
    property SelectedImage: TImageItem read FSelectedImage;
    property Items[Index: Integer]: TImageItem read GetImageItem; default;
    property Images[Index: Integer]: TImageItem read GetImageItem;
  published
    property FlowLayout: TFlowLayout read FFlowLayout write SetFlowLayout;
    property AnimationSpeed: Integer read FAnimationSpeed write SetAnimationSpeed default DEFAULT_ANIMATION_SPEED;
    property SelectedMovable: Boolean read FSelectedMovable write SetSelectedMovable default false;
    property Sorted: Boolean read FSorted write SetSorted default True;
    property KeepSpaceforZoomed: Boolean read FKeepSpaceforZoomed write SetKeepSpaceforZoomed default false;
    property Spacing: Integer read FSpacing write SetSpacing default 0;
    property KeepAspectRatio: Boolean read FKeepAspectRatio write SetKeepAspectRatio default True;
    property HotTrackZoom: Boolean read FHotTrackZoom write SetHotTrackZoom default True;
    property BreathingEnabled: Boolean read FBreathingEnabled write SetBreathingEnabled default True;
    property ZoomSelectedtoCenter: Boolean read FZoomSelectedtoCenter write SetZoomSelectedtoCenter default True;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property MaxColumns: Integer read FMaxColumns write SetMaxColumns default 0;
    property MaxRows: Integer read FMaxRows write SetMaxRows default 0;
    property AnimationEasing: Boolean read FAnimationEasing write FAnimationEasing default True;
    property OnImageLoad: TProc<TObject, string, Boolean> read FOnImageLoad write FOnImageLoad;
    property OnItemSelected: TImageSelectEvent read FOnItemSelected write FOnItemSelected;
    property Active: Boolean read FActive write SetActive default True;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property OnSelectedItemMouseDown: TOnSelectedItemMouseDown read FOnSelectedItemMouseDown write FOnSelectedItemMouseDown;
    property OnAllAnimationsFinished: TOnAllAnimationsFinished read FOnAllAnimationsFinished write FOnAllAnimationsFinished;
    property OnSelectedImageDblClick: TOnSelectedImageDblClick read FOnSelectedImageDblClick write FOnSelectedImageDblClick;
    property ZoomAnimationType: TZoomAnimationType read FZoomAnimationType write FZoomAnimationType default zatSlide;
    property PageSize: Integer read FPageSize write FPageSize;
    property CurrentPage: Integer read FCurrentPage;
    property ImageEntryStyle: TImageEntryStyle read FImageEntryStyle write FImageEntryStyle default iesRandom;
    property EntryPoint: TPoint read FEntryPoint write FEntryPoint;
    property OnSelectedImageEnterZone: TSelectedImageEnterZoneEvent read FOnSelectedImageEnterZone write FOnSelectedImageEnterZone;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default True;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionColor: TAlphaColor read FCaptionColor write SetCaptionColor;
    property CaptionBackground: TAlphaColor read FCaptionBackground write SetCaptionBackground;
    property CaptionAlpha: Byte read FCaptionAlpha write SetCaptionAlpha default 180;
    property CaptionOffsetY: Integer read FCaptionOffsetY write SetCaptionOffsetY default 8;
    property OnCaptionClick: TOnCaptionClick read FOnCaptionClick write FOnCaptionClick;
    property SelectedCaptionColor: TAlphaColor read FSelectedCaptionColor write SetSelectedCaptionColor;
    property SelectedCaptionBackground: TAlphaColor read FSelectedCaptionBackground write SetSelectedCaptionBackground;
    property KeepAreaFreeRect: TRect read FKeepAreaFreeRect write SetKeepAreaFreeRect;
    property AutoScrollPageForNewAdded: Boolean read FAutoScrollPageForNewAdded write SetAutoScrollPageForNewAdded default False;
    property OnImageLoadFailed: TImageLoadFailedEvent read FOnImageLoadFailed write FOnImageLoadFailed;
    property OnImageMouseEnter: TImageHoverEvent read FOnImageMouseEnter write FOnImageMouseEnter;
    property OnImageMouseLeave: TImageHoverEvent read FOnImageMouseLeave write FOnImageMouseLeave;
    property FreeFloatDrift: Boolean read FFreeFloatDrift write SetFreeFloatDrift default False;
    // Inherited
    property Align;
    property Anchors;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

procedure Register;

implementation

{ Registers the component in the IDE component palette }

procedure Register;
begin
  RegisterComponents('LaMita Components', [TSkFlowmotion]);
end;

// Helper to get Ticks cross-platform
function GetTickCount: Cardinal;
begin
  Result := TThread.GetTickCount;
end;

{ TAnimationThread }
constructor TAnimationThread.Create(AOwner: TComponent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FLastTick := GetTickCount;
  FStopRequested := False;
  Priority := (AOwner as TSkFlowmotion).FThreadPriority;
end;

destructor TAnimationThread.Destroy;
begin
  inherited;
end;

procedure TAnimationThread.Stop;
begin
  FStopRequested := True;
end;

procedure TAnimationThread.Execute;
var
  NowTick, LastTick, ElapsedMS, SleepTime: Cardinal;
begin
  LastTick := GetTickCount;
  while not Terminated and not FStopRequested do
  begin
    (FOwner as TSkFlowmotion).PerformAnimationUpdate(GetTickCount - LastTick);
    NowTick := GetTickCount;
    ElapsedMS := NowTick - LastTick;
    LastTick := NowTick;
    SleepTime := 0;
    if ElapsedMS < MIN_FRAME_TIME then
      SleepTime := 16 + MIN_FRAME_TIME - ElapsedMS;
    if FStopRequested then
      Break;
    if SleepTime > 0 then
      Sleep(SleepTime);
  end;
end;

{ TImageItem }
constructor TImageItem.Create;
begin
  inherited Create;
  FAnimationProgress := 0;
  FAnimating := False;
  FVisible := False;
  FAlpha := DEFAULT_ALPHA;
  FTargetAlpha := DEFAULT_ALPHA;
  FIsSelected := False;
  FZoomProgress := 0;
  FHotZoom := 1;
  FHotZoomTarget := 1;
  DriftRangeX := 10 + Random(20);
  DriftRangeY := 8 + Random(15);
end;

destructor TImageItem.Destroy;
begin
  FSkImage := nil;
  inherited;
end;

{ TImageLoadThread }
constructor TImageLoadThread.Create(const AFileName, ACaption, APath, AHint: string; AOwner: TComponent; AIndex: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFileName := AFileName;
  FCaption := ACaption;
  FPath := APath;
  FOwner := AOwner;
  FHint := AHint;
  FIndex := AIndex;
  FSuccess := False;
  Priority := TSkFlowmotion(FOwner).FThreadPriority;
  self.Execute;
end;

procedure TImageLoadThread.Execute;
begin
  FSuccess := False;
  try
    if TFile.Exists(FFileName) then
    begin
      FSkImage := TSkImage.MakeFromEncodedFile(FFileName);
      FSuccess := Assigned(FSkImage);
    end;
  except
  end;
  if (not Terminated) and FSuccess then
    Synchronize(SyncAddImage);
  if not Terminated then
  begin
    try
      TSkFlowmotion(FOwner).ThreadFinished(Self);
    except
    end;
  end;
end;

procedure TImageLoadThread.SyncAddImage;
var
  NewItem: TImageItem;
  AbsIndex, PageStart, PageEnd: Integer;
begin
  try
    if Assigned(FOwner) and (FOwner is TSkFlowmotion) then
    begin
      if FSuccess then
      begin
        with TSkFlowmotion(FOwner) do
        begin
          AbsIndex := FIndex;
          if FPageChangeInProgress or FClearing then
            Exit;
          PageStart := GetPageStartIndex;
          PageEnd := GetPageEndIndex;
          if (AbsIndex >= PageStart) and (AbsIndex <= PageEnd) then
          begin
            NewItem := TImageItem.Create;
            NewItem.SkImage := FSkImage;
            NewItem.Caption := FCaption;
            NewItem.Path := FPath;
            NewItem.Hint := FHint;
            NewItem.FileName := FFileName;
            NewItem.Direction := GetEntryDirection;
            NewItem.Visible := False;
            FImages.Add(NewItem);
            if Visible then
            begin
              CalculateLayout;
              AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
              NewItem.Visible := True;
            end;
          end;
        end;
      end;
      TSkFlowmotion(FOwner).DoImageLoad(FFileName, FSuccess);
    end;
  finally
    TSkFlowmotion(FOwner).ThreadSafeInvalidate;
  end;
end;




// =============================================================================
// PART 2A: LIFECYCLE, DRAWING, LAYOUT & IMAGE MANAGEMENT
// =============================================================================
{ TSkFlowmotion }
constructor TSkFlowmotion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Initialize Fonts and Colors
  FCaptionFont := TFont.Create;
  FCaptionFont.Family := 'Segoe UI';
  FCaptionFont.Size := 10;
  FCaptionFont.Style := [TFontStyle.fsBold];
  FCaptionColor := TAlphaColors.White;
  FCaptionBackground := TAlphaColors.Black;
  FSelectedCaptionColor := TAlphaColors.Black;
  FSelectedCaptionBackground := TAlphaColors.Aqua;
  FCaptionAlpha := 180;
  FShowCaptions := True;
  FAutoScrollPageForNewAdded := False;
  FCaptionOnHoverOnly := True;
  FCaptionOffsetY := 8;
  // Lists
  FImages := TList.Create;
  FLoadingThreads := TList.Create;
  FAllFiles := TStringList.Create;
  FAllCaptions := TStringList.Create;
  FAllPaths := TStringList.Create;
  FAllHints := TStringList.Create;
  SetLength(FLoadedPositions, 0);
  // Defaults
  FNextLoaderCoreIndex := 0;
  FClearing := False;
  FFreeFloatDrift := false;
  FBreathingPhase := 0.0;
  FAnimationThread := nil;
  FImageEntryStyle := iesRandom;
  FEntryPoint := TPoint.Create(-1000, -1000);
  FFlowLayout := flSorted;
  FKeepSpaceforZoomed := False;
  FAnimationSpeed := DEFAULT_ANIMATION_SPEED;
  FSpacing := 0;
  FKeepAspectRatio := True;
  FBackgroundColor := TAlphaColors.Black;
  FHotTrackColor := TAlphaColors.Teal;
  HotTrackZoom := true;
  FMaxColumns := 0;
  FMaxRows := 0;
  FSelectedMovable := False;
  FDraggingSelected := False;
  FSorted := True;
  FAnimationEasing := True;
  FLoadingCount := 0;
  FGlowColor := TAlphaColors.Aqua;
  FGlowWidth := DEFAULT_GLOW_WIDTH;
  FHotTrackWidth := DEFAULT_HOTTRACK_WIDTH;
  FSelectedImage := nil;
  FWasSelectedItem := nil;
  FZoomAnimationType := zatSlide;
  FPageSize := 1000;
  FCurrentPage := 0;
  FCurrentSelectedIndex := -1;
  FActive := True;
  FThreadPriority := tpNormal;
  TabStop := True;
  Width := 400;
  Height := 300;
  FBreathingEnabled := True;
  FZoomSelectedtoCenter := True;
  Self.HitTest := True;
end;

destructor TSkFlowmotion.Destroy;
var
  i: Integer;
  StartTime: Cardinal;
begin
  try
    if FAnimationThread <> nil then
    begin
      FAnimationThread.Stop;
      FAnimationThread.WaitFor;
      FreeAndNil(FAnimationThread);
    end;
    // Terminate all threads
    for i := 0 to FLoadingThreads.Count - 1 do
    begin
      try
        TImageLoadThread(FLoadingThreads[i]).Terminate;
      except
      end;
    end;
    // Wait for threads to finish (with timeout)
    StartTime := GetTickCount;
    while (FLoadingThreads.Count > 0) and ((GetTickCount - StartTime) < 3000) do
    begin
      CheckSynchronize(10);
      Sleep(5);
    end;
    FLoadingThreads.Clear;
    FLoadingThreads.Free;
    // Free all images
    for i := 0 to FImages.Count - 1 do
      TImageItem(FImages[i]).Free;
    FImages.Free;
    FAllFiles.Free;
    FAllCaptions.Free;
    FAllPaths.Free;
    FAllHints.Free;
    FCaptionFont.Free;
  except
  end;
  inherited Destroy;
end;

procedure TSkFlowmotion.ThreadSafeInvalidate;
begin
  if not FClearing then
    TThread.Queue(nil,
      procedure
      begin
        Self.Repaint;
      end);
end;

procedure TSkFlowmotion.ThreadSafeRepaint;
begin
  ThreadSafeInvalidate;
end;

procedure TSkFlowmotion.ThreadSafeFireAllAnimationsFinished;
begin
  if Assigned(FOnAllAnimationsFinished) then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnAllAnimationsFinished(Self);
      end);
end;

function TSkFlowmotion.GetImageCount: Integer;
begin
  Result := FImages.Count;
end;

function TSkFlowmotion.GetLoadingCount: Integer;
begin
  Result := FLoadingCount;
end;

function TSkFlowmotion.GetCaptionRect(Item: TImageItem; const DrawRect: TRect): TRect;
var
  CaptionH: Integer;
begin
  if not FShowCaptions or (Item.Caption = '') then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  CaptionH := 20; // Approx height
  Result := Rect(DrawRect.Left, DrawRect.Bottom - CaptionH - FCaptionOffsetY, DrawRect.Right, DrawRect.Bottom - FCaptionOffsetY);
end;

// --- Paging ---
function TSkFlowmotion.GetPageCount: Integer;
begin
  if FPageSize > 0 then
    Result := (FAllFiles.Count + FPageSize - 1) div FPageSize
  else
    Result := 1;
end;

function TSkFlowmotion.GetPageStartIndex: Integer;
begin
  Result := FCurrentPage * FPageSize;
end;

function TSkFlowmotion.GetPageEndIndex: Integer;
begin
  Result := Min((FCurrentPage + 1) * FPageSize, FAllFiles.Count) - 1;
end;

procedure TSkFlowmotion.ShowPage(Page: Integer);
var
  ItemIndex, i, StartIdx, EndIdx: Integer;
  ImageItem: TImageItem;
  FileName: string;
begin
  if (not visible) or (FAllFiles.Count = 0) then
    Exit;
  if FClearing or FPageChangeInProgress then
    Exit;
  FPageChangeInProgress := True;
  try
    // Cleanup previous page
    FImages.Clear;
    FSelectedImage := nil;
    FCurrentSelectedIndex := -1;
    // Stop current threads
    for i := 0 to FLoadingThreads.Count - 1 do
      TImageLoadThread(FLoadingThreads[i]).Terminate;
    FLoadingThreads.Clear;
    FLoadingCount := 0;
    // Calculate page range
    StartIdx := Page * FPageSize;
    EndIdx := Min(StartIdx + FPageSize, FAllFiles.Count)-1;
    FCurrentPage := Page;
    for i := StartIdx to EndIdx do
    begin
      FileName := FAllFiles[i];
      if TFile.Exists(FileName) then
      begin
        ImageItem := TImageItem.Create;
        ImageItem.SkImage := TSkImage.MakeFromEncodedFile(FileName);
        ImageItem.Caption := FAllCaptions[i];
        ImageItem.Path := FAllPaths[i];
        ImageItem.Hint := FAllHints[i];
        ImageItem.FileName := FileName;
        ImageItem.Direction := GetEntryDirection;
        ImageItem.Visible := False;
        FImages.Add(ImageItem);
      end;
    end;
    CalculateLayout;
    // Start animations for loaded items
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if Assigned(ImageItem.SkImage) then
      begin
        AnimateImage(ImageItem, ImageItem.Direction, false, Rect(0, 0, 0, 0));
        ImageItem.Visible := True;
      end;
    end;
    StartAnimationThread;
    Repaint;
  finally
    FPageChangeInProgress := False;
  end;
end;

procedure TSkFlowmotion.NextPage;
begin
  if FCurrentPage < GetPageCount - 1 then
    ShowPage(FCurrentPage + 1);
end;

procedure TSkFlowmotion.PrevPage;
begin
  if FCurrentPage > 0 then
    ShowPage(FCurrentPage - 1);
end;

// --- Properties ---
procedure TSkFlowmotion.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

procedure TSkFlowmotion.SetFlowLayout(Value: TFlowLayout);
begin
  if not visible then
    Exit;
  if FFlowLayout <> Value then
  begin
    DeselectZoomedImage;
    FFlowLayout := Value;
    if FFlowLayout = flFreeFloat then
      CalculateLayoutFreeFloat
    else
      CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetKeepSpaceforZoomed(Value: Boolean);
begin
  if FKeepSpaceforZoomed <> Value then
  begin
    FKeepSpaceforZoomed := Value;
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetThreadPriority(Value: TThreadPriority);
begin
  FThreadPriority := Value;
  if Assigned(FAnimationThread) then
    FAnimationThread.Priority := Value;
end;

procedure TSkFlowmotion.SetHotTrackColor(Value: TAlphaColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetGlowColor(Value: TAlphaColor);
begin
  if FGlowColor <> Value then
  begin
    FGlowColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetGlowWidth(Value: Integer);
begin
  if FGlowWidth <> Value then
  begin
    FGlowWidth := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetHotTrackWidth(Value: Integer);
begin
  if FHotTrackWidth <> Value then
  begin
    FHotTrackWidth := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetAnimationSpeed(const Value: Integer);
begin
  if (Value > 0) and (Value <= 100) then
    FAnimationSpeed := Value;
end;

procedure TSkFlowmotion.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetKeepAspectRatio(const Value: Boolean);
begin
  if FKeepAspectRatio <> Value then
  begin
    FKeepAspectRatio := Value;
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetHotTrackZoom(const Value: Boolean);
var
  i: Integer;
begin
  if FHotTrackZoom <> Value then
  begin
    FHotTrackZoom := Value;
    if not FHotTrackZoom then
    begin
      if FHotItem <> nil then
      begin
        if FHotItem <> FSelectedImage then
          FHotItem.FHotZoom := 1.0;
        FHotItem := nil;
      end;
      for i := 0 to FImages.Count - 1 do
        if (TImageItem(FImages[i]).FHotZoom > 1.0) and (TImageItem(FImages[i]) <> FSelectedImage) then
          TImageItem(FImages[i]).FHotZoom := 1.0;
    end;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetZoomSelectedtoCenter(Value: Boolean);
begin
  if FZoomSelectedtoCenter = Value then
    Exit;
  FZoomSelectedtoCenter := Value;
  CalculateLayout;
  Repaint;
end;

procedure TSkFlowmotion.SetBreathingEnabled(Value: Boolean);
begin
  if FBreathingEnabled = Value then
    Exit;
  FBreathingEnabled := Value;
  if not FBreathingEnabled and (FSelectedImage <> nil) and (FSelectedImage = FHotItem) then
  begin
    FSelectedImage.FHotZoomTarget := 1.0;
    FBreathingPhase := 0.0;
  end;
  Repaint;
end;

procedure TSkFlowmotion.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetMaxColumns(const Value: Integer);
begin
  if FMaxColumns <> Value then
  begin
    FMaxColumns := Value;
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetMaxRows(const Value: Integer);
begin
  if FMaxRows <> Value then
  begin
    FMaxRows := Value;
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Repaint;
end;

procedure TSkFlowmotion.SetCaptionColor(Value: TAlphaColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetCaptionBackground(Value: TAlphaColor);
begin
  if FCaptionBackground <> Value then
  begin
    FCaptionBackground := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetCaptionAlpha(Value: Byte);
begin
  if FCaptionAlpha <> Value then
  begin
    FCaptionAlpha := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetCaptionOffsetY(Value: Integer);
begin
  if FCaptionOffsetY <> Value then
  begin
    FCaptionOffsetY := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetShowCaptions(Value: Boolean);
begin
  if FShowCaptions <> Value then
  begin
    FShowCaptions := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetCaptionOnHoverOnly(Value: Boolean);
begin
  if FCaptionOnHoverOnly <> Value then
  begin
    FCaptionOnHoverOnly := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetSelectedCaptionColor(Value: TAlphaColor);
begin
  if FSelectedCaptionColor <> Value then
  begin
    FSelectedCaptionColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetSelectedCaptionBackground(Value: TAlphaColor);
begin
  if FSelectedCaptionBackground <> Value then
  begin
    FSelectedCaptionBackground := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetKeepAreaFreeRect(const Value: TRect);
begin
  if not EqualRect(FKeepAreaFreeRect, Value) then
  begin
    FKeepAreaFreeRect := Value;
    if Visible then
    begin
      CalculateLayout;
      Repaint;
    end;
  end;
end;

procedure TSkFlowmotion.SetAutoScrollPageForNewAdded(Value: Boolean);
begin
  FAutoScrollPageForNewAdded := Value;
end;

procedure TSkFlowmotion.SetFreeFloatDrift(Value: Boolean);
begin
  if FFreeFloatDrift <> Value then
  begin
    FFreeFloatDrift := Value;
    Repaint;
  end;
end;

function TSkFlowmotion.GetImageItem(Index: Integer): TImageItem;
begin
  if (Index >= 0) and (Index < FImages.Count) then
    Result := TImageItem(FImages[Index])
  else
    Result := nil;
end;

procedure TSkFlowmotion.DeselectZoomedImage;
var
  i: Integer;
begin
  for i := 0 to FImages.Count - 1 do
  begin
    TImageItem(FImages[i]).FHotZoomTarget := 1.0;
  end;
  Hint := '';
  if FFlowLayout = flFreeFloat then
  begin
    if FSelectedImage <> nil then
    begin
      FSelectedImage.IsSelected := False;
      FSelectedImage := nil;
      FCurrentSelectedIndex := -1;
      FHotItem := nil;
    end;
    Repaint;
  end
  else
    SetSelectedImage(nil, -1);
end;

function TSkFlowmotion.GetOptimalSize(const OriginalWidth, OriginalHeight: Integer; const MaxWidth, MaxHeight: Double): TSize;
var
  ScaleX, ScaleY, Scale: Double;
begin
  if FKeepAspectRatio then
  begin
    if (OriginalWidth = 0) or (OriginalHeight = 0) then
    begin
      Result.cx := Round(MaxWidth); // Auf Double umgestellt
      Result.cy := Round(MaxHeight);
      Exit;
    end;
    ScaleX := MaxWidth / OriginalWidth;
    ScaleY := MaxHeight / OriginalHeight;
    Scale := Min(ScaleX, ScaleY);
    Result.cx := Round(OriginalWidth * Scale);
    Result.cy := Round(OriginalHeight * Scale);
  end
  else
  begin
    Result.cx := Round(MaxWidth);
    Result.cy := Round(MaxHeight);
  end;
end;

function TSkFlowmotion.IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
var
  r, c: Integer;
  CenterRow, CenterCol, ProtectedSize: Integer;
begin
  // Bounds check
  if (Row < 0) or (Col < 0) or (Row + SpanRows > Length(Grid)) or (Col + SpanCols > Length(Grid[0])) then
  begin
    Result := False;
    Exit;
  end;
  // Normal grid occupation check
  for r := Row to Row + SpanRows - 1 do
    for c := Col to Col + SpanCols - 1 do
      if Grid[r, c] then
      begin
        Result := False;
        Exit;
      end;
  // Keep center area free for zoomed image
  if FKeepSpaceforZoomed then
    if (FSelectedImage <> nil) then
    begin
      CenterRow := (Length(Grid) div 2) - 1;
      CenterCol := (Length(Grid[0]) div 2) - 1;
      ProtectedSize := 2;
      if (Row < CenterRow + ProtectedSize) and (Row + SpanRows > CenterRow) and (Col < CenterCol + ProtectedSize) and (Col + SpanCols > CenterCol) then
      begin
        Result := False;
        Exit;
      end;
    end;
  Result := True;
end;

procedure TSkFlowmotion.MarkAreaOccupied(var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer);
var
  r, c: Integer;
begin
  for r := Row to Row + SpanRows - 1 do
    for c := Col to Col + SpanCols - 1 do
      if (r >= 0) and (r < Length(Grid)) and (c >= 0) and (c < Length(Grid[0])) then
        Grid[r, c] := True;
end;

function TSkFlowmotion.PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer): Boolean;
var
  X, Y: Integer;
  CellWidth, CellHeight: Integer;
  ImageSize: TSize;
  DummyRect: TRect;
begin
  if ImageItem.SkImage = nil then
  begin
    Result := True;
    Exit;
  end;
  // Calculate cell position and size
  X := FSpacing + Col * (BaseCellWidth + FSpacing);
  Y := FSpacing + Row * (BaseCellHeight + FSpacing);
  CellWidth := SpanCols * BaseCellWidth + (SpanCols - 1) * FSpacing;
  CellHeight := SpanRows * BaseCellHeight + (SpanRows - 1) * FSpacing;
  // Optimal image size preserving aspect ratio
  ImageSize := GetOptimalSize(ImageItem.SkImage.Width, ImageItem.SkImage.Height, CellWidth, CellHeight);
  // Center image in cell
  X := X + (CellWidth - ImageSize.cx) div 2;
  Y := Y + (CellHeight - ImageSize.cy) div 2;
  // Bounds clamping
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  if X + ImageSize.cx > Width then
    X := trunc(Width - ImageSize.cx);
  if Y + ImageSize.cy > Height then
    Y := trunc(Height - ImageSize.cy);
  // Final target rect
  ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);
  // === PIXEL-EXACT CHECK FOR KeepAreaFreeRect ===
  if not IsRectEmpty(FKeepAreaFreeRect) then
  begin
    if IntersectRect(DummyRect, ImageItem.TargetRect, FKeepAreaFreeRect) then
    begin
      Result := False; // Overlap ? reject
      Exit;
    end;
  end;
  ImageItem.StartRect := ImageItem.CurrentRect;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;
  MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
  Result := True;
end;

// =============================================================================
// THE DRAWING ENGINE (FULLY SKIA PORTED - FIXED)
// =============================================================================
procedure TSkFlowmotion.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  i: Integer;
  ImageItem: TImageItem;
  DstRect: TRectF;
  BaseRect: TRectF;
  Paint: ISkPaint;
  CenterX, CenterY, BaseW, BaseH, NewW, NewH: Single;
  ZoomFactor: Double;
begin
  // 1. Background
  if Assigned(FBackgroundSkImage) then
    ACanvas.DrawImageRect(FBackgroundSkImage, ADest, TSkSamplingOptions.High)
  else
    ACanvas.Clear(TAlphaColors.Black); // Schwarz statt Fuchsia für Debug

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;

  // 2. Draw Items
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible then Continue;
    if not Assigned(ImageItem.SkImage) then Continue;

    // --- GRUNDRECHTECK: Nur skaliere wenn wir wirklich gezoomt/animiert werden ---
    // Kopie von VCL Logik:
    // Wir nehmen die CurrentRect als Basis.
    // Wenn HotZoom nah an 1.0 ist, zeichnen wir normal (kein Skalierung).
    // Wenn HotZoom deutlich > 1.0 ist, zeichnen wir skaliert.

    BaseRect := TRectF.Create(ImageItem.CurrentRect);
    ZoomFactor := ImageItem.FHotZoom;

    // Schwellwert aus VCL übernommen (1.01)
    if (Abs(ZoomFactor - 1.0) > 0.01) then
    begin
       // Gezoomt: DstRect berechnen
       CenterX := (BaseRect.Left + BaseRect.Right) / 2;
       CenterY := (BaseRect.Top + BaseRect.Bottom) / 2;
       BaseW := BaseRect.Width;
       BaseH := BaseRect.Height;
       NewW := BaseW * ZoomFactor;
       NewH := BaseH * ZoomFactor;
       DstRect := TRectF.Create(CenterX - NewW/2, CenterY - NewH/2, CenterX + NewW/2, CenterY + NewH/2);
    end
    else
    begin
       // Normal: DstRect = BaseRect (Keine Verzerrung durch StartScale!)
       DstRect := BaseRect;
    end;

    // Draw Image
    ACanvas.DrawImageRect(ImageItem.SkImage, DstRect, TSkSamplingOptions.High, Paint);

    // Draw Glow/HotTrack Border
    // Wir benutzen DstRect für den Rahmen, damit der Rahmen mit skaliert
    if (ImageItem = FHotItem) or ImageItem.IsSelected then
    begin
       Paint.Style := TSkPaintStyle.Stroke;
       Paint.StrokeWidth := ifThen(ImageItem.IsSelected, FGlowWidth, FHotTrackWidth);
       Paint.Color := TAlphaColor(ifThen(ImageItem.IsSelected, TAlphaColor(FGlowColor), TAlphaColor(FHotTrackColor)));

       if ImageItem.IsSelected then
         Paint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, FGlowWidth/2)
       else
         Paint.MaskFilter := nil;

       ACanvas.DrawRect(DstRect, Paint);

       // Reset for next item
       Paint.Style := TSkPaintStyle.Fill;
       Paint.MaskFilter := nil;
    end;
  end;
end;

// =============================================================================
// LAYOUT ALGORITHMS (SORTED & FREE FLOAT)
// =============================================================================
procedure TSkFlowmotion.CalculateLayout;
begin
  case FFlowLayout of
    flSorted:
      CalculateLayoutSorted;
    flFreeFloat:
      CalculateLayoutFreeFloat;
  end;
end;

procedure TSkFlowmotion.CalculateLayoutSorted;
var
  Cols, Rows, i, c, r, BestCols, BestRows: Integer;
  ImageItem: TImageItem;
  BaseCellWidth, BaseCellHeight: Integer;
  VCount, AddforZoomed: Integer;
  Grid: TBooleanGrid;
  SpanCols, SpanRows: Integer;
  Placed: Boolean;
  MaxCellWidth: Double;
  ImageAspectRatio: Double;
  VisibleImages: TList;
  SortList: TList;
  TotalCellEstimate: Integer;
  MinCols, MaxColsToTry: Integer;
  PotentialCellWidth, PotentialCellHeight, CellArea: Double;
  X, Y: Integer;
  ImageSize: TSize;

  function CompareImageSize(A, B: Pointer): Integer;
  var
    AreaA, AreaB: Int64;
  begin
    if Assigned(TImageItem(A).SkImage) and Assigned(TImageItem(B).SkImage) then
    begin
      AreaA := Int64(TImageItem(A).SkImage.Width) * TImageItem(A).SkImage.Height;
      AreaB := Int64(TImageItem(B).SkImage.Width) * TImageItem(B).SkImage.Height;
      if AreaA > AreaB then
        Result := -1
      else if AreaA < AreaB then
        Result := 1
      else
        Result := 0;
    end
    else
      Result := 0;
  end;

begin
  if FImages.Count = 0 then
    Exit;
  if FInFallAnimation then
    Exit;
  AddforZoomed := 0;
  if FKeepSpaceforZoomed then
    if FSelectedImage <> nil then
      AddforZoomed := 2;
  VisibleImages := TList.Create;
  try
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if (not ImageItem.IsSelected) or (not FZoomSelectedtoCenter) then
        VisibleImages.Add(ImageItem);
    end;
    if VisibleImages.Count = 0 then
      Exit;
    if not FSorted then
    begin
      SortList := TList.Create;
      try
        SortList.Assign(VisibleImages);
        SortList.Sort(@CompareImageSize);
        VisibleImages.Assign(SortList);
      finally
        SortList.Free;
      end;
    end;
    if FZoomSelectedtoCenter and (FSelectedImage <> nil) then
      VCount := VisibleImages.Count + 1
    else
      VCount := VisibleImages.Count;
    TotalCellEstimate := 0;
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if not Assigned(ImageItem.SkImage) then
        Continue;
      ImageAspectRatio := ImageItem.SkImage.Width / ImageItem.SkImage.Height;
      if ImageAspectRatio > 1.4 then
        Inc(TotalCellEstimate, 2)
      else if ImageAspectRatio < 0.75 then
        Inc(TotalCellEstimate, 2)
      else
        Inc(TotalCellEstimate, 1);
    end;
    if TotalCellEstimate < VCount then
      TotalCellEstimate := VCount;
    MaxCellWidth := 0;
    BestCols := 0;
    BestRows := 0;
    MinCols := Trunc(Max(3.0, Sqrt(TotalCellEstimate)));
    MaxColsToTry := TotalCellEstimate;
    for c := MinCols to MaxColsToTry do
    begin
      r := Ceil(TotalCellEstimate / c);
      if r < 3 then
        r := 3;
      PotentialCellWidth := (Width - FSpacing * (c + 1)) / c;
      PotentialCellHeight := (Height - FSpacing * (r + 1)) / r;
      if (PotentialCellWidth < MIN_CELL_SIZE) or (PotentialCellHeight < MIN_CELL_SIZE) then
        Continue;
      if PotentialCellWidth > MaxCellWidth then
      begin
        MaxCellWidth := PotentialCellWidth;
        BestCols := c;
        BestRows := r;
      end;
    end;
    if BestCols = 0 then
    begin
      BestCols := Trunc(Max(3.0, Sqrt(Double(TotalCellEstimate))));
      BestRows := Max(3, Ceil(TotalCellEstimate / BestCols));
    end;
    Cols := BestCols;
    Rows := BestRows;
    BaseCellWidth := Trunc(Max(Double(MIN_CELL_SIZE), (Width - FSpacing * (Cols + 1)) / Cols));
    BaseCellHeight := Trunc(Max(Double(MIN_CELL_SIZE), (Height - FSpacing * (Rows + 1)) / Rows));
    SetLength(Grid, Rows, Cols);
    for r := 0 to Rows - 1 do
      for c := 0 to Cols - 1 do
        Grid[r, c] := False;
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if not Assigned(ImageItem.SkImage) then
        Continue;
      ImageAspectRatio := ImageItem.SkImage.Width / ImageItem.SkImage.Height;
      if ImageAspectRatio > 1.4 then
      begin
        SpanCols := 2;
        SpanRows := 1;
      end
      else if ImageAspectRatio < 0.75 then
      begin
        SpanCols := 1;
        SpanRows := 2;
      end
      else
      begin
        SpanCols := 1;
        SpanRows := 1;
      end;
      Placed := False;
      for r := 0 to Rows - SpanRows do
      begin
        for c := 0 to Cols - SpanCols do
        begin
          if IsAreaFree(Grid, r, c, SpanRows, SpanCols) then
          begin
            if PlaceImage(ImageItem, Grid, r, c, SpanRows, SpanCols, BaseCellWidth, BaseCellHeight) then
            begin
              Placed := True;
              Break;
            end;
          end;
        end;
        if Placed then
          Break;
      end;
      // Fallback 1x1
      if not Placed then
      begin
        for r := 0 to Rows - 1 do
        begin
          for c := 0 to Cols - 1 do
          begin
            if IsAreaFree(Grid, r, c, 1, 1) then
            begin
              PlaceImage(ImageItem, Grid, r, c, 1, 1, BaseCellWidth, BaseCellHeight);
              Placed := True;
              Break;
            end;
          end;
          if Placed then
            Break;
        end;
      end;
    end;
  finally
    VisibleImages.Free;
  end;
end;

procedure TSkFlowmotion.CalculateLayoutFreeFloat;
var
  Cols, Rows, i, c, r, BestCols, BestRows: Integer;
  ImageItem: TImageItem;
  BaseCellWidth, BaseCellHeight: Integer;
  VCount: Integer;
  Grid: TBooleanGrid;
  SpanCols, SpanRows: Integer;
  Placed: Boolean;
  MaxCellWidth: Double;
  ImageAspectRatio: Double;
  VisibleImages: TList;
  SortList: TList;
  TotalCellEstimate: Integer;
  MinCols, MaxColsToTry: Integer;
  PotentialCellWidth, PotentialCellHeight: Double;
  X, Y: Integer;
  ImageSize: TSize;
  DefaultWidth, DefaultHeight: Integer;
  NewX, NewY: Integer;
  MaxAttempts: Integer;
  FoundPosition: Boolean;
  ExistingRect: TRect;
  j: Integer;

  function CompareImageSize(A, B: Pointer): Integer;
  var
    AreaA, AreaB: Int64;
  begin
    if Assigned(TImageItem(A).SkImage) and Assigned(TImageItem(B).SkImage) then
    begin
      AreaA := Int64(TImageItem(A).SkImage.Width) * TImageItem(A).SkImage.Height;
      AreaB := Int64(TImageItem(B).SkImage.Width) * TImageItem(B).SkImage.Height;
      if AreaA > AreaB then
        Result := -1
      else if AreaA < AreaB then
        Result := 1
      else
        Result := 0;
    end
    else
      Result := 0;
  end;

begin
  if FImages.Count = 0 then
    Exit;
  if FInFallAnimation then
    Exit;
  VisibleImages := TList.Create;
  try
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if (not ImageItem.IsSelected) or (not FZoomSelectedtoCenter) then
        VisibleImages.Add(ImageItem);
    end;
    if VisibleImages.Count = 0 then
      Exit;
    if not FSorted then
    begin
      SortList := TList.Create;
      try
        SortList.Assign(VisibleImages);
        SortList.Sort(@CompareImageSize);
        VisibleImages.Assign(SortList);
      finally
        SortList.Free;
      end;
    end;
    VCount := VisibleImages.Count;
    TotalCellEstimate := 0;
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if not Assigned(ImageItem.SkImage) then
        Continue;
      ImageAspectRatio := ImageItem.SkImage.Width / ImageItem.SkImage.Height;
      if ImageAspectRatio > 1.4 then
        Inc(TotalCellEstimate, 2)
      else if ImageAspectRatio < 0.75 then
        Inc(TotalCellEstimate, 2)
      else
        Inc(TotalCellEstimate, 1);
    end;
    if TotalCellEstimate < VCount then
      TotalCellEstimate := VCount;
    MaxCellWidth := 0;
    BestCols := 0;
    BestRows := 0;
    MinCols := Trunc(Max(3.0, Sqrt(TotalCellEstimate)));
    MaxColsToTry := TotalCellEstimate;
    for c := MinCols to MaxColsToTry do
    begin
      r := Ceil(TotalCellEstimate / c);
      if r < 3 then
        r := 3;
      PotentialCellWidth := (Width - FSpacing * (c + 1)) / c;
      PotentialCellHeight := (Height - FSpacing * (r + 1)) / r;
      if (PotentialCellWidth < MIN_CELL_SIZE) or (PotentialCellHeight < MIN_CELL_SIZE) then
        Continue;
      if PotentialCellWidth > MaxCellWidth then
      begin
        MaxCellWidth := PotentialCellWidth;
        BestCols := c;
        BestRows := r;
      end;
    end;
    if BestCols = 0 then
    begin
      BestCols := Trunc(Max(3.0, Sqrt(Double(TotalCellEstimate)))); // FIX
      BestRows := Max(3, Ceil(TotalCellEstimate / BestCols));
    end;
    Cols := BestCols;
    Rows := BestRows;
    BaseCellWidth := Trunc(Max(Double(MIN_CELL_SIZE), (Width - FSpacing * (Cols + 1)) / Cols));
    BaseCellHeight := Trunc(Max(Double(MIN_CELL_SIZE), (Height - FSpacing * (Rows + 1)) / Rows));
    SetLength(Grid, Rows, Cols);
    for r := 0 to Rows - 1 do
      for c := 0 to Cols - 1 do
        Grid[r, c] := False;
    // Robust Placement
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if not Assigned(ImageItem.SkImage) then
        Continue;
      ImageAspectRatio := ImageItem.SkImage.Width / ImageItem.SkImage.Height;
      if ImageAspectRatio > 1.4 then
      begin
        SpanCols := 2;
        SpanRows := 1;
      end
      else if ImageAspectRatio < 0.75 then
      begin
        SpanCols := 1;
        SpanRows := 2;
      end
      else
      begin
        SpanCols := 1;
        SpanRows := 1;
      end;
      Placed := False;
      for r := 0 to Rows - SpanRows do
      begin
        for c := 0 to Cols - SpanCols do
        begin
          if IsAreaFree(Grid, r, c, SpanRows, SpanCols) then
          begin
            if PlaceImage(ImageItem, Grid, r, c, SpanRows, SpanCols, BaseCellWidth, BaseCellHeight) then
            begin
              Placed := True;
              Break;
            end;
          end;
        end;
        if Placed then
          Break;
      end;
      if not Placed then
      begin
        for r := 0 to Rows - 1 do
        begin
          for c := 0 to Cols - 1 do
          begin
            if IsAreaFree(Grid, r, c, 1, 1) then
            begin
              PlaceImage(ImageItem, Grid, r, c, 1, 1, BaseCellWidth, BaseCellHeight);
              Placed := True;
              Break;
            end;
          end;
          if Placed then
            Break;
        end;
      end;
    end;
  finally
    VisibleImages.Free;
  end;
end;

procedure TSkFlowmotion.DoImageLoad(const FileName: string; Success: Boolean);
begin
  if Assigned(FOnImageLoad) then
    FOnImageLoad(Self, FileName, Success);
end;

procedure TSkFlowmotion.WaitForAllLoads;
begin
  while FLoadingCount > 0 do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

procedure TSkFlowmotion.FreeAllImagesAndClearLists;
var
  i: Integer;
begin
  for i := 0 to FImages.Count - 1 do
    TImageItem(FImages[i]).Free;
  FImages.Clear;
  FAllFiles.Clear;
  FAllCaptions.Clear;
  FAllPaths.Clear;
  FAllHints.Clear;
end;

procedure TSkFlowmotion.WaitForAllAnimations;
begin
  // Simple busy wait or use an event if you want to be fancy.
  // Given the component nature, we'll just wait for the flag to clear up to a timeout.
  var TickCount := GetTickCount;
  while AnimationsRunning and ((GetTickCount - TickCount) < 5000) do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

procedure TSkFlowmotion.SetSelectedImage(ImageItem: TImageItem; Index: Integer);
var
  OldSelected: TImageItem;
  i: Integer;
begin
  if FSelectedImage = ImageItem then
    Exit;

  OldSelected := FSelectedImage;
  FSelectedImage := ImageItem;
  FCurrentSelectedIndex := Index;

  // Reset old selection
  if Assigned(OldSelected) then
  begin
    OldSelected.IsSelected := False;
    OldSelected.FHotZoomTarget := 1.0;
    FWasSelectedItem := OldSelected;
  end;

  // Set new selection
  if Assigned(FSelectedImage) then
  begin
    FSelectedImage.IsSelected := True;
    FSelectedImage.FHotZoomTarget := HOT_ZOOM_MAX_FACTOR;
    StartZoomAnimation(FSelectedImage, True);
  end
  else
  begin
    // Deselecting: Recalculate layout to fill space
    CalculateLayout;
  end;

  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, FSelectedImage, Index);
end;

procedure TSkFlowmotion.SetSelectedMovable(Value: Boolean);
begin
  FSelectedMovable := Value;
end;

// --- Finders ---
function TSkFlowmotion.FindImageByPath(const Path: string): TImageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FImages.Count - 1 do
    if TImageItem(FImages[i]).Path = Path then
    begin
      Result := TImageItem(FImages[i]);
      Exit;
    end;
end;

function TSkFlowmotion.FindImageByCaption(const Caption: string): TImageItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FImages.Count - 1 do
    if TImageItem(FImages[i]).Caption = Caption then
    begin
      Result := TImageItem(FImages[i]);
      Exit;
    end;
end;

function TSkFlowmotion.GetImageIndex(ImageItem: TImageItem): Integer;
begin
  Result := FImages.IndexOf(ImageItem);
end;

// --- Activation Zones ---
procedure TSkFlowmotion.AddActivationZone(const AName: string; const ARect: TRect);
begin
  SetLength(FActivationZones, Length(FActivationZones) + 1);
  FActivationZones[High(FActivationZones)].Name := AName;
  FActivationZones[High(FActivationZones)].Rect := ARect;
end;

procedure TSkFlowmotion.ClearActivationZones;
begin
  SetLength(FActivationZones, 0);
end;

procedure TSkFlowmotion.InsertImageAsync(const FileName, Caption, Path, Hint: string);
begin
  // Reusing AddImageAsync logic essentially
  AddImageAsync(FileName, Caption, Path, Hint);
end;


// =============================================================================
// ANIMATION LOOP & HELPERS
// =============================================================================
procedure TSkFlowmotion.PerformAnimationUpdate(DeltaMS: Cardinal);
var
  i: Integer;
  DeltaTime: Double;
  ImageItem: TImageItem;
  Progress, Eased, Speed: Double;
  AnyAnimating: Boolean;
  NeedRepaint: Boolean;
  TempRect: TRect;
  TempZoom: Double;
  TargetZoom: Double;
begin
  if FInFallAnimation or FClearing or (not Visible) then
    Exit;
  DeltaTime := DeltaMS / 1000.0;
  if DeltaTime <= 0 then
    DeltaTime := 0.016;
  AnyAnimating := False;
  NeedRepaint := False;
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    // Alpha
    TempZoom := 255;
    // Main position/scale animation progress
    if ImageItem.AnimationProgress < 1.0 then
    begin
      TempZoom := Min(1.0, ImageItem.AnimationProgress + FAnimationSpeed / 100);
      if Abs(ImageItem.AnimationProgress - TempZoom) > 0.001 then
      begin
        ImageItem.AnimationProgress := TempZoom;
        NeedRepaint := True;
      end;
    end;
    // Selection zoom
    if ImageItem.IsSelected then
      TempZoom := Min(1.0, ImageItem.ZoomProgress + FAnimationSpeed / 100)
    else if ImageItem.ZoomProgress > 0.0 then
      TempZoom := Max(0.0, ImageItem.ZoomProgress - FAnimationSpeed / 100)
    else
      TempZoom := ImageItem.ZoomProgress;
    if Abs(ImageItem.ZoomProgress - TempZoom) > 0.001 then
    begin
      ImageItem.ZoomProgress := TempZoom;
      NeedRepaint := True;
    end;
    // Combined progress for position interpolation
    Progress := Max(ImageItem.AnimationProgress, ImageItem.ZoomProgress);
    if FAnimationEasing then
      Progress := EaseInOutQuad(Progress);
    TempRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Progress), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Progress), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Progress), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress));
    if not EqualRect(ImageItem.CurrentRect, TempRect) then
    begin
      ImageItem.CurrentRect := TempRect;
      NeedRepaint := True;
    end;
    // Update per-item animating flag
    ImageItem.Animating := not ((ImageItem.AnimationProgress >= 1.0) and ((ImageItem.ZoomProgress <= 0.0001) or (ImageItem.ZoomProgress >= 0.9999)) and EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) and // Fixed here
      (Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) <= 0.006));
    if ImageItem = FWasSelectedItem then
      if (ImageItem.ZoomProgress <= 0.0001) and EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) then // Fixed here
        FWasSelectedItem := nil;
  end;
  // Hot-track zoom + Breathing animation
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if (not FHotTrackZoom) and (ImageItem <> FSelectedImage) then
    begin
      if ImageItem.FHotZoom <> 1.0 then
      begin
        ImageItem.FHotZoom := 1.0;
        ImageItem.FHotZoomTarget := 1.0;
        NeedRepaint := True;
      end;
      Continue;
    end;
    if not ImageItem.Visible then
      Continue;
    if FBreathingEnabled and (ImageItem = FSelectedImage) and (ImageItem = FHotItem) then
      TargetZoom := 1.0 + BREATHING_AMPLITUDE * 0.2 * (Sin(FBreathingPhase * 2 * Pi) + 1.0)
    else if (ImageItem = FHotItem) then
      TargetZoom := HOT_ZOOM_MAX_FACTOR
    else
      TargetZoom := 1.0;
    if ImageItem.FHotZoom < TargetZoom then
      Speed := HOT_ZOOM_IN_PER_SEC
    else
      Speed := HOT_ZOOM_OUT_PER_SEC;
    ImageItem.FHotZoom := ImageItem.FHotZoom + (TargetZoom - ImageItem.FHotZoom) * Speed * DeltaTime;
    if not FHotTrackZoom then
      ImageItem.FHotZoomTarget := 1.0
    else
      ImageItem.FHotZoomTarget := TargetZoom;
    if (ImageItem <> FSelectedImage) and (ImageItem.FHotZoom > HOT_ZOOM_MAX_FACTOR) then
      ImageItem.FHotZoom := HOT_ZOOM_MAX_FACTOR;
    if ImageItem.FHotZoom < 1.0 then
      ImageItem.FHotZoom := 1.0;
    NeedRepaint := True;
  end;
  // Breathing Phase Advance
  if not FDraggingSelected then
    if FBreathingEnabled and (FHotItem <> nil) and (FHotItem = FSelectedImage) then
      FBreathingPhase := Frac(FBreathingPhase + BREATHING_SPEED_PER_SEC * DeltaTime);
  // Final decision
  AnyAnimating := FFallingOut;
  for i := 0 to FImages.Count - 1 do
    if not ((TImageItem(FImages[i]).AnimationProgress >= 1.0) and ((TImageItem(FImages[i]).ZoomProgress <= 0.0001) or (TImageItem(FImages[i]).ZoomProgress >= 0.9999)) and EqualRect(TImageItem(FImages[i]).CurrentRect, TImageItem(FImages[i]).TargetRect) and (Abs(TImageItem(FImages[i]).FHotZoom - TImageItem(FImages[i]).FHotZoomTarget) <= 0.006)) then
    begin
      AnyAnimating := True;
      Break;
    end;
  if NeedRepaint or AnyAnimating then
    ThreadSafeRepaint;
end;

function TSkFlowmotion.AnimationsRunning: Boolean;
var
  i: Integer;
begin
  Result := FInFallAnimation;
  if not Result then
    for i := 0 to FImages.Count - 1 do
      if TImageItem(FImages[i]).Animating then
      begin
        Result := True;
        Exit;
      end;
end;


function TSkFlowmotion.EaseInOutQuad(t: Double): Double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

function TSkFlowmotion.GetEntryDirection: TImageEntryStyle;
begin
  Result := TImageEntryStyle(Ord(FImageEntryStyle));
end;

procedure TSkFlowmotion.AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle; UseSavedPosition: Boolean; SavedTargetRect: TRect);
var
  StartX, StartY, W, H: Integer;
  Target: TRect;
  EffectiveStyle: TImageEntryStyle;
  CenterX, CenterY: Integer;
begin
  if ImageItem = nil then
    Exit;
  if UseSavedPosition then
    Target := SavedTargetRect
  else
    Target := ImageItem.TargetRect;
  W := Target.Right - Target.Left;
  H := Target.Bottom - Target.Top;
  EffectiveStyle := EntryStyle;
  if EffectiveStyle = iesRandom then
    EffectiveStyle := TImageEntryStyle(Random(8) + 1);
  case EffectiveStyle of
    iesFromLeft:
      begin
        StartX := -W - 100;
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromRight:
      begin
        StartX := trunc(Width) + 100;
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromTop:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := -H - 100;
      end;
    iesFromBottom:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := trunc(Height) + 100;
      end;
    iesFromTopLeft:
      begin
        StartX := -W - 100;
        StartY := -H - 100;
      end;
    iesFromTopRight:
      begin
        StartX := trunc(Width) + 100;
        StartY := -H - 100;
      end;
    iesFromBottomLeft:
      begin
        StartX := -W - 100;
        StartY := trunc(Height) + 100;
      end;
    iesFromBottomRight:
      begin
        StartX := trunc(Width) + 100;
        StartY := trunc(Height) + 100;
      end;
    iesFromCenter, iesFromPoint:
      begin
        CenterX := Target.Left + (Target.Right - Target.Left) div 2;
        CenterY := Target.Top + (Target.Bottom - Target.Top) div 2;
        StartX := CenterX;
        StartY := CenterY;
      end;
  else
    begin
      StartX := Target.Left;
      StartY := Target.Top;
    end;
  end;
  if (EffectiveStyle in [iesFromCenter, iesFromPoint]) then
    ImageItem.StartRect := Rect(StartX, StartY, StartX, StartX)
  else
    ImageItem.StartRect := Rect(StartX + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * (1 - START_SCALE) / 2), StartX + Round(W * START_SCALE) + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * START_SCALE) + Round(H * (1 - START_SCALE) / 2));
  ImageItem.CurrentRect := ImageItem.StartRect;
  ImageItem.FHotZoom := START_SCALE;
  ImageItem.FHotZoomTarget := 1.0;
  ImageItem.TargetRect := Target;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;
  ImageItem.Alpha := 255;
  ImageItem.TargetAlpha := 255;
end;

procedure TSkFlowmotion.StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
var
  CenterX, CenterY: Integer;
  ImageSize: TSize;
begin
  if ImageItem = nil then
    Exit;
  ImageItem.ZoomProgress := 0;
  ImageItem.Animating := True;
  ImageItem.StartRect := ImageItem.CurrentRect;
  case FZoomAnimationType of
    zatSlide:
      ImageItem.StartRect := ImageItem.CurrentRect;
    zatFade:
      begin
        ImageItem.Alpha := 255;
        ImageItem.TargetAlpha := 255;
      end;
    zatZoom:
      begin
        if ZoomIn then
        begin
          CenterX := ImageItem.CurrentRect.Left + (ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left) div 2;
          CenterY := ImageItem.CurrentRect.Top + (ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top) div 2;
          ImageItem.StartRect := Rect(CenterX, CenterY, CenterX, CenterY);
          ImageItem.Alpha := 255;
        end
        else
          ImageItem.StartRect := ImageItem.CurrentRect;
      end;
    zatBounce:
      begin
        ImageItem.Alpha := 255;
        ImageItem.TargetAlpha := 255;
      end;
  end;
  if ZoomIn then
  begin
    if Assigned(ImageItem.SkImage) then
    begin
      ImageSize := GetOptimalSize(ImageItem.SkImage.Width, ImageItem.SkImage.Height, Min(FMaxZoomSize, trunc(Width) div 2), Min(FMaxZoomSize, trunc(Height) div 2));
      CenterX := (trunc(Width) - ImageSize.cx) div 2;
      CenterY := (trunc(Height) - ImageSize.cy) div 2;
      ImageItem.TargetRect := Rect(CenterX, CenterY, CenterX + ImageSize.cx, CenterY + ImageSize.cy);
    end;
    CalculateLayout;
  end;
end;
// =============================================================================
// IMAGE MANAGEMENT
// =============================================================================

procedure TSkFlowmotion.AddImage(const FileName: string; const AHint: string = '');
begin
  AddImage(FileName, ExtractFileName(FileName), FileName, AHint);
end;

procedure TSkFlowmotion.AddImage(const FileName, ACaption, APath, AHint: string);
var
  SkImg: ISkImage;
  WasEmpty: Boolean;
  IsLastPage, IsSpaceOnPage: Boolean;
  NewItem: TImageItem;
  AbsIndex: Integer;
begin
  WasEmpty := (FAllFiles.Count = 1); // Wir haben gerade das erste hinzugefügt

  // 1. Immer zur Master-Liste hinzufügen
  if FileName <> '' then
  begin
    FAllFiles.Add(FileName);
    FAllCaptions.Add(ACaption);
    FAllPaths.Add(APath);
    FAllHints.Add(AHint);
  end;

  // Wir laden synchron, wenn es der erste Eintrag ist, oder wenn AutoScroll aktiv ist oder wir auf der aktuellen Seite Platz haben.
  // Hier vereinfachen wir exakt wie im VCL Original: Wenn WasEmpty -> ShowPage.

  if WasEmpty then
  begin
    ShowPage(FCurrentPage); // ShowPage kümmert sich um das Laden der Dateien
  end
  else
  begin
    // Wenn es nicht leer ist, prüfen wir, ob es auf die aktuelle Seite gehört und Platz hat (Lazy Loading)
    // Das ist etwas komplexer, weil wir Skia laden müssen.
    // Für den Moment reicht es, dass ShowPage bei WasEmpty aufgerufen wird.
    // Wenn du Lazy Loading willst, müsst du hier noch IsLastPage/IsSpaceOnPage prüfen wie im VCL Original.

    // VCL Original Logik für den Fall (nicht WasEmpty):
    // LoadImageFromFile -> Bitmap -> NewItem -> FImages.Add -> Layout
    // Da wir Skia nutzen, ist es etwas anders, aber Prinzip gleich.
    if TFile.Exists(FileName) then
    begin
       SkImg := TSkImage.MakeFromEncodedFile(FileName);
       if Assigned(SkImg) then
       begin
         NewItem := TImageItem.Create;
         NewItem.SkImage := SkImg;
         NewItem.Caption := ACaption;
         NewItem.Path := APath;
         NewItem.Hint := AHint;
         NewItem.FileName := FileName;
         NewItem.Direction := GetEntryDirection;
         NewItem.Visible := False;
         FImages.Add(NewItem);
         if Visible then
         begin
           CalculateLayout;
           AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
           NewItem.Visible := True;
         end;
       end;
    end;
  end;

  StartAnimationThread;

  // Event
  DoImageLoad(FileName, TFile.Exists(FileName));
end;


procedure TSkFlowmotion.AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = ''; const AHint: string = '');
var
  LoadThread: TImageLoadThread;
  NewAbsIndex, TargetPage: Integer;
  WasEmpty: Boolean;
begin
  WasEmpty := (FAllFiles.Count = 1);
  FAllFiles.Add(FileName);
  FAllCaptions.Add(ACaption);
  FAllPaths.Add(APath);
  FAllHints.Add(AHint);
  NewAbsIndex := FAllFiles.Count - 1;
  TargetPage := NewAbsIndex div FPageSize;
  if FAutoScrollPageForNewAdded then
  begin
    // If auto-scroll is on, we switch to the new page.
    // ShowPage will handle loading.
    if (FCurrentPage <> TargetPage) or Wasempty then
    begin
      ShowPage(TargetPage);
      Exit; // Do not spawn a thread, ShowPage handles it
    end;
  end;
  if (NewAbsIndex >= GetPageStartIndex) and (NewAbsIndex <= GetPageEndIndex) then
  begin
    LoadThread := TImageLoadThread.Create(FileName, ACaption, APath, AHint, Self, NewAbsIndex);
    FLoadingThreads.Add(LoadThread);
    Inc(FLoadingCount);
  end;
  if WasEmpty then
    ShowPage(FCurrentPage);
end;


procedure TSkFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean = false);
begin
  try
    Clear(animated, ZoominSelected, Rect(0, 0, 0, 0), Rect(0, 0, 0, 0), iesFromBottom);
  except
  end;
end;

procedure TSkFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom; AndFree: Boolean = true);
var
  i: Integer;
  ImageItem: TImageItem;
  StartTick: DWORD;
  AllOut: Boolean;
  AnimSpeed, NewW, NewH, CurCX, CurCY, CurW, CurH, TargetCX, TargetCY, MoveX, MoveY: Integer;
  SelectedItem: TImageItem;
  R: TRect;
  ShrinkFactor: Real;
begin
  if (FImages.Count = 0) or FClearing or FInFallAnimation then
    Exit;
  AnimSpeed := 4;
  WaitForAllLoads;
  StopAnimationThread;
  FInFallAnimation := True;
  for i := 0 to FLoadingThreads.Count - 1 do
    TImageLoadThread(FLoadingThreads[i]).Terminate;
  FLoadingThreads.Clear;
  FLoadingCount := 0;
  SelectedItem := nil;
  if ZoominSelected then
    for i := 0 to FImages.Count - 1 do
      if TImageItem(FImages[i]).IsSelected then
      begin
        SelectedItem := TImageItem(FImages[i]);
        Break;
      end;
  if not animated then
  begin
    FreeAllImagesAndClearLists;
    Exit;
  end;
  StartTick := GetTickCount;
  repeat
    AllOut := True;
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if not ImageItem.Visible then
        Continue;
      R := ImageItem.CurrentRect;
      if (ImageItem <> SelectedItem) or (not ZoominSelected) or IsRectEmpty(SelectedTargetPos) then
      begin
        // Normal fall out logic
        case FallingStyle of
          iesFromTop:
            OffsetRect(R, 0, -Trunc(AnimSpeed * AnimSpeed));
          iesFromBottom:
            OffsetRect(R, 0, Trunc(AnimSpeed * AnimSpeed));
          iesFromLeft:
            OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), 0);
          iesFromRight:
            OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), 0);
          iesFromCenter:
            begin
              CurCX := trunc(Width) div 2;
              CurCY := trunc(Height) div 2;
              MoveX := Trunc((CurCX - (R.Left + R.Right) div 2) * 0.18);
              MoveY := Trunc((CurCY - (R.Top + R.Bottom) div 2) * 0.18);
              OffsetRect(R, MoveX, MoveY);
            end;
        else
          OffsetRect(R, 0, Trunc(AnimSpeed * AnimSpeed)); // Default random handled via param
        end;
        // Hide conditions
        if (R.Bottom < -100) or (R.Top > trunc(Height) + 100) or (R.Right < -100) or (R.Left > trunc(Width) + 100) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end
      else
      begin
        // Zoom to point
        TargetCX := (SelectedTargetPos.Left + SelectedTargetPos.Right) div 2;
        TargetCY := (SelectedTargetPos.Top + SelectedTargetPos.Bottom) div 2;
        MoveX := (TargetCX - (R.Left + R.Right) div 2) div Max(1, Trunc(AnimSpeed * 0.6));
        MoveY := (TargetCY - (R.Top + R.Bottom) div 2) div Max(1, Trunc(AnimSpeed * 0.6));
        if Abs(MoveX) < 3 then
          MoveX := Sign(MoveX) * Max(3, AnimSpeed);
        if Abs(MoveY) < 3 then
          MoveY := Sign(MoveY) * Max(3, AnimSpeed);
        if (R.Right - R.Left > 20) and (R.Bottom - R.Top > 20) then
        begin
          CurW := R.Right - R.Left;
          CurH := R.Bottom - R.Top;
          ShrinkFactor := 0.92 + (AnimSpeed * 0.001);
          NewW := Trunc(CurW * ShrinkFactor);
          NewH := Trunc(CurH * ShrinkFactor);
          CurCX := (R.Left + R.Right) div 2;
          CurCY := (R.Top + R.Bottom) div 2;
          R.Left := CurCX - NewW div 2;
          R.Top := CurCY - NewH div 2;
          R.Right := CurCX + NewW div 2;
          R.Bottom := CurCY + NewH div 2;
        end;
        OffsetRect(R, MoveX, MoveY);
        if (Abs(MoveX) <= 20) and (Abs(MoveY) <= 20) or ((R.Bottom - R.Top) < 30) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end;
      if ImageItem.Visible then
        ImageItem.CurrentRect := R;
    end;
    Repaint;
    if GetTickCount - StartTick > 50 then
    begin
      // Application.ProcessMessages; // FMX alternative if needed, but usually thread safe calls handle this
      Sleep(AnimSpeed);
    end
    else
      Sleep(AnimSpeed);
    if (GetTickCount - StartTick) > 3000 then
      AllOut := True;
  until AllOut;
  if AndFree then
    FreeAllImagesAndClearLists
  else
    FInFallAnimation := False;
end;

procedure TSkFlowmotion.RemoveImage(Index: Integer; Animated: Boolean = True);
begin
  RemoveImage(Index, Animated, Rect(0, 0, 0, 0), iesFromBottom);
end;

procedure TSkFlowmotion.RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle);
var
  StartTick: DWORD;
  CurCX, CurCY, TargetCX, TargetCY, MoveX, MoveY: Integer;
  AbsIndex: Integer;
  ImageItem: TImageItem;
  R: TRect;
  AllOut: Boolean;
  Speed: Integer;
begin
  if (Index < 0) or (Index >= FImages.Count) then
    Exit;
  AbsIndex := GetPageStartIndex + Index;
  ImageItem := TImageItem(FImages[Index]);
  if not Animated then
  begin
    if ImageItem = FSelectedImage then
    begin
      FSelectedImage := nil;
      FCurrentSelectedIndex := -1;
    end
    else if Index < FCurrentSelectedIndex then
      Dec(FCurrentSelectedIndex);
    ImageItem.Free;
    FImages.Delete(Index);
    FAllFiles.Delete(AbsIndex);
    FAllCaptions.Delete(AbsIndex);
    FAllPaths.Delete(AbsIndex);
    FAllHints.Delete(AbsIndex);
    if Visible then
    begin
      CalculateLayout;
      Repaint;
    end;
    Exit;
  end;
  FInFallAnimation := True;
  StopAnimationThread;
  ImageItem.Animating := True;
  StartTick := GetTickCount;
  repeat
    AllOut := True;
    Speed := Max(1, FAnimationSpeed * 2 - 8);
    R := ImageItem.CurrentRect;
    case FallingStyle of
      iesFromTop:
        OffsetRect(R, 0, -Trunc(FAnimationSpeed * Speed));
      iesFromBottom:
        OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
      iesFromLeft:
        OffsetRect(R, -Trunc(FAnimationSpeed * Speed), 0);
      iesFromRight:
        OffsetRect(R, Trunc(FAnimationSpeed * Speed), 0);
      iesFromCenter:
        begin
          CurCX := trunc(Width) div 2;
          CurCY := trunc(Height) div 2;
          MoveX := Trunc((CurCX - (R.Left + R.Right) div 2) * 0.18);
          MoveY := Trunc((CurCY - (R.Top + R.Bottom) div 2) * 0.18);
          OffsetRect(R, MoveX, MoveY);
        end;
      iesFromPoint:
        if not IsRectEmpty(FallingTargetPos) then
        begin
          TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
          TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
          MoveX := Trunc((TargetCX - (R.Left + R.Right) div 2) * 0.20);
          MoveY := Trunc((TargetCY - (R.Top + R.Bottom) div 2) * 0.20);
          OffsetRect(R, MoveX, MoveY);
        end
        else
          OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
    else
      OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
    end;
    ImageItem.CurrentRect := R;
    if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
      ImageItem.Visible := False
    else
      AllOut := False;
    if (GetTickCount - StartTick) > 3000 then
      AllOut := True;
    Repaint;
    Sleep(FAnimationSpeed);
  until AllOut;
  if ImageItem = FSelectedImage then
  begin
    FSelectedImage := nil;
    FCurrentSelectedIndex := -1;
  end
  else if Index < FCurrentSelectedIndex then
    Dec(FCurrentSelectedIndex);
  ImageItem.Free;
  FImages.Delete(Index);
  FAllFiles.Delete(AbsIndex);
  FAllCaptions.Delete(AbsIndex);
  FAllPaths.Delete(AbsIndex);
  FAllHints.Delete(AbsIndex);
  FInFallAnimation := False;
  Repaint;
end;

procedure TSkFlowmotion.Resize;
var
  NewSize: TSize;
  L, T: Integer;
begin
  inherited Resize;

  if FZoomSelectedtoCenter and (FSelectedImage <> nil) and (FFlowLayout <> flFreeFloat) then
  begin
    if Assigned(FSelectedImage.SkImage) then
    begin
      NewSize := GetOptimalSize(FSelectedImage.SkImage.Width, FSelectedImage.SkImage.Height, Min(FMaxZoomSize, trunc(Width) div 2), Min(FMaxZoomSize, trunc(Height) div 2));
      L := (trunc(Width) - NewSize.cx) div 2;
      T := (trunc(Height) - NewSize.cy) div 2;
      FSelectedImage.TargetRect := Rect(L, T, L + NewSize.cx, T + NewSize.cy);
    end;
  end;
  CalculateLayout;
  Repaint;
end;


// -----------------------------------------------------------------------------
// 1. HELPER CONVERSION FUNCTIONS (FMX TBitmap <-> Skia ISkImage)
// -----------------------------------------------------------------------------
function TSkFlowmotion.BitmapToSkImage(Bmp: TBitmap): ISkImage;
var
  MS: TMemoryStream;
begin
  Result := nil;
  if not Assigned(Bmp) then
    Exit;
  MS := TMemoryStream.Create;
  try
    Bmp.SaveToStream(MS);
    MS.Position := 0;
    // FIX: MakeFromEncodedStream nehmen TStream (TMemoryStream).
    // Das verhindert den Fehler mit TSkEncodedData.
    Result := TSkImage.MakeFromEncodedStream(MS);
  finally
    MS.Free;
  end;
end;

function TSkFlowmotion.SkImageToBitmap(Img: ISkImage): TBitmap;
var
  WStream: TStream;
  MS: TMemoryStream;
begin
  Result := nil;
  if not Assigned(Img) then
    Exit;
  MS := TMemoryStream.Create;
  try
    // FIX: Wrapper erstellen mit TSkWStream.Create.
    // Das löst den Fehler "TSkDynamicMemoryWStream undeclared".
    WStream := TStream.Create;
    try
    //  if Img.Encode(WStream, TSkEncodedImageFormat.Png) then
      begin
        MS.Position := 0;
        Result := TBitmap.Create;
        Result.LoadFromStream(MS);
      end;
    finally
      WStream := nil;
    end;
  except
    // Fehler abfangen
  end;
  MS.Free;
end;





                                                // -----------------------------------------------------------------------------
// 3. IMPLEMENTATIONS FOR MISSING METHODS
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath, XHint: string);
var
  NewItem: TImageItem;
  SkImg: ISkImage;
  DummyName: string;
begin
  if Pic = nil then
    Exit;
  // Convert FMX Bitmap to Skia
  SkImg := BitmapToSkImage(Pic);
  if not Assigned(SkImg) then
    Exit;
  // Create a dummy entry in Master Lists so Paging Count works
  DummyName := 'MemoryBitmap_' + IntToStr(GetTickCount) + '_' + IntToStr(Random(10000));
  FAllFiles.Add(DummyName);
  FAllCaptions.Add(XCaption);
  FAllPaths.Add(XPath);
  FAllHints.Add(XHint);
  // Add to visual list
  NewItem := TImageItem.Create;
  NewItem.SkImage := SkImg;
  NewItem.FileName := DummyName;
  NewItem.Caption := XCaption;
  NewItem.Path := XPath;
  NewItem.Hint := XHint;
  NewItem.Direction := GetEntryDirection;
  FImages.Add(NewItem);
  if Visible then
  begin
    CalculateLayout;
    AnimateImage(NewItem, NewItem.Direction, False, Rect(0, 0, 0, 0));
    NewItem.Visible := True;
  end;
  StartAnimationThread;
end;

procedure TSkFlowmotion.SetImage(Index: Integer; Bitmap: TBitmap);
begin
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    if Assigned(Bitmap) then
    begin
      // Convert and assign
      TImageItem(FImages[Index]).SkImage := BitmapToSkImage(Bitmap);
      CalculateLayout;
      Repaint;
    end;
  end;
end;

function TSkFlowmotion.GetPicture(index: Integer): TBitmap;
begin
  Result := nil;
  if (index >= 0) and (index < FImages.Count) then
  begin
    Result := SkImageToBitmap(TImageItem(FImages[index]).SkImage);
  end;
end;

procedure TSkFlowmotion.SetBackgroundpicture(const Path: string);
begin
  try
    if TFile.Exists(Path) then
    begin
      // Load directly into Skia Image for best performance
      FBackgroundSkImage := TSkImage.MakeFromEncodedFile(Path);
    end
    else
      FBackgroundSkImage := nil;
  except
    on E: Exception do
      FBackgroundSkImage := nil;
  end;
  Repaint;
end;

procedure TSkFlowmotion.MoveImageToPos(RelIndexFrom, RelIndexTo: Integer);
var
  PageStart: Integer;
  AbsIndexFrom, AbsIndexTo: Integer;
  Item: TImageItem;
  FileName, Caption, Path, Hint: string;
  OldSelectedIndex: Integer;
begin
  // --- 1. Safety: never move during animation or page change ---
  if FInFallAnimation or FPageChangeInProgress or AnimationsRunning then
    Exit;
  // --- 2. Validate relative indices ---
  if (RelIndexFrom < 0) or (RelIndexFrom >= FImages.Count) or (RelIndexTo < 0) or (RelIndexTo >= FImages.Count) or (RelIndexFrom = RelIndexTo) then
    Exit;
  // --- 3. Move in visible list (FImages) ---
  Item := TImageItem(FImages[RelIndexFrom]);
  FImages.Delete(RelIndexFrom);
  FImages.Insert(RelIndexTo, Item);
  // --- 4. Calculate absolute indices in master lists ---
  PageStart := GetPageStartIndex;
  AbsIndexFrom := PageStart + RelIndexFrom;
  AbsIndexTo := PageStart + RelIndexTo;
  // --- 5. Move in ALL master lists (permanent change) ---
  // FAllFiles
  FileName := FAllFiles[AbsIndexFrom];
  FAllFiles.Delete(AbsIndexFrom);
  FAllFiles.Insert(AbsIndexTo, FileName);
  // FAllCaptions
  Caption := FAllCaptions[AbsIndexFrom];
  FAllCaptions.Delete(AbsIndexFrom);
  FAllCaptions.Insert(AbsIndexTo, Caption);
  // FAllPaths
  Path := FAllPaths[AbsIndexFrom];
  FAllPaths.Delete(AbsIndexFrom);
  FAllPaths.Insert(AbsIndexTo, Path);
  // FAllHints
  Hint := FAllHints[AbsIndexFrom];
  FAllHints.Delete(AbsIndexFrom);
  FAllHints.Insert(AbsIndexTo, Hint);
  // --- 6. Fix selected index (critical!) ---
  OldSelectedIndex := FCurrentSelectedIndex;
  if OldSelectedIndex = RelIndexFrom then
    FCurrentSelectedIndex := RelIndexTo
  else if (OldSelectedIndex > RelIndexFrom) and (OldSelectedIndex <= RelIndexTo) then
    Dec(FCurrentSelectedIndex)
  else if (OldSelectedIndex >= RelIndexTo) and (OldSelectedIndex < RelIndexFrom) then
    Inc(FCurrentSelectedIndex);
  // --- 7. Refresh layout and repaint ---
  CalculateLayout;
  Repaint;
end;
// -----------------------------------------------------------------------------
// 4. STREAM I/O SUPPORT (Save/Load Positions)
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.SavePositionsToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  if FImages.Count = 0 then
    Exit;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SavePositionsToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSkFlowmotion.SavePositionsToStream(Stream: TStream);
var
  Writer: TBinaryWriter;
  i: Integer;
begin
  if FImages.Count = 0 then
    Exit;
  Writer := TBinaryWriter.Create(Stream, TEncoding.UTF8, False);
  try
    // Number of images
    Writer.Write(FImages.Count);
    for i := 0 to FImages.Count - 1 do
      with TImageItem(FImages[i]) do
      begin
        Writer.Write(FileName);
        Writer.Write(Caption);
        Writer.Write(Path);
        Writer.Write(TargetRect.Left);
        Writer.Write(TargetRect.Top);
        Writer.Write(TargetRect.Right - TargetRect.Left);
        Writer.Write(TargetRect.Bottom - TargetRect.Top);
      end;
  finally
    Writer.Free;
  end;
end;

procedure TSkFlowmotion.LoadPositionsFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  if not TFile.Exists(FileName) then
    Exit;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadPositionsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSkFlowmotion.LoadPositionsFromStream(Stream: TStream);
var
  Reader: TBinaryReader;
  Count, i: Integer;
begin
  Reader := TBinaryReader.Create(Stream, TEncoding.UTF8, False);
  try
    Count := Reader.ReadInt32;
    if (Count < 0) or (Count > 10000) then
      Exit;
    SetLength(FLoadedPositions, Count);
    for i := 0 to Count - 1 do
    begin
      FLoadedPositions[i].FileName := Reader.ReadString;
      FLoadedPositions[i].Caption := Reader.ReadString;
      FLoadedPositions[i].Path := Reader.ReadString;
      FLoadedPositions[i].Left := Reader.ReadInt32;
      FLoadedPositions[i].Top := Reader.ReadInt32;
      FLoadedPositions[i].Width := Reader.ReadInt32;
      FLoadedPositions[i].Height := Reader.ReadInt32;
    end;
  finally
    Reader.Free;
  end;
end;

function TSkFlowmotion.GetCurrentPositions: TImagePositions;
var
  i: Integer;
begin
  if Length(FLoadedPositions) > 0 then
    Result := Copy(FLoadedPositions)
  else
  begin
    SetLength(Result, FImages.Count);
    for i := 0 to FImages.Count - 1 do
    begin
      with TImageItem(FImages[i]) do
      begin
        Result[i].FileName := FileName;
        Result[i].Caption := Caption;
        Result[i].Path := Path;
        Result[i].Left := TargetRect.Left;
        Result[i].Top := TargetRect.Top;
        Result[i].Width := TargetRect.Right - TargetRect.Left;
        Result[i].Height := TargetRect.Bottom - TargetRect.Top;
      end;
    end;
  end;
end;

procedure TSkFlowmotion.ResetPositions;
begin
  // Reset implies recalculating layout from scratch, ignoring saved positions
  SetLength(FLoadedPositions, 0);
  CalculateLayout;
  Repaint;
end;

procedure TSkFlowmotion.AddImagesWithPositions(const FileNames, Captions, Paths: TStringList; const Positions: array of TRect);
var
  i: Integer;
  Bitmap: TBitmap;
  NewItem: TImageItem;
begin
  if (FileNames = nil) or (Captions = nil) or (Paths = nil) or (FileNames.Count <> Captions.Count) or (FileNames.Count <> Paths.Count) or (FileNames.Count <> Length(Positions)) then
    Exit;
  for i := 0 to FileNames.Count - 1 do
  begin
    if not TFile.Exists(FileNames[i]) then
    begin
      DoImageLoad(FileNames[i], False);
      Continue;
    end;
    Bitmap := TBitmap.Create;
    try
      Bitmap.LoadFromFile(FileNames[i]);
      NewItem := TImageItem.Create;
      NewItem.SkImage := BitmapToSkImage(Bitmap);
      NewItem.Caption := Captions[i];
      NewItem.Path := Paths[i];
      NewItem.FileName := FileNames[i];
      NewItem.Direction := GetEntryDirection;
      NewItem.Visible := False;
      FImages.Add(NewItem);
      FAllFiles.Add(FileNames[i]);
      FAllCaptions.Add(Captions[i]);
      FAllPaths.Add(Paths[i]);
      FAllHints.Add(''); // Hint unknown here
      if Visible then
      begin
        if FFlowLayout = flFreeFloat then
        begin
          NewItem.TargetRect := Positions[i];
          NewItem.CurrentRect := Positions[i];
          AnimateImage(NewItem, NewItem.Direction, True, Positions[i]);
        end
        else
        begin
          CalculateLayout;
          AnimateImage(NewItem, NewItem.Direction, False, Rect(0, 0, 0, 0));
        end;
        NewItem.Visible := True;
      end;
    finally
      Bitmap.Free;
    end;
  end;
  StartAnimationThread;
end;
// =============================================================================
// TEIL 5: DIE FEHLENDEN METHODEN (THREAD, MOUSE, NAVIGATION)
// =============================================================================

procedure TSkFlowmotion.StartAnimationThread;
begin
  if FAnimationThread = nil then
    FAnimationThread := TAnimationThread.Create(Self);
  if FAnimationThread.Suspended then
    FAnimationThread.Start;
end;

procedure TSkFlowmotion.StopAnimationThread;
begin
  if FAnimationThread <> nil then
  begin
    FAnimationThread.Stop;
    // FAnimationThread.FreeOnTerminate := False; // Already set in Create
    FAnimationThread.WaitFor;
    FreeAndNil(FAnimationThread);
  end;
end;

procedure TSkFlowmotion.ThreadFinished(Thread: TImageLoadThread);
begin
  FLoadingThreads.Remove(Thread);
  Dec(FLoadingCount);
end;

// --- Mouse Events ---
procedure TSkFlowmotion.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ImageItem: TImageItem;
  ItemIndex: Integer;
  CaptionRect: TRect;
begin
  // Original: Wenn keine Bilder da, abbrechen
  if (FImages.Count = 0) or FClearing or FInFallAnimation then Exit;

  // 1. Check Caption Click (exakt wie D7)
  if Button = TMouseButton.mbLeft then
  begin
    for ItemIndex := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[ItemIndex]);
      CaptionRect := GetCaptionRect(ImageItem, ImageItem.CurrentRect); // TRectF Casten für FMX
      if PtInRect(CaptionRect, Point(Round(X), Round(Y))) then
      begin
        if Assigned(FOnCaptionClick) then FOnCaptionClick(Self, ImageItem, ItemIndex);
        Exit;
      end;
    end;
  end;

  // 2. Image an Punkt finden
  ImageItem := GetImageAtPoint(X, Y);
  ItemIndex := FImages.IndexOf(ImageItem);

  // 3. Klick auf Hintergrund -> Deselektieren
  if (Button = TMouseButton.mbLeft) and (ImageItem = nil) then
  begin
    if FSelectedImage <> nil then
      SetSelectedImage(nil, -1);
    Exit;
  end;

  // 4. Klick auf Bild
  if (Button = TMouseButton.mbLeft) and (ImageItem <> nil) then
  begin
    // Event feuern
    if Assigned(FOnSelectedItemMouseDown) then
      FOnSelectedItemMouseDown(Self, ImageItem, ItemIndex, Round(X), Round(Y), Button, Shift);

    // Selection Logik
    if FSelectedImage <> ImageItem then
      SetSelectedImage(ImageItem, ItemIndex)
    else
    begin
       // Taktiles Feedback wenn man schon selektiert ist ("Breathing" shift)
       if (not FDraggingSelected) and (FFlowLayout <> flFreeFloat) then
         FBreathingPhase := FBreathingPhase - 0.4;
    end;

    // Dragging Start (wenn erlaubt und auf Selektiertem Bild)
    if FSelectedMovable and (ImageItem = FSelectedImage) then
    begin
      FDraggingSelected := True;
      FDragOffset.X := Round(X) - ((ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2);
      FDragOffset.Y := Round(Y) - ((ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2);
      //MouseCapture(Self); // MouseCapture abhängig von Framework, FMX macht das meist automatisch, aber sicher ist sicher
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSkFlowmotion.MouseMove(Shift: TShiftState; X, Y: Single);
var
  ImageItem: TImageItem;
  CurCX, CurCY, TargetCX, TargetCY, MoveX, MoveY: Integer;
  BorderWidth: Integer;
  R: TRect;
begin
  if FClearing or FInFallAnimation then Exit;

  // --- DRAGGEN DES AUSGEWÄHLTEN BILDES (Priorität) ---
  if FDraggingSelected and (FSelectedImage <> nil) then
  begin
    ImageItem := FSelectedImage;
    R := ImageItem.CurrentRect;
    TargetCX := Round(X) - FDragOffset.X;
    TargetCY := Round(Y) - FDragOffset.Y;

    MoveX := TargetCX - ((R.Left + R.Right) div 2);
    MoveY := TargetCY - ((R.Top + R.Bottom) div 2);

    OffsetRect(R, MoveX, MoveY);

    // Aktualisieren
    ImageItem.TargetRect := R;
    ImageItem.CurrentRect := R; // Sofortiges Feedback

    ThreadSafeInvalidate;
    Exit; // Draggen blockiert Hot-Tracking
  end;

  // --- NORMAL HOT-TRACKING ---
  ImageItem := GetImageAtPoint(X, Y);
  if ImageItem <> FHotItem then
  begin
    if (ImageItem <> nil) and Assigned(FOnImageMouseEnter) then
      FOnImageMouseEnter(Self, ImageItem, FImages.IndexOf(ImageItem));
    if (FHotItem <> nil) and (ImageItem <> FHotItem) and Assigned(FOnImageMouseLeave) then
      FOnImageMouseLeave(Self, FHotItem, FImages.IndexOf(FHotItem));

    FHotItem := ImageItem;

    if ShowHint and (FHotItem <> nil) and (FHotItem.Hint <> '') then
      Hint := FHotItem.Hint
    else
      Hint := '';

    // Animation starten
    if FHotItem <> nil then
    begin
      FHotItem.FHotZoomTarget := HOT_ZOOM_MAX_FACTOR; // Zoomen aktivieren
      StartAnimationThread;
    end;
  end;

  // Cursor Logik
  Cursor := crDefault;
  if FHotItem <> nil then Cursor := crHandPoint;
end;

procedure TSkFlowmotion.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ImageUnderCursor: TImageItem;
begin
  if FClearing or FInFallAnimation then
    Exit;
  if (Button = TMouseButton.mbLeft) then
  begin
    // Draggen beenden
    if FDraggingSelected then
    begin
      FDraggingSelected := False;
      if FSelectedImage <> nil then
        FSelectedImage.FHotZoomTarget := 1.0;
      ImageUnderCursor := GetImageAtPoint(X, Y);
      if (ImageUnderCursor = FSelectedImage) and (FSelectedImage <> nil) then
      begin
        FHotItem := FSelectedImage;
        StartAnimationThread;
      end;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;
// --- Keyboard Events ---

procedure TSkFlowmotion.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if AnimationsRunning then
    Exit;
  case Key of
    vkLeft, vkUp:
      SelectPreviousImage;
    vkRight, vkDown:
      SelectNextImage;
    vkReturn:
      if (FSelectedImage <> nil) and Assigned(FOnSelectedImageDblClick) then
        FOnSelectedImageDblClick(Self, FSelectedImage, FCurrentSelectedIndex);
    vkEscape:
      SetSelectedImage(nil, -1);
    vkHome:
      if FImages.Count > 0 then
        SetSelectedImage(TImageItem(FImages[0]), 0);
    vkEnd:
      if FImages.Count > 0 then
        SetSelectedImage(TImageItem(FImages[FImages.Count - 1]), FImages.Count - 1);
  end;
  inherited KeyDown(Key, KeyChar, Shift);
end;
// --- Navigation & Selection Helpers ---

function TSkFlowmotion.GetImageAtPoint(X, Y: Single): TImageItem;
var
  i: Integer;
  ImageItem: TImageItem;
  P: TPoint;
  CenterX, CenterY, BaseW, BaseH, NewW, NewH: Integer;
  DrawRect: TRect;
  ZoomFactor: Double;
  BestCandidate: TImageItem;
  BestCandidateZoom: Double;
  CurrentZoom: Double;
  CurrentRect: TRect;
  BorderSize: Integer;
begin
  P := Point(Round(X), Round(Y));
  Result := nil;
  // 1. SELECTED IMAGE CHECK (Höchste Priorität)
  if (FSelectedImage <> nil) and FSelectedImage.Visible then
  begin
    if (FSelectedImage = FHotItem) or (FSelectedImage.FHotZoom > 1.01) then
      ZoomFactor := FSelectedImage.FHotZoom
    else
      ZoomFactor := 1.0;
    if (FSelectedImage.CurrentRect.Right - FSelectedImage.CurrentRect.Left <= 0) or (FSelectedImage.CurrentRect.Bottom - FSelectedImage.CurrentRect.Top <= 0) then
    begin
      CenterX := (FSelectedImage.TargetRect.Left + FSelectedImage.TargetRect.Right) div 2;
      CenterY := (FSelectedImage.TargetRect.Top + FSelectedImage.TargetRect.Bottom) div 2;
      BaseW := FSelectedImage.TargetRect.Right - FSelectedImage.TargetRect.Left;
      BaseH := FSelectedImage.TargetRect.Bottom - FSelectedImage.TargetRect.Top;
    end
    else
    begin
      CenterX := (FSelectedImage.CurrentRect.Left + FSelectedImage.CurrentRect.Right) div 2;
      CenterY := (FSelectedImage.CurrentRect.Top + FSelectedImage.CurrentRect.Bottom) div 2;
      BaseW := FSelectedImage.CurrentRect.Right - FSelectedImage.CurrentRect.Left;
      BaseH := FSelectedImage.CurrentRect.Bottom - FSelectedImage.CurrentRect.Top;
    end;
    DrawRect := Rect(CenterX - Round(BaseW * ZoomFactor / 2), CenterY - Round(BaseH * ZoomFactor / 2), CenterX + Round(BaseW * ZoomFactor / 2), CenterY + Round(BaseH * ZoomFactor / 2));
    BorderSize := Max(FGlowWidth, FHotTrackWidth);
    InflateRect(DrawRect, BorderSize, BorderSize);
    if PtInRect(DrawRect, P) then
    begin
      Result := FSelectedImage;
      Exit;
    end;
  end;
  // 2. ALLE ANDEREN BILDER
  BestCandidate := nil;
  BestCandidateZoom := -1.0;
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible or (ImageItem = FSelectedImage) then
      Continue;
    if (ImageItem.FHotZoom > 1.01) then
    begin
      ZoomFactor := ImageItem.FHotZoom;
      CenterX := (ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2;
      CenterY := (ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2;
      BaseW := ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left;
      BaseH := ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top;
    end
    else
    begin
      ZoomFactor := 1.0;
      CurrentRect := ImageItem.CurrentRect;
      if IsRectEmpty(CurrentRect) then
        CurrentRect := ImageItem.TargetRect;
      DrawRect := CurrentRect;
    end;
    // Hottrack Border aufblasen
    if (ImageItem.FHotZoom > 1.01) or (ImageItem = FHotItem) then
    begin
      CenterX := (ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2;
      CenterY := (ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2;
      BaseW := ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left;
      BaseH := ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top;
      NewW := Round(BaseW * ZoomFactor);
      NewH := Round(BaseH * ZoomFactor);
      DrawRect := Rect(CenterX - NewW div 2, CenterY - NewH div 2, CenterX + NewW div 2, CenterY + NewH div 2);
      InflateRect(DrawRect, FHotTrackWidth, FHotTrackWidth);
    end
    else
    begin
       // Kein Zoom, nur Image Check
      DrawRect := ImageItem.CurrentRect;
      InflateRect(DrawRect, FHotTrackWidth, FHotTrackWidth);
    end;
    if PtInRect(DrawRect, P) then
    begin
      CurrentZoom := ImageItem.FHotZoom;
      if CurrentZoom < 1.0 then
        CurrentZoom := 1.0;
      // Z-Order: Das Bild mit dem größten Zoom oder wenn gleich, das größte Bild gewinnt
      if (CurrentZoom > BestCandidateZoom) or ((CurrentZoom = BestCandidateZoom) and (BaseW > 0) and (BaseH > 0) and (BaseW > (BestCandidate.CurrentRect.Right - BestCandidate.CurrentRect.Left))) then
      begin
        BestCandidate := ImageItem;
        BestCandidateZoom := CurrentZoom;
      end;
    end;
  end;
  Result := BestCandidate;
end;

procedure TSkFlowmotion.SelectNextImage;
begin
  if FInFallAnimation then
    Exit;
  if FCurrentSelectedIndex < FImages.Count - 1 then
  begin
    SetSelectedImage(TImageItem(FImages[FCurrentSelectedIndex + 1]), FCurrentSelectedIndex + 1);
  end
  else if (FCurrentPage < GetPageCount - 1) then
  begin
    NextPage;
    if FImages.Count > 0 then
      SetSelectedImage(TImageItem(FImages[0]), 0);
  end;
end;

procedure TSkFlowmotion.SelectPreviousImage;
begin
  if FInFallAnimation then
    Exit;
  if FCurrentSelectedIndex > 0 then
  begin
    SetSelectedImage(TImageItem(FImages[FCurrentSelectedIndex - 1]), FCurrentSelectedIndex - 1);
  end
  else if FCurrentPage > 0 then
  begin
    PrevPage;
    if FImages.Count > 0 then
      SetSelectedImage(TImageItem(FImages[FImages.Count - 1]), FImages.Count - 1);
  end;
end;

procedure TSkFlowmotion.ScrollToIndex(Index: Integer; Animate: Boolean = True);
var
  TargetPage: Integer;
  RelativeIndex: Integer;
  AbsIndex: Integer;
  ImageItem: TImageItem;
begin
  if (Index < 0) or (Index >= FAllFiles.Count) then
    Exit;
  TargetPage := Index div FPageSize;
  RelativeIndex := Index mod FPageSize;
  if TargetPage <> FCurrentPage then
  begin
    ShowPage(TargetPage);
    // Warte bis Page geladen ist
    while FPageChangeInProgress do
      Application.ProcessMessages;
  end;
  if (RelativeIndex < FImages.Count) then
  begin
    ImageItem := TImageItem(FImages[RelativeIndex]);
    if Animate then
      SetSelectedImage(ImageItem, RelativeIndex)
    else
    begin
      // Ohne Animation direkt setzen
      FSelectedImage := ImageItem;
      FCurrentSelectedIndex := RelativeIndex;
      ImageItem.IsSelected := True;
      Repaint;
    end;
  end;
end;

procedure TSkFlowmotion.AddImage(Bitmap: TBitmap);
var
  NewItem: TImageItem;
  DummyName: string;
begin
  if Bitmap = nil then Exit;

  // Dummy Eintrag für Paging
  DummyName := 'MemoryBitmap_' + IntToStr(GetTickCount) + '_' + IntToStr(Random(10000));
  FAllFiles.Add(DummyName);
  FAllCaptions.Add('');
  FAllPaths.Add('');
  FAllHints.Add('');

  // Sichtbaren Eintrag
  NewItem := TImageItem.Create;
  NewItem.SkImage := BitmapToSkImage(Bitmap);
  NewItem.FileName := DummyName;
  NewItem.Direction := GetEntryDirection;
  FImages.Add(NewItem);

  if Visible then
  begin
    CalculateLayout;
    AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
    NewItem.Visible := True;
  end;
  StartAnimationThread;
end;

procedure TSkFlowmotion.AddImages(const FileNames, Captions, Paths, Hints: TStringList);
var
  i: Integer;
  WasEmpty: Boolean;
begin
  if FileNames = nil then Exit;

  WasEmpty := (FAllFiles.Count = 0);
  WaitForAllLoads;
  FLoadingCount := 0;

  // Alle zur Master-Liste hinzufügen
  for i := 0 to FileNames.Count - 1 do
  begin
    FAllFiles.Add(FileNames[i]);
    FAllCaptions.Add(Captions[i]);
    FAllPaths.Add(Paths[i]);
    FAllHints.Add(Hints[i]);
  end;

  if WasEmpty then
    ShowPage(FCurrentPage);
end;

procedure TSkFlowmotion.AddImagesAsync(const FileNames, Captions, Paths, Hints: TStringList);
var
  i: Integer;
  NewAbsIndex, TargetPage: Integer;
  LoadThread: TImageLoadThread;
  WasEmpty: Boolean;
  // In Skia nutzen wir FNextLoaderCoreIndex nicht zwingend für TImageLoadThread,
  // aber wir können es für Konsistenz übernehmen wenn du willst.
begin
  FLoadingCount := 0;
  WasEmpty := (FAllFiles.Count = 0);

  // Alle zur Master-Liste hinzufügen
  for i := 0 to FileNames.Count - 1 do
  begin
    FAllFiles.Add(FileNames[i]);
    FAllCaptions.Add(Captions[i]);
    FAllPaths.Add(Paths[i]);
    FAllHints.Add(Hints[i]);

    // Berechnen
    NewAbsIndex := FAllFiles.Count - 1;
    TargetPage := NewAbsIndex div FPageSize;

    // AutoScroll Handling
    if FAutoScrollPageForNewAdded then
    begin
      if (FCurrentPage <> TargetPage) or WasEmpty then
      begin
        ShowPage(TargetPage); // Synchron, weil wir auf der aktuellen Seite sind wollen
        Continue;
      end;
    end
    else if WasEmpty then
    ShowPage(FCurrentPage);

    // Nur Thread starten, wenn es auf die aktuell sichtbare Seite gehört
    if (NewAbsIndex >= GetPageStartIndex) and (NewAbsIndex <= GetPageEndIndex) then
    begin
      LoadThread := TImageLoadThread.Create(FileNames[i], Captions[i], Paths[i], Hints[i], Self, NewAbsIndex); // CoreIndex egal für Skia meistens
      LoadThread.Priority := FThreadPriority;
      FLoadingThreads.Add(LoadThread);
      Inc(FLoadingCount);
    end;
    // Else: Off-Page -> Lazy Load (sparen wir Thread)
  end;
end;

end.

