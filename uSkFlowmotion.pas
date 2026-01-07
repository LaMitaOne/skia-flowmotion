unit uSkFlowmotion;

interface

uses
  { System }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections,
  { FMX }
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.ImgList,
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

  TSmallPicPosition = (spTopLeft, spTopRight, spBottomLeft, spBottomRight);

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
    FSmallPicIndex: Integer;
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
    property SmallPicIndex: Integer read FSmallPicIndex write FSmallPicIndex; // Added SmallPic Index
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

  // ===================================================================
  // TFlowMasterItem - Virtual Item for global access via AllImageItems[i]
  // ===================================================================
  TFlowMasterItem = class
  private
    FOwner: TObject;
    FIndex: Integer;
    procedure SetCaption(const Value: string);
    function GetCaption: string;
    procedure SetHint(const Value: string);
    function GetHint: string;
    procedure SetSmallPicIndex(const Value: Integer);
    function GetSmallPicIndex: Integer;
  public
    constructor Create(AOwner: TObject);
    // Properties you want to access globally
    property Caption: string read GetCaption write SetCaption;
    property Hint: string read GetHint write SetHint;
    property SmallPicIndex: Integer read GetSmallPicIndex write SetSmallPicIndex;
  end;

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
    FSmallPicIndex: Integer; // Added SmallPic Index
    procedure SyncAddImage;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName, ACaption, APath, AHint: string; AOwner: TComponent; AIndex: Integer; ASmallPicIndex: Integer = -1);
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
    FAllSmallPicIndices: TList;  // Added Master List for SmallPic Indices
    FMasterItem: TFlowMasterItem; // Added Master Item Bridge

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
    FActivationZones: array of TActivationZone; // Added Activation Zones
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

    // SmallPic Support
    FSmallPicImageList: TImageList; // Added ImageList support
    FSmallPicPosition: TSmallPicPosition;
    FSmallPicVisible: Boolean;

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
    function GetMasterItem(Index: Integer): TFlowMasterItem;
    function GetSmallPicBitmap(const Index: Integer): TBitmap;

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
    procedure SetSmallPicVisible(const Value: Boolean);
    procedure SetPageSize(Value: Integer);

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
    procedure AddImage(const FileName: string; const AHint: string = ''; ASmallPicIndex: Integer = -1); overload;
    procedure AddImage(const FileName, ACaption, APath, AHint: string; ASmallPicIndex: Integer = -1); overload;
    procedure AddImage(Bitmap: TBitmap); overload;
    procedure AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = ''; const AHint: string = ''; ASmallPicIndex: Integer = -1);
    procedure AddImages(const FileNames, Captions, Paths, Hints: TStringList; const SmallPicIndices: TList = nil);
    procedure AddImagesAsync(const FileNames, Captions, Paths, Hints: TStringList; const SmallPicIndices: TList = nil);
    procedure InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath, XHint: string; ASmallPicIndex: Integer = -1);
    procedure InsertImageAsync(const FileName, Caption, Path, Hint: string; ASmallPicIndex: Integer = -1);
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
    property AllImageItems[Index: Integer]: TFlowMasterItem read GetMasterItem;
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
    property PageSize: Integer read FPageSize write SetPageSize;
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
    property SmallPicImageList: TImageList read FSmallPicImageList write FSmallPicImageList;
    property SmallPicPosition: TSmallPicPosition read FSmallPicPosition write FSmallPicPosition;
    property SmallPicVisible: Boolean read FSmallPicVisible write SetSmallPicVisible default True;
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

// -----------------------------------------------------------------------------
// COMPONENT REGISTRATION
// -----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('LaMita Components', [TSkFlowmotion]);
end;

// -----------------------------------------------------------------------------
// CROSS-PLATFORM HELPER: GET TICK COUNT
// -----------------------------------------------------------------------------
function GetTickCount: Cardinal;
begin
  Result := TThread.GetTickCount;
end;

// -----------------------------------------------------------------------------
// TFlowMasterItem: Bridge for global AllImageItems access
// -----------------------------------------------------------------------------
constructor TFlowMasterItem.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FIndex := -1;
end;

function TFlowMasterItem.GetCaption: string;
begin
  Result := (FOwner as TSkFlowmotion).FAllCaptions[FIndex];
end;

procedure TFlowMasterItem.SetCaption(const Value: string);
begin
  (FOwner as TSkFlowmotion).FAllCaptions[FIndex] := Value;
  // TODO: Update visible item if on current page
end;

function TFlowMasterItem.GetHint: string;
begin
  Result := (FOwner as TSkFlowmotion).FAllHints[FIndex];
end;

procedure TFlowMasterItem.SetHint(const Value: string);
begin
  (FOwner as TSkFlowmotion).FAllHints[FIndex] := Value;
  // TODO: Update visible item
end;

function TFlowMasterItem.GetSmallPicIndex: Integer;
begin
  Result := Integer((FOwner as TSkFlowmotion).FAllSmallPicIndices[FIndex]);
end;

procedure TFlowMasterItem.SetSmallPicIndex(const Value: Integer);
begin
  (FOwner as TSkFlowmotion).FAllSmallPicIndices[FIndex] := Pointer(Value);
  // TODO: Update visible item
end;

// -----------------------------------------------------------------------------
// TAnimationThread: Runs the animation loop
// -----------------------------------------------------------------------------
constructor TAnimationThread.Create(AOwner: TComponent);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FLastTick := GetTickCount;
  FStopRequested := False;

  if AOwner is TSkFlowmotion then
    Priority := (AOwner as TSkFlowmotion).FThreadPriority
  else
    Priority := tpNormal;
end;

destructor TAnimationThread.destroy;
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
    { Perform animation calculations }
    if FOwner is TSkFlowmotion then
      (FOwner as TSkFlowmotion).PerformAnimationUpdate(GetTickCount - LastTick);

    { Maintain Frame Rate }
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

// -----------------------------------------------------------------------------
// TImageItem: Data container for a single image
// -----------------------------------------------------------------------------
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

  { Initial Zoom State (Breathing) }
  FHotZoom := 1;
  FHotZoomTarget := 1;

  { Initial Drift Parameters }
  DriftRangeX := 10 + Random(20);
  DriftRangeY := 8 + Random(15);
end;

destructor TImageItem.destroy;
begin
  FSkImage := nil;
  inherited;
end;

// -----------------------------------------------------------------------------
// TImageLoadThread: Asynchronous image loader
// -----------------------------------------------------------------------------
constructor TImageLoadThread.Create(const AFileName, ACaption, APath, AHint: string; AOwner: TComponent; AIndex: Integer; ASmallPicIndex: Integer = -1);
begin
  inherited Create(True);
  FreeOnTerminate := True;

  { Store parameters }
  FFileName := AFileName;
  FCaption := ACaption;
  FPath := APath;
  FOwner := AOwner;
  FHint := AHint;
  FIndex := AIndex;
  FSmallPicIndex := ASmallPicIndex; // Store overlay index

  { Set priority }
  if AOwner is TSkFlowmotion then
    Priority := (AOwner as TSkFlowmotion).FThreadPriority
  else
    Priority := tpNormal;

  Self.Execute;
end;

procedure TImageLoadThread.Execute;
begin
  { Load image using Skia }
  if TFile.Exists(FFileName) then
  begin
    FSkImage := TSkImage.MakeFromEncodedFile(FFileName);
  end;

  { Synchronize back to main thread to add image }
  if (not Terminated) and Assigned(FSkImage) then
    Synchronize(SyncAddImage);

  { Notify owner of completion }
  if not Terminated then
  begin
    try
      if FOwner is TSkFlowmotion then
        (FOwner as TSkFlowmotion).ThreadFinished(Self);
    except
    end;
  end;
end;

procedure TImageLoadThread.SyncAddImage;
var
  NewItem: TImageItem;
  AbsIndex, PageStart, PageEnd: Integer;
  SmallIdxFromMaster: Integer;
begin
  try
    if Assigned(FOwner) and (FOwner is TSkFlowmotion) then
    begin
      with TSkFlowmotion(FOwner) do
      begin
        AbsIndex := FIndex;

        { Retrieve SmallPic Index from Master List }
        if AbsIndex < FAllSmallPicIndices.Count then
          SmallIdxFromMaster := Integer(FAllSmallPicIndices[AbsIndex])
        else
          SmallIdxFromMaster := FSmallPicIndex;

        { Check if page is changing or clearing }
        if FPageChangeInProgress or FClearing then
          Exit;

        { Check if image belongs to current page }
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
          NewItem.SmallPicIndex := SmallIdxFromMaster;
          NewItem.Visible := False;
          FImages.Add(NewItem);

          { Start animation if visible }
          if Visible then
          begin
            CalculateLayout;
            AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
            NewItem.Visible := True;
          end;
        end;
      end;
    end;
  finally
    if FOwner is TSkFlowmotion then
      TSkFlowmotion(FOwner).ThreadSafeInvalidate;
  end;
end;

// -----------------------------------------------------------------------------
// TSkFlowmotion: Main Control Implementation
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// LIFECYCLE: CONSTRUCTOR & DESTRUCTOR
// -----------------------------------------------------------------------------
constructor TSkFlowmotion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { --- Initialize Fonts and Colors --- }
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

  { --- Initialize Lists --- }
  FImages := TList.Create;
  FLoadingThreads := TList.Create;
  FAllFiles := TStringList.Create;
  FAllCaptions := TStringList.Create;
  FAllPaths := TStringList.Create;
  FAllHints := TStringList.Create;
  FAllSmallPicIndices := TList.Create; // Init Master List
  SetLength(FLoadedPositions, 0);

  FMasterItem := TFlowMasterItem.Create(Self); // Init Master Item

  { --- Defaults - State --- }
  FNextLoaderCoreIndex := 0;
  FClearing := False;
  FFreeFloatDrift := false;
  FBreathingPhase := 0.0;
  FAnimationThread := nil;
  FImageEntryStyle := iesRandom;
  FEntryPoint := TPoint.Create(-1000, -1000);

  { --- Defaults - Layout --- }
  FFlowLayout := flSorted;
  FKeepSpaceforZoomed := False;
  FAnimationSpeed := DEFAULT_ANIMATION_SPEED;
  FSpacing := 0;
  FKeepAspectRatio := True;

  { --- Defaults - Visuals --- }
  FBackgroundColor := TAlphaColors.Black;
  FHotTrackColor := TAlphaColors.Teal;
  FHotTrackZoom := true;
  FMaxColumns := 0;
  FMaxRows := 0;
  FGlowColor := TAlphaColors.Aqua;
  FGlowWidth := DEFAULT_GLOW_WIDTH;
  FHotTrackWidth := DEFAULT_HOTTRACK_WIDTH;

  { --- Defaults - Selection --- }
  FSelectedImage := nil;
  FWasSelectedItem := nil;
  FZoomAnimationType := zatSlide;
  FSelectedMovable := False;
  FDraggingSelected := False;
  FSorted := True;
  FAnimationEasing := True;
  FLoadingCount := 0;

  { --- Defaults - Paging --- }
  FPageSize := 1000;
  FCurrentPage := 0;
  FCurrentSelectedIndex := -1;

  { --- Defaults - System --- }
  FActive := True;
  FThreadPriority := tpNormal;
  TabStop := True;
  Width := 400;
  Height := 300;
  FBreathingEnabled := True;
  FZoomSelectedtoCenter := True;
  Self.HitTest := True;
end;

destructor TSkFlowmotion.destroy;
var
  i: Integer;
  StartTime: Cardinal;
begin
  try
    { --- Cleanup Animation Thread --- }
    if FAnimationThread <> nil then
    begin
      FAnimationThread.Stop;
      FAnimationThread.WaitFor;
      FreeAndNil(FAnimationThread);
    end;

    { --- Terminate Load Threads --- }
    for i := 0 to FLoadingThreads.Count - 1 do
    begin
      try
        TImageLoadThread(FLoadingThreads[i]).Terminate;
      except
      end;
    end;

    { --- Wait for cleanup with timeout --- }
    StartTime := GetTickCount;
    while (FLoadingThreads.Count > 0) and ((GetTickCount - StartTime) < 3000) do
    begin
      CheckSynchronize(10);
      Sleep(5);
    end;

    FLoadingThreads.Clear;
    FLoadingThreads.Free;

    { --- Free Image Items --- }
    for i := 0 to FImages.Count - 1 do
      TImageItem(FImages[i]).Free;
    FImages.Free;

    { --- Free Master Lists --- }
    FAllFiles.Free;
    FAllCaptions.Free;
    FAllPaths.Free;
    FAllHints.Free;
    FAllSmallPicIndices.Free;

    { --- Free Other Objects --- }
    FCaptionFont.Free;
    FMasterItem.Free;
  except
    { Ensure we don't leak even if destructor crashes }
  end;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------
// INTERNAL METHODS: THREADING & SYNCHRONIZATION
// -----------------------------------------------------------------------------
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
    FAnimationThread.WaitFor;
    FreeAndNil(FAnimationThread);
  end;
end;

procedure TSkFlowmotion.ThreadFinished(Thread: TImageLoadThread);
begin
  FLoadingThreads.Remove(Thread);
  Dec(FLoadingCount);
end;

// -----------------------------------------------------------------------------
// INTERNAL METHODS: PROPERTY SETTERS
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

procedure TSkFlowmotion.SetSelectedMovable(Value: Boolean);
begin
  FSelectedMovable := Value;
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
  if FZoomSelectedtoCenter = Value then Exit;
  FZoomSelectedtoCenter := Value;
  CalculateLayout;
  Repaint;
end;

procedure TSkFlowmotion.SetBreathingEnabled(Value: Boolean);
begin
  if FBreathingEnabled = Value then Exit;
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

procedure TSkFlowmotion.SetSmallPicVisible(const Value: Boolean);
begin
  if FSmallPicVisible <> Value then
  begin
    FSmallPicVisible := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetPageSize(Value: Integer);
begin
  if (Value > 0) and (Value <> FPageSize) then
  begin
    FPageSize := Value;
    ShowPage(0);
  end;
end;

// -----------------------------------------------------------------------------
// INTERNAL METHODS: UTILITIES
// -----------------------------------------------------------------------------
function TSkFlowmotion.GetImageItem(Index: Integer): TImageItem;
begin
  if (Index >= 0) and (Index < FImages.Count) then
    Result := TImageItem(FImages[Index])
  else
    Result := nil;
end;

function TSkFlowmotion.GetImageCount: Integer;
begin
  Result := FImages.Count;
end;

function TSkFlowmotion.GetLoadingCount: Integer;
begin
  Result := FLoadingCount;
end;

function TSkFlowmotion.GetMasterItem(Index: Integer): TFlowMasterItem;
begin
  FMasterItem.FIndex := Index;
  Result := FMasterItem;
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

function TSkFlowmotion.GetCaptionRect(Item: TImageItem; const DrawRect: TRect): TRect;
var
  CaptionH: Integer;
begin
  if not FShowCaptions or (Item.Caption = '') then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;

  CaptionH := 20; // Approximate height
  Result := Rect(DrawRect.Left, DrawRect.Bottom - CaptionH - FCaptionOffsetY, DrawRect.Right, DrawRect.Bottom - FCaptionOffsetY);
end;

// -----------------------------------------------------------------------------
// INTERNAL METHODS: IMAGE CONVERSION HELPERS
// -----------------------------------------------------------------------------
function TSkFlowmotion.SkImageToBitmap(Img: ISkImage): TBitmap;
var
  MS: TMemoryStream;
begin
  Result := nil;
  if not Assigned(Img) then Exit;
  MS := TMemoryStream.Create;
  try
    // TODO: Implement proper saving if needed
  except
  end;
  MS.Free;
end;

function TSkFlowmotion.BitmapToSkImage(Bmp: TBitmap): ISkImage;
var
  MS: TMemoryStream;
begin
  Result := nil;
  if not Assigned(Bmp) then Exit;
  MS := TMemoryStream.Create;
  try
    Bmp.SaveToStream(MS);
    MS.Position := 0;
    Result := TSkImage.MakeFromEncodedStream(MS);
  finally
    MS.Free;
  end;
end;

function TSkFlowmotion.GetOptimalSize(const OriginalWidth, OriginalHeight: Integer; const MaxWidth, MaxHeight: Double): TSize;
var
  ScaleX, ScaleY, Scale: Double;
begin
  if FKeepAspectRatio then
  begin
    if (OriginalWidth = 0) or (OriginalHeight = 0) then
    begin
      Result.cx := Round(MaxWidth);
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

// --- SmallPic Helper ---
function TSkFlowmotion.GetSmallPicBitmap(const Index: Integer): TBitmap;
var
  Sz: TSize;
begin
  Result := nil;

  { 1. Basic checks }
  if (FSmallPicImageList = nil) then Exit;
  if (Index < 0) or (Index >= FSmallPicImageList.Count) then Exit;

  { 2. Determine size.
     WORKAROUND: We cannot reliably read FSmallPicImageList.Width across all FMX versions easily.
     For now, we hardcode a standard 16x16 icon size. }
  Sz := TSize.Create(16, 16);

  { 3. Get Bitmap using complex FMX logic }
  try
    { FMX expects TSizeF usually for GetBitmap in newer versions }
    Result := FSmallPicImageList.Bitmap(TSizeF.Create(Sz.cx, Sz.cy), Index);
  except
    { Fallback if it fails }
    Result := nil;
  end;
end;


// -----------------------------------------------------------------------------
// PAGING METHODS
// -----------------------------------------------------------------------------
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

procedure TSkFlowmotion.ShowPage(Page: Integer);
var
  ItemIndex, i, StartIdx, EndIdx: Integer;
  ImageItem: TImageItem;
  FileName: string;
  AbsSmallIndex: Integer;
begin
  if (not visible) or (FAllFiles.Count = 0) then Exit;
  if FClearing or FPageChangeInProgress then Exit;

  FPageChangeInProgress := True;
  try
    { Cleanup previous page }
    FImages.Clear;
    FSelectedImage := nil;
    FCurrentSelectedIndex := -1;

    { Stop current loading threads }
    for i := 0 to FLoadingThreads.Count - 1 do
      TImageLoadThread(FLoadingThreads[i]).Terminate;
    FLoadingThreads.Clear;
    FLoadingCount := 0;

    { Calculate page range }
    StartIdx := Page * FPageSize;
    EndIdx := Min(StartIdx + FPageSize, FAllFiles.Count) - 1;
    FCurrentPage := Page;

    { Load images for this page }
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

        { Retrieve SmallPic Index from Master List }
        if i < FAllSmallPicIndices.Count then
          ImageItem.SmallPicIndex := Integer(FAllSmallPicIndices[i])
        else
          ImageItem.SmallPicIndex := -1;

        ImageItem.Visible := False;
        FImages.Add(ImageItem);
      end;
    end;

    { Recalculate layout for new items }
    CalculateLayout;

    { Start animations for loaded items }
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if Assigned(ImageItem.SkImage) then
      begin
        AnimateImage(ImageItem, ImageItem.Direction, false, Rect(0, 0, 0, 0));
        ImageItem.Visible := True;
      end;
    end;

    { Start render loop }
    StartAnimationThread;
    Repaint;
  finally
    FPageChangeInProgress := False;
  end;
end;

procedure TSkFlowmotion.SetFlowLayout(Value: TFlowLayout);
begin
  if not visible then Exit;
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

// -----------------------------------------------------------------------------
// ANIMATION LOGIC
// -----------------------------------------------------------------------------
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

procedure TSkFlowmotion.WaitForAllAnimations;
begin
  var TickCount := GetTickCount;
  while AnimationsRunning and ((GetTickCount - TickCount) < 5000) do
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
  FAllSmallPicIndices.Clear;
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
  if ImageItem = nil then Exit;

  { Determine target position }
  if UseSavedPosition then
    Target := SavedTargetRect
  else
    Target := ImageItem.TargetRect;

  W := Target.Right - Target.Left;
  H := Target.Bottom - Target.Top;
  EffectiveStyle := EntryStyle;

  if EffectiveStyle = iesRandom then
    EffectiveStyle := TImageEntryStyle(Random(8) + 1);

  { Calculate Start Position based on animation style }
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

  { Set Start Rect (scaled) }
  if (EffectiveStyle in [iesFromCenter, iesFromPoint]) then
    ImageItem.StartRect := Rect(StartX, StartY, StartX, StartX)
  else
    ImageItem.StartRect := Rect(StartX + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * (1 - START_SCALE) / 2), StartX + Round(W * START_SCALE) + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * START_SCALE) + Round(H * (1 - START_SCALE) / 2));

  { Set Animation State }
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
  if ImageItem = nil then Exit;
  ImageItem.ZoomProgress := 0;
  ImageItem.Animating := True;
  ImageItem.StartRect := ImageItem.CurrentRect;

  { Determine Start Rect based on animation type }
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

  { Calculate Target Rect for Selection }
  if ZoomIn then
  begin
    if Assigned(ImageItem.SkImage) then
    begin
      ImageSize := GetOptimalSize(ImageItem.SkImage.Width, ImageItem.SkImage.Height, Min(FMaxZoomSize, trunc(Width) div 2), Min(FMaxZoomSize, trunc(Height) div 2));
      CenterX := (trunc(Width) - ImageSize.cx) div 2;
      CenterY := (trunc(Height) - ImageSize.cy) div 2;
      ImageItem.TargetRect := Rect(CenterX, CenterY, CenterX + ImageSize.cx, CenterY + ImageSize.cy);
    end;
    { Recalculate layout so other images move out of the center way }
    CalculateLayout;
  end;
end;

// -----------------------------------------------------------------------------
// VISUAL PAINT METHOD (SKIA RENDERING)
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  i: Integer;
  ImageItem: TImageItem;
  DstRect: TRectF;
  BaseRect: TRectF;
  Paint: ISkPaint;
  CenterX, CenterY, BaseW, BaseH, NewW, NewH: Single;
  ZoomFactor: Double;
  ShadowFilter: ISkImageFilter;

  // --- Internal Helper: Draw SmallPic Overlay ---
  procedure DrawSmallPicOverlay(const Item: TImageItem; const BaseRect: TRectF);
  var
    IconBmp: TBitmap;
    IconSkImg: ISkImage;
    IconRect: TRectF;
    Margin: Integer;
  begin
    // Check visibility and validity
    if not FSmallPicVisible then Exit;
    if (Item.SmallPicIndex < 0) or (FSmallPicImageList = nil) then Exit;
    if (Item.SmallPicIndex >= FSmallPicImageList.Count) then Exit;

    // Get Icon from ImageList
    IconBmp := GetSmallPicBitmap(Item.SmallPicIndex);
    if not Assigned(IconBmp) then Exit;

    // Convert to Skia
    IconSkImg := BitmapToSkImage(IconBmp);
    if not Assigned(IconSkImg) then Exit;

    // Calculate Position
    Margin := 2;
    IconRect := TRectF.Create(0, 0, IconSkImg.Width, IconSkImg.Height);

    case FSmallPicPosition of
      spTopLeft:
        begin
          IconRect.Offset(BaseRect.Left + Margin, BaseRect.Top + Margin);
        end;
      spTopRight:
        begin
          IconRect.Offset(BaseRect.Right - Margin - IconRect.Width, BaseRect.Top + Margin);
        end;
      spBottomLeft:
        begin
          IconRect.Offset(BaseRect.Left + Margin, BaseRect.Bottom - Margin - IconRect.Height);
        end;
      spBottomRight:
        begin
          IconRect.Offset(BaseRect.Right - Margin - IconRect.Width, BaseRect.Bottom - Margin - IconRect.Height);
        end;
    end;

    // Draw
    ACanvas.DrawImageRect(IconSkImg, IconRect, TSkSamplingOptions.High);
  end;

begin
  { --- PASS 1: BACKGROUND --- }
  if Assigned(FBackgroundSkImage) then
    ACanvas.DrawImageRect(FBackgroundSkImage, ADest, TSkSamplingOptions.High)
  else
    ACanvas.Clear(TAlphaColors.Black);

  { Initialize Paint Object }
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;

  { Create Shadow Filter (Used for selected image) }
  ShadowFilter := TSkImageFilter.MakeDropShadow(8.0, 8.0, 10.0, 10.0, TAlphaColors.Black, nil);

  // =========================================================================
  // PASS 2: Draw all NORMAL (Non-Selected) Images
  // =========================================================================
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible then Continue;
    if not Assigned(ImageItem.SkImage) then Continue;
    if ImageItem.IsSelected then Continue; // Selected drawn last

    { Calculate Draw Rect }
    BaseRect := TRectF.Create(ImageItem.CurrentRect);
    ZoomFactor := ImageItem.FHotZoom;

    { Apply HotTrack Zoom }
    if (Abs(ZoomFactor - 1.0) > 0.01) then
    begin
      CenterX := (BaseRect.Left + BaseRect.Right) / 2;
      CenterY := (BaseRect.Top + BaseRect.Bottom) / 2;
      BaseW := BaseRect.Width;
      BaseH := BaseRect.Height;
      NewW := BaseW * ZoomFactor;
      NewH := BaseH * ZoomFactor;
      DstRect := TRectF.Create(CenterX - NewW / 2, CenterY - NewH / 2, CenterX + NewW / 2, CenterY + NewH / 2);
    end
    else
    begin
      DstRect := BaseRect;
    end;

    { Draw Image }
    Paint.ImageFilter := nil;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Alpha := 150; // Slight transparency for glass effect

    ACanvas.DrawImageRect(ImageItem.SkImage, DstRect, TSkSamplingOptions.High, Paint);

    { Draw Overlay (SmallPic) }
    DrawSmallPicOverlay(ImageItem, DstRect);

    { Draw HotTrack Border }
    if (ImageItem = FHotItem) then
    begin
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.Color := TAlphaColor(FHotTrackColor);
      Paint.ImageFilter := nil;
      Paint.Alpha := 255;
      Paint.StrokeWidth := FHotTrackWidth;
      Paint.MaskFilter := nil;
      ACanvas.DrawRect(DstRect, Paint);

      Paint.Style := TSkPaintStyle.Fill;
      Paint.MaskFilter := nil;
    end;
  end;

  // =========================================================================
  // PASS 3: Draw SELECTED IMAGE on Top
  // =========================================================================
  if Assigned(FSelectedImage) and FSelectedImage.Visible then
  begin
    ImageItem := FSelectedImage;
    BaseRect := TRectF.Create(ImageItem.CurrentRect);
    ZoomFactor := ImageItem.FHotZoom;

    { Apply Selection/Breathing Zoom }
    if (Abs(ZoomFactor - 1.0) > 0.01) then
    begin
      CenterX := (BaseRect.Left + BaseRect.Right) / 2;
      CenterY := (BaseRect.Top + BaseRect.Bottom) / 2;
      BaseW := BaseRect.Width;
      BaseH := BaseRect.Height;
      NewW := BaseW * ZoomFactor;
      NewH := BaseH * ZoomFactor;
      DstRect := TRectF.Create(CenterX - NewW / 2, CenterY - NewH / 2, CenterX + NewW / 2, CenterY + NewH / 2);
    end
    else
    begin
      DstRect := BaseRect;
    end;

    { Draw Image with Shadow }
    Paint.ImageFilter := ShadowFilter;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Alpha := 120; // More transparency for selected

    ACanvas.DrawImageRect(ImageItem.SkImage, DstRect, TSkSamplingOptions.High, Paint);

    { Draw Overlay (SmallPic) }
    DrawSmallPicOverlay(ImageItem, DstRect);

    { Draw Glow Border }
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.Color := TAlphaColor(FGlowColor);
    Paint.ImageFilter := nil;
    Paint.Alpha := 255;
    Paint.StrokeWidth := FGlowWidth;
    Paint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, FGlowWidth / 2);
    ACanvas.DrawRect(DstRect, Paint);

    Paint.Style := TSkPaintStyle.Fill;
    Paint.MaskFilter := nil;
  end;

  { Cleanup }
end;


// --- Finders ---
function TSkFlowmotion.FindImageByPath(const Path: string): TImageItem;
begin
  // Stub
  Result := nil;
end;

function TSkFlowmotion.FindImageByCaption(const Caption: string): TImageItem;
begin
  // Stub
  Result := nil;
end;

function TSkFlowmotion.GetImageIndex(ImageItem: TImageItem): Integer;
begin
  Result := FImages.IndexOf(ImageItem);
end;

// -----------------------------------------------------------------------------
// RESIZE HANDLER
// -----------------------------------------------------------------------------
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
// IMAGE MANAGEMENT
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.AddImage(const FileName: string; const AHint: string = ''; ASmallPicIndex: Integer = -1);
begin
  AddImage(FileName, ExtractFileName(FileName), FileName, AHint, ASmallPicIndex);
end;

procedure TSkFlowmotion.AddImage(const FileName, ACaption, APath, AHint: string; ASmallPicIndex: Integer = -1);
var
  SkImg: ISkImage;
  WasEmpty: Boolean;
  NewItem: TImageItem;
  AbsIndex: Integer;
begin
  WasEmpty := (FAllFiles.Count = 0);

  { Always add to Master Lists }
  if FileName <> '' then
  begin
    FAllFiles.Add(FileName);
    FAllCaptions.Add(ACaption);
    FAllPaths.Add(APath);
    FAllHints.Add(AHint);
    FAllSmallPicIndices.Add(Pointer(ASmallPicIndex));
  end;

  { If it's the first one, we load the page immediately }
  if WasEmpty then
  begin
    ShowPage(FCurrentPage);
  end
  else
  begin
    { If it's not empty, check if we should load synchronously or not }
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
        NewItem.SmallPicIndex := ASmallPicIndex;
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
  DoImageLoad(FileName, TFile.Exists(FileName));
end;

procedure TSkFlowmotion.AddImage(Bitmap: TBitmap);
var
  NewItem: TImageItem;
  DummyName: string;
begin
  if Bitmap = nil then Exit;
  DummyName := 'MemoryBitmap_' + IntToStr(GetTickCount) + '_' + IntToStr(Random(10000));
  FAllFiles.Add(DummyName);
  FAllCaptions.Add('');
  FAllPaths.Add('');
  FAllHints.Add('');
  FAllSmallPicIndices.Add(Pointer(-1));

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

procedure TSkFlowmotion.AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = ''; const AHint: string = ''; ASmallPicIndex: Integer = -1);
begin
  { Stub for async logic (currently sync for simplicity) }
  AddImage(FileName, ACaption, APath, AHint, ASmallPicIndex);
end;

procedure TSkFlowmotion.AddImages(const FileNames, Captions, Paths, Hints: TStringList; const SmallPicIndices: TList = nil);
var
  i: Integer;
  CurrentSmallIndex: Integer;
begin
  // Stub
  for i := 0 to FileNames.Count - 1 do
  begin
    if Assigned(SmallPicIndices) and (i < SmallPicIndices.Count) then
      CurrentSmallIndex := Integer(SmallPicIndices[i])
    else
      CurrentSmallIndex := -1;

    AddImage(FileNames[i], Captions[i], Paths[i], Hints[i], CurrentSmallIndex);
  end;
end;

procedure TSkFlowmotion.AddImagesAsync(const FileNames, Captions, Paths, Hints: TStringList; const SmallPicIndices: TList = nil);
begin
  // Stub
  AddImages(FileNames, Captions, Paths, Hints, SmallPicIndices);
end;

procedure TSkFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean = false);
begin
  try
    Clear(animated, ZoominSelected, Rect(0, 0, 0, 0), Rect(0, 0, 0, 0), iesFromBottom);
  except
  end;
end;

procedure TSkFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom; AndFree: Boolean = true);
begin
  // Stub for complex clear animation
  FreeAllImagesAndClearLists;
end;

procedure TSkFlowmotion.DoImageLoad(const FileName: string; Success: Boolean);
begin
  if Assigned(FOnImageLoad) then
    FOnImageLoad(Self, FileName, Success);
  if not Success and Assigned(FOnImageLoadFailed) then
    FOnImageLoadFailed(Self, FileName, 'Unknown Error'); // Simple fallback
end;

procedure TSkFlowmotion.WaitForAllLoads;
begin
  while FLoadingCount > 0 do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

// -----------------------------------------------------------------------------
// MOUSE & KEYBOARD EVENT HANDLERS
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ImageItem: TImageItem;
  ItemIndex: Integer;
  CaptionRect: TRect;
begin
  if (FImages.Count = 0) or FClearing or FInFallAnimation then Exit;

  { 1. Check Caption Click }
  if Button = TMouseButton.mbLeft then
  begin
    for ItemIndex := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[ItemIndex]);
      CaptionRect := GetCaptionRect(ImageItem, ImageItem.CurrentRect);
      if PtInRect(CaptionRect, Point(Round(X), Round(Y))) then
      begin
        if Assigned(FOnCaptionClick) then
          FOnCaptionClick(Self, ImageItem, ItemIndex);
        Exit;
      end;
    end;
  end;

  { 2. Find Image at Point }
  ImageItem := GetImageAtPoint(X, Y);
  ItemIndex := FImages.IndexOf(ImageItem);

  { 3. Click on Background -> Deselect }
  if (Button = TMouseButton.mbLeft) and (ImageItem = nil) then
  begin
    if FSelectedImage <> nil then
      SetSelectedImage(nil, -1);
    Exit;
  end;

  { 4. Click on Image }
  if (Button = TMouseButton.mbLeft) and (ImageItem <> nil) then
  begin
    if Assigned(FOnSelectedItemMouseDown) then
      FOnSelectedItemMouseDown(Self, ImageItem, ItemIndex, Round(X), Round(Y), Button, Shift);

    { Selection Logic }
    if FSelectedImage <> ImageItem then
      SetSelectedImage(ImageItem, ItemIndex)
    else
    begin
      { Tactile "Breathing" push }
      if (not FDraggingSelected) and (FFlowLayout <> flFreeFloat) then
        FBreathingPhase := FBreathingPhase - 0.4;
    end;

    { Start Dragging if enabled }
    if FSelectedMovable and (ImageItem = FSelectedImage) then
    begin
      FDraggingSelected := True;
      FDragOffset.X := Round(X) - ((ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2);
      FDragOffset.Y := Round(Y) - ((ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2);
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSkFlowmotion.MouseMove(Shift: TShiftState; X, Y: Single);
var
  ImageItem: TImageItem;
  CurCX, CurCY, TargetCX, TargetCY, MoveX, MoveY: Integer;
  R: TRect;

  // Activation Zone Vars
  NearestZoneDistance: Double;
  ZoneCenterX, ZoneCenterY: Integer;
  i: Integer;
  ZoneRect: TRect;
begin
  if FClearing or FInFallAnimation then Exit;

  // --- DRAGGING SELECTED IMAGE (Priority) ---
  if FDraggingSelected and (FSelectedImage <> nil) then
  begin
    ImageItem := FSelectedImage;
    R := ImageItem.CurrentRect;
    TargetCX := Round(X) - FDragOffset.X;
    TargetCY := Round(Y) - FDragOffset.Y;
    MoveX := TargetCX - ((R.Left + R.Right) div 2);
    MoveY := TargetCY - ((R.Top + R.Bottom) div 2);
    OffsetRect(R, MoveX, MoveY);
    ImageItem.TargetRect := R;
    ImageItem.CurrentRect := R;

    // Check Zones
    if Length(FActivationZones) > 0 then
    begin
      NearestZoneDistance := MaxDouble;
      for i := Low(FActivationZones) to High(FActivationZones) do
      begin
        ZoneRect := FActivationZones[i].Rect;
        ZoneCenterX := (ZoneRect.Left + ZoneRect.Right) div 2;
        ZoneCenterY := (ZoneRect.Top + ZoneRect.Bottom) div 2;
        if (Abs((R.Left + R.Right) div 2 - ZoneCenterX) < 100) and (Abs((R.Top + R.Bottom) div 2 - ZoneCenterY) < 100) then
        begin
           if Assigned(FOnSelectedImageEnterZone) then
             FOnSelectedImageEnterZone(Self, ImageItem, FActivationZones[i].Name);
        end;
      end;
    end;

    ThreadSafeInvalidate;
    Exit;
  end;

  // --- NORMAL HOT-TRACKING ---
  ImageItem := GetImageAtPoint(X, Y);
  if ImageItem <> FHotItem then
  begin
    // Mouse Enter
    if (ImageItem <> nil) and Assigned(FOnImageMouseEnter) then
      FOnImageMouseEnter(Self, ImageItem, FImages.IndexOf(ImageItem));
    // Mouse Leave
    if (FHotItem <> nil) and (ImageItem <> FHotItem) and Assigned(FOnImageMouseLeave) then
      FOnImageMouseLeave(Self, FHotItem, FImages.IndexOf(FHotItem));

    FHotItem := ImageItem;

    if ShowHint and (FHotItem <> nil) and (FHotItem.Hint <> '') then
      Hint := FHotItem.Hint
    else
      Hint := '';

    if FHotItem <> nil then
    begin
      FHotItem.FHotZoomTarget := HOT_ZOOM_MAX_FACTOR;
      StartAnimationThread;
    end;
  end;

  Cursor := crDefault;
  if FHotItem <> nil then
    Cursor := crHandPoint;
end;

procedure TSkFlowmotion.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ImageUnderCursor: TImageItem;
begin
  if FClearing or FInFallAnimation then Exit;

  if (Button = TMouseButton.mbLeft) then
  begin
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

procedure TSkFlowmotion.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if AnimationsRunning then Exit;
  case Key of
    vkLeft, vkUp: SelectPreviousImage;
    vkRight, vkDown: SelectNextImage;
    vkReturn: if (FSelectedImage <> nil) and Assigned(FOnSelectedImageDblClick) then FOnSelectedImageDblClick(Self, FSelectedImage, FCurrentSelectedIndex);
    vkEscape: SetSelectedImage(nil, -1);
    vkHome: if FImages.Count > 0 then SetSelectedImage(TImageItem(FImages[0]), 0);
    vkEnd: if FImages.Count > 0 then SetSelectedImage(TImageItem(FImages[FImages.Count - 1]), FImages.Count - 1);
  end;
  inherited KeyDown(Key, KeyChar, Shift);
end;

// -----------------------------------------------------------------------------
// NAVIGATION HELPERS
// -----------------------------------------------------------------------------
function TSkFlowmotion.GetImageAtPoint(X, Y: Single): TImageItem;
var
  i: Integer;
  ImageItem: TImageItem;
  P: TPoint;
  CenterX, CenterY, BaseW, BaseH, NewW, NewH: Integer;
  DrawRect: TRect;
  ZoomFactor: Double;
  CurrentZoom: Double;
  CurrentRect: TRect;
  BestCandidate: TImageItem;
  BestCandidateZoom: Double;
begin
  P := Point(Round(X), Round(Y));
  Result := nil;

  { 1. SELECTED IMAGE CHECK (Highest Priority) }
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

    InflateRect(DrawRect, Max(FGlowWidth, FHotTrackWidth), Max(FGlowWidth, FHotTrackWidth));

    if PtInRect(DrawRect, P) then
    begin
      Result := FSelectedImage;
      Exit;
    end;
  end;

  { 2. ALL OTHER IMAGES (Z-Order Logic) }
  BestCandidate := nil;
  BestCandidateZoom := -1.0;

  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible or (ImageItem = FSelectedImage) then Continue;

    { Calculate Zoom Factor }
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
      DrawRect := ImageItem.CurrentRect;
    end;

    { Calculate Hit Rect with Border }
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
    end;

    { Check Hit and Z-Order }
    if PtInRect(DrawRect, P) then
    begin
      CurrentZoom := ImageItem.FHotZoom;
      if CurrentZoom < 1.0 then CurrentZoom := 1.0;

      { Pick the one with highest zoom (on top) }
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
  if FInFallAnimation then Exit;
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
  if FInFallAnimation then Exit;
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

// -----------------------------------------------------------------------------
// SELECTED IMAGE LOGIC
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.SetSelectedImage(ImageItem: TImageItem; Index: Integer);
var
  OldSelected: TImageItem;
begin
  if ImageItem = nil then FHotItem := nil;
  { Prevent flickering: Don't interrupt mid-zoom }
  if (ImageItem = nil) and (FSelectedImage <> nil) and (FSelectedImage.ZoomProgress > 0.1) and (FSelectedImage.ZoomProgress < 1) then Exit;
  if FSelectedImage = ImageItem then Exit;

  OldSelected := FSelectedImage;

  { --- 1. Handle OLD selection (Zoom Out animation) --- }
  if OldSelected <> nil then
  begin
    OldSelected.IsSelected := False;
    { Reset zoom for smooth exit }
    if OldSelected.FHotZoom >= 1 then OldSelected.FHotZoom := 1.1;
  end;

  FWasSelectedItem := FSelectedImage;
  if FWasSelectedItem <> nil then
  begin
    FWasSelectedItem.FAnimating := True;
    if FWasSelectedItem = FHotItem then FWasSelectedItem.FHotZoomTarget := 1.0;
    { Start Zoom Out animation }
    StartZoomAnimation(FWasSelectedItem, False);
  end;

  { --- 2. Handle NEW selection --- }
  FSelectedImage := ImageItem;
  FCurrentSelectedIndex := Index;

  if ImageItem = nil then
  begin
    FCurrentSelectedIndex := -1;
    FHotItem := nil;
  end
  else
  begin
    ImageItem.IsSelected := True;
    ImageItem.ZoomProgress := 0;
    if ImageItem.FHotZoom < 1 then ImageItem.FHotZoom := 1.0;

    { Sync Breathing state }
    if FBreathingEnabled then FHotItem := ImageItem else ImageItem.FHotZoomTarget := 1.0;

    FCurrentSelectedIndex := Index;
    ImageItem.AnimationProgress := 0;
    ImageItem.Animating := True;
  end;

  { --- 3. Recalculate Layout ---
  { This determines where the OLD image goes (Grid) and moves others for the NEW image }
  if (FFlowLayout <> flFreeFloat) then CalculateLayout;

  { --- 4. Start Zoom In animation for NEW image --- }
  if (FZoomSelectedtoCenter and (FFlowLayout <> flFreeFloat)) then
  begin
    if ImageItem <> nil then StartZoomAnimation(ImageItem, True);
  end;

  StartAnimationThread;
  if Assigned(FOnItemSelected) then FOnItemSelected(Self, ImageItem, Index);
end;

// -----------------------------------------------------------------------------
// INSERTION & PERSISTENCE (Stubs)
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.AddImagesWithPositions(const FileNames, Captions, Paths: TStringList; const Positions: array of TRect);
begin
  // Stub
end;

procedure TSkFlowmotion.InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath, XHint: string; ASmallPicIndex: Integer = -1);
begin
  // Stub
  AddImage(Pic);
end;

procedure TSkFlowmotion.InsertImageAsync(const FileName, Caption, Path, Hint: string; ASmallPicIndex: Integer = -1);
begin
  // Stub
  AddImageAsync(FileName, Caption, Path, Hint, ASmallPicIndex);
end;

procedure TSkFlowmotion.SetImage(Index: Integer; Bitmap: TBitmap);
begin
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    TImageItem(FImages[Index]).SkImage := BitmapToSkImage(Bitmap);
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.RemoveImage(Index: Integer; Animated: Boolean = True);
begin
  RemoveImage(Index, Animated, Rect(0, 0, 0, 0), iesFromBottom);
end;

procedure TSkFlowmotion.RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle);
begin
  // Stub
end;

function TSkFlowmotion.GetPicture(index: Integer): TBitmap;
begin
  Result := nil;
end;

procedure TSkFlowmotion.SetBackgroundpicture(const Path: string);
begin
  try
    if TFile.Exists(Path) then
      FBackgroundSkImage := TSkImage.MakeFromEncodedFile(Path)
    else
      FBackgroundSkImage := nil;
  except
    FBackgroundSkImage := nil;
  end;
  Repaint;
end;

procedure TSkFlowmotion.MoveImageToPos(RelIndexFrom, RelIndexTo: Integer);
begin
  // Stub
end;

procedure TSkFlowmotion.SavePositionsToFile(const FileName: string);
begin
  // Stub
end;

procedure TSkFlowmotion.LoadPositionsFromFile(const FileName: string);
begin
  // Stub
end;

procedure TSkFlowmotion.SavePositionsToStream(Stream: TStream);
begin
  // Stub
end;

procedure TSkFlowmotion.LoadPositionsFromStream(Stream: TStream);
begin
  // Stub
end;

function TSkFlowmotion.GetCurrentPositions: TImagePositions;
begin
  // Stub
  SetLength(Result, 0);
end;

procedure TSkFlowmotion.ResetPositions;
begin
  CalculateLayout;
  Repaint;
end;

procedure TSkFlowmotion.ScrollToIndex(Index: Integer; Animate: Boolean = True);
begin
  // Stub
end;

// -----------------------------------------------------------------------------
// MISSING METHODS IMPLEMENTATION
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.PerformAnimationUpdate(DeltaMS: Cardinal);
var
  i: Integer;
  DeltaTime: Double;
  ImageItem: TImageItem;
  Progress, Eased, Speed: Double;
  AnyAnimating, NeedRepaint: Boolean;
  TempRect: TRect;
  TempZoom, TargetZoom: Double;
begin
  if FInFallAnimation or FClearing or (not Visible) then Exit;

  DeltaTime := DeltaMS / 1000.0;
  if DeltaTime <= 0 then DeltaTime := 0.016;

  AnyAnimating := False;
  NeedRepaint := False;

  { Update position and scale progress }
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);

    TempZoom := 255;

    { Main position/scale animation progress }
    if ImageItem.AnimationProgress < 1.0 then
    begin
      TempZoom := Min(1.0, ImageItem.AnimationProgress + FAnimationSpeed / 100);
      if Abs(ImageItem.AnimationProgress - TempZoom) > 0.001 then
      begin
        ImageItem.AnimationProgress := TempZoom;
        NeedRepaint := True;
      end;
    end;

    { Selection zoom }
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

    { Combine progress for position interpolation }
    Progress := Max(ImageItem.AnimationProgress, ImageItem.ZoomProgress);
    if FAnimationEasing then Progress := EaseInOutQuad(Progress);

    TempRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Progress), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Progress), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Progress), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress));

    if not EqualRect(ImageItem.CurrentRect, TempRect) then
    begin
      ImageItem.CurrentRect := TempRect;
      NeedRepaint := True;
    end;

    ImageItem.Animating := not ((ImageItem.AnimationProgress >= 1.0) and ((ImageItem.ZoomProgress <= 0.0001) or (ImageItem.ZoomProgress >= 0.9999)) and EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) and (Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) <= 0.006));

    if ImageItem = FWasSelectedItem then
      if (ImageItem.ZoomProgress <= 0.0001) and EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) then
        FWasSelectedItem := nil;
  end;

  { Update HotTrack Zoom + Breathing }
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

    if not ImageItem.Visible then Continue;

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

  { Advance Breathing Phase }
  if not FDraggingSelected then
    if FBreathingEnabled and (FHotItem <> nil) and (FHotItem = FSelectedImage) then
      FBreathingPhase := Frac(FBreathingPhase + BREATHING_SPEED_PER_SEC * DeltaTime);

  { Check if any animations are still running }
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

// -----------------------------------------------------------------------------
// LAYOUT ALGORITHM (SORTED GRID)
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.CalculateLayout;
begin
  case FFlowLayout of
    flSorted:
      CalculateLayoutSorted;
    flFreeFloat:
      CalculateLayoutFreeFloat;
  end;
end;

procedure TSkFlowmotion.CalculateLayoutFreeFloat;
begin
  // For FreeFloat, we essentially reuse sorted layout logic currently
  // to determine positions, but without strict grid ordering logic inside PlaceImage?
  // Actually, usually FreeFloat implies random positions.
  // But here you likely want sorted layout just without the center space logic.
  CalculateLayoutSorted;
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
  if FImages.Count = 0 then Exit;
  if FInFallAnimation then Exit;

  { --- Phase 1: Collect Visible Images (excluding selected if centering) --- }
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

    if VisibleImages.Count = 0 then Exit;

    { --- Phase 2: Sort by size (Largest first) --- }
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

    { --- Phase 3: Estimate Grid Size --- }
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

    { --- Phase 4: Find Optimal Grid Configuration --- }
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

    { --- Phase 5: Place Images in Grid --- }
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

      { Determine Span based on aspect ratio }
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
      { Try to find a free spot }
      for r := 0 to Rows - SpanRows do
      begin
        for c := 0 to Cols - SpanCols do
        begin
          if IsAreaFree(Grid, r, c, SpanRows, SpanCols) then
          begin
            if PlaceImage(ImageItem, Grid, r, c, SpanRows, SpanCols, BaseCellWidth, BaseCellHeight) then
              Placed := True;
            Break;
          end;
        end;
        if Placed then Break;
      end;

      { Fallback: Force to 1x1 if needed }
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
          if Placed then Break;
        end;
      end;
    end;
  finally
    VisibleImages.Free;
  end;
end;

function TSkFlowmotion.IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
var
  r, c: Integer;
  CenterRow, CenterCol, ProtectedSize: Integer;
begin
  { Bounds check }
  if (Row < 0) or (Col < 0) or (Row + SpanRows > Length(Grid)) or (Col + SpanCols > Length(Grid[0])) then
  begin
    Result := False;
    Exit;
  end;

  { Normal grid occupation check }
  for r := Row to Row + SpanRows - 1 do
    for c := Col to Col + SpanCols - 1 do
      if Grid[r, c] then
      begin
        Result := False;
        Exit;
      end;

  { Keep center area free for zoomed image }
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

  { Calculate cell position and size }
  X := FSpacing + Col * (BaseCellWidth + FSpacing);
  Y := FSpacing + Row * (BaseCellHeight + FSpacing);
  CellWidth := SpanCols * BaseCellWidth + (SpanCols - 1) * FSpacing;
  CellHeight := SpanRows * BaseCellHeight + (SpanRows - 1) * FSpacing;

  { Optimal image size preserving aspect ratio }
  ImageSize := GetOptimalSize(ImageItem.SkImage.Width, ImageItem.SkImage.Height, CellWidth, CellHeight);

  { Center image in cell }
  X := X + (CellWidth - ImageSize.cx) div 2;
  Y := Y + (CellHeight - ImageSize.cy) div 2;

  { Bounds clamping }
  if X < 0 then X := 0;
  if Y < 0 then Y := 0;
  if X + ImageSize.cx > Width then X := trunc(Width - ImageSize.cx);
  if Y + ImageSize.cy > Height then Y := trunc(Height - ImageSize.cy);

  { Set Target Rect }
  ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);

  { === PIXEL-EXACT CHECK FOR KeepAreaFreeRect === }
  if not IsRectEmpty(FKeepAreaFreeRect) then
  begin
    if IntersectRect(DummyRect, ImageItem.TargetRect, FKeepAreaFreeRect) then
    begin
      Result := False; // Overlap? reject
      Exit;
    end;
  end;

  { Reset Animation State }
  ImageItem.StartRect := ImageItem.CurrentRect;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;
  MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
  Result := True;
end;

end.
