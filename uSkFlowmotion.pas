{                                                                              }
{ Skia-Flowmotion v0.47 alpha                                                  }
{ based on vcl flowmotion https://github.com/LaMitaOne/Flowmotion              }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{ larate@gmx.net                                                               }
{ https://lamita.jimdosite.com                                                 }
{                                                                              }
{------------------------------------------------------------------------------}

{
 ----Latest Changes
   v 0.47
    - Fixed Infoindicator showing again before panel outside
    - Fixed sometimes selected getting directly back to gridposition and not animate to it when clickng like mad around and select others :D
    - Fixed sometimes item from fullscreen animating back in line getting a moment invisible
    - Added property HoverAliveOnFullscreen en/disables HoverAlive for selected only at fullscreen
    - Implemented basic videoplayer inside of selected image (not working, but starting already, threadhopping works :D)
    - Implemented base for smartnavigation (GetSpatialCandidate -> TSmartImageAction = (siaSelectNorth, siaSelectSouth, siaSelectWest, siaSelectEast) not finished
    - Added property AlwaysShowInfo
    - Added property DeleteClicked -> if active clicked pic gets destroyed
   v 0.46
    - Added new TBackgroundEffect -> beHolographic, beRealMatrix
    - beRealMatrix using live data of items (smallpics too but instable atm, commented out)
    - New propertys -> MatrixFont, MatrixColor, MatrixHeadColor, MatrixSpeed, MatrixFontSize
    - Fixed: Hoveralive not stops anymore at fullscreen
    - some fine tuning, small bugfixes, code cleaning, remove var corpses...
    - Improved InfoIndicator: stops breathing on mouseover
    - Added propertys -> InfoIndicatorHotColor, ShowInfoOnMiddleClick
    - Added OnFullscreenEnter event.
      Fires when the selected image animation finishes (is fully zoomed).
      Requires synchronization with Main Thread for UI safety.
    - Added Swipe Gesture support for InfoPanel when SelectedMovable false or fullscreen
      Mouse Swipe (Left/Right/Up/Down) opens/closes the info panel depending on TInfoPanelDirection.
   v 0.45
    - Added HoverAlive feature (Micro-Hovering).
      Images now gently float around their center position with customizable range and speed.
    - Aded propertys -> HoverAliveRange, HoverAlive, HoverAliveSpeed
    - Added new TInfoPanelDirection: ipdAuto, ipdTop, ipdBottom, ipdLeft, ipdRight
    - Fixed: InfoPanelWidthPercent now working
    - Added new InfoIndicator -> propertys FInfoIndicatorColor, FShowInfoIndicator
      shows arrows when info text in imageitem and on click show infos
    - Added new TFullscreenAngle: fsa0, fsa90, fsa180, fsa270
    - Improved shadow rendering: breathing selected now raises shadow more than hotzoomed
    - Improved RotateDot: stops breathing on mouseover + larger clickable area
    - Improved smallpic & rotatedot positioning with roundedges + smallpicmargin
    - Added property RotateHandleSize
    - New: Set OwnerForm Quality to HighPerformance by default
    - Improved wall sliding physics – now calculates with rotation (commented out for now, more TODO)
    - Split DrawFluidInfo into separate functions for easier extension: DrawFluidInfo_BlurEdge, DrawFluidInfo_Static
    - Fixed: Info text now supports new line at '|'
    - Added new TInfoAnimationStyle: iasTransparent (transparency tied to zoom factor)
    - Added new properties: HotZoomMaxFactor, EnableParticlesOnMouseClick
   v 0.44
    - Implemented Imageitem TargetAlpha for smooth fade alpha.
    - Animated Clear method now runs in our physics thread + alpha fade-out.
    - Fixed shadow perspective alignment for small rotated images too now.
    - Fixed Z-order layering issues during un-zooming(from fullzoomed) and hot-tracking.
    - Fixed live UI updates for SetCaption, SetHint, and SetSmallPicIndex.
    - Added BreathRotationEnabled for subtle breathing rotation effects.
    - Added Imageitem - FInfoText
    - Added new ShowInfoPanel -> overlays more infos txt, animated slidein and look
    - Changed - MidMousebtn now shows/Hides infopanel and on rotatebtn reset angle
    - Added TInfoAnimationStyle = (iasBlurEdge, iasStatic)
   v 0.43
    - skFLM now automatically resizes large images
    - Added MaxInternalPicSize property (default 720px)
    - Implemented real-time collision avoidance
      Images now dynamically move out of the way when the selected is dragged across the screen
      This creates a natural, magnetic interaction effect when KeepSpaceforZoomed is combined with
      SelectedMovable
    - We disable breathing/hotzoom if we are currently dragging the image
      This prevents the "Jitter/Flicker" effect caused by size changes while moving
    - Implemented dynamic shadow scaling based on zoom/breath state
    - Wall Sliding: Hotzoom and breathing effects now respect screen edges.
      Images smoothly slide against borders.
    - lots fine tuning and bugfixes
   }

unit uSkFlowmotion;

interface

uses
  { System }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.IOUtils, System.Generics.Defaults, System.Generics.Collections,
  { FMX }
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.ImgList, FMX.Media,
  { Skia }
  System.Skia, FMX.Skia;

const
  TARGET_FPS = 30;
  MIN_FRAME_TIME = 1000 div TARGET_FPS;
  DEFAULT_ANIMATION_SPEED = 5;
  DEFAULT_ALPHA = 255;
  START_ALPHA = 50;
  START_SCALE = 0.05;
  MIN_CELL_SIZE = 22;
  DEFAULT_GLOW_WIDTH = 2;
  DEFAULT_HOTTRACK_WIDTH = 1;
  DEFAULT_MAX_ZOOM_SIZE = 300;
  HOT_ZOOM_MIN_FACTOR = 1.01;
  HOT_ZOOM_IN_SPEED = 0.07;
  HOT_ZOOM_OUT_SPEED = 0.09;
  HOT_ZOOM_IN_PER_SEC = 2.5;
  HOT_ZOOM_OUT_PER_SEC = 3.0;
  HOT_ZOOM_EPSILON = 0.0001;
  BREATHING_AMPLITUDE = 1.1;
  BREATHING_SPEED_PER_SEC = 0.06;
  ROTATION_SMOOTHING_SPEED = 0.1;
  ROTATION_EPSILON = 0.01;
  MATRIX_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$#+%&@?!';

type
  TFlowLayout = (flSorted, flFreeFloat);

  TFullscreenAngle = (fsa0, fsa90, fsa180, fsa270);

  TInfoPanelDirection = (ipdAuto, ipdTop, ipdBottom, ipdLeft, ipdRight);

  TSurfaceEffect = (sueNone, sueShadow, sueGlow, sueAmbient);

  TBackgroundEffect = (beHolographic, beRealMatrix);

  TPictureBorderType = (btTech, btFull);

  TImageEntryStyle = (iesRandom, iesFromTop, iesFromBottom, iesFromLeft, iesFromRight, iesFromTopLeft, iesFromTopRight, iesFromBottomLeft, iesFromBottomRight, iesFromCenter, iesFromPoint, iesExplode);

  TBooleanGrid = array of array of Boolean;

  TInfoAnimationStyle = (iasBlurEdge, iasStatic, iasTransparent);

  TZoomAnimationType = (zatSlide, zatFade, zatZoom, zatBounce);

  TSmartImageAction = (siaSelectNorth, siaSelectSouth, siaSelectWest, siaSelectEast);

  TMatrixColumn = record
    X: Single;        // X Position on screen
    Y: Single;        // Y Position (Top of the stream)
    Speed: Single;    // Falling speed
    Length: Integer;   // How long the stream is (number of chars)
    Chars: string;     // The random string of characters
    TargetImageIndex: Integer;
  end;

  TParticle = record
    X, Y: Single;
    VX, VY: Single;
    Life: Single; // 1.0 down to 0
    Color: TAlphaColor;
    Size: Single;
  end;

  TSmallPicParticle = record
    Index: Integer;   // Index in FSmallPicImageList
    X, Y: Single;   // Position
    VX, VY: Single;   // Velocity
    Life: Single;    // 1.0 down to 0.0
    Angle: Single;    // Rotation
    VAngle: Single;   // Rotation Velocity
  end;

  TSmallPicPosition = (spTopLeft, spTopRight, spBottomLeft, spBottomRight);

  TImagePosition = record
    FileName: string;
    Caption: string;
    Path: string;
    Hint: string;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Rotation: Single;
  end;

  TImagePositions = array of TImagePosition;

  TImageItem = class
  private
    FSkImage: ISkImage;
    FImageIndex: Integer;
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
    FActualRotation: Single;
    FTargetRotation: Single;
    FGlitchIntensity: Single;
    FDominantColor: TAlphaColor;
    FInfoText: string;
    FIsInfoShowing: Boolean;
    FInfoProgress: Double;
    FHoverX: Single;      // Current X Offset from center
    FHoverY: Single;      // Current Y Offset from center
    FHoverVX: Single;     // Velocity X (Direction)
    FHoverVY: Single;     // Velocity Y (Direction)
  public
    constructor Create;
    destructor Destroy; override;
    property SkImage: ISkImage read FSkImage write FSkImage;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
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
    property SmallPicIndex: Integer read FSmallPicIndex write FSmallPicIndex;
    property ActualRotation: Single read FActualRotation write FActualRotation;
    property GlitchIntensity: Single read FGlitchIntensity write FGlitchIntensity;
    property DominantColor: TAlphaColor read FDominantColor write FDominantColor;
    property InfoText: string read FInfoText write FInfoText;
  end;

  TOnSelectedItemMouseDown = procedure(Sender: TObject; ImageItem: TImageItem; Index, X, Y: Integer; Button: TMouseButton; Shift: TShiftState) of object;

  TOnAllAnimationsFinished = procedure(Sender: TObject) of object;

  TOnSelectedImageDblClick = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TImageSelectEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TOnCaptionClick = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TSmallPicClickEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TImageLoadFailedEvent = procedure(Sender: TObject; const FileName: string; const ErrorMsg: string) of object;

  TImageHoverEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TImageFullscreenEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TActivationZone = record
    Name: string;
    Rect: TRectF;
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
    FSmallPicIndex: Integer;
    FInfoText: string;
    procedure SyncAddImage;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName, ACaption, APath, AHint, AInfoText: string; AOwner: TComponent; AIndex: Integer; ASmallPicIndex: Integer = -1);
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
    FAllSmallPicIndices: TList;
    FAllInfoTexts: TStringList;
    FMasterItem: TFlowMasterItem;
    FMaxInternalPicSize: Integer;
    // Threading
    FNextLoaderCoreIndex: Integer;
    FAnimationThread: TAnimationThread;
    FLoadingThreads: TList;
    FLoadingCount: Integer;
    // Animation State
    FAnimationSpeed: Integer;
    FAnimationEasing: Boolean;
    FInFallAnimation: Boolean;
    FFallingOut: Boolean;
    FPageOutProgress: Double;
    FImageEntryStyle: TImageEntryStyle;
    FEntryPoint: TPoint;
    FInfoPanelStyle: TInfoAnimationStyle;
    FFullscreenAngle: TFullscreenAngle;
    FHotZoomMaxFactor: Single;
    FInfoPanelDirection: TInfoPanelDirection;
    FHoverAliveOnFullscreen: Boolean;
    // Clearing Animation State
    FIsClearing: Boolean;            // True if we are currently animating a Clear
    FClearingStyle: TImageEntryStyle; // Stores the FallingStyle (Direction)
    FClearingTargetPos: TPoint;     // Stores the FallingTargetPoint
    FClearZoomSelected: Boolean;     // Stores if we zoom the selected item
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
    FIsZoomedToFill: Boolean;
    FPreviousRotation: Single;
    FPrevRect: TRect;
    //videoplayer
    FVideoPlayer: TMediaPlayerControl;
    FTestVideoFile: string;
    FPendingVideoFile: string;
    FCaptureVideo: Boolean;
    FLastVideoRect: TRectF;  // Tracks last video position to avoid unnecessary updates
    FLastVideoAngle: Single; // Tracks last video angle
    FSelectedVideoSnapshot: ISkImage;
    // Visual
    FBackgroundSkImage: ISkImage;
    FBackgroundColor: TAlphaColor;
    FHotTrackColor: TAlphaColor;
    FHotTrackWidth: Integer;
    FHotTrackZoom: Boolean;
    FIsMouseOverInfoIndicator: Boolean;
    FBreathingPhase: Double;
    FBreathRotationEnabled: Boolean;
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
    FShowSmallPicOnlyOnHover: Boolean;
    FKeepAreaFreeRect: TRect;
    FCornerRadius: Single;
    FRotationAllowed: Boolean;
    FAnimatedBackground: Boolean;
    FParticles: TList<TParticle>;
    FEnableParticlesOnMouseClick: Boolean;
    FIsRotating: Boolean;
    FRotatingImage: TImageItem;
    FLastMouseAngle: Single;
    FGridOffsetY: Single;
    FStartingAngle: Single;    //-1 is random rotation
    FParticleColor: TAlphaColor;
    FRotateHandlePosition: TSmallPicPosition;
    FRotateHandleSize: Integer;
    FSurfaceEffect: TSurfaceEffect;
    FRoundEdges: Integer;
    FPictureBorderType: TPictureBorderType;
    FRotateDotColor: TAlphaColor;
    FRotateDotHotColor: TAlphaColor;
    FRotateDotDownColor: TAlphaColor;
    FTechBracketWidth: Integer;
    FInfoFont: TFont;
    FInfoTextColor: TAlphaColor;
    FInfoPanelWidthPercent: Single;
    FHoverAlive: Boolean;
    FHoverAliveRange: Integer;
    FHoverAliveSpeed: Single;
    FInfoIndicatorColor: TAlphaColor;
    FShowInfoIndicator: Boolean;
    FBackgroundEffect: TBackgroundEffect;
    FMatrixCols: array of TMatrixColumn;
    FMatrixFont: TFont;
    FMatrixColor: TAlphaColor;
    FMatrixHeadColor: TAlphaColor;
    FMatrixSpeed: Single;
    FMatrixFontSize: Single;
    FSmallPicParticles: TList<TSmallPicParticle>;
    FSpawningSmallPics: Boolean;
    FInfoIndicatorHotColor: TAlphaColor;

    // Selection & Zoom
    FSelectedImage: TImageItem;
    FWasSelectedItem: TImageItem;
    FCurrentSelectedIndex: Integer;
    FMaxZoomSize: Integer;
    FZoomAnimationType: TZoomAnimationType;
    FSelectedMovable: Boolean;
    FDraggingSelected: Boolean;
    FDraggingImage: Boolean;
    FDraggedImage: TImageItem;
    FHotItem: TImageItem;
    FActivationZones: array of TActivationZone;
    FOnCaptionClick: TOnCaptionClick;
    FAlphaStatic: Integer;
    FAlphaHotPhase: Integer;
    FAlphaHotSelected: Integer;
    FIsMouseOverHandle: Boolean;
    FShowInfoOnMiddleClick: Boolean;
    FOnFullscreenEnterFired: Boolean;
    FGestureStartPos: TPointF;
    FGestureActive: Boolean;
    FAlwaysShowInfo: Boolean;
    FDeleteClicked: Boolean;

    // Paging
    FPageSize: Integer;
    FCurrentPage: Integer;
    FPageChangeInProgress: Boolean;
    FAutoScrollPageForNewAdded: Boolean;
    FTargetPage: Integer;

    // State
    FActive: Boolean;
    FThreadPriority: TThreadPriority;

    // SmallPic Support
    FSmallPicImageList: TImageList;
    FSmallPicPosition: TSmallPicPosition;
    FSmallPicVisible: Boolean;
    FSmallpicMargin: Integer;

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
    FOnSmallPicClick: TSmallPicClickEvent;
    FOnFullscreenEnter: TImageFullscreenEvent;

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
    procedure FreeAllImagesAndClearLists;
    procedure AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle; UseSavedPosition: Boolean; SavedTargetRect: TRect);
    procedure StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
    procedure DrawImageWithEffect(const ACanvas: ISkCanvas; const ImageItem: TImageitem; const DstRect: TRectF; const BasePaint: ISkPaint);
    function CalculateClearingTarget(ImageItem: TImageItem; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle; ZoominSelected: Boolean; SelectedTargetPos: TRect; Index: Integer): TRect;
    procedure DrawFluidInfo(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas);
    procedure DrawFluidInfo_Static(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas; LocalBmp: TBitmap; const CurrentPanelLeft, CurrentPanelRight, CurrentPanelTop, CurrentPanelBottom: Single; const BreathingPulse: Single; const ZoomFactor: Single; const InfoLines: TStringList; const LineSpacing: Single; const Paint: ISkPaint; const SkFont: ISkFont; const MaxLines: Integer; const ActualDirection: TInfoPanelDirection);
    procedure DrawFluidInfo_BlurEdge(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas; LocalBmp: TBitmap; const CurrentPanelLeft, CurrentPanelRight, CurrentPanelTop, CurrentPanelBottom: Single; const WaveX, WaveY: Single; const BreathingPulse, ZoomFactor: Single; const InfoLines: TStringList; const LineSpacing: Single; const Paint: ISkPaint; const SkFont: ISkFont; const MaxLines: Integer; const ActualDirection: TInfoPanelDirection);
    procedure DrawFluidInfo_Transparent(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas; LocalBmp: TBitmap; const CurrentPanelLeft, CurrentPanelRight, CurrentPanelTop, CurrentPanelBottom: Single; const BreathingPulse: Single; const ZoomFactor: Single; const InfoLines: TStringList; const LineSpacing: Single; const Paint: ISkPaint; const SkFont: ISkFont; const MaxLines: Integer; const ActualDirection: TInfoPanelDirection);
    procedure DrawInfoIndicator(ACanvas: ISkCanvas; const VisualRect: TRectF; const AItem: TImageItem);
    procedure CalculateLayout;
    procedure CalculateLayoutSorted;
    procedure CalculateLayoutFreeFloat;
    function IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
    procedure MarkAreaOccupied(var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer);
    function PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer): Boolean;
    function GetOptimalSize(const OriginalWidth, OriginalHeight: Integer; const MaxWidth, MaxHeight: Double): TSize;
    procedure InitMatrix;
    function GetLiveImageDataString(ImgIdx: Integer): string;
    procedure DrawBackgroundEffects(const ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint);
    procedure DrawMatrixBackground(const ACanvas: ISkCanvas);
    procedure CreateNewVideoSnapshot(AWidth, AHeight: Single);
    procedure DoAnimateBoom(AImageItem: TImageItem; ASync: Boolean);
    procedure DrawSmallPicParticles(ACanvas: ISkCanvas);   //atm instable

    //videoplayer
    procedure SetVideoFileSync;

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
    function GetRotateHandleRect(const ItemRect: TRect): TRect;
    function GetLoadingCount: Integer;
    procedure SetSelectedImage(ImageItem: TImageItem; Index: Integer);
    //function SkImageToBitmap(Img: ISkImage): TBitmap;   //we need probably later
    function BitmapToSkImage(Bmp: TBitmap): ISkImage;
    function GetMasterItem(Index: Integer): TFlowMasterItem;
    function GetSmallPicBitmap(const Index: Integer): TBitmap;
    function GetVisualRect(ImageItem: TImageItem): TRectF;
    function GetDominantColor(const Img: ISkImage): TAlphaColor;
    function GetLocalPoint(const P: TPointF; const Rect: TRectF; AngleDeg: Single): TPointF;
    function ResizeImageIfNeeded(const Source: ISkImage): ISkImage;
    function GetInfoPanelRect(const AItem: TImageItem; const VisualRect: TRectF): TRectF;
    // Property Setters
    procedure SetActive(Value: Boolean);
    procedure SetSelectedMovable(Value: Boolean);
    procedure SetHotZoomMaxFactor(const Value: Single);
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
    procedure SetShowSmallPicOnlyOnHover(const Value: Boolean);
    procedure SetSelectedCaptionColor(Value: TAlphaColor);
    procedure SetRotateDotColor(Value: TAlphaColor);
    procedure SetRotateDotHotColor(Value: TAlphaColor);
    procedure SetRotateDotDownColor(Value: TAlphaColor);
    procedure SetSelectedCaptionBackground(Value: TAlphaColor);
    procedure SetKeepAreaFreeRect(const Value: TRect);
    procedure SetAutoScrollPageForNewAdded(Value: Boolean);
    procedure SetFreeFloatDrift(Value: Boolean);
    procedure SetInfoPanelDirection(const Value: TInfoPanelDirection);
    procedure SetSmallPicVisible(const Value: Boolean);
    procedure SetEnableParticlesOnMouseClick(const Value: Boolean);
    procedure SetPageSize(Value: Integer);
    procedure SetHoverAlive(const Value: Boolean);
    procedure SetHoverAliveRange(const Value: Integer);
    procedure SetCornerRadius(const Value: Single);
    procedure SetRotationAllowed(const Value: Boolean);
    procedure SetAnimatedBackground(const Value: Boolean);
    procedure SpawnParticles(X, Y: Single; Count: Integer; Color: TAlphaColor);
    procedure SetShowInfoIndicator(const Value: Boolean);
    procedure SetInfoIndicatorColor(const Value: TAlphaColor);
    procedure SetBackgroundEffect(const Value: TBackgroundEffect);
    procedure SetMatrixColor(const Value: TAlphaColor);
    procedure SetMatrixHeadColor(const Value: TAlphaColor);
    procedure SetMatrixFontSize(const Value: Single);
    procedure SetMatrixSpeed(const Value: Single);
    procedure SetParticleColor(const Value: TAlphaColor);
    procedure SetRotateHandlePosition(const Value: TSmallPicPosition);
    procedure SetStartingAngle(const Value: Single);
    procedure SetSurfaceEffect(const Value: TSurfaceEffect);
    procedure SetRoundEdges(const Value: Integer);
    procedure SetPictureBorderType(const Value: TPictureBorderType);
    procedure SetOnSmallPicClick(const Value: TSmallPicClickEvent);
    procedure SetTechBracketWidth(const Value: Integer);
    procedure SetBreathRotationEnabled(const Value: Boolean);
    procedure SetInfoFont(Value: TFont);
    procedure SetInfoTextColor(Value: TAlphaColor);
    procedure SetInfoPanelWidthPercent(Value: Single);
    procedure SetRotateHandleSize(const Value: Integer);
    procedure SetInfoPanelStyle(const Value: TInfoAnimationStyle);
    procedure SetInfoIndicatorHotColor(const Value: TAlphaColor);
    procedure SetShowInfoOnMiddleClick(const Value: Boolean);
    procedure SetTestVideoFile(const Value: string);
    procedure SetCaptureVideo(const Value: Boolean);
    procedure SetAlwaysShowInfo(const Value: Boolean);
    procedure SetDeleteClicked(const Value: Boolean);
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
    procedure AddImagesWithPositions(const FileNames, Captions, Paths, Hints, Rotations: TStringList; const Positions: array of TImagePosition);
    function GetCurrentPositions: TImagePositions;
    procedure ResetPositions;
    procedure ResetAllRotations;
    // Image management
    procedure AddImage(const FileName: string; const AHint: string = ''; ASmallPicIndex: Integer = -1); overload;
    procedure AddImage(const FileName, ACaption, APath, AHint, AInfoText: string; ASmallPicIndex: Integer = -1); overload;
    procedure AddImage(Bitmap: TBitmap); overload;
    procedure AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = ''; const AHint: string = ''; const AInfo: string = ''; ASmallPicIndex: Integer = -1);
    procedure AddImages(const FileNames, Captions, Paths, Hints, Infos: TStringList; const SmallPicIndices: TList = nil);
    procedure AddImagesAsync(const FileNames, Captions, Paths, Hints, Infotxts: TStringList; const SmallPicIndices: TList = nil);
    procedure InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath, XHint, XInfo: string; ASmallPicIndex: Integer = -1);
    procedure InsertImageAsync(const FileName, Caption, Path, Hint, Info: string; ASmallPicIndex: Integer = -1);
    procedure SetImage(Index: Integer; Bitmap: TBitmap);
    procedure RemoveImage(Index: Integer; Animated: Boolean = True); overload;
    procedure RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom; AndFree: Boolean = true); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean = false); overload;
    procedure MoveImageToPos(RelIndexFrom, RelIndexTo: Integer);

    // Activation zone
    procedure AddActivationZone(const AName: string; const ARect: TRectF);
    procedure ClearActivationZones;
    // Navigation
    procedure SelectNextImage;
    procedure SelectPreviousImage;
    procedure DeselectZoomedImage;
    procedure ScrollToIndex(Index: Integer; Animate: Boolean = True);
    procedure ZoomSelectedToFull;
    function GetSpatialCandidate(CurrentImage: TImageItem; const Action: TSmartImageAction): TImageItem;

    procedure SelectNorth;
    procedure SelectSouth;
    procedure SelectWest;
    procedure SelectEast;
    // Search
    function FindImageByPath(const Path: string): TImageItem;
    function FindImageByCaption(const Caption: string): TImageItem;
    function GetImageIndex(ImageItem: TImageItem): Integer;
    function GetImageAtPoint(X, Y: Single): TImageItem;
    // Utilities
    function GetPicture(index: Integer): TBitmap;
    procedure SetBackgroundpicture(const Path: string);
    procedure WaitForAllLoads;
    procedure PutAllToAngle(const Angle: Single);
    procedure RotateAllBy(const AngleDelta: Single);
    procedure ShowInfoPanel(ImageItem: TImageItem);
    // Properties
    property MaxZoomSize: Integer read FMaxZoomSize write FMaxZoomSize default DEFAULT_MAX_ZOOM_SIZE;
    property HotTrackColor: TAlphaColor read FHotTrackColor write SetHotTrackColor;
    property GlowColor: TAlphaColor read FGlowColor write SetGlowColor;
    property GlowWidth: Integer read FGlowWidth write SetGlowWidth;
    property HotTrackWidth: Integer read FHotTrackWidth write SetHotTrackWidth;
    property ShowSmallPicOnlyOnHover: Boolean read FShowSmallPicOnlyOnHover write SetShowSmallPicOnlyOnHover default True;
    property CaptionOnHoverOnly: Boolean read FCaptionOnHoverOnly write SetCaptionOnHoverOnly default True;
    property PageCount: Integer read GetPageCount;
    property CurrentSelectedIndex: Integer read FCurrentSelectedIndex;
    property ImageCount: Integer read GetImageCount;
    property LoadingCount: Integer read GetLoadingCount;
    property SelectedImage: TImageItem read FSelectedImage;
    property Items[Index: Integer]: TImageItem read GetImageItem; default;
    property Images[Index: Integer]: TImageItem read GetImageItem;
    property AllImageItems[Index: Integer]: TFlowMasterItem read GetMasterItem;
    property TechBracketWidth: Integer read FTechBracketWidth write SetTechBracketWidth;
    property DeleteClicked: Boolean read FDeleteClicked write SetDeleteClicked;
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
    property HoverAliveOnFullscreen: Boolean read FHoverAliveOnFullscreen write FHoverAliveOnFullscreen default True;
    property ImageEntryStyle: TImageEntryStyle read FImageEntryStyle write FImageEntryStyle default iesRandom;
    property EntryPoint: TPoint read FEntryPoint write FEntryPoint;
    property OnSelectedImageEnterZone: TSelectedImageEnterZoneEvent read FOnSelectedImageEnterZone write FOnSelectedImageEnterZone;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default false;
    property InfoFont: TFont read FInfoFont write SetInfoFont;
    property InfoTextColor: TAlphaColor read FInfoTextColor write SetInfoTextColor default TAlphaColors.Aqua;
    property InfoPanelWidthPercent: Single read FInfoPanelWidthPercent write SetInfoPanelWidthPercent;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionColor: TAlphaColor read FCaptionColor write SetCaptionColor;
    property CaptionBackground: TAlphaColor read FCaptionBackground write SetCaptionBackground;
    property CaptionAlpha: Byte read FCaptionAlpha write SetCaptionAlpha default 140;
    property CaptionOffsetY: Integer read FCaptionOffsetY write SetCaptionOffsetY default 8;
    property OnCaptionClick: TOnCaptionClick read FOnCaptionClick write FOnCaptionClick;
    property RotateDotColor: TAlphaColor read FRotateDotColor write SetRotateDotColor;
    property RotateDotHotColor: TAlphaColor read FRotateDotHotColor write SetRotateDotHotColor;
    property RotateDotDownColor: TAlphaColor read FRotateDotDownColor write SetRotateDotDownColor;
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
    property SmallpicMargin: Integer read FSmallpicMargin write FSmallpicMargin;
    property SmallPicVisible: Boolean read FSmallPicVisible write SetSmallPicVisible default True;
    property CornerRadius: Single read FCornerRadius write SetCornerRadius;
    property RotateHandleSize: Integer read FRotateHandleSize write SetRotateHandleSize;
    property RotationAllowed: Boolean read FRotationAllowed write SetRotationAllowed default True;
    property RotateHandlePosition: TSmallPicPosition read FRotateHandlePosition write SetRotateHandlePosition default spTopRight;
    property AnimatedBackground: Boolean read FAnimatedBackground write SetAnimatedBackground default False;
    property ParticleColor: TAlphaColor read FParticleColor write SetParticleColor;
    property StartingAngle: Single read FStartingAngle write SetStartingAngle;
    property EnableParticlesOnMouseClick: Boolean read FEnableParticlesOnMouseClick write SetEnableParticlesOnMouseClick default False; // <--- ADD THIS
    property SurfaceEffect: TSurfaceEffect read FSurfaceEffect write SetSurfaceEffect default sueNone;
    property RoundEdges: Integer read FRoundEdges write SetRoundEdges default 0;
    property PictureBorderType: TPictureBorderType read FPictureBorderType write SetPictureBorderType default btTech;
    property AlphaStatic: Integer read FAlphaStatic write FAlphaStatic;
    property AlphaHotPhase: Integer read FAlphaHotPhase write FAlphaHotPhase;
    property AlphaHotSelected: Integer read FAlphaHotSelected write FAlphaHotSelected;
    property OnSmallPicClick: TSmallPicClickEvent read FOnSmallPicClick write SetOnSmallPicClick;
    property MaxInternalPicSize: Integer read FMaxInternalPicSize write FMaxInternalPicSize default 720;
    property BreathRotationEnabled: Boolean read FBreathRotationEnabled write SetBreathRotationEnabled default True;
    property FullscreenAngle: TFullscreenAngle read FFullscreenAngle write FFullscreenAngle;
    property InfoPanelAnimationStyle: TInfoAnimationStyle read FInfoPanelStyle write SetInfoPanelStyle;
    property HotZoomMaxFactor: Single read FHotZoomMaxFactor write SetHotZoomMaxFactor;
    property InfoPanelDirection: TInfoPanelDirection read FInfoPanelDirection write SetInfoPanelDirection;
    property HoverAlive: Boolean read FHoverAlive write SetHoverAlive default False;
    property HoverAliveRange: Integer read FHoverAliveRange write SetHoverAliveRange default 8;
    property HoverAliveSpeed: Single read FHoverAliveSpeed write FHoverAliveSpeed;
    property InfoIndicatorColor: TAlphaColor read FInfoIndicatorColor write SetInfoIndicatorColor;
    property ShowInfoIndicator: Boolean read FShowInfoIndicator write SetShowInfoIndicator;
    property BackgroundEffect: TBackgroundEffect read FBackgroundEffect write SetBackgroundEffect;
    property MatrixFont: TFont read FMatrixFont write FMatrixFont;
    property MatrixColor: TAlphaColor read FMatrixColor write SetMatrixColor;
    property MatrixHeadColor: TAlphaColor read FMatrixHeadColor write SetMatrixHeadColor;
    property MatrixSpeed: Single read FMatrixSpeed write SetMatrixSpeed;
    property MatrixFontSize: Single read FMatrixFontSize write SetMatrixFontSize;
    property InfoIndicatorHotColor: TAlphaColor read FInfoIndicatorHotColor write SetInfoIndicatorHotColor;
    property ShowInfoOnMiddleClick: Boolean read FShowInfoOnMiddleClick write SetShowInfoOnMiddleClick;
    property OnFullscreenEnter: TImageFullscreenEvent read FOnFullscreenEnter write FOnFullscreenEnter;
    property TestVideoFile: string read FTestVideoFile write SetTestVideoFile;
    property CaptureVideo: Boolean read FCaptureVideo write SetCaptureVideo;
    property AlwaysShowInfo: Boolean read FAlwaysShowInfo write SetAlwaysShowInfo default False;

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

var
  // Global pointers to help the standalone Sort function access class data
  GlobalSortHotItem: TImageItem;

implementation
// === PROTECTED ACCESS CRACKER ===

type
  TControlAccess = class(TControl);

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
var
  Flow: TSkFlowmotion;
  PageStart, RelIndex: Integer;
begin
  Flow := TSkFlowmotion(FOwner);

  // 1. Update Master List (Persistent storage)
  if (FIndex >= 0) and (FIndex < Flow.FAllCaptions.Count) then
    Flow.FAllCaptions[FIndex] := Value;

  // 2. Update Visible Item (Real-time update if on screen)
  if Flow.Visible then
  begin
    PageStart := Flow.FCurrentPage * Flow.FPageSize;

    // Check if this item belongs to the currently visible page
    if (FIndex >= PageStart) and (FIndex < PageStart + Flow.FPageSize) then
    begin
      RelIndex := FIndex - PageStart;

      // Safety check to ensure the visual list has this item
      if (RelIndex >= 0) and (RelIndex < Flow.FImages.Count) then
      begin
        TImageItem(Flow.FImages[RelIndex]).Caption := Value;
        Flow.Repaint; // Trigger redraw to show change immediately
      end;
    end;
  end;
end;

procedure TFlowMasterItem.SetHint(const Value: string);
var
  Flow: TSkFlowmotion;
  PageStart, RelIndex: Integer;
begin
  Flow := TSkFlowmotion(FOwner);

  // 1. Update Master List (Persistent storage)
  if (FIndex >= 0) and (FIndex < Flow.FAllHints.Count) then
    Flow.FAllHints[FIndex] := Value;

  // 2. Update Visible Item (Real-time update if on screen)
  if Flow.Visible then
  begin
    PageStart := Flow.FCurrentPage * Flow.FPageSize;

    // Check if this item belongs to the currently visible page
    if (FIndex >= PageStart) and (FIndex < PageStart + Flow.FPageSize) then
    begin
      RelIndex := FIndex - PageStart;

      if (RelIndex >= 0) and (RelIndex < Flow.FImages.Count) then
      begin
        TImageItem(Flow.FImages[RelIndex]).Hint := Value;
        // Hint usually appears on hover, so Repaint is good to have ready
        Flow.Repaint;
      end;
    end;
  end;
end;

procedure TFlowMasterItem.SetSmallPicIndex(const Value: Integer);
var
  Flow: TSkFlowmotion;
  PageStart, RelIndex: Integer;
begin
  Flow := TSkFlowmotion(FOwner);

  // 1. Update Master List (Persistent storage)
  if (FIndex >= 0) and (FIndex < Flow.FAllSmallPicIndices.Count) then
    Flow.FAllSmallPicIndices[FIndex] := Pointer(Value);

  // 2. Update Visible Item (Real-time update if on screen)
  if Flow.Visible then
  begin
    PageStart := Flow.FCurrentPage * Flow.FPageSize;

    // Check if this item belongs to the currently visible page
    if (FIndex >= PageStart) and (FIndex < PageStart + Flow.FPageSize) then
    begin
      RelIndex := FIndex - PageStart;

      if (RelIndex >= 0) and (RelIndex < Flow.FImages.Count) then
      begin
        TImageItem(Flow.FImages[RelIndex]).SmallPicIndex := Value;
        Flow.Repaint; // Trigger redraw to show/hide new icon
      end;
    end;
  end;
end;

function TFlowMasterItem.GetHint: string;
begin
  Result := (FOwner as TSkFlowmotion).FAllHints[FIndex];
end;

function TFlowMasterItem.GetSmallPicIndex: Integer;
begin
  Result := Integer((FOwner as TSkFlowmotion).FAllSmallPicIndices[FIndex]);
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
  FAlpha := START_ALPHA;
  FTargetAlpha := DEFAULT_ALPHA;
  FIsSelected := False;
  FZoomProgress := 0;
  FInfoText := '';
  { Initial Zoom State (Breathing) }
  FHotZoom := 1;
  FHotZoomTarget := 1;
  { Initial Drift Parameters }
  DriftRangeX := 10 + Random(20);
  DriftRangeY := 8 + Random(15);
  FIsInfoShowing := False;
  FInfoProgress := 0.0;
  // === INIT HOVER MOVEMENT ===
  // Random direction and slow speed
  // (Random - 0.5) gives a value between -0.5 and 0.5.
  // We multiply by 0.3 to keep it very slow.
  FHoverX := 0;
  FHoverY := 0;
  FHoverVX := (Random - 0.5) * 0.3;
  FHoverVY := (Random - 0.5) * 0.3;
end;

destructor TImageItem.destroy;
begin
  FSkImage := nil;
  inherited;
end;
// -----------------------------------------------------------------------------
// TImageLoadThread: Asynchronous image loader
// -----------------------------------------------------------------------------

constructor TImageLoadThread.Create(const AFileName, ACaption, APath, AHint, AInfoText: string; AOwner: TComponent; AIndex: Integer; ASmallPicIndex: Integer = -1);
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
  FInfoText := AInfoText;
  FSmallPicIndex := ASmallPicIndex;
  { Set priority }
  if AOwner is TSkFlowmotion then
    Priority := (AOwner as TSkFlowmotion).FThreadPriority
  else
    Priority := tpNormal;
  Self.Execute;
end;

procedure TImageLoadThread.Execute;
begin
  FSuccess := False;
  try
    if TFile.Exists(FFileName) then
    begin
      FSkImage := TSkImage.MakeFromEncodedFile(FFileName);
      if Assigned(FSkImage) then
        FSuccess := True
      else
        FSuccess := False;
    end;
  except
    FSuccess := False;
  end;

  if (not Terminated) and FSuccess then
    Synchronize(SyncAddImage);

  if not Terminated then
  begin
    try
      if FOwner is TSkFlowmotion then
      begin
        FSkImage := (FOwner as TSkFlowmotion).ResizeImageIfNeeded(FSkImage);
        (FOwner as TSkFlowmotion).DoImageLoad(FFileName, FSuccess);
        if not FSuccess then
        begin
          if Assigned((FOwner as TSkFlowmotion).FOnImageLoadFailed) then
            (FOwner as TSkFlowmotion).FOnImageLoadFailed(Self, FFileName, 'Failed to decode image or file not found.');
        end;
      end;
    except
    end;
  end;

  // --- Notify owner to remove us from list and Free us ---
  if (not Terminated) and (FOwner is TSkFlowmotion) then
    (FOwner as TSkFlowmotion).ThreadFinished(Self);
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

        // === ADD INFO TEXT TO MASTER LIST ===
        if AbsIndex < FAllInfoTexts.Count then
          FAllInfoTexts[AbsIndex] := FInfoText
        else
          FAllInfoTexts.Add(FInfoText);

        { Check if page is changing or clearing }
        if FPageChangeInProgress or FisClearing then
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
          NewItem.InfoText := FInfoText;
          NewItem.ImageIndex := AbsIndex;
          NewItem.FileName := FFileName;
          NewItem.Direction := GetEntryDirection;
          NewItem.SmallPicIndex := SmallIdxFromMaster;
          NewItem.DominantColor := GetDominantColor(FSkImage);
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
var
  LParent: TFMXObject;
begin
  inherited Create(AOwner);

  //Put form quality to high performance
  if not (csDesigning in ComponentState) then
  begin
    // 1. Try Owner first (Most of the time, Owner is the Form)
    if (AOwner <> nil) and (AOwner is TCommonCustomForm) then
    begin
      TCustomForm(AOwner).Quality := TCanvasQuality.HighPerformance;
    end
    else
    begin
      // 2. If Owner isn't the form, walk up the visual Parent chain
      LParent := Self.Parent;
      while Assigned(LParent) and not (LParent is TCommonCustomForm) do
        LParent := LParent.Parent;

      if Assigned(LParent) and (LParent is TCommonCustomForm) then
        TCustomForm(LParent).Quality := TCanvasQuality.HighPerformance;
    end;
  end;

  { --- Initialize Fonts and Colors --- }
  FCaptionFont := TFont.Create;
  FCaptionFont.Family := 'Segoe UI';
  FCaptionFont.Size := 10;
  FCaptionFont.Style := [TFontStyle.fsBold];
  FCaptionColor := TAlphaColors.White;
  FCaptionBackground := TAlphaColors.Black;
  FSelectedCaptionColor := TAlphaColors.Black;
  FSelectedCaptionBackground := TAlphaColors.Aqua;
  FCaptionAlpha := 140;
  FShowCaptions := true;
  FAutoScrollPageForNewAdded := False;
  FShowSmallPicOnlyOnHover := True;
  FCaptionOnHoverOnly := True;
  FCaptionOffsetY := 8;
  // === INITIALIZE INFO FONT ===
  FInfoFont := TFont.Create;
  FInfoFont.Family := 'Segoe UI';
  FInfoFont.Size := 22; // MUCH LARGER DEFAULT
  FInfoFont.Style := [TFontStyle.fsBold]; // BOLD BY DEFAULT
  FInfoTextColor := TAlphaColors.Aqua; // DEFAULT COLOR
  FInfoPanelWidthPercent := 0.3; // COVERS 30% OF IMAGE
  { --- Initialize Lists --- }
  FImages := TList.Create;
  FLoadingThreads := TList.Create;
  FAllFiles := TStringList.Create;
  FAllCaptions := TStringList.Create;
  FAllPaths := TStringList.Create;
  FAllHints := TStringList.Create;
  FAllInfoTexts := TStringList.Create;
  FAllSmallPicIndices := TList.Create; // Init Master List
  SetLength(FLoadedPositions, 0);
  FMasterItem := TFlowMasterItem.Create(Self); // Init Master Item
  { --- Defaults - State --- }
  FNextLoaderCoreIndex := 0;
  FisClearing := False;
  FFreeFloatDrift := false;
  FBreathingPhase := 0.0;
  FBreathRotationEnabled := False;
  FAnimationThread := nil;
  FImageEntryStyle := iesRandom;
  FInfoPanelStyle := iasTransparent;   //iasBlurEdge, iasStatic, iasTransparent
  FEntryPoint := TPoint.Create(-1000, -1000);
  FIsRotating := False;
  FShowInfoOnMiddleClick := True;
  FOnFullscreenEnterFired := False;
  { --- Defaults - Layout --- }
  FFlowLayout := flSorted;
  FKeepSpaceforZoomed := false;
  FAnimationSpeed := DEFAULT_ANIMATION_SPEED;
  FSpacing := 0;
  FKeepAspectRatio := True;
  { --- Defaults - Visuals --- }
  FHotZoomMaxFactor := 1.3;
  FFullscreenAngle := fsa0;  //fsa0, fsa90, fsa180, fsa270);
  FBackgroundColor := TAlphaColors.Black;
  FHotTrackColor := TAlphaColors.Teal;
  FHotTrackZoom := true;
  FMaxColumns := 0;
  FMaxRows := 0;
  FAlwaysShowInfo := False;
  FInfoIndicatorColor := TAlphaColors.Darkorange;
  FInfoIndicatorHotColor := TAlphaColors.Aqua;
  FShowInfoIndicator := True;
  FEnableParticlesOnMouseClick := True;
  FGlowColor := TAlphaColors.Aqua;
  FGlowWidth := DEFAULT_GLOW_WIDTH;
  FHotTrackWidth := DEFAULT_HOTTRACK_WIDTH;
  FParticles := TList<TParticle>.Create;
  FCornerRadius := 0.0;
  FRotationAllowed := True;
  FAnimatedBackground := False;
  FGridOffsetY := 0;
  FParticleColor := TAlphaColors.Aqua;
  FRotateHandlePosition := spTopRight;
  FStartingAngle := -1;  //-1 is some random
  FSurfaceEffect := sueShadow;   //sueNone, sueShadow, sueGlow, sueAmbient
  FPictureBorderType := btFull; //btFull, btTech
  FRoundEdges := 10;
  FAlphaStatic := 220;
  FAlphaHotPhase := 255;
  FAlphaHotSelected := 210;
  FSmallpicMargin := 8;
  FTechBracketWidth := 25;
  FRotateHandleSize := 18;
  FRotateDotColor := TAlphaColors.Teal;
  RotateDotDownColor := TAlphaColors.Orange;
  FRotateDotHotColor := TAlphaColors.Aqua;
  FHoverAlive := True;
  FHoverAliveRange := 8;
  FHoverAliveSpeed := 20.0;
  FHoverAliveOnFullscreen := True;
  FBackgroundEffect := beRealMatrix; //beHolographic, beRealMatrix
  FMatrixFont := TFont.Create;
  FMatrixFont.Family := 'Consolas';
  FMatrixFont.Size := 22;
  FMatrixColor := TAlphaColors.Teal;
  FMatrixHeadColor := TAlphaColors.Black;
  FMatrixSpeed := 10.0;
  FMatrixFontSize := 14.0;
  SetLength(FMatrixCols, 0);
  { --- Defaults - Selection --- }
  FSelectedImage := nil;
  FWasSelectedItem := nil;
  FZoomAnimationType := zatSlide;
  FSelectedMovable := False;
  FDraggingSelected := False;
  FSorted := True;
  FAnimationEasing := True;
  FLoadingCount := 0;
  FDeleteClicked := False;
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
  FMaxInternalPicSize := 720;
  // ==========================================================
  // CREATE VIDEO PLAYER (Initialize at Startup)
  // ==========================================================
  FCaptureVideo := False;
  FVideoPlayer := TMediaPlayerControl.Create(Self);
  FVideoPlayer.Parent := Self;
  FVideoPlayer.SetBounds(10, 10, 100, 100); // Start off-screen
  FVideoPlayer.Visible := True; // Keep it hidden initially
  FVideoPlayer.HitTest := False; // Pass mouse clicks to Skia
  FVideoPlayer.MediaPlayer := TMediaPlayer.Create(Self);
  FVideoPlayer.MediaPlayer.Volume := 100;
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
    if Assigned(FVideoPlayer) then
      FVideoPlayer.Free;
    { --- Terminate Load Threads --- }
    for i := 0 to FLoadingThreads.Count - 1 do
    begin
      try
        TImageLoadThread(FLoadingThreads[i]).Terminate;
      except
      end;
    end;
   { --- Wait briefly for them to finish naturally --- }
    StartTime := GetTickCount;
    while (FLoadingThreads.Count > 0) and ((GetTickCount - StartTime) < 1000) do
    begin
      CheckSynchronize(10);
      Sleep(5);
    end;
    { --- Force kill any remaining threads --- }
    // Since FreeOnTerminate is False, we MUST free them here to avoid leak
    for i := 0 to FLoadingThreads.Count - 1 do
    begin
      try
        TImageLoadThread(FLoadingThreads[i]).Free;
      except
      end;
    end;

    FLoadingThreads.Clear; // Now pointers are safe to clear
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
    FAllInfoTexts.Free;
    FAllSmallPicIndices.Free;
    { --- Free Other Objects --- }
    if Assigned(FCaptionFont) then
      FCaptionFont.Free;
    if Assigned(FInfoFont) then
      FInfoFont.Free;
    if Assigned(FMasterItem) then
      FMasterItem.Free;
    if Assigned(FParticles) then
      FParticles.Free;
    if Assigned(FSmallPicParticles) then
      FSmallPicParticles.Free;
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

procedure TSkFlowmotion.SetDeleteClicked(const Value: Boolean);
begin
  if FDeleteClicked <> Value then
  begin
    FDeleteClicked := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetAlwaysShowInfo(const Value: Boolean);
begin
  if FAlwaysShowInfo <> Value then
  begin
    FAlwaysShowInfo := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetTestVideoFile(const Value: string);
begin
  if FTestVideoFile = Value then
    Exit;

  // 1. Store value locally first (prevents re-entry)
  FTestVideoFile := Value;

  // 2. Trigger the Safe Sync
  TThread.Synchronize(nil, SetVideoFileSync);
end;

procedure TSkFlowmotion.SetCaptureVideo(const Value: Boolean);
begin
  if FCaptureVideo <> Value then
  begin
    FCaptureVideo := Value;
    // Hide and stop player if turning off
    if not FCaptureVideo then
      if Assigned(FVideoPlayer) then
      begin
        FVideoPlayer.Visible := False;
        FVideoPlayer.MediaPlayer.Stop;
        FVideoPlayer.MediaPlayer.Clear;
        FVideoPlayer.Visible := False;
      end;
  end;
end;

procedure TSkFlowmotion.SetShowInfoOnMiddleClick(const Value: Boolean);
begin
  if FShowInfoOnMiddleClick <> Value then
    FShowInfoOnMiddleClick := Value;
end;

procedure TSkFlowmotion.SetInfoIndicatorHotColor(const Value: TAlphaColor);
begin
  if FInfoIndicatorHotColor <> Value then
  begin
    FInfoIndicatorHotColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetBackgroundEffect(const Value: TBackgroundEffect);
begin
  if FBackgroundEffect <> Value then
  begin
    FBackgroundEffect := Value;
    if FBackgroundEffect = beRealMatrix then
      InitMatrix;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetMatrixColor(const Value: TAlphaColor);
begin
  if FMatrixColor <> Value then
  begin
    FMatrixColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetMatrixHeadColor(const Value: TAlphaColor);
begin
  if FMatrixHeadColor <> Value then
  begin
    FMatrixHeadColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetMatrixSpeed(const Value: Single);
begin
  if FMatrixSpeed <> Value then
  begin
    FMatrixSpeed := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetMatrixFontSize(const Value: Single);
begin
  if FMatrixFontSize <> Value then
  begin
    FMatrixFontSize := Value;
    if FBackgroundEffect = beRealMatrix then
      InitMatrix;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetInfoIndicatorColor(const Value: TAlphaColor);
begin
  if FInfoIndicatorColor <> Value then
  begin
    FInfoIndicatorColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetShowInfoIndicator(const Value: Boolean);
begin
  if FShowInfoIndicator <> Value then
  begin
    FShowInfoIndicator := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetHoverAlive(const Value: Boolean);
begin
  if FHoverAlive <> Value then
  begin
    FHoverAlive := Value;
    // Reset positions if turned off so they snap back cleanly?
    // Optional: Resetting offsets makes it look sharper when stopping.
    if not Value then
    begin
      var i: Integer;
      for i := 0 to FImages.Count - 1 do
      begin
        TImageItem(FImages[i]).FHoverX := 0;
        TImageItem(FImages[i]).FHoverY := 0;
      end;
    end;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetHoverAliveRange(const Value: Integer);
begin
  if FHoverAliveRange <> Value then
  begin
    FHoverAliveRange := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetEnableParticlesOnMouseClick(const Value: Boolean);
begin
  if FEnableParticlesOnMouseClick <> Value then
  begin
    FEnableParticlesOnMouseClick := Value;
    // No need to repaint, this is a state flag
  end;
end;

procedure TSkFlowmotion.SetInfoPanelDirection(const Value: TInfoPanelDirection);
begin
  if FInfoPanelDirection <> Value then
  begin
    FInfoPanelDirection := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetHotZoomMaxFactor(const Value: Single);
begin
  if FHotZoomMaxFactor <> Value then
  begin
    FHotZoomMaxFactor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetInfoPanelStyle(const Value: TInfoAnimationStyle);
begin
  if FInfoPanelStyle <> Value then
  begin
    FInfoPanelStyle := Value;
    Repaint; // Update screen immediately to show change
  end;
end;

procedure TSkFlowmotion.SetRotateHandleSize(const Value: Integer);
begin
  if FRotateHandleSize <> Value then
  begin
    FRotateHandleSize := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetInfoFont(Value: TFont);
begin
  FInfoFont.Assign(Value);
  Repaint;
end;

procedure TSkFlowmotion.SetInfoTextColor(Value: TAlphaColor);
begin
  if FInfoTextColor <> Value then
  begin
    FInfoTextColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetInfoPanelWidthPercent(Value: Single);
begin
  if (Value < 0) or (Value > 1.0) then
    Exit; // 0 to 1.0
  if FInfoPanelWidthPercent <> Value then
  begin
    FInfoPanelWidthPercent := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetBreathRotationEnabled(const Value: Boolean);
begin
  if FBreathRotationEnabled <> Value then
  begin
    FBreathRotationEnabled := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetTechBracketWidth(const Value: Integer);
begin
  if FTechBracketWidth <> Value then
  begin
    FTechBracketWidth := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetOnSmallPicClick(const Value: TSmallPicClickEvent);
begin
  FOnSmallPicClick := Value;
end;

procedure TSkFlowmotion.SetPictureBorderType(const Value: TPictureBorderType);
begin
  if FPictureBorderType <> Value then
  begin
    FPictureBorderType := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetRoundEdges(const Value: Integer);
begin
  if FRoundEdges <> Value then
  begin
    FRoundEdges := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetSurfaceEffect(const Value: TSurfaceEffect);
begin
  if FSurfaceEffect <> Value then
  begin
    FSurfaceEffect := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetParticleColor(const Value: TAlphaColor);
begin
  if FParticleColor <> Value then
  begin
    FParticleColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetRotateHandlePosition(const Value: TSmallPicPosition);
begin
  if FRotateHandlePosition <> Value then
  begin
    FRotateHandlePosition := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetStartingAngle(const Value: Single);
begin
  if FStartingAngle <> Value then
  begin
    FStartingAngle := Value;
  end;
end;

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
          FHotItem.FHotZoomTarget := 1.0;
        FHotItem := nil;
      end;
      for i := 0 to FImages.Count - 1 do
        if (TImageItem(FImages[i]).FHotZoom > 1.0) and (TImageItem(FImages[i]) <> FSelectedImage) then
          TImageItem(FImages[i]).FHotZoomTarget := 1.0;
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

procedure TSkFlowmotion.SetShowSmallPicOnlyOnHover(const Value: Boolean);
begin
  if FShowSmallPicOnlyOnHover <> Value then
  begin
    FShowSmallPicOnlyOnHover := Value;
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

procedure TSkFlowmotion.SetRotateDotColor(Value: TAlphaColor);
begin
  if FRotateDotColor <> Value then
  begin
    FRotateDotColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetRotateDotDownColor(Value: TAlphaColor);
begin
  if FRotateDotDownColor <> Value then
  begin
    FRotateDotDownColor := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetRotateDotHotColor(Value: TAlphaColor);
begin
  if FRotateDotHotColor <> Value then
  begin
    FRotateDotHotColor := Value;
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

procedure TSkFlowmotion.ShowInfoPanel(ImageItem: TImageItem);
begin
  if ImageItem = nil then
    Exit;

  // Toggle the state (ON or OFF)
  ImageItem.FIsInfoShowing := not ImageItem.FIsInfoShowing;

  // Wake up the engine
  StartAnimationThread;
end;
// -----------------------------------------------------------------------------
// PutAllToAngle
// Sets the rotation angle of ALL visible images to a specific value.
// Example: PutAllToAngle(45) makes them all diagonal.
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.PutAllToAngle(const Angle: Single);
var
  i: Integer;
  ImageItem: TImageItem;
begin
  if FImages.Count = 0 then
    Exit;

  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if Assigned(ImageItem) then
    begin
      // 1. Set the Target (The goal)
      ImageItem.FTargetRotation := Angle;

      // 2. Mark as animating so the thread runs and repaints
      ImageItem.Animating := True;
    end;
  end;

  // 3. Wake up the thread so we can see them spin
  StartAnimationThread;
end;
// -----------------------------------------------------------------------------
// RotateAllBy
// Adds 'AngleDelta' to the CURRENT rotation of all images.
// Useful for Physics/Sensor effects (e.g. Phone Tilting).
// If Image A is at 10 deg and we call RotateAllBy(5), it becomes 15 deg.
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.RotateAllBy(const AngleDelta: Single);
var
  i: Integer;
  ImageItem: TImageItem;
begin
  if FImages.Count = 0 then
    Exit;

  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if Assigned(ImageItem) then
    begin
      // 1. Update the Target (Current Target + Delta)
      // This keeps the relative rotation between images intact
      ImageItem.FTargetRotation := ImageItem.FTargetRotation + AngleDelta;

      // 2. Mark as animating so the smoothing engine runs
      ImageItem.Animating := True;
    end;
  end;

  // 3. Wake up the thread
  StartAnimationThread;
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

function TSkFlowmotion.GetDominantColor(const Img: ISkImage): TAlphaColor;
var
  Surface: ISkSurface;
  Pixmap: ISkPixmap;
  Info: TSkImageInfo;
  P: PByte;
begin
  Result := TAlphaColors.Black; // Default fallback
  if not Assigned(Img) then
    Exit;

  try
    // 1. Create Image Info (RGBA8888, Premul Alpha)
    Info := TSkImageInfo.Create(1, 1, TSkColorType.RGBA8888, TSkAlphaType.Premul);

    // 2. Create a 1x1 pixel Surface
    Surface := TSkSurface.MakeRaster(Info);
    if not Assigned(Surface) then
      Exit;

    // 3. Draw image onto 1x1 surface (Skia averages pixels automatically)
    Surface.Canvas.Clear(TAlphaColors.Black);
    Surface.Canvas.DrawImageRect(Img, RectF(0, 0, Img.Width, Img.Height), RectF(0, 0, 1, 1), TSkSamplingOptions.High);

    // 4. Read the pixel data back
    Pixmap := Surface.PeekPixels;
    if Assigned(Pixmap) then
    begin
      // Get pointer to pixel data
      P := Pixmap.Pixels;

      if Assigned(P) then
      begin
        // RGBA8888 stores bytes as: R, G, B, A
        // TAlphaColor is: (A shl 24) or (B shl 16) or (G shl 8) or R
        Result := (P[3] shl 24) or (P[2] shl 16) or (P[1] shl 8) or P[0];
      end;
    end;
  except
    // Fallback to black on any error
    Result := TAlphaColors.Black;
  end;
end;
  //with rotation calculation but...making problems still, top bottom calc wrong left right works fine and then ...
  //we get more to do at rotate itself and on layout some lay above then too, so atm we not use but save
{
function TSkFlowmotion.GetVisualRect(ImageItem: TImageItem): TRectF;
var
  // Use a Local BaseRect (Copy) to avoid overwriting Animated state
  BaseRect: TRectF;
  CenterX, CenterY, BaseW, BaseH, NewW, NewH: Single;
  ZoomFactor: Double;
  ShiftX, ShiftY: Single;
  // Rotation & Bounding Box vars
  VisualAngle: Single;
  Rad, SinA, CosA: Single;
  HW, HH: Single;
  CornerX, CornerY: Single;
  MinX, MinY, MaxX, MaxY: Single;
  i: Integer;
 // Helper Lambda to update corners based on current Result
  procedure UpdateCorners;
  var
  i: Integer;
  begin
    // Re-Calculate HW/HH based on current Result size
    HW := (Result.Width) / 2;
    HH := (Result.Height) / 2;
    // Re-Calculate Center
    CenterX := (Result.Left + Result.Right) / 2;
    CenterY := (Result.Top + Result.Bottom) / 2;
    Rad := VisualAngle * (PI / 180);
    SinA := Sin(Rad);
    CosA := Cos(Rad);
    // Loop through 4 corners to find Min/Max
    MinX := 0; MinY := 0; MaxX := 0; MaxY := 0;
    for i := 0 to 3 do
    begin
      if (i = 0) or (i = 3) then // Left corners
        CornerX := -HW
      else // Right corners
        CornerX := HW;
      if (i = 0) or (i = 1) then // Top corners
        CornerY := -HH
      else // Bottom corners
        CornerY := HH;
      // Rotate vector
      CornerX := CenterX + (CornerX * CosA - CornerY * SinA);
      CornerY := CenterY + (CornerX * SinA + CornerY * CosA);
      // Update Min/Max
      if i = 0 then
      begin
        MinX := CornerX; MaxX := CornerX;
        MinY := CornerY; MaxY := CornerY;
      end
      else
      begin
        if CornerX < MinX then MinX := CornerX;
        if CornerX > MaxX then MaxX := CornerX;
        if CornerY < MinY then MinY := CornerY;
        if CornerY > MaxY then MaxY := CornerY;
      end;
    end;
  end;
begin
  if not ImageItem.Visible then
    Exit;
  // ==========================================
  // 1. IGNORE WALLS WHILE CLEARING
  // ==========================================
  if FIsClearing then
  begin
    Result := TRectF.Create(ImageItem.CurrentRect);
    Exit;
  end;
  // ==========================================
  // 2. COPY CURRENT STATE TO LOCAL
  // ==========================================
  if IsRectEmpty(ImageItem.CurrentRect) then
    BaseRect := ImageItem.TargetRect
  else
    BaseRect := ImageItem.CurrentRect;
  CenterX := (BaseRect.Left + BaseRect.Right) / 2;
  CenterY := (BaseRect.Top + BaseRect.Bottom) / 2;
  BaseW := BaseRect.Width;
  BaseH := BaseRect.Height;
  ZoomFactor := ImageItem.FHotZoom;
  // 3. CALCULATE VISUAL ANGLE (Sync with Draw)
  // We MUST use the same angle as Draw (ActualRotation + Wobble) to calculate corners correctly.
  VisualAngle := ImageItem.FActualRotation;
  if FBreathingEnabled and BreathRotationEnabled and (Abs(VisualAngle) > 0.1) then
    VisualAngle := VisualAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
  // 4. CALCULATE UNBOUNDED ZOOMED RECT
  if Abs(ZoomFactor - 1.0) > 0.01 then
  begin
    NewW := BaseW * ZoomFactor;
    NewH := BaseH * ZoomFactor;
    Result := TRectF.Create(CenterX - NewW / 2, CenterY - NewH / 2, CenterX + NewW / 2, CenterY + NewH / 2);
  end
  else
    Result := BaseRect;
  // ==========================================================
  // 5. "HIT THE WALLS" LOGIC (Iterative Anchoring)
  // ==========================================================
  // We shift the image in steps to ensure all corners stay inside.
  // 1. Check Left/Right, Shift X.
  // 2. Recalculate Corners.
  // 3. Check Top/Bottom, Shift Y.
  // 4. Recalculate Corners.
  // --- CHECK LEFT WALL ---
  UpdateCorners;
  if MinX < 0 then
  begin
    ShiftX := -MinX; // Shift Right
    Result.Offset(ShiftX, 0);
  end;
  // --- CHECK TOP WALL ---
  UpdateCorners; // Re-calc because X shifted
  if MinY < 0 then
  begin
    ShiftY := -MinY; // Shift Down
    Result.Offset(0, ShiftY);
  end;
  // --- CHECK RIGHT WALL ---
  UpdateCorners; // Re-calc because Y shifted
  if MaxX > Width then
  begin
    ShiftX := Width - MaxX; // Shift Left
    // Safety: If image is wider than screen, we keep Left anchor (0).
    // We check current Left against 0. If Left < 0, we are wider than screen.
    // We don't force shift. We just let it clip (VCL behavior).
    if (Result.Left + ShiftX) >= 0 then
      Result.Offset(ShiftX, 0);
  end;
  // --- CHECK BOTTOM WALL ---
  UpdateCorners; // Re-calc because X shifted
  if MaxY > Height then
  begin
    ShiftY := Height - MaxY; // Shift Up
    // Safety: If image is taller than screen, we keep Top anchor (0).
    if (Result.Top + ShiftY) >= 0 then
      Result.Offset(0, ShiftY);
  end;
  // Result is the final Clamped Visual Rect
end;
 }

       //without rotataion calculated in:

// -----------------------------------------------------------------------------
// GetVisualRect
// Returns the Visual Rect of an image.
// IMPORTANT: Bypasses Wall Sliding if FIsZoomedToFill is True.
// -----------------------------------------------------------------------------

function TSkFlowmotion.GetVisualRect(ImageItem: TImageItem): TRectF;
var
  BaseRect: TRectF;
  CenterX, CenterY, BaseW, BaseH, NewW, NewH: Single;
  ZoomFactor: Double;
  ShiftX, ShiftY: Single;
begin
  if not ImageItem.Visible then
    Exit;

  // ==========================================================
  // BYPASS WALL SLIDING DURING SPECIAL STATES
  // ==========================================================
  // 1. Clearing: No physics
  if FIsClearing then
  begin
    Result := TRectF.Create(ImageItem.CurrentRect);
    Exit;
  end;

  // 2. Zoomed To Fill: Use target directly (Centered/Anchored logic handled in ZoomSelectedToFull)
  if FIsZoomedToFill then
  begin
    Result := TRectF.Create(ImageItem.CurrentRect);

    // === HOVER ALIVE CONTROL FOR FULLSCREEN ===
    // If this is the Selected Image (Fullscreen) and HoverAliveOnFullscreen is OFF,
    // we do NOT apply the hover offset (Image stays still).
    if FHoverAlive then
    begin
      if (ImageItem = FSelectedImage) and (not FHoverAliveOnFullscreen) then
        // Skip offset -> Selected Image stays perfectly still

      else
        // Apply offset -> Normal Hover (Background images or Enabled property)
        Result.Offset(ImageItem.FHoverX, ImageItem.FHoverY);
    end;
    Exit;
  end;

  // ==========================================================
  // 1. CALCULATE BASE DIMENSIONS
  // ==========================================================
  if (ImageItem.CurrentRect.Right > ImageItem.CurrentRect.Left) and (ImageItem.CurrentRect.Bottom > ImageItem.CurrentRect.Top) then
  begin
    BaseRect := TRectF.Create(ImageItem.CurrentRect);
    CenterX := (BaseRect.Left + BaseRect.Right) / 2;
    CenterY := (BaseRect.Top + BaseRect.Bottom) / 2;
    BaseW := BaseRect.Width;
    BaseH := BaseRect.Height;
  end
  else
  begin
    BaseRect := TRectF.Create(ImageItem.TargetRect);
    CenterX := (BaseRect.Left + BaseRect.Right) / 2;
    CenterY := (BaseRect.Top + BaseRect.Bottom) / 2;
    BaseW := BaseRect.Width;
    BaseH := BaseRect.Height;
  end;

  // ==========================================================
  // 2. CALCULATE ZOOM FACTOR & STRICT FITTING
  // ==========================================================
  ZoomFactor := ImageItem.FHotZoom;

  // Calculate intended size
  NewW := BaseW * ZoomFactor;
  NewH := BaseH * ZoomFactor;

  // ==========================================================
  // RESTRICT MAX SIZE TO SCREEN
  // ==========================================================
  // If the intended size is wider than screen, shrink ZoomFactor.
  if NewW > Self.Width then
    ZoomFactor := Self.Width / BaseW;

  // If the intended size is taller than screen, shrink ZoomFactor.
  // Also ensure we don't blow up Width while shrinking Height (Min logic).
  if NewH > Self.Height then
  begin
    ZoomFactor := Min(ZoomFactor, Self.Height / BaseH);
  end;

  // Recalculate strict size
  NewW := BaseW * ZoomFactor;
  NewH := BaseH * ZoomFactor;

  // Create Unbounded Rect
  Result := TRectF.Create(CenterX - NewW / 2, CenterY - NewH / 2, CenterX + NewW / 2, CenterY + NewH / 2);

  // ==========================================================
  // 3. WALL SLIDING / ANCHORING
  // ==========================================================
  // Because NewW <= Width and NewH <= Height (due to fix above),
  // we only need to check if the *position* (Center) places it off-screen.
  // We shift it back inside.

  ShiftX := 0;
  ShiftY := 0;

  // Anchor Left (Don't go < 0)
  if Result.Left < 0 then
    ShiftX := -Result.Left;

  // Anchor Top (Don't go < 0)
  if Result.Top < 0 then
    ShiftY := -Result.Top;

  // Anchor Right (Don't go > Width)
  // Only shift if it doesn't conflict with Left anchor
  if Result.Right > Self.Width then
    ShiftX := Self.Width - Result.Right;

  // Anchor Bottom (Don't go > Height)
  // Only shift if it doesn't conflict with Top anchor
  if Result.Bottom > Self.Height then
    ShiftY := Self.Height - Result.Bottom;

  // Apply calculated shifts
  if (ShiftX <> 0) or (ShiftY <> 0) then
    Result.Offset(ShiftX, ShiftY);

  // ==========================================================
  // APPLY HOVER OFFSET (Micro-Movement)
  // ==========================================================
  if FHoverAlive then
  begin
    Result.Offset(ImageItem.FHoverX, ImageItem.FHoverY);
  end;
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

procedure TSkFlowmotion.AddActivationZone(const AName: string; const ARect: TRectF);
begin
  SetLength(FActivationZones, Length(FActivationZones) + 1);
  FActivationZones[High(FActivationZones)].Name := AName;
  FActivationZones[High(FActivationZones)].Rect := ARect;
end;

procedure TSkFlowmotion.ClearActivationZones;
begin
  SetLength(FActivationZones, 0);
end;
// -----------------------------------------------------------------------------
// INTERNAL METHODS: IMAGE CONVERSION HELPERS
// -----------------------------------------------------------------------------
 {     //for later use
function TSkFlowmotion.SkImageToBitmap(Img: ISkImage): TBitmap;
var
  MS: TMemoryStream;
begin
  Result := nil;
  if not Assigned(Img) then
    Exit;
  MS := TMemoryStream.Create;
  try
    // Encode the Skia image to a Stream
    // Using PNG format ensures transparency (Alpha) is preserved
    Img.EncodeToStream(MS, TSkEncodedImageFormat.PNG, 80);
    MS.Position := 0;
    // Create FMX Bitmap and load from the stream
    Result := TBitmap.Create;
    Result.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;
}

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
    Result := TSkImage.MakeFromEncodedStream(MS);
  finally
    MS.Free;
  end;
end;

function TSkFlowmotion.ResizeImageIfNeeded(const Source: ISkImage): ISkImage;
var
  Info: TSkImageInfo;
  Surface: ISkSurface;
  NewW, NewH: Integer;
  Scale: Single;
begin
  Result := Source;

  // 1. Safety checks
  if not Assigned(Source) then
    Exit;
  if FMaxInternalPicSize <= 0 then
    Exit; // 0 means "Keep Original"

  // 2. Check if resizing is needed
  if (Source.Width <= FMaxInternalPicSize) and (Source.Height <= FMaxInternalPicSize) then
    Exit;

  // 3. Calculate new dimensions preserving Aspect Ratio
  if Source.Width > Source.Height then
    Scale := FMaxInternalPicSize / Source.Width
  else
    Scale := FMaxInternalPicSize / Source.Height;

  NewW := Round(Source.Width * Scale);
  NewH := Round(Source.Height * Scale);

  // 4. Create a temporary Surface to draw the scaled version
  // We use RGBA8888 and Premul Alpha which is standard for FMX/Skia
  Info := TSkImageInfo.Create(NewW, NewH, TSkColorType.RGBA8888, TSkAlphaType.Premul);
  Surface := TSkSurface.MakeRaster(Info);

  if Assigned(Surface) then
  begin
    Surface.Canvas.Clear(TAlphaColors.Null);
    // High quality filter ensures the resized picture looks good
    // Change to TSkSamplingOptions.Medium if loading is still too slow
    Surface.Canvas.DrawImageRect(Source, RectF(0, 0, Source.Width, Source.Height), RectF(0, 0, NewW, NewH), TSkSamplingOptions.High);

    // 5. Return the new, smaller image
    Result := Surface.MakeImageSnapshot;
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
  if (FSmallPicImageList = nil) then
    Exit;
  if (Index < 0) or (Index >= FSmallPicImageList.Count) then
    Exit;
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
  FIsZoomedToFill := False;
  FOnFullscreenEnterFired := False;
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
  i, StartIdx, EndIdx: Integer;
  ImageItem: TImageItem;
  FileName: string;
  StartTime: Cardinal;
begin
  if (not visible) or (FAllFiles.Count = 0) then
    Exit;
  if FisClearing or FPageChangeInProgress then
    Exit;

  StopAnimationThread;
  FPageChangeInProgress := True;
  try
    { --- 1. CLEANUP PREVIOUS PAGE --- }
    // We must explicitly free the ImageItem objects
    for i := 0 to FImages.Count - 1 do
      TImageItem(FImages[i]).Free;
    FImages.Clear;

    FSelectedImage := nil;
    FCurrentSelectedIndex := -1;

    { --- 2. STOP AND CLEANUP LOADING THREADS --- }
    // First, tell all threads to stop
    for i := 0 to FLoadingThreads.Count - 1 do
      TImageLoadThread(FLoadingThreads[i]).Terminate;

    // Wait a brief moment for threads to finish naturally
    StartTime := GetTickCount;
    while (FLoadingThreads.Count > 0) and ((GetTickCount - StartTime) < 500) do
    begin
      CheckSynchronize(10);
      Sleep(10);
    end;

    // Free any thread objects that are still alive
    for i := 0 to FLoadingThreads.Count - 1 do
    begin
      try
        TImageLoadThread(FLoadingThreads[i]).Free;
      except
        // Ignore errors if thread is already dead
      end;
    end;
    FLoadingThreads.Clear;
    FLoadingCount := 0;

    { --- 3. LOAD NEW PAGE --- }
    StartIdx := Page * FPageSize;
    EndIdx := Min(StartIdx + FPageSize, FAllFiles.Count) - 1;
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

        { Retrieve SmallPic Index from Master List }
        if i < FAllSmallPicIndices.Count then
          ImageItem.SmallPicIndex := Integer(FAllSmallPicIndices[i])
        else
          ImageItem.SmallPicIndex := -1;

        ImageItem.Visible := False;
        FImages.Add(ImageItem);
      end;
    end;

    { --- 4. RECALCULATE AND ANIMATE --- }
    CalculateLayout;
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

procedure TSkFlowmotion.NextPage;
begin
  if FInFallAnimation or FPageChangeInProgress then
    Exit;
  if FCurrentPage < GetPageCount - 1 then
    ShowPage(FCurrentPage + 1);
end;

procedure TSkFlowmotion.PrevPage;
begin
  if FInFallAnimation or FPageChangeInProgress then
    Exit;
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

procedure TSkFlowmotion.FreeAllImagesAndClearLists;
var
  i: Integer;
begin
  FisClearing := True;
  try
    { Free Visual Items }
    for i := 0 to FImages.Count - 1 do
      TImageItem(FImages[i]).Free;
    FImages.Clear;

    { Clear Master Lists }
    FAllFiles.Clear;
    FAllCaptions.Clear;
    FAllPaths.Clear;
    FAllHints.Clear;
    FAllSmallPicIndices.Clear;
    FAllInfoTexts.Clear; // <--- ADD THIS

    { Reset State }
    FHotItem := nil;
    FSelectedImage := nil;
    FWasSelectedItem := nil;
    FCurrentSelectedIndex := -1;
    FCurrentPage := 0;
    FBreathingPhase := 0.0;
    FRotatingImage := nil;
    FIsRotating := False;
    FDraggedImage := nil;
    FDraggingImage := False;
    FDraggingSelected := False;
    FIsMouseOverHandle := False;
  finally
    FisClearing := False;
    FInFallAnimation := False;
    //StopAnimationThread;
    Repaint;
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
  StartAngleCalc: Single;
begin
  if ImageItem = nil then
    Exit;

  { Determine target position }
  if UseSavedPosition then
    Target := SavedTargetRect
  else
    Target := ImageItem.TargetRect;

  W := Target.Right - Target.Left;
  H := Target.Bottom - Target.Top;

  EffectiveStyle := EntryStyle;
  if EffectiveStyle = iesRandom then
    EffectiveStyle := TImageEntryStyle(Random(8) + 1); // 1..8 = the eight side directions

  { Calculate Start Position based on animation style }
  case EffectiveStyle of
    iesFromLeft:
      begin
        StartX := -W - 100; // far outside left
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromRight:
      begin
        StartX := trunc(Width) + 100; // far outside right
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromTop:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := -H - 100; // far above
      end;
    iesFromBottom:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := trunc(Height) + 100; // far below
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
    iesFromCenter:
      begin
        CenterX := Target.Left + (Target.Right - Target.Left) div 2;
        CenterY := Target.Top + (Target.Bottom - Target.Top) div 2;
        StartX := CenterX;
        StartY := CenterY;
      end;
    iesFromPoint:
      begin
        CenterX := FEntryPoint.X;
        CenterY := FEntryPoint.Y;
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
    ImageItem.StartRect := Rect(StartX, StartY, StartX, StartX) // single pixel
  else
    ImageItem.StartRect := Rect(StartX + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * (1 - START_SCALE) / 2), StartX + Round(W * START_SCALE) + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * START_SCALE) + Round(H * (1 - START_SCALE) / 2));

  { Set Animation State }
  ImageItem.CurrentRect := ImageItem.StartRect;
  ImageItem.FHotZoom := 1.0;
  ImageItem.FHotZoomTarget := 1.0;
  ImageItem.TargetRect := Target;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;

  // We ONLY set TargetAlpha. We DO NOT force Alpha to 0.
  // This allows the animation thread to handle the transition smoothly
  // from the current Alpha (e.g., START_ALPHA) to TargetAlpha.
  // ImageItem.Alpha := 0;  <--- REMOVED THIS LINE
  ImageItem.TargetAlpha := FAlphaStatic;    // Ensure we fade in to static alpha

  { === APPLY STARTING ROTATION === }
  // Logic to handle Fixed Angle vs Random Angle
  // Case 1: User wants a specific fixed angle (FStartingAngle is not -1)
  if FStartingAngle <> -1 then
  begin
    StartAngleCalc := FStartingAngle;
  end
  else
  begin
    // Case 2: User wants random angle (FStartingAngle is -1)
    // We generate a random value.
    StartAngleCalc := (Random - 0.5) * 20;
  end;
  // We must set BOTH properties.
  // ActualRotation: Where the image is NOW (at the start of animation).
  // TargetRotation: Where the smoothing engine thinks it SHOULD BE.
  // If Target is 0 and Actual is Random, the engine fights you.
  ImageItem.FActualRotation := StartAngleCalc;
  ImageItem.FTargetRotation := StartAngleCalc;
end;

procedure TSkFlowmotion.StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
var
  CenterX, CenterY: Integer;
  ImageSize: TSize;
  // NEW: Vars for Dynamic Clamping
  DynMaxW, DynMaxH: Integer;
  MaxLimitW, MaxLimitH: Integer;
begin
  if ImageItem = nil then
    Exit;
  ImageItem.ZoomProgress := 0;
  ImageItem.Animating := True;
  ImageItem.StartRect := ImageItem.CurrentRect;

  // ==========================================================
  // DYNAMIC SIZE LIMITING (80% Screen Rule)
  // ==========================================================
  if Assigned(ImageItem.SkImage) then
  begin
    // 1. Calculate Dynamic Limit (80% of Screen)
    DynMaxW := Trunc(Self.Width * 0.8);
    DynMaxH := Trunc(Self.Height * 0.8);

    // 2. Take the smaller of Property(FMaxZoomSize) and 80% Screen
    MaxLimitW := Min(FMaxZoomSize, DynMaxW);
    MaxLimitH := Min(FMaxZoomSize, DynMaxH);

    // 3. Calculate Target Size respecting this limit
    ImageSize := GetOptimalSize(ImageItem.SkImage.Width, ImageItem.SkImage.Height, MaxLimitW, MaxLimitH);
  end
  else
  begin
    ImageSize.cx := ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left;
    ImageSize.cy := ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top;
  end;

  // === ZOOM OUT LOGIC ===
  if not ZoomIn then
  begin
    // In FreeFloat, restore to small grid position
    // In Sorted, trigger Layout to recalculate
    if FFlowLayout = flFreeFloat then
      ImageItem.TargetRect := ImageItem.OriginalTargetRect
    else
      // === ONLY RECALCULATE IF ZOOMING TO CENTER ===
      // If we are "Zooming In Place", we do NOT call CalculateLayout.
      // The TargetRect is already set to the grid cell (from previous load/click).
      // We simply shrink back to that TargetRect. Other images stay still.
      if FZoomSelectedtoCenter and FKeepSpaceforZoomed then
      CalculateLayout;
    Exit;
  end;

  // === ZOOM IN LOGIC ===
  if FFlowLayout = flFreeFloat then
    Exit;

  // === SORTED MODE ZOOM IN LOGIC (This runs ONLY if flSorted) ===
  if FZoomSelectedtoCenter then
  begin
    // === ZOOM TO CENTER (Normal Layout) ===
    // Uses the clamped ImageSize calculated above
    CenterX := (trunc(Width) - ImageSize.cx) div 2;
    CenterY := (trunc(Height) - ImageSize.cy) div 2;
    ImageItem.TargetRect := Rect(CenterX, CenterY, CenterX + ImageSize.cx, CenterY + ImageSize.cy);
  end
  else
  begin
    // === ZOOM IN PLACE (If Center Off / Normal Layout) ===
    CenterX := (ImageItem.StartRect.Left + ImageItem.StartRect.Right) div 2;
    CenterY := (ImageItem.StartRect.Top + ImageItem.StartRect.Bottom) div 2;
    ImageItem.TargetRect := Rect(CenterX - (ImageSize.cx div 2), CenterY - (ImageSize.cy div 2), CenterX + (ImageSize.cx div 2), CenterY + (ImageSize.cy div 2));
  end;

  { Recalculate layout so other images move out of center way }
  // === ONLY RECALCULATE IF ZOOMING TO CENTER ===
  if FZoomSelectedtoCenter and FKeepSpaceforZoomed then
    CalculateLayout;
end;
// -----------------------------------------------------------------------------
// VISUAL PAINT METHOD (SKIA RENDERING)
// -----------------------------------------------------------------------------

// Helper comparison function for sorting items by Z-Order (drawing priority)
// Used in the Paint/Draw method to determine the order in which items are rendered.
// Higher return value means "A should be drawn after B" (A on top).

function CompareZOrder(A, B: Pointer): Integer;
var
  Zoom1, Zoom2: Single;
  Area1, Area2: Int64;
  Y1, Y2: Single;          // Center Y-position used as tertiary sort key
  Rect1, Rect2: TRectF;
  Img1, Img2: TImageItem;
begin
  Img1 := TImageItem(A);
  Img2 := TImageItem(B);

  // 1. Primary sort key: Zoom/HotZoom factor (highest zoom drawn on top)
  Zoom1 := Img1.FHotZoom;
  Zoom2 := Img2.FHotZoom;

  if Zoom1 <> Zoom2 then
  begin
    // Higher zoom wins → return positive if Img1 is more zoomed
    Result := IfThen(Zoom1 > Zoom2, 1, -1);
    Exit;
  end;

  // 2. Secondary sort key: Visible area size (larger area drawn on top)
  // Prefer CurrentRect if available (during animation/drag), otherwise fall back to TargetRect
  if not IsRectEmpty(Img1.CurrentRect) then
    Rect1 := Img1.CurrentRect
  else
    Rect1 := Img1.TargetRect;

  if not IsRectEmpty(Img2.CurrentRect) then
    Rect2 := Img2.CurrentRect
  else
    Rect2 := Img2.TargetRect;

  Area1 := Trunc(Rect1.Width * Rect1.Height);
  Area2 := Trunc(Rect2.Width * Rect2.Height);

  if Area1 <> Area2 then
  begin
    Result := IfThen(Area1 > Area2, 1, -1);
    Exit;
  end;

  // 3. Tertiary sort key: Vertical center position (higher Y = further back / lower on screen)
  // This gives a natural "3D perspective" feel: items lower on screen appear closer
  Y1 := Rect1.Top + Rect1.Height / 2;  // Center Y of item 1
  Y2 := Rect2.Top + Rect2.Height / 2;  // Center Y of item 2

  if Abs(Y1 - Y2) > 1.0 then  // Small tolerance to avoid floating-point noise
  begin
    Result := IfThen(Y1 > Y2, 1, -1);  // Higher Y (lower on screen) = drawn later (closer)
    Exit;
  end;

  // 4. Final fallback: Master image index (stable order, prevents z-fighting/flicker)
  // Items with higher index are drawn later (on top) when everything else is equal
  if Img1.FImageIndex <> Img2.FImageIndex then
  begin
    Result := IfThen(Img1.FImageIndex > Img2.FImageIndex, 1, -1);
    Exit;
  end;

  // Equal in all criteria → same Z-order
  Result := 0;
end;

// -----------------------------------------------------------------------------
// DRAW FLUID INFO (Main Function)
// -----------------------------------------------------------------------------
procedure TSkFlowmotion.DrawFluidInfo(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas);
var
  LocalBmp: TBitmap;
  InfoLines: TStringList;
  CurrentLine, CurrentWord: string;
  CharI: Integer;
  CleanInfoText: string;
  LineWidth: Single;
  MaxCaptionWidth: Integer;
  LineSpacing: Single;
  LineTextWidth, TextDrawX, DrawY: Single;
  ZoomFactor, Amplitude, WaveX, WaveY, CenterX, CenterY, BreathingPulse: Single;
  TotalImageWidth, TotalImageHeight, TargetPanelWidth: Single;
  // NEW: Vars for Geometry Calculation
  TargetPanelL, TargetPanelR, TargetPanelT, TargetPanelB: Single;
  TargetPanelW, TargetPanelH: Single;
  CurrentPanelL, CurrentPanelR, CurrentPanelT, CurrentPanelB: Single;
  CurrentPanelW, CurrentPanelH: Single;
  // Local Paint/Font Objects
  LPaint: ISkPaint;
  LSkFont: ISkFont;
  LSkStyle: TSkFontStyle;
  // Vars for layout constraints
  ActualDirection: TInfoPanelDirection;
  // Resolution Helper

  function IsWide: Boolean;
  begin
    Result := (TotalImageWidth > TotalImageHeight);
  end;

begin
  if not Assigned(AItem) then
    Exit;

  if AItem.FInfoProgress <= 0.01 then
    Exit;

  // 1. Create Paint & Font
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  LSkStyle := TSkFontStyle.Normal;
  if TFontStyle.fsBold in FInfoFont.Style then
    LSkStyle := TSkFontStyle.Bold;
  if TFontStyle.fsItalic in FInfoFont.Style then
    LSkStyle := TSkFontStyle.Italic;
  LSkFont := TSkFont.Create(TSkTypeface.MakeFromName(FInfoFont.Family, LSkStyle), FInfoFont.Size);

  // 2. Setup Memory Objects
  LocalBmp := nil;
  InfoLines := nil;

  try
    LocalBmp := TBitmap.Create;
    LocalBmp.Canvas.Font.Family := FInfoFont.Family;
    LocalBmp.Canvas.Font.Size := FInfoFont.Size;
    LocalBmp.Canvas.Font.Style := FInfoFont.Style;

    // MANUAL TEXT PARSING
    CleanInfoText := AItem.FInfoText;
    CleanInfoText := StringReplace(CleanInfoText, '\n', #10, [rfReplaceAll]);

    InfoLines := TStringList.Create;
    LineSpacing := FInfoFont.Size * 1.4;

    // ==========================================================
    // CALCULATE TARGET DIMENSIONS (W and H)
    // ==========================================================
    TotalImageWidth := VisualRect.Right - VisualRect.Left;
    TotalImageHeight := VisualRect.Bottom - VisualRect.Top;

    // 1. Determine Direction & Resolve Auto
    ActualDirection := FInfoPanelDirection;
    if ActualDirection = ipdAuto then
    begin
      if IsWide then
        ActualDirection := ipdRight
      else
        ActualDirection := ipdBottom;
    end;

    // 2. Calculate Target Width and Height
    // NOTE: ipdLeft/Right are vertical strips. ipdTop/Bottom are horizontal strips.
    // Vertical strips use Image Width (Full height, % width).
    // Horizontal strips use Image Height (Full width, % height).

    case ActualDirection of
      ipdLeft, ipdRight:
        begin
          TargetPanelW := TotalImageWidth * FInfoPanelWidthPercent; // % of Width
          TargetPanelH := TotalImageHeight; // Full Height
        end;
      ipdTop, ipdBottom:
        begin
          TargetPanelW := TotalImageWidth; // Full Width
          TargetPanelH := TotalImageHeight * FInfoPanelWidthPercent; // % of Height
        end;
    end;

    if TargetPanelW < 10 then
      TargetPanelW := 10;
    if TargetPanelH < 10 then
      TargetPanelH := 10;

    // 3. Calculate Max Text Width (Based on Target Panel Width)
    // Margin: 20px left, 20px right. Total 40px.
    MaxCaptionWidth := Trunc(TargetPanelW - 40);
    if MaxCaptionWidth < 10 then
      MaxCaptionWidth := 10;

    // ==========================================================
    // MANUAL TEXT PARSING (CHARACTER BY CHARACTER)
    // ==========================================================
    CurrentLine := '';
    CurrentWord := '';
    CharI := 1;

    while CharI <= Length(CleanInfoText) do
    begin
      // 1. Check for NEW LINE CHARACTERS (\n)
      if CleanInfoText[CharI] = #10 then
      begin
        InfoLines.Add(CurrentLine);
        CurrentLine := '';
        Inc(CharI);
        Continue;
      end;

      // 2. Check for PIPE (|) -> Forces New Line
      if CleanInfoText[CharI] = '|' then
      begin
        InfoLines.Add(CurrentLine);
        CurrentLine := '';
        Inc(CharI);
        Continue;
      end;

      // 3. Check for SPACE
      if CleanInfoText[CharI] = ' ' then
      begin
        if CurrentWord <> '' then
        begin
          if CurrentLine = '' then
            LineWidth := Trunc(LocalBmp.Canvas.TextWidth(CurrentWord))
          else
            LineWidth := Trunc(LocalBmp.Canvas.TextWidth(CurrentLine + ' ' + CurrentWord));

          if LineWidth > MaxCaptionWidth then
          begin
            if CurrentLine <> '' then
              InfoLines.Add(CurrentLine);
            CurrentLine := CurrentWord;
          end
          else
          begin
            if CurrentLine = '' then
              CurrentLine := CurrentWord
            else
              CurrentLine := CurrentLine + ' ' + CurrentWord;
          end;
        end;
        CurrentWord := '';
        Inc(CharI);
        Continue;
      end;

      // 4. Normal Character
      CurrentWord := CurrentWord + CleanInfoText[CharI];
      Inc(CharI);

      // 5. End of String Check
      if CharI > Length(CleanInfoText) then
      begin
        if CurrentWord <> '' then
        begin
          if CurrentLine = '' then
            LineWidth := Trunc(LocalBmp.Canvas.TextWidth(CurrentWord))
          else
            LineWidth := Trunc(LocalBmp.Canvas.TextWidth(CurrentLine + ' ' + CurrentWord));

          if LineWidth > MaxCaptionWidth then
          begin
            if CurrentLine <> '' then
              InfoLines.Add(CurrentLine);
            CurrentLine := CurrentWord;
          end
          else
          begin
            if CurrentLine = '' then
              CurrentLine := CurrentWord
            else
              CurrentLine := CurrentLine + ' ' + CurrentWord;
          end;
        end;
      end;
    end;

    if CurrentLine <> '' then
      InfoLines.Add(CurrentLine);

    // ==========================================================
    // GEOMETRY CALCULATION (Slide In Logic)
    // ==========================================================
    ZoomFactor := Max(1.0, AItem.FHotZoom);
    Amplitude := 5.0 * ZoomFactor;
    BreathingPulse := 1.0 + (Sin(FBreathingPhase * 2 * PI) * 0.1);

    // 1. Determine Target Position (Based on Direction + NO PANEL OFFSETS)
    case ActualDirection of
      ipdLeft:
        begin
          TargetPanelL := VisualRect.Left;
          TargetPanelR := TargetPanelL + TargetPanelW;
          TargetPanelT := VisualRect.Top;
          TargetPanelB := VisualRect.Bottom;
        end;
      ipdRight:
        begin
          TargetPanelR := VisualRect.Right;
          TargetPanelL := TargetPanelR - TargetPanelW;
          TargetPanelT := VisualRect.Top;
          TargetPanelB := VisualRect.Bottom;
        end;
      ipdTop:
        begin
          TargetPanelT := VisualRect.Top;
          TargetPanelB := TargetPanelT + TargetPanelH;
          TargetPanelL := VisualRect.Left;
          TargetPanelR := VisualRect.Right;
        end;
      ipdBottom:
        begin
          TargetPanelB := VisualRect.Bottom;
          TargetPanelT := TargetPanelB - TargetPanelH;
          TargetPanelL := VisualRect.Left;
          TargetPanelR := VisualRect.Right;
        end;
    end;

    // 2. Determine Animated Position (Based on Progress)
    case ActualDirection of
      ipdLeft: // Expanding to Right (Fixed L)
        begin
          CurrentPanelL := TargetPanelL;
          CurrentPanelW := TargetPanelW * AItem.FInfoProgress;
          CurrentPanelR := CurrentPanelL + CurrentPanelW;
          CurrentPanelT := TargetPanelT;
          CurrentPanelB := TargetPanelB;
        end;
      ipdRight: // Expanding to Left (Fixed R)
        begin
          CurrentPanelR := TargetPanelR;
          CurrentPanelW := TargetPanelW * AItem.FInfoProgress;
          CurrentPanelL := TargetPanelR - CurrentPanelW;
          CurrentPanelT := TargetPanelT;
          CurrentPanelB := TargetPanelB;
        end;
      ipdTop: // Expanding Down (Fixed T)
        begin
          CurrentPanelT := TargetPanelT;
          CurrentPanelH := TargetPanelH * AItem.FInfoProgress;
          CurrentPanelB := CurrentPanelT + CurrentPanelH;
          CurrentPanelL := TargetPanelL;
          CurrentPanelR := TargetPanelR;
        end;
      ipdBottom: // Expanding Up (Fixed B)
        begin
          CurrentPanelB := TargetPanelB;
          CurrentPanelH := TargetPanelH * AItem.FInfoProgress;
          CurrentPanelT := TargetPanelB - CurrentPanelH;
          CurrentPanelL := TargetPanelL;
          CurrentPanelR := TargetPanelR;
        end;
    end;

    // ==========================================================
    // CALCULATE WAVE POSITIONS (For Blur Edge)
    // ==========================================================
    CenterX := (CurrentPanelL + CurrentPanelR) / 2;
    CenterY := (CurrentPanelT + CurrentPanelB) / 2;

    WaveX := CurrentPanelL; // Default
    WaveY := CurrentPanelT; // Default

    if Self.FInfoPanelStyle = iasBlurEdge then
    begin
      case ActualDirection of
        ipdLeft:
          WaveX := CurrentPanelR; // Right edge moving
        ipdRight:
          WaveX := CurrentPanelL; // Left edge moving
        ipdTop:
          WaveY := CurrentPanelB; // Bottom edge moving
        ipdBottom:
          WaveY := CurrentPanelT; // Top edge moving
      end;

      // Apply Sine wave to the relevant coordinate
      if ActualDirection in [ipdLeft, ipdRight] then
        WaveX := WaveX + Sin((CenterY * 0.05)) * Amplitude
      else
        WaveY := WaveY + Sin((CenterX * 0.05)) * Amplitude;
    end;

    // ==========================================================
    // 3. Draw based on Style
    // ==========================================================
    // REMOVE MAXLINES CALC. PASS FULL COUNT.
    LPaint.ImageFilter := nil;
    LPaint.Style := TSkPaintStyle.Fill;
    LPaint.AntiAlias := True;

    var TotalLines: Integer;
    TotalLines := InfoLines.Count;

    if Self.FInfoPanelStyle = iasStatic then
      DrawFluidInfo_Static(AItem, VisualRect, ACanvas, LocalBmp, CurrentPanelL, CurrentPanelR, CurrentPanelT, CurrentPanelB, BreathingPulse, ZoomFactor, InfoLines, LineSpacing, LPaint, LSkFont, TotalLines, ActualDirection)
    else if Self.FInfoPanelStyle = iasBlurEdge then
      DrawFluidInfo_BlurEdge(AItem, VisualRect, ACanvas, LocalBmp, CurrentPanelL, CurrentPanelR, CurrentPanelT, CurrentPanelB, WaveX, WaveY, BreathingPulse, ZoomFactor, InfoLines, LineSpacing, LPaint, LSkFont, TotalLines, ActualDirection)
    else if Self.FInfoPanelStyle = iasTransparent then
      DrawFluidInfo_Transparent(AItem, VisualRect, ACanvas, LocalBmp, CurrentPanelL, CurrentPanelR, CurrentPanelT, CurrentPanelB, BreathingPulse, ZoomFactor, InfoLines, LineSpacing, LPaint, LSkFont, TotalLines, ActualDirection);

  finally
    if Assigned(LocalBmp) then
      LocalBmp.Free;
    if Assigned(InfoLines) then
      InfoLines.Free;
  end;
end;
// -----------------------------------------------------------------------------
// DRAW FLUID INFO: TRANSPARENT STYLE (Helper) - LARGER MARGIN
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.DrawFluidInfo_Transparent(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas; LocalBmp: TBitmap; const CurrentPanelLeft, CurrentPanelRight, CurrentPanelTop, CurrentPanelBottom: Single; const BreathingPulse: Single; const ZoomFactor: Single; const InfoLines: TStringList; const LineSpacing: Single; const Paint: ISkPaint; const SkFont: ISkFont; const MaxLines: Integer; const ActualDirection: TInfoPanelDirection);
var
  StripRect: TRectF;
  PanelAlpha: Single;
  i: Integer;
  TextDrawX, DrawY, LineTextWidth: Single;
  TextHeight: Single;
  LocalPaint: ISkPaint;
  ActualMaxLines: Integer;
  BaseMargin: Single; // <--- ONLY THIS
begin
  ACanvas.Save;
  ACanvas.ClipRect(TRectF.Create(CurrentPanelLeft, CurrentPanelTop, CurrentPanelRight, CurrentPanelBottom));

  LocalPaint := TSkPaint.Create(Paint);

  LocalPaint.Style := TSkPaintStyle.Fill;
  LocalPaint.Color := TAlphaColors.Black;
  LocalPaint.ImageFilter := nil;

  PanelAlpha := 110.0 * BreathingPulse * ZoomFactor;
  if PanelAlpha > 180.0 then
    PanelAlpha := 180.0;
  if PanelAlpha < 10 then
    PanelAlpha := 10;

  LocalPaint.Alpha := Round(PanelAlpha);

  StripRect := TRectF.Create(CurrentPanelLeft, CurrentPanelTop, CurrentPanelRight, CurrentPanelBottom);
  ACanvas.DrawRect(StripRect, LocalPaint);

  LocalPaint.Color := FInfoTextColor;
  LocalPaint.Style := TSkPaintStyle.Fill;
  LocalPaint.Alpha := 255;

  ActualMaxLines := Min(InfoLines.Count, MaxLines);

  // ==========================================================
  // MARGIN: 25 pixels down from top
  // ==========================================================
  BaseMargin := 25.0;

  for i := 0 to ActualMaxLines - 1 do
  begin
    LineTextWidth := Trunc(LocalBmp.Canvas.TextWidth(InfoLines[i]));
    TextHeight := Trunc(LocalBmp.Canvas.TextHeight(InfoLines[i]));

    // TEXT ALIGNMENT
    if ActualDirection = ipdLeft then
      TextDrawX := CurrentPanelLeft + BaseMargin
    else if ActualDirection = ipdRight then
      TextDrawX := CurrentPanelLeft + BaseMargin
    else
      TextDrawX := ((CurrentPanelLeft + CurrentPanelRight) / 2) - (LineTextWidth / 2);

    // VERTICAL ALIGNMENT
    // Top, Bottom, Left, Right: Start at Top + Margin
    DrawY := (CurrentPanelTop + BaseMargin) + (i * LineSpacing);

    // CLIP CHECK
    if DrawY + TextHeight > CurrentPanelBottom then
      Break;

    ACanvas.DrawSimpleText(InfoLines[i], TextDrawX, DrawY, SkFont, LocalPaint);
  end;

  ACanvas.Restore;
end;
// -----------------------------------------------------------------------------
// DRAW FLUID INFO: STATIC STYLE (Helper) - LARGER MARGIN
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.DrawFluidInfo_Static(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas; LocalBmp: TBitmap; const CurrentPanelLeft, CurrentPanelRight, CurrentPanelTop, CurrentPanelBottom: Single; const BreathingPulse: Single; const ZoomFactor: Single; const InfoLines: TStringList; const LineSpacing: Single; const Paint: ISkPaint; const SkFont: ISkFont; const MaxLines: Integer; const ActualDirection: TInfoPanelDirection);
var
  StripRect: TRectF;
  PanelAlpha: Single;
  i: Integer;
  TextDrawX, DrawY, LineTextWidth: Single;
  TextHeight: Single;
  LocalPaint: ISkPaint;
  ActualMaxLines: Integer;
  BaseMargin: Single; // <--- ONLY THIS
begin
  ACanvas.Save;
  ACanvas.ClipRect(TRectF.Create(CurrentPanelLeft, CurrentPanelTop, CurrentPanelRight, CurrentPanelBottom));

  LocalPaint := TSkPaint.Create(Paint);

  LocalPaint.Style := TSkPaintStyle.Fill;
  LocalPaint.Color := TAlphaColors.Black;

  PanelAlpha := 240.0;
  LocalPaint.Alpha := Round(PanelAlpha);

  StripRect := TRectF.Create(CurrentPanelLeft, CurrentPanelTop, CurrentPanelRight, CurrentPanelBottom);
  ACanvas.DrawRect(StripRect, LocalPaint);

  LocalPaint.ImageFilter := nil;
  LocalPaint.Color := FInfoTextColor;
  LocalPaint.Style := TSkPaintStyle.Fill;
  LocalPaint.Alpha := 255;

  ActualMaxLines := Min(InfoLines.Count, MaxLines);

  // ==========================================================
  // MARGIN: 25 pixels down from top
  // ==========================================================
  BaseMargin := 25.0;

  for i := 0 to ActualMaxLines - 1 do
  begin
    LineTextWidth := Trunc(LocalBmp.Canvas.TextWidth(InfoLines[i]));
    TextHeight := Trunc(LocalBmp.Canvas.TextHeight(InfoLines[i]));

    if ActualDirection = ipdLeft then
      TextDrawX := CurrentPanelLeft + BaseMargin
    else if ActualDirection = ipdRight then
      TextDrawX := CurrentPanelLeft + BaseMargin
    else
      TextDrawX := ((CurrentPanelLeft + CurrentPanelRight) / 2) - (LineTextWidth / 2);

    DrawY := (CurrentPanelTop + BaseMargin) + (i * LineSpacing);

    if DrawY + TextHeight > CurrentPanelBottom then
      Break;

    ACanvas.DrawSimpleText(InfoLines[i], TextDrawX, DrawY, SkFont, LocalPaint);
  end;

  ACanvas.Restore;
end;
// -----------------------------------------------------------------------------
// DRAW FLUID INFO: BLUR EDGE STYLE (Helper) - LARGER MARGIN
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.DrawFluidInfo_BlurEdge(const AItem: TImageItem; const VisualRect: TRectF; ACanvas: ISkCanvas; LocalBmp: TBitmap; const CurrentPanelLeft, CurrentPanelRight, CurrentPanelTop, CurrentPanelBottom: Single; const WaveX, WaveY: Single; const BreathingPulse: Single; const ZoomFactor: Single; const InfoLines: TStringList; const LineSpacing: Single; const Paint: ISkPaint; const SkFont: ISkFont; const MaxLines: Integer; const ActualDirection: TInfoPanelDirection);
var
  Builder: ISkPathBuilder;
  Path: ISkPath;
  GlowPaint: ISkPaint;
  Filter: ISkImageFilter;
  StripRect: TRectF;
  StripAlpha, Dist, PanelAlpha: Single;
  i, X, Y: Integer;
  TextDrawX, DrawY, LineTextWidth: Single;
  TextHeight: Single;
  LocalPaint: ISkPaint;
  ActualMaxLines: Integer;
  BaseMargin: Single; // <--- ONLY THIS
begin
  ACanvas.Save;
  ACanvas.ClipRect(TRectF.Create(CurrentPanelLeft, CurrentPanelTop, CurrentPanelRight, CurrentPanelBottom));

  // 1. Draw Blur Edge (Line)
  Builder := TSkPathBuilder.Create;

  case ActualDirection of
    ipdLeft, ipdRight:
      begin
        Builder.MoveTo(WaveX, CurrentPanelTop);
        Builder.LineTo(WaveX, CurrentPanelBottom);
      end;
    ipdTop, ipdBottom:
      begin
        Builder.MoveTo(CurrentPanelLeft, WaveY);
        Builder.LineTo(CurrentPanelRight, WaveY);
      end;
  end;
  Path := Builder.Snapshot;

  GlowPaint := TSkPaint.Create;
  GlowPaint.Style := TSkPaintStyle.Stroke;
  GlowPaint.StrokeWidth := 15.0;
  GlowPaint.Color := TAlphaColors.Cyan;
  GlowPaint.Alpha := 120;
  Filter := TSkImageFilter.MakeBlur(20.0, 20.0, nil, TSkTileMode.Clamp);
  GlowPaint.ImageFilter := Filter;
  ACanvas.DrawPath(Path, GlowPaint);

  LocalPaint := TSkPaint.Create(Paint);
  LocalPaint.Style := TSkPaintStyle.Stroke;
  LocalPaint.StrokeWidth := 3.0;
  LocalPaint.Color := TAlphaColors.White;
  LocalPaint.Alpha := 180;
  Filter := TSkImageFilter.MakeBlur(4.0, 4.0, nil, TSkTileMode.Clamp);
  LocalPaint.ImageFilter := Filter;
  ACanvas.DrawPath(Path, LocalPaint);

  // 2. Draw Inside Panel
  Filter := TSkImageFilter.MakeBlur(2.0, 2.0, nil, TSkTileMode.Clamp);

  if Assigned(AItem.SkImage) then
  begin
    LocalPaint.ImageFilter := Filter;
    LocalPaint.Style := TSkPaintStyle.Fill;
    LocalPaint.Alpha := 255;
    LocalPaint.Color := TAlphaColors.White;
    ACanvas.DrawImageRect(AItem.SkImage, VisualRect, TSkSamplingOptions.High, LocalPaint);
  end;

  // 3. Draw Black Overlay (Gradient Fog)
  LocalPaint := TSkPaint.Create(Paint);
  LocalPaint.Style := TSkPaintStyle.Fill;
  LocalPaint.ImageFilter := nil;
  LocalPaint.Color := TAlphaColors.Black;

  if ActualDirection in [ipdLeft, ipdRight] then
  begin
    for X := Round(WaveX) to Round(CurrentPanelRight) do
    begin
      Dist := X - WaveX;
      if (Dist < 30) then
        StripAlpha := (Dist / 30.0) * BreathingPulse
      else
        StripAlpha := BreathingPulse;

      PanelAlpha := 255.0 * StripAlpha * ZoomFactor;
      if PanelAlpha > 255.0 then
        PanelAlpha := 255.0;
      if PanelAlpha < 0 then
        PanelAlpha := 0;

      LocalPaint.Alpha := Round(PanelAlpha);
      if LocalPaint.Alpha > 5 then
      begin
        StripRect := TRectF.Create(X, CurrentPanelTop, X + 4, CurrentPanelBottom);
        ACanvas.DrawRect(StripRect, LocalPaint);
      end;
    end;
  end
  else
  begin
    for Y := Round(WaveY) to Round(CurrentPanelBottom) do
    begin
      Dist := Y - WaveY;
      if (Dist < 30) then
        StripAlpha := (Dist / 30.0) * BreathingPulse
      else
        StripAlpha := BreathingPulse;

      PanelAlpha := 255.0 * StripAlpha * ZoomFactor;
      if PanelAlpha > 255.0 then
        PanelAlpha := 255.0;
      if PanelAlpha < 0 then
        PanelAlpha := 0;

      LocalPaint.Alpha := Round(PanelAlpha);
      if LocalPaint.Alpha > 5 then
      begin
        StripRect := TRectF.Create(CurrentPanelLeft, Y, CurrentPanelRight, Y + 4);
        ACanvas.DrawRect(StripRect, LocalPaint);
      end;
    end;
  end;

  // 4. Draw Text
  LocalPaint.ImageFilter := nil;
  LocalPaint.Color := FInfoTextColor;
  LocalPaint.Style := TSkPaintStyle.Fill;
  LocalPaint.Alpha := 255;

  ActualMaxLines := Min(InfoLines.Count, MaxLines);

  // ==========================================================
  // MARGIN: 30 pixels down from top (More for Blur)
  // ==========================================================
  BaseMargin := 30.0;

  for i := 0 to ActualMaxLines - 1 do
  begin
    LineTextWidth := Trunc(LocalBmp.Canvas.TextWidth(InfoLines[i]));
    TextHeight := Trunc(LocalBmp.Canvas.TextHeight(InfoLines[i]));

    if ActualDirection = ipdLeft then
      TextDrawX := CurrentPanelLeft + BaseMargin
    else if ActualDirection = ipdRight then
      TextDrawX := CurrentPanelLeft + BaseMargin
    else
      TextDrawX := ((CurrentPanelLeft + CurrentPanelRight) / 2) - (LineTextWidth / 2);

    DrawY := (CurrentPanelTop + BaseMargin) + (i * LineSpacing);

    if DrawY + TextHeight > CurrentPanelBottom then
      Break;

    ACanvas.DrawSimpleText(InfoLines[i], TextDrawX, DrawY, SkFont, LocalPaint);
  end;

  ACanvas.Restore;
end;

procedure TSkFlowmotion.DrawInfoIndicator(ACanvas: ISkCanvas; const VisualRect: TRectF; const AItem: TImageItem);
var
  Builder: ISkPathBuilder;
  Paint: ISkPaint;
  Apex, TL, BR: TPointF;
  FTempDirection: TInfoPanelDirection;
  EdgeMargin: Single;
begin
  // Master Switch
  if not FShowInfoIndicator then
    Exit;

  // Only draw if info exists
  if (AItem.InfoText = '') then
    Exit;

  // === FIXED VISIBILITY LOGIC ===
  // 1. If panel is OPEN (State = True), don't draw.
  if AItem.FIsInfoShowing then
    Exit;

  // 2. If panel is ANIMATING OUT (State = False, but Progress > 0), don't draw.
  // We use 0.01 as a threshold to ensure the panel is practically gone before showing the arrow.
  if AItem.FInfoProgress > 0.01 then
    Exit;

  // ==========================================================
  // DRAWING CODE (Only executes if Panel is fully closed)
  // ==========================================================

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;

  // >>> DYNAMIC COLOR LOGIC (Same as Rotate Handle) <<<
  if FIsMouseOverInfoIndicator then
    Paint.Color := FInfoIndicatorHotColor     // Hover (Aqua)
  else
    Paint.Color := FInfoIndicatorColor;       // Normal (Teal/Default)
  Paint.Alpha := 255; // Full Opacity
  Paint.AntiAlias := True;

  // --- CALCULATE DIRECTION ---
  if FInfoPanelDirection = ipdAuto then
  begin
    if VisualRect.Width > VisualRect.Height then
      FTempDirection := ipdRight
    else
      FTempDirection := ipdBottom;
  end
  else
    FTempDirection := FInfoPanelDirection;

  // Margin from edge
  EdgeMargin := 10;

  Builder := TSkPathBuilder.Create;

  case FTempDirection of
    ipdLeft: // Panel on Left. Button on Left Edge.
      begin
        Apex := TPointF.Create(VisualRect.Left + EdgeMargin, (VisualRect.Top + VisualRect.Bottom) / 2);
        TL := TPointF.Create(Apex.X - 10, Apex.Y - 30);
        BR := TPointF.Create(Apex.X - 10, Apex.Y + 30);
      end;
    ipdRight: // Panel on Right. Button on Right Edge.
      begin
        Apex := TPointF.Create(VisualRect.Right - EdgeMargin, (VisualRect.Top + VisualRect.Bottom) / 2);
        TL := TPointF.Create(Apex.X + 10, Apex.Y - 30);
        BR := TPointF.Create(Apex.X + 10, Apex.Y + 30);
      end;
    ipdTop: // Panel on Top. Button on Top Edge.
      begin
        Apex := TPointF.Create((VisualRect.Left + VisualRect.Right) / 2, VisualRect.Top + EdgeMargin);
        TL := TPointF.Create(Apex.X - 30, Apex.Y - 10);
        BR := TPointF.Create(Apex.X + 30, Apex.Y - 10);
      end;
    ipdBottom: // Panel on Bottom. Button on Bottom Edge.
      begin
        Apex := TPointF.Create((VisualRect.Left + VisualRect.Right) / 2, VisualRect.Bottom - EdgeMargin);
        TL := TPointF.Create(Apex.X - 30, Apex.Y + 10);
        BR := TPointF.Create(Apex.X + 30, Apex.Y + 10);
      end;
  end;

  Builder.MoveTo(Apex);
  Builder.LineTo(TL);
  Builder.LineTo(BR);
  Builder.Close;

  ACanvas.DrawPath(Builder.Snapshot, Paint);
end;

function TSkFlowmotion.GetInfoPanelRect(const AItem: TImageItem; const VisualRect: TRectF): TRectF;
var
  PanelW, PanelH: Single;
  ActDir: TInfoPanelDirection;
begin
  Result := RectF(0, 0, 0, 0);
  if (AItem.InfoText = '') then
    Exit;

  if FInfoPanelDirection = ipdAuto then
  begin
    if VisualRect.Width > VisualRect.Height then
      ActDir := ipdRight
    else
      ActDir := ipdBottom;
  end
  else
    ActDir := FInfoPanelDirection;

  case ActDir of
    ipdLeft:
      begin
        PanelW := VisualRect.Width * FInfoPanelWidthPercent;
        Result := RectF(VisualRect.Left, VisualRect.Top, VisualRect.Left + PanelW, VisualRect.Bottom);
      end;
    ipdRight:
      begin
        PanelW := VisualRect.Width * FInfoPanelWidthPercent;
        Result := RectF(VisualRect.Right - PanelW, VisualRect.Top, VisualRect.Right, VisualRect.Bottom);
      end;
    ipdTop:
      begin
        PanelH := VisualRect.Height * FInfoPanelWidthPercent;
        Result := RectF(VisualRect.Left, VisualRect.Top, VisualRect.Right, VisualRect.Top + PanelH);
      end;
    ipdBottom:
      begin
        PanelH := VisualRect.Height * FInfoPanelWidthPercent;
        Result := RectF(VisualRect.Left, VisualRect.Bottom - PanelH, VisualRect.Right, VisualRect.Bottom);
      end;
  end;
end;

procedure TSkFlowmotion.InitMatrix;
var
  i, j, ColsCount: Integer;
  SourceData: string;
begin
  // >>> EXIT IF NO IMAGES <<<
  if (Width <= 0) or (FImages.Count = 0) then
    Exit;

  if FMatrixFontSize <= 0 then
    ColsCount := Trunc(Width / 10)
  else
    ColsCount := Trunc(Width / (FMatrixFontSize * 0.6));

  if ColsCount < 1 then
    ColsCount := 1;
  if ColsCount > 300 then
    ColsCount := 300;

  SetLength(FMatrixCols, ColsCount);

  for i := 0 to ColsCount - 1 do
  begin
    FMatrixCols[i].X := i * (FMatrixFontSize * 0.6);
    FMatrixCols[i].Y := -Random(Trunc(Height * 2));
    FMatrixCols[i].Speed := 5.0 + Random(15);

    // >>> SAFE INDEX <<<
    // Only pick from valid range. Use Count - 1 as max index.
    if FImages.Count > 1 then
      FMatrixCols[i].TargetImageIndex := Random(FImages.Count - 1)
    else
      FMatrixCols[i].TargetImageIndex := 0;

    // <--- GET REAL LIVE DATA --->
    SourceData := GetLiveImageDataString(FMatrixCols[i].TargetImageIndex);

    // Determine Length based on data string (capped for visual sanity)
    FMatrixCols[i].Length := Length(SourceData);
    if FMatrixCols[i].Length > 30 then
      FMatrixCols[i].Length := 30; // Cap max length

    // Seed column with data
    FMatrixCols[i].Chars := '';
    for j := 1 to FMatrixCols[i].Length do
    begin
      // Fill initial string with data
      if j <= Length(SourceData) then
        FMatrixCols[i].Chars := FMatrixCols[i].Chars + SourceData[j]
      else
        FMatrixCols[i].Chars := FMatrixCols[i].Chars + ' '; // Pad with space
    end;
  end;
end;

function TSkFlowmotion.GetLiveImageDataString(ImgIdx: Integer): string;
var
  Img: TImageItem;
  CapStr: string;
begin
  Result := 'NO_DATA';

  // Safety check: Is index valid?
  if (ImgIdx < 0) or (ImgIdx >= FImages.Count) then
    Exit;

  // Safety check: Does image item exist?
  Img := TImageItem(FImages[ImgIdx]);
  if Img = nil then
    Exit;

  // 1. Prepare Caption
  CapStr := Img.Caption;
  if (CapStr = '') then
    CapStr := 'No Caption';

  // 2. Sanitize Caption
  // Remove newlines to keep it clean
  CapStr := StringReplace(CapStr, #13#10, '', [rfReplaceAll]);
  CapStr := StringReplace(CapStr, #10, '', [rfReplaceAll]);

  // Escape % so it displays as text instead of being a formatting issue
  CapStr := StringReplace(CapStr, '%', '%%', [rfReplaceAll]);

  // Truncate
  if Length(CapStr) > 20 then
    CapStr := Copy(CapStr, 1, 20);

  // 3. MANUAL STRING CONCATENATION (Safe & Fast)
  // We build the string piece by piece.
  // FloatToStrF(Val, ffFixed, 10, Digits) creates "1.50" style strings.

  Result := 'ID:' + IntToStr(Img.ImageIndex) + '|C_X:' + FloatToStrF(Img.CurrentRect.Left, ffFixed, 10, 0) + '|C_Y:' + FloatToStrF(Img.CurrentRect.Top, ffFixed, 10, 0) + '|T_X:' + FloatToStrF(Img.TargetRect.Left, ffFixed, 10, 0) + '|T_Y:' + FloatToStrF(Img.TargetRect.Top, ffFixed, 10, 0) + '|W:' + IntToStr(Img.TargetRect.Right - Img.TargetRect.Left) + '|H:' + IntToStr(Img.TargetRect.Bottom - Img.TargetRect.Top) + '|Z:' + FloatToStrF(Img.FHotZoom, ffFixed, 10, 2) + // 2 decimal places
    '|R:' + FloatToStrF(Img.ActualRotation, ffFixed, 10, 1) + // 1 decimal place
    '|A:' + IntToStr(Img.Alpha) + '|' + CapStr;
end;

procedure TSkFlowmotion.DrawMatrixBackground(const ACanvas: ISkCanvas);
var
  i, j: Integer;
  LPaint: ISkPaint;
  LSkFont: ISkFont;
  CharY: Single;
  CurrentChar: string;
  Img: TImageItem;
  TargetIdx: Integer; // Helper var for safety check
begin
  if not FAnimatedBackground then
    Exit;
  if (Length(FMatrixCols) = 0) and (FBackgroundEffect = beRealMatrix) then
    InitMatrix;
  if Length(FMatrixCols) = 0 then
    Exit;

  // Setup Paint
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Fill;

  // Setup Font
  LSkFont := TSkFont.Create(TSkTypeface.MakeFromName(FMatrixFont.Family, TSkFontStyle.Normal), FMatrixFontSize);

  // <--- ADD TRY/EXCEPT BLOCK --->
  try
    // Loop through every column
    for i := 0 to High(FMatrixCols) do
    begin
      // >>> VALIDATE INDEX BEFORE DRAWING <<<
      // Check if the stored index is within the valid range of FImages
      TargetIdx := FMatrixCols[i].TargetImageIndex;
      if (TargetIdx < 0) or (TargetIdx >= FImages.Count) then
        Continue; // Skip this column if index is invalid

      // Draw characters Vertically
      for j := 1 to Length(FMatrixCols[i].Chars) do
      begin
        // Calculate Y position for this specific character
        CharY := FMatrixCols[i].Y + ((j - 1) * FMatrixFontSize);

        // Optimization: Only draw if roughly on screen
        if (CharY > -FMatrixFontSize) and (CharY < Height + FMatrixFontSize) then
        begin
          CurrentChar := FMatrixCols[i].Chars[j];

          // CHECK: Is this the "Head" (The last character/bottom of stream)?
          if j = Length(FMatrixCols[i].Chars) then
          begin
            // HEAD: Bright Color, Full Opacity
            LPaint.Color := FMatrixHeadColor;
            LPaint.AlphaF := 1.0;
          end
          else
          begin
            // BODY: Dimmer Color, Some Transparency
            LPaint.Color := FMatrixColor;
            LPaint.AlphaF := 0.6;
          end;

          // Draw single character
          ACanvas.DrawSimpleText(CurrentChar, FMatrixCols[i].X, CharY, LSkFont, LPaint);
        end;
      end;
    end;
  except
    // --- CATCH ERROR ---
    // If anything crashes inside this specific column/drawing operation, we catch it here.
    // This prevents a crash from affecting the main images drawing.
  end;
end;

procedure TSkFlowmotion.DrawBackgroundEffects(const ACanvas: ISkCanvas; const ADest: TRectF; const Paint: ISkPaint);
var
  WaveX, WaveY: Single;
  RRect: TRectF;
begin
  case FBackgroundEffect of

    beHolographic:
      begin
        if Assigned(FBackgroundSkImage) then
        begin
          if FAnimatedBackground then
          begin
            Paint.Style := TSkPaintStyle.Fill;
            Paint.ImageFilter := nil;
            Paint.Alpha := 255;
            FGridOffsetY := FGridOffsetY + 1.0;
            if FGridOffsetY > 1000 then
              FGridOffsetY := 0;
            WaveX := Sin(FGridOffsetY * 0.02) * 10;
            WaveY := Sin(FGridOffsetY * 0.03) * 10;

            // LAYER 1 (Base)
            ACanvas.DrawImageRect(FBackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);

            // LAYER 2 (Offset 1)
            Paint.Alpha := 100;
            RRect := ADest;
            OffsetRect(RRect, WaveX, WaveY);
            ACanvas.DrawImageRect(FBackgroundSkImage, RRect, TSkSamplingOptions.High, Paint);

            // LAYER 3 (Offset 2)
            Paint.Alpha := 40;
            RRect := ADest;
            OffsetRect(RRect, -WaveX, -WaveY);
            ACanvas.DrawImageRect(FBackgroundSkImage, RRect, TSkSamplingOptions.High, Paint);
          end
          else
          begin
            // Static fallback if animation disabled
            ACanvas.DrawImageRect(FBackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);
          end;
        end;
      end;

    beRealMatrix:
      begin
        // 1. Draw static background image (Always visible)
        if Assigned(FBackgroundSkImage) then
        begin
          Paint.Style := TSkPaintStyle.Fill;
          Paint.ImageFilter := nil;
          Paint.Alpha := 255;
          ACanvas.DrawImageRect(FBackgroundSkImage, ADest, TSkSamplingOptions.High, Paint);
        end;

        // 2. Draw Matrix Rain ONLY if AnimatedBackground is TRUE
        if FAnimatedBackground then
        begin
          // Draw Matrix on top
          DrawMatrixBackground(ACanvas);
        end;
      end;
  end;
end;

procedure TSkFlowmotion.DrawImageWithEffect(const ACanvas: ISkCanvas; const ImageItem: TImageitem; const DstRect: TRectF; const BasePaint: ISkPaint);
var
  LPaint, LayerPaint: ISkPaint;
  ShadowFilter: ISkImageFilter;
  LayerBounds: TRectF;
  RR: ISkRoundRect;
  ShadowDx, ShadowDy, ShadowRad: Single;
  BaseOffset: Single;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  // ---------------------------------------------------------
  // 1. PREPARE LAYER (Required for clipping effects cleanly)
  // ---------------------------------------------------------
  LayerPaint := TSkPaint.Create;
  LayerPaint.Color := TAlphaColors.White;
  LayerPaint.Alpha := Round((FAlphaStatic / 255) * ImageItem.Alpha);

  LayerBounds := DstRect;
  LayerBounds.Inflate(25, 25);

  ACanvas.SaveLayer(LayerBounds, LayerPaint);

   // ---------------------------------------------------------
  // 2. PREPARE FILTER (Shadow/Glow/Ambient)
  // ---------------------------------------------------------

  // === DYNAMIC SCALING ===
  var ZoomFactor: Single;
  ZoomFactor := ImageItem.FHotZoom;

  // === CALCULATE ROTATED SHADOW OFFSET ===
  // To keep the light source static (e.g., Top-Left of screen) while
  // the image rotates, we must rotate the shadow offset in the opposite
  // direction of the image's rotation.
  BaseOffset := 5.0; // Standard offset (SHADOW_OFFSET_X)

  if ImageItem.ActualRotation <> 0 then
  begin
    // -ActualRotation ensures we rotate the vector opposite to image rotation
    ShadowRad := -ImageItem.ActualRotation * (PI / 180);
    ShadowDx := (BaseOffset * Cos(ShadowRad)) - (BaseOffset * Sin(ShadowRad));
    ShadowDy := (BaseOffset * Sin(ShadowRad)) + (BaseOffset * Cos(ShadowRad));
  end
  else
  begin
    ShadowDx := BaseOffset;
    ShadowDy := BaseOffset;
  end;

  // Apply Zoom factor to the rotated offset
  ShadowDx := ShadowDx * ZoomFactor;
  ShadowDy := ShadowDy * ZoomFactor;
  // === END ROTATED SHADOW CALCULATION ===

  case FSurfaceEffect of
    sueShadow:
      begin
        // USE CALCULATED ShadowDx/ShadowDy INSTEAD OF STATIC 5.0
        ShadowFilter := TSkImageFilter.MakeDropShadow(ShadowDx, ShadowDy, 10.0 * ZoomFactor, // SigmaX (Blur Radius)
          10.0 * ZoomFactor, // SigmaY (Blur Radius)
          TAlphaColors.Black, nil);

        LPaint.ImageFilter := ShadowFilter;
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.Color := TAlphaColors.Black;
        LPaint.Alpha := FAlphaStatic;
      end;

    sueGlow:
      begin
        ShadowFilter := TSkImageFilter.MakeDropShadow(ShadowDx, ShadowDy, 10.0 * ZoomFactor, 10.0 * ZoomFactor, FGlowColor, nil);

        LPaint.ImageFilter := ShadowFilter;
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.Color := FGlowColor;
        LPaint.Alpha := FAlphaStatic;
      end;

    sueAmbient:
      begin
        ShadowFilter := TSkImageFilter.MakeDropShadow(ShadowDx, ShadowDy, 10.0 * ZoomFactor, 10.0 * ZoomFactor, ImageItem.DominantColor, nil);

        LPaint.ImageFilter := ShadowFilter;
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.Color := ImageItem.DominantColor;
        LPaint.Alpha := FAlphaStatic;
      end;

    sueNone:
      begin
        LPaint.ImageFilter := nil;
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.Color := TAlphaColors.Black;
        LPaint.Alpha := FAlphaStatic;
      end;
  end;

  // ---------------------------------------------------------
  // 3. DRAW SHAPE (This creates the effect OUTSIDE)
  // ---------------------------------------------------------
  if FRoundEdges > 0 then
  begin
    RR := TSkRoundRect.Create(DstRect, FRoundEdges, FRoundEdges);
    // Only draw the shape if we have an effect, otherwise it's invisible/black
    if FSurfaceEffect <> sueNone then
      ACanvas.DrawRoundRect(RR, LPaint);

    // Clip to the rounded rect so image stays inside
    ACanvas.ClipRoundRect(RR, TSkClipOp.Intersect, True);
  end
  else
  begin
    // Square Logic
    if FSurfaceEffect <> sueNone then
      ACanvas.DrawRect(DstRect, LPaint);

    // Clip to the square rect
    ACanvas.ClipRect(DstRect, TSkClipOp.Intersect, True);
  end;

  // ---------------------------------------------------------
  // 4. DRAW CLEAN IMAGE (No filter applied here)
  // ---------------------------------------------------------
  LPaint.ImageFilter := nil;
  LPaint.Style := TSkPaintStyle.Fill;
  LPaint.Alpha := FAlphaStatic; // Restore alpha for the image content
  ACanvas.DrawImageRect(ImageItem.SkImage, DstRect, TSkSamplingOptions.High, LPaint);

  ACanvas.Restore;
end;

procedure TSkFlowmotion.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  VisRect: TRectF;
  ClipRR: TSkRoundRect;
  i, MaxLineWidth, LineTextWidth, HandleSize, Margin: Integer;
  ImageItem: TImageItem;
  Paint: ISkPaint;
  ShadowFilter: ISkImageFilter;
  CenterX, CenterY: Single;
  HandleRect: TRectF;
  VisualRect: TRectF;
  P: TParticle;
  ShadowRad, ShadowDx, ShadowDy: Single;
  // Z-Ordering Lists
  StaticImages: TList;
  AnimatingImages: TList;
  EnteringImages: TList;
  CurrentItem: TImageItem;
const
  SHADOW_OFFSET_X = 8.0;
  SHADOW_OFFSET_Y = 8.0;
  // --------------------------------------------------------------
  // Renders Caption with Word Wrapping & Alpha Background
  // --------------------------------------------------------------

  procedure DrawCaption(Item: TImageItem; const DrawRect: TRectF);
  var
    LocalBmp: TBitmap;
    Lines: TStringList;
    i, LineHeight, LineWidth: Integer;
    MaxCaptionWidth, MaxCaptionHeight: Integer;
    CurrentLine, Word: string;
    WordStart, WordEnd: Integer;
    ActualLinesToShow, MaxLinesToShow: Integer;
    TextRect, LineRect: TRectF;
    InflatedDrawRect: TRectF;
    CapTop, DrawX, DrawY: Single;
    SkFont: ISkFont;
    SkStyle: TSkFontStyle;
  begin
    if not FShowCaptions or (Item.Caption = '') then
      Exit;
    if FCaptionOnHoverOnly and (Item <> FHotItem) and (Item <> FSelectedImage) then
      Exit;

    LocalBmp := TBitmap.Create;
    try
      LocalBmp.Canvas.Font.Family := FCaptionFont.Family;
      LocalBmp.Canvas.Font.Size := FCaptionFont.Size;
      LocalBmp.Canvas.Font.Style := FCaptionFont.Style;

      MaxCaptionHeight := Trunc(LocalBmp.Canvas.TextHeight('Hg') * 1.4) + 12;
      MaxCaptionWidth := Trunc(DrawRect.Right - DrawRect.Left) - 30;
      if MaxCaptionWidth < 30 then
        MaxCaptionWidth := 40;

      Lines := TStringList.Create;
      CurrentLine := '';
      i := 1;
      while i <= Length(Item.Caption) do
      begin
        while (i <= Length(Item.Caption)) and (Item.Caption[i] = ' ') do
          Inc(i);
        if i > Length(Item.Caption) then
          Break;
        WordStart := i;
        while (i <= Length(Item.Caption)) and (Item.Caption[i] <> ' ') do
          Inc(i);
        WordEnd := i - 1;
        Word := Copy(Item.Caption, WordStart, WordEnd - WordStart + 1);

        if CurrentLine = '' then
          LineWidth := Trunc(LocalBmp.Canvas.TextWidth(Word))
        else
          LineWidth := Trunc(LocalBmp.Canvas.TextWidth(CurrentLine + ' ' + Word));

        if LineWidth > MaxCaptionWidth then
        begin
          if CurrentLine <> '' then
            Lines.Add(CurrentLine);
          CurrentLine := Word;
        end
        else
        begin
          if CurrentLine = '' then
            CurrentLine := Word
          else
            CurrentLine := CurrentLine + ' ' + Word;
        end;
      end;
      if CurrentLine <> '' then
        Lines.Add(CurrentLine);

      LineHeight := Trunc(LocalBmp.Canvas.TextHeight('Hg')) + 2;
      MaxCaptionHeight := Max(MaxCaptionHeight, LineHeight * 2);
      MaxLinesToShow := 2;
      if MaxLinesToShow < 1 then
        MaxLinesToShow := 1;
      ActualLinesToShow := Min(MaxLinesToShow, Lines.Count);

      if Lines.Count > MaxLinesToShow then
        MaxCaptionHeight := MaxCaptionHeight
      else
        MaxCaptionHeight := Lines.Count * LineHeight + 12;

      InflatedDrawRect := DrawRect;
      if (InflatedDrawRect.Bottom - InflatedDrawRect.Top) < MaxCaptionHeight + FCaptionOffsetY then
      begin
        CapTop := InflatedDrawRect.Bottom - MaxCaptionHeight - FCaptionOffsetY;
        if CapTop < 0 then
          CapTop := 0;
        TextRect.Top := CapTop;
        TextRect.Bottom := TextRect.Top + MaxCaptionHeight;
        TextRect.Left := InflatedDrawRect.Left;
        TextRect.Right := InflatedDrawRect.Right;
      end
      else
      begin
        TextRect.Top := InflatedDrawRect.Bottom - MaxCaptionHeight - FCaptionOffsetY;
        TextRect.Bottom := TextRect.Top + MaxCaptionHeight;
        TextRect.Left := InflatedDrawRect.Left;
        TextRect.Right := InflatedDrawRect.Right;
      end;

      MaxLineWidth := 0;
      for i := 0 to ActualLinesToShow - 1 do
      begin
        LineTextWidth := Trunc(LocalBmp.Canvas.TextWidth(Lines[i]));
        if LineTextWidth > MaxLineWidth then
          MaxLineWidth := LineTextWidth;
      end;

      TextRect.Left := TextRect.Left + ((TextRect.Right - TextRect.Left) - (MaxLineWidth + 24)) / 2.0;
      TextRect.Right := TextRect.Left + (MaxLineWidth + 24);

      if TextRect.Bottom > Height then
      begin
        TextRect.Bottom := Height;
        TextRect.Top := Height - MaxCaptionHeight;
        if TextRect.Top < 0 then
          TextRect.Top := 0;
      end;
      if TextRect.Top < 0 then
        TextRect.Top := 0;

      Paint.Style := TSkPaintStyle.Fill;
      if Item.IsSelected then
        Paint.Color := (FSelectedCaptionBackground and $00FFFFFF) or (FCaptionAlpha shl 24)
      else
        Paint.Color := (FCaptionBackground and $00FFFFFF) or (FCaptionAlpha shl 24);
      Paint.AlphaF := 1.0;
      Paint.AntiAlias := True;
      Paint.Alpha := FCaptionAlpha;
      Paint.ImageFilter := nil;
      ACanvas.DrawRoundRect(TextRect, 4.0, 4.0, Paint);

      SkStyle := TSkFontStyle.Normal;
      if TFontStyle.fsBold in FCaptionFont.Style then
        SkStyle := TSkFontStyle.Bold;
      if TFontStyle.fsItalic in FCaptionFont.Style then
        SkStyle := TSkFontStyle.Italic;

      SkFont := TSkFont.Create(TSkTypeface.MakeFromName(FCaptionFont.Family, SkStyle), FCaptionFont.Size);

      Paint.Style := TSkPaintStyle.Fill;
      Paint.AlphaF := 1.0;
      if Item.IsSelected then
        Paint.Color := FSelectedCaptionColor
      else
        Paint.Color := FCaptionColor;

      for i := 0 to ActualLinesToShow - 1 do
      begin
        LineTextWidth := Trunc(LocalBmp.Canvas.TextWidth(Lines[i]));
        DrawX := TextRect.Left + (TextRect.Right - TextRect.Left - LineTextWidth) / 2.0;
        DrawY := (TextRect.Top + 6 + (i * LineHeight)) + LineHeight - (LineHeight * 0.25);
        ACanvas.DrawSimpleText(Lines[i], DrawX, DrawY, SkFont, Paint);
      end;

    finally
      Lines.Free;
      LocalBmp.Free;
    end;
  end;

  procedure DrawSmallPicOverlay(const Item: TImageItem; const BaseRect: TRectF);
  var
    IconBmp: TBitmap;
    IconSkImg: ISkImage;
    IconRect: TRectF;
    Margin: Integer;
  begin
    if FShowSmallPicOnlyOnHover and (Item <> FHotItem) and (Item <> FSelectedImage) then
      Exit;
    if not FSmallPicVisible then
      Exit;
    if (Item.SmallPicIndex < 0) or (FSmallPicImageList = nil) then
      Exit;
    if (Item.SmallPicIndex >= FSmallPicImageList.Count) then
      Exit;
    IconBmp := GetSmallPicBitmap(Item.SmallPicIndex);
    if not Assigned(IconBmp) then
      Exit;
    IconSkImg := BitmapToSkImage(IconBmp);
    if not Assigned(IconSkImg) then
      Exit;
    Margin := FSmallpicMargin;
    //raise matching to round edges
    if FRoundEdges > 0 then
      Margin := Margin + Round(FRoundEdges * 0.7);
    IconRect := TRectF.Create(0, 0, IconSkImg.Width, IconSkImg.Height);
    case FSmallPicPosition of
      spTopLeft:
        IconRect.Offset(BaseRect.Left + Margin, BaseRect.Top + Margin);
      spTopRight:
        IconRect.Offset(BaseRect.Right - Margin - IconRect.Width, BaseRect.Top + Margin);
      spBottomLeft:
        IconRect.Offset(BaseRect.Left + Margin, BaseRect.Bottom - Margin - IconRect.Height);
      spBottomRight:
        IconRect.Offset(BaseRect.Right - Margin - IconRect.Width, BaseRect.Bottom - Margin - IconRect.Height);
    end;
    ACanvas.DrawImageRect(IconSkImg, IconRect, TSkSamplingOptions.High, Paint);
  end;

  procedure DrawTechBrackets(const R: TRectF; const P: ISkPaint; const Item: TImageItem);
  var
    Len: Single;
    L, T, Rgt, B: Single;
    ImageAlpha: Byte; // Used to match the image's pulsing/fading alpha
  begin
    // === USE PROPERTY FOR LENGTH (ARMS) ===
    // We use the property here so you can choose how long the arms are.
    Len := FTechBracketWidth;
    L := R.Left;
    T := R.Top;
    Rgt := R.Right;
    B := R.Bottom;
    // === DETERMINE ALPHA BASED ON STATE ===
    // Selected images use HotSelected alpha.
    // Hot/Animating images use HotPhase alpha.
    // Others use Static alpha.
    if Item = FSelectedImage then
      ImageAlpha := FAlphaHotSelected
    else if (Item = FHotItem) or Item.Animating then
      ImageAlpha := FAlphaHotPhase
    else
      ImageAlpha := FAlphaStatic;
    Paint.Style := TSkPaintStyle.Stroke;
    Paint.ImageFilter := nil;
    Paint.AntiAlias := True;
    Paint.Alpha := ImageAlpha; // Apply calculated alpha
    // === COLOR BASED ON STATE ===
    // Selected items use Glow Color.
    // Other items (Hot/Static) use Hot Track Color.
    if Item.IsSelected then
      Paint.Color := FGlowColor
    else
      Paint.Color := FHotTrackColor;
    // === THICKNESS BASED ON STATE ===
    // Selected items use GlowWidth (Thicker).
    // Other items (Hot/Static) use HotTrackWidth (Thinner).
    if Item.IsSelected then
      Paint.StrokeWidth := FGlowWidth
    else
      Paint.StrokeWidth := FHotTrackWidth;
    // === DRAW BRACKET LINES (Standard [] Shape) ===
    ACanvas.DrawLine(L, T, L + Len, T, P);
    ACanvas.DrawLine(L, T, L, T + Len, P);
    ACanvas.DrawLine(Rgt - Len, T, Rgt, T, P);
    ACanvas.DrawLine(Rgt, T, Rgt, T + Len, P);
    ACanvas.DrawLine(L, B - Len, L, B, P);
    ACanvas.DrawLine(L, B, L + Len, B, P);
    ACanvas.DrawLine(Rgt - Len, B, Rgt, B, P);
    ACanvas.DrawLine(Rgt, B, Rgt, B - Len, P);
  end;

  procedure DrawAndAnimateParticles;
  var
    PPaint: ISkPaint;
    i: Integer;
  begin
    if FParticles.Count = 0 then
      Exit;
    PPaint := TSkPaint.Create;
    PPaint.Style := TSkPaintStyle.Fill;
    for i := FParticles.Count - 1 downto 0 do
    begin
      P := FParticles[i];
      P.X := P.X + P.VX;
      P.Y := P.Y + P.VY;
      P.Life := P.Life - 0.05;
      PPaint.Color := P.Color;
      PPaint.AlphaF := P.Life;
      ACanvas.DrawRect(TRectF.Create(P.X, P.Y, P.X + P.Size, P.Y + P.Size), PPaint);
      if P.Life <= 0 then
        FParticles.Delete(i)
      else
        FParticles[i] := P;
    end;
  end;
  // Core Drawing Logic for an item (Returns Visual Rect for hit checks)

  function ProcessItem(Item: TImageItem; UseGlow: Boolean): TRectF;
  var
    VisRect: TRectF;
    Cx, Cy: Single;
    ImageAlpha: Byte;
    IsEntering: Boolean; // VCL Logic for "Entering"
  begin
    Result := TRectF.Create(0, 0, 0, 0);
    if not Item.Visible then
      Exit;
    if not Assigned(Item.SkImage) then
      Exit;

    // ==========================================================
    // Use GetVisualRect correctly for Wall Sliding
    // ==========================================================
    VisRect := GetVisualRect(Item);

    ACanvas.Save;

    // Handle Rotation
    var DrawAngle: Single;
    DrawAngle := Item.ActualRotation;
    if FBreathingEnabled and FBreathRotationEnabled and (Item = FSelectedImage) and (Abs(DrawAngle) > 0.1) then
    begin
      DrawAngle := DrawAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
    end;
    if DrawAngle <> 0 then
    begin
      // Calculate Center for Rotation based on the ALREADY CORRECTED VisualRect
      Cx := (VisRect.Left + VisRect.Right) / 2;
      Cy := (VisRect.Top + VisRect.Bottom) / 2;
      ACanvas.Translate(Cx, Cy);
      ACanvas.Rotate(DrawAngle);
      ACanvas.Translate(-Cx, -Cy);
    end;

    // Determine Alpha
    // === DO NOT USE "ANIMATING" FOR ALPHA SELECTION ===
    // We only differentiate between Selected and Normal (Static).
    // If item is moving (Layout change) but not Hot/Selected,
    // it should retain its Static Alpha (e.g., 220),
    // not jump to Hot Alpha (255).
    // Item.Alpha handles the fading (0 -> Static).
    // We multiply Item.Alpha by the Base Alpha (Static/Hot/Selected).

    if Item = FSelectedImage then
      ImageAlpha := Round((FAlphaHotSelected / 255) * Item.Alpha)
    else if (Item = FHotItem) then
      ImageAlpha := Round((FAlphaHotPhase / 255) * Item.Alpha)
    else
      ImageAlpha := Round((FAlphaStatic / 255) * Item.Alpha);

    Paint.ImageFilter := nil;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Alpha := ImageAlpha;

    // Draw Image (Always draw image + shadow)
    DrawImageWithEffect(ACanvas, Item, VisRect, Paint);

    // === DRAW EXTRAS (Borders / Tech Brackets / SmallPic / Caption) ===
    // ===========================================================

    // <--- CHECK IF ENTERING --->
    // A new image is "entering" (flying in) if:
    // Not hovered AND Not Selected AND Animation Progress < 99%
    // AND HotZoom < 99% AND Not actively hot-tracking (HotZoom vs Target)
    IsEntering := (Item <> FHotItem) and (Item.AnimationProgress < 0.99) and (Item.FHotZoom < 0.99) and (Item <> FSelectedImage) and (not (Abs(Item.FHotZoom - Item.FHotZoomTarget) > HOT_ZOOM_EPSILON));

    // ONLY DRAW THESE IF NOT ENTERING
    if not IsEntering then
    begin
      // 1. CHECK STATE: NORMAL OR (HOT / SELECTED)
      if not ((Item = FHotItem) or Item.IsSelected) then
      begin
        // === NORMAL IMAGE: Subtle Border ===
        Paint.Style := TSkPaintStyle.Stroke;
        Paint.Color := TAlphaColor($FF373737);
        Paint.Alpha := FAlphaStatic;
        Paint.StrokeWidth := 1;
        Paint.ImageFilter := nil;

        if FRoundEdges > 0 then
          ACanvas.DrawRoundRect(VisRect, FRoundEdges, FRoundEdges, Paint)
        else
          ACanvas.DrawRect(VisRect, Paint);
      end
      else
      begin
        // === HOT OR SELECTED IMAGE ===
        Paint.Style := TSkPaintStyle.Stroke;
        Paint.Color := TAlphaColor(FHotTrackColor);
        Paint.ImageFilter := nil;
        Paint.AntiAlias := True;
        Paint.Alpha := ImageAlpha;

        Paint.StrokeWidth := FHotTrackWidth;

        case FPictureBorderType of
          btTech:
            DrawTechBrackets(VisRect, Paint, Item);
          btFull:
            begin
              if FRoundEdges > 0 then
                ACanvas.DrawRoundRect(VisRect, FRoundEdges, FRoundEdges, Paint)
              else
                ACanvas.DrawRect(VisRect, Paint);
            end;
        end;
      end;

      // Draw Caption
      DrawCaption(Item, VisRect);

      // Draw SmallPic
      DrawSmallPicOverlay(Item, VisRect);
    end;
    // === END OF ENTERING CHECK ===

    ACanvas.Restore;
    Result := VisRect;
  end;

begin
  // =========================================================================
  // 0. PREPARE BUCKETS FOR Z-ORDERING
  // =========================================================================
  StaticImages := TList.Create;
  GlobalSortHotItem := FHotItem;
  AnimatingImages := TList.Create;
  EnteringImages := TList.Create;
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  ShadowFilter := TSkImageFilter.MakeDropShadow(SHADOW_OFFSET_X, SHADOW_OFFSET_Y, 10.0, 10.0, TAlphaColors.Black, nil);
  try
   // =========================================================================
  // 1. BACKGROUND
  // =========================================================================

  // 1. Always clear to solid base color first
    ACanvas.Clear(FBackgroundColor);

  // 2. Draw Effects (Images/Animations/Matrix)
  // We pass the Paint object we just created to reuse it
    DrawBackgroundEffects(ACanvas, ADest, Paint);
     // <--- DRAW FALLING SMALL PICS --->
   // DrawSmallPicParticles(ACanvas);

    // =========================================================================
    // 2. SORT IMAGES INTO Z-ORDER BUCKETS
    // =========================================================================
    if FImages.Count > 0 then
    begin
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if Assigned(FWasSelectedItem) and (ImageItem = FWasSelectedItem) and (FSelectedImage = nil) and ((FWasSelectedItem.ZoomProgress > 0.001) or (FWasSelectedItem.Animating)) then
          Continue; // handled in step 7.5

        if ImageItem = FSelectedImage then
          Continue; // Selected handled separately

        // Is it an entering (flying in) image?
        if (ImageItem.AnimationProgress < 0.99) and (ImageItem.FHotZoom < 0.99) then
        begin
          EnteringImages.Add(ImageItem);
          ImageItem.Animating := True;
        end
        // Is it an animating or hot-zoomed image?
        else if ImageItem.Animating or (Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) > HOT_ZOOM_EPSILON) then
        begin
          AnimatingImages.Add(ImageItem);
        end
        else
        begin
          StaticImages.Add(ImageItem);
        end;
      end;
      StaticImages.Sort(@CompareZOrder);
    end;

    // =========================================================================
    // 3. DRAW STATIC IMAGES (Back layer)
    // =========================================================================
    for i := 0 to StaticImages.Count - 1 do
    begin
      ProcessItem(TImageItem(StaticImages[i]), False);
    end;

    // =========================================================================
    // 4. DRAW ANIMATING / HOT-ZOOMED IMAGES (Middle layer)
    // Sorted by Zoom (Highest = On Top) to prevent flicker
    // =========================================================================
    if AnimatingImages.Count > 0 then
    begin
      AnimatingImages.Sort(@CompareZOrder);
      for i := 0 to AnimatingImages.Count - 1 do
      begin
        CurrentItem := TImageItem(AnimatingImages[i]);
        if CurrentItem = FSelectedImage then
          Continue;
        ProcessItem(CurrentItem, True);
      end;
    end;

    // =========================================================================
    // 5. DRAW HOT ITEM (Top of Animating)
    // =========================================================================
    if (FHotItem <> nil) and (FHotItem <> FSelectedImage) then
    begin
      ProcessItem(FHotItem, True);
    end;

   // =========================================================================
    // 6. DRAW SELECTED IMAGE (Absolute Top)
    // =========================================================================
    if Assigned(FSelectedImage) and FSelectedImage.Visible then
    begin
      ImageItem := FSelectedImage;
      VisualRect := GetVisualRect(ImageItem);
      var SelZoom: Single;
      SelZoom := ImageItem.FHotZoom;

      // === SHADOW SCALING ===
      var ShadowMultiplier: Single;
      if FBreathingEnabled then
        ShadowMultiplier := 2.0  // Significantly larger/softer shadow
      else
        ShadowMultiplier := 1.0; // Normal size

      // === CALCULATE VISUAL ANGLE (With Wobble) ===
      var SelDrawAngle: Single;
      SelDrawAngle := ImageItem.FActualRotation;
      if FBreathingEnabled and BreathRotationEnabled and (Abs(SelDrawAngle) > 0.1) then
        SelDrawAngle := SelDrawAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);

      // === CALCULATE SHADOW BASE ANGLE (Follows Image) ===
      // We use ActualRotation here. The shadow rotates WITH the card.
      // This gives the correct "Perspective" where the shadow is attached to the object.
      var ShadowBaseAngle: Single;
      ShadowBaseAngle := ImageItem.FActualRotation;

      ACanvas.Save;

      // === CALCULATE SHADOW OFFSET ===
      var MaxShadowOffset, MaxBlurRadius: Single;
      if ShadowBaseAngle <> 0 then
      begin
        ShadowRad := -ShadowBaseAngle * (PI / 180);
        ShadowDx := ((SHADOW_OFFSET_X * Cos(ShadowRad)) - (SHADOW_OFFSET_Y * Sin(ShadowRad))) * SelZoom * ShadowMultiplier;
        ShadowDy := ((SHADOW_OFFSET_X * Sin(ShadowRad)) + (SHADOW_OFFSET_Y * Cos(ShadowRad))) * SelZoom * ShadowMultiplier;

        // Pre-calculate max extent for LayerBounds
        MaxShadowOffset := Max(Abs(ShadowDx), Abs(ShadowDy));
      end
      else
      begin
        ShadowDx := SHADOW_OFFSET_X * SelZoom * ShadowMultiplier;
        ShadowDy := SHADOW_OFFSET_Y * SelZoom * ShadowMultiplier;
        MaxShadowOffset := Max(Abs(ShadowDx), Abs(ShadowDy));
      end;
      // === END SHADOW CALCULATION ===

      // === CALCULATE DYNAMIC LAYER BOUNDS ===
      // We ensure the layer buffer is big enough for the shadow + blur.
      // This prevents the shadow from being "cut" by the layer boundaries.
      MaxBlurRadius := 10.0 * SelZoom * ShadowMultiplier;
      var LayerBounds: TRectF := VisualRect;
      // Inflate by Max Offset + Max Blur + 10 (padding)
      LayerBounds.Inflate(MaxShadowOffset + MaxBlurRadius + 20, MaxShadowOffset + MaxBlurRadius + 20);

      // === CREATE SHADOW FILTER ===
      ShadowFilter := TSkImageFilter.MakeDropShadow(ShadowDx, ShadowDy, 10.0 * SelZoom * ShadowMultiplier, // Dynamic Blur Radius
        10.0 * SelZoom * ShadowMultiplier, TAlphaColors.Black, nil);

      // === ROTATE CANVAS FOR IMAGE (Uses SelDrawAngle for wobble) ===
      if SelDrawAngle <> 0 then
      begin
        CenterX := (VisualRect.Left + VisualRect.Right) / 2;
        CenterY := (VisualRect.Top + VisualRect.Bottom) / 2;
        ACanvas.Translate(CenterX, CenterY);
        ACanvas.Rotate(SelDrawAngle);
        ACanvas.Translate(-CenterX, -CenterY);
      end;

      // === DRAW IMAGE & SHADOW ===
      if FRoundEdges > 0 then
      begin
        var LayerPaint: ISkPaint := TSkPaint.Create;
        LayerPaint.Color := TAlphaColors.White;
        LayerPaint.Alpha := FAlphaHotSelected;
        LayerPaint.AntiAlias := True;

        // Use the calculated LayerBounds here
        ACanvas.SaveLayer(LayerBounds, LayerPaint);

        Paint.ImageFilter := ShadowFilter;
        Paint.Style := TSkPaintStyle.Fill;
        Paint.Color := TAlphaColors.Black;
        Paint.Alpha := 255;

        var RR: ISkRoundRect := TSkRoundRect.Create(VisualRect, FRoundEdges, FRoundEdges);
        ACanvas.DrawRoundRect(RR, Paint);

        ACanvas.ClipRoundRect(RR, TSkClipOp.Intersect, True);
        Paint.ImageFilter := nil;
        Paint.Alpha := 255;
        ACanvas.DrawImageRect(ImageItem.SkImage, VisualRect, TSkSamplingOptions.High, Paint);

        ACanvas.Restore;
      end
      else
      begin
        // Square images use standard LayerSave or direct draw
        // For square images with huge shadows, direct draw is safer unless clipping is needed
        // Let's use the SafeLayer logic for consistency
        var LayerPaint: ISkPaint := TSkPaint.Create;
        LayerPaint.Color := TAlphaColors.White;
        LayerPaint.Alpha := FAlphaHotSelected;
        LayerPaint.AntiAlias := True;

        ACanvas.SaveLayer(LayerBounds, LayerPaint);

        Paint.ImageFilter := ShadowFilter;
        Paint.AntiAlias := True;
        Paint.Style := TSkPaintStyle.Fill;
        Paint.Alpha := FAlphaHotSelected;
        ACanvas.DrawImageRect(ImageItem.SkImage, VisualRect, TSkSamplingOptions.High, Paint);

        ACanvas.Restore;
      end;

    // ==========================================================
    // DRAW VIDEO SNAPSHOT IF CAPTURING
    // ==========================================================
    // We draw the video frame ON TOP of the image.

      if FCaptureVideo and Assigned(FSelectedVideoSnapshot) then
        if (not FSelectedVideoSnapshot.ImageInfo.IsEmpty) then
        begin
          ACanvas.Save;
      // Draw Snapshot
          Paint.Style := TSkPaintStyle.Fill;
          ACanvas.DrawImageRect(FSelectedVideoSnapshot, VisRect, TSkSamplingOptions.High, Paint);
          ACanvas.Restore;
        end;

      // === DRAW ROTATION HANDLE ===
      if FRotationAllowed and (not FIsZoomedToFill) then
      begin
        Margin := FSmallpicMargin;
        //raise matching to round edges
        if FRoundEdges > 0 then
          Margin := Margin + Round(FRoundEdges * 0.7);
        HandleSize := FRotateHandleSize;

        case FRotateHandlePosition of
          spTopLeft:
            HandleRect := TRectF.Create(VisualRect.Left + Margin, VisualRect.Top + Margin, VisualRect.Left + Margin + HandleSize, VisualRect.Top + Margin + HandleSize);
          spTopRight:
            HandleRect := TRectF.Create(VisualRect.Right - Margin - HandleSize, VisualRect.Top + Margin, VisualRect.Right - Margin, VisualRect.Top + Margin + HandleSize);
          spBottomLeft:
            HandleRect := TRectF.Create(VisualRect.Left + Margin, VisualRect.Bottom - Margin - HandleSize, VisualRect.Left + Margin + HandleSize, VisualRect.Bottom - Margin);
          spBottomRight:
            HandleRect := TRectF.Create(VisualRect.Right - Margin - HandleSize, VisualRect.Bottom - Margin - HandleSize, VisualRect.Right - Margin, VisualRect.Bottom - Margin);
        end;

        Paint.Style := TSkPaintStyle.Stroke;
        Paint.StrokeWidth := 2.5;
        Paint.Color := TAlphaColors.Teal;
        Paint.ImageFilter := TSkImageFilter.MakeDropShadow(1, 1, 4.0, 4.0, TAlphaColors.Black, nil);

        Paint.Alpha := 1;
        ACanvas.DrawOval(TRectF.Create(HandleRect.Left, HandleRect.Top, HandleRect.Right, HandleRect.Bottom), Paint);

        Paint.StrokeWidth := 3;
        if FIsRotating then
          Paint.Color := FRotateDotDownColor
        else if FIsMouseOverHandle then
          Paint.Color := FRotateDotHotColor
        else
          Paint.Color := FRotateDotColor;
        Paint.ImageFilter := nil;
        ACanvas.DrawOval(TRectF.Create(HandleRect.Left + 2, HandleRect.Top + 2, HandleRect.Right - 2, HandleRect.Bottom - 2), Paint);
      end;

      DrawInfoIndicator(ACanvas, VisualRect, FSelectedImage);

      DrawCaption(FSelectedImage, VisualRect);

      DrawSmallPicOverlay(FSelectedImage, VisualRect);

      // === CLIP PANEL TO ROUNDED CORNERS ===
      if FRoundEdges > 0 then
      begin
        ACanvas.Save;
        var RR: ISkRoundRect;
        RR := TSkRoundRect.Create(VisualRect, FRoundEdges, FRoundEdges);
        ACanvas.ClipRoundRect(RR, TSkClipOp.Intersect, True);

        DrawFluidInfo(FSelectedImage, VisualRect, ACanvas);

        ACanvas.Restore;
      end
      else
      begin
        // Square image, no special clipping needed
        DrawFluidInfo(FSelectedImage, VisualRect, ACanvas);
      end;

      ACanvas.Restore;

      Paint.ImageFilter := nil;
      Paint.AntiAlias := True;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.Color := TAlphaColor(FGlowColor);
      Paint.StrokeWidth := FGlowWidth;
      Paint.Alpha := 255;
      ACanvas.Save;

      // === CALCULATE BORDER ROTATION WITH WOBBLE ===
      var BorderRotAngle: Single;
      BorderRotAngle := ImageItem.FActualRotation;
      if FBreathingEnabled and BreathRotationEnabled and (Abs(BorderRotAngle) > 0.1) then
      begin
        BorderRotAngle := BorderRotAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
      end;
      if BorderRotAngle <> 0 then
      begin
        CenterX := (VisualRect.Left + VisualRect.Right) / 2;
        CenterY := (VisualRect.Top + VisualRect.Bottom) / 2;
        ACanvas.Translate(CenterX, CenterY);
        ACanvas.Rotate(BorderRotAngle);
        ACanvas.Translate(-CenterX, -CenterY);
      end;
      case FPictureBorderType of
        btTech:
          DrawTechBrackets(VisualRect, Paint, ImageItem);
        btFull:
          begin
            if FRoundEdges > 0 then
              ACanvas.DrawRoundRect(VisualRect, FRoundEdges, FRoundEdges, Paint)
            else
              ACanvas.DrawRect(VisualRect, Paint);
          end;
      end;
      ACanvas.Restore;
    end;

    // =========================================================================
    // 7. DRAW ENTERING IMAGES (Top-most layer)
    // Sorted so new images appear on top
    // =========================================================================
    if EnteringImages.Count > 0 then
    begin
      EnteringImages.Sort(@CompareZOrder);
      for i := 0 to EnteringImages.Count - 1 do
      begin
        // Draw with Subtle style (UseGlow = False) -> No heavy borders
        // ProcessItem will automatically skip borders because of IsEntering check.
        ProcessItem(TImageItem(EnteringImages[i]), False);
      end;
    end;

    // =========================================================================
    // 7.5 DRAW PREVIOUSLY SELECTED (Un-Zooming Priority)
    // =========================================================================
    // If we un-selected an image (e.g. ZoomToFull off), FSelectedImage becomes nil.
    // The item falls into "AnimatingImages" bucket. New "Entering" images fly in ON TOP.
    // We must draw FWasSelectedItem on top of everything else during the transition.
    if Assigned(FWasSelectedItem) and (FSelectedImage = nil) and FWasSelectedItem.Visible then
    begin
      // We only draw it if it is actually animating (Zooming out or Breathing).
      // If ZoomProgress is 0 and it's static, it's just a normal image
      // and should follow normal Z-order rules.
      if (FWasSelectedItem.ZoomProgress > 0.001) or (FWasSelectedItem.Animating) then
      begin
        // UseGlow = False so it doesn't look "Hot" while shrinking.
        // Our previous Alpha fixes ensure it uses FAlphaStatic.
        ProcessItem(FWasSelectedItem, False);
      end;
    end;

  finally
    StaticImages.Free;
    AnimatingImages.Free;
    EnteringImages.Free;
  end;

  DrawAndAnimateParticles;
end;

// -----------------------------------------------------------------
// DrawSmallPicParticles
// Renders the falling small pic debris
// -----------------------------------------------------------------

 //atm instable

procedure TSkFlowmotion.DrawSmallPicParticles(ACanvas: ISkCanvas);
var
  i, Count: Integer;
  IconBmp: TBitmap;
  IconSkImg: ISkImage;
  Paint: ISkPaint;
  SP: TSmallPicParticle;
  ScaleW, ScaleH: Single;
  // LOCAL SNAPSHOT TO AVOID RACE CONDITION
  LocalParticles: TArray<TSmallPicParticle>;
begin
  if not FAnimatedBackground then
    Exit;
  if not Assigned(FSmallPicParticles) or (FSmallPicParticles.Count = 0) then
    Exit;
  if not FSmallPicVisible then
    Exit;
  if FSmallPicImageList = nil then
    Exit;
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.AntiAlias := True;
  // ==========================================================
  // STEP 1: CREATE SNAPSHOT (Thread Safe Copy)
  // ==========================================================
  // We copy the list to a local array. This allows the Physics thread
  // to modify/delete items from FSmallPicParticles safely while we are drawing.
  // Without this, "List index out of bounds" or "Access Violation" crashes occur.
  Count := FSmallPicParticles.Count;
  SetLength(LocalParticles, Count);
  for i := 0 to Count - 1 do
    LocalParticles[i] := FSmallPicParticles[i];
  // ==========================================================
  // STEP 2: DRAW FROM SNAPSHOT
  // ==========================================================
  // We iterate the local copy. Even if the main list is cleared now,
  // this local copy remains valid for the duration of this function.
  for i := 0 to Count - 1 do
  begin
    SP := LocalParticles[i];
    // Safety Checks
    if (SP.Index < 0) or (SP.Index >= FSmallPicImageList.Count) then
      Continue;
    IconBmp := GetSmallPicBitmap(SP.Index);
    if not Assigned(IconBmp) then
      Continue;
    IconSkImg := BitmapToSkImage(IconBmp);
    if not Assigned(IconSkImg) then
      Continue;
    // Alpha (Fade out)
    Paint.AlphaF := Max(0, SP.Life) * 0.5;
    // ==========================================================
    // SAFE TRANSFORM CHAIN (Uses Canvas State Stack)
    // ==========================================================
    ACanvas.Save;
    // 1. Move to particle position (X, Y)
    ACanvas.Translate(SP.X, SP.Y);
    // 2. Scale (Shrink) logic
    ScaleW := Max(0.2, SP.Life);
    ScaleH := Max(0.2, SP.Life);
    ACanvas.Translate(IconSkImg.Width / 2, IconSkImg.Height / 2);
    ACanvas.Scale(ScaleW, ScaleH);
    ACanvas.Translate(-IconSkImg.Width / 2, -IconSkImg.Height / 2);
    // 3. Rotate logic
    ACanvas.Translate(IconSkImg.Width / 2, IconSkImg.Height / 2);
    ACanvas.Rotate(SP.Angle);
    ACanvas.Translate(-IconSkImg.Width / 2, -IconSkImg.Height / 2);
    // Draw at (0,0) relative to current transformation
    ACanvas.DrawImageRect(IconSkImg, TRectF.Create(0, 0, IconSkImg.Width, IconSkImg.Height), TSkSamplingOptions.High, Paint);
    ACanvas.Restore;
  end;
end;


// -----------------------------------------------------------------------------
// ZoomSelectedToFull
// TOGGLES selected image:
// 1. If Grid -> Expands to Full Screen (Centered).
//    Angle is set based on FFullscreenAngle.
//    Size is calculated based on Full Screen (ignoring small FMaxZoomSize).
// 2. If Full  -> Returns to exact previous Grid position (and angle).
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.ZoomSelectedToFull;
var
  ImageSize: TSize;
  NewLeft, NewTop: Integer;
  Margin: Integer;
  // Vars for Screen Position
  L, R, T, B: Integer; // Left, Right, Top, Bottom bounds
  // Vars for Swapping Constraints (Rotation Logic)
  DynMaxW, DynMaxH: Integer;
begin
  // Safety checks
  if not Assigned(FSelectedImage) then
    Exit;
  if FInFallAnimation or FPageChangeInProgress or (FFlowLayout = flFreeFloat) then
    Exit;
    // >>> RESET EVENT FLAG <<<
  FOnFullscreenEnterFired := False;
  Margin := 20; // Standard margin from screen edges

  // ---------------------------------------------------------
  // CASE A: GO BACK TO GRID (Toggle OFF)
  // ---------------------------------------------------------
  if FIsZoomedToFill then
  begin
    // === RESTORE ROTATION ===
    // Restore to the rotation we had before we zoomed in
    FSelectedImage.TargetRect := FPrevRect;
    FSelectedImage.FTargetRotation := FPreviousRotation;
    DeselectZoomedImage;
    FIsZoomedToFill := False;
  end
  // ---------------------------------------------------------
  // CASE B: GO TO FULL SCREEN (Toggle ON)
  // ---------------------------------------------------------
  else
  begin
    // === SAVE "ACTUAL" ROTATION (What we see) ===
    // We save this so we can restore it when zooming out
    FPreviousRotation := FSelectedImage.FActualRotation;
    FPrevRect := FSelectedImage.FCurrentRect;

    // 1. CALCULATE DYNAMIC CONSTRAINTS (Rotation Logic)
    // ==========================================================
    // We swap Width/Height limits if rotating 90 or 270 degrees
    // to ensure the image fits in the available screen space.
    L := Margin;              // Left Bound
    R := trunc(Width) - Margin; // Right Bound
    T := Margin;              // Top Bound
    B := trunc(Height) - Margin; // Bottom Bound

    case FFullscreenAngle of
      fsa0, fsa180:
        begin
          // Landscape bounds
          DynMaxW := R - L;
          DynMaxH := B - T;
        end;
      fsa90, fsa270:
        begin
          // Portrait bounds (Swap W and H)
          // If we rotate 90deg, Original Width becomes Visual Height.
          // So Original Width limit is Screen Height.
          DynMaxW := B - T; // Screen Height
          DynMaxH := R - L; // Screen Width
        end;
    end;

    // 2. CALCULATE IMAGE SIZE
    // ==========================================================
    // We IGNORE FMaxZoomSize here to ensure the image actually fills the screen.
    // We only use the Dynamic Limits calculated above.
    if Assigned(FSelectedImage.SkImage) then
      ImageSize := GetOptimalSize(FSelectedImage.SkImage.Width, FSelectedImage.SkImage.Height, DynMaxW, DynMaxH)
    else
    begin
      ImageSize.cx := FSelectedImage.CurrentRect.Right - FSelectedImage.CurrentRect.Left;
      ImageSize.cy := FSelectedImage.CurrentRect.Bottom - FSelectedImage.CurrentRect.Top;
    end;

    // 3. CALCULATE TARGET POSITION (Centered)
    // ==========================================================
    NewLeft := (trunc(Width) - ImageSize.cx) div 2;
    NewTop := (trunc(Height) - ImageSize.cy) div 2;

    // 4. FORCE ROTATION (If required)
    // ==========================================================
    // We force the angle to match the requested FullscreenAngle.

    case FFullscreenAngle of
      fsa0:
        FSelectedImage.FTargetRotation := 0;
      fsa90:
        FSelectedImage.FTargetRotation := 90;
      fsa180:
        FSelectedImage.FTargetRotation := 180;
      fsa270:
        FSelectedImage.FTargetRotation := 270;
    end;

      // IMPORTANT: Stop Breathing to prevent jitter
      // When we force a full screen zoom, we want it static.
      // We set Target to 1.0 explicitly.
    FSelectedImage.FHotZoomTarget := 1.0;
    FSelectedImage.FHotZoom := 1.0; // Snap immediately

    // 5. ANCHOR ANIMATION: Set Start to where image is NOW
    FSelectedImage.StartRect := FSelectedImage.CurrentRect;

    // 6. SET TARGET TO FULL SCREEN
    FSelectedImage.TargetRect := Rect(NewLeft, NewTop, NewLeft + ImageSize.cx, NewTop + ImageSize.cy);

    // 7. RESET PROGRESS to start Zoom IN animation
    FSelectedImage.ZoomProgress := 0;
    FSelectedImage.AnimationProgress := 0;
    FSelectedImage.Animating := True;
    FIsZoomedToFill := True;
    // >>> RESET EVENT FLAG (Ready to trigger when finished) <<<
    FOnFullscreenEnterFired := False;
  end;

  // Trigger animation loop
  StartAnimationThread;
end;
// --- Finders ---

function TSkFlowmotion.FindImageByPath(const Path: string): TImageItem;
var
  i: Integer;
  AbsoluteIndex: Integer;
begin
  Result := nil;
  for i := 0 to FAllPaths.Count - 1 do
  begin
    if SameText(FAllPaths[i], Path) then
    begin
      AbsoluteIndex := i;
      if (AbsoluteIndex >= GetPageStartIndex) and (AbsoluteIndex <= GetPageEndIndex) then
      begin
        Result := TImageItem(FImages[AbsoluteIndex - GetPageStartIndex]);
      end
      else
      begin
        // Switch page if necessary
        FCurrentPage := AbsoluteIndex div FPageSize;
        ShowPage(FCurrentPage);
        if (AbsoluteIndex >= GetPageStartIndex) and (AbsoluteIndex <= GetPageEndIndex) then
          Result := TImageItem(FImages[AbsoluteIndex - GetPageStartIndex]);
      end;
      Break;
    end;
  end;
end;

function TSkFlowmotion.FindImageByCaption(const Caption: string): TImageItem;
var
  i: Integer;
  AbsoluteIndex, RelativeIndex, TargetPage: Integer;
begin
  Result := nil;
  for i := 0 to FAllCaptions.Count - 1 do
  begin
    if SameText(FAllCaptions[i], Caption) then
    begin
      AbsoluteIndex := i;

      if (AbsoluteIndex >= GetPageStartIndex) and (AbsoluteIndex <= GetPageEndIndex) then
      begin
        RelativeIndex := AbsoluteIndex - GetPageStartIndex;
        if (RelativeIndex >= 0) and (RelativeIndex < FImages.Count) then
          Result := TImageItem(FImages[RelativeIndex]);
      end
      else
      begin
        TargetPage := AbsoluteIndex div FPageSize;
        if (TargetPage >= 0) and (TargetPage < GetPageCount) and (TargetPage <> FCurrentPage) then
        begin
          ShowPage(TargetPage);
          RelativeIndex := AbsoluteIndex - GetPageStartIndex;
          if (RelativeIndex >= 0) and (RelativeIndex < FImages.Count) then
            Result := TImageItem(FImages[RelativeIndex]);
        end;
      end;
      Break;
    end;
  end;
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
  if FBackgroundEffect = beRealMatrix then
    InitMatrix;
  if (FlowLayout = flFreeFloat) then
  //we need only set selected pos on free layout, normal sets it self at calc layout
    if FZoomSelectedtoCenter and (FSelectedImage <> nil) then
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
  AddImage(FileName, ExtractFileName(FileName), FileName, AHint, '', ASmallPicIndex);
end;

procedure TSkFlowmotion.AddImage(const FileName, ACaption, APath, AHint, AInfoText: string; ASmallPicIndex: Integer = -1);
var
  SkImg: ISkImage;
  WasEmpty: Boolean;
  NewItem: TImageItem;
begin
  // If Clearing is active (Threaded animation) or Page changing, do not add.
  // Prevents Race Condition with FAllFiles clearing in background thread.
  if FisClearing or FPageChangeInProgress then
    Exit;
  WasEmpty := (FAllFiles.Count = 0);
  { Always add to Master Lists }
  if FileName <> '' then
  begin
    FAllFiles.Add(FileName);
    FAllCaptions.Add(ACaption);
    FAllPaths.Add(APath);
    FAllHints.Add(AHint);
    FAllInfoTexts.Add(AInfoText);
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
        SkImg := ResizeImageIfNeeded(SkImg);
        NewItem := TImageItem.Create;
        NewItem.SkImage := SkImg;
        NewItem.Caption := ACaption;
        NewItem.Path := APath;
        NewItem.Hint := AHint;
        NewItem.ImageIndex := FAllFiles.Count - 1;
        NewItem.FileName := FileName;
        NewItem.Direction := GetEntryDirection;
        NewItem.SmallPicIndex := ASmallPicIndex;
        NewItem.DominantColor := GetDominantColor(SkImg);
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
  // If Clearing is active (Threaded animation) or Page changing, do not add.
  // Prevents Race Condition with FAllFiles clearing in background thread.
  if FisClearing or FPageChangeInProgress then
    Exit;
  if Bitmap = nil then
    Exit;
  DummyName := 'MemoryBitmap_' + IntToStr(GetTickCount) + '_' + IntToStr(Random(10000));
  FAllFiles.Add(DummyName);
  FAllCaptions.Add('');
  FAllPaths.Add('');
  FAllHints.Add('');
  FAllSmallPicIndices.Add(Pointer(-1));
  NewItem := TImageItem.Create;
  NewItem.SkImage := BitmapToSkImage(Bitmap);
  NewItem.FileName := DummyName;
  NewItem.ImageIndex := FAllFiles.Count - 1;
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

procedure TSkFlowmotion.AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = ''; const AHint: string = ''; const AInfo: string = ''; ASmallPicIndex: Integer = -1);
var
  NewAbsIndex, TargetPage: Integer;
  LoadThread: TImageLoadThread;
  WasEmpty: Boolean;
begin
  // If Clearing is active (Threaded animation) or Page changing, do not add.
  // Prevents Race Condition with FAllFiles clearing in background thread.
  if FisClearing or FPageChangeInProgress then
    Exit;
  // Basic safety check
  if FileName = '' then
    Exit;

  WasEmpty := (FAllFiles.Count = 0);

  { ==========================================================
  1. Add ALL to Master Lists (Updates Counts/PageCount)
  ========================================================== }
  FAllFiles.Add(FileName);
  FAllCaptions.Add(ACaption);
  FAllPaths.Add(APath);
  FAllHints.Add(AHint);
  FAllSmallPicIndices.Add(Pointer(ASmallPicIndex));

  // Calculate the absolute index of the new item
  NewAbsIndex := FAllFiles.Count - 1;

  { ==========================================================
  2. Determine Page and Spawn Loading Thread
  ========================================================== }
  TargetPage := NewAbsIndex div FPageSize;

  if WasEmpty then
  begin
    // If the list was empty, we must load the page immediately to show the image
    ShowPage(FCurrentPage);
  end
  else if (TargetPage = FCurrentPage) then
  begin
    // Only spawn a thread if this specific image belongs to the CURRENT page
    LoadThread := TImageLoadThread.Create(FileName, ACaption, APath, AHint, AInfo, Self, NewAbsIndex, ASmallPicIndex);
    LoadThread.Priority := FThreadPriority;
    FLoadingThreads.Add(LoadThread);
    Inc(FLoadingCount);
  end;
  // Else: Image is off-page. It stays in Master Lists but is NOT loaded.
  // It will be loaded later when you navigate to that page.

  // Ensure animation/render loop is active
  StartAnimationThread;
end;

procedure TSkFlowmotion.AddImages(const FileNames, Captions, Paths, Hints, Infos: TStringList; const SmallPicIndices: TList = nil);
var
  i: Integer;
  SkImg: ISkImage;
  NewItem: TImageItem;
  AbsIndex: Integer;
begin
  // If Clearing is active (Threaded animation) or Page changing, do not add.
  // Prevents Race Condition with FAllFiles clearing in background thread.
  if FisClearing or FPageChangeInProgress then
    Exit;
  if (FileNames = nil) or (Captions = nil) or (Paths = nil) or (Hints = nil) then
    Exit;
  if (FileNames.Count <> Captions.Count) or (FileNames.Count <> Paths.Count) or (FileNames.Count <> Hints.Count) then
    Exit;

  // 1. Always add to Master Lists (Updates Counts/PageCount)
  for i := 0 to FileNames.Count - 1 do
  begin
    FAllFiles.Add(FileNames[i]);
    FAllCaptions.Add(Captions[i]);
    FAllPaths.Add(Paths[i]);
    FAllHints.Add(Hints[i]);
    FAllInfoTexts.Add(Infos[i]);
    if Assigned(SmallPicIndices) and (i < SmallPicIndices.Count) then
      FAllSmallPicIndices.Add(Pointer(SmallPicIndices[i]))
    else
      FAllSmallPicIndices.Add(Pointer(-1));
  end;

  // 2. Load Images (Sync)
  // We only load them into FImages if they fit on the Current Page
  for i := 0 to FileNames.Count - 1 do
  begin
    if TFile.Exists(FileNames[i]) then
    begin
      SkImg := TSkImage.MakeFromEncodedFile(FileNames[i]);
      if Assigned(SkImg) then
      begin
        SkImg := ResizeImageIfNeeded(SkImg);
        NewItem := TImageItem.Create;
        NewItem.SkImage := SkImg;
        NewItem.Caption := Captions[i];
        NewItem.Path := Paths[i];
        NewItem.Hint := Hints[i];
        NewItem.InfoText := Infos[i];
        NewItem.ImageIndex := FAllFiles.Count - 1;
        NewItem.FileName := FileNames[i];
        NewItem.Direction := GetEntryDirection;
        NewItem.DominantColor := GetDominantColor(SkImg);
        NewItem.Visible := False;

        // RETRIEVE SmallPic Index FROM MASTER LIST (Absolute Index)
        AbsIndex := (FAllFiles.Count - FileNames.Count) + i;

        if AbsIndex < FAllSmallPicIndices.Count then
          NewItem.SmallPicIndex := Integer(FAllSmallPicIndices[AbsIndex])
        else
          NewItem.SmallPicIndex := -1;

        // CHECK PAGESIZE BEFORE ADDING TO VISIBLE LIST ===
        // If Visible List (FImages) is full, we skip adding to it.
        // This prevents SelectNextImage from seeing items that belong to next pages.
        if FImages.Count < FPageSize then
        begin
          FImages.Add(NewItem);
          if Visible then
          begin
            CalculateLayout;
            AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
            NewItem.Visible := True;
          end;
        end
        else
        begin
          // Item is off-page. We created it but don't add to FImages.
          // We MUST Free it to avoid memory leak!
          // It stays in Master Lists (FAllFiles) so page count is correct.
          NewItem.Free;
        end;
      end;
    end;
  end;
  StartAnimationThread;
end;

procedure TSkFlowmotion.AddImagesAsync(const FileNames, Captions, Paths, Hints, Infotxts: TStringList; const SmallPicIndices: TList = nil);
var
  i: Integer;
  LoadThread: TImageLoadThread;
  NewAbsIndex, TargetPage: Integer;
  CurrentSmallIndex: Integer;
  WasEmpty: Boolean;
begin
  // If Clearing is active (Threaded animation) or Page changing, do not add.
  // Prevents Race Condition with FAllFiles clearing in background thread.
  if FisClearing or FPageChangeInProgress then
    Exit;
  if (FileNames = nil) or (Captions = nil) or (Paths = nil) or (Hints = nil) then
    Exit;
  if (FileNames.Count <> Captions.Count) or (FileNames.Count <> Paths.Count) or (FileNames.Count <> Hints.Count) then
    Exit;

  FLoadingCount := 0; // Reset loading counter
  WasEmpty := (FAllFiles.Count = 0);

  // ==========================================================
  // 1. Add ALL to Master Lists (Updates Counts/PageCount)
  // ==========================================================
  for i := 0 to FileNames.Count - 1 do
  begin
    FAllFiles.Add(FileNames[i]);
    FAllCaptions.Add(Captions[i]);
    FAllPaths.Add(Paths[i]);
    FAllHints.Add(Hints[i]);
    FAllInfoTexts.Add(Infotxts[i]);
    if Assigned(SmallPicIndices) and (i < SmallPicIndices.Count) then
      FAllSmallPicIndices.Add(Pointer(SmallPicIndices[i]))
    else
      FAllSmallPicIndices.Add(Pointer(-1));
  end;

  // ==========================================================
  // 2. Spawn Loading Threads ONLY for items on CURRENT PAGE
  // ==========================================================
  for i := 0 to FileNames.Count - 1 do
  begin
    // Calculate Absolute Index in Master List
    NewAbsIndex := FAllFiles.Count - FileNames.Count + i;
    TargetPage := NewAbsIndex div FPageSize;

    // --- AutoScroll Logic ---
    if FAutoScrollPageForNewAdded then
    begin
      if FCurrentPage <> TargetPage then
      begin
        // Switch page immediately. ShowPage will load these images.
        ShowPage(TargetPage);
        // Do NOT spawn threads here, ShowPage handles it.
        Continue;
      end;
    end;

    if WasEmpty then
    begin
      ShowPage(FCurrentPage);
    end
    else
    begin
    // --- Lazy Loading Logic ---
    // Only spawn a thread if this specific image belongs to the CURRENT page
      if (NewAbsIndex >= GetPageStartIndex) and (NewAbsIndex <= GetPageEndIndex) then
      begin
      // Retrieve SmallPic Index from Master List
        CurrentSmallIndex := Integer(FAllSmallPicIndices[NewAbsIndex]);

      // Create and Start Thread
        LoadThread := TImageLoadThread.Create(FileNames[i], Captions[i], Paths[i], Hints[i], Infotxts[i], Self, NewAbsIndex, CurrentSmallIndex);
        LoadThread.Priority := FThreadPriority;
        FLoadingThreads.Add(LoadThread);
        Inc(FLoadingCount);
      end;
    end;
    // Else: Image is off-page. It stays in Master Lists but is NOT loaded.
    // It will be loaded later when you navigate to that page.
  end;
end;

procedure TSkFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean = false);
begin
  Clear(animated, ZoominSelected, Rect(0, 0, 0, 0), Rect(0, 0, 0, 0), iesFromBottom);
end;

procedure TSkFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom; AndFree: Boolean = true);
var
  i: Integer;
  ImageItem: TImageItem;
  TargetRect: TRect;
begin
  if (FImages.Count = 0) or FIsClearing then
    Exit;
  // 1. Instant Clear
  if (FImages.Count = 0) or FIsClearing then
    Exit;
  if not animated then
  begin
    FreeAllImagesAndClearLists;
    Exit;
  end;

  // 2. Stop any other interfering processes
 // StopAnimationThread; // Keep commented out if you don't want AV

  // 3. Setup Clearing State
  FIsClearing := True;

  FSpawningSmallPics := False;    //atm buggy

  FClearingStyle := FallingStyle;
  FClearingTargetPos := Point(FallingTargetPos.Left, FallingTargetPos.Top);
  FClearZoomSelected := ZoominSelected;
  // 4. Setup Targets for ALL images
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    // --- SETUP ANIMATION STATE ---
    ImageItem.AnimationProgress := 0.0;
    ImageItem.Animating := True;
    // --- ALPHA SETUP ---
    ImageItem.Alpha := ImageItem.Alpha; // Keep current alpha to start
    ImageItem.TargetAlpha := 0;       // Target is 0 (Invisible)
    // --- POSITION TARGETS ---
    ImageItem.StartRect := ImageItem.CurrentRect;
    // Calculate where this specific image should go
    TargetRect := CalculateClearingTarget(ImageItem, FallingTargetPos, FallingStyle, ZoominSelected, SelectedTargetPos, i);
    ImageItem.TargetRect := TargetRect;
  end;
  // 5. Start the engine and return immediately
  // The thread will handle the movement and call FreeAllImagesAndClearLists when done.
  StartAnimationThread;
end;

function TSkFlowmotion.CalculateClearingTarget(ImageItem: TImageItem; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle; ZoominSelected: Boolean; SelectedTargetPos: TRect; Index: Integer): TRect;
var
  W, H: Integer;
  TargetCX, TargetCY: Integer;
begin
  W := ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left;
  H := ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top;

  if ZoominSelected and ImageItem.IsSelected and not IsRectEmpty(SelectedTargetPos) then
  begin
    Result := SelectedTargetPos;
  end
  else
  begin
    case FallingStyle of
      // === FORCE TARGETS FAR OFF SCREEN (+10000px) ===
      iesFromTop:
        Result := Rect(ImageItem.CurrentRect.Left, -H - 10000, ImageItem.CurrentRect.Right, -10000);
      iesFromBottom:
        Result := Rect(ImageItem.CurrentRect.Left, Trunc(Height) + 10000, ImageItem.CurrentRect.Right, Trunc(Height) + H + 10000);
      iesFromLeft:
        Result := Rect(-W - 10000, ImageItem.CurrentRect.Top, -10000, ImageItem.CurrentRect.Bottom);
      iesFromRight:
        Result := Rect(Trunc(Width) + 10000, ImageItem.CurrentRect.Top, Trunc(Width) + W + 10000, ImageItem.CurrentRect.Bottom);
      iesFromTopLeft:
        Result := Rect(-W - 10000, -H - 10000, -10000, -10000);
      iesFromTopRight:
        Result := Rect(Trunc(Width) + 10000, -H - 10000, Trunc(Width) + W + 10000, -10000);
      iesFromBottomLeft:
        Result := Rect(-W - 10000, Trunc(Height) + 10000, -10000, Trunc(Height) + H + 10000);
      iesFromBottomRight:
        Result := Rect(Trunc(Width) + 10000, Trunc(Height) + 10000, Trunc(Width) + W + 10000, Trunc(Height) + H + 10000);
      iesRandom:
        begin
          // Pick a random direction far away
          case Random(8) of
            0:
              Result := Rect(ImageItem.CurrentRect.Left, -H - 10000, ImageItem.CurrentRect.Right, -10000);
            1:
              Result := Rect(ImageItem.CurrentRect.Left, Trunc(Height) + 10000, ImageItem.CurrentRect.Right, Trunc(Height) + H + 10000);
            2:
              Result := Rect(-W - 10000, ImageItem.CurrentRect.Top, -10000, ImageItem.CurrentRect.Bottom);
            3:
              Result := Rect(Trunc(Width) + 10000, ImageItem.CurrentRect.Top, Trunc(Width) + W + 10000, ImageItem.CurrentRect.Bottom);
            4:
              Result := Rect(-W - 10000, -H - 10000, -10000, -10000);
            5:
              Result := Rect(Trunc(Width) + 10000, -H - 10000, Trunc(Width) + W + 10000, -10000);
            6:
              Result := Rect(-W - 10000, Trunc(Height) + 10000, -10000, Trunc(Height) + H + 10000);
            7:
              Result := Rect(Trunc(Width) + 10000, Trunc(Height) + 10000, Trunc(Width) + W + 10000, Trunc(Height) + H + 10000);
          else
            Result := Rect(ImageItem.CurrentRect.Left, Trunc(Height) + 10000, ImageItem.CurrentRect.Right, Trunc(Height) + H + 10000);
          end;
        end;
      iesFromCenter:
        begin
          // Move to far corner
          Result := Rect(Trunc(Width) + 10000, Trunc(Height) + 10000, Trunc(Width) + W + 10000, Trunc(Height) + H + 10000);
        end;
      iesFromPoint:
        if not IsRectEmpty(FallingTargetPos) then
        begin
          TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
          TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
          Result := Rect(TargetCX - W div 2, TargetCY - H div 2, TargetCX + W div 2, TargetCY + H div 2);
        end
        else
          Result := Rect(ImageItem.CurrentRect.Left, Trunc(Height) + 10000, ImageItem.CurrentRect.Right, Trunc(Height) + H + 10000); // Fallback
    else
      Result := ImageItem.CurrentRect; // Fallback
    end;
  end;
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
  HandleRect, CapRect, SmallPicRect, InfoEdgeHitRect, InfoRectInt: TRect;
  VisualRect: TRectF;
  CheckPt: TPointF;
  CapHeight, L, T, R, B, IconW, IconH, Margin: Integer;
  OldSelected: TImageItem;
  IconBmp: TBitmap;
  // --- Vars for Logic ---
  DrawAngle: Single;
  HitTestRect: TRect;
  ClickAngle: Single;
  InfoRectF: TRectF;
  ActDir: TInfoPanelDirection;
  LocalHitRectF: TRectF;
  LocalPt: TPointF;
  SafeY_Top, SafeY_Bottom, SafeX_Left, SafeX_Right: Single;
begin
  // ==========================================================
  // 0. START GESTURE TRACKING (Put this first!)
  // ==========================================================
  FGestureActive := False;
  if (Button = TMouseButton.mbLeft) and Assigned(FSelectedImage) then
  begin
    FGestureActive := True;
    FGestureStartPos := TPointF.Create(X, Y);
  end;
  // ==========================================================
  // 1. FORCE FOCUS
  // ==========================================================
  SetFocus;
  inherited MouseDown(Button, Shift, X, Y);

  if FIsClearing then
    Exit;
  if (FImages.Count = 0) or FisClearing or FInFallAnimation then
    Exit;

  // ==========================================================
  // 2. CAPTURE STATE
  // ==========================================================
  OldSelected := FSelectedImage;
  ImageItem := GetImageAtPoint(X, Y);
  ItemIndex := FImages.IndexOf(ImageItem);

  // ==========================================================
  // 3. BACKGROUND CLICK
  // ==========================================================
  if (Button = TMouseButton.mbLeft) and (ImageItem = nil) then
  begin
    if FSelectedImage <> nil then
      SetSelectedImage(nil, -1);
    if FEnableParticlesOnMouseClick then
      SpawnParticles(X, Y, 20, FParticleColor);
    Exit;
  end;

  // ==========================================================
  // 4. ROTATION HANDLE CLICK (Priority 1)
  // ==========================================================
  if (Button = TMouseButton.mbLeft) and FRotationAllowed and Assigned(FSelectedImage) and (not FIsZoomedToFill) then
  begin
    VisualRect := GetVisualRect(FSelectedImage);
    L := Round(VisualRect.Left);
    T := Round(VisualRect.Top);
    R := Round(VisualRect.Right);
    B := Round(VisualRect.Bottom);
    HandleRect := GetRotateHandleRect(rect(L, T, R, B));

    // --- CALCULATE CLICK ANGLE ---
    DrawAngle := FSelectedImage.FActualRotation;
    if FBreathingEnabled and BreathRotationEnabled and (Abs(DrawAngle) > 0.1) then
      DrawAngle := DrawAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
    CheckPt := GetLocalPoint(TPointF.Create(X, Y), VisualRect, DrawAngle);

    // >>> BIG HIT AREA FOR BETTER CLICK <<<
    HitTestRect := HandleRect;
    InflateRect(HitTestRect, 15, 15);

    if PtInRect(HitTestRect, Point(Round(CheckPt.X), Round(CheckPt.Y))) then
    begin
      FIsRotating := True;
      FRotatingImage := FSelectedImage;
      FLastMouseAngle := ArcTan2(Y - ((VisualRect.Top + VisualRect.Bottom) / 2), X - ((VisualRect.Left + VisualRect.Right) / 2)) * (180 / Pi);
      if FEnableParticlesOnMouseClick then
        SpawnParticles(X, Y, 20, FParticleColor);
      Exit;
    end;
  end;


  // ==========================================================
  // 6. INFO PANEL TOGGLE (Corrected Screen Coords + Corner Exclusion + Rotation)
  // ==========================================================
  if (Button = TMouseButton.mbLeft) and Assigned(FSelectedImage) and (FSelectedImage.InfoText <> '') then
  begin
    // >>> UPDATE L, T, R, B FOR SELECTED IMAGE <<<
    VisualRect := GetVisualRect(FSelectedImage);
    L := Round(VisualRect.Left);
    T := Round(VisualRect.Top);
    R := Round(VisualRect.Right);
    B := Round(VisualRect.Bottom);

    // --- CALCULATE MARGIN (Clear Corners/Dots) ---
    Margin := FSmallpicMargin;
    if FRoundEdges > 0 then
      Margin := Margin + Round(FRoundEdges * 0.7);

    // --- CALCULATE DIRECTION (Auto) ---
    ActDir := FInfoPanelDirection;
    if ActDir = ipdAuto then
    begin
      if VisualRect.Width > VisualRect.Height then
        ActDir := ipdRight
      else
        ActDir := ipdBottom;
    end;

    // -------------------------------------------------
    // CASE A: PANEL IS OPEN -> TRY TO HIDE (Click Body)
    // -------------------------------------------------
    if FSelectedImage.FIsInfoShowing then
    begin
      // Check Panel Body (Full Black Box)
      InfoRectF := GetInfoPanelRect(FSelectedImage, VisualRect);
      InfoRectInt := Rect(Round(InfoRectF.Left), Round(InfoRectF.Top), Round(InfoRectF.Right), Round(InfoRectF.Bottom));

      // Use Local Point for rotation support
      DrawAngle := FSelectedImage.FActualRotation;
      if FBreathingEnabled and BreathRotationEnabled and (Abs(DrawAngle) > 0.1) then
        DrawAngle := DrawAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
      LocalPt := GetLocalPoint(TPointF.Create(X, Y), VisualRect, DrawAngle);

      if PtInRect(InfoRectInt, Point(Round(LocalPt.X), Round(LocalPt.Y))) then
      begin
        if FEnableParticlesOnMouseClick then
          SpawnParticles(X, Y, 20, FParticleColor);
        ShowInfoPanel(FSelectedImage); // Toggle (Hide)
        Exit;
      end;
    end
    // -------------------------------------------------
    // CASE B: PANEL IS CLOSED -> TRY TO SHOW (Click Edge Strip)
    // -------------------------------------------------
    else
    begin
      // >>> CORNER EXCLUSION LOGIC (Spare Out Edges) <<<
      var HandleSize: Single;
      HandleSize := FRotateHandleSize;

      // Define Safe Zone (Margin + HandleSize) from corners
      SafeY_Top := T + Margin + HandleSize;
      SafeY_Bottom := B - Margin - HandleSize;
      SafeX_Left := L + Margin + HandleSize;
      SafeX_Right := R - Margin - HandleSize;

      // --- 1. CALCULATE LOCAL HIT STRIP (Fixed relative to VisualRect) ---
      LocalHitRectF := RectF(0, 0, 0, 0);
      case ActDir of
        ipdLeft:   // Panel Left. Click Left Edge.
          LocalHitRectF := RectF(VisualRect.Left, VisualRect.Top, VisualRect.Left + 30, VisualRect.Bottom);
        ipdRight:  // Panel Right. Click Right Edge.
          LocalHitRectF := RectF(VisualRect.Right - 30, VisualRect.Top, VisualRect.Right, VisualRect.Bottom);
        ipdTop:    // Panel Top. Click Top Edge.
          LocalHitRectF := RectF(VisualRect.Left, VisualRect.Top, VisualRect.Right, VisualRect.Top + 30);
        ipdBottom: // Panel Bottom. Click Bottom Edge.
          LocalHitRectF := RectF(VisualRect.Left, VisualRect.Bottom - 30, VisualRect.Right, VisualRect.Bottom);
      end;

      // --- 2. TRANSFORM MOUSE (Screen) -> MOUSE (Local Image) ---
      DrawAngle := FSelectedImage.FActualRotation;
      if FBreathingEnabled and BreathRotationEnabled and (Abs(DrawAngle) > 0.1) then
        DrawAngle := DrawAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
      LocalPt := GetLocalPoint(TPointF.Create(X, Y), VisualRect, DrawAngle);

      // --- 3. APPLY MARGIN CHECK (In Local Space) ---
      if PtInRect(Rect(Round(LocalHitRectF.Left), Round(LocalHitRectF.Top), Round(LocalHitRectF.Right), Round(LocalHitRectF.Bottom)), Point(Round(LocalPt.X), Round(LocalPt.Y))) then
      begin
        // Check Specific Safe Zones to avoid clicking Rotate/SmallPic
        if (ActDir = ipdLeft) or (ActDir = ipdRight) then
        begin
          if (LocalPt.Y <= SafeY_Top) or (LocalPt.Y >= SafeY_Bottom) then
            Exit; // Clicked in Top/Right/Bottom-Right corner zones
        end
        else // Top/Bottom
        begin
          if (LocalPt.X <= SafeX_Left) or (LocalPt.X >= SafeX_Right) then
            Exit; // Clicked in Top-Left/Top-Right corner zones
        end;
        if FEnableParticlesOnMouseClick then
          SpawnParticles(X, Y, 20, FParticleColor);
        ShowInfoPanel(FSelectedImage); // Toggle (Show)
        Exit;
      end;
    end;
  end;

  // ==========================================================
  // 7. CAPTION CLICK
  // ==========================================================
  if (Button = TMouseButton.mbLeft) and ShowCaptions then
  begin
    if ImageItem <> nil then
    begin
      CapHeight := Round(FCaptionFont.Size * 1.4) + 4;
      CapRect.Left := Round(ImageItem.CurrentRect.Left);
      CapRect.Right := Round(ImageItem.CurrentRect.Right);
      CapRect.Bottom := Round(ImageItem.CurrentRect.Bottom);
      CapRect.Top := CapRect.Bottom - CapHeight - FCaptionOffsetY;
      if PtInRect(CapRect, Point(Round(X), Round(Y))) then
      begin
        if Assigned(FOnCaptionClick) then
          FOnCaptionClick(Self, ImageItem, ItemIndex);
        if FEnableParticlesOnMouseClick then
          SpawnParticles(X, Y, 20, FParticleColor);
        Exit;
      end;
    end;
  end;

  // ==========================================================
  // 8. SMALLPIC CLICK
  // ==========================================================
  if (Button = TMouseButton.mbLeft) and FSmallPicVisible then
  begin
    if ImageItem <> nil then
    begin
      Margin := FSmallpicMargin;
      if FRoundEdges > 0 then
        Margin := Margin + Round(FRoundEdges * 0.7);
      VisualRect := GetVisualRect(ImageItem);
      L := Round(VisualRect.Left);
      T := Round(VisualRect.Top);
      R := Round(VisualRect.Right);
      B := Round(VisualRect.Bottom);

      if (ImageItem.SmallPicIndex >= 0) and (FSmallPicImageList <> nil) and (ImageItem.SmallPicIndex < FSmallPicImageList.Count) then
      begin
        IconBmp := GetSmallPicBitmap(ImageItem.SmallPicIndex);
        if Assigned(IconBmp) then
        begin
          IconW := IconBmp.Width;
          IconH := IconBmp.Height;

          case FSmallPicPosition of
            spTopLeft:
              begin
                SmallPicRect.Left := L + Margin;
                SmallPicRect.Top := T + Margin;
                SmallPicRect.Right := SmallPicRect.Left + IconW;
                SmallPicRect.Bottom := SmallPicRect.Top + IconH;
              end;
            spTopRight:
              begin
                SmallPicRect.Right := R - Margin;
                SmallPicRect.Top := T + Margin;
                SmallPicRect.Left := SmallPicRect.Right - IconW;
                SmallPicRect.Bottom := SmallPicRect.Top + IconH;
              end;
            spBottomLeft:
              begin
                SmallPicRect.Left := L + Margin;
                SmallPicRect.Bottom := B - Margin;
                SmallPicRect.Top := SmallPicRect.Bottom - IconH;
                SmallPicRect.Right := SmallPicRect.Left + IconW;
              end;
            spBottomRight:
              begin
                SmallPicRect.Right := R - Margin;
                SmallPicRect.Bottom := B - Margin;
                SmallPicRect.Left := SmallPicRect.Right - IconW;
                SmallPicRect.Top := SmallPicRect.Bottom - IconH;
              end;
          end;

          if PtInRect(SmallPicRect, Point(Round(X), Round(Y))) then
          begin
            if Assigned(FOnSmallPicClick) then
              FOnSmallPicClick(Self, ImageItem, ItemIndex);
            if FEnableParticlesOnMouseClick then
              SpawnParticles(X, Y, 20, FParticleColor);
            Exit;
          end;
        end;
      end;
    end;
  end;

  // ==========================================================
  // 9. DOUBLE CLICK
  // ==========================================================
  if (Button = TMouseButton.mbLeft) and (ssDouble in Shift) then
  begin
    if ImageItem <> nil then
    begin
      if FSelectedImage <> ImageItem then
        SetSelectedImage(ImageItem, ItemIndex);
      if Assigned(FOnSelectedImageDblClick) then
        FOnSelectedImageDblClick(Self, ImageItem, ItemIndex);
    end;
    if FEnableParticlesOnMouseClick then
      SpawnParticles(X, Y, 20, FParticleColor);
    Exit;
  end;

  // ==========================================================
  // 10. LEFT CLICK ON IMAGE (Select/Drag)
  // ==========================================================
  if Button = TMouseButton.mbLeft then
  begin
    if Assigned(FOnSelectedItemMouseDown) then
      FOnSelectedItemMouseDown(Self, ImageItem, ItemIndex, Round(X), Round(Y), Button, Shift);

    // 1. FREEFLOAT MODE
    if FFlowLayout = flFreeFloat then
    begin
      FDraggingImage := True;
      FDraggedImage := ImageItem;
      var VisRect: TRectF;
      VisRect := GetVisualRect(ImageItem);
      FDragOffset.X := Round(X) - Round((VisRect.Left + VisRect.Right) / 2);
      FDragOffset.Y := Round(Y) - Round((VisRect.Top + VisRect.Bottom) / 2);
      if FSelectedImage <> ImageItem then
        SetSelectedImage(ImageItem, ItemIndex);
      if FEnableParticlesOnMouseClick then
        SpawnParticles(X, Y, 20, FParticleColor);
      Exit;
    end
    else // SORTED LAYOUT MODE
    begin
      // Change selection?
      if FSelectedImage <> ImageItem then
      begin
        if FSelectedImage <> nil then
        begin
          FSelectedImage.IsSelected := False;
          if FSelectedImage = FWasSelectedItem then
            FWasSelectedItem := nil;
        end;
        SetSelectedImage(ImageItem, ItemIndex);
      end;

      // HANDLE DRAGGING TO PREVENT FLICKER
      if FSelectedMovable and (ImageItem = FSelectedImage) and (ImageItem = OldSelected) and (not FIsZoomedToFill) then
      begin
        FDraggingSelected := True;
        FDragOffset.X := Round(X) - ((ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2);
        FDragOffset.Y := Round(Y) - ((ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2);
      end
      else if FSelectedImage = ImageItem then
      begin
         // Selected but not dragging (new selection or breathing update)
        if (not FDraggingImage) and (FFlowLayout <> flFreeFloat) then
          FBreathingPhase := FBreathingPhase - 0.4;
      end;
    end;

    // ==========================================================
    // === DELETE LOGIC
    // ==========================================================
    // Check if property is enabled and Image is selected
    if FDeleteClicked and (FSelectedImage <> nil) then
    begin
      if ImageItem = FSelectedImage then
      begin
        //animated destroy
        // DoAnimateBoom(ImageItem, False);

        if (FSelectedImage.FImageIndex >= 0) and (FSelectedImage.FImageIndex < FAllFiles.Count) then
        begin
          FAllFiles.Delete(FSelectedImage.FImageIndex);
          FAllCaptions.Delete(FSelectedImage.FImageIndex);
          FAllPaths.Delete(FSelectedImage.FImageIndex);
          FAllHints.Delete(FSelectedImage.FImageIndex);
          FAllSmallPicIndices.Delete(FSelectedImage.FImageIndex);
        end;

        // Remove from Visual List (FImages)
        FImages.Delete(FImages.IndexOf(FSelectedImage));

        SpawnParticles(X, Y, 200, FParticleColor);

        // Reset State (Clear Selection)
        FSelectedImage := nil;
        FCurrentSelectedIndex := -1;
        FHotItem := nil;
        Hint := '';

        // Trigger layout recalculation
        CalculateLayout;
        Repaint;

        // Exit immediately, do NOT run SelectNextImage / Hot tracking
        Exit;
      end;
    end;
  end;

  // ==========================================================
  // 11. PARTICLES ON CLICK (Final)
  // ==========================================================
  if FEnableParticlesOnMouseClick then
    if (Button = TMouseButton.mbLeft) and (ImageItem <> nil) then
      SpawnParticles(X, Y, 20, FParticleColor);

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSkFlowmotion.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ImageUnderCursor: TImageItem;
  TargetAngle: Single;
begin
  if FisClearing or FInFallAnimation then
    Exit;

  // ==========================================================
  // 0. RESET GESTURE
  // ==========================================================
  FGestureActive := False;

   // ==========================================================
  // FEATURE: MIDDLE CLICK
  // ==========================================================
  if (Button = TMouseButton.mbMiddle) then
  begin
    // >>> CHECK PROPERTY <<<
    if FShowInfoOnMiddleClick then
    begin
      if Assigned(FSelectedImage) then
      begin
        var VisRect: TRectF;
        var L, T, R, B: Integer;
        var HandleRect: TRect;
        var CheckPt: TPointF;
        var HitAngle: Single;

        VisRect := GetVisualRect(FSelectedImage);
        L := Round(VisRect.Left);
        T := Round(VisRect.Top);
        R := Round(VisRect.Right);
        B := Round(VisRect.Bottom);
        HandleRect := GetRotateHandleRect(rect(L, T, R, B));

        // --- CHECK ROTATE HANDLE ---
        HitAngle := FSelectedImage.FActualRotation;
        if FBreathingEnabled and BreathRotationEnabled and (Abs(HitAngle) > 0.1) then
          HitAngle := HitAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);

        CheckPt := GetLocalPoint(TPointF.Create(X, Y), VisRect, HitAngle);

        if PtInRect(HandleRect, Point(Round(CheckPt.X), Round(CheckPt.Y))) then
        begin
          // Handle Clicked -> Reset Rotation
          if FIsRotating then
            FIsRotating := False;

          TargetAngle := FStartingAngle;
          if TargetAngle = -1 then
            TargetAngle := 0;
          FSelectedImage.FTargetRotation := TargetAngle;
          if FEnableParticlesOnMouseClick then
            SpawnParticles(X, Y, 20, FParticleColor);
          Exit;
        end;

        // --- CHECK IMAGE BODY ---
        if GetImageAtPoint(X, Y) = FSelectedImage then
        begin
          // Image Body Clicked -> Toggle Info Panel
          ShowInfoPanel(FSelectedImage);
          Exit;
        end;
      end;
    end;
    Exit;
  end;


  // ==========================================================
  // 1. Handle Rotation Stop (Left Button Release)
  // ==========================================================
  if FIsRotating then
  begin
    if Assigned(FSelectedImage) then
      FSelectedImage.FTargetRotation := FSelectedImage.FActualRotation;
    FIsRotating := False;
    MouseMove(Shift, X, Y);
    if Assigned(FSelectedImage) then
    begin
      FSelectedImage.FGlitchIntensity := 1.0;
      if FEnableParticlesOnMouseClick then
        SpawnParticles(X, Y, 50, TAlphaColors.Red);
    end;
    Exit;
  end;

  if (Button = TMouseButton.mbLeft) then
  begin
    // 2. Stop FreeFloat Dragging
    if FDraggingImage then
    begin
      FDraggingImage := False;
      if FDraggedImage <> nil then
      begin
        FDraggedImage.FHotZoomTarget := 1.0;
        FDraggedImage := nil;
      end;
      ImageUnderCursor := GetImageAtPoint(X, Y);
      if (ImageUnderCursor <> nil) then
      begin
        FHotItem := ImageUnderCursor;
        StartAnimationThread;
      end;
    end;
    // 3. Stop Selected Image Dragging
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

   //Tactile feedback
    if FSelectedImage <> nil then
    begin
      if FSelectedImage.FHotZoom >= 1.1 then
        FSelectedImage.FHotZoomTarget := FSelectedImage.FHotZoom - 0.02;
      FBreathingPhase := FBreathingPhase - 0.4;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSkFlowmotion.MouseMove(Shift: TShiftState; X, Y: Single);
var
  CurrentAngle: Single;
  ImageItem: TImageItem;
  NewHot: TImageItem;
  NewCenterX, NewCenterY: Integer;
  CenterP: TPointF;
  HandleRect: TRect;
  VisualRect: TRectF;
  CheckPt: TPointF;
  L, T, R, B: Integer;
  // --- VARIABLES FOR ACTIVATION ZONE SCALING ---
  SelectedCenter: TPointF;
  NearestZoneDist: Single;
  ScaleFactor: Single;
  ScaleDistance: Single;
  iZone: Integer;
  ZoneRect: TRectF;
  ZoneCenter: TPointF;
begin
  if FisClearing or FInFallAnimation then
    Exit;

  ScaleDistance := 380.0;

  // ==========================================================
  // GESTURE LOGIC: Check for Swipes (Show Info Panel)
  // ==========================================================
  // ONLY RUNS IF WE STARTED A CLICK IN MOUSEDOWN and Fullscreen
  if FGestureActive and Assigned(FSelectedImage) and (FIsZoomedToFill or (not SelectedMovable)) then
  begin
    var SwipeDist: Single;
    var ActualDir: TInfoPanelDirection;
    var TriggerPanel: Boolean;

    SwipeDist := 200.0; // How many pixels to swipe
    TriggerPanel := False;

    // Resolve Auto Direction (Portrait vs Landscape)
    ActualDir := FInfoPanelDirection;
    if ActualDir = ipdAuto then
    begin
      if FSelectedImage.CurrentRect.Width > FSelectedImage.CurrentRect.Height then
        ActualDir := ipdRight // Landscape -> Panel Right
      else
        ActualDir := ipdBottom; // Portrait -> Panel Bottom
    end;

    // Check Swipes based on Direction
    // Logic: We check the delta (Difference) from Start Point
    case ActualDir of
      ipdLeft:   // Panel Left.  Swipe Right (X Increases) to open.
        if (X - FGestureStartPos.X) > SwipeDist then
          TriggerPanel := True;

      ipdRight:  // Panel Right. Swipe Left (X Decreases) to open.
        if (FGestureStartPos.X - X) > SwipeDist then
          TriggerPanel := True;

      ipdTop:    // Panel Top.   Swipe Down (Y Increases) to open.
        if (Y - FGestureStartPos.Y) > SwipeDist then
          TriggerPanel := True;

      ipdBottom: // Panel Bottom. Swipe Up (Y Decreases) to open.
        if (FGestureStartPos.Y - Y) > SwipeDist then
          TriggerPanel := True;
    end;

    // If Valid Swipe Detected -> Toggle Panel
    if TriggerPanel then
    begin
      ShowInfoPanel(FSelectedImage);
      if FEnableParticlesOnMouseClick then
        SpawnParticles(X, Y, 50, TAlphaColors.Red);
      // Stop tracking until next click
      FGestureActive := False;
    end;
  end;

  // --- 1. ROTATION (Active) ---
  if not FIsZoomedToFill then
    if FIsRotating and Assigned(FRotatingImage) then
    begin
      CenterP := TPointF.Create((FRotatingImage.CurrentRect.Left + FRotatingImage.CurrentRect.Right) / 2, (FRotatingImage.CurrentRect.Top + FRotatingImage.CurrentRect.Bottom) / 2);
      CurrentAngle := ArcTan2(Y - CenterP.Y, X - CenterP.X) * (180 / Pi);
      FRotatingImage.FActualRotation := FRotatingImage.FActualRotation + (CurrentAngle - FLastMouseAngle);
      FLastMouseAngle := CurrentAngle;

      // Glitch while rotating
      FRotatingImage.FGlitchIntensity := 1.0;

      Cursor := crSizeNWSE;
      Repaint;
      Exit;
    end;

  // --- 2. DRAGGING FREEFLOAT ---
  if (FFlowLayout = flFreeFloat) and FDraggingImage and (FDraggedImage <> nil) then
  begin
    ImageItem := FDraggedImage;
    NewCenterX := Round(X) - FDragOffset.X;
    NewCenterY := Round(Y) - FDragOffset.Y;
    with ImageItem.TargetRect do
      ImageItem.TargetRect := Rect(NewCenterX - (Right - Left) div 2, NewCenterY - (Bottom - Top) div 2, NewCenterX + (Right - Left) div 2, NewCenterY + (Bottom - Top) div 2);
    ImageItem.CurrentRect := ImageItem.TargetRect;

    // === RECALCULATE LAYOUT SO OTHERS MOVE OUT OF WAY ===
    // We call CalculateLayout here. With the "Preserve Position" check below,
    // existing items stay, but other items are re-placed around the dragged one.
    CalculateLayout;

    StartAnimationThread;
    Repaint;
    Exit;
  end;

  // --- 3. DRAGGING SELECTED (Sorted or FreeFloat Selected) ---
  // Note: In Sorted mode, we usually drag to re-position. In FreeFloat, same logic.
  if FDraggingSelected and (FSelectedImage <> nil) then
  begin
    ImageItem := FSelectedImage;
    NewCenterX := Round(X) - FDragOffset.X;
    NewCenterY := Round(Y) - FDragOffset.Y;
    with ImageItem.TargetRect do
      ImageItem.TargetRect := Rect(NewCenterX - (Right - Left) div 2, NewCenterY - (Bottom - Top) div 2, NewCenterX + (Right - Left) div 2, NewCenterY + (Bottom - Top) div 2);
    ImageItem.CurrentRect := ImageItem.TargetRect;

    // === RECALCULATE LAYOUT SO OTHERS MOVE OUT OF WAY ===
    CalculateLayout;

    StartAnimationThread;
    Repaint;

    // ==========================================================
    // ACTIVATION ZONE SCALING (Proximity Effect)
    // ==========================================================
    if (Length(FActivationZones) > 0) then
    begin
      // Get center of the image we are dragging
      SelectedCenter := TPointF.Create((ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) / 2, (ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) / 2);

      // Find distance to nearest zone center
      NearestZoneDist := 99999;
      for iZone := 0 to High(FActivationZones) do
      begin
        ZoneRect := FActivationZones[iZone].Rect;
        ZoneCenter := TPointF.Create((ZoneRect.Left + ZoneRect.Right) / 2, (ZoneRect.Top + ZoneRect.Bottom) / 2);
        if Hypot(SelectedCenter.X - ZoneCenter.X, SelectedCenter.Y - ZoneCenter.Y) < NearestZoneDist then
          NearestZoneDist := Hypot(SelectedCenter.X - ZoneCenter.X, SelectedCenter.Y - ZoneCenter.Y);
      end;

      // Apply scaling based on distance (Closer = Smaller)
      if NearestZoneDist < ScaleDistance then
      begin
        ScaleFactor := 0.9 + 0.1 * (NearestZoneDist / ScaleDistance); // Scales from 1.0 down to 0.9
        ImageItem.FHotZoom := ScaleFactor;
        ImageItem.FHotZoomTarget := ScaleFactor; // Keep it there
      end
      else
      begin
        // Reset if not in zone
        if Abs(ImageItem.FHotZoom - 1.0) > 0.01 then
        begin
          ImageItem.FHotZoomTarget := 1.0;
          //ImageItem.FHotZoom := 1.0; // Force reset
        end;
      end;
    end;

    Exit;
  end;

  // --- 4. HOT TRACKING ---
  NewHot := GetImageAtPoint(X, Y);

  //not at zooming back from just max zoomed
  if not (Assigned(FWasSelectedItem) and (NewHot = FWasSelectedItem) and (FSelectedImage = nil) and ((FWasSelectedItem.ZoomProgress > 0.001) or (FWasSelectedItem.Animating))) then
    if (NewHot <> FHotItem) then
    begin
      if (NewHot <> nil) and Assigned(FOnImageMouseEnter) then
        FOnImageMouseEnter(Self, NewHot, FImages.IndexOf(NewHot));
      if (FHotItem <> nil) and (NewHot <> FHotItem) and Assigned(FOnImageMouseLeave) then
        FOnImageMouseLeave(Self, FHotItem, FImages.IndexOf(FHotItem));

      FHotItem := NewHot;

      if ShowHint and (FHotItem <> nil) and (FHotItem.Hint <> '') then
        Hint := FHotItem.Hint
      else
        Hint := '';

      if FHotItem <> nil then
      begin
        FHotItem.FHotZoomTarget := FHotZoomMaxFactor;
        StartAnimationThread;
      end;
    end
    else if (NewHot = nil) and (FHotItem <> nil) then
    begin
      FHotItem.FHotZoomTarget := 1.0;
      FHotItem := nil;
      Hint := '';
      StartAnimationThread;
    end;

  // --- 5. CHECK ROTATION HANDLE HOVER (Priority 1) ---
  if FRotationAllowed and Assigned(FSelectedImage) then
  begin
    VisualRect := GetVisualRect(FSelectedImage);
    L := Round(VisualRect.Left);
    T := Round(VisualRect.Top);
    R := Round(VisualRect.Right);
    B := Round(VisualRect.Bottom);
    HandleRect := GetRotateHandleRect(rect(L, T, R, B));

    // === CALCULATE HANDLE ANGLE (Must match Draw) ===
    var HandleAngle: Single;
    HandleAngle := FSelectedImage.FActualRotation;
    if FBreathingEnabled and BreathRotationEnabled and (Abs(HandleAngle) > 0.1) then
      HandleAngle := HandleAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
    CheckPt := GetLocalPoint(TPointF.Create(X, Y), VisualRect, HandleAngle); // Use calculated

    // >>> INFLATE HIT AREA FOR HOVER CURSOR <<<
    // We create a temporary larger rectangle for the hit test.
    // This matches the "easy click" zone we added to MouseDown.
    var HitTestRect: TRect;
    HitTestRect := HandleRect;
    InflateRect(HitTestRect, FRotateHandleSize, FRotateHandleSize);

    // >>> USE HitTestRect HERE FOR CURSOR <<<
    if not FIsZoomedToFill then
      if PtInRect(HitTestRect, Point(Round(CheckPt.X), Round(CheckPt.Y))) then
      begin
        Cursor := crSizeNWSE;
        FSelectedImage.FHotZoomTarget := 1.0;
        FIsMouseOverHandle := True;
        Exit;
      end
      else
      begin
        FIsMouseOverHandle := False;
      end;
  end;

  // ==========================================================
  // 6. CHECK INFO INDICATOR HOVER (Priority 1.5)
  // ==========================================================
  if Assigned(FSelectedImage) and (FSelectedImage.InfoText <> '') and (not FSelectedImage.FIsInfoShowing) and FShowInfoIndicator then
  begin
    VisualRect := GetVisualRect(FSelectedImage);

    // === CALCULATE ROTATION ANGLE (Must match Draw) ===
    // The arrow rotates WITH the image. We must rotate the mouse point INTO image space.
    var DrawAngle: Single;
    DrawAngle := FSelectedImage.FActualRotation;
    if FBreathingEnabled and BreathRotationEnabled and (Abs(DrawAngle) > 0.1) then
      DrawAngle := DrawAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);

    // === TRANSFORM MOUSE TO LOCAL SPACE ===
    // This rotates the mouse point (X,Y) around the center of VisualRect by -DrawAngle.
    // Result: CheckPt is where the mouse is relative to an un-rotated image.
    CheckPt := GetLocalPoint(TPointF.Create(X, Y), VisualRect, DrawAngle);

    // === DETERMINE DIRECTION & STRIP BOUNDS ===
    var ActDir: TInfoPanelDirection;
    ActDir := FInfoPanelDirection;
    if ActDir = ipdAuto then
    begin
      if VisualRect.Width > VisualRect.Height then
        ActDir := ipdRight
      else
        ActDir := ipdBottom;
    end;

    FIsMouseOverInfoIndicator := False; // Reset

    // Define Local Hit Strip
    var LocalHitRect: TRect;
    case ActDir of
      ipdLeft:
        LocalHitRect := Rect(Round(VisualRect.Left), Round(VisualRect.Top), Round(VisualRect.Left + 30), Round(VisualRect.Bottom));
      ipdRight:
        LocalHitRect := Rect(Round(VisualRect.Right - 30), Round(VisualRect.Top), Round(VisualRect.Right), Round(VisualRect.Bottom));
      ipdTop:
        LocalHitRect := Rect(Round(VisualRect.Left), Round(VisualRect.Top), Round(VisualRect.Right), Round(VisualRect.Top + 30));
      ipdBottom:
        LocalHitRect := Rect(Round(VisualRect.Left), Round(VisualRect.Bottom - 30), Round(VisualRect.Right), Round(VisualRect.Bottom));
    end;

    // === PERFORM HIT TEST ===
    // CheckPt is in local space, LocalHitRect is in local space (axis aligned).
    if PtInRect(LocalHitRect, Point(Round(CheckPt.X), Round(CheckPt.Y))) then
      FIsMouseOverInfoIndicator := True;
  end
  else
    FIsMouseOverInfoIndicator := False;

  // --- 7. CURSOR DEFAULTS (Priority 2) ---
  if FHotItem <> nil then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

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
// -----------------------------------------------------------------------------
// HELPER FOR ROTATION MATH
// -----------------------------------------------------------------------------
// Transforms a screen point (P) into the local coordinate system of a rotated rect

function TSkFlowmotion.GetLocalPoint(const P: TPointF; const Rect: TRectF; AngleDeg: Single): TPointF;
var
  Center, D: TPointF;
  Rad, CosA, SinA: Single;
begin
  Center.X := (Rect.Left + Rect.Right) / 2;
  Center.Y := (Rect.Top + Rect.Bottom) / 2;

  // Vector from center to point
  D.X := P.X - Center.X;
  D.Y := P.Y - Center.Y;

  // Inverse rotation matrix
  Rad := -AngleDeg * (PI / 180);
  CosA := Cos(Rad);
  SinA := Sin(Rad);

  // Rotate vector
  Result.X := D.X * CosA - D.Y * SinA;
  Result.Y := D.X * SinA + D.Y * CosA;

  // Translate back
  Result.X := Result.X + Center.X;
  Result.Y := Result.Y + Center.Y;
end;
// -----------------------------------------------------------------------------
// NAVIGATION HELPERS
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// SMART NAVIGATION (Spatial SelectNorth/South/etc.)
// --------------------------------------------------------------

function TSkFlowmotion.GetSpatialCandidate(CurrentImage: TImageItem; const Action: TSmartImageAction): TImageItem;
var
  i: Integer;
  Item: TImageItem;
  VisualRect: TRectF;
  CenterX, CenterY: Single;
  VisItem: TImageItem;
  CheckPt: TPointF; // Use TPointF (from System.Types)
  Dist, BestDist: Double;
begin
  Result := nil;

  // Center calculation is still Single, as it's easier to calculate center of a Rect
  CenterX := (CurrentImage.CurrentRect.Left + CurrentImage.CurrentRect.Right) / 2;
  CenterY := (CurrentImage.CurrentRect.Top + CurrentImage.CurrentRect.Bottom) / 2;

  // Initialize best distance to a large value
  BestDist := 999999;

  for i := 0 to FImages.Count - 1 do
  begin
    Item := TImageItem(FImages[i]);

    // Skip Self, Not Visible, or Currently Selected items
    if (Item = nil) or (Item = CurrentImage) or (not Item.Visible) or (Item = FSelectedImage) then
      Continue;

    // Get visual bounds (which includes wall sliding)
    VisualRect := GetVisualRect(Item);

    // Rotate Mouse into Image Local Space (Supports Rotation)
    if Item.FActualRotation <> 0 then
    begin
      // We use the TPointF.Create overload that takes X, Y
      CheckPt := TPointF.Create(CurrentImage.CurrentRect.Right, CurrentImage.CurrentRect.Bottom);
      // Rotate point around Item center
      CheckPt.X := CenterX - (CenterX - CheckPt.X) * Cos(Item.FActualRotation * (PI / 180)) + (CenterY - CheckPt.Y) * Sin(Item.FActualRotation * (PI / 180));
      CheckPt.Y := CenterY - (CenterY - CheckPt.Y * Cos(Item.FActualRotation * (PI / 180)) + (CenterX - CheckPt.X) * Sin(Item.FActualRotation * (PI / 180)));
    end
    else
    begin
      // No rotation, just copy directly
      CheckPt := TPointF.Create(CurrentImage.CurrentRect.Right, CurrentImage.CurrentRect.Bottom);
    end;

    // Calculate distance squared
    Dist := Hypot(CheckPt.X - VisualRect.Left - (VisualRect.Right - VisualRect.Left) / 2, CheckPt.Y - VisualRect.Top - (VisualRect.Bottom - VisualRect.Top) / 2);

    // Find closest item based on distance
    if (Dist < BestDist) then
    begin
      BestDist := Dist;
      Result := Item;
    end;
  end;
end;

procedure TSkFlowmotion.SelectNorth;
begin
  // Use Smart Spatial Selection
  var Candidate: TImageItem;
  Candidate := GetSpatialCandidate(FSelectedImage, siaSelectNorth);
  if Assigned(Candidate) then
  begin
    SetSelectedImage(Candidate, FImages.IndexOf(Candidate));
  end;
end;

procedure TSkFlowmotion.SelectSouth;
var
  Candidate: TImageItem;
begin
  Candidate := GetSpatialCandidate(FSelectedImage, siaSelectSouth);
  if Assigned(Candidate) then
  begin
    SetSelectedImage(Candidate, FImages.IndexOf(Candidate));
  end;
end;

procedure TSkFlowmotion.SelectWest;
var
  Candidate: TImageItem;
begin
  Candidate := GetSpatialCandidate(FSelectedImage, siaSelectWest);
  if Assigned(Candidate) then
  begin
    SetSelectedImage(Candidate, FImages.IndexOf(Candidate));
  end;
end;

procedure TSkFlowmotion.SelectEast;
var
  Candidate: TImageItem;
begin
  Candidate := GetSpatialCandidate(FSelectedImage, siaSelectEast);
  if Assigned(Candidate) then
  begin
    SetSelectedImage(Candidate, FImages.IndexOf(Candidate));
  end;
end;

function TSkFlowmotion.GetImageAtPoint(X, Y: Single): TImageItem;
var
  i: Integer;
  ImageItem: TImageItem;
  P, CheckPt: TPointF;
  DrawRect: TRectF;
  BestCandidate: TImageItem;
  BestCandidateZoom: Double;
  VisualRect: TRectF;
begin
  P := TPointF.Create(X, Y);
  { 1. SELECTED IMAGE CHECK (Highest Priority) }
  if (FSelectedImage <> nil) and FSelectedImage.Visible then
  begin
    VisualRect := GetVisualRect(FSelectedImage);
    // === CALCULATE HIT ANGLE (Must match Draw) ===
    var HitAngle: Single;
    HitAngle := FSelectedImage.FActualRotation;
    if FBreathingEnabled and BreathRotationEnabled and (Abs(HitAngle) > 0.1) then
      HitAngle := HitAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);
    // Transform mouse to local space if rotated
    if HitAngle <> 0 then
      CheckPt := GetLocalPoint(P, VisualRect, HitAngle) // Use calculated HitAngle
    else
      CheckPt := P;

    // Inflate slightly for border tolerance
    DrawRect := VisualRect;
    InflateRect(DrawRect, Max(FGlowWidth, FHotTrackWidth), Max(FGlowWidth, FHotTrackWidth));

    if DrawRect.Contains(CheckPt) then
    begin
      Result := FSelectedImage;
      Exit;
    end;
  end;

  { 2. ALL OTHER IMAGES (Z-Order Logic) }
  BestCandidate := nil;
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible or (ImageItem = FSelectedImage) then
      Continue;

    VisualRect := GetVisualRect(ImageItem);

    // Transform mouse to local space if rotated
    if ImageItem.ActualRotation <> 0 then
      CheckPt := GetLocalPoint(P, VisualRect, ImageItem.ActualRotation)
    else
      CheckPt := P;

    { Calculate Hit Rect with Border }
    DrawRect := VisualRect;
    if (ImageItem.FHotZoom > 1.01) or (ImageItem = FHotItem) then
      InflateRect(DrawRect, FHotTrackWidth, FHotTrackWidth);

    { Check Hit and Z-Order }
    if DrawRect.Contains(CheckPt) then
    begin
      if ImageItem.FHotZoom < 1.0 then
        BestCandidateZoom := 1.0
      else
        BestCandidateZoom := ImageItem.FHotZoom;

      { Pick the one with highest zoom (on top) }
      if (BestCandidateZoom > BestCandidateZoom) or ((BestCandidateZoom = BestCandidateZoom) and (BestCandidate <> nil) and (VisualRect.Width > 0) and (VisualRect.Height > 0) and (VisualRect.Width > GetVisualRect(BestCandidate).Width)) then
      begin
        BestCandidate := ImageItem;
      end
      else if BestCandidate = nil then
      begin
        BestCandidate := ImageItem;
      end;
    end;
  end;
  Result := BestCandidate;
end;

procedure TSkFlowmotion.ResetAllRotations;
var
  i: Integer;
begin
  // Iterate all visible images on current page
  for i := 0 to FImages.Count - 1 do
  begin
    // Set target rotation to 0 degrees (Straight Up)
    TImageItem(FImages[i]).FTargetRotation := 0;
  end;
  // Redraw to show changes immediately
  Repaint;
end;

procedure TSkFlowmotion.SelectNextImage;
begin
  if FPageChangeInProgress then
    Exit;

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
  if FPageChangeInProgress then
    Exit;

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
// -----------------------------------------------------------------------------
// SELECTED IMAGE LOGIC
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.SetSelectedImage(ImageItem: TImageItem; Index: Integer);
var
  OldSelected: TImageItem;
begin
  if ImageItem = nil then
    FHotItem := nil;
  { Prevent flickering: Don't interrupt mid-zoom }
  if (ImageItem = nil) and (FSelectedImage <> nil) and (FSelectedImage.ZoomProgress > 0.1) and (FSelectedImage.ZoomProgress < 1) then
    Exit;
  if FSelectedImage = ImageItem then
    Exit;
  OldSelected := FSelectedImage;
  { --- 1. Handle OLD selection (Zoom Out animation) --- }
  if OldSelected <> nil then
  begin
    OldSelected.IsSelected := False;
    //if OldSelected.FIsInfoShowing then
    //  OldSelected.FIsInfoShowing := False;
    { Reset zoom for smooth exit }
    if OldSelected.FHotZoom >= 1 then
      OldSelected.FHotZoomTarget := 1.1;
  end;
  FWasSelectedItem := FSelectedImage;
  if FWasSelectedItem <> nil then
  begin
    FWasSelectedItem.FAnimating := True;
    FIsZoomedToFill := False;

    if FWasSelectedItem = FHotItem then
      FWasSelectedItem.FHotZoomTarget := 1.0;
    { Start Zoom Out animation }
    if (FFlowLayout <> flFreeFloat) then
      StartZoomAnimation(FWasSelectedItem, False);
  end;
  { --- 2. Handle NEW selection --- }
  FSelectedImage := ImageItem;
  FCurrentSelectedIndex := Index;
  if ImageItem = nil then
  begin
    FCurrentSelectedIndex := -1;
    FHotItem := nil;
    FIsZoomedToFill := False;
  end
  else
  begin
    ImageItem.IsSelected := True;

    // If AlwaysShowInfo is active, trigger the panel immediately
    if FAlwaysShowInfo and (ImageItem.InfoText <> '') then
    begin
      ImageItem.FIsInfoShowing := True;
    end;

    ImageItem.ZoomProgress := 0;
    if ImageItem.FHotZoom < 1 then
      ImageItem.FHotZoomTarget := 1.0;
    { === RESET BREATHING PHASE === }
    // We reset the phase to 0.0 so the new image "breathes in" from the bottom (1.0).
    // This prevents it from snapping to 1.3 immediately if the phase was at a peak.
    if FBreathingEnabled then
    begin
      FBreathingPhase := 0;
      // Force target to 1.0 to ensure smooth Zoom In animation starts correctly.
      ImageItem.FHotZoomTarget := 1.0;
      FHotItem := ImageItem;
    end
    else
    begin
      ImageItem.FHotZoomTarget := 1.0;
      FHotItem := nil;
    end;
    FCurrentSelectedIndex := Index;
    ImageItem.AnimationProgress := 0;
    ImageItem.Animating := True;
  end;
  { --- 3. Recalculate Layout ---
  { This determines where the OLD image goes (Grid) and moves others for the NEW image }
  if (FFlowLayout <> flFreeFloat) then
    CalculateLayout;
  { --- 4. Start Zoom In animation for NEW image --- }
  if ImageItem <> nil then
    StartZoomAnimation(ImageItem, True);
  StartAnimationThread;
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, ImageItem, Index);
end;
// -----------------------------------------------------------------------------
// INSERTION & PERSISTENCE
// -----------------------------------------------------------------------------

procedure TSkFlowmotion.AddImagesWithPositions(const FileNames, Captions, Paths, Hints, Rotations: TStringList; const Positions: array of TImagePosition);
var
  i: Integer;
  SkImg: ISkImage;
  NewItem: TImageItem;
  PosRect: TRect;
begin
  if (FileNames = nil) or (Captions = nil) or (Paths = nil) or (FileNames.Count <> Length(Positions)) then
    Exit;

  for i := 0 to FileNames.Count - 1 do
  begin
    if TFile.Exists(FileNames[i]) then
    begin
      SkImg := TSkImage.MakeFromEncodedFile(FileNames[i]);
      if Assigned(SkImg) then
      begin
        NewItem := TImageItem.Create;
        NewItem.SkImage := SkImg;
        NewItem.Caption := Captions[i];
        NewItem.Path := Paths[i];
        NewItem.Hint := Hints[i];
        NewItem.FActualRotation := Strtoint(Rotations[i]);
        NewItem.FileName := FileNames[i];
        NewItem.Direction := GetEntryDirection;
        NewItem.ImageIndex := FAllFiles.Count - 1;
        NewItem.Visible := False;
        FImages.Add(NewItem);

        // Add to Master Lists
        FAllFiles.Add(FileNames[i]);
        FAllCaptions.Add(Captions[i]);
        FAllPaths.Add(Paths[i]);
        FAllHints.Add('');
        FAllSmallPicIndices.Add(Pointer(-1));

        if Visible then
        begin
          PosRect := Rect(Positions[i].Left, Positions[i].Top, Positions[i].Left + Positions[i].Width, Positions[i].Top + Positions[i].Height);
          if FFlowLayout = flFreeFloat then
          begin
            NewItem.TargetRect := PosRect;
            NewItem.OriginalTargetRect := PosRect;
            AnimateImage(NewItem, NewItem.Direction, True, PosRect);
          end
          else
          begin
            // In Sorted mode, the saved position is just a preference,
            // usually CalculateLayout overrides it. But we can try to force it.
            NewItem.TargetRect := PosRect;
            AnimateImage(NewItem, NewItem.Direction, False, Rect(0, 0, 0, 0));
          end;
          NewItem.Visible := True;
        end;
      end;
    end;
  end;
  StartAnimationThread;
end;

procedure TSkFlowmotion.InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath, XHint, XInfo: string; ASmallPicIndex: Integer = -1);
var
  NewItem: TImageItem;
  DummyName: string;
begin
  if Pic = nil then
    Exit;

  DummyName := 'MemoryBitmap_' + IntToStr(GetTickCount) + '_' + IntToStr(Random(10000));

  // Add to Master Lists manually to ensure Hint is saved
  FAllFiles.Add(DummyName);
  FAllCaptions.Add(XCaption);
  FAllPaths.Add(XPath);
  FAllHints.Add(XHint);
  FAllInfoTexts.Add(XInfo);
  FAllSmallPicIndices.Add(Pointer(ASmallPicIndex));

  // Create and Configure Item
  NewItem := TImageItem.Create;
  NewItem.SkImage := BitmapToSkImage(Pic);
  NewItem.FileName := DummyName;
  NewItem.Caption := XCaption;
  NewItem.Path := XPath;
  NewItem.Hint := XHint;
  NewItem.InfoText := XInfo;
  NewItem.ImageIndex := FAllFiles.Count - 1;
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
  StartAnimationThread;
end;

procedure TSkFlowmotion.InsertImageAsync(const FileName, Caption, Path, Hint, Info: string; ASmallPicIndex: Integer = -1);
var
  NewAbsIndex: Integer;
  LoadThread: TImageLoadThread;
  TargetPage: Integer;
  WasEmpty: Boolean;
begin
  // Check if this is the very first image added
  WasEmpty := (FAllFiles.Count = 0);

  { ==========================================================
  1. Add ALL to Master Lists (Updates Counts/PageCount)
  ========================================================== }
  FAllFiles.Add(FileName);
  FAllCaptions.Add(Caption);
  FAllPaths.Add(Path);
  FAllHints.Add(Hint);
  FAllInfoTexts.Add(Info);
  FAllSmallPicIndices.Add(Pointer(ASmallPicIndex));

  // Calculate the absolute index of the new item
  NewAbsIndex := FAllFiles.Count - 1;

  { ==========================================================
  2. Determine Loading Strategy
  ========================================================== }
  if WasEmpty then
  begin
    // If the list was empty, we must load the page immediately
    ShowPage(FCurrentPage);
  end
  else
  begin
    // Calculate which page this new image belongs to
    TargetPage := NewAbsIndex div FPageSize;

    // Only load if it is on the CURRENT page
    if (TargetPage = FCurrentPage) then
    begin
      // Create and Start Thread
      LoadThread := TImageLoadThread.Create(FileName, Caption, Path, Hint, Info, Self, NewAbsIndex, ASmallPicIndex);
      LoadThread.Priority := FThreadPriority;
      FLoadingThreads.Add(LoadThread);
      Inc(FLoadingCount);
    end;
    // Else: Image is off-page. It stays in Master Lists but is NOT loaded.
    // It will be loaded later when you navigate to that page.
  end;

  // Ensure animation/render loop is active
  StartAnimationThread;
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
var
  StartTick: Cardinal;
  AbsIndex: Integer;
  ImageItem: TImageItem;
  R: TRect;
  AllOut: Boolean;
  Speed: Integer;
  CurCX, CurCY, TargetCX, TargetCY, MoveX, MoveY: Integer;
begin
  // 1. Safety checks
  if (Index < 0) or (Index >= FImages.Count) then
    Exit;

  AbsIndex := GetPageStartIndex + Index;
  if (AbsIndex < 0) or (AbsIndex >= FAllFiles.Count) then
    Exit;

  ImageItem := TImageItem(FImages[Index]);

  // 2. Instant remove if not animated
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

    // Remove from Master Lists
    if (AbsIndex >= 0) and (AbsIndex < FAllSmallPicIndices.Count) then
      FAllSmallPicIndices.Delete(AbsIndex);
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

  // 3. Animated remove
  StopAnimationThread;
  ImageItem.Animating := True;

  StartTick := GetTickCount;
  Speed := Max(1, FAnimationSpeed * 2 - 8);

  repeat
    AllOut := True;
    R := ImageItem.CurrentRect;

    // Falling Logic (Simulated from VCL)
    case FallingStyle of
      iesFromTop:
        OffsetRect(R, 0, -Trunc(FAnimationSpeed * Speed));
      iesFromBottom:
        OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
      iesFromLeft:
        OffsetRect(R, -Trunc(FAnimationSpeed * Speed), 0);
      iesFromRight:
        OffsetRect(R, Trunc(FAnimationSpeed * Speed), 0);
      iesFromTopLeft:
        OffsetRect(R, -Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
      iesFromTopRight:
        OffsetRect(R, Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
      iesFromBottomLeft:
        OffsetRect(R, -Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
      iesFromBottomRight:
        OffsetRect(R, Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
      iesRandom:
        case Random(8) of
          0:
            OffsetRect(R, 0, -Trunc(FAnimationSpeed * Speed));
          1:
            OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
          2:
            OffsetRect(R, -Trunc(FAnimationSpeed * Speed), 0);
          3:
            OffsetRect(R, Trunc(FAnimationSpeed * Speed), 0);
          4:
            OffsetRect(R, -Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
          5:
            OffsetRect(R, Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
          6:
            OffsetRect(R, -Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
          7:
            OffsetRect(R, Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
        end;
      iesFromCenter:
        begin
          CurCX := Trunc(Width) div 2;
          CurCY := Trunc(Height) div 2;
          MoveX := Trunc((CurCX - (R.Left + R.Right) / 2) * 0.18);
          MoveY := Trunc((CurCY - (R.Top + R.Bottom) / 2) * 0.18);
          OffsetRect(R, MoveX, MoveY);
        end;
      iesFromPoint:
        if not IsRectEmpty(FallingTargetPos) then
        begin
          TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
          TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
          MoveX := Trunc((TargetCX - (R.Left + R.Right) / 2) * 0.20);
          MoveY := Trunc((TargetCY - (R.Top + R.Bottom) / 2) * 0.20);
          OffsetRect(R, MoveX, MoveY);
        end;
    end;

    // Hide conditions
    if (FallingStyle in [iesFromTop, iesFromBottom, iesFromLeft, iesFromRight, iesFromTopLeft, iesFromTopRight, iesFromBottomLeft, iesFromBottomRight, iesRandom]) then
    begin
      if (R.Bottom < -100) or (R.Top > Trunc(Height) + 100) or (R.Right < -100) or (R.Left > Trunc(Width) + 100) then
        ImageItem.Visible := False
      else
        AllOut := False;
    end
    else if FallingStyle = iesFromCenter then
    begin
      CurCX := (R.Left + R.Right) div 2;
      CurCY := (R.Top + R.Bottom) div 2;
      if (Abs(CurCX - Trunc(Width) div 2) < 80) and (Abs(CurCY - Trunc(Height) div 2) < 80) then
        ImageItem.Visible := False
      else
        AllOut := False;
    end
    else if FallingStyle = iesFromPoint then
    begin
      if not IsRectEmpty(FallingTargetPos) then
      begin
        TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
        TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
        CurCX := (R.Left + R.Right) div 2;
        CurCY := (R.Top + R.Bottom) div 2;
        if (Abs(CurCX - TargetCX) < 200) and (Abs(CurCY - TargetCY) < 200) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end
      else
      begin
        if (R.Bottom < -100) or (R.Top > Trunc(Height) + 100) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end;
    end;

    ImageItem.CurrentRect := R;
    Repaint;
    Application.ProcessMessages;
    Sleep(FAnimationSpeed);

    // Timeout safety
    if (GetTickCount - StartTick) > 3000 then
      AllOut := True;

  until AllOut;

  // 4. Final Cleanup
  if ImageItem = FSelectedImage then
  begin
    FSelectedImage := nil;
    FCurrentSelectedIndex := -1;
  end
  else if Index < FCurrentSelectedIndex then
    Dec(FCurrentSelectedIndex);

  ImageItem.Free;
  FImages.Delete(Index);

  // Remove from Master Lists
  if (AbsIndex >= 0) and (AbsIndex < FAllSmallPicIndices.Count) then
    FAllSmallPicIndices.Delete(AbsIndex);
  FAllFiles.Delete(AbsIndex);
  FAllCaptions.Delete(AbsIndex);
  FAllPaths.Delete(AbsIndex);
  FAllHints.Delete(AbsIndex);

  if Visible then
  begin
    CalculateLayout;
    Repaint;
  end;

  if (FImages.Count = 0) and (FCurrentPage > 0) then
    PrevPage;
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
var
  PageStart: Integer;
  AbsIndexFrom, AbsIndexTo: Integer;
  Item: TImageItem;
  FileName, Caption, Path, Hint: string;
  OldSelectedIndex: Integer;
  TempSmallIndex: Integer;
begin
  // Safety
  if FInFallAnimation or FPageChangeInProgress or AnimationsRunning then
    Exit;
  if (RelIndexFrom < 0) or (RelIndexFrom >= FImages.Count) or (RelIndexTo < 0) or (RelIndexTo >= FImages.Count) or (RelIndexFrom = RelIndexTo) then
    Exit;

  // 1. Move in visible list (FImages)
  Item := TImageItem(FImages[RelIndexFrom]);
  FImages.Delete(RelIndexFrom);
  FImages.Insert(RelIndexTo, Item);

  // 2. Calculate absolute indices in master lists
  PageStart := GetPageStartIndex;
  AbsIndexFrom := PageStart + RelIndexFrom;
  AbsIndexTo := PageStart + RelIndexTo;

  // 3. Move in ALL master lists (permanent change)
  FileName := FAllFiles[AbsIndexFrom];
  Caption := FAllCaptions[AbsIndexFrom];
  Path := FAllPaths[AbsIndexFrom];
  Hint := FAllHints[AbsIndexFrom];
  TempSmallIndex := Integer(FAllSmallPicIndices[AbsIndexFrom]);

  FAllFiles.Delete(AbsIndexFrom);
  FAllFiles.Insert(AbsIndexTo, FileName);

  FAllCaptions.Delete(AbsIndexFrom);
  FAllCaptions.Insert(AbsIndexTo, Caption);

  FAllPaths.Delete(AbsIndexFrom);
  FAllPaths.Insert(AbsIndexTo, Path);

  FAllHints.Delete(AbsIndexFrom);
  FAllHints.Insert(AbsIndexTo, Hint);

  FAllSmallPicIndices.Delete(AbsIndexFrom);
  FAllSmallPicIndices.Insert(AbsIndexTo, Pointer(TempSmallIndex));

  // 4. Fix selected index (critical!)
  OldSelectedIndex := FCurrentSelectedIndex;
  if OldSelectedIndex = RelIndexFrom then
    FCurrentSelectedIndex := RelIndexTo
  else if (OldSelectedIndex > RelIndexFrom) and (OldSelectedIndex <= RelIndexTo) then
    Dec(FCurrentSelectedIndex)
  else if (OldSelectedIndex >= RelIndexTo) and (OldSelectedIndex < RelIndexFrom) then
    Inc(FCurrentSelectedIndex);

  // 5. Refresh layout and repaint
  CalculateLayout;
  Repaint;
end;

procedure TSkFlowmotion.SavePositionsToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SavePositionsToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSkFlowmotion.LoadPositionsFromFile(const FileName: string);
var
  Stream: TFileStream;
  i, j: Integer;
  LoadedItem: TImageItem;
begin
  if not TFile.Exists(FileName) then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadPositionsFromStream(Stream);
  finally
    Stream.Free;
  end;

  // ==========================================================
  // APPLY LOADED DATA TO EXISTING ITEMS
  // We match by FileName to ensure data goes to the right image
  // ==========================================================
  if Length(FLoadedPositions) > 0 then
  begin
    for i := 0 to Length(FLoadedPositions) - 1 do
    begin
      // Find the item in FImages that matches this filename
      for j := 0 to FImages.Count - 1 do
      begin
        LoadedItem := TImageItem(FImages[j]);
        if SameText(LoadedItem.FileName, FLoadedPositions[i].FileName) then
        begin
          // Apply Position (TargetRect handles animation)
          LoadedItem.TargetRect := Rect(FLoadedPositions[i].Left, FLoadedPositions[i].Top, FLoadedPositions[i].Left + FLoadedPositions[i].Width, FLoadedPositions[i].Top + FLoadedPositions[i].Height);

          // Force update to CurrentRect to snap immediately (optional, but usually desired on load)
          LoadedItem.CurrentRect := LoadedItem.TargetRect;

          // Apply Hint
          LoadedItem.Hint := FLoadedPositions[i].Hint;

          // Apply Rotation
          LoadedItem.FActualRotation := FLoadedPositions[i].Rotation;

          Break; // Found it, move to next loaded item
        end;
      end;
    end;

    // Refresh layout and repaint
    CalculateLayout;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SavePositionsToStream(Stream: TStream);
var
  Writer: TWriter;
  i: Integer;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Number of images
    Writer.WriteInteger(FImages.Count);

    for i := 0 to FImages.Count - 1 do
      with TImageItem(FImages[i]) do
      begin
        Writer.WriteString(FileName);
        Writer.WriteString(Caption);
        Writer.WriteString(Path);
        Writer.WriteString(Hint);

        Writer.WriteInteger(TargetRect.Left);
        Writer.WriteInteger(TargetRect.Top);
        Writer.WriteInteger(TargetRect.Right - TargetRect.Left);
        Writer.WriteInteger(TargetRect.Bottom - TargetRect.Top);

        Writer.WriteSingle(FActualRotation);
      end;
  finally
    Writer.Free;
  end;
end;

procedure TSkFlowmotion.LoadPositionsFromStream(Stream: TStream);
var
  Reader: TReader;
  Count, i: Integer;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    Count := Reader.ReadInteger;
    // Sanity check
    if (Count < 0) or (Count > 10000) then
      Exit;

    SetLength(FLoadedPositions, Count);

    for i := 0 to Count - 1 do
    begin
      FLoadedPositions[i].FileName := Reader.ReadString;
      FLoadedPositions[i].Caption := Reader.ReadString;
      FLoadedPositions[i].Path := Reader.ReadString;
      FLoadedPositions[i].Hint := Reader.ReadString;

      FLoadedPositions[i].Left := Reader.ReadInteger;
      FLoadedPositions[i].Top := Reader.ReadInteger;
      FLoadedPositions[i].Width := Reader.ReadInteger;
      FLoadedPositions[i].Height := Reader.ReadInteger;

      FLoadedPositions[i].Rotation := Reader.ReadSingle;
    end;
  finally
    Reader.Free;
  end;
end;

function TSkFlowmotion.GetCurrentPositions: TImagePositions;
var
  i: Integer;
begin
  // Returns the TargetRects AND Hints/Rotation of all images currently in FImages
  SetLength(Result, FImages.Count);
  for i := 0 to FImages.Count - 1 do
  begin
    with TImageItem(FImages[i]) do
    begin
      Result[i].FileName := FileName;
      Result[i].Caption := Caption;
      Result[i].Path := Path;
      Result[i].Hint := Hint;
      Result[i].Left := TargetRect.Left;
      Result[i].Top := TargetRect.Top;
      Result[i].Width := TargetRect.Right - TargetRect.Left;
      Result[i].Height := TargetRect.Bottom - TargetRect.Top;
      Result[i].Rotation := ActualRotation;
    end;
  end;
end;

procedure TSkFlowmotion.ResetPositions;
begin
  CalculateLayout;
  Repaint;
end;

procedure TSkFlowmotion.ScrollToIndex(Index: Integer; Animate: Boolean = True);
var
  TargetPage, RelativeIndex: Integer;
begin
  if (Index < 0) or (Index >= FAllFiles.Count) then
    Exit;

  TargetPage := Index div FPageSize;
  RelativeIndex := Index mod FPageSize;

  if TargetPage <> FCurrentPage then
    ShowPage(TargetPage)
  else
  begin
    // If we are already on the page, just select the image
    if (RelativeIndex >= 0) and (RelativeIndex < FImages.Count) then
      SetSelectedImage(TImageItem(FImages[RelativeIndex]), RelativeIndex);
  end;
end;

procedure TSkFlowmotion.PerformAnimationUpdate(DeltaMS: Cardinal);
var
  i, StartTick: Cardinal;
  ImageItem: TImageItem;
  Progress, Eased, Speed: Double;
  AnyAnimating, AllOut, NeedRepaint: Boolean;
  TempRect: TRect;
  TempZoom, TargetZoom, SpawnY, SpawnX: Double;
  // Vars for Drift
  DriftTime: Single;
  DriftOffsetX, DriftOffsetY: Single;
  // Vars for SmallPic Spawning
  ImgItem: TImageItem;
  VisRect: TRectF;
  L, T, R, B: Integer;
  Margin: Integer;
  SPP: TSmallPicParticle;
  FallX, FallY, FallW, FallH: INteger;
  // Vars for Falling Out
begin
  // -----------------------------------------------------------------------
  // Early exit conditions
  // -----------------------------------------------------------------------
  if (not Visible) then
    Exit;

  // -----------------------------------------------------------------------
  // Convert milliseconds from thread to seconds
  // -----------------------------------------------------------------------
  var DeltaTime: Double;
  DeltaTime := DeltaMS / 1000.0;
  if DeltaTime <= 0 then
    DeltaTime := 0.016; // fallback ~60 fps

 // ==========================================================
  // PHASE -1.5: SMALL PIC FALLING PHYSICS
  // ==========================================================

  // A. SPAWN PARTICLES (Only once when clearing starts)
  if FIsClearing and FSpawningSmallPics then
  begin
    // Safety check list exists
    if not Assigned(FSmallPicParticles) then
      FSmallPicParticles := TList<TSmallPicParticle>.Create;

    // Loop through all current images to spawn particles
    for i := 0 to FImages.Count - 1 do
    begin
      ImgItem := TImageItem(FImages[i]);

      // Check if this image has a valid smallpic
      if (ImgItem.SmallPicIndex >= 0) and (FSmallPicImageList <> nil) and (ImgItem.SmallPicIndex < FSmallPicImageList.Count) then
      begin
        VisRect := GetVisualRect(ImgItem);

        // Calculate current smallpic position (Visual Rect + Margin)
        // We copy logic from DrawSmallPicOverlay to find exact center
        Margin := FSmallpicMargin;
        if FRoundEdges > 0 then
          Margin := Margin + Round(FRoundEdges * 0.7);

        // We determine the visual center of the smallpic
        case FSmallPicPosition of
          spTopLeft:
            begin
              SPP.X := (VisRect.Left + Margin + 8); // +8 is half of default 16x16 icon
              SPP.Y := (VisRect.Top + Margin + 8);
            end;
          spTopRight:
            begin
              SPP.X := VisRect.Right - Margin - 8;
              SPP.Y := VisRect.Top + Margin + 8;
            end;
          spBottomLeft:
            begin
              SPP.X := VisRect.Left + Margin + 8;
              SPP.Y := VisRect.Bottom - Margin - 8;
            end;
          spBottomRight:
            begin
              SPP.X := VisRect.Right - Margin - 8;
              SPP.Y := VisRect.Bottom - Margin - 8;
            end;
        end;

        // Set Properties
        SPP.Index := ImgItem.SmallPicIndex;
        SPP.Life := 1.0;
        SPP.Angle := 0;
        SPP.VAngle := (Random - 0.5) * 10.0; // Random rotation speed

        // Calculate Velocity based on FallingStyle
        case FClearingStyle of
          iesFromTop:
            begin
              SPP.VX := (Random - 0.5) * 5.0; // Random X
              SPP.VY := 5.0 + Random * 5.0;     // Down (Positive Y in Skia)
            end;
          iesFromBottom:
            begin
              SPP.VX := (Random - 0.5) * 5.0;
              SPP.VY := -(5.0 + Random * 5.0); // Up (Negative Y)
            end;
          iesFromLeft:
            begin
              SPP.VX := 5.0 + Random * 5.0;
              SPP.VY := (Random - 0.5) * 5.0;
            end;
          iesFromRight:
            begin
              SPP.VX := -(5.0 + Random * 5.0);
              SPP.VY := (Random - 0.5) * 5.0;
            end;
          iesRandom:
            begin
              SPP.VX := (Random - 0.5) * 10.0;
              SPP.VY := (Random - 0.5) * 10.0;
            end;
          iesExplode:
            begin
              // High speed outward from center
              // We use random high speed
              SPP.VX := (Random - 0.5) * 20.0;
              SPP.VY := (Random - 0.5) * 20.0;
            end;
        else
          begin
            SPP.VX := 0;
            SPP.VY := 5.0; // Default down
          end;
        end;

        FSmallPicParticles.Add(SPP);
      end;
    end;

    // Turn off spawner so we only spawn once per clear
    FSpawningSmallPics := False;            //ATM off, works but sometimes after a while can crash
  end;

  // B. UPDATE PHYSICS (If particles exist) - PROTECTED
  if Assigned(FSmallPicParticles) and (FSmallPicParticles.Count > 0) then
  begin
    try
      // Use a local index variable so we can react to list size changes
      i := FSmallPicParticles.Count - 1;
      while i >= 0 do
      begin
        // Safety: check if index is still valid
        if (i < 0) or (i >= FSmallPicParticles.Count) then
        begin
          Dec(i);
          Continue;
        end;
        SPP := FSmallPicParticles[i];
        // ==========================================================
        // If particle goes below screen, force Life to 0
        // ==========================================================
        if (SPP.Y > Self.Height) or (SPP.Y < -50) then
          SPP.Life := 0;
        // Move
        SPP.X := SPP.X + (SPP.VX * DeltaTime * 10.0);
        SPP.Y := SPP.Y + (SPP.VY * DeltaTime * 10.0);
        // Rotate
        SPP.Angle := SPP.Angle + (SPP.VAngle * DeltaTime);
        // Decay
        SPP.Life := SPP.Life - (DeltaTime * 0.8);
        // Delete if Dead
        if SPP.Life <= 0 then
          FSmallPicParticles.Delete(i)
        else
        begin
          FSmallPicParticles[i] := SPP; // Update
          NeedRepaint := True;
        end;
        // Decrement index safely
        Dec(i);
      end;
    except
      // Catches any Access Violation from thread conflict.
      // The thread continues running ("doesn't get stuck").
    end;
  end;

  // -----------------------------------------------------------------------
  // Determine initial animation state
  // -----------------------------------------------------------------------
  NeedRepaint := False;

  // Global time for drift (shared by all items)
  DriftTime := GetTickCount / 1000.0;

  // ==========================================================
  // PHASE -1: MATRIX BACKGROUND PHYSICS (REAL DATA)
  // ==========================================================
  if FBackgroundEffect = beRealMatrix then
  begin
    // Safety: Check if columns exist and images exist
    if (Length(FMatrixCols) > 0) and (FImages.Count > 0) then
    begin
      // ==========================================================
      // ONLY UPDATE PHYSICS IF ANIMATEDBACKGROUND IS TRUE
      // ==========================================================
      if FAnimatedBackground then
      begin
        for i := 0 to High(FMatrixCols) do
        begin
          // <--- MASTER TRY BLOCK --->
          try
            // 1. MOVE DOWN
            FMatrixCols[i].Y := FMatrixCols[i].Y + (FMatrixCols[i].Speed * DeltaTime * FMatrixSpeed * 0.5);

            // 2. RESET IF OFF SCREEN
            if FMatrixCols[i].Y > (Height + (FMatrixCols[i].Length * FMatrixFontSize)) then
            begin
              // >>> RESET TO HEIGHT * 2 <<<
              // Using Trunc(Height) can spawn columns at Y = -50 (near top).
              // They fall 50px, become visible briefly, then continue.
              // Using Trunc(Height * 2) ensures they start far above (e.g. -1500).
              FMatrixCols[i].Y := -Random(Trunc(Height * 2));
              FMatrixCols[i].Speed := 5.0 + Random(15);

              // >>> VALIDATE INDEX <<<
              // Re-validate index here too, as images might have been cleared
              if (FMatrixCols[i].TargetImageIndex < 0) or (FMatrixCols[i].TargetImageIndex >= FImages.Count) then
              begin
                if FImages.Count > 1 then
                  FMatrixCols[i].TargetImageIndex := Random(FImages.Count - 1)
                else
                  FMatrixCols[i].TargetImageIndex := 0;
              end;

              // Re-seed text immediately
              FMatrixCols[i].Chars := GetLiveImageDataString(FMatrixCols[i].TargetImageIndex);

              // Adjust length to match new data
              FMatrixCols[i].Length := Length(FMatrixCols[i].Chars);
              if FMatrixCols[i].Length > 30 then
                FMatrixCols[i].Length := 30; // Cap max length
            end;

            // 3. LIVE DATA UPDATES ("DECODING" EFFECT)
            // Only run if we have a valid target image
            // >>> VALIDATE INDEX <<<
            if (FMatrixCols[i].TargetImageIndex >= 0) and (FMatrixCols[i].TargetImageIndex < FImages.Count) then
            begin
              if Random(100) < 15 then
              begin
                // <--- REFRESH LIVE DATA STREAM --->
                // Pull fresh coords from the image right now
                FMatrixCols[i].Chars := GetLiveImageDataString(FMatrixCols[i].TargetImageIndex);

                // Keep length consistent or allow pulses?
                // Let's trim to current max length to prevent layout jumps
                if Length(FMatrixCols[i].Chars) > 30 then
                  FMatrixCols[i].Chars := Copy(FMatrixCols[i].Chars, 1, 30);
              end;
            end;

            NeedRepaint := True;
          except
            // --- CATCH ERROR ---
            // If anything crashes in this specific column, we just skip it.
          end;
        end;
      end;
    end;
  end;

  { --------------------------------------------------------
   PHASE 1: CLEARING ANIMATION (Falling Out)
   -------------------------------------------------------- }
  if FIsClearing then
  begin
    var ClearFinished: Boolean;

    if FImages.Count = 0 then
    begin
      // Stop thread if list emptied
      FIsClearing := False;
      Exit;
    end;

    StartTick := GetTickCount;
    AllOut := True;
    for i := 0 to FImages.Count - 1 do
    begin

      // Dynamic Bound Check
      if i >= FImages.Count then
        Break;
      ImageItem := TImageItem(FImages[i]);
      if ImageItem = nil then
        Continue;

      ClearFinished := False;

      // 1. Advance Progress
      if ImageItem.AnimationProgress < 1.0 then
      begin
        ImageItem.AnimationProgress := ImageItem.AnimationProgress + (FAnimationSpeed / 100);
        if ImageItem.AnimationProgress > 1.0 then
          ImageItem.AnimationProgress := 1.0;
        NeedRepaint := True;
      end;

      // 2. Interpolate Position
      Progress := ImageItem.AnimationProgress;
      if FAnimationEasing then
        Progress := EaseInOutQuad(Progress);

      TempRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Progress), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Progress), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Progress), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress));

      if not EqualRect(ImageItem.CurrentRect, TempRect) then
      begin
        ImageItem.CurrentRect := TempRect;
        NeedRepaint := True;
      end;

      // 3. Check Finished (Position Done AND Alpha Faded Out)
      // This ensures we hide them ONLY when they reach the target point or are fully transparent.
      if (ImageItem.AnimationProgress >= 0.99) or (ImageItem.Alpha <= 1) then
        ClearFinished := True;

      // 4. FADE ALPHA
      var AlphaStep: Integer;
      AlphaStep := Max(1, Round(FAnimationSpeed * 4.0));
      if ImageItem.Alpha > ImageItem.TargetAlpha then
        ImageItem.Alpha := Min(ImageItem.TargetAlpha, ImageItem.Alpha + AlphaStep)
      else
        ImageItem.Alpha := Max(ImageItem.TargetAlpha, ImageItem.Alpha - AlphaStep);

      // 5. HIDE ITEM WHEN FINISHED
      if ClearFinished then
        ImageItem.Visible := False  // Explicitly hide to stop interaction/render
      else
        AllOut := False;

      if (GetTickCount - StartTick) > 2000 then
        AllOut := True; // Safety timeout
    end;

    // 6. Finalize if ALL items finished
    if AllOut then
    begin
      FIsClearing := False;
      FreeAllImagesAndClearLists;
      ThreadSafeFireAllAnimationsFinished;
      Exit;
    end;
  end; // End PHASE 0

  { --------------------------------------------------------
  PHASE 2: PAGE FALL-OUT ANIMATION (Animated Page Change)
  -------------------------------------------------------- }
  if FFallingOut then
  begin
    FPageOutProgress := FPageOutProgress + (FAnimationSpeed / 100);
    if FPageOutProgress >= 1.0 then
    begin
      FPageOutProgress := 1.0;
      FFallingOut := False;
      FInFallAnimation := False;
      if (FTargetPage >= 0) and (FTargetPage < GetPageCount) then
        ShowPage(FTargetPage);
    end
    else
    begin
      Eased := EaseInOutQuad(FPageOutProgress);

      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);

        // Move items off-screen to the Right
        FallX := Round(Width + 200);
        FallY := ImageItem.StartRect.Top;
        FallW := ImageItem.StartRect.Right - ImageItem.StartRect.Left;
        FallH := ImageItem.StartRect.Bottom - ImageItem.StartRect.Top;

        TempRect := Rect(FallX, FallY, FallX + FallW, FallY + FallH);
        TempRect := Rect(Round(ImageItem.StartRect.Left + (TempRect.Left - ImageItem.StartRect.Left) * Eased), Round(ImageItem.StartRect.Top + (TempRect.Top - ImageItem.StartRect.Top) * Eased), Round(ImageItem.StartRect.Right + (TempRect.Right - ImageItem.StartRect.Right) * Eased), Round(ImageItem.StartRect.Bottom + (TempRect.Bottom - ImageItem.StartRect.Bottom) * Eased));

        if not EqualRect(ImageItem.CurrentRect, TempRect) then
        begin
          ImageItem.CurrentRect := TempRect;
          NeedRepaint := True;
        end;
      end;
    end;
  end
  else
  begin
    { ==========================================================
    PHASE 3: Normal item animations (move + zoom in/out)
    ========================================================== }
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);

      // ==========================================================
      // HOVER ALIVE LOGIC (Micro-Movement)
      // ==========================================================
      if FHoverAlive then
      begin
        // === FULLSCREEN SKIP LOGIC ===
        // If we are in Fullscreen mode, this is the Selected Image,
        // and the property is disabled, skip physics updates.
        if FIsZoomedToFill and (ImageItem = FSelectedImage) and (not FHoverAliveOnFullscreen) then
          Continue;
        // === END SKIP LOGIC ===

        // 1. Apply Velocity to Offset
        // We use a property FHoverAliveSpeed to control how fast they float
        ImageItem.FHoverX := ImageItem.FHoverX + (ImageItem.FHoverVX * DeltaTime * FHoverAliveSpeed);
        ImageItem.FHoverY := ImageItem.FHoverY + (ImageItem.FHoverVY * DeltaTime * FHoverAliveSpeed);
        // ... rest of the bounce logic ...

        // 2. Bounce X (Left/Right Limits)
        if ImageItem.FHoverX > FHoverAliveRange then
        begin
          ImageItem.FHoverX := FHoverAliveRange;       // Clamp to max
          ImageItem.FHoverVX := -Abs(ImageItem.FHoverVX); // Reverse direction (force negative)
        end
        else if ImageItem.FHoverX < -FHoverAliveRange then
        begin
          ImageItem.FHoverX := -FHoverAliveRange;      // Clamp to min
          ImageItem.FHoverVX := Abs(ImageItem.FHoverVX);   // Reverse direction (force positive)
        end;

        // 3. Bounce Y (Top/Bottom Limits)
        if ImageItem.FHoverY > FHoverAliveRange then
        begin
          ImageItem.FHoverY := FHoverAliveRange;
          ImageItem.FHoverVY := -Abs(ImageItem.FHoverVY);
        end
        else if ImageItem.FHoverY < -FHoverAliveRange then
        begin
          ImageItem.FHoverY := -FHoverAliveRange;
          ImageItem.FHoverVY := Abs(ImageItem.FHoverVY);
        end;

        // 4. Flag for repaint
        NeedRepaint := True;
      end;

      // ==========================================================
      // CHECK FULLSCREEN COMPLETION (After Animation Loop)
      // ==========================================================
      if FIsZoomedToFill and (not FOnFullscreenEnterFired) and Assigned(FSelectedImage) then
      begin
        // We check if the selected image has reached its final state
        // Position (AnimationProgress) AND Scale (ZoomProgress) AND Rotation must all be close to target
        if (FSelectedImage.AnimationProgress >= 0.99) and (FSelectedImage.ZoomProgress >= 0.99) and (Abs(FSelectedImage.FActualRotation - FSelectedImage.FTargetRotation) <= ROTATION_EPSILON) then
        begin
          // Trigger Event
          if Assigned(FOnFullscreenEnter) then
          begin
            // >>> SYNCHRONIZE TO MAIN THREAD <<<
            // This ensures the event handler (e.g. ShowMessage)
            // runs safely on the Main Thread.
            TThread.Synchronize(nil,
              procedure
              begin
                FOnFullscreenEnter(Self, FSelectedImage, FCurrentSelectedIndex);
              end);
          end;

          // Set Flag so we don't trigger again until next zoom start
          FOnFullscreenEnterFired := True;
        end;
      end;

      // === FLUID INFO PANEL ANIMATION ===
      if ImageItem.FIsInfoShowing then
      begin
        // Slide In
        if ImageItem.FInfoProgress < 1.0 then
        begin
          ImageItem.FInfoProgress := ImageItem.FInfoProgress + 0.05;
          if ImageItem.FInfoProgress > 1.0 then
            ImageItem.FInfoProgress := 1.0;
          NeedRepaint := True;
        end;
      end
      else
      begin
        // Slide Out (Hide)
        if ImageItem.FInfoProgress > 0.0 then
        begin
          ImageItem.FInfoProgress := ImageItem.FInfoProgress - 0.05;
          if ImageItem.FInfoProgress < 0.0 then
            ImageItem.FInfoProgress := 0.0;
          NeedRepaint := True;
        end;
      end;

      // Update Glitch
      if ImageItem.FGlitchIntensity > 0 then
      begin
        ImageItem.FGlitchIntensity := ImageItem.FGlitchIntensity - 0.05;
        if ImageItem.FGlitchIntensity < 0 then
          ImageItem.FGlitchIntensity := 0;
        NeedRepaint := True;
      end;

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
      if FAnimationEasing then
        Progress := EaseInOutQuad(Progress);

      TempRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Progress), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Progress), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Progress), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress));

      // === APPLY DRIFT ===
      if FFreeFloatDrift and (FFlowLayout = flFreeFloat) and (ImageItem <> FDraggedImage) and (ImageItem <> FSelectedImage) and (ImageItem <> FRotatingImage) then
      begin
        DriftOffsetX := Sin(DriftTime * ImageItem.DriftRangeX) * 1.0;
        DriftOffsetY := Cos(DriftTime * ImageItem.DriftRangeY) * 1.0;
        OffsetRect(TempRect, Round(DriftOffsetX), Round(DriftOffsetY));
        ImageItem.TargetRect := TempRect;
      end;

      if not EqualRect(ImageItem.CurrentRect, TempRect) then
      begin
        ImageItem.CurrentRect := TempRect;
        NeedRepaint := True;
      end;

      // --- ALPHA FADING LOGIC (Dynamic Target) ---
      // === REMOVE "Item.Animating" FROM TARGET ALPHA ===
      // We must NOT treat "Animating" (Layout moving) as "Hot".
      // Only Selected or Hovered images should fade to HotAlpha.
      if ImageItem.IsSelected then
        ImageItem.TargetAlpha := FAlphaHotSelected
      else if (ImageItem = FHotItem) then
        ImageItem.TargetAlpha := FAlphaHotPhase
      else
        ImageItem.TargetAlpha := FAlphaStatic;

      if ImageItem.Alpha <> ImageItem.TargetAlpha then
      begin
        var AlphaStep: Integer;
        AlphaStep := Max(1, Round(FAnimationSpeed * 1.5));
        if ImageItem.Alpha < ImageItem.TargetAlpha then
          ImageItem.Alpha := Min(ImageItem.TargetAlpha, ImageItem.Alpha + AlphaStep)
        else
          ImageItem.Alpha := Max(ImageItem.TargetAlpha, ImageItem.Alpha - AlphaStep);
        NeedRepaint := True;
      end;

      ImageItem.Animating := not ((ImageItem.AnimationProgress >= 1.0) and ((ImageItem.ZoomProgress <= 0.0001) or (ImageItem.ZoomProgress >= 0.9999)) and EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) and (Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) <= 0.006));

      if ImageItem = FWasSelectedItem then
        if (ImageItem.ZoomProgress <= 0.0001) and EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) then
          FWasSelectedItem := nil;
    end;

    // ===================================================================
    // PHASE 4: ROTATION SMOOTHING
    // ===================================================================
    if not FIsRotating then
    begin
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if Abs(ImageItem.FActualRotation - ImageItem.FTargetRotation) > ROTATION_EPSILON then
        begin
          ImageItem.FActualRotation := ImageItem.FActualRotation + (ImageItem.FTargetRotation - ImageItem.FActualRotation) * ROTATION_SMOOTHING_SPEED;
          NeedRepaint := True;
        end
        else
        begin
          ImageItem.FActualRotation := ImageItem.FTargetRotation;
        end;
      end;
    end;

    { Update HotTrack Zoom + Breathing }
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);

      // === OVERRIDE IF ZOOMED TO FULL ===
      if FIsZoomedToFill and (ImageItem = FSelectedImage) then
      begin
        ImageItem.FHotZoom := 1.0;
        ImageItem.FHotZoomTarget := 1.0;
        Continue;
      end;
      // === END OVERRIDE ===

      if (not FHotTrackZoom) and (ImageItem <> FSelectedImage) then
      begin
        if ImageItem.FHotZoom <> 1.0 then
        begin
          ImageItem.FHotZoomTarget := 1.0;
          NeedRepaint := True;
        end;
        Continue;
      end;
      if not ImageItem.Visible then
        Continue;

      // Breathing Logic
      if FIsMouseOverHandle or FIsMouseOverInfoIndicator then
      begin
         //keep breathing state when we are over rotatedot with mouse
        TargetZoom := ImageItem.FHotZoom;
      end
      else
      begin
        if FBreathingEnabled and (ImageItem = FSelectedImage) and (ImageItem = FHotItem) then
        begin
          if FDraggingSelected or (FDraggingImage and (ImageItem = FDraggedImage)) then
            TargetZoom := 1.0
          else
            TargetZoom := 1.02 + BREATHING_AMPLITUDE * 0.2 * (Sin(FBreathingPhase * 2 * Pi) + 1.0);
        end
        else if (ImageItem = FSelectedImage) then
          TargetZoom := 1.0
        else if (ImageItem = FHotItem) then
          TargetZoom := FHotZoomMaxFactor
        else
          TargetZoom := 1.0;
      end;

      //Hotzoom logic
      if ImageItem.FHotZoom < TargetZoom then
        Speed := HOT_ZOOM_IN_PER_SEC
      else
        Speed := HOT_ZOOM_OUT_PER_SEC;
      ImageItem.FHotZoom := ImageItem.FHotZoom + (TargetZoom - ImageItem.FHotZoom) * Speed * DeltaTime;
      if not FHotTrackZoom then
        ImageItem.FHotZoomTarget := 1.0
      else
        ImageItem.FHotZoomTarget := TargetZoom;

      if (ImageItem <> FSelectedImage) and (ImageItem.FHotZoom > FHotZoomMaxFactor) then
        ImageItem.FHotZoom := FHotZoomMaxFactor;
      if ImageItem.FHotZoom < 1.0 then
        ImageItem.FHotZoom := 1.0;

      NeedRepaint := True;
    end;

    { Advance Breathing Phase }
    if not FDraggingSelected then
      if FBreathingEnabled and (FHotItem <> nil) and (FHotItem = FSelectedImage) then
        FBreathingPhase := Frac(FBreathingPhase + BREATHING_SPEED_PER_SEC * DeltaTime);
  end;

  // ==========================================================
  // PHASE 5: VIDEO OVERLAY SYNC (Using Skia Snapshot)
  // ==========================================================
  if Assigned(FSelectedImage) and FCaptureVideo and (FTestVideoFile <> '') then
  begin
    // 1. Ensure Static Image is Visible (Layer 0)
    FSelectedImage.Visible := True;

    // 2. Calculate Target Visual State
    VisRect := GetVisualRect(FSelectedImage);

    // --- CALCULATE VISUAL ROTATION (Including Breathing) ---
    var VisualAngle: Single;
    VisualAngle := FSelectedImage.FActualRotation;
    if FBreathingEnabled and FBreathRotationEnabled and (Abs(VisualAngle) > 0.1) then
      VisualAngle := VisualAngle + (Sin(FBreathingPhase * 2 * PI) * 1.5);

    // 3. OPTIMIZATION: Check if we actually need to update UI
    // If position and angle haven't changed much, skip update.
    // This prevents "Very Slow" issue caused by excessive UI calls.
    var UpdateNeeded: Boolean;
    UpdateNeeded := False;

    // Check Position Tolerance (1 pixel movement trigger)
    if (Abs(VisRect.Left - FLastVideoRect.Left) > 1.0) or (Abs(VisRect.Top - FLastVideoRect.Top) > 1.0) or (Abs(VisRect.Width - FLastVideoRect.Width) > 1.0) or (Abs(VisRect.Height - FLastVideoRect.Height) > 1.0) then
      UpdateNeeded := True;

    // Check Rotation Tolerance
    if Abs(VisualAngle - FLastVideoAngle) > 0.1 then
      UpdateNeeded := True;

    // 4. ASYNC UPDATE (Using TThread.Queue for UI Safety)
    // We use Queue instead of Synchronize to keep animation smooth.
    if UpdateNeeded or (FLastVideoRect.Width = 0) then // Force first update
    begin
      // Save current state for next frame comparison
      FLastVideoRect := VisRect;
      FLastVideoAngle := VisualAngle;

      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FVideoPlayer) then
          begin
              // === PLAY MEDIA ===
            if FVideoPlayer.MediaPlayer.State <> TMediaState.Playing then
            begin
              if FVideoPlayer.MediaPlayer.FileName <> FTestVideoFile then
                FVideoPlayer.MediaPlayer.FileName := FTestVideoFile;
              try
                FVideoPlayer.MediaPlayer.Play;
              except

              end;
            end;
          end;
              // F. UPDATE GLOBAL SNAPSHOT (For next frame comparison)
              // We use a critical section to free old snapshot safely (avoid memory leaks / AV if exception occurs)
          TThread.Queue(nil,
            procedure
            begin
          // 1. Update Local State
              if Assigned(FSelectedVideoSnapshot) then
                FLastVideoRect := VisRect;
              if Assigned(FVideoPlayer) then
                FLastVideoAngle := VisualAngle;
              CreateNewVideoSnapshot(VisRect.Width, VisRect.Height);

          // 2. Request Repaint (Updates Screen)
          // Calling this here ensures thread safety (VCL/FireMonkey rule)
          // It schedules Paint immediately, preventing flicker or missing frames
              ThreadSafeRepaint;
            end);

        end);

    end;

  end
  else if Assigned(FSelectedImage) then
  begin
    // If not capturing, ensure Static Image is visible
    FSelectedImage.Visible := True;
    {
    // Hide Video Player safely
    if Assigned(FVideoPlayer) and FVideoPlayer.Visible then
    begin
      TThread.Queue(nil,
        procedure
        begin
          if Assigned(FVideoPlayer) then
            FVideoPlayer.Visible := False;
        end);
    end;   }
  end;

  { Check if any animations are still running }
  AnyAnimating := FFallingOut;
  for i := 0 to FImages.Count - 1 do
    if not ((TImageItem(FImages[i]).AnimationProgress >= 1.0) and ((TImageItem(FImages[i]).ZoomProgress <= 0.0001) or (TImageItem(FImages[i]).ZoomProgress >= 0.9999)) and EqualRect(TImageItem(FImages[i]).CurrentRect, TImageItem(FImages[i]).TargetRect) and (Abs(TImageItem(FImages[i]).FHotZoom - TImageItem(FImages[i]).FHotZoomTarget) <= 0.006)) then
    begin
      AnyAnimating := True;
      Break;
    end;

   // === STOP THREAD IF LIST EMPTY & NOT CLEARING ===
  if (not FIsClearing) and (FImages.Count = 0) and (FAnimationThread <> nil) then
  begin
    FAnimationThread.Stop;
  end;

  if NeedRepaint or AnyAnimating then
    ThreadSafeRepaint;
end;

// --------------------------------------------------------------
// BOOM / DELETE PHYSICS
// --------------------------------------------------------------
procedure TSkFlowMotion.DoAnimateBoom(AImageItem: TImageItem; ASync: Boolean);
var
  i, ColsCount: Integer;
  Item: TImageItem;
  Timeout: Cardinal;
  TempRect: TRectF;
  SP: TSmallPicParticle;
  CenterX, CenterY: Single;
  RandomDir: Integer;
  SourceL, SourceT, SourceB, SourceR: Single;
  // Vars for Explosion logic
  ImageCX, ImageCY: Single;
  TargetCX, TargetCY: Single;
  RandomDist, ExpSpeed: Single;
  AngleStep: Single;
  AlphaStep: Single;
begin
  RandomDir := Random(8) + 1; // 1..8 = 4 directions x 2 diagonals
    // 2. Calculate Center of deleted image
    // VisualRect includes "Zoom" or "Wall Sliding" visual state.
  TempRect := GetVisualRect(AImageItem);

  ImageCX := (TempRect.Left + TempRect.Right) / 2;
  ImageCY := (TempRect.Top + TempRect.Bottom) / 2;

    // 3. Spawn 4 Particles (TopLeft, TopRight, BottomLeft, BottomRight)
  // We use 8 directions (4 corners * 2 diagonals)
  // TSmallPicParticle Index 1
  // Top-Left (0)
  SP.Index := AImageItem.SmallPicIndex;

  // Position (Center + Margin + Offset)
  SP.X := ImageCX - 8; // Fixed Left relative to center
  SP.Y := ImageCY - 8; // Fixed Top relative to center

  SP.Life := 1.0; // Fully Visible

  // Direction: Top-Left -> Move Up / Left
  if (RandomDir = 1) then
  begin
    SP.VX := -10.0 + Random * 10.0; // Move UP (Negative Y)
    SP.VY := -(10.0 + Random * 10.0);
      // Rotate Clockwise (Random start)
    SP.Angle := 10.0 + Random * 10.0; // 10 to 20 deg
    SP.VAngle := -(10.0 + Random * 10.0);
  end
  else if (RandomDir = 2) then
  begin
      // Top-Right (1)
    SP.X := ImageCX + 8.0; // Right
    SP.Y := ImageCY - 8.0; // Top
    SP.VX := 10.0 + Random * 10.0;
    SP.VY := -(10.0 + Random * 10.0);
    SP.Angle := 135 + (Random - 0.5) * 20.0;
    SP.VAngle := -(10.0 + Random * 10.0);
  end
  else if (RandomDir = 3) then
  begin
      // Bottom-Left (1)
    SP.X := ImageCX - 8.0; // Left
    SP.Y := ImageCY + 8.0; // Bottom
    SP.VX := -(10.0 + Random * 10.0);
    SP.VY := 10.0 + Random * 10.0;
    SP.Angle := -45 + (Random - 0.5) * 20.0;
    SP.VAngle := 5.0 + Random * 10.0;
  end
  else if (RandomDir = 4) then
  begin
      // Bottom-Right (1)
    SP.X := ImageCX + 8.0; // Right
    SP.Y := ImageCY + 8.0; // Bottom
    SP.VX := -(10.0 + Random * 10.0);
    SP.VY := 10.0 + Random * 10.0;
    SP.Angle := 45 + (Random - 0.5) * 20.0;
    SP.VAngle := -5.0 + Random * 10.0;
  end
  else if (RandomDir = 5) then
  begin
      // Explosion (All 4 diagonals)
    SP.X := ImageCX;
    SP.Y := ImageCY;

      // Velocity: Random direction (Outward)
    SP.VX := (Random - 0.5) * 30.0;
    SP.VY := (Random - 0.5) * 30.0;

      // Rotation: Fast spin
    SP.Angle := Random * 2 * 30.0;
    SP.VAngle := 20.0 * Random * 10.0;

    SP.Life := 1.0;
    SP.Index := SP.Index; // Re-use index (TopLeft)

    FSmallPicParticles.Add(SP);

      // Top-Right
    SP.Index := SP.Index; // Re-use index
    SP.VX := -SP.VX; // Mirror X
    SP.VY := SP.VY;     // Mirror Y
    SP.Angle := -SP.Angle; // Mirror Angle
    SP.VAngle := -SP.VAngle;
    FSmallPicParticles.Add(SP);

      // Bottom-Left
    SP.Index := SP.Index;
    SP.VX := -SP.VX;
    SP.VY := -SP.VY;
    SP.Angle := -SP.Angle;
    SP.VAngle := SP.VAngle;
    SP.Life := 1.0;
    FSmallPicParticles.Add(SP);

      // Bottom-Right
    SP.Index := SP.Index;
    SP.VX := -SP.VX;
    SP.VY := -SP.VY;
    SP.Angle := SP.Angle;
    SP.VAngle := -SP.VAngle;
    SP.Life := 1.0;
    FSmallPicParticles.Add(SP);
  end;

end;

procedure TSkFlowmotion.CreateNewVideoSnapshot(AWidth, AHeight: Single);
var
  Surface: ISkSurface;
  Pixmap: ISkPixmap;
  Info: TSkImageInfo;
  Data: TBitmap;
  Pixels: PByte;
  MS: TMemoryStream;
begin
  if (not Assigned(FSelectedImage)) or (not Assigned(FVideoPlayer)) then
    Exit;

    // 1. Create Bitmap (100x100)
  Data := TBitmap.Create;
  try
      // Set size explicitly to ensure bitmap matches what we draw (Zoomed Rect size)
    Data.SetSize(Trunc(AWidth), Trunc(AHeight));

    // Capture Frame using Player.PaintTo
    // We draw the player (Invisible) onto this Bitmap.
    FVideoPlayer.PaintTo(Data.Canvas, RectF(0, 0, Width, Height));

    // Convert Bitmap -> Skia Snapshot
    MS := TMemoryStream.Create;
    try
      // Save bitmap to stream
      Data.SaveToStream(MS);
      MS.Position := 0; // Ensure stream start

      // Load back into Skia
      FSelectedVideoSnapshot := TSkImage.MakeFromEncodedStream(MS);
    finally
      MS.Free;
    end;
  except
    // If PaintTo failed (e.g. Player destroyed), Snapshot is nil (nothing drawn)
    FSelectedVideoSnapshot := nil;
    Exit;
  end;
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
  // FreeFloat layout uses the exact same algorithm as Sorted layout
  // to calculate initial positions (Grid placement). The "Free" part
  // is the ability to drag any image (handled in MouseDown/MouseMove).
  if FImages.Count = 0 then
    Exit;
  if FInFallAnimation then
    Exit;

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
  CenterX, CenterY: Single;
  SelGridCol, SelGridRow: Integer;
  GridCClamped, GridRClamped: Integer;

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
  if FIsClearing then
    Exit;

  { --- Phase 1: Collect Visible Images --- }
  // === GRID STABILITY ===
  // We include Selected Image in VisibleImages ONLY if Zooming In Place.
  // This keeps Grid Size (Cols, Rows) stable and prevents rearrangement.
  // If Zoom To Center, we exclude it to create space/hole.
  AddforZoomed := 0;
  VisibleImages := TList.Create;
  try
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if FDraggingSelected and (ImageItem = FSelectedImage) then
        Continue; // Skip layout for dragged selected image

      if (FFlowLayout = flFreeFloat) and FDraggingImage and (ImageItem = FDraggedImage) then
        Continue; // Skip layout for dragged freefloat image
      // =================================================================

      // Include if (Not Selected) OR (Selected but Zooming In Place)
      if (not ImageItem.IsSelected) or (not FZoomSelectedtoCenter) then
        VisibleImages.Add(ImageItem);
    end;
    if VisibleImages.Count = 0 then
      Exit;

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
    // === RAISE GRID SIZE (Higher Slack) ===
    // When KeepSpaceforZoomed and ZoomSelectedtoCenter are TRUE, we must
    // estimate the grid size as if there are MORE items, to create free space.
    if (FSelectedImage <> nil) and FKeepSpaceforZoomed and FZoomSelectedtoCenter then
      VCount := VisibleImages.Count + 3
    else
      VCount := VisibleImages.Count; // Standard exact count

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
    MinCols := Trunc(Max(3.0, Sqrt(Double(TotalCellEstimate))));
    MaxColsToTry := TotalCellEstimate;
    if MaxColsToTry > 1000 then
      MaxColsToTry := 1000;

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
    if (FSelectedImage <> nil) and FKeepSpaceforZoomed and FZoomSelectedtoCenter then
      Cols := BestCols + 1
    else
      Cols := BestCols;
    Rows := BestRows;

    { --- Phase 5: Place Images in Grid --- }
    BaseCellWidth := Trunc(Max(Double(MIN_CELL_SIZE), (Width - FSpacing * (Cols + 1)) / Cols));
    BaseCellHeight := Trunc(Max(Double(MIN_CELL_SIZE), (Height - FSpacing * (Rows + 1)) / Rows));

    SetLength(Grid, Rows, Cols);
    for r := 0 to Rows - 1 do
      for c := 0 to Cols - 1 do
        Grid[r, c] := False;

    // *** SMART 2x2 BLOCKING ***
    // We block space around selected image to keep others away.
    // This works for both "Zoom To Center" (Blocks Center) and "Zoom In Place" (Blocks At Pos).  //-last one not good enough for now
    if (FFlowLayout <> flFreeFloat) and FKeepSpaceforZoomed and FZoomSelectedtoCenter and Assigned(FSelectedImage) then
    begin
      if (FSelectedImage.TargetRect.Right > FSelectedImage.TargetRect.Left) and (FSelectedImage.TargetRect.Bottom > FSelectedImage.TargetRect.Top) then
      begin
        CenterX := (FSelectedImage.TargetRect.Left + FSelectedImage.TargetRect.Right) / 2;
        CenterY := (FSelectedImage.TargetRect.Top + FSelectedImage.TargetRect.Bottom) / 2;
      end
      else
      begin
        CenterX := (FSelectedImage.CurrentRect.Left + FSelectedImage.CurrentRect.Right) / 2;
        CenterY := (FSelectedImage.CurrentRect.Top + FSelectedImage.CurrentRect.Bottom) / 2;
      end;

      if (BaseCellWidth + FSpacing) > 0 then
        SelGridCol := Trunc((CenterX - FSpacing) / (BaseCellWidth + FSpacing))
      else
        SelGridCol := 0;

      if (BaseCellHeight + FSpacing) > 0 then
        SelGridRow := Trunc((CenterY - FSpacing) / (BaseCellHeight + FSpacing))
      else
        SelGridRow := 0;

      if SelGridCol < 0 then
        SelGridCol := 0;
      if SelGridCol >= Cols then
        SelGridCol := Cols - 1;
      if SelGridRow < 0 then
        SelGridRow := 0;
      if SelGridRow >= Rows then
        SelGridRow := Rows - 1;

      // Block 2x2 area (Indices: Row-1, Row, Row+1 x Col-1, Col, Col+1)
      // This ensures other images don't sit right next to the selected one.
      for r := Max(0, SelGridRow - 1) to Min(Rows - 1, SelGridRow + 1) do
        for c := Max(0, SelGridCol - 1) to Min(Cols - 1, SelGridCol + 1) do
          Grid[r, c] := True;
    end;

    // Place other images
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if not Assigned(ImageItem.SkImage) then
        Continue;

      // *** PLACE SELECTED ITEM (MARK OCCUPIED) ***
      // If Zoom In Place, Selected is included in VisibleImages.
      // We must place it to mark the grid cell occupied, but keep it zoomed.
      if ImageItem.IsSelected and (not FZoomSelectedtoCenter) then
      begin
        // Mark the grid cell occupied so others don't overlap.
        // We assume the item corresponds to the current placement loop (r, c).
        // But we need to know WHICH cell.
        // We can't easily know which cell inside this loop without complex state.
        // ALTERNATIVE: We let it fall through to PlaceImage logic below.
      end;

      // *** FREEFLOAT LOGIC ***
      if (FFlowLayout = flFreeFloat) and (not IsRectEmpty(ImageItem.TargetRect)) then
      begin
        if IsRectEmpty(ImageItem.OriginalTargetRect) then
          ImageItem.OriginalTargetRect := ImageItem.TargetRect;
        Continue;
      end;

      // Normal Placement Logic
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
        if Placed then
          Break;
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
          if Placed then
            Break;
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
begin
  { --- Phase 1: Bounds Check --- }
  if (Row < 0) or (Col < 0) or (Row + SpanRows > Length(Grid)) or (Col + SpanCols > Length(Grid[0])) then
  begin
    Result := False;
    Exit;
  end;

  { --- Phase 2: Normal Grid Occupation Check --- }
  // This checks cells that were marked by MarkGridRectsOverlapping
  // (Or normally occupied items)
  for r := Row to Row + SpanRows - 1 do
    for c := Col to Col + SpanCols - 1 do
      if Grid[r, c] then
      begin
        Result := False;
        Exit;
      end;

  { --- Phase 3: User Defined KeepAreaFreeRect Check --- }
  if not IsRectEmpty(FKeepAreaFreeRect) then
  begin
    if IntersectRect(FKeepAreaFreeRect, Rect(0, 0, 0, 0)) then // Dummy rect
    begin
      // Simplified check: reject if new placement intersects user rect
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
{ - making some problems sometimes selected goign directly to old layout pos...
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
  // Set Target Rect
  ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);
  // === PIXEL-EXACT CHECK FOR KeepAreaFreeRect ===
  if not IsRectEmpty(FKeepAreaFreeRect) then
  begin
    if IntersectRect(DummyRect, ImageItem.TargetRect, FKeepAreaFreeRect) then
    begin
      Result := False; // Overlap? reject
      Exit;
    end;
  end;
  // ==========================================================
  // SMART ANIMATION RESET LOGIC
  // ==========================================================
  // 1. Check if we are currently in Entry Animation (Flying in)
  // If Progress is low, we are busy coming from Offscreen. Do NOT disturb StartRect.
  if ImageItem.AnimationProgress < 0.95 then
  begin
    // Just update TargetRect. Keep StartRect as set by AnimateImage.
    Result := True;
    MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
    Exit;
  end;
  // 2. Check if we need to move (Relayout Animation)
  // If Current Position != Target Position, we need to animate.
  if not EqualRect(ImageItem.CurrentRect, ImageItem.TargetRect) then
  begin
    // Set Start Point to where we are NOW.
    // This ensures "Center -> Grid" or "Grid A -> Grid B" works.
    ImageItem.StartRect := ImageItem.CurrentRect;
    // SAFETY: If StartRect is garbage (0,0,0), snap immediately.
    // This prevents "Outside Screen" glitches.
    if IsRectEmpty(ImageItem.StartRect) or (ImageItem.StartRect.Right <= ImageItem.StartRect.Left) then
      ImageItem.StartRect := ImageItem.TargetRect;
    // === START: SAFETY CHECK FOR STARTRECT ===
    // Reset Animation State
    if IsRectEmpty(ImageItem.CurrentRect) then
      ImageItem.StartRect := ImageItem.TargetRect // Safety: If current is invalid, start from Target (Snap)
    else
      ImageItem.StartRect := ImageItem.CurrentRect; // Normal: Animate from current position
    // Kickstart Layout Animation
    ImageItem.AnimationProgress := 0;
    ImageItem.Animating := True;
  end;
  // Mark grid slot occupied
  MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
  Result := True;
end;        }

        //upper version was more safe at edges and all but...too tight making problems

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
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  if X + ImageSize.cx > Width then
    X := trunc(Width - ImageSize.cx);
  if Y + ImageSize.cy > Height then
    Y := trunc(Height - ImageSize.cy);
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

  { === START: SAFETY CHECK FOR STARTRECT === }
  { Reset Animation State }
  if IsRectEmpty(ImageItem.CurrentRect) then
    ImageItem.StartRect := ImageItem.TargetRect // Safety: If current is invalid, start from Target (Snap)
  else
    ImageItem.StartRect := ImageItem.CurrentRect; // Normal: Animate from current position

  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;

  MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
  Result := True;
end;

procedure TSkFlowMotion.SetVideoFileSync;
begin
  // Only run logic if player exists
  if not Assigned(FVideoPlayer) then
    Exit;

  // 3. Update State
  if FTestVideoFile <> '' then
  begin
    // A. Safe Access (Main Thread)
    if FTestVideoFile <> FVideoPlayer.MediaPlayer.FileName then
    begin
      // Stop and Clear safely
      if FVideoPlayer.MediaPlayer.State <> TMediaState.Stopped then
        FVideoPlayer.MediaPlayer.Stop;
      FVideoPlayer.MediaPlayer.Clear;
      FVideoPlayer.MediaPlayer.FileName := FTestVideoFile;
      // Only Play if we are in "Capture" mode
      if FCaptureVideo then
      begin
        try
          FVideoPlayer.MediaPlayer.Play;
        except
          // Ignore errors silently so it doesn't crash the component
        end;
      end
    end
    else
    begin
    // Stop and Clear immediately
      if FVideoPlayer.MediaPlayer.State <> TMediaState.Stopped then
        FVideoPlayer.MediaPlayer.Stop;
      FVideoPlayer.MediaPlayer.Clear;
    end;
  end;
end;

procedure TSkFlowmotion.SetCornerRadius(const Value: Single);
begin
  if FCornerRadius <> Value then
  begin
    FCornerRadius := Value;
    Repaint;
  end;
end;

procedure TSkFlowmotion.SetRotationAllowed(const Value: Boolean);
begin
  FRotationAllowed := Value;
end;

procedure TSkFlowmotion.SetAnimatedBackground(const Value: Boolean);
begin
  if FAnimatedBackground <> Value then
  begin
    FAnimatedBackground := Value;
    Repaint;
  end;
end;

function TSkFlowmotion.GetRotateHandleRect(const ItemRect: TRect): TRect;
var
  Margin, HandleSize: Integer;
begin
  HandleSize := FRotateHandleSize; // Size of the dot
  Margin := FSmallpicMargin;     // Distance from edge
  //raise matching to round edges
  if FRoundEdges > 0 then
    Margin := Margin + Round(FRoundEdges * 0.7);

  case FRotateHandlePosition of
    spTopLeft:
      Result := Rect(ItemRect.Left + Margin, ItemRect.Top + Margin, ItemRect.Left + Margin + HandleSize, ItemRect.Top + Margin + HandleSize);
    spTopRight:
      Result := Rect(ItemRect.Right - Margin - HandleSize, ItemRect.Top + Margin, ItemRect.Right - Margin, ItemRect.Top + Margin + HandleSize);
    spBottomLeft:
      Result := Rect(ItemRect.Left + Margin, ItemRect.Bottom - Margin - HandleSize, ItemRect.Left + Margin + HandleSize, ItemRect.Bottom - Margin);
    spBottomRight:
      Result := Rect(ItemRect.Right - Margin - HandleSize, ItemRect.Bottom - Margin - HandleSize, ItemRect.Right - Margin, ItemRect.Bottom - Margin);
  end;
end;

procedure TSkFlowmotion.SpawnParticles(X, Y: Single; Count: Integer; Color: TAlphaColor);
var
  i: Integer;
  P: TParticle;
  Angle: Single;
begin
  // If no specific color passed, use global property (for clicks)
  if Color = 0 then
    Color := FParticleColor;

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
    FParticles.Add(P);
  end;
end;

end.
