{===============================================================================
    ___  _ _                 ___
   / _ \| | |__ _ _ __  __ _| _ ) _____ __™
  | (_) | | / _` | '  \/ _` | _ \/ _ \ \ /
   \___/|_|_\__,_|_|_|_\__,_|___/\___/_\_\
  Embed & Control Ollama In Your Delphi App

 Copyright © 2025-present tinyBigGAMES™ LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/OllamaBox

 See LICENSE file for license information
===============================================================================}

unit OllamaBox.Utils;

{$I OllamaBox.Defines.inc}

interface

uses
  WinApi.Windows,
  WinApi.Messages,
  Winapi.ActiveX,
  WinApi.MMSystem,
  Winapi.DirectSound,
  WinApi.ShellAPI,
  Winapi.PsAPI,
  Winapi.TlHelp32,
  System.Generics.Collections,
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  System.StrUtils,
  System.Classes,
  System.Math,
  System.JSON,
  System.TypInfo,
  System.Rtti,
  System.IniFiles,
  System.Zip,
  System.SyncObjs,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.NetEncoding,
  System.NetConsts,
  System.Threading,
  System.Win.Registry;

type
  { TobPointerArray1D<T> }
  TobPointerArray1D<T> = record
    class function GetValue(P: Pointer; Index: Integer): T; static;
    class procedure SetValue(P: Pointer; Index: Integer; const Value: T); static;
  end;

  { TobPointerArray2D<T> }
  TobPointerArray2D<T> = record
    class function GetValue(P: Pointer; Row, Col: Integer): T; static;
    class procedure SetValue(P: Pointer; Row, Col: Integer; const Value: T); static;
  end;

  { obSchemaDescription }
  obSchemaDescription = class(TCustomAttribute)
  private
    FDescription: string;
  public
    constructor Create(const ADescription: string);
    property Description: string read FDescription;
  end;

  { TobTokenPrintAction }
  TobTokenPrintAction = (tpaWait, tpaAppend, tpaNewline);

  { TobTokenResponse }
  TobTokenResponse = record
  private
    FRaw: string;                  // Full response as is
    FTokens: array of string;      // Actual tokens
    FMaxLineLength: Integer;       // Define confined space, in chars for fixed width font
    FWordBreaks: array of char;    // What is considered a logical word-break
    FLineBreaks: array of char;    // What is considered a logical line-break
    FWords: array of String;       // Response but as array of "words"
    FWord: string;                 // Current word accumulating
    FLine: string;                 // Current line accumulating
    FFinalized: Boolean;           // Know the finalization is done
    FRightMargin: Integer;
    function HandleLineBreaks(const AToken: string): Boolean;
    function SplitWord(const AWord: string; var APrefix, ASuffix: string): Boolean;
    function GetLineLengthMax(): Integer;
  public
    procedure Initialize;
    property RightMargin: Integer read FRightMargin;
    property MaxLineLength: Integer read FMaxLineLength;
    function  GetRightMargin(): Integer;
    procedure SetRightMargin(const AMargin: Integer);
    function  GetMaxLineLength(): Integer;
    procedure SetMaxLineLength(const ALength: Integer);
    function AddToken(const aToken: string): TobTokenPrintAction;
    function LastWord(const ATrimLeft: Boolean=False): string;
    function Finalize: Boolean;
    procedure Clear();
  end;

  { TobPayloadStream }
  TobPayloadStream = class(TStream)
  private const
    cWaterMarkGUID: TGUID = '{9FABA105-EDA8-45C3-89F4-369315A947EB}';
  private type
    EPayloadStream = class(Exception);

    TPayloadStreamFooter = packed record
      WaterMark: TGUID;
      ExeSize: Int64;
      DataSize: Int64;
    end;

    TPayloadStreamOpenMode = (
      pomRead,    // read mode
      pomWrite    // write (append) mode
    );
  private
    fMode: TPayloadStreamOpenMode;  // stream open mode
    fFileStream: TFileStream;       // file stream for payload
    fDataStart: Int64;              // start of payload data in file
    fDataSize: Int64;               // size of payload
    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
    procedure InitFooter(out Footer: TPayloadStreamFooter);
    function ReadFooter(const FileStream: TFileStream; out Footer: TPayloadStreamFooter): Boolean;
  public
    property CurrentPos: Int64 read GetPosition write SetPosition;
    constructor Create(const FileName: string; const Mode: TPayloadStreamOpenMode);
    destructor Destroy; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    property DataSize: Int64 read fDataSize;
  end;

  { obUtils }
  obUtils = class
  public type
    DownloadFileProgressCallback = reference to procedure(const AFilename: string; const APercent: Single; var AAbort: Boolean);
    UnzipFileProgressCallback = reference to procedure(const AFileName: string; const APercent: Single; var AAbort: Boolean);
  private
    class constructor Create();
    class destructor Destroy();
  private class var
    FCriticalSection: TCriticalSection;
    FMarshaller: TMarshaller;
    FPerformanceFrequency: Int64;
    FMimeMap: TDictionary<string, string>;
  public
    class procedure UnitInit(); static;
    class procedure EnterCriticalSection(); static;
    class procedure LeaveCriticalSection(); static;
    class function  AsUTF8(const AText: string): Pointer; static;
    class function  GetPhysicalProcessorCount(): DWORD; static;
    class function  EnableVirtualTerminalProcessing(): DWORD; static;
    class function  GetEnvVarValue(const AVarName: string): string; static;
    class function  IsStartedFromDelphiIDE(): Boolean; static;
    class procedure ProcessMessages(); static;
    class function  RandomRange(const AMin, AMax: Integer): Integer; static;
    class function  RandomRangef(const AMin, AMax: Single): Single; static;
    class function  RandomBool(): Boolean; static;
    class function  GetRandomSeed(): Integer; static;
    class procedure SetRandomSeed(const AVaLue: Integer); static;
    class procedure Wait(const AMilliseconds: Double); static;
    class function  SanitizeFromJson(const aText: string): string; static;
    class function  SanitizeToJson(const aText: string): string; static;
    class function  GetJsonSchema(const AClass: TClass; const AMethodName: string): string; static;
    class function  GetJsonSchemas(AClass: TClass): string; static;
    class function  CallStaticMethod(const AClass: TClass; const AMethodName: string; const AArgs: array of TValue): TValue; static;
    class function  GetISO8601DateTime(): string; static;
    class function  GetISO8601DateTimeLocal(): string; static;
    class function  GetLocalDateTime(): string; static;
    class function  HasEnoughDiskSpace(const AFilePath: string; ARequiredSize: Int64): Boolean; static;
    class procedure StringToStream(const AString: string; const AStream: TStream); static;
    class function  StringFromStream(const AStream: TStream): string; static;
    class function  AddPathToSystemPath(const APath: string): Boolean; static;
    class function  RemovePathFromSystemPath(const APath: string): Boolean; static;
    class function  GetEXEPath(): string; static;
    class function  RemoveQuotes(const AText: string): string; static;
    class function  FormatNumberWithCommas(const AValue: Int64): string; static;
    class function  GetRandomThinkingResult(): string; static;
    class function  FileToBase64(const AFilename: string): string; static;
    class function  GetMimeTypeFromExt(const AFileName: string): string;
    class function  GetFormattedDate(): string; static;
    class function  GetTimeZoneAbbreviation(): string; static;
    class function  GetFormattedTime(): string; static;
    class procedure SetUserEnvironmentVariable(const AName, AValue: string); static;
    class function  GetUserEnvironmentVariable(const AName: string): string; static;
    class procedure RemoveUserEnvironmentVariable(const AName: string); static;
    class function  UserEnvironmentVariableExists(const AName: string): Boolean; static;
    class function  TrimLeadingCRLF(const S: string): string;
    class function  TrimTrailingCRLF(const S: string): string;
    class function  DownloadFile(const AUrl, ATargetFile: string; const AProgressCallback: obUtils.DownloadFileProgressCallback): Boolean;
    class function  UnzipFile(const AZipFile, ATargetDir: string; const AProgress: obUtils.UnzipFileProgressCallback): Boolean;
    class procedure DeleteFolder(const AFolder: string; const ARecursive: Boolean);
    class function  TavilyWebSearch(const AAPIKey, AQuery: string): string; static;
    class function  LemonfoxTTS(const AAPIKey: string; const AInputText: string; const AOutputFile: string='speech.wav'; const AVoice: string='bella'; const ALanguage: string='en'; const AFormat: string='wav'; const ASpeed: Single=1.0; const AWordTimestamps: Boolean=False): Boolean; static;
    class function  JinaEmbeddings(const AApiKey: string; const AInputs: array of string; const ARole: string = 'document'; AModel: string='jina-clip-v2'; const ADimensions: Integer = 1024; const ANormalize: Boolean = True; const AEmbeddingType: string = 'float'): TArray<TArray<Single>>; static;
    class procedure MessageBox(const ATitle: string; const AMsg: string; const AArgs: array of const); static;
    class function  GetFullPath(const ABasePath, ARelativePath: string): string; static;
  end;

const
  obLF   = AnsiChar(#10);
  obCR   = AnsiChar(#13);
  obCRLF = obLF+obCR;
  obESC  = AnsiChar(#27);

  obVK_ESC = 27;

  // Cursor Movement
  obCSICursorPos = obESC + '[%d;%dH';         // Set cursor position
  obCSICursorUp = obESC + '[%dA';             // Move cursor up
  obCSICursorDown = obESC + '[%dB';           // Move cursor down
  obCSICursorForward = obESC + '[%dC';        // Move cursor forward
  obCSICursorBack = obESC + '[%dD';           // Move cursor backward
  obCSISaveCursorPos = obESC + '[s';          // Save cursor position
  obCSIRestoreCursorPos = obESC + '[u';       // Restore cursor position
  obCSICursorHomePos = obESC + '[H';          // Move cursor to home position

  // Cursor Visibility
  obCSIShowCursor = obESC + '[?25h';          // Show cursor
  obCSIHideCursor = obESC + '[?25l';          // Hide cursor
  obCSIBlinkCursor = obESC + '[?12h';         // Enable cursor blinking
  obCSISteadyCursor = obESC + '[?12l';        // Disable cursor blinking

  // Screen Manipulation
  obCSIClearScreen = obESC + '[2J';           // Clear screen
  obCSIClearLine = obESC + '[2K';             // Clear line
  obCSIClearToEndOfLine = obESC + '[K';       // Clear from cusor to end of line
  obCSIScrollUp = obESC + '[%dS';             // Scroll up by n lines
  obCSIScrollDown = obESC + '[%dT';           // Scroll down by n lines

  // Text Formatting
  obCSIBold = obESC + '[1m';                  // Bold text
  obCSIUnderline = obESC + '[4m';             // Underline text
  obCSIResetFormat = obESC + '[0m';           // Reset text formatting
  obCSIResetBackground = #27'[49m';         // Reset background text formatting
  obCSIResetForeground = #27'[39m';         // Reset forground text formatting
  obCSIInvertColors = obESC + '[7m';          // Invert foreground/background
  obCSINormalColors = obESC + '[27m';         // Normal colors

  obCSIDim = obESC + '[2m';
  obCSIItalic = obESC + '[3m';
  obCSIBlink = obESC + '[5m';
  obCSIFramed = obESC + '[51m';
  obCSIEncircled = obESC + '[52m';

  // Text Modification
  obCSIInsertChar = obESC + '[%d@';           // Insert n spaces at cursor position
  obCSIDeleteChar = obESC + '[%dP';           // Delete n characters at cursor position
  obCSIEraseChar = obESC + '[%dX';            // Erase n characters at cursor position

  // Colors (Foreground and Background)
  obCSIFGRGB = obESC + '[38;2;%d;%d;%dm';        // Foreground RGB
  obCSIBGRGB = obESC + '[48;2;%d;%d;%dm';        // Backg

  obCSIFGBlack = obESC + '[30m';
  obCSIFGRed = obESC + '[31m';
  obCSIFGGreen = obESC + '[32m';
  obCSIFGYellow = obESC + '[33m';
  obCSIFGBlue = obESC + '[34m';
  obCSIFGMagenta = obESC + '[35m';
  obCSIFGCyan = obESC + '[36m';
  obCSIFGWhite = obESC + '[37m';

  obCSIBGBlack = obESC + '[40m';
  obCSIBGRed = obESC + '[41m';
  obCSIBGGreen = obESC + '[42m';
  obCSIBGYellow = obESC + '[43m';
  obCSIBGBlue = obESC + '[44m';
  obCSIBGMagenta = obESC + '[45m';
  obCSIBGCyan = obESC + '[46m';
  obCSIBGWhite = obESC + '[47m';

  obCSIFGBrightBlack = obESC + '[90m';
  obCSIFGBrightRed = obESC + '[91m';
  obCSIFGBrightGreen = obESC + '[92m';
  obCSIFGBrightYellow = obESC + '[93m';
  obCSIFGBrightBlue = obESC + '[94m';
  obCSIFGBrightMagenta = obESC + '[95m';
  obCSIFGBrightCyan = obESC + '[96m';
  obCSIFGBrightWhite = obESC + '[97m';

  obCSIBGBrightBlack = obESC + '[100m';
  obCSIBGBrightRed = obESC + '[101m';
  obCSIBGBrightGreen = obESC + '[102m';
  obCSIBGBrightYellow = obESC + '[103m';
  obCSIBGBrightBlue = obESC + '[104m';
  obCSIBGBrightMagenta = obESC + '[105m';
  obCSIBGBrightCyan = obESC + '[106m';
  obCSIBGBrightWhite = obESC + '[107m';

type
  { TobCharSet }
  TobCharSet = set of AnsiChar;

  /// <summary>
  ///   <c>obConsole</c> is a static class responsible for handling console output.
  ///   It provides utility methods for printing messages to the currently active console,
  ///   if one is present. This class is designed for logging, debugging, and
  ///   real-time text output within a console-based environment.
  /// </summary>
  obConsole = class
  private class var
    FInputCodePage: Cardinal;
    FOutputCodePage: Cardinal;
    FTeletypeDelay: Integer;
    FKeyState: array [0..1, 0..255] of Boolean;
  private
    class constructor Create();
    class destructor Destroy();
  public
    class procedure UnitInit();

    /// <summary>
    ///   Prints a message to the currently active console, if available.
    ///   This method does not append a newline after the message.
    /// </summary>
    /// <param name="AMsg">
    ///   The message to be printed to the console.
    /// </param>
    class procedure Print(const AMsg: string); overload; static;

    /// <summary>
    ///   Prints a message to the currently active console, if available,
    ///   and appends a newline at the end.
    /// </summary>
    /// <param name="AMsg">
    ///   The message to be printed to the console.
    /// </param>
    class procedure PrintLn(const AMsg: string); overload; static;

    /// <summary>
    ///   Prints a formatted message to the currently active console, if available.
    ///   This method allows placeholders in the message string to be replaced
    ///   with values from the provided argument array.
    ///   No newline is appended after the message.
    /// </summary>
    /// <param name="AMsg">
    ///   The format string containing placeholders for argument substitution.
    /// </param>
    /// <param name="AArgs">
    ///   An array of values to be inserted into the format string.
    /// </param>
    class procedure Print(const AMsg: string; const AArgs: array of const); overload; static;

    /// <summary>
    ///   Prints a formatted message to the currently active console, if available,
    ///   and appends a newline at the end.
    ///   This method allows placeholders in the message string to be replaced
    ///   with values from the provided argument array.
    /// </summary>
    /// <param name="AMsg">
    ///   The format string containing placeholders for argument substitution.
    /// </param>
    /// <param name="AArgs">
    ///   An array of values to be inserted into the format string.
    /// </param>
    class procedure PrintLn(const AMsg: string; const AArgs: array of const); overload; static;

    /// <summary>
    ///   Prints an empty line to the currently active console, if available.
    ///   This method is equivalent to printing an empty string without appending a newline.
    /// </summary>
    class procedure Print(); overload; static;

    /// <summary>
    ///   Prints a blank line to the currently active console, if available.
    ///   This method simply appends a newline to create a visual separation in output.
    /// </summary>
    class procedure PrintLn(); overload; static;

    class procedure GetCursorPos(X, Y: PInteger); static;
    class procedure SetCursorPos(const X, Y: Integer); static;
    class procedure SetCursorVisible(const AVisible: Boolean); static;
    class procedure HideCursor(); static;
    class procedure ShowCursor(); static;
    class procedure SaveCursorPos(); static;
    class procedure RestoreCursorPos(); static;
    class procedure MoveCursorUp(const ALines: Integer); static;
    class procedure MoveCursorDown(const ALines: Integer); static;
    class procedure MoveCursorForward(const ACols: Integer); static;
    class procedure MoveCursorBack(const ACols: Integer); static;

    class procedure ClearScreen(); static;

    /// <summary>
    ///   Clears the current line where the cursor is positioned in the active console.
    ///   This removes any text from the line without affecting other content in the console.
    /// </summary>
    class procedure ClearLine(); static;
    class procedure ClearToEndOfLine(); static;

    class procedure ClearLineFromCursor(const AColor: string); static;

    class procedure SetBoldText(); static;
    class procedure ResetTextFormat(); static;
    class procedure SetForegroundColor(const AColor: string); static;
    class procedure SetBackgroundColor(const AColor: string); static;
    class procedure SetForegroundRGB(const ARed, AGreen, ABlue: Byte); static;
    class procedure SetBackgroundRGB(const ARed, AGreen, ABlue: Byte); static;

    class procedure GetSize(AWidth: PInteger; AHeight: PInteger); static;

    class procedure SetTitle(const ATitle: string); static;
    class function  GetTitle(): string; static;

    class function  HasOutput(): Boolean; static;
    class function  WasRunFrom(): Boolean; static;
    class procedure WaitForAnyKey(); static;
    class function  AnyKeyPressed(): Boolean; static;

    class procedure ClearKeyStates(); static;
    class procedure ClearKeyboardBuffer(); static;

    class function  IsKeyPressed(AKey: Byte): Boolean; static;
    class function  WasKeyReleased(AKey: Byte): Boolean; static;
    class function  WasKeyPressed(AKey: Byte): Boolean; static;

    class function  ReadKey(): WideChar; static;
    class function  ReadLnX(const AAllowedChars: TobCharSet; AMaxLength: Integer; const AColor: string=obCSIFGWhite): string; static;

    /// <summary>
    ///   Pauses execution and waits for user input in the active console.
    ///   This method is typically used to create a breakpoint in execution, allowing
    ///   the user to acknowledge a message before continuing.
    /// </summary>
    /// <param name="AForcePause">
    ///   If set to <c>True</c>, the pause is enforced even if no console is detected.
    ///   If <c>False</c>, the pause only occurs if a console is present.
    /// </param>
    /// <param name="AColor">
    ///   The foreground color used to display the message in the console.
    ///   Defaults to <c>phCSIFGWhite</c>.
    /// </param>
    /// <param name="AMsg">
    ///   An optional message to display before pausing execution.
    ///   If no message is provided, the function simply waits for user input.
    /// </param>
    class procedure Pause(const AForcePause: Boolean = False; AColor: string = obCSIFGWhite;
      const AMsg: string = ''); static;

    class function  WrapTextEx(const ALine: string; AMaxCol: Integer; const ABreakChars: TobCharSet=[' ', '-', ',', ':', #9]): string; static;
    class procedure Teletype(const AText: string; const AColor: string=obCSIFGWhite; const AMargin: Integer=10; const AMinDelay: Integer=0; const AMaxDelay: Integer=3; const ABreakKey: Byte=VK_ESCAPE); static;
  end;

  { TobObject }
  TobObject = class
  protected
    FError: string;
    function  GetError(): string; virtual;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;
    procedure SetError(const AText: string; const AArgs: array of const); virtual;
    property Error: string read GetError;
  end;

  /// <summary>
  ///   <c>TloWavPlayer</c> is a WAV file player class that provides functionality
  ///   to load and play WAV audio files. It allows basic operations such as opening,
  ///   playing, stopping, and adjusting the volume of the audio playback.
  /// </summary>
  TobWavPlayer = class(TobObject)
  private
    FDirectSound: IDirectSound;
    FPrimaryBuffer: IDirectSoundBuffer;
    FSecondaryBuffer: IDirectSoundBuffer;
    FVolume: Single;
    function LoadWavFile(const AFileName: string): Boolean;
    procedure SetVolume(const AValue: Single);
  public
    /// <summary>
    ///   Initializes a new instance of the <c>TphWavPlayer</c> class.
    ///   This constructor sets up the necessary resources for audio playback.
    /// </summary>
    constructor Create(); override;

    /// <summary>
    ///   Destroys the instance of the <c>TphWavPlayer</c> class.
    ///   This destructor ensures proper cleanup of allocated resources before
    ///   the object is removed from memory.
    /// </summary>
    destructor Destroy; override;

    /// <summary>
    ///   Opens the WAV player, optionally attaching it to a window handle.
    ///   This method prepares the player for loading and playing audio files.
    /// </summary>
    /// <param name="AHandle">
    ///   An optional window handle that can be associated with the player.
    ///   Defaults to <c>0</c>, meaning it will try to a handle from the current Console if there is one.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the player was successfully opened; otherwise, <c>False</c>.
    /// </returns>
    function Open(const AHandle: HWND = 0): Boolean;

    /// <summary>
    ///   Checks whether the WAV player is currently open and available for use.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the player is open; otherwise, <c>False</c>.
    /// </returns>
    function IsOpen(): Boolean;

    /// <summary>
    ///   Closes the WAV player, releasing any associated resources.
    ///   This should be called when playback is no longer needed.
    /// </summary>
    procedure Close();

    /// <summary>
    ///   Loads a WAV file from the specified file path into the player.
    ///   The file must be a valid WAV format for successful loading.
    /// </summary>
    /// <param name="AFileName">
    ///   The full path to the WAV file to be loaded.
    /// </param>
    procedure LoadFromFile(const AFileName: string);

    /// <summary>
    ///   Starts playback of the loaded WAV file.
    ///   The audio will play from the beginning unless it is already playing.
    /// </summary>
    procedure Play;

    /// <summary>
    ///   Stops the current audio playback.
    ///   This halts the playback of the loaded WAV file.
    /// </summary>
    procedure Stop;

    /// <summary>
    ///   Controls the playback volume of the WAV player.
    ///   The volume level is represented as a floating-point value from 0 (silence) to 1 (full volume).
    /// </summary>
    property Volume: Single read FVolume write SetVolume;
  end;

  { TobMarkdownStreamFilter }
  TobMarkdownStreamFilter = record
  private
    FInTag: Boolean;
    FTagBuffer: string;
    FWhitelist: TArray<string>;
    FOutputBuffer: string;

    function ExtractTagName(const Tag: string; out IsClosing: Boolean): string;
    function ConvertToMarkdown(const TagName: string; const IsClosing: Boolean): string;
    function IsWhitelisted(const TagName: string): Boolean;
  public
    procedure Clear();
    procedure SetWhitelist(const Tags: array of string);
    function ProcessChunk(const Chunk: string): string;
  end;

  { TobToolHandler }
  TobToolHandler = reference to procedure(const AToolName: string; const AParameters: TJSONObject);

  { TobToolStreamProcessor }
  TobToolStreamProcessor = class(TobObject)
  private
    FBuffer: string;
    FInJsonDetection: Boolean;
    FOpenBraces: Integer;
    FOnToolDetected: TobToolHandler;
    FOnTextOutput: TProc<string>;
    FResponse: string;
    FInMarkdownCodeBlock: Boolean;
    FPotentialCodeBlockStart: Boolean;
    function IsValidToolJson(const AJsonText: string; out AToolName: string; out AParameters: TJSONObject): Boolean;
  public
    constructor Create(); override;
    destructor Destroy; override;
    procedure Clear();
    procedure ProcessToken(const AToken: string);
    property Response: string read FResponse write FResponse;
    property OnToolDetected: TobToolHandler read FOnToolDetected write FOnToolDetected;
    property OnTextOutput: TProc<string> read FOnTextOutput write FOnTextOutput;
  end;

  { TobConfigFile }
  TobConfigFile = class(TobObject)
  private
    FHandle: TIniFile;
    FFilename: string;
    FSection: TStringList;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    function  Open(const AFilename: string=''): Boolean;
    procedure Close();
    function  Opened(): Boolean;
    procedure Update();
    function  RemoveSection(const AName: string): Boolean;
    procedure SetValue(const ASection, AKey, AValue: string);  overload;
    procedure SetValue(const ASection, AKey: string; AValue: Integer); overload;
    procedure SetValue(const ASection, AKey: string; AValue: Boolean); overload;
    procedure SetValue(const ASection, AKey: string; AValue: Pointer; AValueSize: Cardinal); overload;
    function  GetValue(const ASection, AKey, ADefaultValue: string): string; overload;
    function  GetValue(const ASection, AKey: string; ADefaultValue: Integer): Integer; overload;
    function  GetValue(const ASection, AKey: string; ADefaultValue: Boolean): Boolean; overload;
    procedure GetValue(const ASection, AKey: string; AValue: Pointer; AValueSize: Cardinal); overload;
    function  RemoveKey(const ASection, AKey: string): Boolean;
    function  GetSectionValues(const ASection: string): Integer;
    function  GetSectionValue(const AIndex: Integer; const ADefaultValue: string): string; overload;
    function  GetSectionValue(const AIndex, ADefaultValue: Integer): Integer; overload;
    function  GetSectionValue(const AIndex: Integer; const ADefaultValue: Boolean): Boolean; overload;
  end;

implementation

{ TobPointerArray1D<T> }
class function TobPointerArray1D<T>.GetValue(P: Pointer; Index: Integer): T;
var
  Ptr: PByte;
begin
  Ptr := PByte(P);
  Inc(Ptr, Index * SizeOf(T));
  Move(Ptr^, Result, SizeOf(T));
end;

class procedure TobPointerArray1D<T>.SetValue(P: Pointer; Index: Integer; const Value: T);
var
  Ptr: PByte;
begin
  Ptr := PByte(P);
  Inc(Ptr, Index * SizeOf(T));
  Move(Value, Ptr^, SizeOf(T));
end;

{ TobPointerArray2D<T> }
class function TobPointerArray2D<T>.GetValue(P: Pointer; Row, Col: Integer): T;
var
  PP: PPointer;
  Ptr: PByte;
begin
  PP := PPointer(P);
  Inc(PP, Row);
  Ptr := PByte(PP^);
  Inc(Ptr, Col * SizeOf(T));
  Move(Ptr^, Result, SizeOf(T));
end;

class procedure TobPointerArray2D<T>.SetValue(P: Pointer; Row, Col: Integer; const Value: T);
var
  PP: PPointer;
  Ptr: PByte;
begin
  PP := PPointer(P);
  Inc(PP, Row);
  Ptr := PByte(PP^);
  Inc(Ptr, Col * SizeOf(T));
  Move(Value, Ptr^, SizeOf(T));
end;

{ obSchemaDescription }
constructor obSchemaDescription.Create(const ADescription: string);
begin
  FDescription := ADescription;
end;

{ TobTokenResponse }
procedure TobTokenResponse.Initialize;
begin
  // Defaults
  FRaw := '';
  SetLength(FTokens, 0);
  SetLength(FWordBreaks, 0);
  SetLength(FLineBreaks, 0);
  SetLength(FWords, 0);
  FWord := '';
  FLine := '';
  FFinalized := False;
  FRightMargin := 10;

  // If stream output is sent to a destination without wordwrap,
  // the TatTokenResponse will find wordbreaks and split into lines by full words

  // Stream is tabulated into full words based on these break characters
  // !Syntax requires at least one!
  SetLength(FWordBreaks, 4);
  FWordBreaks[0] := ' ';
  FWordBreaks[1] := '-';
  FWordBreaks[2] := ',';
  FWordBreaks[3] := '.';

  // Stream may contain forced line breaks
  // !Syntax requires at least one!
  SetLength(FLineBreaks, 2);
  FLineBreaks[0] := #13;
  FLineBreaks[1] := #10;

  SetRightMargin(10);
  SetMaxLineLength(120);
end;

function TobTokenResponse.AddToken(const aToken: string): TobTokenPrintAction;
var
  LPrefix, LSuffix: string;
begin
  // Keep full original response
  FRaw := FRaw + aToken;                    // As continuous string
  Setlength(FTokens, Length(FTokens)+1);    // Make space
  FTokens[Length(FTokens)-1] := aToken;     // As an array

  // Accumulate "word"
  FWord := FWord + aToken;

  // If stream contains linebreaks, print token out without added linebreaks
  if HandleLineBreaks(aToken) then
    exit(TobTokenPrintAction.tpaAppend)

  // Check if a natural break exists, also split if word is longer than the allowed space
  // and print out token with or without linechange as needed
  else if SplitWord(FWord, LPrefix, LSuffix) or FFinalized then
    begin
      // On last call when Finalized we want access to the line change logic only
      // Bad design (fix on top of a fix) Would be better to separate word slipt and line logic from eachother
      if not FFinalized then
        begin
          Setlength(FWords, Length(FWords)+1);        // Make space
          FWords[Length(FWords)-1] := LPrefix;        // Add new word to array
          FWord := LSuffix;                         // Keep the remainder of the split
        end;

      // Word was split, so there is something that can be printed

      // Need for a new line?
      if Length(FLine) + Length(LastWord) > GetLineLengthMax() then
        begin
          Result  := TobTokenPrintAction.tpaNewline;
          FLine   := LastWord;                  // Reset Line (will be new line and then the word)
        end
      else
        begin
          Result  := TobTokenPrintAction.tpaAppend;
          FLine   := FLine + LastWord;          // Append to the line
        end;
    end
  else
    begin
      Result := TobTokenPrintAction.tpaWait;
    end;
end;

function TobTokenResponse.HandleLineBreaks(const AToken: string): Boolean;
var
  LLetter, LLineBreak: Integer;
begin
  Result := false;

  for LLetter := Length(AToken) downto 1 do                   // We are interested in the last possible linebreak
  begin
    for LLineBReak := 0 to Length(Self.FLineBreaks)-1 do       // Iterate linebreaks
    begin
      if AToken[LLetter] = FLineBreaks[LLineBreak] then        // If linebreak was found
      begin
        // Split into a word by last found linechange (do note the stored word may have more linebreak)
        Setlength(FWords, Length(FWords)+1);                          // Make space
        FWords[Length(FWords)-1] := FWord + LeftStr(AToken, Length(AToken)-LLetter); // Add new word to array

        // In case aToken did not end after last LF
        // Word and new line will have whatever was after the last linebreak
        FWord := RightStr(AToken, Length(AToken)-LLetter);
        FLine := FWord;

        // No need to go further
        exit(true);
      end;
    end;
  end;
end;

function TobTokenResponse.Finalize: Boolean;
begin
  // Buffer may contain something, if so make it into a word
  if FWord <> ''  then
    begin
      Setlength(FWords, Length(FWords)+1);      // Make space
      FWords[Length(FWords)-1] := FWord;        // Add new word to array
      Self.FFinalized := True;                // Remember Finalize was done (affects how last AddToken-call behaves)
      exit(true);
    end
  else
    Result := false;
end;

procedure TobTokenResponse.Clear();
begin
  FRaw := '';
  SetLength(FTokens, 0);
  SetLength(FWords, 0);
  FWord := '';
  FLine := '';
  FFinalized := False;
end;

function TobTokenResponse.LastWord(const ATrimLeft: Boolean): string;
begin
  Result := FWords[Length(FWords)-1];
  if ATrimLeft then
    Result := Result.TrimLeft;
end;

function TobTokenResponse.SplitWord(const AWord: string; var APrefix, ASuffix: string): Boolean;
var
  LLetter, LSeparator: Integer;
begin
  Result := false;

  for LLetter := 1 to Length(AWord) do               // Iterate whole word
  begin
    for LSeparator := 0 to Length(FWordBreaks)-1 do   // Iterate all separating characters
    begin
      if AWord[LLetter] = FWordBreaks[LSeparator] then // check for natural break
      begin
        // Let the world know there's stuff that can be a reason for a line change
        Result := True;

        APrefix := LeftStr(AWord, LLetter);
        ASuffix := RightStr(AWord, Length(AWord)-LLetter);
      end;
    end;
  end;

  // Maybe the word is too long but there was no natural break, then cut it to LineLengthMax
  if Length(AWord) > GetLineLengthMax() then
  begin
    Result := True;
    APrefix := LeftStr(AWord, GetLineLengthMax());
    ASuffix := RightStr(AWord, Length(AWord)-GetLineLengthMax());
  end;
end;

function TobTokenResponse.GetLineLengthMax(): Integer;
begin
  Result := FMaxLineLength - FRightMargin;
end;

function  TobTokenResponse.GetRightMargin(): Integer;
begin
  Result := FRightMargin;
end;

procedure TobTokenResponse.SetRightMargin(const AMargin: Integer);
begin
  FRightMargin := AMargin;
end;

function  TobTokenResponse.GetMaxLineLength(): Integer;
begin
  Result := FMaxLineLength;
end;

procedure TobTokenResponse.SetMaxLineLength(const ALength: Integer);
begin
  FMaxLineLength := ALength;
end;

{ TobPayloadStream }
procedure TobPayloadStream.InitFooter(out Footer: TPayloadStreamFooter);
begin
  FillChar(Footer, SizeOf(Footer), 0);
  Footer.WaterMark := cWaterMarkGUID;
end;

function TobPayloadStream.ReadFooter(const FileStream: TFileStream;
  out Footer: TPayloadStreamFooter): Boolean;
var
  FileLen: Int64;
begin
  // Check that file is large enough for a footer!
  FileLen := FileStream.Size;
  if FileLen > SizeOf(Footer) then
  begin
    // Big enough: move to start of footer and read it
    FileStream.Seek(-SizeOf(Footer), soEnd);
    FileStream.Read(Footer, SizeOf(Footer));
  end
  else
    // File not large enough for footer: zero it
    // .. this ensures watermark is invalid
    FillChar(Footer, SizeOf(Footer), 0);
  // Return if watermark is valid
  Result := IsEqualGUID(Footer.WaterMark, cWaterMarkGUID);
end;

constructor TobPayloadStream.Create(const FileName: string; const Mode: TPayloadStreamOpenMode);
var
  Footer: TPayloadStreamFooter; // footer record for payload data
begin
  inherited Create;
  // Open file stream
  fMode := Mode;
  case fMode of
    pomRead: fFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    pomWrite: fFileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive);
  end;
  // Check for existing payload
  if ReadFooter(fFileStream, Footer) then
  begin
    // We have payload: record start and size of data
    fDataStart := Footer.ExeSize;
    fDataSize := Footer.DataSize;
  end
  else
  begin
    // There is no existing payload: start is end of file
    fDataStart := fFileStream.Size;
    fDataSize := 0;
  end;
  // Set initial file position per mode
  case fMode of
    pomRead: fFileStream.Seek(fDataStart, soBeginning);
    pomWrite: fFileStream.Seek(fDataStart + fDataSize, soBeginning);
  end;
end;

destructor TobPayloadStream.Destroy;
var
  Footer: TPayloadStreamFooter; // payload footer record
begin
  if fMode = pomWrite then
  begin
    // We're in write mode: we need to update footer
    if fDataSize > 0 then
    begin
      // We have payload, so need a footer record
      InitFooter(Footer);
      Footer.ExeSize := fDataStart;
      Footer.DataSize := fDataSize;
      fFileStream.Seek(0, soEnd);
      fFileStream.WriteBuffer(Footer, SizeOf(Footer));
    end
    else
    begin
      // No payload => no footer
      fFileStream.Size := fDataStart;
    end;
  end;
  // Free file stream
  FreeAndNil(fFileStream);
  inherited;
end;

function TobPayloadStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  // Perform actual seek in underlying file stream
  Result := fFileStream.Seek(Offset, Origin);
end;

procedure TobPayloadStream.SetSize(const NewSize: Int64);
begin
  // Set size of file stream
  fFileStream.Size := NewSize;
end;

function TobPayloadStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  // Read data from file stream and return bytes read
  Result := fFileStream.Read(Buffer, Count);
end;

function TobPayloadStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  // Check in write mode
  if fMode <> pomWrite then
  begin
    raise EPayloadStream.Create(
      'TkaPayloadStream can''t write in read mode.');
  end;
  // Write the data to file stream and return bytes written
  Result := fFileStream.Write(Buffer, Count);
  // Check if stream has grown
  fDataSize := Max(fDataSize, fFileStream.Position - fDataStart);
end;

function TobPayloadStream.GetPosition: Int64;
begin
  Result := fFileStream.Position - fDataStart;
end;

procedure TobPayloadStream.SetPosition(const Value: Int64);
begin
  fFileStream.Position := fDataStart + Value;
end;

{ obUtils }
class constructor obUtils.Create();
begin
  QueryPerformanceFrequency(FPerformanceFrequency);

  FCriticalSection := TCriticalSection.Create();

  Randomize();

  FMimeMap := TDictionary<string, string>.Create;

  // Images
  FMimeMap.Add('.jpg', 'image/jpeg');
  FMimeMap.Add('.jpeg', 'image/jpeg');
  FMimeMap.Add('.png', 'image/png');
  FMimeMap.Add('.gif', 'image/gif');
  FMimeMap.Add('.bmp', 'image/bmp');
  FMimeMap.Add('.webp', 'image/webp');
  FMimeMap.Add('.svg', 'image/svg+xml');
  FMimeMap.Add('.ico', 'image/x-icon');

  // Documents
  FMimeMap.Add('.pdf', 'application/pdf');
  FMimeMap.Add('.doc', 'application/msword');
  FMimeMap.Add('.docx', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document');
  FMimeMap.Add('.xls', 'application/vnd.ms-excel');
  FMimeMap.Add('.xlsx', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
  FMimeMap.Add('.ppt', 'application/vnd.ms-powerpoint');
  FMimeMap.Add('.pptx', 'application/vnd.openxmlformats-officedocument.presentationml.presentation');
  FMimeMap.Add('.txt', 'text/plain');
  FMimeMap.Add('.csv', 'text/csv');
  FMimeMap.Add('.json', 'application/json');
  FMimeMap.Add('.xml', 'application/xml');
  FMimeMap.Add('.html', 'text/html');
  FMimeMap.Add('.htm', 'text/html');
  FMimeMap.Add('.rtf', 'application/rtf');

  // Archives
  FMimeMap.Add('.zip', 'application/zip');
  FMimeMap.Add('.rar', 'application/vnd.rar');
  FMimeMap.Add('.7z', 'application/x-7z-compressed');
  FMimeMap.Add('.tar', 'application/x-tar');
  FMimeMap.Add('.gz', 'application/gzip');
  FMimeMap.Add('.bz2', 'application/x-bzip2');
  FMimeMap.Add('.xz', 'application/x-xz');

  // Audio
  FMimeMap.Add('.mp3', 'audio/mpeg');
  FMimeMap.Add('.wav', 'audio/wav');
  FMimeMap.Add('.ogg', 'audio/ogg');
  FMimeMap.Add('.flac', 'audio/flac');
  FMimeMap.Add('.aac', 'audio/aac');
  FMimeMap.Add('.m4a', 'audio/mp4');

  // Video
  FMimeMap.Add('.mp4', 'video/mp4');
  FMimeMap.Add('.avi', 'video/x-msvideo');
  FMimeMap.Add('.mov', 'video/quicktime');
  FMimeMap.Add('.wmv', 'video/x-ms-wmv');
  FMimeMap.Add('.flv', 'video/x-flv');
  FMimeMap.Add('.mkv', 'video/x-matroska');
  FMimeMap.Add('.webm', 'video/webm');

  // Executables & Scripts
  FMimeMap.Add('.exe', 'application/octet-stream');
  FMimeMap.Add('.dll', 'application/octet-stream');
  FMimeMap.Add('.bat', 'application/x-msdos-program');
  FMimeMap.Add('.sh', 'application/x-sh');
  FMimeMap.Add('.ps1', 'application/x-powershell');
  FMimeMap.Add('.cmd', 'application/x-msdos-program');

  // Fonts
  FMimeMap.Add('.ttf', 'font/ttf');
  FMimeMap.Add('.otf', 'font/otf');
  FMimeMap.Add('.woff', 'font/woff');
  FMimeMap.Add('.woff2', 'font/woff2');

  // Code files
  FMimeMap.Add('.js', 'application/javascript');
  FMimeMap.Add('.css', 'text/css');
  FMimeMap.Add('.php', 'application/x-httpd-php');
  FMimeMap.Add('.py', 'text/x-python');
  FMimeMap.Add('.java', 'text/x-java-source');
  FMimeMap.Add('.c', 'text/x-c');
  FMimeMap.Add('.cpp', 'text/x-c++');
  FMimeMap.Add('.h', 'text/x-c');
  FMimeMap.Add('.cs', 'text/plain');
  FMimeMap.Add('.rb', 'text/x-ruby');
  FMimeMap.Add('.go', 'text/x-go');
  FMimeMap.Add('.swift', 'text/x-swift');
  FMimeMap.Add('.rs', 'text/x-rust');
end;

class destructor obUtils.Destroy();
begin
  FMimeMap.Free();
  FCriticalSection.Free();
end;

class procedure obUtils.UnitInit();
begin
  // force constructor
end;

class procedure obUtils.EnterCriticalSection();
begin
  FCriticalSection.Enter;
end;

class procedure obUtils.LeaveCriticalSection();
begin
  FCriticalSection.Leave;
end;


class function  obUtils.AsUTF8(const AText: string): Pointer;
begin
  Result := FMarshaller.AsUtf8(AText).ToPointer;
end;

class function obUtils.GetPhysicalProcessorCount(): DWORD;
var
  BufferSize: DWORD;
  Buffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  ProcessorInfo: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;
  Offset: DWORD;
begin
  Result := 0;
  BufferSize := 0;

  // Call GetLogicalProcessorInformation with buffer size set to 0 to get required buffer size
  if not GetLogicalProcessorInformation(nil, BufferSize) and (WinApi.Windows.GetLastError() = ERROR_INSUFFICIENT_BUFFER) then
  begin
    // Allocate buffer
    GetMem(Buffer, BufferSize);
    try
      // Call GetLogicalProcessorInformation again with allocated buffer
      if GetLogicalProcessorInformation(Buffer, BufferSize) then
      begin
        ProcessorInfo := Buffer;
        Offset := 0;

        // Loop through processor information to count physical processors
        while Offset + SizeOf(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= BufferSize do
        begin
          if ProcessorInfo.Relationship = RelationProcessorCore then
            Inc(Result);

          Inc(ProcessorInfo);
          Inc(Offset, SizeOf(SYSTEM_LOGICAL_PROCESSOR_INFORMATION));
        end;
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

class function obUtils.EnableVirtualTerminalProcessing(): DWORD;
var
  HOut: THandle;
  LMode: DWORD;
begin
  HOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if HOut = INVALID_HANDLE_VALUE then
  begin
    Result := GetLastError;
    Exit;
  end;

  if not GetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if not SetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  Result := 0;  // Success
end;

class function obUtils.GetEnvVarValue(const AVarName: string): string;
var
  LBufSize: Integer;
begin
  LBufSize := GetEnvironmentVariable(PChar(AVarName), nil, 0);
  if LBufSize > 0 then
    begin
      SetLength(Result, LBufSize - 1);
      GetEnvironmentVariable(PChar(AVarName), PChar(Result), LBufSize);
    end
  else
    Result := '';
end;

class function obUtils.IsStartedFromDelphiIDE(): Boolean;
begin
  // Check if the IDE environment variable is present
  Result := (GetEnvironmentVariable('BDS') <> '');
end;

class procedure obUtils.ProcessMessages();
var
  LMsg: TMsg;
begin
  while Integer(PeekMessage(LMsg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;

function _RandomRange(const aFrom, aTo: Integer): Integer;
var
  LFrom: Integer;
  LTo: Integer;
begin
  LFrom := aFrom;
  LTo := aTo;

  if AFrom > ATo then
    Result := Random(LFrom - LTo) + ATo
  else
    Result := Random(LTo - LFrom) + AFrom;
end;

class function  obUtils.RandomRange(const AMin, AMax: Integer): Integer;
begin
  Result := _RandomRange(AMin, AMax + 1);
end;

class function  obUtils.RandomRangef(const AMin, AMax: Single): Single;
var
  LNum: Single;
begin
  LNum := _RandomRange(0, MaxInt) / MaxInt;
  Result := AMin + (LNum * (AMax - AMin));
end;

class function  obUtils.RandomBool(): Boolean;
begin
  Result := Boolean(_RandomRange(0, 2) = 1);
end;

class function  obUtils.GetRandomSeed(): Integer;
begin
  Result := System.RandSeed;
end;

class procedure obUtils.SetRandomSeed(const AVaLue: Integer);
begin
  System.RandSeed := AVaLue;
end;

class procedure obUtils.Wait(const AMilliseconds: Double);
var
  LStartCount, LCurrentCount: Int64;
  LElapsedTime: Double;
begin
  // Get the starting value of the performance counter
  QueryPerformanceCounter(LStartCount);

  // Convert milliseconds to seconds for precision timing
  repeat
    QueryPerformanceCounter(LCurrentCount);
    LElapsedTime := (LCurrentCount - LStartCount) / FPerformanceFrequency * 1000.0; // Convert to milliseconds
  until LElapsedTime >= AMilliseconds;
end;

class function  obUtils.SanitizeToJson(const aText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(aText) do
  begin
    case aText[i] of
      '\': Result := Result + '\\';
      '"': Result := Result + '\"';
      '/': Result := Result + '\/';
      #8:  Result := Result + '\b';
      #9:  Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else
        Result := Result + aText[i];
    end;
  end;
  Result := Result;
end;

class function  obUtils.SanitizeFromJson(const aText: string): string;
var
  LText: string;
begin
  LText := aText;
  LText := LText.Replace('\n', #10);
  LText := LText.Replace('\r', #13);
  LText := LText.Replace('\b', #8);
  LText := LText.Replace('\t', #9);
  LText := LText.Replace('\f', #12);
  LText := LText.Replace('\/', '/');
  LText := LText.Replace('\"', '"');
  LText := LText.Replace('\\', '\');
  Result := LText;
end;

// Helper function to map Delphi types to JSON Schema types
function GetJsonType(DelphiType: TRttiType): string;
begin
  if not Assigned(DelphiType) then
    Exit('null');

  case DelphiType.TypeKind of
    tkInteger, tkInt64: Result := 'integer';
    tkFloat: Result := 'number';
    tkChar, tkWChar, tkString, tkLString, tkWString, tkUString: Result := 'string';
    tkEnumeration:
      begin
        if SameText(DelphiType.Name, 'Boolean') then
          Result := 'boolean'
        else
          Result := 'string';
      end;
    tkClass, tkRecord: Result := 'object';
    tkSet, tkDynArray, tkArray: Result := 'array';
  else
    Result := 'unknown';
  end;
end;

class function obUtils.GetJsonSchema(const AClass: TClass; const AMethodName: string): string;
var
  JsonRoot, JsonFunction, JsonParams, JsonProperties, JsonParamObj: TJSONObject;
  RequiredArray: TJSONArray;
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  Param: TRttiParameter;
  Attr: TCustomAttribute;
  ParamDescription: string;
begin
  Result := '';
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass.ClassInfo);
    if not Assigned(RttiType) then Exit;

    Method := RttiType.GetMethod(AMethodName);
    if not Assigned(Method) then Exit;

    // Ensure the method is a STATIC method
    if not (Method.MethodKind in [mkClassFunction, mkClassProcedure]) then
      Exit; // Return empty result if it's not a static method

    // Root JSON Object
    JsonRoot := TJSONObject.Create;
    JsonRoot.AddPair('type', 'function');

    // Function JSON Object
    JsonFunction := TJSONObject.Create;
    JsonFunction.AddPair('name', AMethodName);

    // Extract method description (if available)
    for Attr in Method.GetAttributes do
      if Attr is obSchemaDescription then
        JsonFunction.AddPair('description', obSchemaDescription(Attr).Description);

    // Parameter Section
    JsonParams := TJSONObject.Create;
    JsonParams.AddPair('type', 'object');

    JsonProperties := TJSONObject.Create;
    RequiredArray := TJSONArray.Create;

    for Param in Method.GetParameters do
    begin
      JsonParamObj := TJSONObject.Create;
      JsonParamObj.AddPair('type', GetJsonType(Param.ParamType));

      // Extract parameter description (if available)
      ParamDescription := '';
      for Attr in Param.GetAttributes do
        if Attr is obSchemaDescription then
          ParamDescription := obSchemaDescription(Attr).Description;

      if ParamDescription <> '' then
        JsonParamObj.AddPair('description', ParamDescription);

      JsonProperties.AddPair(Param.Name, JsonParamObj);
      RequiredArray.AddElement(TJSONString.Create(Param.Name));
    end;

    JsonParams.AddPair('properties', JsonProperties);
    JsonParams.AddPair('required', RequiredArray);
    JsonFunction.AddPair('parameters', JsonParams);

    // Return Type
    if Assigned(Method.ReturnType) then
      JsonFunction.AddPair('return_type', GetJsonType(Method.ReturnType))
    else
      JsonFunction.AddPair('return_type', 'void');

    JsonRoot.AddPair('function', JsonFunction);

    Result := JsonRoot.Format();
    JsonRoot.Free();

  finally
    Context.Free;
  end;
end;

class function obUtils.CallStaticMethod(const AClass: TClass; const AMethodName: string; const AArgs: array of TValue): TValue;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
begin
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass.ClassInfo);
    if not Assigned(RttiType) then
      raise Exception.Create('Class RTTI not found.');

    Method := RttiType.GetMethod(AMethodName);
    if not Assigned(Method) then
      raise Exception.CreateFmt('Method "%s" not found.', [AMethodName]);

    // Ensure the method is a class method (STATIC method)
    if not (Method.MethodKind in [mkClassFunction, mkClassProcedure]) then
      raise Exception.CreateFmt('Method "%s" is not a static class method.', [AMethodName]);

    // Invoke the method dynamically
    Result := Method.Invoke(nil, AArgs);
  finally
    Context.Free;
  end;
end;

class function obUtils.GetJsonSchemas(AClass: TClass): string;
var
  JsonRoot, JsonTool: TJSONObject;
  JsonToolsArray: TJSONArray;
  Context: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
begin
  Result := '';
  JsonRoot := TJSONObject.Create;
  JsonToolsArray := TJSONArray.Create;
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AClass.ClassInfo);
    if not Assigned(RttiType) then Exit;

    // Loop through all published methods
    for Method in RttiType.GetMethods do
    begin
      // Ensure the method is published and static
      if (Method.Visibility = mvPublished) and
         (Method.MethodKind in [mkClassFunction, mkClassProcedure]) then
      begin
        // Get the JSON schema for the method
        JsonTool := TJSONObject.ParseJSONValue(GetJsonSchema(AClass, Method.Name)) as TJSONObject;
        if Assigned(JsonTool) then
          JsonToolsArray.AddElement(JsonTool);
      end;
    end;

    // Add tools array to the root JSON object
    JsonRoot.AddPair('tools', JsonToolsArray);
    Result := JsonRoot.Format();
    JsonRoot.Free();
  finally
    Context.Free;
  end;
end;

class function obUtils.GetISO8601DateTime(): string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now);
end;

class function obUtils.GetISO8601DateTimeLocal(): string;
var
  TZI: TTimeZoneInformation;
  Bias, HoursOffset, MinsOffset: Integer;
  TimeZoneStr: string;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD, TIME_ZONE_ID_DAYLIGHT:
      Bias := TZI.Bias + TZI.DaylightBias; // Adjust for daylight saving time
    else
      Bias := 0; // Default to UTC if timezone is unknown
  end;

  HoursOffset := Abs(Bias) div 60;
  MinsOffset := Abs(Bias) mod 60;

  if Bias = 0 then
    TimeZoneStr := 'Z'
  else
    TimeZoneStr := Format('%s%.2d:%.2d', [IfThen(Bias > 0, '-', '+'), HoursOffset, MinsOffset]);

  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now) + TimeZoneStr;
end;

class function obUtils.GetLocalDateTime(): string;
begin
 Result := FormatDateTime('dddd, dd mmmm yyyy hh:nn:ss AM/PM', Now);
end;

class function obUtils.HasEnoughDiskSpace(const AFilePath: string; ARequiredSize: Int64): Boolean;
var
  LFreeAvailable, LTotalSpace, LTotalFree: Int64;
  LDrive: string;
begin
  Result := False;

  // Resolve the absolute path in case of a relative path
  LDrive := ExtractFileDrive(TPath.GetFullPath(AFilePath));

  // If there is no drive letter, use the current drive
  if LDrive = '' then
    LDrive := ExtractFileDrive(TDirectory.GetCurrentDirectory);

  // Ensure drive has a trailing backslash
  if LDrive <> '' then
    LDrive := LDrive + '\';

  if GetDiskFreeSpaceEx(PChar(LDrive), LFreeAvailable, LTotalSpace, @LTotalFree) then
    Result := LFreeAvailable >= ARequiredSize;
end;

class procedure obUtils.StringToStream(const AString: string; const AStream: TStream);
var
  n: LongInt;
begin
  n := ByteLength(AString);
  AStream.Write(n, SizeOf(n));
  if n > 0 then AStream.Write(AString[1], n);
end;

class function obUtils.StringFromStream(const AStream: TStream): string;
var
  n: LongInt;
begin
  AStream.Read(n, SizeOf(n));
  SetLength(Result, n div SizeOf(Char));
  if n > 0 then AStream.Read(Result[1], n);
end;

class function obUtils.AddPathToSystemPath(const APath: string): Boolean;
var
  LCurrentPath: string;
begin
  Result := False;
  if not TDirectory.Exists(APath) then Exit;

  SetLength(LCurrentPath, GetEnvironmentVariable('PATH', nil, 0) - 1);
  GetEnvironmentVariable('PATH', PChar(LCurrentPath), Length(LCurrentPath) + 1);

  if not LCurrentPath.Contains(APath) then
  begin
    LCurrentPath := APath + ';' + LCurrentPath;
    if SetEnvironmentVariable('PATH', PChar(LCurrentPath)) then
    begin
      SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar('Environment')));
      Result := True;
    end;
  end;
end;

class function obUtils.RemovePathFromSystemPath(const APath: string): Boolean;
var
  LCurrentPath: string;
  LNewPath: string;
begin
  Result := False;

  if not TDirectory.Exists(APath) then Exit;

  SetLength(LCurrentPath, GetEnvironmentVariable('PATH', nil, 0) - 1);
  GetEnvironmentVariable('PATH', PChar(LCurrentPath), Length(LCurrentPath) + 1);

  LNewPath := LCurrentPath.Replace(';' + APath, '', [rfReplaceAll, rfIgnoreCase]);
  LNewPath := LNewPath.Replace(APath + ';', '', [rfReplaceAll, rfIgnoreCase]);
  LNewPath := LNewPath.Replace(APath, '', [rfReplaceAll, rfIgnoreCase]);

  if LNewPath <> LCurrentPath then
  begin
    if SetEnvironmentVariable('PATH', PChar(LNewPath)) then
    begin
      SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar('Environment')));
      Result := True;
    end;
  end;
end;

class function  obUtils.GetEXEPath(): string;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
end;

class function  obUtils.RemoveQuotes(const AText: string): string;
var
  S: string;
begin
  S := AnsiDequotedStr(aText, '"');
  Result := AnsiDequotedStr(S, '''');
end;

class function obUtils.FormatNumberWithCommas(const AValue: Int64): string;
begin
  Result := FormatFloat('#,##0', AValue);
end;

class function obUtils.GetRandomThinkingResult(): string;
const
  CMessages: array[0..9] of string = (
    'Here’s what I came up with:',
    'This is what I found:',
    'Here’s my answer:',
    'Done! Here’s the result:',
    'Here’s my response:',
    'I’ve worked it out:',
    'Processing complete. Here’s my output:',
    'Here’s what I think:',
    'After thinking it through, here’s my take:',
    'Solution ready! Check this out:'
  );
begin
  Result := CMessages[Random(Length(CMessages))];
end;

class function obUtils.FileToBase64(const AFilename: string): string;
var
  LFileStream: TFileStream;
  LBytesStream: TBytesStream;
begin
  Result := '';
  if AFilename.IsEmpty then Exit;

  if not TFile.Exists(AFilename) then Exit;

  LFileStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    LBytesStream := TBytesStream.Create;
    try
      LBytesStream.CopyFrom(LFileStream, LFileStream.Size);
      Result := TNetEncoding.Base64.EncodeBytesToString(LBytesStream.Bytes, LBytesStream.Size);

      // Remove newline characters
      Result := StringReplace(Result, #13, '', [rfReplaceAll]); // Remove carriage returns
      Result := StringReplace(Result, #10, '', [rfReplaceAll]); // Remove line feeds

    finally
      LBytesStream.Free;
    end;
  finally
    LFileStream.Free;
  end;
end;

class function obUtils.GetFormattedDate(): string;
const
  MonthNames: array[1..12] of string =
    ('January', 'February', 'March', 'April', 'May', 'June',
     'July', 'August', 'September', 'October', 'November', 'December');
  DaySuffix: array[1..31] of string =
    ('st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th', 'th',
     'th', 'th', 'th', 'th', 'th', 'th', 'th', 'th', 'th', 'th',
     'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th', 'th',
     'st');
var
  Today: TDateTime;
  Day, Year: Word;
  Month: string;
  Suffix: string;
begin
  Today := Now;
  Day := DayOf(Today);
  Year := YearOf(Today);
  Month := MonthNames[MonthOf(Today)];
  Suffix := DaySuffix[Day];

  Result := Format('%s, %s %d%s, %d', [
    FormatDateTime('dddd', Today), Month, Day, Suffix, Year
  ]);
end;

class function  obUtils.GetMimeTypeFromExt(const AFileName: string): string;
var
  LFilename: string;
  LExt: string;
begin
  Result := '';
  LFilename := AFilename.Trim();
  if LFilename.IsEmpty then Exit;

  LExt := TPath.GetExtension(AFileName).ToLower;

   if FMimeMap.ContainsKey(LExt) then
      Result := FMimeMap[LExt];
end;

class function obUtils.GetTimeZoneAbbreviation(): string;
var
  TZI: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TZI) of
    TIME_ZONE_ID_STANDARD: Result := TZI.StandardName;
    TIME_ZONE_ID_DAYLIGHT: Result := TZI.DaylightName;
  else
    Result := 'UTC'; // Fallback
  end;
end;

class function obUtils.GetFormattedTime(): string;
var
  Hour, Min, Sec, MSec: Word;
  AMPM, TimeZone: string;
begin
  DecodeTime(Now, Hour, Min, Sec, MSec);

  if Hour = 0 then
  begin
    Hour := 12;
    AMPM := 'AM';
  end
  else if Hour < 12 then
    AMPM := 'AM'
  else if Hour = 12 then
    AMPM := 'PM'
  else
  begin
    Hour := Hour - 12;
    AMPM := 'PM';
  end;

  TimeZone := GetTimeZoneAbbreviation;
  Result := Format('%d:%.2d %s %s', [Hour, Min, AMPM, TimeZone]);
end;

const
  ENV_REG_PATH = 'Environment';  // Registry path for user variables

// Set a persistent environment variable (current user)
class procedure  obUtils.SetUserEnvironmentVariable(const AName, AValue: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(ENV_REG_PATH, True) then
    begin
      Reg.WriteString(AName, AValue);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  // Notify Windows about the change
  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar(ENV_REG_PATH)));
end;

// Remove a persistent environment variable (current user)
class procedure  obUtils.RemoveUserEnvironmentVariable(const AName: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(ENV_REG_PATH, False) then
    begin
      if Reg.ValueExists(AName) then
      begin
        Reg.DeleteValue(AName);
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;

  // Notify Windows about the change
  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar(ENV_REG_PATH)));
end;

// Check if a persistent environment variable exists
class function obUtils.UserEnvironmentVariableExists(const AName: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(ENV_REG_PATH) then
    begin
      Result := Reg.ValueExists(AName);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

// Retrieve a persistent environment variable's value (current user)
class function  obUtils.GetUserEnvironmentVariable(const AName: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(ENV_REG_PATH) and Reg.ValueExists(AName) then
    begin
      Result := Reg.ReadString(AName);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

class function obUtils.TrimLeadingCRLF(const S: string): string;
var
  I, LLen: Integer;
begin
  LLen := Length(S);
  I := 1;
  while (I <= LLen) and ((S[I] = #10) or (S[I] = #13)) do
    Inc(I);
  Result := Copy(S, I, LLen - I + 1);
end;

class function obUtils.TrimTrailingCRLF(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and ((S[I] = #10) or (S[I] = #13)) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

class function obUtils.DownloadFile(const AUrl, ATargetFile: string; const AProgressCallback: DownloadFileProgressCallback): Boolean;
var
  LHttpClient: THTTPClient;
  LResponse: IHTTPResponse;
  LFileStream: TFileStream;
  LPercent: Single;
  LFilename: string;
begin
  Result := False;

  LFilename := TPath.GetFileName(AUrl);

  LHttpClient := THTTPClient.Create;
  try
    try
      // Create a file stream for the target file
      TDirectory.CreateDirectory(ExtractFilePath(ATargetFile));
      LFileStream := TFileStream.Create(ATargetFile, fmCreate);
      try
        // Configure HTTP client
        LHttpClient.AllowCookies := True;
        LHttpClient.HandleRedirects := True;

        // Set up the OnReceiveData event to track download progress
        LPercent := -1;
        LHttpClient.ReceiveDataCallBack :=
          procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var Abort: Boolean)
          var
            LNewPercent: Single;
          begin
            if AContentLength > 0 then
            begin
              LNewPercent := (AReadCount / AContentLength) * 100;
              if (LNewPercent <> LPercent) and Assigned(AProgressCallback) then
              begin
                LPercent := LNewPercent;
                AProgressCallback(LFilename, LPercent, Abort);
              end;
            end;
          end;

        // Execute the GET request with the progress tracking
        LResponse := LHttpClient.Get(AUrl, LFileStream);

        if LResponse.StatusCode <> 200 then
          Exit;

        Result := TFile.Exists(ATargetFile);
      finally
        LFileStream.Free;
      end;
    except
      on E: Exception do
        Exit;
    end;
  finally
    LHttpClient.Free;
  end;
end;

class function obUtils.UnzipFile(const AZipFile, ATargetDir: string; const AProgress: UnzipFileProgressCallback): Boolean;
var
  LZip: TZipFile;
  I, LTotal: Integer;
  LEntryName: string;
  LPercent: Single;
  LAbort: Boolean;
begin
  Result := False;
  LAbort := False;
  LZip := TZipFile.Create;
  try
    LZip.Open(AZipFile, zmRead);
    if not TDirectory.Exists(ATargetDir) then
      TDirectory.CreateDirectory(ATargetDir);

    LTotal := LZip.FileCount;
    for I := 0 to LTotal - 1 do
    begin
      LEntryName := LZip.FileNames[I];
      LPercent := (I + 1) / LTotal * 100;
      if Assigned(AProgress) then
        AProgress(LEntryName, LPercent, LAbort);
      if LAbort then
        Exit(False);
      LZip.Extract(I, ATargetDir, True);
    end;
    Result := True;
  except
    // silently fail and return false
  end;
  LZip.Free;
end;

class procedure obUtils.DeleteFolder(const AFolder: string; const ARecursive: Boolean);
begin
  if TDirectory.Exists(AFolder) then
    TDirectory.Delete(AFolder, ARecursive);
end;

class function obUtils.TavilyWebSearch(const AAPIKey, AQuery: string): string;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JsonRequest, JsonResponse: TJSONObject;
  StringContent: TStringStream;
  Url: string;
  ResultsArray: TJSONArray;
  ResultsString: string;
  I: Integer;
  ResultObj: TJSONObject;
begin
  Result := '';
  if AAPIKey.IsEmpty then Exit;
  HttpClient := THTTPClient.Create;
  try
    // Set the API URL
    Url := 'https://api.tavily.com/search';
    // Create JSON request body
    JsonRequest := TJSONObject.Create;
    try
      JsonRequest.AddPair('api_key', AAPIKey);
      JsonRequest.AddPair('query', AQuery);
      JsonRequest.AddPair('topic', 'general');
      JsonRequest.AddPair('search_depth', 'advanced');
      // Note: 'include_answer' was duplicated in the original code, removed one instance
      JsonRequest.AddPair('include_answer', TJSONBool.Create(True));
      JsonRequest.AddPair('include_images', TJSONBool.Create(False));
      JsonRequest.AddPair('include_image_descriptions', TJSONBool.Create(False));
      JsonRequest.AddPair('include_raw_content', TJSONBool.Create(False));
      JsonRequest.AddPair('max_results', TJSONNumber.Create(5));
      JsonRequest.AddPair('include_domains', TJSONArray.Create); // Empty array
      JsonRequest.AddPair('exclude_domains', TJSONArray.Create); // Empty array
      // Convert JSON to string stream
      StringContent := TStringStream.Create(JsonRequest.ToString, TEncoding.UTF8);
      try
        // Set content type to application/json
        HttpClient.ContentType := 'application/json';
        // Perform the POST request
        Response := HttpClient.Post(Url, StringContent);
        // Check if the response is successful
        if Response.StatusCode = 200 then
        begin
          // Parse the JSON response
          JsonResponse := TJSONObject.ParseJSONValue(Response.ContentAsString(TEncoding.UTF8)) as TJSONObject;
          try
            ResultsString := '';
            // Get the results array
            if JsonResponse.TryGetValue<TJSONArray>('results', ResultsArray) then
            begin
              // Convert results array to string
              for I := 0 to ResultsArray.Count - 1 do
              begin
                ResultObj := ResultsArray.Items[I] as TJSONObject;
                ResultsString := ResultsString + Format('Result %d: Title: %s, URL: %s, Content: %s',
                  [I + 1,
                   ResultObj.GetValue<string>('title'),
                   ResultObj.GetValue<string>('url'),
                   ResultObj.GetValue<string>('content')]);

                // Add newline if not the last item
                if I < ResultsArray.Count - 1 then
                  ResultsString := ResultsString + sLineBreak;
              end;
            end;

            // Extract the 'answer' field from the response
            if JsonResponse.TryGetValue('answer', Result) then
            begin
              // Add the results string to the answer
              if not ResultsString.IsEmpty then
                Result := Result + sLineBreak + sLineBreak + 'Search Results:' + sLineBreak + ResultsString;
                Result := Result.Trim();
                Result := TrimLeadingCRLF(Result);
                Result := TrimTrailingCRLF(Result);
            end
            else
            begin
              // If no answer but we have results, return just the results
              if not ResultsString.IsEmpty then
              begin
                Result := 'Search Results:' + sLineBreak + ResultsString;
                Result := Result.Trim();
                Result := TrimLeadingCRLF(Result);
                Result := TrimTrailingCRLF(Result);
              end
              else
                raise Exception.Create('Both "answer" field and "results" array are missing in the API response.');
            end;
          finally
            JsonResponse.Free;
          end;
        end
        else
        begin
          raise Exception.CreateFmt('Error: %d - %s', [Response.StatusCode, Response.StatusText]);
        end;
      finally
        StringContent.Free;
      end;
    finally
      JsonRequest.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

class function obUtils.LemonfoxTTS(const AAPIKey: string; const AInputText: string; const AOutputFile: string; const AVoice: string; const ALanguage: string; const AFormat: string; const ASpeed: Single; const AWordTimestamps: Boolean): Boolean;
var
  HTTPClient: THTTPClient;
  StringContent: TStringStream;
  Response: IHTTPResponse;
  JSONBody: TJSONObject;
  FileStream: TFileStream;
  ResponseStream: TMemoryStream;
  JSONResponse: TJSONObject;
  Base64Audio: string;
begin
  Result := False;

  HTTPClient := THTTPClient.Create;
  try
    HTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + AAPIKey;
    HTTPClient.CustomHeaders['Content-Type'] := 'application/json';

    JSONBody := TJSONObject.Create;
    try
      JSONBody.AddPair('voice', AVoice);
      JSONBody.AddPair('language', ALanguage);
      JSONBody.AddPair('response_format', AFormat);
      JSONBody.AddPair('speed', TJSONNumber.Create(ASpeed));
      JSONBody.AddPair('word_timestamps', TJSONBool.Create(AWordTimestamps));
      JSONBody.AddPair('input', AInputText);

      StringContent := TStringStream.Create(JSONBody.ToString, TEncoding.UTF8);
      try
        Response := HTTPClient.Post('https://api.lemonfox.ai/v1/audio/speech', StringContent);

        if Response.StatusCode = 200 then
        begin
          if TFile.Exists(AOutputFile) then
          begin
            TFile.Delete(AOutputFile);
          end;

          if AWordTimestamps then
          begin
            JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString(TEncoding.UTF8)) as TJSONObject;
            try
              if Assigned(JSONResponse) and JSONResponse.TryGetValue<string>('audio_base64', Base64Audio) then
              begin
                // Decode Base64 audio and save to file
                FileStream := TFileStream.Create(AOutputFile, fmCreate);
                try
                  FileStream.WriteBuffer(TNetEncoding.Base64.DecodeStringToBytes(Base64Audio)[0],
                    Length(TNetEncoding.Base64.DecodeStringToBytes(Base64Audio)));
                  Result := TFile.Exists(AOutputFile);
                finally
                  FileStream.Free;
                end;
              end;
            finally
              JSONResponse.Free;
            end;
          end
          else
          begin
            // Save the binary content directly to file
            ResponseStream := TMemoryStream.Create;
            try
              Response.ContentStream.Position := 0;
              ResponseStream.CopyFrom(Response.ContentStream, Response.ContentStream.Size);
              ResponseStream.SaveToFile(AOutputFile);
              Result := TFile.Exists(AOutputFile);
            finally
              ResponseStream.Free;
            end;
          end;
        end
        else
          raise Exception.CreateFmt('Error: %d - %s', [Response.StatusCode, Response.StatusText]);
      finally
        StringContent.Free;
      end;
    finally
      JSONBody.Free;
    end;
  finally
    HTTPClient.Free;
  end;
end;

class function obUtils.JinaEmbeddings(const AApiKey: string; const AInputs: array of string; const ARole: string; AModel: string; const ADimensions: Integer; const ANormalize: Boolean; const AEmbeddingType: string): TArray<TArray<Single>>;
const
  JINA_API_URL = 'https://api.jina.ai/v1/embeddings';
var
  LHttpClient: THttpClient;
  LRequestBody: TStringStream;
  LResponse: IHTTPResponse;
  LJsonRequest, LJsonResponse, LJsonEmbedding: TJSONObject;
  LInputArray, LEmbeddingsArray: TJSONArray;
  I, J: Integer;

  LDataArray: TJSONArray;
  LEmbeddingObject: TJSONObject;

  // Function to check if a string is Base64 encoded
  function IsBase64(const S: string): Boolean;
  const
    Base64Chars = ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '='];
  var
    I, LLen: Integer;
  begin
    Result := False;
    LLen := Length(S);

    // Base64 should be at least 4 characters and multiple of 4
    if (LLen < 4) or (LLen mod 4 <> 0) then
      Exit;

    // Check if all characters are valid Base64
    for I := 1 to LLen do
    begin
      if not CharInSet(S[I], Base64Chars) then
        Exit;
    end;

    // If the string passes all checks, assume it's Base64
    Result := True;
  end;

begin
  SetLength(Result, 0);

  LHttpClient := THttpClient.Create;
  try
    LHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + AApiKey;
    LHttpClient.ContentType := 'application/json';

    LJsonRequest := TJSONObject.Create;
    try
      LJsonRequest.AddPair('model', AModel);
      LJsonRequest.AddPair('dimensions', TJSONNumber.Create(ADimensions));
      LJsonRequest.AddPair('normalized', TJSONBool.Create(ANormalize));
      LJsonRequest.AddPair('embedding_type', AEmbeddingType);
      if ARole = 'query' then
        LJsonRequest.AddPair('task', 'retrieval.query');

      // Add Inputs (Supports text, image URLs, and Base64 images)
      LInputArray := TJSONArray.Create;
      for I := 0 to High(AInputs) do
      begin
        LJsonEmbedding := TJSONObject.Create;

        if Pos('http', AInputs[I]) = 1 then
          LJsonEmbedding.AddPair('image', AInputs[I])  // If input is an image URL
        else if IsBase64(AInputs[I]) then
          LJsonEmbedding.AddPair('image', AInputs[I])  // If input is Base64
        else
          LJsonEmbedding.AddPair('text', AInputs[I]);  // If input is text

        LInputArray.AddElement(LJsonEmbedding);
      end;
      LJsonRequest.AddPair('input', LInputArray);

      LRequestBody := TStringStream.Create(LJsonRequest.ToString, TEncoding.UTF8);
      try
        LResponse := LHttpClient.Post(JINA_API_URL, LRequestBody);
        if LResponse.StatusCode <> 200 then
          raise Exception.CreateFmt('Jina API request failed: %d - %s', [LResponse.StatusCode, LResponse.StatusText]);

        LJsonResponse := TJSONObject.ParseJSONValue(LResponse.ContentAsString(TEncoding.UTF8)) as TJSONObject;
        try
          if not Assigned(LJsonResponse) then
            raise Exception.Create('Invalid JSON response from Jina API.');

          // Handle API errors if response contains "error" field
          if LJsonResponse.GetValue('error') <> nil then
            raise Exception.Create('Jina API Error: ' + LJsonResponse.GetValue('error').Value);

          // Check if "data" exists and is a valid array
          LDataArray := LJsonResponse.GetValue('data') as TJSONArray;
          if not Assigned(LDataArray) then
            raise Exception.Create('"data" field is missing or not an array.');

          SetLength(Result, LDataArray.Count);

          // Iterate through each embedding entry
          for I := 0 to LDataArray.Count - 1 do
          begin
            LEmbeddingObject := LDataArray.Items[I] as TJSONObject;

            // Get the actual "embedding" array
            LEmbeddingsArray := LEmbeddingObject.GetValue('embedding') as TJSONArray;
            if not Assigned(LEmbeddingsArray) then
              raise Exception.CreateFmt('Missing "embedding" field in entry %d.', [I]);

            // Store embedding values
            SetLength(Result[I], LEmbeddingsArray.Count);
            for J := 0 to LEmbeddingsArray.Count - 1 do
              Result[I][J] := LEmbeddingsArray.Items[J].AsType<Double>;
          end;

        finally
          LJsonResponse.Free;
        end;
      finally
        LRequestBody.Free;
      end;
    finally
      LJsonRequest.Free;
    end;
  finally
    LHttpClient.Free;
  end;
end;

class procedure obUtils.MessageBox(const ATitle: string; const AMsg: string; const AArgs: array of const);
begin
  WinApi.Windows.MessageBox(0, PWideChar(Format(AMsg, AArgs)), PWideChar(ATitle), MB_OK);
end;

class function obUtils.GetFullPath(const ABasePath, ARelativePath: string): string;
  function NormalizeBasePath(const APath: string): string;
  begin
    if DirectoryExists(APath) then
      Result := IncludeTrailingPathDelimiter(APath)
    else
      Result := IncludeTrailingPathDelimiter(ExtractFilePath(APath));
  end;
begin
  //Result := ExpandFileName(NormalizeBasePath(ABasePath) + ARelativePath);
  if TPath.IsRelativePath(ARelativePath) then
    Result := ExpandFileName(IncludeTrailingPathDelimiter(
                IfThen(DirectoryExists(ABasePath),
                  ABasePath,
                  ExtractFilePath(ABasePath))) + ARelativePath)
  else
    Result := ExpandFileName(ARelativePath);
end;

{ obConsole }
class constructor obConsole.Create();
begin
  FTeletypeDelay := 0;

  // save current console codepage
  FInputCodePage := GetConsoleCP();
  FOutputCodePage := GetConsoleOutputCP();

  // set code page to UTF8
  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);

  obUtils.EnableVirtualTerminalProcessing();
end;

class destructor obConsole.Destroy();
begin
  // restore code page
  SetConsoleCP(FInputCodePage);
  SetConsoleOutputCP(FOutputCodePage);
end;

class procedure obConsole.UnitInit();
begin
end;

class procedure obConsole.Print(const AMsg: string);
begin
  if not HasOutput() then Exit;
  Write(AMsg+obCSIResetFormat);
end;

class procedure obConsole.PrintLn(const AMsg: string);
begin
  if not HasOutput() then Exit;
  WriteLn(AMsg+obCSIResetFormat);
end;

class procedure obConsole.Print(const AMsg: string; const AArgs: array of const);
begin
  if not HasOutput() then Exit;
  Write(Format(AMsg, AArgs)+obCSIResetFormat);
end;

class procedure obConsole.PrintLn(const AMsg: string; const AArgs: array of const);
begin
  if not HasOutput() then Exit;
  WriteLn(Format(AMsg, AArgs)+obCSIResetFormat);
end;

class procedure obConsole.Print();
begin
  if not HasOutput() then Exit;
  Write(obCSIResetFormat);
end;

class procedure obConsole.PrintLn();
begin
  if not HasOutput() then Exit;
  WriteLn(obCSIResetFormat);
end;

class procedure obConsole.GetCursorPos(X, Y: PInteger);
var
  hConsole: THandle;
  BufferInfo: TConsoleScreenBufferInfo;
begin
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  if hConsole = INVALID_HANDLE_VALUE then
    Exit;

  if not GetConsoleScreenBufferInfo(hConsole, BufferInfo) then
    Exit;

  if Assigned(X) then
    X^ := BufferInfo.dwCursorPosition.X;
  if Assigned(Y) then
    Y^ := BufferInfo.dwCursorPosition.Y;
end;

class procedure obConsole.SetCursorPos(const X, Y: Integer);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSICursorPos, [X, Y]));
end;

class procedure obConsole.SetCursorVisible(const AVisible: Boolean);
var
  ConsoleInfo: TConsoleCursorInfo;
  ConsoleHandle: THandle;
begin
  ConsoleHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  ConsoleInfo.dwSize := 25; // You can adjust cursor size if needed
  ConsoleInfo.bVisible := AVisible;
  SetConsoleCursorInfo(ConsoleHandle, ConsoleInfo);
end;

class procedure obConsole.HideCursor();
begin
  if not HasOutput() then Exit;
  Write(obCSIHideCursor);
end;

class procedure obConsole.ShowCursor();
begin
  if not HasOutput() then Exit;
  Write(obCSIShowCursor);
end;

class procedure obConsole.SaveCursorPos();
begin
  if not HasOutput() then Exit;
  Write(obCSISaveCursorPos);
end;

class procedure obConsole.RestoreCursorPos();
begin
  if not HasOutput() then Exit;
  Write(obCSIRestoreCursorPos);
end;

class procedure obConsole.MoveCursorUp(const ALines: Integer);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSICursorUp, [ALines]));
end;

class procedure obConsole.MoveCursorDown(const ALines: Integer);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSICursorDown, [ALines]));
end;

class procedure obConsole.MoveCursorForward(const ACols: Integer);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSICursorForward, [ACols]));
end;

class procedure obConsole.MoveCursorBack(const ACols: Integer);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSICursorBack, [ACols]));
end;

class procedure obConsole.ClearScreen();
begin
  if not HasOutput() then Exit;
  Write(#12);
  Write(obCSIClearScreen);
  Write(obCSICursorHomePos);
end;

class procedure obConsole.ClearLine();
begin
  if not HasOutput() then Exit;
  Write(obCR);
  Write(obCSIClearLine);
end;

class procedure obConsole.ClearToEndOfLine();
begin
  if not HasOutput() then Exit;
  Write(obCSIClearToEndOfLine);
end;

class procedure obConsole.ClearLineFromCursor(const AColor: string);
var
  LConsoleOutput: THandle;
  LConsoleInfo: TConsoleScreenBufferInfo;
  LNumCharsWritten: DWORD;
  LCoord: TCoord;
begin
  LConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);

  if GetConsoleScreenBufferInfo(LConsoleOutput, LConsoleInfo) then
  begin
    LCoord.X := 0;
    LCoord.Y := LConsoleInfo.dwCursorPosition.Y;

    Print(AColor, []);
    FillConsoleOutputCharacter(LConsoleOutput, ' ', LConsoleInfo.dwSize.X
      - LConsoleInfo.dwCursorPosition.X, LCoord, LNumCharsWritten);
    SetConsoleCursorPosition(LConsoleOutput, LCoord);
  end;
end;

class procedure obConsole.SetBoldText();
begin
  if not HasOutput() then Exit;
  Write(obCSIBold);
end;

class procedure obConsole.ResetTextFormat();
begin
  if not HasOutput() then Exit;
  Write(obCSIResetFormat);
end;

class procedure obConsole.SetForegroundColor(const AColor: string);
begin
  if not HasOutput() then Exit;
  Write(AColor);
end;

class procedure obConsole.SetBackgroundColor(const AColor: string);
begin
  if not HasOutput() then Exit;
  Write(AColor);
end;

class procedure obConsole.SetForegroundRGB(const ARed, AGreen, ABlue: Byte);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSIFGRGB, [ARed, AGreen, ABlue]));
end;

class procedure obConsole.SetBackgroundRGB(const ARed, AGreen, ABlue: Byte);
begin
  if not HasOutput() then Exit;
  Write(Format(obCSIBGRGB, [ARed, AGreen, ABlue]));
end;

class procedure obConsole.GetSize(AWidth: PInteger; AHeight: PInteger);
var
  LConsoleInfo: TConsoleScreenBufferInfo;
begin
  if not HasOutput() then Exit;

  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), LConsoleInfo);
  if Assigned(AWidth) then
    AWidth^ := LConsoleInfo.dwSize.X;

  if Assigned(AHeight) then
  AHeight^ := LConsoleInfo.dwSize.Y;
end;

class procedure obConsole.SetTitle(const ATitle: string);
begin
  WinApi.Windows.SetConsoleTitle(PChar(ATitle));
end;

class function  obConsole.GetTitle(): string;
const
  MAX_TITLE_LENGTH = 1024;
var
  LTitle: array[0..MAX_TITLE_LENGTH] of WideChar;
  LTitleLength: DWORD;
begin
  // Get the console title and store it in LTitle
  LTitleLength := GetConsoleTitleW(LTitle, MAX_TITLE_LENGTH);

  // If the title is retrieved, assign it to the result
  if LTitleLength > 0 then
    Result := string(LTitle)
  else
    Result := '';
end;

class function  obConsole.HasOutput(): Boolean;
var
  LStdHandle: THandle;
begin
  LStdHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  Result := (LStdHandle <> INVALID_HANDLE_VALUE) and
            (GetFileType(LStdHandle) = FILE_TYPE_CHAR);
end;

class function  obConsole.WasRunFrom(): Boolean;
var
  LStartupInfo: TStartupInfo;
begin
  LStartupInfo.cb := SizeOf(TStartupInfo);
  GetStartupInfo(LStartupInfo);
  Result := ((LStartupInfo.dwFlags and STARTF_USESHOWWINDOW) = 0);
end;

class procedure obConsole.WaitForAnyKey();
var
  LInputRec: TInputRecord;
  LNumRead: Cardinal;
  LOldMode: DWORD;
  LStdIn: THandle;
begin
  LStdIn := GetStdHandle(STD_INPUT_HANDLE);
  GetConsoleMode(LStdIn, LOldMode);
  SetConsoleMode(LStdIn, 0);
  repeat
    ReadConsoleInput(LStdIn, LInputRec, 1, LNumRead);
  until (LInputRec.EventType and KEY_EVENT <> 0) and
    LInputRec.Event.KeyEvent.bKeyDown;
  SetConsoleMode(LStdIn, LOldMode);
end;

class function  obConsole.AnyKeyPressed(): Boolean;
var
  LNumberOfEvents     : DWORD;
  LBuffer             : TInputRecord;
  LNumberOfEventsRead : DWORD;
  LStdHandle           : THandle;
begin
  Result:=false;
  //get the console handle
  LStdHandle := GetStdHandle(STD_INPUT_HANDLE);
  LNumberOfEvents:=0;
  //get the number of events
  GetNumberOfConsoleInputEvents(LStdHandle,LNumberOfEvents);
  if LNumberOfEvents<> 0 then
  begin
    //retrieve the event
    PeekConsoleInput(LStdHandle,LBuffer,1,LNumberOfEventsRead);
    if LNumberOfEventsRead <> 0 then
    begin
      if LBuffer.EventType = KEY_EVENT then //is a Keyboard event?
      begin
        if LBuffer.Event.KeyEvent.bKeyDown then //the key was pressed?
          Result:=true
        else
          FlushConsoleInputBuffer(LStdHandle); //flush the buffer
      end
      else
      FlushConsoleInputBuffer(LStdHandle);//flush the buffer
    end;
  end;
end;

class procedure obConsole.ClearKeyStates();
begin
  FillChar(FKeyState, SizeOf(FKeyState), 0);
  ClearKeyboardBuffer();
end;

class procedure obConsole.ClearKeyboardBuffer();
var
  LInputRecord: TInputRecord;
  LEventsRead: DWORD;
  LMsg: TMsg;
begin
  while PeekConsoleInput(GetStdHandle(STD_INPUT_HANDLE), LInputRecord, 1, LEventsRead) and (LEventsRead > 0) do
  begin
    ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), LInputRecord, 1, LEventsRead);
  end;

  while PeekMessage(LMsg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do
  begin
    // No operation; just removing messages from the queue
  end;
end;

class function  obConsole.IsKeyPressed(AKey: Byte): Boolean;
begin
  Result := (GetAsyncKeyState(AKey) and $8000) <> 0;
end;

class function  obConsole.WasKeyReleased(AKey: Byte): Boolean;
begin
  Result := False;
  if IsKeyPressed(AKey) and (not FKeyState[1, AKey]) then
  begin
    FKeyState[1, AKey] := True;
    Result := True;
  end
  else if (not IsKeyPressed(AKey)) and (FKeyState[1, AKey]) then
  begin
    FKeyState[1, AKey] := False;
    Result := False;
  end;
end;

class function  obConsole.WasKeyPressed(AKey: Byte): Boolean;
begin
  Result := False;
  if IsKeyPressed(AKey) and (not FKeyState[1, AKey]) then
  begin
    FKeyState[1, AKey] := True;
    Result := False;
  end
  else if (not IsKeyPressed(AKey)) and (FKeyState[1, AKey]) then
  begin
    FKeyState[1, AKey] := False;
    Result := True;
  end;
end;

class function  obConsole.ReadKey(): WideChar;
var
  LInputRecord: TInputRecord;
  LEventsRead: DWORD;
begin
  repeat
    ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), LInputRecord, 1, LEventsRead);
  until (LInputRecord.EventType = KEY_EVENT) and LInputRecord.Event.KeyEvent.bKeyDown;
  Result := LInputRecord.Event.KeyEvent.UnicodeChar;
end;

class function  obConsole.ReadLnX(const AAllowedChars: TobCharSet; AMaxLength: Integer; const AColor: string): string;
var
  LInputChar: Char;
begin
  Result := '';

  repeat
    LInputChar := ReadKey;

    if CharInSet(LInputChar, AAllowedChars) then
    begin
      if Length(Result) < AMaxLength then
      begin
        if not CharInSet(LInputChar, [#10, #0, #13, #8])  then
        begin
          //Print(LInputChar, AColor);
          Print('%s%s', [AColor, LInputChar]);
          Result := Result + LInputChar;
        end;
      end;
    end;
    if LInputChar = #8 then
    begin
      if Length(Result) > 0 then
      begin
        //Print(#8 + ' ' + #8);
        Print(#8 + ' ' + #8, []);
        Delete(Result, Length(Result), 1);
      end;
    end;
  until (LInputChar = #13);

  PrintLn();
end;

class procedure obConsole.Pause(const AForcePause: Boolean; AColor: string; const AMsg: string);
var
  LDoPause: Boolean;
begin
  if not HasOutput then Exit;

  ClearKeyStates();
  ClearKeyboardBuffer();

  if not AForcePause then
  begin
    LDoPause := True;
    if WasRunFrom() then LDoPause := False;
    if obUtils.IsStartedFromDelphiIDE() then LDoPause := True;
    if not LDoPause then Exit;
  end;

  WriteLn;
  if AMsg = '' then
    Print('%sPress any key to continue... ', [aColor])
  else
    Print('%s%s', [aColor, AMsg]);

  WaitForAnyKey();
  WriteLn;
end;

class function  obConsole.WrapTextEx(const ALine: string; AMaxCol: Integer; const ABreakChars: TobCharSet): string;
var
  LText: string;
  LPos: integer;
  LChar: Char;
  LLen: Integer;
  I: Integer;
begin
  LText := ALine.Trim;

  LPos := 0;
  LLen := 0;

  while LPos < LText.Length do
  begin
    Inc(LPos);

    LChar := LText[LPos];

    if LChar = #10 then
    begin
      LLen := 0;
      continue;
    end;

    Inc(LLen);

    if LLen >= AMaxCol then
    begin
      for I := LPos downto 1 do
      begin
        LChar := LText[I];

        if CharInSet(LChar, ABreakChars) then
        begin
          LText.Insert(I, #10);
          Break;
        end;
      end;

      LLen := 0;
    end;
  end;

  Result := LText;
end;

class procedure obConsole.Teletype(const AText: string; const AColor: string; const AMargin: Integer; const AMinDelay: Integer; const AMaxDelay: Integer; const ABreakKey: Byte);
var
  LText: string;
  LMaxCol: Integer;
  LChar: Char;
  LWidth: Integer;
begin
  GetSize(@LWidth, nil);
  LMaxCol := LWidth - AMargin;

  LText := WrapTextEx(AText, LMaxCol);

  for LChar in LText do
  begin
    obUtils.ProcessMessages();
    Print('%s%s', [AColor, LChar]);
    if not obUtils.RandomBool() then
      FTeletypeDelay := obUtils.RandomRange(AMinDelay, AMaxDelay);
    obUtils.Wait(FTeletypeDelay);
    if IsKeyPressed(ABreakKey) then
    begin
      ClearKeyboardBuffer;
      Break;
    end;
  end;
end;

{ TobObject }
constructor TobObject.Create();
begin
  inherited;
end;

destructor TobObject.Destroy();
begin
  inherited;
end;

procedure TobObject.SetError(const AText: string; const AArgs: array of const);
begin
  FError := Format(AText, AArgs);
end;

function  TobObject.GetError(): string;
begin
  Result := FError;
end;

{ TobWavPlayer }
procedure TobWavPlayer.SetVolume(const AValue: Single);
var
  LValue: Single;
begin
  LValue := AValue;
  if LValue < 0 then LValue := 0
  else if LValue > 1 then LValue := 1;
  FVolume := LValue;
  if Assigned(FSecondaryBuffer) then
    FSecondaryBuffer.SetVolume(Round((1 - LValue) * -10000));
end;

constructor TobWavPlayer.Create();
begin
  inherited;
end;

destructor TobWavPlayer.Destroy;
begin
  Close();
  inherited;
end;

function TobWavPlayer.Open(const AHandle: HWND): Boolean;
var
  BufferDesc: DSBUFFERDESC;
  WFX: TWaveFormatEx;
  LHandle: HWND;
begin
  Result := False;

  LHandle := AHandle;

  if Failed(DirectSoundCreate(nil, FDirectSound, nil)) then
  begin
    SetError('Failed to initialize DirectSound', []);
    Close();
    Exit;
  end;

  if LHandle = 0 then
    LHandle := GetConsoleWindow(); // Use console window if no handle is provided

  if Failed(FDirectSound.SetCooperativeLevel(LHandle, DSSCL_PRIORITY)) then
  begin
    SetError('Failed to set cooperative level', []);
    Exit;
  end;

  FillChar(BufferDesc, SizeOf(BufferDesc), 0);
  BufferDesc.dwSize := SizeOf(BufferDesc);
  BufferDesc.dwFlags := DSBCAPS_PRIMARYBUFFER;

  if Failed(FDirectSound.CreateSoundBuffer(BufferDesc, FPrimaryBuffer, nil)) then
  begin
    SetError('Failed to create primary sound buffer', []);
    Close();
    Exit;
  end;

  FillChar(WFX, SizeOf(WFX), 0);
  WFX.wFormatTag := WAVE_FORMAT_PCM;
  WFX.nChannels := 2;
  WFX.nSamplesPerSec := 44100;
  WFX.wBitsPerSample := 16;
  WFX.nBlockAlign := (WFX.nChannels * WFX.wBitsPerSample) div 8;
  WFX.nAvgBytesPerSec := WFX.nSamplesPerSec * WFX.nBlockAlign;

  FPrimaryBuffer.SetFormat(@WFX);

  Result := True;
end;

function TobWavPlayer.IsOpen(): Boolean;
begin
  Result := False;

  if not Assigned(FDirectSound) then Exit;

  Result := True;
end;

procedure TobWavPlayer.Close();
begin
  Stop();

  FSecondaryBuffer := nil;
  FPrimaryBuffer := nil;
  FDirectSound := nil;
end;

procedure TobWavPlayer.LoadFromFile(const AFileName: string);
begin
  if not IsOpen() then Exit;

  if not LoadWavFile(AFileName) then
  begin
    SetError('Failed to load WAV file', []);
    Exit;
  end;
end;

function TobWavPlayer.LoadWavFile(const AFileName: string): Boolean;
var
  BufferDesc: DSBUFFERDESC;
  WaveFormat: TWaveFormatEx;
  WaveData: Pointer;
  WaveSize: DWORD;
  DataPtr1, DataPtr2: Pointer;
  DataSize1, DataSize2: DWORD;
  HMMIO: THandle;
  MMCKInfo, MMCKInfoSub: TMMCKInfo;
begin
  Result := False;

  if not IsOpen() then Exit;

  HMMIO := mmioOpen(PChar(AFileName), nil, MMIO_READ or MMIO_ALLOCBUF);
  if HMMIO = 0 then Exit;

  MMCKInfo.fccType := mmioStringToFOURCC('WAVE', 0);
  if mmioDescend(HMMIO, @MMCKInfo, nil, MMIO_FINDRIFF) <> MMSYSERR_NOERROR then Exit;

  MMCKInfoSub.ckid := mmioStringToFOURCC('fmt ', 0);
  if mmioDescend(HMMIO, @MMCKInfoSub, @MMCKInfo, MMIO_FINDCHUNK) <> MMSYSERR_NOERROR then Exit;

  if mmioRead(HMMIO, @WaveFormat, SizeOf(WaveFormat)) <> SizeOf(WaveFormat) then Exit;
  mmioAscend(HMMIO, @MMCKInfoSub, 0);

  MMCKInfoSub.ckid := mmioStringToFOURCC('data', 0);
  if mmioDescend(HMMIO, @MMCKInfoSub, @MMCKInfo, MMIO_FINDCHUNK) <> MMSYSERR_NOERROR then Exit;

  WaveSize := MMCKInfoSub.cksize;
  GetMem(WaveData, WaveSize);
  if mmioRead(HMMIO, WaveData, WaveSize) <> Integer(WaveSize) then Exit;
  mmioClose(HMMIO, 0);

  FillChar(BufferDesc, SizeOf(BufferDesc), 0);
  BufferDesc.dwSize := SizeOf(BufferDesc);
  BufferDesc.dwFlags := DSBCAPS_STATIC or DSBCAPS_CTRLVOLUME;
  BufferDesc.dwBufferBytes := WaveSize;
  BufferDesc.lpwfxFormat := @WaveFormat;

  if Failed(FDirectSound.CreateSoundBuffer(BufferDesc, FSecondaryBuffer, nil)) then Exit;
  if Failed(FSecondaryBuffer.Lock(0, WaveSize, @DataPtr1, @DataSize1, @DataPtr2, @DataSize2, 0)) then Exit;
  Move(WaveData^, DataPtr1^, DataSize1);
  if DataPtr2 <> nil then Move((PByte(WaveData) + DataSize1)^, DataPtr2^, DataSize2);
  FSecondaryBuffer.Unlock(DataPtr1, DataSize1, DataPtr2, DataSize2);

  FreeMem(WaveData);
  Result := True;
end;

procedure TobWavPlayer.Play();
begin
  if Assigned(FSecondaryBuffer) then
    FSecondaryBuffer.Play(0, 0, 0);
end;

procedure TobWavPlayer.Stop();
begin
  if Assigned(FSecondaryBuffer) then
    FSecondaryBuffer.Stop;
end;

{ TobMarkdownStreamFilter }
procedure TobMarkdownStreamFilter.Clear();
begin
  FInTag := False;
  FTagBuffer := '';
  FOutputBuffer := '';
end;

procedure TobMarkdownStreamFilter.SetWhitelist(const Tags: array of string);
var
  I: Integer;
begin
  SetLength(FWhitelist, Length(Tags));
  for I := 0 to High(Tags) do
    FWhitelist[I] := LowerCase(Tags[I]);
end;

function TobMarkdownStreamFilter.IsWhitelisted(const TagName: string): Boolean;
begin
  Result := MatchText(TagName, FWhitelist);
end;

function TobMarkdownStreamFilter.ExtractTagName(const Tag: string; out IsClosing: Boolean): string;
var
  Temp: string;
begin
  Temp := Tag.Trim(['<', '>', ' ']);
  IsClosing := Temp.StartsWith('/');
  if IsClosing then
    Temp := Temp.Substring(1);
  Result := Temp.Split([' ', '>'])[0].ToLower;
end;

function TobMarkdownStreamFilter.ConvertToMarkdown(const TagName: string; const IsClosing: Boolean): string;
begin
  // Only convert known tags
  if TagName = 'b' then
    Exit('**')
  else if TagName = 'strong' then
    Exit('**')
  else if TagName = 'i' then
    Exit('_')
  else if TagName = 'em' then
    Exit('_')
  else if (TagName = 'br') then
    Exit(sLineBreak)
  else
    Exit(''); // remove unknown tags unless whitelisted
end;

function TobMarkdownStreamFilter.ProcessChunk(const Chunk: string): string;
var
  I: Integer;
  C: Char;
  TagName: string;
  IsClosing: Boolean;
  Markdown: string;
begin
  for I := 1 to Length(Chunk) do
  begin
    C := Chunk[I];

    if FInTag then
    begin
      FTagBuffer := FTagBuffer + C;
      if C = '>' then
      begin
        // Tag is complete
        TagName := ExtractTagName(FTagBuffer, IsClosing);

        if IsWhitelisted(TagName) then
          FOutputBuffer := FOutputBuffer + '<' + FTagBuffer
        else
        begin
          Markdown := ConvertToMarkdown(TagName, IsClosing);
          FOutputBuffer := FOutputBuffer + Markdown;
        end;

        FTagBuffer := '';
        FInTag := False;
      end;
    end
    else if C = '<' then
    begin
      FInTag := True;
      FTagBuffer := '';
    end
    else
    begin
      FOutputBuffer := FOutputBuffer + C;
    end;
  end;

  Result := FOutputBuffer;
  FOutputBuffer := '';
end;

{ TobToolStreamProcessor }
constructor TobToolStreamProcessor.Create();
begin
  inherited;

  Clear();
end;

destructor TobToolStreamProcessor.Destroy();
begin
  inherited;
end;

function TobToolStreamProcessor.IsValidToolJson(const AJsonText: string; out AToolName: string; out AParameters: TJSONObject): Boolean;
var
  LJsonObj: TJSONObject;
  LParamValue: TJSONValue;
  LJsonText: string;
begin
  Result := False;
  AToolName := '';
  AParameters := nil;

  LJsonText := AJsonText;

  //{“tool”: “web_search”, “parameters”: {“query”: “elon musk net worth today”}}
  LJsonText := LJsonText.Replace('“', '"').Replace('”', '"');

  try
    LJsonObj := TJSONObject.ParseJSONValue(LJsonText) as TJSONObject;
    if LJsonObj = nil then
      Exit;

    try
      // Check if it has the tool format
      if not LJsonObj.TryGetValue('tool', AToolName) then
        Exit;

      LParamValue := LJsonObj.GetValue('parameters');
      if not (LParamValue is TJSONObject) then
        Exit;

      // Clone the parameters to avoid memory issues
      AParameters := TJSONObject(LParamValue.Clone) as TJSONObject;
      Result := True;
    finally
      // Always free the original JSON object
      LJsonObj.Free;
    end;
  except
    Result := False;
    // Make sure we clean up if anything fails
    AParameters.Free;
    AParameters := nil;
  end;
end;

procedure TobToolStreamProcessor.Clear();
begin
  FBuffer := '';
  FInJsonDetection := False;
  FOpenBraces := 0;
  FResponse := '';
  FInMarkdownCodeBlock := False;
  FPotentialCodeBlockStart := False;
  SetError('', []);
end;

procedure TobToolStreamProcessor.ProcessToken(const AToken: string);
var
  I: Integer;
  C: Char;
  LJsonText: string;
  LIsComplete: Boolean;
  LToolName: string;
  LParameters: TJSONObject;
begin
  // Handle possible ``` followed by "json"
  if FPotentialCodeBlockStart then
  begin
    FPotentialCodeBlockStart := False;

    if SameText(Trim(AToken), 'json') then
    begin
      FInMarkdownCodeBlock := True;
      Exit; // don't output "json"
    end
    else
    begin
      if Assigned(FOnTextOutput) then
        FOnTextOutput('```'); // output previously skipped ```
      // continue processing AToken as normal
    end;
  end;

  // Handle full ```json on the same token
  if SameText(Trim(AToken), '```json') then
  begin
    FInMarkdownCodeBlock := True;
    Exit;
  end;

  // Handle bare ```
  if SameText(Trim(AToken), '```') then
  begin
    if FInMarkdownCodeBlock then
    begin
      FInMarkdownCodeBlock := False;
    end
    else
    begin
      FPotentialCodeBlockStart := True;
    end;
    Exit;
  end;

  // If in markdown block, suppress output but still do tool detection
  if FInMarkdownCodeBlock then
  begin
    // Start JSON detection if needed
    if (Pos('{', AToken) > 0) and not FInJsonDetection then
    begin
      FInJsonDetection := True;
      FBuffer := '';
      FOpenBraces := 0;
    end;

    if FInJsonDetection then
    begin
      FBuffer := FBuffer + AToken;

      for I := 1 to Length(AToken) do
      begin
        C := AToken[I];
        if C = '{' then Inc(FOpenBraces)
        else if C = '}' then Dec(FOpenBraces);
      end;

      if (FOpenBraces = 0) and (Pos('{', FBuffer) > 0) then
      begin
        LJsonText := FBuffer;
        LParameters := nil;
        LIsComplete := IsValidToolJson(LJsonText, LToolName, LParameters);

        if LIsComplete then
        begin
          try
            if Assigned(FOnToolDetected) then
              FOnToolDetected(LToolName, LParameters);
          finally
            LParameters.Free;
          end;
        end;

        FBuffer := '';
        FInJsonDetection := False;
      end
      else if Length(FBuffer) > 1000 then
      begin
        FBuffer := '';
        FInJsonDetection := False;
      end;
    end;

    Exit; // skip output
  end;

  // === Normal JSON detection ===
  if (Pos('{', AToken) > 0) and not FInJsonDetection then
  begin
    FInJsonDetection := True;
    FBuffer := '';
    FOpenBraces := 0;
  end;

  if FInJsonDetection then
  begin
    FBuffer := FBuffer + AToken;

    for I := 1 to Length(AToken) do
    begin
      C := AToken[I];
      if C = '{' then Inc(FOpenBraces)
      else if C = '}' then Dec(FOpenBraces);
    end;

    if (FOpenBraces = 0) and (Pos('{', FBuffer) > 0) then
    begin
      LJsonText := FBuffer;
      LParameters := nil;
      LIsComplete := IsValidToolJson(LJsonText, LToolName, LParameters);

      if LIsComplete then
      begin
        try
          if Assigned(FOnToolDetected) then
            FOnToolDetected(LToolName, LParameters);
        finally
          LParameters.Free;
        end;
      end
      else
      begin
        if Assigned(FOnTextOutput) then
          FOnTextOutput(FBuffer);
      end;

      FBuffer := '';
      FInJsonDetection := False;
      Exit;
    end;

    if Length(FBuffer) > 1000 then
    begin
      if Assigned(FOnTextOutput) then
        FOnTextOutput(FBuffer);
      FBuffer := '';
      FInJsonDetection := False;
    end;

    Exit;
  end;

  // Normal token output
  if Assigned(FOnTextOutput) then
    FOnTextOutput(AToken);
end;

{ TobConfigFile }
constructor TobConfigFile.Create();
begin
  inherited;
  FHandle := nil;
  FSection := TStringList.Create();
end;

destructor TobConfigFile.Destroy();
begin
  Close;
  FSection.Free();
  inherited;
end;

function  TobConfigFile.Open(const AFilename: string=''): Boolean;
var
  LFilename: string;
begin
  Close;
  LFilename := AFilename;
  if LFilename.IsEmpty then LFilename := TPath.ChangeExtension(ParamStr(0), 'ini');
  FHandle := TIniFile.Create(LFilename);
  Result := Boolean(FHandle <> nil);
  FFilename := LFilename;
end;

procedure TobConfigFile.Close();
begin
  if not Opened then Exit;
  FHandle.UpdateFile;
  FreeAndNil(FHandle);
end;

function  TobConfigFile.Opened(): Boolean;
begin
  Result := Boolean(FHandle <> nil);
end;

procedure TobConfigFile.Update();
begin
  if not Opened then Exit;
  FHandle.UpdateFile;
end;

function  TobConfigFile.RemoveSection(const AName: string): Boolean;
var
  LName: string;
begin
  Result := False;
  if not Opened then Exit;
  LName := AName;
  if LName.IsEmpty then Exit;
  FHandle.EraseSection(LName);
  Result := True;
end;

procedure TobConfigFile.SetValue(const ASection, AKey, AValue: string);
begin
  if not Opened then Exit;
  FHandle.WriteString(ASection, AKey, AValue);
end;

procedure TobConfigFile.SetValue(const ASection, AKey: string; AValue: Integer);
begin
  if not Opened then Exit;
  SetValue(ASection, AKey, AValue.ToString);
end;

procedure TobConfigFile.SetValue(const ASection, AKey: string; AValue: Boolean);
begin
  if not Opened then Exit;
  SetValue(ASection, AKey, AValue.ToInteger);
end;

procedure TobConfigFile.SetValue(const ASection, AKey: string; AValue: Pointer; AValueSize: Cardinal);
var
  LValue: TMemoryStream;
begin
  if not Opened then Exit;
  if AValue = nil then Exit;
  LValue := TMemoryStream.Create;
  try
    LValue.Position := 0;
    LValue.Write(AValue^, AValueSize);
    LValue.Position := 0;
    FHandle.WriteBinaryStream(ASection, AKey, LValue);
  finally
    FreeAndNil(LValue);
  end;
end;

function  TobConfigFile.GetValue(const ASection, AKey, ADefaultValue: string): string;
begin
  Result := '';
  if not Opened then Exit;
  Result := FHandle.ReadString(ASection, AKey, ADefaultValue);
end;

function  TobConfigFile.GetValue(const ASection, AKey: string; ADefaultValue: Integer): Integer;
var
  LResult: string;
begin
  Result := ADefaultValue;
  if not Opened then Exit;
  LResult := GetValue(ASection, AKey, ADefaultValue.ToString);
  Integer.TryParse(LResult, Result);
end;

function  TobConfigFile.GetValue(const ASection, AKey: string; ADefaultValue: Boolean): Boolean;
begin
  Result := ADefaultValue;
  if not Opened then Exit;
  Result := GetValue(ASection, AKey, ADefaultValue.ToInteger).ToBoolean;
end;

procedure TobConfigFile.GetValue(const ASection, AKey: string; AValue: Pointer; AValueSize: Cardinal);
var
  LValue: TMemoryStream;
  LSize: Cardinal;
begin
  if not Opened then Exit;
  if not Assigned(AValue) then Exit;
  if AValueSize = 0 then Exit;
  LValue := TMemoryStream.Create;
  try
    LValue.Position := 0;
    FHandle.ReadBinaryStream(ASection, AKey, LValue);
    LSize := AValueSize;
    if AValueSize > LValue.Size then
      LSize := LValue.Size;
    LValue.Position := 0;
    LValue.Write(AValue^, LSize);
  finally
    FreeAndNil(LValue);
  end;
end;

function  TobConfigFile.RemoveKey(const ASection, AKey: string): Boolean;
var
  LSection: string;
  LKey: string;
begin
  Result := False;
  if not Opened then Exit;
  LSection := ASection;
  LKey := AKey;
  if LSection.IsEmpty then Exit;
  if LKey.IsEmpty then Exit;
  FHandle.DeleteKey(LSection, LKey);
  Result := True;
end;

function  TobConfigFile.GetSectionValues(const ASection: string): Integer;
var
  LSection: string;
begin
  Result := 0;
  if not Opened then Exit;
  LSection := ASection;
  if LSection.IsEmpty then Exit;
  FSection.Clear;
  FHandle.ReadSectionValues(LSection, FSection);
  Result := FSection.Count;
end;

function  TobConfigFile.GetSectionValue(const AIndex: Integer; const ADefaultValue: string): string;
begin
  Result := '';
  if not Opened then Exit;
  if (AIndex < 0) or (AIndex > FSection.Count - 1) then Exit;
  Result := FSection.ValueFromIndex[AIndex];
  if Result = '' then Result := ADefaultValue;
end;

function  TobConfigFile.GetSectionValue(const AIndex, ADefaultValue: Integer): Integer;
begin
  Result := ADefaultValue;
  if not Opened then Exit;
  Result := string(GetSectionValue(AIndex, ADefaultValue.ToString)).ToInteger;
end;

function  TobConfigFile.GetSectionValue(const AIndex: Integer; const ADefaultValue: Boolean): Boolean;
begin
  Result := ADefaultValue;
  if not Opened then Exit;
  Result := string(GetSectionValue(AIndex, ADefaultValue.ToString)).ToBoolean
end;

{ =========================================================================== }

initialization
begin
  ReportMemoryLeaksOnShutdown := True;
  SetExceptionMask(GetExceptionMask + [exOverflow, exInvalidOp]);
  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);
  obUtils.EnableVirtualTerminalProcessing();
  Randomize();
end;

finalization
begin
end;

end.
