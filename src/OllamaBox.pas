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

unit OllamaBox;

{$I OllamaBox.Defines.inc}

interface

uses
  WinApi.Windows,
  Winapi.PsAPI,
  System.Generics.Collections,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Math,
  System.NetEncoding,
  System.Net.HttpClient,
  System.JSON,
  System.Hash,
  EasyJson,
  OllamaBox.Utils;

const

  /// <summary>
  ///   The opening tag used to wrap model output during the "thinking" phase.
  /// </summary>
  CobThinkOpenTag = '<think>';

  /// <summary>
  ///   The closing tag used to terminate the "thinking" output block.
  /// </summary>
  CobThinkCloseTag = '</think>';

  /// <summary>
  ///   The opening tag used to wrap model output during the "responding" phase.
  /// </summary>
  CobResponseOpenTag = '<response>';

  /// <summary>
  ///   The closing tag used to terminate the "responding" output block.
  /// </summary>
  CobResponseCloseTag = '</response>';

type

  /// <summary>
  ///   Represents the current status of a process or operation, such as model pulling or inference.
  /// </summary>
  /// <remarks>
  ///   Used primarily to indicate progress phases in callback notifications.
  /// </remarks>
  TobStatus = (obStart, obInProgress, obEnd);

  /// <summary>
  ///   A simple callback type for event notifications that require no parameters.
  /// </summary>
  /// <remarks>
  ///   Often used for signaling the start or end of logical operations such as thinking or responding.
  /// </remarks>
  TobCallback = reference to procedure();

  /// <summary>
  ///   A callback type used to determine whether an ongoing operation should be cancelled.
  /// </summary>
  /// <returns>
  ///   <c>True</c> to request cancellation; otherwise, <c>False</c> to continue.
  /// </returns>
  /// <remarks>
  ///   This is evaluated periodically during inference or long-running tasks to allow user interruption.
  /// </remarks>
  TobCancelCallback = reference to function(): Boolean;

  /// <summary>
  ///   A callback type invoked each time a new token is generated during model inference.
  /// </summary>
  /// <param name="AToken">
  ///   The token string produced by the model.
  /// </param>
  /// <remarks>
  ///   Useful for streaming responses to the console, UI, or log in real-time.
  /// </remarks>
  TobNextTokenCallback = reference to procedure(const AToken: string);

  /// <summary>
  ///   A callback type invoked during the process of pulling a model from the Ollama registry.
  /// </summary>
  /// <param name="AMessage">
  ///   A status message describing the current step or phase.
  /// </param>
  /// <param name="APercent">
  ///   A float value (0.0 to 100.0) indicating overall progress.
  /// </param>
  /// <param name="AStatus">
  ///   The current status of the operation (start, in-progress, or end).
  /// </param>
  /// <remarks>
  ///   This callback provides real-time updates to the caller during model download and setup.
  /// </remarks>
  TobPullModelCallback = reference to procedure(
    const AMessage: string;
    const APercent: Double;
    const AStatus: TobStatus
  );

  /// <summary>
  ///   TOllamaBox is a high-level Delphi wrapper for managing local Ollama model execution,
  ///   including downloading, hosting, inference, tool streaming, and callback integration.
  /// </summary>
  /// <remarks>
  ///   This class encapsulates the full lifecycle of an embedded Ollama runtime instance:
  ///   - Pulling models from the Ollama registry
  ///   - Running the Ollama server in-process
  ///   - Handling prompts, responses, and context tokens
  ///   - Supporting image inputs and system prompts
  ///   - Providing hooks for cancellation, streaming, and event feedback<br/><br/>
  ///   It extends <see cref="TobObject"/>, and acts as a complete container for text and multimodal inference.
  /// </remarks>
  TOllamaBox = class(TobObject)
  protected type
    HttpOperation = (obHttpGet, obHttpPost);
    TImage = record
      Binary: string;
      MimeType: string;
    end;
    TImages = TList<TImage>;
  protected
    FProcessHandle: THandle;
    FProcessID: DWORD;
    FServerPort: UInt32;
    FModelPath: string;
    FServerPath: string;
    FServerDownloadPath: string;
    FImages: TImages;
    FResponseStreamPos: Int32;
    FResponseStream: TStringStream;
    FModel: string;
    FPrompt: string;
    FKeepAlive: Int32;
    FSuffix: string;
    FSystem: TStringList;
    FContext: TEasyJson;
    FResponse: string;
    FPartialResponse: string;
    FInThink: Boolean;
    FShowThinking: Boolean;
    FInResponse: Boolean;
    FShowResponding: Boolean;
    FInputTokens: Uint32;
    FOutputTokens: Uint32;
    FTotalTokens: UInt32;
    FSpeed: double;
    FTime: double;
    FTemperature: Single;
    FMaxContext: UInt32;
    FGPULayers: Int32;
    FMainGPU: Int32;
    FSeed: Int32;
    FThreads: Int32;
    FTokenResponse: TobTokenResponse;
    FWasCancelled: Boolean;
    FHttpStatusCode: Int32;
    FHttpStatusText: string;
    FOnCancel: TobCancelCallback;
    FOnNextToken: TobNextTokenCallback;
    FOnThinkStart: TobCallback;
    FOnThinkEnd: TobCallback;
    FOnResponseStart: TobCallback;
    FOnResponseEnd: TobCallback;
    FOnPullModel: TobPullModelCallback;
    FTool: TobToolStreamProcessor;
    FFilter: TobMarkdownStreamFilter;
    FConfigFile: TobConfigFile;
    FTavilyApiKey: string;
    FLemonFoxApiKey: string;
    FOllamVersion: string;
    FPrompts: TobPromptDatabase;
    function  CheckLatestOllamaVersion(out AVersionTag, ADownloadURL: string): Boolean;
    function  BuildServerCmdLine: string;
    function  GetUrl(const ARoute: string): string;
    function  GetContext(): TArray<Integer>;
    procedure SetContext(const AJsonArray: string);
    procedure SetTemperature(const AValue: Single);
    procedure SetKeepAlive(const AValue: Int32);
    procedure SetGPULayers(const AValue: Int32);
    procedure SetMainGPU(const AValue: Int32);
    procedure SetThreads(const AValue: Int32);
    function  GetSystem(): string;
    function  Http(const AURL: string; const AOperation: TOllamaBox.HttpOperation; const ABody: string; out AResponse: string): Boolean;
    procedure DoGetOllamaVersion();
    procedure DoNextToken(const AToken: string);
    function  DoCancel(): Boolean;
    procedure DoPull(const AMessage: string; const APercent: Double; const AStatus: TobStatus);
    procedure DoReceiveDataGenerate(const ASender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
    procedure DoReceiveDataPull(const ASender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
  public
    /// <summary>
    ///   Initializes a new instance of the class.
    ///   This constructor sets up any internal structures, default values,
    ///   or state required for the object to function correctly.
    ///   Override this method in descendant classes to add custom initialization logic.
    /// </summary>
    constructor Create(); override;

    /// <summary>
    ///   Destroys the current instance of the class and frees any resources
    ///   it has allocated during its lifetime.
    ///   Override this method in descendant classes to perform custom cleanup tasks.
    /// </summary>
    destructor Destroy(); override;

    /// <summary>
    ///   Returns the version of the TOllamaBox component using Semantic Versioning (SemVer).
    /// </summary>
    /// <returns>
    ///   A string in the SemVer format <c>MAJOR.MINOR.PATCH</c>, such as <c>"1.2.0"</c>.
    /// </returns>
    /// <remarks>
    ///   The version string follows the Semantic Versioning 2.0.0 specification:
    ///   <list type="bullet">
    ///     <item><b>MAJOR</b> – for incompatible API changes</item>
    ///     <item><b>MINOR</b> – for backward-compatible feature additions</item>
    ///     <item><b>PATCH</b> – for backward-compatible bug fixes</item>
    ///   </list>
    ///   This is the version of the TOllamaBox wrapper itself, not the Ollama runtime or model version.
    /// </remarks>
    function GetVersion(): string;

    /// <summary>
    ///   Returns the version of the Ollama runtime currently installed or running.
    /// </summary>
    /// <returns>
    ///   A string representing the Ollama version, typically in SemVer format (e.g., <c>"0.1.34"</c>).
    /// </returns>
    /// <remarks>
    ///   This method queries the embedded or locally hosted Ollama server to retrieve
    ///   the version of the runtime environment used for model inference and API handling.
    ///   It is useful for compatibility checks, diagnostics, and logging.
    /// </remarks>
    function GetOllamaVersion(): string;

    /// <summary>
    ///   Displays an ASCII art logo to the currently active console,
    ///   using the specified text color for visual emphasis.
    /// </summary>
    /// <param name="AColor">
    ///   A string representing a console color name as defined by the CIS (Common Interface Specification),
    ///   such as <c>'Black'</c>, <c>'Blue'</c>, <c>'Green'</c>, <c>'Cyan'</c>, <c>'Red'</c>, <c>'Magenta'</c>,
    ///   <c>'Brown'</c>, <c>'LightGray'</c>, <c>'DarkGray'</c>, <c>'LightBlue'</c>, <c>'LightGreen'</c>,
    ///   <c>'LightCyan'</c>, <c>'LightRed'</c>, <c>'LightMagenta'</c>, <c>'Yellow'</c>, or <c>'White'</c>.
    /// </param>
    /// <remarks>
    ///   The color specified must match one of the CIS-defined color names supported by the console system.
    ///   If the specified color is not recognized, the logo may be displayed using the default console color.
    ///   This method writes directly to the standard output stream.
    /// </remarks>
    procedure DisplayLogo(const AColor: string);

    /// <summary>
    ///   Downloads the latest version of the Ollama server from its official GitHub repository
    ///   and extracts the downloaded archive to the location specified by the <c>ServerPath</c> property.
    ///   The downloaded archive is stored temporarily in the <c>ServerDownloadPath</c> location.
    /// </summary>
    /// <remarks>
    ///   This method handles all steps required to fetch and unpack the Ollama server binary,
    ///   ensuring it is ready to be launched locally.
    ///   If the target directory already exists, it may be overwritten or updated as needed.
    /// </remarks>
    procedure DownloadServer();

    /// <summary>
    ///   Starts an instance of the Ollama server within the current process context.
    ///   The server is bound to the port specified by the <c>ServerPort</c> property.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the server was started successfully; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   This method is intended for embedding the Ollama server directly into the
    ///   hosting application's process. It does not spawn an external process or service.
    /// </remarks>
    function StartServer(): Boolean;

    /// <summary>
    ///   Checks whether the Ollama server is currently running within the context of the calling application.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the embedded Ollama server is active in the current process; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   This method verifies that the Ollama server instance was started using <see cref="StartServer"/>
    ///   and is still alive in-process. It does not check for external or system-wide Ollama instances.
    /// </remarks>
    function ServerStarted(): Boolean;

    /// <summary>
    ///   Stops the running instance of the Ollama server that was started within
    ///   the current application process.
    /// </summary>
    /// <remarks>
    ///   This method shuts down the server gracefully, releasing any resources
    ///   it may be using. If the server is not running, this method performs no action.
    /// </remarks>
    procedure StopServer();

    /// <summary>
    ///   Checks whether the Ollama server is currently running and accepting connections
    ///   at the address specified by <see cref="GetServerBaseAPIUrl"/>.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the Ollama server is active and responsive; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   This method confirms that the server process is running and that its HTTP API is reachable
    ///   and ready to serve requests. It may perform a health check or version query on the target URL
    ///   to validate that the server is not only started but fully initialized.
    /// </remarks>
    function ServerRunning(): Boolean;

    /// <summary>
    ///   Returns the base URL used to access the Ollama server's REST API,
    ///   typically including the protocol, host, and port number.
    /// </summary>
    /// <returns>
    ///   A string representing the base API URL, such as <c>http://localhost:11434</c>.
    /// </returns>
    /// <remarks>
    ///   This URL is constructed based on the value of the <c>ServerPort</c> property.
    ///   It is used as the root endpoint for all subsequent API requests to the server.
    /// </remarks>
    function GetServerBaseAPIUrl(): string;

    /// <summary>
    ///   Removes all system prompts from the current conversation context.
    /// </summary>
    /// <remarks>
    ///   Use this method to reset the conversation state by clearing previously added
    ///   system-level instructions that guide the behavior of the language model.
    /// </remarks>
    procedure ClearSystem();

    /// <summary>
    ///   Adds a new system prompt to the conversation context using a formatted string.
    /// </summary>
    /// <param name="AText">
    ///   A format string representing the system message, which may include placeholders for arguments.
    /// </param>
    /// <param name="AArgs">
    ///   An array of values to substitute into the format string <c>AText</c>.
    /// </param>
    /// <returns>
    ///   The zero-based index of the newly added system prompt.
    /// </returns>
    /// <remarks>
    ///   System prompts are special messages that define global instructions or behavior
    ///   for the language model (e.g., tone, restrictions, or roles).
    ///   These are typically injected before any user or assistant messages.
    /// </remarks>
    function AddSystem(const AText: string; const AArgs: array of const): UInt32;

    /// <summary>
    ///   Returns the number of system prompts currently added to the context.
    /// </summary>
    /// <returns>
    ///   A 32-bit unsigned integer representing the count of system messages.
    /// </returns>
    /// <remarks>
    ///   Each system prompt influences how the language model interprets and generates responses.
    /// </remarks>
    function SystemCount(): UInt32;

    /// <summary>
    ///   Removes a specific system prompt from the conversation context by index.
    /// </summary>
    /// <param name="AIndex">
    ///   The zero-based index of the system prompt to remove.
    /// </param>
    /// <remarks>
    ///   This allows fine-grained control over which system-level instructions remain active.
    ///   If the index is out of bounds, the behavior is implementation-defined.
    /// </remarks>
    procedure RemoveSystem(const AIndex: UInt32);

    /// <summary>
    ///   Clears the current conversation context, removing all stored messages,
    ///   including user, assistant, and intermediate model state.
    /// </summary>
    /// <remarks>
    ///   This method resets the internal vector or token buffer that maintains conversation history.
    ///   It is typically used to start a fresh session without prior influence.
    /// </remarks>
    procedure ClearContext();

    /// <summary>
    ///   Saves the current conversation context to a file for later restoration.
    /// </summary>
    /// <param name="AFilename">
    ///   The full path of the file where the context should be saved.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the context was successfully saved; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   The saved context includes all internal vectorized data, such as tokens or embeddings,
    ///   and can be restored using <see cref="LoadContext"/>.
    ///   The file format is implementation-defined but must be compatible with the inference backend.
    /// </remarks>
    function SaveContext(const AFilename: string): Boolean;

    /// <summary>
    ///   Loads a previously saved conversation context from a file into the current session.
    /// </summary>
    /// <param name="AFilename">
    ///   The full path to the file containing the saved context.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the context was loaded successfully; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   This method restores the model’s internal state from a saved snapshot,
    ///   allowing inference to continue seamlessly from where it left off.
    ///   The file must match the format expected by the underlying model runtime.
    /// </remarks>
    function LoadContext(const AFilename: string): Boolean;

    /// <summary>
    ///   Clears the list of image inputs from the current context, removing all
    ///   previously added images intended for multimodal inference.
    /// </summary>
    /// <remarks>
    ///   This method resets the internal image array used when submitting image-based prompts
    ///   to models that support visual inputs. Typically, the images are preprocessed and
    ///   encoded as Base64 strings suitable for the target model.
    /// </remarks>
    procedure ClearImages();

    /// <summary>
    ///   Adds an image file to the current input context for use with
    ///   multimodal models that accept image inputs alongside text.
    /// </summary>
    /// <param name="AFilename">
    ///   The full path to the image file to be loaded and converted.
    /// </param>
    /// <returns>
    ///   <c>True</c> if the image was successfully loaded and converted to Base64;
    ///   otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   The image is read from disk and encoded to Base64, typically in formats such as PNG or JPEG.
    ///   The resulting encoded image is added to the internal image array for submission to the model.
    ///   This is intended for use with multimodal LLMs like LLaVA, Gemma-Vision, or similar.
    /// </remarks>
    function AddImage(const AFilename: string): Boolean;

    /// <summary>
    ///   Pulls the specified model from the Ollama model database into the local cache.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the model was successfully downloaded and prepared; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   The model to be pulled is defined by the <c>Model</c> property.
    ///   This method contacts the Ollama registry and retrieves the corresponding model data
    ///   for use in local inference. Internet access is required.
    /// </remarks>
    function Pull(): Boolean;

    /// <summary>
    ///   Runs inference using the current model and prompt values.
    /// </summary>
    /// <returns>
    ///   <c>True</c> if the model generated a valid response; otherwise, <c>False</c>.
    /// </returns>
    /// <remarks>
    ///   This method uses the <c>Model</c> and <c>Prompt</c> properties to
    ///   perform a text generation task. If a response is produced,
    ///   it may be stored internally or returned by a separate accessor.
    /// </remarks>
    function Generate(): Boolean;

    /// <summary>
    ///   Performs a web search using Tavily.com and returns the top result(s)
    ///   as a single aggregated text string.
    /// </summary>
    /// <param name="AQuery">
    ///   The search query to send to Tavily's API.
    /// </param>
    /// <returns>
    ///   A plain text string containing the search results, typically composed
    ///   of summarized information or extracted relevant snippets.
    /// </returns>
    /// <remarks>
    ///   This function is designed to enable retrieval-augmented generation (RAG)
    ///   by augmenting the prompt with real-time web information.
    ///   It requires a valid Tavily API key and internet access.
    /// </remarks>
    function WebSearch(const AQuery: string): string;

    /// <summary>
    ///   The TCP port number on which the embedded Ollama server will listen for API requests.
    /// </summary>
    /// <remarks>
    ///   Default Ollama installations typically use port <c>11434</c>, but this property
    ///   allows customization to avoid conflicts or to support multiple instances.
    ///   It must be set before calling <see cref="StartServer"/>.
    /// </remarks>
    property ServerPort: UInt32 read FServerPort write FServerPort;

    /// <summary>
    ///   The full path to the local model file or directory to be used for inference.
    /// </summary>
    /// <remarks>
    ///   This path should point to a valid GGUF or other supported format, depending on the backend.
    ///   If using Ollama-managed models, this is resolved internally based on the pulled model name.
    /// </remarks>
    property ModelPath: string read FModelPath write FModelPath;

    /// <summary>
    ///   The destination folder where the Ollama server will be extracted and run.
    /// </summary>
    /// <remarks>
    ///   After calling <see cref="DownloadServer"/>, the Ollama executable and its dependencies
    ///   will be located at this path. The server will be executed from this location when started.
    /// </remarks>
    property ServerPath: string read FServerPath write FServerPath;

    /// <summary>
    ///   The temporary folder where the Ollama server ZIP file will be downloaded.
    /// </summary>
    /// <remarks>
    ///   This directory is used during the <see cref="DownloadServer"/> operation
    ///   to store the ZIP archive fetched from the Ollama GitHub repository before extraction.
    ///   The file may be deleted after successful installation.
    /// </remarks>
    property ServerDownloadPath: string read FServerDownloadPath write FServerDownloadPath;

    /// <summary>
    ///   The name or tag of the model to be used for inference and interaction.
    /// </summary>
    /// <remarks>
    ///   This value corresponds to a model identifier in the Ollama registry (e.g., <c>'llama3'</c>, <c>'gemma:7b'</c>).
    ///   It is used by methods such as <see cref="Pull"/> and <see cref="Generate"/> to resolve the appropriate model version.
    /// </remarks>
    property Model: string read FModel write FModel;

    /// <summary>
    ///   The user prompt or input text to be submitted to the model during inference.
    /// </summary>
    /// <remarks>
    ///   This value serves as the main instruction or question passed to the language model.
    ///   It is consumed by the <see cref="Generate"/> method to produce a completion or response.
    /// </remarks>
    property Prompt: string read FPrompt write FPrompt;

    /// <summary>
    ///   The number of seconds to keep the Ollama server process alive during idle periods.
    /// </summary>
    /// <remarks>
    ///   - A value of <c>0</c> disables keep-alive, and the server will shut down immediately after handling a request.<br/>
    ///   - A positive value (e.g., <c>60</c>) will keep the server running for that many seconds after the last request.<br/>
    ///   - A negative value (e.g., <c>-1</c>) instructs the runtime to keep the model loaded in VRAM indefinitely,
    ///     avoiding unloads and improving performance for repeated inference.<br/><br/>
    ///   Changes to this property are applied through the <see cref="SetKeepAlive"/> method.
    /// </remarks>
    property KeepAlive: Int32 read FKeepAlive write SetKeepAlive;

    /// <summary>
    ///   Specifies which GPU device to use for model inference acceleration.
    /// </summary>
    /// <remarks>
    ///   - A value of <c>-1</c> automatically selects the best available GPU device.<br/>
    ///   - A value of <c>0</c> or higher explicitly selects a specific GPU index (e.g., <c>0</c> for the first GPU).<br/>
    ///   This setting is useful in multi-GPU systems or for assigning workloads to dedicated devices.
    ///   Changing this property applies through <see cref="SetMainGPU"/>.
    /// </remarks>
    property MainGPU: Int32 read FMainGPU write SetMainGPU;

    /// <summary>
    ///   Sets the number of model layers to be offloaded to the GPU for acceleration.
    /// </summary>
    /// <remarks>
    ///   - A value of <c>-1</c> indicates that all available layers should be processed on the GPU.<br/>
    ///   - A value of <c>0</c> forces all computation to use the CPU (no GPU acceleration).<br/>
    ///   - A positive value <c>1..N</c> specifies the number of transformer layers to be executed on the GPU.<br/><br/>
    ///   This setting enables fine-grained control over GPU usage and VRAM consumption,
    ///   and is especially useful for optimizing inference performance on constrained hardware.
    /// </remarks>
    property GPULayers: Int32 read FGPULayers write SetGPULayers;

    /// <summary>
    ///   Sets the maximum context size, in tokens, to be used during inference.
    /// </summary>
    /// <remarks>
    ///   This value controls the number of tokens the model can consider for input and output combined.<br/>
    ///   The model size and <c>MaxContext</c> must both fit into available VRAM when GPU acceleration is used.<br/>
    ///   Increasing this value enables longer conversations or documents, but also increases memory usage.
    /// </remarks>
    property MaxContext: UInt32 read FMaxContext write FMaxContext;

    /// <summary>
    ///   Specifies the text to append immediately after the model’s generated response.
    /// </summary>
    /// <remarks>
    ///   This suffix is added only after the model finishes generating its output,
    ///   and is not considered part of the prompt or input context.<br/>
    ///   Common examples include newline characters, punctuation, or markers like <c>'</end>'</c>
    ///   to signal the end of a generated block.
    /// </remarks>
    property Suffix: string read FSuffix write FSuffix;

    /// <summary>
    ///   Controls the randomness of the model's output by adjusting the sampling temperature.
    /// </summary>
    /// <remarks>
    ///   - A temperature of <c>0.0</c> results in deterministic output (always the same response).<br/>
    ///   - Values between <c>0.2</c> and <c>1.0</c> introduce increasing levels of creativity and variability.<br/>
    ///   - Higher values may produce more diverse but less accurate or coherent results.<br/><br/>
    ///   Typical defaults range from <c>0.7</c> to <c>0.9</c>. Adjusting this value impacts
    ///   response diversity and is especially useful for generative or creative tasks.
    /// </remarks>
    property Temperature: Single read FTemperature write SetTemperature;

    /// <summary>
    ///   Sets the random seed used to initialize the generation process.
    /// </summary>
    /// <remarks>
    ///   - A value of <c>-1</c> or any negative number causes the engine to use a random seed each time.<br/>
    ///   - Any non-negative integer produces deterministic output for the same input and parameters.<br/><br/>
    ///   Use this property to enable reproducibility in generation results, useful in testing and evaluation.
    /// </remarks>
    property Seed: Int32 read FSeed write FSeed;

    /// <summary>
    ///   Specifies the number of CPU threads to use during model inference.
    /// </summary>
    /// <remarks>
    ///   - Setting this value to <c>0</c> or less will cause the runtime to automatically choose the optimal number of threads.<br/>
    ///   - A higher number of threads can improve performance on multi-core systems but may increase CPU usage.<br/><br/>
    ///   This setting is important for tuning local inference performance, especially in CPU-bound environments.
    /// </remarks>
    property Threads: Int32 read FThreads write SetThreads;

    /// <summary>
    ///   Gets the current internal token context used during model inference.
    /// </summary>
    /// <remarks>
    ///   This array contains the numeric token IDs that represent the conversation state,
    ///   including prior prompts, responses, and system messages.<br/>
    ///   The context is used as input to the model and can be saved, cleared, or restored using related methods.
    /// </remarks>
    property Context: TArray<Integer> read GetContext;

    /// <summary>
    ///   Contains the last generated response produced by the model after a call to <see cref="Generate"/>.
    /// </summary>
    /// <remarks>
    ///   This value is populated after a successful inference pass. It may be cleared when starting a new generation,
    ///   and is typically displayed to the user or consumed by other application logic.
    /// </remarks>
    property Response: string read FResponse;

    /// <summary>
    ///   Returns the combined system prompt content currently active in the session.
    /// </summary>
    /// <remarks>
    ///   This property aggregates all added system prompts into a single string
    ///   that can be inspected, displayed, or logged.<br/>
    ///   Use <see cref="AddSystem"/>, <see cref="ClearSystem"/>, and <see cref="SystemCount"/> to manage individual entries.
    /// </remarks>
    property System: string read GetSystem;

    /// <summary>
    ///   Gets the number of tokens used as input during the most recent inference request.
    /// </summary>
    /// <remarks>
    ///   This includes all tokens derived from the prompt, system messages, and any prefilled context.
    ///   Token count may vary depending on the tokenizer used and model configuration.
    /// </remarks>
    property InputTokens: UInt32 read FInputTokens;

    /// <summary>
    ///   Gets the number of tokens generated by the model as output in the most recent inference.
    /// </summary>
    /// <remarks>
    ///   This reflects the length of the generated response, measured in model tokens rather than characters.
    ///   Useful for tracking model usage and applying token-based limits.
    /// </remarks>
    property OutputTokens: UInt32 read FOutputTokens;

    /// <summary>
    ///   Gets the total number of tokens processed during the last generation cycle.
    /// </summary>
    /// <remarks>
    ///   This is the sum of <see cref="InputTokens"/> and <see cref="OutputTokens"/>.
    ///   It represents the total token usage for a single call to <see cref="Generate"/>.
    /// </remarks>
    property TotalTokens: UInt32 read FTotalTokens;

    /// <summary>
    ///   Gets the generation speed in tokens per second during the last inference.
    /// </summary>
    /// <remarks>
    ///   This metric is calculated based on the total number of output tokens
    ///   divided by the elapsed generation time. It provides a useful measure
    ///   for evaluating model performance and hardware efficiency.
    /// </remarks>
    property Speed: Double read FSpeed;

    /// <summary>
    ///   Gets the total elapsed time in seconds for the most recent inference operation.
    /// </summary>
    /// <remarks>
    ///   This includes the time taken for tokenization, evaluation, sampling,
    ///   and final output assembly. The value is measured in floating-point seconds
    ///   with sub-second precision.
    /// </remarks>
    property Time: Double read FTime;

    /// <summary>
    ///   Indicates whether the most recent inference operation was cancelled before completion.
    /// </summary>
    /// <remarks>
    ///   This flag is set to <c>True</c> if the generation process was interrupted
    ///   by a user action or internal cancellation request. It is automatically cleared
    ///   at the start of a new generation cycle.
    /// </remarks>
    property WasCancelled: Boolean read FWasCancelled;

    /// <summary>
    ///   Controls whether token output should be wrapped with &lt;think&gt;&lt;/think&gt; tags during generation.
    /// </summary>
    /// <remarks>
    ///   When <c>True</c>, the model's output will be enclosed between <c>&lt;think&gt;</c> and <c>&lt;/think&gt;</c> tags,
    ///   allowing consumers of the output to detect or style "thinking" content in real time.<br/>
    ///   This is useful in streaming UIs or terminals where visual cues help indicate that generation is in progress.
    /// </remarks>
    property ShowThinking: Boolean read FShowThinking write FShowThinking;

    /// <summary>
    ///   Indicates whether the model is currently in "thinking" mode — i.e., generating a response.
    /// </summary>
    /// <remarks>
    ///   Returns <c>True</c> when inference is active and tokens are being generated.<br/>
    ///   This is especially useful when paired with <see cref="ShowThinking"/> to drive conditional UI elements
    ///   or stream markers.
    /// </remarks>
    property Thinking: Boolean read FInThink;

    /// <summary>
    ///   Controls whether the token output should be wrapped with &lt;response&gt;&lt;/response&gt; tags during generation.
    /// </summary>
    /// <remarks>
    ///   When <c>True</c>, all generated tokens will be streamed within <c>&lt;response&gt;</c> and <c>&lt;/response&gt;</c> tags.
    ///   This is useful for downstream parsing, HTML rendering, or marking AI responses in terminal UIs and logs.
    ///   The tags are emitted at the start and end of the generation phase if enabled.
    /// </remarks>
    property ShowResponding: Boolean read FShowResponding write FShowResponding;

    /// <summary>
    ///   Indicates whether the model is currently in "responding" mode — that is, actively generating a response.
    /// </summary>
    /// <remarks>
    ///   Returns <c>True</c> during active response generation. This flag is typically set at the start of the
    ///   <see cref="Generate"/> operation and cleared once output is complete.
    ///   It can be used to drive conditional logic or UI state, especially when <see cref="ShowResponding"/> is enabled.
    /// </remarks>
    property Responding: Boolean read FInResponse;

    /// <summary>
    ///   Gets the HTTP status code returned from the most recent API request.
    /// </summary>
    /// <remarks>
    ///   This value reflects the raw HTTP response code (e.g., <c>200</c> for success, <c>404</c> for not found,
    ///   <c>500</c> for server error, etc.) returned by requests such as model pulling, web search, or inference.
    ///   It can be used to detect and handle failure cases programmatically.
    /// </remarks>
    property HttpStatusCode: Int32 read FHttpStatusCode;

    /// <summary>
    ///   Gets the HTTP status text or reason phrase returned with the last response.
    /// </summary>
    /// <remarks>
    ///   This is the textual message associated with the last HTTP status code (e.g., <c>"OK"</c>, <c>"Not Found"</c>, <c>"Internal Server Error"</c>).
    ///   It provides a human-readable explanation of the response and can be useful for logging or displaying errors to users.
    /// </remarks>
    property HttpStatusText: string read FHttpStatusText;

    /// <summary>
    ///   Event callback triggered during inference to check for cancellation requests.
    /// </summary>
    /// <remarks>
    ///   This event is periodically called during the generation process.
    ///   If the callback returns <c>True</c>, the inference will be aborted gracefully.
    ///   Useful for supporting user-interrupt or timeout mechanisms in long-running inference sessions.
    /// </remarks>
    property OnCancel: TobCancelCallback read FOnCancel write FOnCancel;

    /// <summary>
    ///   Event callback invoked each time a new token is generated by the model.
    /// </summary>
    /// <remarks>
    ///   This is used for streaming output or progressive display of responses.
    ///   The callback receives the token text as it becomes available, allowing integration
    ///   with live console display, UI updates, or logging.
    /// </remarks>
    property OnNextToken: TobNextTokenCallback read FOnNextToken write FOnNextToken;

    /// <summary>
    ///   Event triggered immediately before the model begins its "thinking" phase.
    /// </summary>
    /// <remarks>
    ///   This event is typically fired just before token generation starts, and is used to
    ///   update UI, logs, or trigger timing instrumentation.
    ///   It pairs with <see cref="OnThinkEnd"/> to bracket the duration of inference.
    /// </remarks>
    property OnThinkStart: TobCallback read FOnThinkStart write FOnThinkStart;

    /// <summary>
    ///   Event triggered immediately after the model completes its "thinking" phase.
    /// </summary>
    /// <remarks>
    ///   This event is called as soon as token generation finishes or is cancelled.
    ///   It is commonly used to finalize progress indicators or re-enable UI components.
    /// </remarks>
    property OnThinkEnd: TobCallback read FOnThinkEnd write FOnThinkEnd;

    /// <summary>
    ///   Event triggered immediately before the model begins emitting its response tokens.
    /// </summary>
    /// <remarks>
    ///   This event is fired at the start of the response streaming phase, after any thinking delay,
    ///   and before the first token is sent to the output stream or UI.
    ///   It is useful for triggering animations, UI locks, or tagging output sections.
    /// </remarks>
    property OnResponseStart: TobCallback read FOnResponseStart write FOnResponseStart;

    /// <summary>
    ///   Event triggered immediately after the model completes its response.
    /// </summary>
    /// <remarks>
    ///   This is fired after the last output token is generated or the operation is cancelled.
    ///   It can be used to finalize display output, hide streaming indicators, or trigger downstream logic.
    /// </remarks>
    property OnResponseEnd: TobCallback read FOnResponseEnd write FOnResponseEnd;

    /// <summary>
    ///   Event callback triggered when a model is being pulled from the Ollama model registry.
    /// </summary>
    /// <remarks>
    ///   This callback is invoked during the execution of <see cref="Pull"/>, typically to indicate
    ///   the start of a download, progress updates, or completion of a model fetch operation.<br/>
    ///   It can be used to display status messages, update progress bars, or log activity.
    /// </remarks>
    property OnPullModel: TobPullModelCallback read FOnPullModel write FOnPullModel;

    /// <summary>
    ///   Returns the current instance of the tool stream processor used to interpret tool calls in the model output.
    /// </summary>
    /// <remarks>
    ///   The stream processor is responsible for detecting, parsing, and dispatching tool-related blocks
    ///   from the model's output — such as structured JSON commands or function calls.
    ///   This instance can be accessed to customize tool behavior, inspect intermediate results,
    ///   or register new tool handlers.
    /// </remarks>
    property Tool: TobToolStreamProcessor read FTool;

    /// <summary>
    ///   Provides access to the internal prompt database used for storing and retrieving named prompt templates.
    /// </summary>
    /// <remarks>
    ///   The prompt database allows you to manage reusable prompt definitions, including system instructions,
    ///   user messages, or prefilled templates. These can be loaded, saved, or referenced by name when building
    ///   structured inference requests.
    ///   <para>
    ///   Useful for scenarios where consistent prompts are needed across sessions or dynamically injected
    ///   into the generation workflow.
    ///   </para>
    /// </remarks>
    property Prompts: TobPromptDatabase read FPrompts;

  end;

implementation

{ TOllamaBox }
function TOllamaBox.CheckLatestOllamaVersion(out AVersionTag, ADownloadURL: string): Boolean;
var
  Http: THTTPClient;
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  JsonText: string;
begin
  Result := False;
  AVersionTag := '';
  ADownloadURL := '';

  Http := THTTPClient.Create;
  try
    Response := Http.Get('https://api.github.com/repos/ollama/ollama/releases/latest');
    if Response.StatusCode <> 200 then
      Exit;

    JsonText := Response.ContentAsString(TEncoding.UTF8);
    JsonObj := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
    if not Assigned(JsonObj) then
      Exit;

    try
      if JsonObj.TryGetValue<string>('tag_name', AVersionTag) then
      begin
        ADownloadURL := Format('https://github.com/ollama/ollama/releases/download/%s/ollama-windows-amd64.zip', [AVersionTag]);
        Result := True;
      end;
    finally
      JsonObj.Free;
    end;
  finally
    Http.Free;
  end;
end;

function TOllamaBox.BuildServerCmdLine(): string;
var
  LOllamaExePath: string;
begin
  LOllamaExePath := TPath.Combine(FServerPath,  'ollama.exe');
  Result := Format('"%s" serve', [LOllamaExePath]);
end;

function TOllamaBox.GetUrl(const ARoute: string): string;
begin
  Result := Format('%s/api/%s', [GetServerBaseAPIUrl(), ARoute]);
end;

function TOllamaBox.GetContext(): TArray<Integer>;
begin
  Result := FContext.AsArray.GetValue<TArray<Integer>>;
end;

procedure TOllamaBox.SetContext(const AJsonArray: string);
begin
  FContext.Free();
  FContext := TEasyJson.Create(AJSonArray);
end;

procedure TOllamaBox.SetTemperature(const AValue: Single);
begin
  FTemperature := EnsureRange(AValue, 0, 1);
end;

procedure TOllamaBox.SetKeepAlive(const AValue: Int32);
begin
  FKeepAlive := EnsureRange(AValue, -1, MaxInt);
end;

procedure TOllamaBox.SetGPULayers(const AValue: Int32);
begin
  FGPULayers := EnsureRange(AValue, -1, MaxInt);
end;

procedure TOllamaBox.SetMainGPU(const AValue: Int32);
begin
  FMainGPU := EnsureRange(AValue, -1, MaxInt);
end;

procedure TOllamaBox.SetThreads(const AValue: Int32);
begin
  FThreads := EnsureRange(AValue, -1, MaxInt);
end;

function  TOllamaBox.GetSystem(): string;
begin
  Result := FSystem.Text;
end;

function  TOllamaBox.Http(const AURL: string; const AOperation: TOllamaBox.HttpOperation; const ABody: string; out AResponse: string): Boolean;
var
  LHTTPClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  Result := False;
  AResponse := '';

  try
    // Initialize HTTP client
    LHTTPClient := THTTPClient.Create;
    try
      // Execute request based on operation type
      case AOperation of
        obHttpGet:
          LResponse := LHTTPClient.Get(AURL);
        obHttpPost:
          LResponse := LHTTPClient.Post(AURL, TStringStream.Create(ABody, TEncoding.UTF8), nil);
      end;

      // Get response
      AResponse := LResponse.ContentAsString;
      if LResponse.StatusCode = 200 then
      begin
        Result := True;
      end;
    finally
      LHTTPClient.Free;
    end;
  except
    on E: Exception do
    begin
      AResponse := E.Message;
      Result := False;
    end;
  end;
end;

procedure TOllamaBox.DoGetOllamaVersion();
var
  LResponse: string;
  LJson: TEasyJson;
begin
  if Http(GetUrl('version'), obHttpGet, '', LResponse) then
  begin
    LJson := TEasyJson.Create(LResponse);
    try
      if LJson.HasPath('version') then
        FOllamVersion := LJson.Path['version'].AsString();
    finally
      LJson.Free();
    end;
  end;
end;

procedure TOllamaBox.DoNextToken(const AToken: string);
var
  LToken, LChunk: string;
  LPos: Integer;
  LFoundTag: Boolean;
begin
  LToken := AToken;
  LToken := obUtils.SanitizeFromJson(LToken);
  LToken := FFilter.ProcessChunk(LToken);
  FPartialResponse := FPartialResponse + LToken;

  repeat
    LFoundTag := False;

    // Handle <think>
    LPos := Pos(CobThinkOpenTag, FPartialResponse);
    if LPos > 0 then
    begin
      // Emit content before the tag (if allowed)
      if LPos > 1 then
      begin
        LChunk := Copy(FPartialResponse, 1, LPos - 1);
        FResponse := FResponse + LChunk;
        if FShowThinking then
        begin
          if Assigned(FOnNextToken) then
            FOnNextToken(LChunk)
          else
            obConsole.Print(LChunk);
        end;
      end;
      // Trigger <think> event
      if Assigned(FOnThinkStart) then FOnThinkStart();
      // Mark we're inside a <think> block
      FInThink := True;
      // Emit <think> tag without extra line breaks
      if FShowThinking then
      begin
        if Assigned(FOnNextToken) then
          FOnNextToken(CobThinkOpenTag+obCRLF)
        else
          obConsole.Print(CobThinkOpenTag+obCRLF);
      end;
      FResponse := FResponse + CobThinkOpenTag;
      Delete(FPartialResponse, 1, LPos + Length(CobThinkOpenTag) - 1);
      LFoundTag := True;
      Continue;
    end;

    // Handle </think>
    LPos := Pos(CobThinkCloseTag, FPartialResponse);
    if LPos > 0 then
    begin
      if LPos > 1 then
      begin
        LChunk := Copy(FPartialResponse, 1, LPos - 1);
        FResponse := FResponse + LChunk;
        if FShowThinking then
        begin
          if Assigned(FOnNextToken) then
            FOnNextToken(LChunk)
          else
            obConsole.Print(LChunk);
        end;
      end;
      // Trigger </think> event
      if Assigned(FOnThinkEnd) then FOnThinkEnd;
      if FShowThinking then
      begin
        if Assigned(FOnNextToken) then
          FOnNextToken(obCRLF+CobThinkCloseTag)
        else
          obConsole.Print(obCRLF+CobThinkCloseTag);
      end;
      FInThink := False;
      FResponse := FResponse + CobThinkCloseTag;
      Delete(FPartialResponse, 1, LPos + Length(CobThinkCloseTag) - 1);
      LFoundTag := True;
      Continue;
    end;

    // Handle <response>
    LPos := Pos(CobResponseOpenTag, FPartialResponse);
    if LPos > 0 then
    begin
      // Emit content before the tag
      if LPos > 1 then
      begin
        LChunk := Copy(FPartialResponse, 1, LPos - 1);
        FResponse := FResponse + LChunk;
        if (not FInThink) or FShowThinking then
        begin
          if Assigned(FOnNextToken) then
            FOnNextToken(LChunk)
          else
            obConsole.Print(LChunk);
        end;
      end;
      // Trigger <response> event
      if Assigned(FOnResponseStart) then FOnResponseStart();
      // Mark we're inside a <response> block
      FInResponse := True;
      // Add to response without extra line breaks
      FResponse := FResponse + CobResponseOpenTag;
      // Add visible tag if configured to show
      if FShowResponding then
      begin
        if Assigned(FOnNextToken) then
          FOnNextToken(CobResponseOpenTag+obCRLF)
        else
          obConsole.Print(CobResponseOpenTag+obCRLF);
      end;
      Delete(FPartialResponse, 1, LPos + Length(CobResponseOpenTag) - 1);
      LFoundTag := True;
      Continue;
    end;

    // Handle </response>
    LPos := Pos(CobResponseCloseTag, FPartialResponse);
    if LPos > 0 then
    begin
      if LPos > 1 then
      begin
        LChunk := Copy(FPartialResponse, 1, LPos - 1);
        FResponse := FResponse + LChunk;
        if (not FInThink) or FShowThinking then
        begin
          if Assigned(FOnNextToken) then
            FOnNextToken(LChunk)
          else
            obConsole.Print(LChunk);
        end;
      end;
      // Trigger </response> event
      if Assigned(FOnResponseEnd) then FOnResponseEnd;
      // Add to response without extra line breaks
      FResponse := FResponse + CobResponseCloseTag;
      // Add visible tag if configured to show
      if FShowResponding then
      begin
        if Assigned(FOnNextToken) then
          FOnNextToken(obCRLF+CobResponseCloseTag)
        else
          obConsole.Print(obCRLF+CobResponseCloseTag);
      end;
      FInResponse := False;
      Delete(FPartialResponse, 1, LPos + Length(CobResponseCloseTag) - 1);
      LFoundTag := True;
      Continue;
    end;

  until not LFoundTag;

  // Check for incomplete tags
  if (SameText('<thi', FPartialResponse)) or
     (SameText('</t', FPartialResponse)) or
     (SameText('<res', FPartialResponse)) or
     (SameText('</r', FPartialResponse)) then
    Exit;

  // Emit any normal content
  if FPartialResponse <> '' then
  begin
    FResponse := FResponse + FPartialResponse;
    // Only show outside-thinking text or if thinking display is enabled
    if (not FInThink) or FShowThinking then
    begin
      if Assigned(FOnNextToken) then
        FOnNextToken(FPartialResponse)
      else
        obConsole.Print(FPartialResponse);
    end;
    FPartialResponse := '';
  end;
end;

function  TOllamaBox.DoCancel(): Boolean;
begin
  if Assigned(FOnCancel) then
    Result := FOnCancel()
  else
    Result := obConsole.IsKeyPressed(obVK_ESC);

   FWasCancelled := Result;
end;

procedure TOllamaBox.DoPull(const AMessage: string; const APercent: Double; const AStatus: TobStatus);
var
  LMessage: string;
begin
  if Assigned(FOnPullModel) then
    FOnPullModel(AMessage, APercent, AStatus)
  else
    begin
      if not AMessage.IsEmpty then
      begin
        LMessage := Format(obCR+'%s %.2f%% completed...', [AMessage, APercent]);
        obConsole.Print(LMessage);
        obConsole.ClearToEndOfLine();
      end;

      if AStatus = obEnd then
        obConsole.ClearLine();
    end;
end;

procedure TOllamaBox.DoReceiveDataGenerate(const ASender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
var
  LData: string;
  LJson: TEasyJson;
  LTextBuffer: string;
  LLine: string;
  LRet: Integer;
  LTokenStr: string;
  LEvalDuration: UInt64;
begin
  AAbort := DoCancel();

  // get text respone stream text
  LTextBuffer := FResponseStream.DataString;

  if LTextBuffer.IsEmpty then Exit;

  // loop over text
  repeat
    AAbort := DoCancel();

    // get index of LF
    LRet := LTextBuffer.IndexOf(#10, FResponseStreamPos);

    // process line
    if LRet >= 0 then
    begin
      // get the line from buffer
      LLine := LTextBuffer.Substring(FResponseStreamPos, LRet - FResponseStreamPos);
      FResponseStreamPos := LRet + 1;

      // check for valid line
      if LLine.IsEmpty or
         (LLine.StartsWith(#10)) then
        continue;

      // sanitize line
      LData := LLine.Trim([' ', #13, #10]);

      // get json response
      LJson := TEasyJson.Create(LData);
      try
        if LJson.HasPath('response') then
        begin
          LTokenStr := LJson.Path['response'].AsString();

          FTool.ProcessToken(LTokenStr);

        end;

        if LJson.HasPath('done') then
        begin
          if LJson.Path['done'].AsBoolean then
          begin

            if FTokenResponse.Finalize then
            begin
              case FTokenResponse.AddToken('') of
                tpaWait:
                begin
                end;

                tpaAppend:
                begin
                  DoNextToken(FTokenResponse.LastWord(False));
                end;

                tpaNewline:
                begin
                  DoNextToken(#10);
                  DoNextToken(FTokenResponse.LastWord(True));
                end;
              end;
            end;

            if LJson.HasPath('context') then
            begin
              SetContext(LJson.Path['context'].ToString);
             end;

            if LJson.HasPath('prompt_eval_count') then
              FInputTokens := LJson.Path['prompt_eval_count'].AsUInt64;

            if LJson.HasPath('prompt_eval_count') then
              FOutputTokens := LJson.Path['eval_count'].AsUInt64;

            FTotalTokens := FInputTokens + FOutputTokens;

            if LJson.HasPath('eval_duration') then
              LEvalDuration := LJson.Path['eval_duration'].AsUInt64()
            else
              LEvalDuration := 0;

            //to calculate how fast the response is generated in tokens per second (token/s), divide eval_count / eval_duration * 10^9.
            if (LEvalDuration > 0) then
            begin
              FSpeed := FOutputTokens / (LEvalDuration/1_000_000_000);
              FTime := LEvalDuration / 1_000_000_000;
            end;
          end;
        end;

      finally
        // free json object
        LJson.Free;
      end;
    end;

    // repeat until no more lines
  until (LRet < 0);
end;

procedure TOllamaBox.DoReceiveDataPull(const ASender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean);
var
  LData: string;
  LJson: TEasyJson;
  LTextBuffer: string;
  LLine: string;
  LRet: Integer;
  LStatus: string;
  LTotal: Int64;
  LCompleted: Int64;
  LProgress: Double;

begin
  // get text respone stream text
  LTextBuffer := FResponseStream.DataString;

  if LTextBuffer.IsEmpty then Exit;

  // loop over text
  repeat

    // get index of LF
    LRet := LTextBuffer.IndexOf(#10, FResponseStreamPos);

    // process line
    if LRet >= 0 then
    begin
      // get the line from buffer
      LLine := LTextBuffer.Substring(FResponseStreamPos, LRet - FResponseStreamPos);
      FResponseStreamPos := LRet + 1;

      // check for valid line
      if LLine.IsEmpty or
         (LLine.StartsWith(#10)) then
        continue;

      // sanitize line
      LData := LLine.Trim([' ', #13, #10]);
      //writeln(LData);

      // get json response
      LTotal := 0;
      LCompleted := 0;
      LJson := TEasyJson.Create(LData);
      try
        if LJson.HasPath('total') then
          LTotal := LJson.Path['total'].AsInt64;

        if LJson.HasPath('completed') then
          LCompleted := LJson.Path['completed'].AsInt64;

        if LTotal > 0 then
          LProgress := (LCompleted / LTotal) * 100
        else
          LProgress := 100;

        if LJson.HasPath('status') then
        begin
          LStatus := LJson.Path['status'].AsString();

          If LStatus.StartsWith('pulling', True) then
            LStatus := Format('Pulling %s', [FModel]);

          DoPull(LStatus, LProgress, obInProgress);
        end;

      finally
        // free json object
        LJson.Free;
      end;
    end;

    // repeat until no more lines
  until (LRet < 0);
end;

constructor TOllamaBox.Create();
var
  LFilename: string;
begin
  inherited;

  FConfigFile := TobConfigFile.Create();

  LFilename := TPath.Combine(obUtils.GetEXEPath,'OllamaBox.ini');

  FConfigFile.Open(LFilename);
  if not TFile.Exists(LFilename) then
  begin
    FConfigFile.SetValue('APIKEY', 'Tavily', '');
    FConfigFile.SetValue('APIKEY', 'LemonFox', '');
    FConfigFile.Update();
  end;
  FTavilyApiKey := FConfigFile.GetValue('APIKEY', 'Tavily', '');
  FLemonFoxApiKey := FConfigFile.GetValue('APIKEY', 'LemonFox', '');

  FImages := TImages.Create();
  FSystem := TStringList.Create();
  FContext := TEasyJson.Create('[]');
  FResponseStream := TStringStream.Create('', TEncoding.UTF8);
  FTool := TobToolStreamProcessor.Create();
  FTokenResponse.Initialize();

  FServerPort := 43210;
  FModelPath := 'res\models';
  FServerPath := 'res\ollama';
  FServerDownloadPath := 'res\download';
  FProcessHandle := 0;
  FProcessID := 0;

  Model := 'gemma3:4b-it-qat';
  Temperature := 1.0;
  Seed := -1;
  KeepAlive := -1;
  MainGPU := 0;
  GPULayers := -1;
  MaxContext := 1024*4;
  Suffix := '';
  Threads := -1;
  ClearSystem();
  ShowThinking := True;
  ShowResponding := True;

  FFilter.SetWhitelist(['response', 'think']);

  FTool.OnToolDetected :=
    procedure (const AToolName: string; const AParameters: TJSONObject)
    var
      LQuery: string;
      LResult: string;
    begin
      obConsole.PrintLn(obCRLF+'toolcall: %s'+obCRLF, [AToolName]);
      LQuery := AParameters.GetValue<string>('query').Trim();
      LResult := WebSearch(LQuery).Trim();
      FTool.Response := LResult;
    end;

  FTool.OnTextOutput :=
    procedure (AToken: string)
    begin
      case FTokenResponse.AddToken(AToken) of
        tpaWait:
        begin
        end;

        tpaAppend:
        begin
          DoNextToken(FTokenResponse.LastWord(False));
        end;

        tpaNewline:
        begin
          DoNextToken(#10);
          DoNextToken(FTokenResponse.LastWord(True));
        end;
      end;
    end;

  FPrompts := TobPromptDatabase.Create();
  FPrompts.Load('Prompts.txt');

  AddSystem(FPrompts.GetPrompt('IdPrompt'), []);
  AddSystem(FPrompts.GetPrompt('AwarenessPrompt'), [obUtils.GetFormattedDate(), obUtils.GetFormattedTime()]);
  AddSystem(FPrompts.GetPrompt('DeepThinkPrompt'), []);
  AddSystem(FPrompts.GetPrompt('SystemPrompt'), [FPrompts.GetPrompt('ToolsPrompt')]);

end;

destructor TOllamaBox.Destroy();
begin
  FPrompts.Free();
  FTool.Free();
  FResponseStream.Free();
  FContext.Free();
  FSystem.Free();
  FImages.Free();
  FConfigFile.Free();
  StopServer();

  inherited;
end;

procedure DownloadProgress(const AFilename: string; const APercent: Single; var AAbort: Boolean);
begin
  AAbort := obConsole.IsKeyPressed(obVK_ESC);
  obConsole.Print(#13+'Downloading "%s" %.2f%% completed...', [TPath.GetFileName(AFilename), APercent]);
  obConsole.ClearToEndOfLine();
end;

procedure UnzipProgress(const AFilename: string; const APercent: Single; var AAbort: Boolean);
begin
  obConsole.Print(#13+'Extracting %.2f%% completed...', [APercent]);
  obConsole.ClearToEndOfLine();
end;

function   TOllamaBox.GetVersion(): string;
begin
  Result := '0.1.0';
end;

function TOllamaBox.GetOllamaVersion(): string;
begin
  Result := FOllamVersion;
end;

procedure TOllamaBox.DisplayLogo(const AColor: string);
begin
  obConsole.PrintLn(AColor+FPrompts.GetPrompt('AsciiLogo'));
end;

procedure TOllamaBox.DownloadServer();
var
  LVersion: string;
  LUrl: string;
  LFilename: string;
begin
  if not CheckLatestOllamaVersion(LVersion, LUrl) then Exit;

  LFilename := TPath.Combine(FServerDownloadPath, LVersion+'.zip');
  if not TFile.Exists(LFilename) then
    begin
      obUtils.DeleteFolder(FServerDownloadPath, True);

      if obUtils.DownloadFile(LUrl, LFilename, DownloadProgress) then
      begin
        obUtils.DeleteFolder(FServerPath, True);
        if obUtils.UnzipFile(LFilename, FServerPath, UnzipProgress) then
          begin
            // success
          end
        else
          begin
            // failed
          end;
      end;
    end
  else
    begin
      if not TDirectory.Exists(FServerPath) then
      begin
        if obUtils.UnzipFile(LFilename, FServerPath, DownloadProgress) then
          begin
            // success
          end
        else
          begin
            // failed
          end;
      end;
    end;
end;

function TOllamaBox.StartServer(): Boolean;
var
  LStartupInfo: TStartupInfo;
  LProcessInfo: TProcessInformation;
  LCmdLine: string;
  LHostValue: string;
  LErrorCode: DWORD;
begin
  Result := False;

  if FProcessHandle <> 0 then
  begin
    // Since process already running return TRUE
    Exit(True);
  end;

  ZeroMemory(@LStartupInfo, SizeOf(LStartupInfo));
  ZeroMemory(@LProcessInfo, SizeOf(LProcessInfo));
  LStartupInfo.cb := SizeOf(LStartupInfo);
  LStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  LStartupInfo.wShowWindow := SW_HIDE;

  LCmdLine := BuildServerCmdLine;

  // Set environment directly in process
  LHostValue := Format('127.0.0.1:%d', [FServerPort]);

  // Set Ollama host/port
  if not SetEnvironmentVariable('OLLAMA_HOST', PChar(LHostValue)) then
  begin
    SetError('Failed to set OLLAMA_HOST environment variable: %s', [SysErrorMessage(GetLastError())]);
    Exit;
  end;

  // Set Ollama models path
  if FModelPath <> '' then
  begin
    if not SetEnvironmentVariable('OLLAMA_MODELS', PChar(FModelPath)) then
    begin
      SetError('Failed to set OLLAMA_MODELS environment variable: %s', [SysErrorMessage(GetLastError())]);
      Exit;
    end;
  end;

  // Set flash attention | OLLAMA_FLASH_ATTENTION 1
  if not SetEnvironmentVariable('OLLAMA_FLASH_ATTENTION', '1') then
  begin
    SetError('Failed to set OLLAMA_FLASH_ATTENTION environment variable: %s', [SysErrorMessage(GetLastError())]);
    Exit;
  end;

  // Launch process
  if not CreateProcess(nil, PChar(LCmdLine), nil, nil, False,
    CREATE_NO_WINDOW, nil, nil, LStartupInfo, LProcessInfo) then
  begin
    LErrorCode := GetLastError();
    SetError('Failed to launch Ollama: %s', [SysErrorMessage(LErrorCode)]);
    Exit;
  end;

  FProcessHandle := LProcessInfo.hProcess;
  FProcessID := LProcessInfo.dwProcessId;
  CloseHandle(LProcessInfo.hThread);

  DoGetOllamaVersion();

  Result := True;
end;

function  TOllamaBox.ServerStarted(): Boolean;
begin
  Result := Boolean(FProcessID <> 0);
end;

// remove all ollama.exe instances
procedure TOllamaBox.StopServer();
var
  ProcessIDs: array[0..1023] of DWORD;
  BytesReturned, NumProcesses, i: DWORD;
  hProcess: THandle;
  ModuleName: array[0..MAX_PATH] of Char;
  ProcessName: string;
begin
  // First, kill our main Ollama server process
  if FProcessHandle <> 0 then
  begin
    TerminateProcess(FProcessHandle, 0);
    CloseHandle(FProcessHandle);
    FProcessHandle := 0;
    FProcessID := 0;
    FOllamVersion := '';
  end;

  // Next, find and terminate any other Ollama processes
  if EnumProcesses(@ProcessIDs[0], SizeOf(ProcessIDs), BytesReturned) then
  begin
    NumProcesses := BytesReturned div SizeOf(DWORD);

    for i := 0 to NumProcesses - 1 do
    begin
      if ProcessIDs[i] <> 0 then
      begin
        // Get process handle with required access rights
        hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_TERMINATE, False, ProcessIDs[i]);

        if hProcess <> 0 then
        begin
          try
            // Get the executable name
            if GetModuleFileNameEx(hProcess, 0, ModuleName, MAX_PATH) > 0 then
            begin
              ProcessName := LowerCase(ModuleName);

              // If it's an ollama process, terminate it
              if Pos('ollama.exe', ProcessName) > 0 then
              begin
                TerminateProcess(hProcess, 0);
              end;
            end;
          finally
            CloseHandle(hProcess);
          end;
        end;
      end;
    end;
  end;
end;

function  TOllamaBox.ServerRunning(): Boolean;
var
  LResponse: string;
begin
  Result := False;
  if not ServerStarted() then Exit;
  if Http(GetServerBaseAPIUrl(), obHttpGet, '', LResponse) then
    Result := Boolean(LResponse = 'Ollama is running')
  else
    SetError('Ollama service is not running', []);
end;

function TOllamaBox.GetServerBaseAPIUrl(): string;
begin
  Result := Format('http://localhost:%d', [FServerPort]);
end;

procedure TOllamaBox.ClearSystem();
begin
  FSystem.Clear();
end;

function TOllamaBox.AddSystem(const AText: string; const AArgs: array of const): UInt32;
begin
  Result := FSystem.Add(Format(AText, AArgs));
end;

function  TOllamaBox.SystemCount(): UInt32;
begin
  Result := FSystem.Count;
end;

procedure TOllamaBox.RemoveSystem(const AIndex: UInt32);
begin
  if (AIndex >= SystemCount) then
  begin
    SetError('Invalid system index: %d', [AIndex]);
    Exit;
  end;
  FSystem.Delete(AIndex);
end;

procedure TOllamaBox.ClearContext();
begin
  FContext.Free();
  FContext := TEasyJson.Create('[]');
  FResponse := '';
end;

function TOllamaBox.SaveContext(const AFilename: string): Boolean;
begin
  Result := False;

  if AFilename.IsEmpty then
  begin
    SetError('Context filename was not set', []);
    Exit;
  end;

  Result := FContext.SaveToFile(AFilename);

end;

function TOllamaBox.LoadContext(const AFilename: string): Boolean;
var
  LFilename: string;
begin
  Result := False;

  LFilename := TPath.ChangeExtension(AFilename, 'json');

  if not TFile.Exists(LFilename) then
  begin
    SetError('Context file was not found: %s', [LFilename]);
    Exit;
  end;

  Result := FContext.LoadFromFile(LFilename);

end;

procedure TOllamaBox.ClearImages();
begin
  FImages.Clear();
end;

function TOllamaBox.AddImage(const AFilename: string): Boolean;
var
  LFileStream: TFileStream;
  LMemoryStream: TMemoryStream;
  LImage: TImage;
begin
  Result := False;

  if not TFile.Exists(AFilename) then
  begin
    SetError('File does not exist: %s', [AFilename]);
    Exit;
  end;

  try
    LFileStream := TFileStream.Create(AFilename, fmOpenRead);
    try
      LMemoryStream := TMemoryStream.Create();
      try
        LMemoryStream.CopyFrom(LFileStream, LFileStream.Size);

        LImage.Binary := TNetEncoding.Base64.EncodeBytesToString(LMemoryStream.Memory, LMemoryStream.Size);
        LImage.MimeType := obUtils.GetMimeTypeFromExt(AFilename);

        FImages.Add(LImage);
        Result := True;
      finally
        LMemoryStream.Free();
      end;
    finally
      LFileStream.Free();
    end;
  except
    on E: Exception do
      SetError('Error encoding image: %s', [E.Message]);
  end;
end;

function  TOllamaBox.Pull(): Boolean;
var
  LHttpClient: THttpClient;
  LHttpResponse: IHttpResponse;
  LContent: TStringStream;
  LPrompt: string;
  LJson: TEasyJson;
  LContentText: string;
begin
  Result := False;

  if not ServerStarted then
  begin
    SetError('Ollama server is not active', []);
    Exit;
  end;

  if FModel.IsEmpty then
  begin
    SetError('Model not specified.', []);
    Exit;
  end;

  LJson := TEasyJson.Create();
  try
    LJson.Put('model', FModel)
         .Put('stream', True);

    LPrompt := LJson.ToString();
  finally
    LJson.Free();
  end;

  FResponse := '';
  FPartialResponse := '';
  FInThink := False;
  FInResponse := False;
  FResponseStreamPos := 0;
  FHttpStatusCode := 0;
  FHttpStatusText := '';
  FResponseStream.Clear();
  FTokenResponse.Clear();

  LHttpClient := THttpClient.Create;
  try
    LContent := TStringStream.Create(LPrompt, TEncoding.UTF8);
    try
      LHttpClient.CustomHeaders['Content-Type'] := 'application/json';
      LHttpClient.OnReceiveData := DoReceiveDataPull;


      DoPull('', 100, obStart);
      LHttpResponse := LHttpClient.Post(GetUrl('pull'), LContent, FResponseStream);
      DoPull('', 100, obEnd);

      LContentText := LHttpResponse.ContentAsString(TEncoding.UTF8);

      FHttpStatusCode := LHttpResponse.StatusCode;
      FHttpStatusText := LHttpResponse.StatusText;

      LJson := TEasyJson.Create(LContentText);
      try
        if LHttpResponse.StatusCode = 200 then
        begin
          Result := True;
        end
        else
          begin
            if LJson.HasPath('error') then
              SetError(LJson.Path['error'].AsString, [])
            else
              SetError('HTTP Error: %d - %s', [LHttpResponse.StatusCode, LHttpResponse.StatusText]);
          end;
      finally
        LJson.Free();
      end;
    finally
      LContent.Free;
    end;
  finally
    LHttpClient.Free();
  end;
end;

function TOllamaBox.Generate(): Boolean;
var
  LJson: TEasyJson;
  LHttpClient: THttpClient;
  LHttpResponse: IHttpResponse;
  LPrompt: string;
  LContent: TStringStream;
  LContentText: string;
  LDone: Boolean;
  LToolResponseFlag: Boolean;
  LImage: TImage;
  I: Integer;
begin
  Result := False;

  if not ServerStarted() then
  begin
    SetError('Ollama server is not active', []);
    Exit;
  end;

  if FModel.IsEmpty then
  begin
    SetError('Model not specified.', []);
    Exit;
  end;

  LToolResponseFlag := False;

  repeat
    LDone := False;


    FResponse := '';
    FPartialResponse := '';
    FInThink := False;
    FInResponse := False;
    FResponseStreamPos := 0;
    FHttpStatusCode := 0;
    FHttpStatusText := '';
    FResponseStream.Clear();
    FTokenResponse.Clear();
    FFilter.Clear();
    FTool.Clear();

    LJson := TEasyJson.Create();
    try
      LJson.Put('model', FModel);
      LJson.Put('prompt', FPrompt);
      LJson.Put('suffix', FSuffix);
      LJson.Put('stream', True);
      LJson.Put('keep_alive', Integer(FKeepAlive));
      LJson.Put('system', GetSystem());
      LJson.Put('context', FContext);

      with LJson.AddArray('images') do
      begin
        I := 0;
        for LImage in FImages do
        begin
          Put(I, LImage.Binary);
          Inc(I);
        end;
      end;


      with LJson.AddObject('options') do
      begin
        // MaxContext
        Put('num_ctx', UInt32(FMaxContext));

        // GPULayers
        if (FGPULayers < 0) then
          Put('num_gpu', MaxInt)
        else
          Put('num_gpu', FGPULayers);

        // Temperature
        Put('temperature', FTemperature);

        // Seed
        if FSeed < 0 then
          Put('seed', obUtils.RandomRange(0, MaxInt-1))
        else
          Put('seed', FSeed);

        // Threads
        if FThreads <= 0 then
          Put('num_thread', obUtils.GetPhysicalProcessorCount())
        else
          Put('num_thread', FThreads);
      end;

      LPrompt := LJson.ToString();

      //WriteLn(LJson.Format());
    finally
      LJson.Free();
    end;

    // Send http request
    LHttpClient := THttpClient.Create;
    try
      LContent := TStringStream.Create(LPrompt, TEncoding.UTF8);
      try

        LHttpClient.CustomHeaders['Content-Type'] := 'application/json';
        LHttpClient.OnReceiveData := DoReceiveDataGenerate;

        LHttpResponse := LHttpClient.Post(GetUrl('generate'), LContent, FResponseStream);

        LContentText := LHttpResponse.ContentAsString(TEncoding.UTF8);

        FHttpStatusCode := LHttpResponse.StatusCode;
        FHttpStatusText := LHttpResponse.StatusText;

        LJson := TEasyJson.Create(LContentText);
        try
          if LHttpResponse.StatusCode = 200 then
          begin
            if WasCancelled then
            begin
              SetError('Inference was called by user', []);
              Exit;
            end;

            if not FTool.Response.IsEmpty then
            begin
              if LToolResponseFlag then
              begin
                DoNextToken(CobResponseCloseTag);
              end;
              LToolResponseFlag := True;
              DoNextToken(CobResponseOpenTag);
              //Prompt := Format(CToolResponsePrompt, [FTool.Response]);
              Prompt := FPrompts.GetFormattedPrompt('ToolResponsePrompt', [FTool.Response]);
              continue;
            end;

            LDone := True;

            if LToolResponseFlag then
            begin
              DoNextToken(CobResponseCloseTag);
              LToolResponseFlag := False;
            end;

            Result := True;
          end
          else
            begin
              if LJson.HasPath('error') then
                SetError(LJson.Path['error'].AsString, [])
              else
                SetError('HTTP Error: %d - %s', [LHttpResponse.StatusCode, LHttpResponse.StatusText]);
              LDone := True;
            end;
        finally
          LJson.Free();
        end;
      finally
        LContent.Free;
      end;
    finally
      LHttpClient.Free();
    end;
  until LDone;
end;

function TOllamaBox.WebSearch(const AQuery: string): string;
begin
  Result := obUtils.TavilyWebSearch(FTavilyApiKey, AQuery);
end;

end.
