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

unit UOllamaBox64;

{$I OllamaBox.Defines.inc}

interface

uses
  OllamaBox.Utils,
  OllamaBox;

function  obCreate(): TOllamaBox; stdcall; exports obCreate;
procedure obFree(var AOllamaBox: TOllamaBox); stdcall; exports obFree;
function  obGetVersion(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetVersion;
function  obGetOllamaVersion(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetOllamaVersion;
procedure obDisplayLogo(const AOllamaBox: TOllamaBox; const AColor: PWideChar); stdcall; exports obDisplayLogo;
procedure obDownloadServer(const AOllamaBox: TOllamaBox); stdcall; exports obDownloadServer;
function  obStartServer(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obStartServer;
function  obServerStarted(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obStartServer;
procedure obStopServer(const AOllamaBox: TOllamaBox); stdcall; exports obStopServer;
function  obServerRunning(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obServerRunning;
function  obGetServerBaseAPIUrl(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetServerBaseAPIUrl;
procedure obClearSystem(const AOllamaBox: TOllamaBox); stdcall; exports obClearSystem;
function  obAddSystem(const AOllamaBox: TOllamaBox; const AText: PWideChar): UInt32; stdcall; exports obAddSystem;
function  obSystemCount(const AOllamaBox: TOllamaBox): UInt32; stdcall; exports obSystemCount;
procedure obRemoveSystem(const AOllamaBox: TOllamaBox; const AIndex: UInt32); stdcall; exports obRemoveSystem;
procedure obClearContext(const AOllamaBox: TOllamaBox); stdcall; exports obClearContext;
function  obSaveContext(const AOllamaBox: TOllamaBox; const AFilename: PWideChar): Boolean; stdcall; exports obSaveContext;
function  obLoadContext(const AOllamaBox: TOllamaBox; const AFilename: PWideChar): Boolean; stdcall; exports obLoadContext;
procedure obClearImages(const AOllamaBox: TOllamaBox); stdcall; exports obClearImages;
function  obAddImage(const AOllamaBox: TOllamaBox; const AFilename: PWideChar): Boolean; stdcall; exports obAddImage;
function  obPull(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obPull;
function  obGenerate(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obGenerate;
function  obWebSearch(const AOllamaBox: TOllamaBox; const AQuery: PWideChar): PWideChar; stdcall; exports obWebSearch;
function  obGetServerPort(const AOllamaBox: TOllamaBox): UInt32; stdcall; exports obGetServerPort;
procedure obSetServerPort(const AOllamaBox: TOllamaBox; const APort: UInt32); stdcall; exports obSetServerPort;
function  obGetModelPath(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetModelPath;
procedure obSetModelPath(const AOllamaBox: TOllamaBox; const APath: PWideChar); stdcall; exports obSetModelPath;
function  obGetServerPath(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetServerPath;
procedure obSetServerPath(const AOllamaBox: TOllamaBox; const APath: PWideChar); stdcall; exports obSetServerPath;
function  obGetServerDownloadPath(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetServerDownloadPath;
procedure obSetServerDownloadPath(const AOllamaBox: TOllamaBox; const APath: PWideChar); stdcall; exports obSetServerDownloadPath;
function  obGetModel(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetModel;
procedure obSetModel(const AOllamaBox: TOllamaBox; const AModel: PWideChar); stdcall; exports obSetModel;
function  obGetPrompt(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetPrompt;
procedure obSetPrompt(const AOllamaBox: TOllamaBox; const APrompt: PWideChar); stdcall; exports obSetPrompt;
function  obGetKeepAlive(const AOllamaBox: TOllamaBox): Int32; stdcall; exports obGetKeepAlive;
procedure obSetKeepAlive(const AOllamaBox: TOllamaBox; const AKeepAlive: Int32); stdcall; exports obSetKeepAlive;
function  obGetMainGPU(const AOllamaBox: TOllamaBox): Int32; stdcall; exports obGetMainGPU;
procedure obSetMainGPU(const AOllamaBox: TOllamaBox; const AMainGPU: Int32); stdcall; exports obSetMainGPU;
function  obGetGPULayers(const AOllamaBox: TOllamaBox): Int32; stdcall; exports obGetGPULayers;
procedure obSetGPULayer(const AOllamaBox: TOllamaBox; const AGPULayers: Int32); stdcall; exports obSetGPULayer;
function  obGetMaxContext(const AOllamaBox: TOllamaBox): UInt32; stdcall; exports obGetMaxContext;
procedure obSetMaxContext(const AOllamaBox: TOllamaBox; const AMaxContext: UInt32); stdcall; exports obSetMaxContext;
function  obGetSuffix(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetSuffix;
procedure obSetSuffix(const AOllamaBox: TOllamaBox; const ASuffix: PWideChar); stdcall; exports obSetSuffix;
function  obGetTemperature(const AOllamaBox: TOllamaBox): Single; stdcall; exports obGetTemperature;
procedure obSetTemperature(const AOllamaBox: TOllamaBox; const ATemperature: Single); stdcall; exports obSetTemperature;
function  obGetSeed(const AOllamaBox: TOllamaBox): Int32; stdcall; exports obGetSeed;
procedure obSetSeed(const AOllamaBox: TOllamaBox; const ASeed: Int32); stdcall; exports obSetSeed;
function  obGetThreads(const AOllamaBox: TOllamaBox): Int32; stdcall; exports obGetThreads;
procedure obSetThreads(const AOllamaBox: TOllamaBox; const AThreads: Int32); stdcall; exports obSetThreads;
function  obGetResponse(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetResponse;
function  obGetSystem(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obGetSystem;
function  obGetInputTokens(const AOllamaBox: TOllamaBox): UInt32; stdcall; exports obGetInputTokens;
function  obGetOutputTokens(const AOllamaBox: TOllamaBox): UInt32; stdcall; exports obGetOutputTokens;
function  obGetTotalTokens(const AOllamaBox: TOllamaBox): UInt32; stdcall; exports obGetTotalTokens;
function  obGetSpeed(const AOllamaBox: TOllamaBox): Double; stdcall; exports obGetSpeed;
function  obGetTime(const AOllamaBox: TOllamaBox): Double; stdcall; exports obGetTime;
function  obGetWasCancelled(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obGetWasCancelled;
function  obGetShowThinking(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obGetShowThinking;
procedure obSetShowThinking(const AOllamaBox: TOllamaBox; const AShowThinking: Boolean); stdcall; exports obSetShowThinking;
function  obThinking(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obThinking;
function  obGetShowResponding(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obGetShowResponding;
procedure obSetShowResponding(const AOllamaBox: TOllamaBox; const AShowResponding: Boolean); stdcall; exports obSetShowResponding;
function  obResponding(const AOllamaBox: TOllamaBox): Boolean; stdcall; exports obResponding;
function  obHttpStatusCode(const AOllamaBox: TOllamaBox): Int32; stdcall; exports obHttpStatusCode;
function  obHttpStatusText(const AOllamaBox: TOllamaBox): PWideChar; stdcall; exports obHttpStatusText;

const
  CobStart      = 0;
  CobInProgress = 1;
  CobEnd        = 2;

type
  TobCallback          = procedure(const AUserData: Pointer); stdcall;
  TobCancelCallback    = function(const AUserData: Pointer): Boolean; stdcall;
  TobNextTokenCallback = procedure(const AToken: PWideChar; const AUserData: Pointer); stdcall;
  TobPullModelCallback = procedure(const AMessage: PWideChar; const APercent: Double; const AStatus: UInt32; const AUserData: Pointer); stdcall;

(*

  TobStatus = (obStart, obInProgress, obEnd);
  TobCallback = reference to procedure();
  TobCancelCallback = reference to function(): Boolean;
  TobNextTokenCallback = reference to procedure(const AToken: string);
  TobPullModelCallback = reference to procedure(const AMessage: string; const APercent: Double; const AStatus: TobStatus);

property Context: TArray<Integer> read GetContext;
property OnCancel: TobCancelCallback read FOnCancel write FOnCancel;
property OnNextToken: TobNextTokenCallback read FOnNextToken write FOnNextToken;
property OnThinkStart: TobCallback read FOnThinkStart write FOnThinkStart;
property OnThinkEnd: TobCallback read FOnThinkEnd write FOnThinkEnd;
property OnResponseStart: TobCallback read FOnResponseStart write FOnResponseStart;
property OnResponseEnd: TobCallback read FOnResponseEnd write FOnResponseEnd;
property OnPullModel: TobPullModelCallback read FOnPullModel write FOnPullModel;
property Tool: TobToolStreamProcessor read FTool;
property Prompts: TobPromptDatabase read FPrompts;

function  obGetCancel(): TobCancelCallback;
procedure obSetChancel(const AOllamaBox: TOllamaBox; const ACallback: TobCancelCallback);

*)

procedure obSetOnCancel(const AOllamaBox: TOllamaBox; const AHandler: TobCancelCallback; const AUserData: Pointer); stdcall; exports obSetOnCancel;
procedure obSetOnNextToken(const AOllamaBox: TOllamaBox; const AHandler: TobNextTokenCallback; const AUserData: Pointer); stdcall; exports obSetOnNextToken;
procedure obSetOnThinkStart(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer); stdcall; exports obSetOnThinkStart;
procedure obSetOnThinkEnd(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer); stdcall; exports obSetOnThinkEnd;
procedure obSetOnResponseStart(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer); stdcall; exports obSetOnResponseStart;
procedure obSetOnResponseEnd(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer); stdcall; exports obSetOnResponseEnd;
procedure obSetOnPullModel(const AOllamaBox: TOllamaBox; const AHandler: TobPullModelCallback; const AUserData: Pointer); stdcall; exports obSetOnPullModel;


implementation

procedure obSetOnCancel(const AOllamaBox: TOllamaBox; const AHandler: TobCancelCallback; const AUserData: Pointer);
begin
end;

procedure obSetOnNextToken(const AOllamaBox: TOllamaBox; const AHandler: TobNextTokenCallback; const AUserData: Pointer);
begin
  if not Assigned(AOllamaBox) then Exit;

  if not Assigned(AHandler) then
    AOllamaBox.OnNextToken := nil
  else
    AOllamaBox.OnNextToken :=
      procedure (const AToken: string)
      begin
        AHandler(PWideChar(AToken), AUserData);
      end;
end;

procedure obSetOnThinkStart(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer);
begin
  if not Assigned(AOllamaBox) then Exit;

  if not Assigned(AHandler) then
    AOllamaBox.OnThinkStart := nil
  else
    AOllamaBox.OnThinkStart :=
      procedure ()
      begin
        AHandler(AUserData);
      end;
end;

procedure obSetOnThinkEnd(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer);
begin
  if not Assigned(AOllamaBox) then Exit;

  if not Assigned(AHandler) then
    AOllamaBox.OnThinkEnd := nil
  else
    AOllamaBox.OnThinkEnd :=
      procedure ()
      begin
        AHandler(AUserData);
      end;
end;

procedure obSetOnResponseStart(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer);
begin
  if not Assigned(AOllamaBox) then Exit;

  if not Assigned(AHandler) then
    AOllamaBox.OnResponseStart := nil
  else
    AOllamaBox.OnResponseStart :=
      procedure ()
      begin
        AHandler(AUserData);
      end;
end;

procedure obSetOnResponseEnd(const AOllamaBox: TOllamaBox; const AHandler: TobCallback; const AUserData: Pointer);
begin
  if not Assigned(AOllamaBox) then Exit;

  if not Assigned(AHandler) then
    AOllamaBox.OnResponseEnd := nil
  else
    AOllamaBox.OnResponseEnd :=
      procedure ()
      begin
        AHandler(AUserData);
      end;
end;

procedure obSetOnPullModel(const AOllamaBox: TOllamaBox; const AHandler: TobPullModelCallback; const AUserData: Pointer);
begin
  if not Assigned(AOllamaBox) then Exit;

  if not Assigned(AHandler) then
    AOllamaBox.OnPullModel := nil
  else
    AOllamaBox.OnPullModel :=
      procedure (const AMessage: string; const APercent: Double; const AStatus: TobStatus)
      begin
        AHandler(PWideChar(AMessage), APercent, Ord(AStatus), AUserData);
      end;
end;

function  obCreate(): TOllamaBox;
begin
  Result := TOllamaBox.Create();
end;

procedure obFree(var AOllamaBox: TOllamaBox);
begin
  if Assigned(AOllamaBox) then
  begin
    AOllamaBox.Free();
    AOllamaBox := nil;
  end;
end;

function  obGetVersion(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;
  Result := PWideChar(AOllamaBox.GetVersion());
end;

function  obGetOllamaVersion(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.GetOllamaVersion());
end;

procedure obDisplayLogo(const AOllamaBox: TOllamaBox; const AColor: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.DisplayLogo(AColor);
end;

procedure obDownloadServer(const AOllamaBox: TOllamaBox);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.DownloadServer();
end;

function  obStartServer(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.StartServer();
end;

function  obServerStarted(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.ServerStarted();
end;

procedure obStopServer(const AOllamaBox: TOllamaBox);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.StopServer();
end;

function  obServerRunning(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.ServerRunning();
end;

function  obGetServerBaseAPIUrl(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.GetServerBaseAPIUrl());
end;

procedure obClearSystem(const AOllamaBox: TOllamaBox);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ClearSystem();
end;

function  obAddSystem(const AOllamaBox: TOllamaBox; const AText: PWideChar): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.AddSystem(string(AText), [])
end;

function  obSystemCount(const AOllamaBox: TOllamaBox): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.SystemCount();
end;

procedure obRemoveSystem(const AOllamaBox: TOllamaBox; const AIndex: UInt32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.RemoveSystem(AIndex);
end;

procedure obClearContext(const AOllamaBox: TOllamaBox);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ClearContext();
end;

function  obSaveContext(const AOllamaBox: TOllamaBox; const AFilename: PWideChar): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.SaveContext(string(AFilename));
end;

function  obLoadContext(const AOllamaBox: TOllamaBox; const AFilename: PWideChar): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.LoadContext(string(AFilename));
end;

procedure obClearImages(const AOllamaBox: TOllamaBox);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ClearImages();
end;

function  obAddImage(const AOllamaBox: TOllamaBox; const AFilename: PWideChar): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.AddImage(string(AFilename));
end;

function  obPull(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Pull();
end;

function  obGenerate(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Generate();
end;

function  obWebSearch(const AOllamaBox: TOllamaBox; const AQuery: PWideChar): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.WebSearch(string(AQuery)));
end;

function  obGetServerPort(const AOllamaBox: TOllamaBox): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.ServerPort;
end;

procedure obSetServerPort(const AOllamaBox: TOllamaBox; const APort: UInt32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ServerPort := APort;
end;

function  obGetModelPath(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.ModelPath);
end;

procedure obSetModelPath(const AOllamaBox: TOllamaBox; const APath: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ModelPath := string(APath);
end;

function  obGetServerPath(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.ServerPath);
end;

procedure obSetServerPath(const AOllamaBox: TOllamaBox; const APath: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ServerPath := string(APath);
end;

function  obGetServerDownloadPath(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.ServerDownloadPath);
end;

procedure obSetServerDownloadPath(const AOllamaBox: TOllamaBox; const APath: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ServerDownloadPath := string(APath);
end;

function  obGetModel(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.Model);
end;

procedure obSetModel(const AOllamaBox: TOllamaBox; const AModel: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.Model := string(AModel);
end;

function  obGetPrompt(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.Prompt);
end;

procedure obSetPrompt(const AOllamaBox: TOllamaBox; const APrompt: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.Prompt := string(APrompt);
end;

function  obGetKeepAlive(const AOllamaBox: TOllamaBox): Int32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.KeepAlive;
end;

procedure obSetKeepAlive(const AOllamaBox: TOllamaBox; const AKeepAlive: Int32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.KeepAlive := AKeepAlive;
end;

function  obGetMainGPU(const AOllamaBox: TOllamaBox): Int32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.MainGPU;
end;

procedure obSetMainGPU(const AOllamaBox: TOllamaBox; const AMainGPU: Int32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.MainGPU := AMainGPU;
end;

function  obGetGPULayers(const AOllamaBox: TOllamaBox): Int32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.GPULayers;
end;

procedure obSetGPULayer(const AOllamaBox: TOllamaBox; const AGPULayers: Int32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.GPULayers := AGPULayers;
end;

function  obGetMaxContext(const AOllamaBox: TOllamaBox): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.MaxContext;
end;

procedure obSetMaxContext(const AOllamaBox: TOllamaBox; const AMaxContext: UInt32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.MaxContext := AMaxContext;
end;

function  obGetSuffix(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.Suffix);
end;

procedure obSetSuffix(const AOllamaBox: TOllamaBox; const ASuffix: PWideChar);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.Suffix := string(ASuffix);
end;

function  obGetTemperature(const AOllamaBox: TOllamaBox): Single;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Temperature;
end;

procedure obSetTemperature(const AOllamaBox: TOllamaBox; const ATemperature: Single);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.Temperature := ATemperature;
end;

function  obGetSeed(const AOllamaBox: TOllamaBox): Int32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Seed;
end;

procedure obSetSeed(const AOllamaBox: TOllamaBox; const ASeed: Int32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.Seed := ASeed;
end;

function  obGetThreads(const AOllamaBox: TOllamaBox): Int32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Threads;
end;

procedure obSetThreads(const AOllamaBox: TOllamaBox; const AThreads: Int32);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.Threads := AThreads;

end;

function  obGetResponse(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.Response);
end;

function  obGetSystem(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.System);
end;

function  obGetInputTokens(const AOllamaBox: TOllamaBox): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.InputTokens;
end;

function  obGetOutputTokens(const AOllamaBox: TOllamaBox): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.OutputTokens;
end;

function  obGetTotalTokens(const AOllamaBox: TOllamaBox): UInt32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.TotalTokens;
end;

function  obGetSpeed(const AOllamaBox: TOllamaBox): Double;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Speed;
end;

function  obGetTime(const AOllamaBox: TOllamaBox): Double;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Time;
end;

function  obGetWasCancelled(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.WasCancelled;
end;

function  obGetShowThinking(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.ShowThinking;
end;

procedure obSetShowThinking(const AOllamaBox: TOllamaBox; const AShowThinking: Boolean);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ShowThinking := AShowThinking;
end;

function  obThinking(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Thinking;
end;

function  obGetShowResponding(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.ShowResponding;
end;

procedure obSetShowResponding(const AOllamaBox: TOllamaBox; const AShowResponding: Boolean);
begin
  if not Assigned(AOllamaBox) then Exit;

  AOllamaBox.ShowResponding := AShowResponding;
end;

function  obResponding(const AOllamaBox: TOllamaBox): Boolean;
begin
  Result := False;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.Responding;
end;

function  obHttpStatusCode(const AOllamaBox: TOllamaBox): Int32;
begin
  Result := 0;
  if not Assigned(AOllamaBox) then Exit;

  Result := AOllamaBox.HttpStatusCode;
end;

function  obHttpStatusText(const AOllamaBox: TOllamaBox): PWideChar;
begin
  Result := nil;
  if not Assigned(AOllamaBox) then Exit;

  Result := PWideChar(AOllamaBox.HttpStatusText);
end;

end.
