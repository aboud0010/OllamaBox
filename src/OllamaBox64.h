/*******************************************************************************
    ___  _ _                 ___
   / _ \| | |__ _ _ __  __ _| _ ) _____ __™
  | (_) | | / _` | '  \/ _` | _ \/ _ \ \ /
   \___/|_|_\__,_|_|_|_\__,_|___/\___/_\_\
  Embed & Control Ollama In Your Delphi App

 Copyright © 2025-present tinyBigGAMES™ LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/OllamaBox

 See LICENSE file for license information
*******************************************************************************/

#ifndef OLLAMABOX64_H
#define OLLAMABOX64_H

// Check for supported platform
#ifndef _WIN64
#error "Unsupported platform"
#endif

// Link in OllamaBox64.lib
#pragma comment(lib,"OllamaBox64.lib")

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <wchar.h>

#define CobStart       0
#define CobInProgress  1
#define CcbEnd         2

typedef void* TOllamaBox;

typedef void (__stdcall *TobCallback)(void* AUserData);
typedef int  (__stdcall *TobCancelCallback)(void* AUserData);
typedef void (__stdcall *TobNextTokenCallback)(const wchar_t* AToken, void* AUserData);
typedef void (__stdcall *TobPullModelCallback)(const wchar_t* AMessage, double APercent, uint32_t AStatus, void* AUserData);

TOllamaBox __stdcall obCreate(void);
void __stdcall obFree(TOllamaBox* AOllamaBox);

void __stdcall obSetOnCancel(TOllamaBox AOllamaBox, TobCancelCallback AHandler, void* AUserData);
void __stdcall obSetOnNextToken(TOllamaBox AOllamaBox, TobNextTokenCallback AHandler, void* AUserData);
void __stdcall obSetOnThinkStart(TOllamaBox AOllamaBox, TobCallback AHandler, void* AUserData);
void __stdcall obSetOnThinkEnd(TOllamaBox AOllamaBox, TobCallback AHandler, void* AUserData);
void __stdcall obSetOnResponseStart(TOllamaBox AOllamaBox, TobCallback AHandler, void* AUserData);
void __stdcall obSetOnResponseEnd(TOllamaBox AOllamaBox, TobCallback AHandler, void* AUserData);
void __stdcall obSetOnPullModel(TOllamaBox AOllamaBox, TobPullModelCallback AHandler, void* AUserData);

const wchar_t* __stdcall obGetVersion(TOllamaBox AOllamaBox);
const wchar_t* __stdcall obGetOllamaVersion(TOllamaBox AOllamaBox);
void __stdcall obDisplayLogo(TOllamaBox AOllamaBox, const wchar_t* AColor);
void __stdcall obDownloadServer(TOllamaBox AOllamaBox);
int  __stdcall obStartServer(TOllamaBox AOllamaBox);
int  __stdcall obServerStarted(TOllamaBox AOllamaBox);
void __stdcall obStopServer(TOllamaBox AOllamaBox);
int  __stdcall obServerRunning(TOllamaBox AOllamaBox);

const wchar_t* __stdcall obGetServerBaseAPIUrl(TOllamaBox AOllamaBox);
void __stdcall obClearSystem(TOllamaBox AOllamaBox);
uint32_t __stdcall obAddSystem(TOllamaBox AOllamaBox, const wchar_t* AText);
uint32_t __stdcall obSystemCount(TOllamaBox AOllamaBox);
void __stdcall obRemoveSystem(TOllamaBox AOllamaBox, uint32_t AIndex);
const wchar_t* __stdcall obGetSystem(TOllamaBox AOllamaBox);

void __stdcall obClearContext(TOllamaBox AOllamaBox);
int  __stdcall obSaveContext(TOllamaBox AOllamaBox, const wchar_t* AFilename);
int  __stdcall obLoadContext(TOllamaBox AOllamaBox, const wchar_t* AFilename);

void __stdcall obClearImages(TOllamaBox AOllamaBox);
int  __stdcall obAddImage(TOllamaBox AOllamaBox, const wchar_t* AFilename);
int  __stdcall obPull(TOllamaBox AOllamaBox);
int  __stdcall obGenerate(TOllamaBox AOllamaBox);

const wchar_t* __stdcall obWebSearch(TOllamaBox AOllamaBox, const wchar_t* AQuery);
uint32_t __stdcall obGetServerPort(TOllamaBox AOllamaBox);
void __stdcall obSetServerPort(TOllamaBox AOllamaBox, uint32_t APort);
const wchar_t* __stdcall obGetModelPath(TOllamaBox AOllamaBox);
void __stdcall obSetModelPath(TOllamaBox AOllamaBox, const wchar_t* APath);
const wchar_t* __stdcall obGetServerPath(TOllamaBox AOllamaBox);
void __stdcall obSetServerPath(TOllamaBox AOllamaBox, const wchar_t* APath);
const wchar_t* __stdcall obGetServerDownloadPath(TOllamaBox AOllamaBox);
void __stdcall obSetServerDownloadPath(TOllamaBox AOllamaBox, const wchar_t* APath);
const wchar_t* __stdcall obGetModel(TOllamaBox AOllamaBox);
void __stdcall obSetModel(TOllamaBox AOllamaBox, const wchar_t* AModel);
const wchar_t* __stdcall obGetPrompt(TOllamaBox AOllamaBox);
void __stdcall obSetPrompt(TOllamaBox AOllamaBox, const wchar_t* APrompt);

int32_t __stdcall obGetKeepAlive(TOllamaBox AOllamaBox);
void __stdcall obSetKeepAlive(TOllamaBox AOllamaBox, int32_t AKeepAlive);
int32_t __stdcall obGetMainGPU(TOllamaBox AOllamaBox);
void __stdcall obSetMainGPU(TOllamaBox AOllamaBox, int32_t AMainGPU);
int32_t __stdcall obGetGPULayers(TOllamaBox AOllamaBox);
void __stdcall obSetGPULayer(TOllamaBox AOllamaBox, int32_t AGPULayers);
uint32_t __stdcall obGetMaxContext(TOllamaBox AOllamaBox);
void __stdcall obSetMaxContext(TOllamaBox AOllamaBox, uint32_t AMaxContext);
const wchar_t* __stdcall obGetSuffix(TOllamaBox AOllamaBox);
void __stdcall obSetSuffix(TOllamaBox AOllamaBox, const wchar_t* ASuffix);
float __stdcall obGetTemperature(TOllamaBox AOllamaBox);
void __stdcall obSetTemperature(TOllamaBox AOllamaBox, float ATemperature);
int32_t __stdcall obGetSeed(TOllamaBox AOllamaBox);
void __stdcall obSetSeed(TOllamaBox AOllamaBox, int32_t ASeed);
int32_t __stdcall obGetThreads(TOllamaBox AOllamaBox);
void __stdcall obSetThreads(TOllamaBox AOllamaBox, int32_t AThreads);

const wchar_t* __stdcall obGetResponse(TOllamaBox AOllamaBox);
uint32_t __stdcall obGetInputTokens(TOllamaBox AOllamaBox);
uint32_t __stdcall obGetOutputTokens(TOllamaBox AOllamaBox);
uint32_t __stdcall obGetTotalTokens(TOllamaBox AOllamaBox);
double __stdcall obGetSpeed(TOllamaBox AOllamaBox);
double __stdcall obGetTime(TOllamaBox AOllamaBox);
int __stdcall obGetWasCancelled(TOllamaBox AOllamaBox);

int __stdcall obGetShowThinking(TOllamaBox AOllamaBox);
void __stdcall obSetShowThinking(TOllamaBox AOllamaBox, int AShowThinking);
int __stdcall obThinking(TOllamaBox AOllamaBox);
int __stdcall obGetShowResponding(TOllamaBox AOllamaBox);
void __stdcall obSetShowResponding(TOllamaBox AOllamaBox, int AShowResponding);
int __stdcall obResponding(TOllamaBox AOllamaBox);

int32_t __stdcall obHttpStatusCode(TOllamaBox AOllamaBox);
const wchar_t* __stdcall obHttpStatusText(TOllamaBox AOllamaBox);

#ifdef __cplusplus
}
#endif

#endif // OLLAMABOX64_H
