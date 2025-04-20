@echo off
cd /d "%~dp0"
call polib /machine:x64 /out:"..\src\OllamaBox64.lib" "..\bin\OllamaBox64.dll"