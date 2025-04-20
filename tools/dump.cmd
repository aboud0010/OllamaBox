@echo off
cd /d "%~dp0"
call tdump "..\bin\OllamaBox64.dll" > OllamaBox64.dll.txt
call OllamaBox64.dll.txt
