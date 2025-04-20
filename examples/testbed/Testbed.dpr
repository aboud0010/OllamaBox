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

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  OllamaBox in '..\..\src\OllamaBox.pas',
  OllamaBox.Utils in '..\..\src\OllamaBox.Utils.pas',
  UTestbed in 'UTestbed.pas',
  OllamaBox64 in '..\..\src\OllamaBox64.pas';

begin
  try
    RunTests();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
