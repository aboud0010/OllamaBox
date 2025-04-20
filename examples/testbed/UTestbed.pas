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

unit UTestbed;

interface

{
  ===== USAGE NOTES =====
  * GPU Settings:
   - Setting `MainGPU` to `-1` will automatically select the best GPU
     available on your system.
   - Alternatively, you can specify a GPU by setting `MainGPU` to `0 - N`
     (where `N` is the GPU index).
   - For `MaxGPULayers`:
     - Setting it to `-1` will use all available layers on the GPU.
     - Setting it to `0` will use the CPU only.
     - Setting it to `1 - N` will offload a specific number of layers to the
       GPU.

  * Customizing Output:
   - You can configure various callbacks to control the model's output
     according to your needs.

  * Optimized for Local Inference:
   - Locama is designed for efficient local inference on consumer-grade
     hardware. Using a 4-bit quantized model ensures fast loading and
     performance on modern consumer GPUs.

  * Get search api key from:
   - https://tavily.com/
   - You get 1000 free searches per month.
   - Add the API key to the [APIKEY] section of OllamaBox.ini under the Tavily
     entry.

  * See comments in `OllamaBox.ini.Template.txt` file
}

uses
  System.SysUtils,
  OllamaBox64,
  OllamaBox.Utils,
  OllamaBox;

procedure RunTests();

implementation

{ -----------------------------------------------------------------------------
 Test01: OllamaBox Interactive Chat
 This procedure demonstrates an interactive command-line chat session using
 the TOllamaBox class. It performs the following actions:
 - Downloads and starts the Ollama server
 - Pulls the default model
 - Displays a splash screen and handles response events
 - Supports command input (/help, /clear, /bye)
 - Streams responses with real-time token feedback and performance stats
 - Saves and loads context from a "session" file

 The OnNextToken and OnResponseStart events are used to handle token streaming
 and UI state updates. This is a complete CLI chat loop using OllamaBox.
------------------------------------------------------------------------------ }
procedure Test01();
var
  LOllamaBox: TOllamaBox;
  LPrompt: string;
  LDone: Boolean;
  LCmd: string;
begin
  // Create an instance of TOllamaBox
  LOllamaBox := TOllamaBox.Create();
  try
    // Setup token streaming callback
    LOllamaBox.OnNextToken :=
      procedure (const AToken: string)
      begin
        if LOllamaBox.Thinking then
          obConsole.Print(obCSIDim+obCSIFGWhite+AToken)  // dim output if "thinking"
        else
          obConsole.Print(obCSIFGGreen+AToken);          // green output for normal stream
      end;

    // Setup initial console state
    obConsole.SetTitle('OllamaBox - Chat');
    obConsole.ClearScreen();
    obConsole.PrintLn('One moment...');

    // Download and start the Ollama server
    LOllamaBox.DownloadServer();
    LOllamaBox.StartServer();

    // Check if Ollam server is running
    if not LOllamaBox.ServerRunning() then
    begin
      obConsole.PrintLn(LOllamaBox.Error);
      Exit;
    end;

    // Pull the default model
    LOllamaBox.Pull();

    // Set initial inference parameters
    LOllamaBox.Temperature := 1.0;
    LOllamaBox.Seed := -1;
    LOllamaBox.ShowThinking := False;
    LOllamaBox.Prompt := 'Hello.';

    // Setup splash screen callback for first response
    LOllamaBox.OnResponseStart :=
      procedure
      begin
        obConsole.ClearScreen();
        obConsole.PrintLn();
        LOllamaBox.DisplayLogo(obCSIFGMagenta);
        obConsole.PrintLn(obCSIFGCyan+'      OllamaBox v%s | Ollama v%s', [LOllamaBox.GetVersion(), LOllamaBox.GetOllamaVersion()]);
        obConsole.PrintLn(obCRLF+obCSIDim+obCSIFGWhite+'Type your question and press ENTER or type');
        obConsole.PrintLn(obCSIDim+obCSIFGWhite+'"/help" for available commands.'+obCRLF);
      end;

    // Run the initial model generation (greeting)
    if not LOllamaBox.Generate() then Exit;

    // Enable live thinking display
    LOllamaBox.ShowThinking := True;

    // Disable splash for future generations
    LOllamaBox.OnResponseStart := nil;

    // Update inference params for deterministic output
    LOllamaBox.Temperature := 0.0;
    LOllamaBox.Seed := 42;

    // Load conversation context from file
    LOllamaBox.LoadContext('session');

    // Print a blank line for spacing
    obConsole.PrintLn(obCRLF);

    // Initialize session loop
    LDone := False;
    while not LDone do
    begin
      // Let the message queue process events
      obUtils.ProcessMessages();

      // Prompt the user
      obConsole.PrintLn('Question:');
      obConsole.Print('>');
      ReadLn(LPrompt);

      // Skip empty input
      if LPrompt.Trim.IsEmpty then
        continue;

      // Handle slash commands
      LCmd := LPrompt.Trim;
      if LCmd.StartsWith('/') then
      begin
        if SameText(LCmd, '/bye') then
        begin
          LDone := True;
          continue;
        end
        else if SameText(LCmd, '/clear') then
        begin
          LOllamaBox.ClearContext();
          obConsole.PrintLn(obCSIFGBrightYellow+obCRLF+'Cleared context.'+obCRLF);
          continue;
        end
        else if SameText(LCmd, '/help') then
        begin
          obConsole.PrintLn(obCSIFGBrightYellow+obCRLF+'Available commands:');
          obConsole.PrintLn(obCSIFGBrightYellow+'/bye   - quit');
          obConsole.PrintLn(obCSIFGBrightYellow+'/clear - clear session');
          obConsole.PrintLn(obCSIFGBrightYellow+'/help  - show this help');
          obConsole.PrintLn();
          continue;
        end
        else
        begin
          obConsole.PrintLn(obCSIFGRed+'Invalid command.'+obCRLF);
          continue;
        end;
      end;

      // Assign prompt and generate response
      LOllamaBox.Prompt := LPrompt;
      obConsole.PrintLn(obCRLF+'Response:');

      // Run inference and show results
      if LOllamaBox.Generate() then
      begin
        obConsole.PrintLn(obCRLF+obCRLF+obCSIFGBrightYellow+
          'Tokens in: %d, out: %d, total: %d | time: %.2f secs, speed: %.2f t/s',
          [LOllamaBox.InputTokens, LOllamaBox.OutputTokens,
           LOllamaBox.TotalTokens, LOllamaBox.Time, LOllamaBox.Speed]);
        obConsole.PrintLn();
      end
      else
      begin
        obConsole.PrintLn(obCRLF+obCRLF+obCSIFGRed+'%s', [LOllamaBox.Error]);
        obConsole.PrintLn();
      end;
    end;

    // Save context before exiting
    LOllamaBox.SaveContext('session');

    // Final "goodbye" message
    LOllamaBox.Temperature := 1.0;
    LOllamaBox.Seed := -1;
    LOllamaBox.ShowThinking := False;
    LOllamaBox.Prompt := 'Bye.';
    LOllamaBox.Generate();

    obConsole.PrintLn();

  finally
    // Free OllamaBox instance
    LOllamaBox.Free();
  end;
end;

{ -----------------------------------------------------------------------------
 Test02_NextToken: Token Stream Callback
 This callback is used by `Test02` to handle token-by-token streaming output
 from the Ollama model. It receives each token as it is generated and writes it
 directly to the console using `write`.

 Parameters:
   AToken     - Pointer to the wide-character string containing the token.
   AUserData  - Optional user-defined pointer (unused in this example).

 This callback is passed to `obSetOnNextToken` and is invoked automatically
 during inference when new tokens are available.
------------------------------------------------------------------------------ }
procedure Test02_NextToken(const AToken: PWideChar; const AUserData: Pointer); stdcall;
begin
  // Write the token to the console
  write(string(AToken));
end;

{ -----------------------------------------------------------------------------
 Test02: Procedural API Demo
 This procedure demonstrates how to use the OllamaBox procedural API to perform
 a basic prompt and response exchange. It does the following:
 - Creates a new OllamaBox instance using `obCreate`
 - Downloads and starts the embedded Ollama server
 - Verifies the server is running
 - Sets a callback for streaming tokens (`Test02_NextToken`)
 - Sends a prompt and streams the response
 - Frees the OllamaBox instance on exit

 This example uses only the procedural functions, making it ideal for simpler
 integrations or language bindings that do not support Delphi-style classes.
------------------------------------------------------------------------------ }
procedure Test02();
var
  LOllamaBox: OllamaBox64.TOllamaBox;
begin
  // Create a new instance of TOllamaBox using the procedural API
  LOllamaBox := obCreate();
  try
    // Display OllamaBox ascii logo
    obDisplayLogo(LOllamaBox, nil);
    WriteLn;

    // Download the Ollama server if not already present
    obDownloadServer(LOllamaBox);

    // Start the Ollama server and exit if it fails
    if not obStartServer(LOllamaBox) then Exit;

    // Confirm the server is running before continuing
    if not obServerRunning(LOllamaBox) then Exit;

   // Pull the default model
    obPull(LOllamaBox);

    // Set a token streaming callback
    obSetOnNextToken(LOllamaBox, Test02_NextToken, nil);

    // Provide a prompt to the model
    obSetPrompt(LOllamaBox, 'who are you?');

    // Generate a response and stream the output
    obGenerate(LOllamaBox);

  finally
    // Free the instance and clean up
    obFree(LOllamaBox);
  end;
end;


{ -----------------------------------------------------------------------------
 RunTests: Entry Point for Example Execution
 This procedure serves as the main test runner for the OllamaBox examples. It
 selects and executes a specific test routine based on the value of LNum. This
 structure allows for easy expansion by adding new cases as more tests are
 created. After running the selected test, the console is paused for review.
------------------------------------------------------------------------------ }
procedure RunTests();
var
  LNum: Integer;
begin
  // Set the test number to run
  LNum := 02;

  // Select and execute the corresponding test
  case LNum of
    01: Test01();
    02: Test02();
  end;

  // Pause the console so results can be reviewed before closing
  obConsole.Pause();
end;

end.
