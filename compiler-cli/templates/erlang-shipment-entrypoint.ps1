$ErrorActionPreference = "Stop"

$PackageName = "$PACKAGE_NAME_FROM_GLEAM"
$BaseDirectory = $PSScriptRoot
$ScriptCommand = $args[0]

$CodePath = Join-Path -Path $BaseDirectory -ChildPath "\*\ebin" -Resolve

function Run {
  erl `
    -pa $CodePath `
    -eval "$PackageName@@main:run($PackageName)" `
    -noshell `
    -extra $args
}

function Shell {
  erl -pa $CodePath
}

switch ($ScriptCommand) {
  "run" {
    Run $args[1..($args.Length - 1)]
  }
  "shell" {
    Shell
  }
  default {
    Write-Host "usage:"
    Write-Host "  entrypoint.ps1 `$COMMAND"
    Write-Host ""
    Write-Host "commands:"
    Write-Host "  run    Run the project main function"
    Write-Host "  shell  Run an Erlang shell"
    exit 1
  }
}
