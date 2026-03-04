$ErrorActionPreference = "Stop"

$PackageName = "$PACKAGE_NAME_FROM_GLEAM"
$BaseDirectory = $PSScriptRoot
$ScriptCommand = $args[0]

$CodePath = Join-Path -Path $BaseDirectory -ChildPath "\*\ebin" -Resolve

function Run {
  erl `
    -pa $CodePath `
    -eval 'case erlang:system_info(otp_release) of "$OTP_VERSION_FROM_GLEAM" -> ok; V -> io:format(standard_error, "warning: This Erlang shipment was built with Erlang/OTP $OTP_VERSION_FROM_GLEAM but you are running Erlang/OTP ~s.~nThe shipment may fail to run. Please use Erlang/OTP $OTP_VERSION_FROM_GLEAM or rebuild the shipment with your current version.~n", [V]) end' `
    -eval "$PackageName@@main:run($PackageName)" `
    -noshell `
    -extra $args
}

function Shell {
  erl `
    -pa $CodePath `
    -eval 'case erlang:system_info(otp_release) of "$OTP_VERSION_FROM_GLEAM" -> ok; V -> io:format(standard_error, "warning: This Erlang shipment was built with Erlang/OTP $OTP_VERSION_FROM_GLEAM but you are running Erlang/OTP ~s.~nThe shipment may fail to run. Please use Erlang/OTP $OTP_VERSION_FROM_GLEAM or rebuild the shipment with your current version.~n", [V]) end'
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
