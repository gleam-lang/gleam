$ErrorActionPreference = "Stop"

$PackageName = "$PACKAGE_NAME_FROM_GLEAM"
$BaseDirectory = $PSScriptRoot
$ScriptCommand = $args[0]
$OtpVersion = "$OTP_VERSION_FROM_GLEAM"
$OtpVersionCheck = "case erlang:system_info(otp_release) of `"$OtpVersion`" -> ok; V -> io:format(standard_error, `"warning: This Erlang shipment was built with Erlang/OTP $OtpVersion but you are running Erlang/OTP ~s.~nThe shipment may fail to run. Please use Erlang/OTP $OtpVersion or rebuild the shipment with your current version.~n`", [V]) end"

$CodePath = Join-Path -Path $BaseDirectory -ChildPath "\*\ebin" -Resolve

function CheckOtpVersion {
  erl `
    -noshell `
    -eval $OtpVersionCheck `
    -eval 'halt().'

  if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
  }
}

function Run {
  erl `
    -pa $CodePath `
    -eval $OtpVersionCheck `
    -eval "$PackageName@@main:run($PackageName)" `
    -noshell `
    -extra $args
}

function Shell {
  CheckOtpVersion
  erl `
    -pa $CodePath
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
