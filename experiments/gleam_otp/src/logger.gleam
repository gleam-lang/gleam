// Bindings to the OTP logger
// http://erlang.org/doc/man/logger.html

import order
import any

pub enum Level =
  | Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Info
  | Debug;

// Logging API functions

type Report(r) =
  { r };

pub external fn log(Level, Report(r)) -> Unit = "logger" "log";

pub external fn emergency(Report(r)) -> Unit = "logger" "emergency";

pub external fn alert(Report(r)) -> Unit = "logger" "alert";

pub external fn critical(Report(r)) -> Unit = "logger" "alert";

pub external fn error(Report(r)) -> Unit = "logger" "error";

pub external fn warning(Report(r)) -> Unit = "logger" "warning";

pub external fn notice(Report(r)) -> Unit = "logger" "notice";

pub external fn info(Report(r)) -> Unit = "logger" "info";

pub external fn debug(Report(r)) -> Unit = "logger" "debug";

// Metadata API functions

type Metadata(r) =
  { r };

pub external fn metadata(Metadata(r)) -> Unit = "logger" "update_process_metadata";

pub external fn overwrite_metadata(Metadata(r)) -> Unit = "logger" "set_process_metadata";

pub external fn reset_metadata() -> Unit = "logger" "unset_process_metadata";

pub external fn get_metadata() -> Metadata(r) = "logger" "get_process_metadata";

// Misc API functinos

pub external fn compare(Level, Level) -> order:Order = "logger" "compare_levels";

// Formatter callback module

pub enum CheckResult(fail_reason) =
  | Ok
  | Error(fail_reason);

// TODO: function for decoding this
// msg :=
//      {io:format(), [term()]} |
//      {report, report()} |
//      {string, unicode:chardata()},
pub external type LogMsg;

pub type LogEvent(m) =
  {
    level = Level,
    msg = LogMsg,
    metadata = Metadata(m),
  };

type Formatter(config, fail_reason, m) =
  module { r |
    fn check_config(FormatterConfig(config)) -> CheckResult(fail_reason)

    fn format(LogEvent(m), FormatterConfig(config)) -> String
  };

// TODO: Handler callback module
