import Foreign

pub external type Timeout
;

pub fn hibernate() => Timeout {
  Foreign:unsafeCoerce('hibernate')
}

pub fn never() => Timeout {
  Foreign:unsafeCoerce('infinity')
}

pub fn milliseconds(ms: Int) => Timeout {
  Foreign:unsafeCoerce(ms)
}

pub external fn seconds(Int) => Timeout = 'timer' 'seconds'

pub external fn minutes(Int) => Timeout = 'timer' 'minutes'

pub external fn hours(Int) => Timeout = 'timer' 'hours'
