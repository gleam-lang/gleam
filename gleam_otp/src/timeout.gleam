import Foreign

pub external type Timeout
;

pub fn hibernate() => Timeout {
  foreign:unsafeCoerce('hibernate')
}

pub fn never() => Timeout {
  foreign:unsafeCoerce('infinity')
}

pub fn milliseconds(ms: Int) => Timeout {
  foreign:unsafeCoerce(ms)
}

pub external fn seconds(Int) => Timeout = 'timer' 'seconds'

pub external fn minutes(Int) => Timeout = 'timer' 'minutes'

pub external fn hours(Int) => Timeout = 'timer' 'hours'
