// https://github.com/gleam-lang/gleam/issues/807
import power.{Power}

pub fn module_function(power: Power) {
  // Here we are referring to the `power` module's function, not a field on the
  // `Power` record.
  power.to_int(power)
}

pub fn record_field(power: Power) {
  // Here we are referring to the `Power` record's field.
  power.value
}
