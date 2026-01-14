import mod_with_numbers_0123456789
import shadowed_module.{ShadowPerson}

pub fn mod_with_numbers_test() {
  assert mod_with_numbers_0123456789.hello() == "world"
}

pub fn shadowed_module_test() {
  let shadowed_module = ShadowPerson(18)
  let shadowed_module = shadowed_module.celebrate_birthday(shadowed_module)
  assert shadowed_module.age == 19
}
