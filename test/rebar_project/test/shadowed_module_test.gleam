import should
import shadowed_module.{Person}

pub fn shadowed_module_test() {
  let shadowed_module = Person(18)
  let shadowed_module = shadowed_module.celebrate_birthday(shadowed_module)

  should.equal(shadowed_module.age, 19)
}
