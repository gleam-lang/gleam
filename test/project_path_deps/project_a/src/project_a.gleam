import project_b
import project_c

pub type TypeA {
  VariantB(project_b.TypeB)
  VariantC(project_c.TypeC)
}

pub fn main() {
  let _ = VariantB(project_b.new("thing", "name"))
  let _ = VariantC(project_c.new("thing"))
}
