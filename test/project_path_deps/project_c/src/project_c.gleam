import project_d

pub type TypeC {
  ConstructorC(project_d.TypeD)
}

pub fn new(str) {
  ConstructorC(project_d.ConstructorD(str))
}
