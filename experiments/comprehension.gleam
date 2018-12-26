import result:Ok

pub fn list() {
  // Unsure about the value of this one.
  // The syntax clashes with the matching bind syntax below, which I prefer.
  for {
    x <- [1, 2, 3]
    y <- [1, 2, 3, 4, 5] when y > x
    x + y
  }
}

pub fn matching() {
  // All the Error cases must be of the same type for this to work
  Ok(mod_name) <- read_name()
  Ok(src) <- read_source(mod_name)
  Ok(tokens) <- tokenize(src)
  Ok(ast) <- parse(tokens)
  Ok(annotated_ast) <- infer(ast, mod_name)
  Ok(beam) <- codegen(annotated_ast, mod_name)
  Ok(_) <- write_file(beam, mod_name)
  Ok(())
}
