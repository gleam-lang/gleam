import importable.{NoFields}

pub fn no_fields_qualified_and_unqualified_test() {
  assert importable.NoFields == NoFields
}

pub fn no_fields_assert_assignment_test() {
  let result = {
    let assert importable.NoFields = importable.NoFields
  }
  assert result == importable.NoFields
}

pub fn no_fields_unqualified_assert_assignment_test() {
  let result = {
    let assert NoFields = importable.NoFields
  }
  assert result == importable.NoFields
}

pub fn no_fields_let_assignment_test() {
  let result = {
    let importable.NoFields = importable.NoFields
  }
  assert result == importable.NoFields
}

pub fn no_fields_unqualified_let_assignment_test() {
  let result = {
    let NoFields = importable.NoFields
  }
  assert result == importable.NoFields
}
