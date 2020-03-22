use super::*;

#[test]
fn module_test() {
    macro_rules! assert_format {
        ($src:expr $(,)?) => {
            let src = $src.to_string();
            let stripped = crate::parser::strip_extra(src.as_ref());
            let ast = crate::grammar::ModuleParser::new()
                .parse(&stripped)
                .unwrap();
            assert_eq!(src, pretty_module(&ast));
        };
    }

    assert_format!("\n");
}

// #[test]
// fn end_to_end_test() {
//     let already_formatted = "
// import other
// import something/else
// import library.{ThingA, ThingB}
// import doctor as seuss

// pub type RoseTree(a) {
//   Node(val: a, children: List(RoseTree(a)))
//   Leaf(val: a)
// }

// type Option(a) = Result(a, Nil)

// pub external type Opaque

// pub external fn random_float() -> Float = \"rand\" \"uniform\"

// fn fully_typed(first: Int) -> Int {
//     first + 1
// }

// fn id(x: a, y: b) {
//     x
// }

// pub fn x() {
//     id(1.0, 1)
// }

// fn lets() {
//     let x = 1
//     let y = 2
//     x + y
// }

// fn patterns(x) {
//     case x {
//         1 -> 42
//         _other -> {
//             let x = 3
//             3 + 4
//         }
//     }
// }
// ";
//     let stripped = crate::parser::strip_extra(already_formatted);
//     if let Ok(module) = crate::grammar::ModuleParser::new().parse(&stripped) {
//         println!("FORMATTED\n-------------\n{}", format(80, module.to_doc()));
//     } else {
//         println!("ERROR");
//     }
// }
