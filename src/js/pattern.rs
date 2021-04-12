use super::*;

// pub fn pattern<'a>(p: &'a TypedPattern) -> Document<'a> {
//     let mut checks = vec![];
//     Document::String("let ".to_string()).append(match p {
//         // TODO could this be called Nil list or simthing similar
//         Pattern::Nil { .. } => {
//             checks.push("gleam$tmp.length === 0".to_doc());
//             "[]".to_doc()
//         },
//         Pattern::Var { name, .. } => name.to_doc(),
//         // In erl/pattern.rs
//         // Uses collect_cons, but for a pattern type
//         _ => {
//             println!("p: {:?}", p);
//             unimplemented!("pattern")
//         }
//     })
// }