use crate::assert_erl;

#[test]
fn unused_params() {
    assert_erl!(
        "
        fn foo() {
            let parent_subject = new_subject()
            let assert Ok(call_subject) = receive(parent_subject)
            let assert Ok(_) = try_call(call_subject, fn(subject) { #(1, subject) })
        }

        type Subject(message) {
            Subject(message)
        }

        fn new_subject() -> Subject(message) {
            todo
        }

        fn receive(subject: Subject(message)) -> Result(message, Nil) {
            todo
        }

        type CallError(msg) {
            CalleeDown(reason: String)
            CallTimeout
        }

        fn try_call(
            subject: Subject(request),
            make_request: fn(Subject(response)) -> request
        ) -> Result(response, CallError(response)) {
            todo
        }
        "
    );
}
