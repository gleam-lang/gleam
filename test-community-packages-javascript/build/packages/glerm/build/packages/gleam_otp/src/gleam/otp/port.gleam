/// Ports are how code running on the Erlang virtual machine interacts with
/// the outside world. Bytes of data can be sent to and read from ports,
/// providing a form of message passing to an external program or resource.
///
/// For more information on ports see the [Erlang ports documentation][1].
///
/// [1]: https://erlang.org/doc/reference_manual/ports.html
///
pub type Port
