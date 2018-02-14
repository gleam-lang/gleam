module Foreign

export Foreign, new/1

/// Foreign data is data that we don't know the type of yet.
/// We likely get data like this from interop with Erlang, or from
/// IO with the outside world.
///
foreign type Foreign

/// Convert any Gleam data into Foreign data.
///
foreign new :gleam_native :identity :: a -> Foreign
