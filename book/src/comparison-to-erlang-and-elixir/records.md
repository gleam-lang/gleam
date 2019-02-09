# Records

Gleam's records are a collection of names and values where each value can be
of any type.

At runtime they are maps with atom keys. They are not the same as Erlang
records, which are tuples. They are similar to Elixir's structs, but do not
need to be declared prior to being used.

```
// Gleam
{
  name = "Rex",
  size = 40,
}
```
```
# Elixir
%{
  name: "Rex",
  size: 40
}
```
```
% Erlang
#{
  name => <<"Rex">>,
  size => 40
}.
```

Record values can be accessed using the `.` syntax, as in Elixir.

The Gleam compiler keeps tracks of what fields and values each record has and
will present a compile time error if you try to use a record field that does
not exist or has the incorrect type.

```
// Gleam
let pup = { name = "Rex", size = 40 }
let name = pup.name

pup.huh  // Compile time error
```
```
# Elixir
pup = %{ name: "Rex", size: 40 }
name = pup.name

pup.huh  # Runtime error
```
```
% Erlang
Pup = #{name => <<"Rex">>, size => 40}.
#{name => Name} = Pup.

#{huh => Huh} = Pup.  % Runtime error
```

A new record with updated or additional fields can be created using the update
syntax.

The new/updated record fields do not have to have the same types as the
original fields.

```
// Gleam
let pup = { name = "Rex", size = 40 }
let dog = { pup | size = 70, playful = True }

pup  // { name = "Rex", size = 40 }
dog  // { name = "Rex", size = 70, playful = True }
```
```
# Elixir
pup = %{ name: "Rex", size: 40 }
dog = %{ pup | size: 70, playful: true }

pup  # { name: "Rex", size: 40 }
dog  # { name: "Rex", size: 70, playful: true }
```
```
% Erlang
Pup = #{name => <<"Rex">>, size => 40},
Dog = Pup#{size => 70, playful => True}.

Pup.  % #{name => <<"Rex">>, size => 40}
Dog.  % #{name => <<"Rex">>, size => 70, playful => true}
```
