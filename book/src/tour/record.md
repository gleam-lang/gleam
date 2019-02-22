# Records

Gleam's records are a collection of names and values where each value can be
of any type.

```rust,noplaypen
{
  name = "Rex",
  size = 40,
}
```


## Accessing field values

Record values can be accessed using the `record.field_name` syntax.

```rust,noplaypen
let pup = { name = "Rex", size = 40 }
let name = pup.name

name  // => "Rex"
```


## Inserting field values

A new record with updated or additional fields can be created using the
`{ record | field_name = value }` update syntax.

The updated record fields do not have to have the same types as the original
fields.

```rust,noplaypen
let pup = { name = "Rex", size = 40 }
let dog = { pup | size = 70, playful = True }

pup  // { name = "Rex", size = 40 }
dog  // { name = "Rex", size = 70, playful = True }
```


## Types

The type of a record depends upon the names and types of its fields.

```rust,noplaypen
{ name = "Rex" }             // Type { name = String }
{ name = "Rex", size = 40 }  // Type { name = String, size = Int }
```

The Gleam compiler keeps tracks of what fields and values each record has and
will present a compile time error if you try to use a record field that does
not exist or has the incorrect type.

```rust,noplaypen
let pup = { name = "Rex", size = 40 }

pup.address   // Compile time error! Unknown field
pup.name + 1  // Compile time error! Wrong type
```


### Parameterised record fields

Gleam's type system aims to be as permissive when it comes to records passed
to functions. Take this function for example.

```rust,noplaypen
fn get_following_year(record) {
  record.year + 1
}
```

The type of this function is `fn({ a | year = Int }) -> Int`.

The `a |` in `{ a | year = Int}` means "any other fields", so this function
can be called with any record so long as the record has a `year` field that
has an `Int` value.

```rust,noplaypen
let date = { day = 5, month: 1, year = 2019 }
let book = { title = "Sabriel", year = 1995 }
let soup = { kind = "Tomato", spicy = False }
let wine = { kind = "Fancy!", year = "Good" }

get_following_year(date)  // => 2020
get_following_year(book)  // => 1996
get_following_year(soup)  // Compile time error! No year field
get_following_year(wine)  // Compile time error! Wrong field type
```


## Erlang interop

At runtime records are maps with atom keys. They are not the same as Erlang
records, which are tuples. They are similar to Elixir's structs, but do not
need to be declared prior to being used.

