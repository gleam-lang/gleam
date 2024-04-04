@0xb533a99cfdbcedbe;

# This Cap'n Proto schema is compiled into Rust code for use in the compiler.
#
# We don't want the compiler build to depend on the Cap'n Proto compiler so 
# the Cap'n Proto to Rust build step is commented out in `build.rs`.
#
# This schema is not considered a stable API and may change at any time.

struct Property(Value) {
  key @0 :Text;
  value @1 :Value;
}

struct Option(Value) {
  union {
    none @0 :Void;
    some @1 :Value;
  }
}

struct Module {
  name @0 :Text;
  types @1 :List(Property(TypeConstructor));
  values @2 :List(Property(ValueConstructor));
  accessors @3 :List(Property(AccessorsMap));
  package @4 :Text;
  typesConstructors @5 :List(Property(TypesVariantConstructors));
  unusedImports @6 :List(SrcSpan);
  containsTodo @7 :Bool;
}

struct TypesVariantConstructors {
  variants @0 :List(TypeValueConstructor);
  typeParametersIds @1 :List(UInt16);
}

struct TypeValueConstructor {
  name @0 :Text;
  parameters @1 :List(TypeValueConstructorParameter);
}

struct TypeValueConstructorParameter {
  type @0 :Type;
}

struct TypeConstructor {
  type @0 :Type;
  # TODO: convert this to an int as we only need to reconstruct type vars, 
  # not other types
  # TODO: test
  parameters @1 :List(Type); 
  module @2 :Text;
  public @3 :Bool;
  deprecated @4 :Text;
}

struct AccessorsMap {
  type @0 :Type;
  accessors @1 :List(Property(RecordAccessor));
}

struct RecordAccessor {
  type @0 :Type;
  index @1 :UInt16;
  label @2 :Text;
}

struct Type {
  union {
    app :group {
      name @0 :Text;
      module @1 :Text;
      parameters @2 :List(Type);
      package @7 :Text;
    }

    fn :group {
      arguments @3 :List(Type);
      return @4 :Type;
    }

    var :group {
      id @5 :UInt64;
    }

    tuple :group {
      elements @6 :List(Type);
    }
  }
}

struct ValueConstructor {
  type @0 :Type;
  variant @1 :ValueConstructorVariant;
  public @2 :Bool;
  deprecated @3 :Text;
}

struct Implementations {
  gleam @0 :Bool;
  erlang @1 :Bool;
  javascript @2 :Bool;
}

struct ValueConstructorVariant {
  union {
    moduleConstant :group {
      literal @0 :Constant;
      location @1 :SrcSpan;
      module @2 :Text;
      documentation @14 :Text;
      implementations @19 :Implementations;
    }

    moduleFn :group {
      name @3 :Text;
      fieldMap @4 :Option(FieldMap);
      module @5 :Text;
      arity @6 :UInt16;
      location @7 :SrcSpan;
      documentation @15 :Text;
      implementations @18 :Implementations;
    }

    record :group {
      name @8 :Text;
      arity @9 :UInt16;
      fieldMap @10 :Option(FieldMap);
      location @11 :SrcSpan;
      module @12 :Text;
      constructorsCount @13 :UInt16;
      documentation @16 :Text;
      constructorIndex @17 :UInt16;
    }
  }
}

struct SrcSpan {
  start @0 :UInt32;
  end @1 :UInt32;
}

# Cap'n Proto only permits pointer types to be used as type parameters
struct BoxedUInt16 {
  value @0 :UInt16;
}

# Cap'n Proto only permits pointer types to be used as type parameters
struct BoxedUInt32 {
  value @0 :UInt32;
}

struct FieldMap {
  arity @0 :UInt32;
  fields @1 :List(Property(BoxedUInt32));
}

struct Constant {
  union {
    int @0 :Text;
    float @1 :Text;
    string @2 :Text;
    tuple @3 :List(Constant);

    list :group {
      elements @4 :List(Constant);
      type @5 :Type;
    }

    record :group {
      args @6 :List(Constant);
      tag @7 :Text;
      typ @8 :Type;
    }

    bitArray @9 :List(BitArraySegment);

    var :group {
      module @10 :Text;
      name @11 :Text;
      typ @12 :Type;
      constructor @13 :ValueConstructor;
    }
  }
}

struct BitArraySegment {
  value @0 :Constant;
  options @1 :List(BitArraySegmentOption);
  type @2 :Type;
}

struct BitArraySegmentOption {
  union {
    bytes @0 :Void;

    integer @1 :Void;

    float @2 :Void;

    bits @3 :Void;

    utf8 @4 :Void;

    utf16 @5 :Void;

    utf32 @6 :Void;

    utf8Codepoint @7 :Void;

    utf16Codepoint @8 :Void;

    utf32Codepoint @9 :Void;

    signed @10 :Void;

    unsigned @11 :Void;

    big @12 :Void;

    little @13 :Void;

    native @14 :Void;

    size :group {
      value @15 :Constant;
      shortForm @16 :Bool;
    }

    unit :group {
      value @17 :UInt8;
      shortForm @18 :Bool;
    }
  }
}
