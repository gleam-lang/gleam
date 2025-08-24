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
  lineNumbers @6 :LineNumbers;
  srcPath @7 :Text;
  isInternal @8 :Bool;
  requiredVersion @9 :Version;
  typeAliases @10 :List(Property(TypeAliasConstructor));
  documentation @11 :List(Text);
  containsEcho @12 :Bool;
  references @13 :References;
}

struct References {
  importedModules @0 :List(Text);
  valueReferences @1 :List(ReferenceMap);
  typeReferences @2 :List(ReferenceMap);
}

struct ReferenceMap {
  module @0 :Text;
  name @1 :Text;
  references @2 :List(Reference);
}

struct Reference {
  location @0 :SrcSpan;
  kind @1 :ReferenceKind;
}

struct ReferenceKind {
  union {
    qualified @0 :Void;
    unqualified @1 :Void;
    import @2 :Void;
    definition @3 :Void;
    alias @4 :Void;
  }
}

struct TypeAliasConstructor {
    publicity @0 :Publicity;
    module @1 :Text;
    type @2 :Type;
    arity @3 :UInt32;
    deprecation @4 :Text;
    documentation @5 :Text;
    origin @6 :SrcSpan;
}

struct Version {
  major @0 :UInt32;
  minor @1 :UInt32;
  patch @2 :UInt32;
}

struct TypesVariantConstructors {
  variants @0 :List(TypeValueConstructor);
  typeParametersIds @1 :List(UInt16);
  opaque @2 :Bool;
}

struct TypeValueConstructor {
  name @0 :Text;
  parameters @1 :List(TypeValueConstructorParameter);
  documentation @2 :Text;
}

struct TypeValueConstructorParameter {
  type @0 :Type;
  label @1 :Text;
  documentation @2 :Text;
}

struct TypeConstructor {
  type @0 :Type;
  # TODO: convert this to an int as we only need to reconstruct type vars,
  # not other types
  # TODO: test
  parameters @1 :List(Type);
  module @2 :Text;
  publicity @3 :Publicity;
  deprecated @4 :Text;
  origin @5 :SrcSpan;
  documentation @6 :Text;
}

struct AccessorsMap {
  type @0 :Type;
  sharedAccessors @1 :List(Property(RecordAccessor));
  variantSpecificAccessors @2 :List(VariantSpecificAccessors);
  publicity @3 :Publicity;
}

struct VariantSpecificAccessors {
  accessors @0 :List(Property(RecordAccessor));
}

struct RecordAccessor {
  type @0 :Type;
  index @1 :UInt16;
  label @2 :Text;
  documentation @3 :Text;
}

# UInt16 cannot be used as a generic parameter to Option,
# so we need to create a custom type for this.
struct InferredVariant {
  union {
    unknown @0 :Void;
    inferred @1 :UInt16;
  }
}

struct Type {
  union {
    app :group {
      name @0 :Text;
      module @1 :Text;
      parameters @2 :List(Type);
      package @7 :Text;
      inferredVariant @8 :InferredVariant;
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
  publicity @2 :Publicity;
  deprecated @3 :Text;
}

struct Publicity {
  union {
    public @0 :Void;
    private @1 :Void;
    internal @2 :Option(SrcSpan);
  }
}

struct Implementations {
  gleam @0 :Bool;
  usesErlangExternals @1 :Bool;
  usesJavascriptExternals @2 :Bool;
  canRunOnErlang @3 :Bool;
  canRunOnJavascript @4 :Bool;
}

struct ValueConstructorVariant {
  union {
    moduleConstant :group {
      literal @0 :Constant;
      location @1 :SrcSpan;
      module @2 :Text;
      name @22 :Text;
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
      externalErlang @20 :Option(External);
      externalJavascript @21 :Option(External);
      purity @23 :Purity;
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

struct Purity {
  union {
    pure @0 :Void;
    trustedPure @1 :Void;
    impure @2 :Void;
    unknown @3 :Void;
  }
}

struct External {
  module @0 :Text;
  function @1 :Text;
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
      type @8 :Type;
    }

    bitArray @9 :List(BitArraySegment);

    var :group {
      module @10 :Text;
      name @11 :Text;
      type @12 :Type;
      constructor @13 :ValueConstructor;
    }

    stringConcatenation :group {
      left @14 :Constant;
      right @15 :Constant;
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

struct LineNumbers {
  lineStarts @0 :List(UInt32);
  length @1 :UInt32;
  mapping @2 :List(Character);
}

struct Character {
  byteIndex @0 :UInt64;
  lengthUtf8 @1 :UInt8;
  lengthUtf16 @2 :UInt8;
}
