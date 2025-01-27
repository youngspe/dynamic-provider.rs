Dynamically request arbitrarily-typed values from providers with borrowed data.

## Motivation

Rust's `Any` trait allows for downcasting to access the underlying type of a `dyn` object. Unfortunately, this is limited to `'static` types (types that contain no borrowed data) because there's no way to distinguish types with different lifetimes by their `TypeId`s.

This crate provides the dynamically-sized [`Query`] type with lifetime parameters and an internal tag indicating what type of value it's meant to hold,
potentially containing those lifetimes.

The [`Provide`] and [`ProvideRef`] traits supply values to queries, the latter being `dyn`-capable.

### Prior Art

- [Rust RFC 3192](https://rust-lang.github.io/rfcs/3192-dyno.html)
  - [implementation](https://github.com/nrc/provide-any) by [@nrc](https://github.com/nrc)
- [supply](https://crates.io/crates/supply) by [@konnorandrews](https://github.com/konnorandrews)

## Concepts

<details open id="lifetime-lists"><summary>Lifetime lists</summary>

Lists of lifetime variables are represented by types implementing [`Lt`].
You can describe a lifetime list with the [`Lt!`] macro:

```rust
use dynamic_provider::Lt;

/// Lifetime list for retrieving values from `'data` with an argument that
/// borrows `'input`.
type CustomLifetimes<'data, 'input> = Lt!['data, 'input];

/// Prepend a `'local` lifetime to lifetime list `L`.
type PrependLifetime<'local, L> = Lt!['local, ..L];
```

</details>
<details open id="type-functions"><summary>Type functions</summary>

[`TypeFn`] implementations describe a type that is parameterized over an arbitrary
lifetime list.

A simple type function can be described with the [`TypeFn!`] macro:

```rust
type RefPair<A, B> = dynamic_provider::TypeFn![for<'a, 'b> (&'a A, &'b B)];
```

</details>
<details open id="resource-tags"><summary>Resource tags</summary>

[`ResourceTag`] implementations describe how values may be provided to a [`Query`].
The trait has two associated [`TypeFn`]s: `Arg`, which determines what values are needed to request the resource,
and `Out`, which defines the type of the resource value.

The [`define_tag!`] macro provides a convenient way to declare resource tags:

```rust
dynamic_provider::define_tag! {
    // syntax:
    // [pub] tag NAME[<...GENERICS>]: [for<...LIFETIMES>] [ARG =>] OUT [where BOUNDS];
    pub tag Field<T>: for<'data, 'key> &'key str => &'data T where T: ?Sized;
    pub tag StatusCode: i32;
}
```

</details>
<details open id="providers"><summary>Providers</summary> <dl><dt>[`Provide<L>`][`Provide`]</dt>
<dd>

Supplies values with the lifetime variables in `L` to the [`Query`] object passed to the
[`provide()`][provide-method] method.
</dd><dt>[`ProvideRef<L>`][`ProvideRef`]</dt>
<dd>****

Supplies values from a reference to `Self` with the lifetime of the reference and the lifetime
variables in `L`.

A reference to a `Provide` implementation (say, `&'x impl ProvideRef<L>` or `&'x mut impl ProvideRef<L>`) automatically implements `Provide<'x, ..L>` for all `'x`.
</dd></dl>
</details>

## Examples

### Dynamic field access

```rust
use dynamic_provider::{define_tag, request, Lt, Provide, ProvideRef, Query};

define_tag! {
    /// Represents dynamic access to a field.
    ///
    /// The argument of type `&'key str` indicates the name of the field being
    /// accessed.
    pub tag Field<T: ?Sized>: for<'data, 'key> &'key str => &'data T;
}

struct MyData {
    foo: String,
    bar: String,
    baz: Vec<u8>,
}

impl<'key> ProvideRef<Lt!['key]> for MyData {
    fn provide_ref<'data>(&'data self, query: &mut Query<Lt!['data, 'key]>) {
        query
            .put_where::<Field<str>>(|key| *key == "foo", |_| &self.foo)
            .put_where::<Field<str>>(|key| *key == "bar", |_| &self.bar)
            .put_where::<Field<[u8]>>(|key| *key == "baz", |_| &self.baz);
    }
}

/// Dynamically gets a reference to the field with name `key` and type `T`, if
/// provided.
fn get_field<'data, T: ?Sized + 'static>(
    data: &'data dyn for<'key> ProvideRef<Lt!['key]>,
    key: &str,
) -> Option<&'data T> {
    request! { from data;
        tag x: Field<T> where arg <- key => Some(x),
        else => None,
    }
}

// Retrieve fields from a value of type MyData:
let data = MyData {
    foo: "foo!".into(),
    bar: "bar!".into(),
    baz: b"baz!".into(),
};

assert_eq!(get_field::<str>(&data, "foo").unwrap(), "foo!");
assert_eq!(get_field::<str>(&data, "bar").unwrap(), "bar!");
assert_eq!(get_field::<[u8]>(&data, "baz").unwrap(), b"baz!");

// Use () when you need an empty provider:
assert!(get_field::<str>(&(), "foo").is_none());
```

## Features

### `"alloc"`

Adds the [`ProvideBox`] trait.
Adds trait implementations for `Rc`, `Box`, and `String`,
and enables additional provided values for `std` types.

[`define_tag`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/macro.define_tag.html
[provide-method]: #providers
[`Lt`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/trait.Lt.html
[`Lt!`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/macro.Lt.html
[`Provide`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/struct.Query.html
[`ProvideBox`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/trait.ProvideBox.html
[`ProvideRef`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/trait.ProvideRef.html
[`Query`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/struct.Query.html
[`ResourceTag`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/trait.ResourceTag.html
[`TypeFn`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/trait.TypeFn.html
[`TypeFn!`]: https://docs.rs/dynamic-provider/latest/dynamic_provider/macro.TypeFn.html
