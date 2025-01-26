Dynamically request arbitrarily-typed values from providers.

## Concepts

<details open><summary>

### Lifetime lists
</summary>

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
<details open><summary>

### Type functions
</summary>

[`TypeFn`] implementations describe a type that is parameterized over an arbitrary
lifetime list.

A simple type function can be described with the [`TypeFn!`] macro:

```rust
type RefPair<A, B> = dynamic_provider::TypeFn![for<'a, 'b> (&'a A, &'b B)];
```

</details>
<details open><summary>

### Type tags
</summary>

```rust
dynamic_provider::define_tag! {
    pub tag StatusCode: i32;
    pub tag Field<T: ?Sized>: for<'data, 'key> &'key str => &'data T;
}
```

</details>
<details open><summary>

### Providers
</summary>

<dl><dt>

[`Provide<L>`][`Provide`]
</dt><dd>

Supplies values with the lifetime variables in `L` to the [`Query`] object passed to the
[`provide()`][provide-method] method.
</dd><dt>

[`ProvideRef<L>`][`ProvideRef`]
</dt><dd>

Supplies values from a reference to `Self` with the lifetime of the reference and the lifetime
variables in `L`.
Each implementation automatically implements `Provider<'x, ..L>` for all `'x`.
</dd></dl>
</details>

## Example

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

[`TypeFn!`]: #type-functions
[`TypeFn`]: #type-functions
[`Lt!`]: #lifetime-lists
[`Lt`]: #lifetime-lists
[`Provide`]: #providers
[`ProvideRef`]: #providers
[provide-method]: #providers
[`Query`]: #

