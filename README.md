Dynamically request arbitrarily-typed values from providers.

## Lifetime lists

## Type functions

## Type tags

TODO: fix this example

```rust ignore
dynamic_provider::define_tag! {
    pub tag StatusCode: for<'x> i32;
    pub tag Field<T: ?Sized>: for<'x, 'key> &'key str => &'x T;
}
```

## Providers
