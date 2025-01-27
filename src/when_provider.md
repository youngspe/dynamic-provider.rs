```rust
use dynamic_provider::{when_provider, ProvideRef};

dynamic_provider::define_tag! {
    tag Details: for<'x> (&'x str, &'x str);
}

fn get_details(provider: &dyn ProvideRef) -> String {
    when_provider(provider)
        .has_value::<String>(|s| s)
        .has_ref::<str>(|s| s.into())
        .has::<Details>((), |(a, b)| format!("{a}, {b}"))
        .or_else(|_| "unknown".into())
}
```
