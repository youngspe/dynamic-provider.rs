use core::convert::Infallible;

use crate::{
    lt::{HktWrapper, LifetimeHkt},
    Lt,
};

impl<H: ?Sized> TypeFn for HktWrapper<H>
where
    Self: for<'y> LifetimeHkt<Actual<'y>: TypeFn>,
{
    type Output<L: Lt> = <L::NextTypeFn<Self> as TypeFn>::Output<L::NextExtending>;
}

/// Represents a type that is parameterized over one or more lifetime variables.
///
/// When a type implementing `TypeFn` is needed, it is usually best to use [`TypeFn!`][trait@TypeFn]
/// rather than manually implement `TypeFn`.
///
/// ## Note
///
/// Types implementing this trait are generally meant to be used in generic parameters but not instantiated as values.
pub trait TypeFn: Sized {
    type Output<L: Lt>: ?Sized;
}

impl TypeFn for () {
    type Output<L: Lt> = Self;
}

impl TypeFn for Infallible {
    type Output<L: Lt> = Self;
}

/// Evaluates to a [`TypeFn`] implementation.
///
/// ## Usage
///
/// ```ignore
/// TypeFn![for<'x, 'y, 'z> (Cow<'x, str>, &'y str, &'z mut [u8])]
/// ```
#[macro_export]
macro_rules! TypeFn {
    (for <> $T:ty) => { $crate::Value<$T> };
    (for <$lt0:lifetime $(,$lt:lifetime)* $(,)?> $T:ty) => {
        $crate::LifetimeHkt![for<$lt0> $crate::TypeFn![for <$($lt),*> $T]]
    };
    ($T:ty) => { $crate::Value<$T> };
}
