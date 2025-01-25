use core::{convert::Infallible, marker::PhantomData};

use crate::TypeFn;

mod sealed {
    pub trait LifetimeHkt {}
    pub trait Lt {}
}

#[doc(hidden)]
pub trait _LifetimeHktOf<'lt>: sealed::LifetimeHkt {
    type T: ?Sized;
}

pub trait LifetimeHkt {
    type Actual<'x>: ?Sized;
}

pub type Actual<'lt, H> = <H as LifetimeHkt>::Actual<'lt>;

impl<H> LifetimeHkt for HktWrapper<H>
where
    H: ?Sized + for<'x> _LifetimeHktOf<'x>,
{
    type Actual<'x> = <H as _LifetimeHktOf<'x>>::T;
}

#[doc(hidden)]
pub struct Invariant<T: ?Sized>(PhantomData<fn(PhantomData<T>) -> PhantomData<T>>);
#[doc(hidden)]
pub struct Lifetime<'lt>(Invariant<&'lt ()>);

#[doc(hidden)]
pub struct HktWrapper<L: ?Sized>(Infallible, Invariant<L>);

pub type LtList<'x, L = ()> = (Lifetime<'x>, L);

/// Represents a list of zero or more lifetime variables
pub trait Lt: sealed::Lt {
    type Next: Lt;
    type NextExtending: Lt;
    type Extend<'a>: Lt;
    type Actual<H: LifetimeHkt>: ?Sized;
    type NextTypeFn<F: for<'x> LifetimeHkt<Actual<'x>: TypeFn>>: TypeFn;
}

impl sealed::Lt for () {}
impl<L> sealed::Lt for LtList<'_, L> where L: sealed::Lt {}

impl Lt for () {
    type Next = ();
    type NextExtending = LtList<'static, Self>;
    type Extend<'a> = LtList<'a, Self>;
    type Actual<H: LifetimeHkt> = H::Actual<'static>;
    type NextTypeFn<F: for<'x> LifetimeHkt<Actual<'x>: TypeFn>> = Self::Actual<F>;
}

impl<'lt, L> Lt for LtList<'lt, L>
where
    L: Lt,
{
    type Next = L;
    type NextExtending = L::Extend<'lt>;
    type Extend<'a> = Self;
    type Actual<H: LifetimeHkt> = H::Actual<'lt>;
    type NextTypeFn<F: for<'x> LifetimeHkt<Actual<'x>: TypeFn>> = Self::Actual<F>;
}

impl<T: ?Sized + sealed::LifetimeHkt> sealed::LifetimeHkt for HktWrapper<T> {}

/// Evaluates to an [`Lt`] implementation with the given list of lifetime specifiers.
///
/// ## Usage
///
/// ```ignore
/// Lt!['a, 'b, 'c]
/// ```
///
/// Another lifetime list can be concatenated using `..`:
///
/// ```ignore
/// Lt!['a, 'b, 'c, ..L]
/// ```
#[macro_export]
macro_rules! Lt {
    () => { () };
    ($lt0:lifetime $(, $($rest:tt)*)?) => {
        $crate::lt::LtList::<$lt0, $crate::Lt![$($($rest)*)?]>
    };
    (..$Ty:ty) => { $Ty };
}

/// Evaluates to a [`LifetimeHkt`] implementation.
///
/// ## Usage
///
/// ```ignore
/// LifetimeHkt![for<'x> Cow<'x, str>]
/// ```
#[macro_export]
macro_rules! LifetimeHkt {
    (for <$lt:lifetime> $T:ty) => {
        $crate::lt::HktWrapper::<dyn for<$lt> $crate::lt::_LifetimeHktOf<$lt, T = $T>>
    };
}
