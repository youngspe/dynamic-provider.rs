use core::marker::PhantomData;

use crate::{tag::TypeTag, LifetimeHkt};

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
pub type HktWrapper<L> = Invariant<L>;

#[doc(hidden)]
pub struct Final<T: ?Sized>(Invariant<T>);

pub type LtList<'x, L = ()> = (Lifetime<'x>, L);

/// Represents a list of zero or more lifetime variables
pub trait Lt: sealed::Lt {
    type Next: Lt<Signature = <Self::Signature as LtSignature>::Next>;
    type Signature: LtSignature;
    type TypeFnOutput<F: TypeFn<<Self::Next as Lt>::Signature>>: ?Sized;
}

pub trait LtSignature: 'static {
    type Next: LtSignature;
    type As<'x>: Lt<Signature = Self> + 'x;
}

impl sealed::Lt for () {}
impl<L> sealed::Lt for LtList<'_, L> where L: sealed::Lt {}

impl Lt for () {
    type Next = ();
    type Signature = ();
    type TypeFnOutput<F: TypeFn<Self::Signature>> = Actual<'static, F::OutputOver<()>>;
}

impl LtSignature for () {
    type Next = ();
    type As<'x> = ();
}

impl<'lt, L> Lt for LtList<'lt, L>
where
    L: Lt,
{
    type Next = L;
    type Signature = LtList<'static, L::Signature>;
    type TypeFnOutput<F: TypeFn<L::Signature>> = Actual<'lt, F::OutputOver<L>>;
}
impl<L> LtSignature for LtList<'static, L>
where
    L: LtSignature,
{
    type Next = L;
    type As<'x> = LtList<'x, L::As<'x>>;
}

impl<T: ?Sized> sealed::LifetimeHkt for Final<T> {}

impl<T: ?Sized + 'static> TypeTag for Final<T> {
    type Arg = ();
    type Def = Self;
}

impl<T: ?Sized + sealed::LifetimeHkt> sealed::LifetimeHkt for HktWrapper<T> {}

impl<'lt, F: ?Sized + 'static> TypeTag for HktWrapper<F>
where
    F: _LifetimeHktOf<'lt>,
    for<'x> <F as _LifetimeHktOf<'x>>::T: TypeTag,
{
    type Arg = ();
    type Def = Self;
}

/// Represents a type that is parameterized over one or more lifetime variables.
///
/// The lifetime parameters are broken into two parts:
/// the <dfn>primary lifetime</dfn> and the <dfn>lifetime tail</dfn>.
///
/// The primary lifetime parameter is implied for every `TypeFn` implementation,
/// but the lifetime tail is a [`LtList`] or [`LtSignature`] that must be explicitly specified.
///
/// For a more concrete example, `TypeFn![for<'x, 'y, 'z> Foo<'x, 'y, 'z>]` is a type that implements
/// ```ignore
/// for<'x, 'y, 'z> TypeFn<
///     Lt!['static, 'static],
///     OutputOver<Lt!['y, 'z]>: LifetimeHkt<Actual<'x> = Foo<'x, 'y, 'z>>,
/// >
/// ```
///
/// When a type implementing `TypeFn` is needed, it is usually best to use [`TypeFn!`].
///
/// A manual implementation of `TypeFn` may be necessary to operate on another implementation.
///
/// For example, [`Ref<F>`] represents a borrow of `F`'s output type over the primary lifetime parameter.
/// To work on any lifetime tail supported by `F` a manual implementation like this is necessary:
///
/// ```
/// use dynamic_provider::{LifetimeHkt, Lt, LtSignature, TypeFn, TypeFnOutput};
///
/// struct Ref<F>(F);
///
/// impl<F, S> TypeFn<S> for Ref<F>
/// where
///     F: TypeFn<S>,
///     S: LtSignature,
/// {
///     type OutputOver<L: Lt<Signature = S>> = LifetimeHkt![for<'x> &'x TypeFnOutput<'x, F, L>];
/// }
/// ```
///
/// ## Note
///
/// Types implementing this trait are generally meant to be used in generic parameters but not instantiated as values.
///
/// [`Ref<F>`]: crate::tag::Ref
pub trait TypeFn<STail: LtSignature>: Sized {
    /// A [`LifetimeHkt`] representing the output type as a function of the primary lifetime
    /// parameter given the lifetime tail `LTail`.
    ///
    /// [`LifetimeHkt`] is used here rather than a generic lifetime parameter because it supports
    /// implied lifetime bounds.
    type OutputOver<LTail: Lt<Signature = STail>>: LifetimeHkt;
}

impl<STail: LtSignature> TypeFn<STail> for () {
    type OutputOver<L: Lt<Signature = STail>> = LifetimeHkt![for<'x> ()];
}

impl<STail: LtSignature, T: ?Sized> TypeFn<STail> for Final<T> {
    type OutputOver<L: Lt<Signature = STail>> = LifetimeHkt![for<'x> T];
}

impl<H: ?Sized, STail> TypeFn<STail> for HktWrapper<H>
where
    STail: LtSignature,
    H: for<'x> _LifetimeHktOf<'x, T: TypeFn<STail::Next>>,
{
    type OutputOver<L: Lt<Signature = STail>> =
        LifetimeHkt![for<'lt> L::TypeFnOutput<<H as _LifetimeHktOf<'lt>>::T>];
}

pub trait TypeFnOf<LTail: Lt>:
    TypeFn<LTail::Signature, OutputOver<LTail>: for<'x> LifetimeHkt<Actual<'x> = Self::Output<'x>>>
{
    type Output<'x>: ?Sized;
}

/// The output of a [`TypeFn`] `F` with primary lifetime `'x` and lifetime tail `LTail`.
///
/// For example, this `TypeFn`
/// ```ignore
/// type MyTypeFn = TypeFn![for<'x, y, z> Foo<'x, 'y, 'z>];
/// ```
///
/// would satisfy the following:
/// ```ignore
/// TypeFnOutput<'x, MyTypeFn, Lt!['y, 'z]> == Foo<'x, 'y, 'z>;
/// ```
pub type TypeFnOutput<'x, F, LTail = ()> = <F as TypeFnOf<LTail>>::Output<'x>;

impl<F, LTail> TypeFnOf<LTail> for F
where
    LTail: Lt,
    F: TypeFn<LTail::Signature>,
{
    type Output<'x> = Actual<'x, F::OutputOver<LTail>>;
}

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
///
/// This macro can also be used to produce [`LtSignature`] implementations by using `'static` lifetimes:
/// ```ignore
/// Lt!['static, 'static, 'static]
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

/// Evaluates to a [`TypeFn`] implementation.
///
/// ## Usage
///
/// ```ignore
/// TypeFn![for<'x, 'y, 'z> (Cow<'x, str>, &'y str, &'z mut [u8])]
/// ```
#[macro_export]
macro_rules! TypeFn {
    (for <> $T:ty) => { $crate::lt::Final<$T> };
    (for <$lt0:lifetime $(,$lt:lifetime)* $(,)?> $T:ty) => {
        $crate::LifetimeHkt![for<$lt0> $crate::TypeFn![for <$($lt),*> $T]]
    };
    ($T:ty) => { $crate::lt::Final::<$T> };
}
