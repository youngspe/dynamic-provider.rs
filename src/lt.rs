use core::marker::PhantomData;

use crate::{tag::TypeTag, utils::Implies, LifetimeHkt};

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

pub trait Lt: sealed::Lt {
    type Next: Lt<Signature = <Self::Signature as LtSignature>::Next>;
    type Signature: LtSignature;
    type TypeFnOutput<F: TypeFn<<Self::Next as Lt>::Signature>>: ?Sized;
}

pub trait LtSignature: 'static {
    type Next: LtSignature;
    type As<'x>: Lt<Signature = Self> + 'x;
}

pub trait WithLt<L: Lt> {
    type Value: ?Sized;
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

impl<T: ?Sized> WithLt<()> for Final<T> {
    type Value = T;
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

impl<'lt, L, F: ?Sized, T: ?Sized> WithLt<(Lifetime<'lt>, L)> for HktWrapper<F>
where
    L: Lt,
    F: for<'x> _LifetimeHktOf<'x>,
    <F as _LifetimeHktOf<'lt>>::T: WithLt<L, Value = T>,
{
    type Value = T;
}

pub trait TypeFn<STail: LtSignature>: Sized {
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

pub trait TypeFnOfSized<LTail: Lt>: for<'x> TypeFnOf<LTail, Output<'x>: Sized> {}

pub type TypeFnOutput<'x, F, LTail = ()> = <F as TypeFnOf<LTail>>::Output<'x>;

impl<F, LTail> TypeFnOf<LTail> for F
where
    LTail: Lt,
    F: TypeFn<LTail::Signature>,
{
    type Output<'x> = Actual<'x, F::OutputOver<LTail>>;
}

impl<F, LTail> TypeFnOfSized<LTail> for F
where
    LTail: Lt,
    F: TypeFn<LTail::Signature>,
    for<'x> Actual<'x, F::OutputOver<LTail>>: Sized,
{
}

#[macro_export]
macro_rules! Lt {
    () => { () };
    ($lt0:lifetime $(, $($rest:tt)*)?) => {
        $crate::lt::LtList::<$lt0, $crate::Lt![$($($rest)*)?]>
    };
    (..$Ty:ty) => { $Ty };
}

#[macro_export]
macro_rules! LifetimeHkt {
    (for <$lt:lifetime> $T:ty) => {
        $crate::lt::HktWrapper::<dyn for<$lt> $crate::lt::_LifetimeHktOf<$lt, T = $T>>
    };
}

#[macro_export]
macro_rules! TypeFn {
    (for <> $T:ty) => { $crate::lt::Final<$T> };
    (for <$lt0:lifetime, $($lt:lifetime),* $(,)?> $T:ty) => {
        $crate::LifetimeHkt![for<$lt0> $crate::TypeFn![for <$($lt),*> $T]]
    };
    ($T:ty) => { $crate::lt::Final::<$T> };
}
