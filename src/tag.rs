use core::{any::TypeId, convert::Infallible, marker::PhantomData};

use crate::{type_fn::TypeFn, LifetimeHkt, Lt};

/// A unique identifier for a specific [`ResourceTag`]-implementing type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagId(TypeId);

impl TagId {
    /// Returns the Tag ID of type `Tag`.
    pub fn of<Tag: ResourceTag>() -> Self {
        Tag::_tag_id(PreventOverridingTagId)
    }
}

/// This type prevents custom [`ResourceTag`] impls from overriding [`ResourceTag::_tag_id()`] because it's not accessible by
/// name outside of the crate.
pub struct PreventOverridingTagId;

/// A marker type used as a key for requesting values from [`Provide`][crate::Provide].
///
/// ## Note
///
/// Types implementing this trait are generally meant to be used in generic parameters but not instantiated as values.
pub trait ResourceTag: 'static {
    /// A [`TypeFn`] specifying the argument type of this tag.
    /// The argument contains any information needed to provide this resource.
    type Arg: TypeFn;
    /// A [`TypeFn`] specifying the resolved type of this tag.
    type Out: TypeFn;

    #[doc(hidden)]
    fn _tag_id(_: PreventOverridingTagId) -> TagId {
        TagId(TypeId::of::<Self>())
    }
}

/// A [`ResourceTag`] that is defined over the given lifetimes.
///
/// This should not be implemented directly; it will be automatically implemented.
pub trait TagFor<L: Lt>:
    ResourceTag<Arg: TypeFn<Output<L> = Self::ArgValue>, Out: TypeFn<Output<L> = Self::Value>>
{
    /// The argument type.
    type ArgValue;
    /// The output type.
    type Value;
}

impl<Tag, L, ArgValue, Value> TagFor<L> for Tag
where
    Tag: ResourceTag<Arg: TypeFn<Output<L> = ArgValue>, Out: TypeFn<Output<L> = Value>>,
    L: Lt,
{
    type ArgValue = ArgValue;
    type Value = Value;
}

impl ResourceTag for () {
    type Arg = ();
    type Out = ();
}

/// A [`TypeFn`] that corresponds to `T` for all lifetime arguments.
/// Can be used as a [`ResourceTag`] when `T` has a fixed size and contains no borrowed data
/// (i.e. `T: Sized + 'static`).
pub struct Value<T: ?Sized>(Infallible, PhantomData<T>);

impl<T: ?Sized> TypeFn for Value<T> {
    type Output<L: Lt> = T;
}

impl<T: ?Sized + 'static> ResourceTag for Value<T> {
    type Arg = ();
    type Out = crate::TypeFn![T];
}

/// A [`ResourceTag`] that corresponds to a shared reference to the output value of `Tag`.
/// Can also be used as an operator on [`TypeFn`] implementations.
///
/// For example: <code>Ref<[Value<T\>][Value]></code> implements the following for all `'x, T: 'static`:
///
/// ```ignore
/// TypeFn<Output<Lt!['x]> = &'x T> + ResourceTag + TagFor<Lt!['x], Value = &'x T>
/// ```
pub struct Ref<Tag>(Tag);

impl<F> TypeFn for Ref<F>
where
    F: TypeFn,
{
    type Output<L: Lt> = L::Actual<LifetimeHkt![for<'y> &'y F::Output<L>]>;
}

impl<Tag: ResourceTag> ResourceTag for Ref<Tag> {
    type Arg = Tag::Arg;
    type Out = Ref<Tag::Out>;
}

/// A [`ResourceTag`] that corresponds to a unique reference to the output value of `Tag`.
/// Can also be used as an operator on [`TypeFn`] implementations.
///
/// For example: <code>Mut<[Value<T\>][Value]></code> implements the following for all `'x, T: 'static`:
///
/// ```ignore
/// TypeFn<Output<Lt!['x]> = &'x mut T> + ResourceTag + TagFor<Lt!['x], Value = &'x mut T>
/// ```
pub struct Mut<Tag>(Tag);

impl<F> TypeFn for Mut<F>
where
    F: TypeFn,
{
    type Output<L: Lt> = L::Actual<LifetimeHkt![for<'y> &'y mut F::Output<L>]>;
}

impl<Tag: ResourceTag> ResourceTag for Mut<Tag> {
    type Arg = Tag::Arg;
    type Out = Mut<Tag::Out>;
}

/// A tag that is never meant to be fulfilled but has a valid [`TagId`].
pub(crate) struct MarkerTag<T: ?Sized> {
    _p: PhantomData<T>,
    _i: Infallible,
}

impl<T: ?Sized + 'static> ResourceTag for MarkerTag<T> {
    type Arg = ();
    type Out = Value<(Infallible, T)>;
}
