use core::{any::TypeId, marker::PhantomData};

use crate::{
    lt::{LtSignature, TypeFn, TypeFnOf, TypeFnOutput},
    LifetimeHkt, Lt,
};

/// A unique identifier for a specific [`TypeTag`]-implementing type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagId(TypeId);

impl TagId {
    /// Returns the Tag ID of type `Tag`.
    pub fn of<Tag: TypeTag>() -> Self {
        Tag::_tag_id(PreventOverridingTagId)
    }
}

/// This type prevents custom [`TypeTag`] impls from overriding [`TypeTag::_tag_id()`] because it's not accessible by
/// name outside of the crate.
pub struct PreventOverridingTagId;

/// A marker type used as a key for requesting values from [`Provide`][crate::Provide].
///
/// ## Note
///
/// Types implementing this trait are generally meant to be used in generic parameters but not instantiated as values.
pub trait TypeTag: 'static {
    /// A [`TypeFn`] specifying the argument type of this tag.
    type Arg: 'static;
    /// A [`TypeFn`] specifying the resolved type of this tag.
    type Def: 'static;

    #[doc(hidden)]
    fn _tag_id(_: PreventOverridingTagId) -> TagId {
        TagId(TypeId::of::<Self>())
    }
}

/// A [`TypeTag`] that is defined over the given lifetime tail.
/// This should not be implemented directly; it will be auto-implemented for tag types where
/// [`TypeTag::Arg`] and [`TypeTag::Def`] implement [`TypeFnOf<LTail>`].
pub trait TagFor<LTail: Lt>: TypeTag<Arg = Self::ArgTypeFn, Def = Self::TypeFn> {
    #[doc(hidden)]
    type ArgTypeFn: for<'x> TypeFnOf<LTail, Output<'x> = Self::ArgValue<'x>>;
    #[doc(hidden)]
    type TypeFn: for<'x> TypeFnOf<LTail, Output<'x> = Self::Value<'x>>;

    /// The argument type, given the primary lifetime `'x`.
    type ArgValue<'x>;
    /// The output type, given the primary lifetime `'x`.
    type Value<'x>;
}

impl<Tag, LTail> TagFor<LTail> for Tag
where
    Tag: TypeTag,
    for<'x> Tag::Arg: TypeFnOf<LTail, Output<'x>: Sized>,
    for<'x> Tag::Def: TypeFnOf<LTail, Output<'x>: Sized>,
    LTail: Lt,
{
    type ArgTypeFn = Tag::Arg;
    type TypeFn = Tag::Def;

    type ArgValue<'x> = TypeFnOutput<'x, Tag::Arg, LTail>;
    type Value<'x> = TypeFnOutput<'x, Tag::Def, LTail>;
}

impl TypeTag for () {
    type Arg = ();
    type Def = ();
}

/// A [`TypeTag`] that corresponds to a value of type `T` that contains no borrowed data (i.e. `T: 'static`).
pub struct Value<T: ?Sized>(PhantomData<T>);

impl<T: ?Sized + 'static> TypeTag for Value<T> {
    type Arg = ();
    type Def = crate::TypeFn![T];
}

/// A [`TypeTag`] that corresponds to a shared reference to the output value of `Tag`.
///
/// For example, <code>Ref<[Value<T\>]></code> corresponds to `&'x T`, where `'x` is the primary
/// lifetime parameter.
/// This type can be similarly used to operate on [`TypeFn`] implementations.
pub struct Ref<Tag>(PhantomData<Tag>);

impl<F, S> TypeFn<S> for Ref<F>
where
    F: TypeFn<S>,
    S: LtSignature,
{
    type OutputOver<L: Lt<Signature = S>> = LifetimeHkt![for<'x> &'x TypeFnOutput<'x, F, L>];
}

impl<Tag: TypeTag> TypeTag for Ref<Tag> {
    type Arg = Tag::Arg;
    type Def = Ref<Tag::Def>;
}

/// A [`TypeTag`] that corresponds to a unique reference to the output value of `Tag`.
///
/// For example, <code>Mut<[Value<T\>]></code> corresponds to `&'x mut T`, where `'x` is the primary
/// lifetime parameter.
/// This type can be similarly used to operate on [`TypeFn`] implementations.
pub struct Mut<Tag>(PhantomData<Tag>);

impl<F, S> TypeFn<S> for Mut<F>
where
    F: TypeFn<S>,
    S: LtSignature,
{
    type OutputOver<L: Lt<Signature = S>> = LifetimeHkt![for<'x> &'x mut TypeFnOutput<'x, F, L>];
}

impl<Tag: TypeTag> TypeTag for Mut<Tag> {
    type Arg = Tag::Arg;
    type Def = Mut<Tag::Def>;
}
