use core::{any::TypeId, marker::PhantomData};

use crate::{
    lt::{Actual, LtSignature, TypeFn, TypeFnOf, TypeFnOfSized, TypeFnOutput},
    LifetimeHkt, Lt, WithLt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TagId(TypeId);

impl TagId {
    pub fn of<Tag: TypeTag>() -> Self {
        Tag::_tag_id(PreventOverridingTagId)
    }
}

pub struct PreventOverridingTagId;

pub trait TypeTag: 'static {
    type Arg: 'static;
    type Def: 'static;

    #[doc(hidden)]
    fn _tag_id(_: PreventOverridingTagId) -> TagId {
        TagId(TypeId::of::<Self>())
    }
}

pub trait TagFor<LTail: Lt>: TypeTag<Arg = Self::ArgTypeFn, Def = Self::TypeFn> {
    type ArgTypeFn: for<'x> TypeFnOf<LTail, Output<'x> = Self::ArgValue<'x>>;
    type TypeFn: for<'x> TypeFnOf<LTail, Output<'x> = Self::Value<'x>>;

    type ArgValue<'x>;
    type Value<'x>;
}

impl<Tag, LTail> TagFor<LTail> for Tag
where
    Tag: TypeTag,
    Tag::Arg: TypeFnOfSized<LTail>,
    Tag::Def: TypeFnOfSized<LTail>,
    LTail: Lt,
{
    type ArgTypeFn = Tag::Arg;
    type TypeFn = Tag::Def;

    type ArgValue<'x> = TypeFnOutput<'x, Tag::Arg, LTail>;
    type Value<'x> = TypeFnOutput<'x, Tag::Def, LTail>;
}

impl<L: Lt> WithLt<L> for () {
    type Value = ();
}

impl TypeTag for () {
    type Arg = ();
    type Def = ();
}

pub struct Value<T: ?Sized>(PhantomData<T>);

impl<T: ?Sized + 'static> TypeTag for Value<T> {
    type Arg = ();
    type Def = crate::TypeFn![T];
}

pub struct Ref<Tag>(PhantomData<Tag>);

impl<Tag, S> TypeFn<S> for Ref<Tag>
where
    Tag: TypeFn<S>,
    S: LtSignature,
{
    type OutputOver<L: Lt<Signature = S>> =
        LifetimeHkt![for<'x> &'x Actual<'x, <Tag as TypeFn<S>>::OutputOver<L>>];
}

impl<Tag: TypeTag> TypeTag for Ref<Tag> {
    type Arg = Tag::Arg;
    type Def = Ref<Tag::Def>;
}

pub struct Mut<Tag>(PhantomData<Tag>);

impl<Tag, S> TypeFn<S> for Mut<Tag>
where
    Tag: TypeFn<S>,
    S: LtSignature,
{
    type OutputOver<L: Lt<Signature = S>> = LifetimeHkt![for<'x> &'x mut TypeFnOutput<'x, Tag, L>];
}

impl<Tag: TypeTag> TypeTag for Mut<Tag> {
    type Arg = Tag::Arg;
    type Def = Mut<Tag::Def>;
}
