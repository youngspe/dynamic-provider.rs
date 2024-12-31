use core::{any::TypeId, mem};

use crate::{
    lt::{Lt, WithLt},
    tag::{Mut, Ref, TagFor, TagId, Value},
};

#[derive(Default)]
enum QueryState<T, Arg> {
    #[default]
    Invalid,
    Value(T),
    Arg(Arg),
}

#[repr(transparent)]
pub struct TypedQuery<T, Arg> {
    inner: Query<QueryState<T, Arg>>,
}

impl<T, Arg> TypedQuery<T, Arg> {
    fn fulfill(&mut self, value: T) {
        if let QueryState::Arg { .. } = self.inner.state {
            self.inner.state = QueryState::Value(value);
            self.inner.tag_id = None;
        }
    }

    fn fulfill_with(&mut self, value: impl FnOnce(Arg) -> T) {
        if let QueryState::Arg(arg) = mem::take(&mut self.inner.state) {
            self.inner.state = QueryState::Value(value(arg));
            self.inner.tag_id = None;
        }
    }
}

pub unsafe trait ErasedQuery<'x, L: Lt> {}

unsafe impl<L, T, Arg> ErasedQuery<'_, L> for QueryState<T, Arg> where L: Lt {}

pub struct Query<Q: ?Sized> {
    pub tag_id: Option<TagId>,
    pub state: Q,
}

impl<'data, T, Arg> Query<QueryState<T, Arg>> {
    fn new<'x, Tag, L>(arg: Arg) -> Self
    where
        Tag: TagFor<L, Value<'x> = T, ArgValue<'x> = Arg>,
        L: Lt,
    {
        Self {
            tag_id: Some(TagId::of::<Tag>()),
            state: QueryState::Arg(arg),
        }
    }
}

pub type DynQuery<'data, 'x, LTail> = Query<dyn ErasedQuery<'x, LTail> + 'data>;

impl<'data, 'x, LTail: Lt> DynQuery<'data, 'x, LTail> {
    pub fn downcast<'y, Tag: TagFor<LTail>>(
        &mut self,
    ) -> Option<&mut TypedQuery<Tag::Value<'y>, Tag::ArgValue<'y>>> {
        if self.tag_id == Some(TagId::of::<Tag>()) {
            // SAFETY: `Tag` is the same type used to create this query, so the underlying type should be
            // TypedQuery<Tag::Value, Tag::ArgValue>
            let query = unsafe {
                &mut *(self as *mut Self as *mut TypedQuery<Tag::Value<'y>, Tag::ArgValue<'y>>)
            };

            return Some(query);
        }

        None
    }

    pub fn is_fulfilled(&self) -> bool {
        self.tag_id.is_some()
    }

    pub fn expects<Tag: TagFor<LTail>>(&self) -> bool {
        self.tag_id == Some(TagId::of::<Tag>())
    }

    pub fn put<Tag: TagFor<LTail>>(&mut self, value: Tag::Value<'x>) -> &mut Self {
        if let Some(state) = self.downcast::<Tag>() {
            state.fulfill(value)
        }

        self
    }

    pub fn put_with<Tag: TagFor<LTail>>(
        &mut self,
        value: impl FnOnce(Tag::ArgValue<'x>) -> Tag::Value<'x>,
    ) -> &mut Self {
        if let Some(state) = self.downcast::<Tag>() {
            state.fulfill_with(value);
        }

        self
    }

    pub fn put_value<T: 'static>(&mut self, value: T) -> &mut Self {
        self.put::<Value<T>>(value)
    }
    pub fn put_value_with<T: 'static>(&mut self, value: impl FnOnce() -> T) -> &mut Self {
        self.put::<Value<T>>(value())
    }

    pub fn put_ref<T: ?Sized + 'static>(&mut self, value: &'x T) -> &mut Self {
        self.put::<Ref<Value<T>>>(value)
    }

    pub fn put_mut<T: ?Sized + 'static>(&mut self, value: &'x mut T) -> &mut Self {
        self.put::<Mut<Value<T>>>(value)
    }

    pub fn using<T>(&mut self, value: T) -> QueryUsing<'_, 'x, T, LTail> {
        QueryUsing {
            value: Some(value),
            query: self,
        }
    }
}

pub fn with_query<'x, Tag: TagFor<LTail>, LTail: Lt, R>(
    f: impl FnOnce(&mut DynQuery<'_, 'x, LTail>) -> R,
    arg: Tag::ArgValue<'x>,
) -> (R, Option<Tag::Value<'x>>)
where {
    let mut query = Query::new::<Tag, LTail>(arg);
    let out = f(&mut query as _);

    let value = match query.state {
        QueryState::Value(value) => Some(value),
        _ => None,
    };

    (out, value)
}

pub struct QueryUsing<'q, 'x, T, L: Lt> {
    value: Option<T>,
    query: &'q mut DynQuery<'q, 'x, L>,
}

impl<'x, V, L: Lt> QueryUsing<'_, 'x, V, L> {
    pub fn finish(self) -> Option<V> {
        self.value
    }

    fn put_inner<Tag: TagFor<L>>(
        &mut self,
        f: impl FnOnce(V, Tag::ArgValue<'x>) -> Tag::Value<'x>,
    ) {
        let Some(state) = self.query.downcast::<Tag>() else {
            return;
        };

        let Some(value) = self.value.take() else {
            return;
        };

        state.fulfill_with(|arg| f(value, arg));
    }

    pub fn put<Tag: TagFor<L>>(mut self, f: impl FnOnce(V) -> Tag::Value<'x>) -> Self {
        if self.value.is_some() {
            self.put_inner::<Tag>(|v, _| f(v));
        }

        self
    }

    pub fn put_with_arg<Tag: TagFor<L>>(
        mut self,
        f: impl FnOnce(V, Tag::ArgValue<'x>) -> Tag::Value<'x>,
    ) -> Self {
        if self.value.is_some() {
            self.put_inner::<Tag>(f);
        }

        self
    }

    pub fn put_value<T: 'static>(self, f: impl FnOnce(V) -> T) -> Self {
        self.put::<Value<T>>(f)
    }

    pub fn put_ref<T: 'static + ?Sized>(self, f: impl FnOnce(V) -> &'x T) -> Self
where {
        self.put::<Ref<Value<T>>>(f)
    }

    pub fn put_mut<T: 'static + ?Sized>(self, f: impl FnOnce(V) -> &'x mut T) -> Self
where {
        self.put::<Mut<Value<T>>>(f)
    }
}
