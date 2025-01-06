use core::{convert::Infallible, mem};

use crate::{
    lt::Lt,
    tag::{Mut, Ref, TagFor, TagId, Value},
};

#[derive(Default)]
enum QueryState<T, Arg> {
    #[default]
    Invalid,
    Value(T),
    Arg(Arg),
}

/// Result of successful [`Query::downcast()`] call.
#[repr(transparent)]
struct TypedQuery<T, Arg> {
    inner: QueryGeneric<QueryState<T, Arg>>,
}

impl<T, Arg> TypedQuery<T, Arg> {
    fn fulfill(&mut self, value: T) {
        if let QueryState::Arg { .. } = self.inner.state {
            self.inner.state = QueryState::Value(value);
            self.inner.tag_id = None;
        }
    }

    fn fulfill_with(&mut self, f: impl FnOnce(Arg) -> T) {
        self.try_fulfill_with::<Infallible>(|arg| Ok(f(arg)));
    }

    fn try_fulfill_with<R>(&mut self, f: impl FnOnce(Arg) -> Result<T, (Arg, R)>) -> Option<R> {
        let QueryState::Arg(arg) = mem::take(&mut self.inner.state) else {
            return None;
        };
        let out;
        (out, self.inner.state) = match f(arg) {
            Ok(value) => (None, QueryState::Value(value)),
            Err((arg, out)) => (Some(out), QueryState::Arg(arg)),
        };
        self.inner.tag_id = None;
        out
    }
}

/// ## Safety
/// It's assumed an implementor is `QueryState<Tag::Value<'x>, Tag::ArgValue<'x>>` for some `Arg: TagFor<L>`
/// and can be downcast back to the concrete type if `Tag` is known.
pub unsafe trait ErasedQueryState<'x, L: Lt> {}

unsafe impl<L, T, Arg> ErasedQueryState<'_, L> for QueryState<T, Arg> where L: Lt {}

/// Generic type meant for unsizing to [Query].
#[repr(C)]
pub struct QueryGeneric<Q: ?Sized> {
    /// Identifies the tag type with which `state` was created.
    /// When the query has been fulfilled, this will be set to `None` to indicate no future
    /// downcasts need be attempted.
    tag_id: Option<TagId>,
    /// Either a `QueryState` or a `dyn ErasedQueryState` holding the query's internal state
    /// (whether fulfilled or unfulfilled).
    state: Q,
}

impl<T, Arg> QueryGeneric<QueryState<T, Arg>> {
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

/// A type-erased query ready to pass to [`crate::Provide::provide()`].
///
/// Providers may use this type to supply tagged values.
pub type Query<'data, 'x, LTail = ()> = QueryGeneric<dyn ErasedQueryState<'x, LTail> + 'data>;

impl<'x, LTail: Lt> Query<'_, 'x, LTail> {
    fn downcast<Tag: TagFor<LTail>>(
        &mut self,
    ) -> Option<&mut TypedQuery<Tag::Value<'x>, Tag::ArgValue<'x>>> {
        if self.tag_id == Some(TagId::of::<Tag>()) {
            // SAFETY: `Tag` is the same type used to create this query, so the underlying type should be
            // TypedQuery<Tag::Value, Tag::ArgValue>
            let query = unsafe {
                &mut *(self as *mut Self as *mut TypedQuery<Tag::Value<'x>, Tag::ArgValue<'x>>)
            };

            return Some(query);
        }

        None
    }

    /// Returns `true` if the query has been fulfilled and no values will be accepted in the future.
    pub fn is_fulfilled(&self) -> bool {
        self.tag_id.is_none()
    }

    /// Returns `true` if this query would accept a value tagged with `Tag`.
    ///
    /// **Note**: this will return `false` if a value tagged with `Tag` _was_ expected and has been
    /// fulfilled, as it will not accept additional values.
    pub fn expects<Tag: TagFor<LTail>>(&self) -> bool {
        self.tag_id == Some(TagId::of::<Tag>())
    }

    /// Attempts to fulfill the query with a value marked with `Tag`.
    pub fn put<Tag: TagFor<LTail>>(&mut self, value: Tag::Value<'x>) -> &mut Self {
        if let Some(state) = self.downcast::<Tag>() {
            state.fulfill(value)
        }

        self
    }

    /// Attempts to fulfill the query with a function returning a value marked with `Tag`.
    ///
    /// The function will not be called if the query does not accept `Tag`.
    pub fn put_with<Tag: TagFor<LTail>>(
        &mut self,
        f: impl FnOnce(Tag::ArgValue<'x>) -> Tag::Value<'x>,
    ) -> &mut Self {
        if let Some(state) = self.downcast::<Tag>() {
            state.fulfill_with(f);
        }

        self
    }

    /// Behaves similarly to [`Self::put_with()`], except that the query will **not** be fulfilled
    /// if `predicate` returns `false`.
    ///
    /// The function will not be called if the query does not accept `Tag`.
    pub fn put_where<Tag: TagFor<LTail>>(
        &mut self,
        predicate: impl FnOnce(&mut Tag::ArgValue<'x>) -> bool,
        f: impl FnOnce(Tag::ArgValue<'x>) -> Tag::Value<'x>,
    ) -> &mut Self {
        self.try_put::<Tag>(|mut arg| {
            if predicate(&mut arg) {
                Ok(f(arg))
            } else {
                Err(arg)
            }
        })
    }

    /// Behaves similary to [`Self::put_with()`] when the function returns `Ok(_)`.
    /// When the function returns `Err(arg)`, the query will **not** be fulfilled.
    pub fn try_put<Tag: TagFor<LTail>>(
        &mut self,
        f: impl FnOnce(Tag::ArgValue<'x>) -> Result<Tag::Value<'x>, Tag::ArgValue<'x>>,
    ) -> &mut Self {
        if let Some(state) = self.downcast::<Tag>() {
            state.try_fulfill_with(|arg| f(arg).map_err(|e| (e, ())));
        }

        self
    }

    /// Attempts to fulfill the query with a `T` marked with [`Value<T>`].
    pub fn put_value<T: 'static>(&mut self, value: T) -> &mut Self {
        self.put::<Value<T>>(value)
    }

    /// Attempts to fulfill the query with a function returning a `T` marked with [`Value<T>`].
    pub fn put_value_with<T: 'static>(&mut self, value: impl FnOnce() -> T) -> &mut Self {
        self.put::<Value<T>>(value())
    }

    /// Attempts to fulfill the query with a `&'x T` marked with [`Ref<Value<T>>`].
    pub fn put_ref<T: ?Sized + 'static>(&mut self, value: &'x T) -> &mut Self {
        self.put::<Ref<Value<T>>>(value)
    }

    /// Attempts to fulfill the query with a `&'x mut T` marked with [`Mut<Value<T>>`].
    pub fn put_mut<T: ?Sized + 'static>(&mut self, value: &'x mut T) -> &mut Self {
        self.put::<Mut<Value<T>>>(value)
    }

    /// Packs a context value of type `C` along with this query.
    ///
    /// The context will be consumed only when fulfilling the query.
    /// If the query is not fulfilled, the context will be returned by [`QueryUsing::finish()`]
    ///
    /// ## Example
    ///
    /// ```
    /// use dynamic_provider::{Provide, Query};
    ///
    /// #[derive(Debug)]
    /// struct MyProvider {
    ///     x: i32,
    ///     y: String,
    ///     z: Vec<u8>,
    /// }
    ///
    /// impl<'x> Provide<'x> for MyProvider {
    ///     fn provide(self, query: &mut Query<'_, 'x>) -> Option<Self> {
    ///         query
    ///             .using(self)
    ///             .put_value(|this| this.x)
    ///             .put_value(|this| this.y)
    ///             .put_value(|this| this.z)
    ///             .finish()
    ///     }
    /// }
    ///
    /// let my_provider = MyProvider {
    ///     x: 0,
    ///     y: "Foo".into(),
    ///     z: vec![1, 2, 3],
    /// };
    ///
    /// assert_eq!(my_provider.request_value::<String>().unwrap(), "Foo");
    /// ```
    pub fn using<C>(&mut self, context: C) -> QueryUsing<'_, 'x, C, LTail> {
        QueryUsing {
            context: Some(context),
            query: self,
        }
    }
}

/// Creates a [Query] expecting a value marked with `Tag` and passes it to the given function.
///
/// Returns a pair of:
/// 1. The value of type `R` returned by the given function.
/// 1. `Some(_)` if the query was fulfilled, otherwise `None`
pub fn with_query<'x, Tag: TagFor<LTail>, LTail: Lt, R>(
    f: impl FnOnce(&mut Query<'_, 'x, LTail>) -> R,
    arg: Tag::ArgValue<'x>,
) -> (R, Option<Tag::Value<'x>>)
where {
    let mut query = QueryGeneric::new::<Tag, LTail>(arg);
    let out = f(&mut query as _);

    let value = match query.state {
        QueryState::Value(value) => Some(value),
        _ => None,
    };

    (out, value)
}

/// Packs a context value of type `C` alongside the query that will be passed to a function
/// fulfilling the query.
///
/// See [`Query::using()`].
pub struct QueryUsing<'q, 'x, C, L: Lt> {
    query: &'q mut Query<'q, 'x, L>,
    context: Option<C>,
}

impl<'x, C, L: Lt> QueryUsing<'_, 'x, C, L> {
    /// Releases the context value, if still available.
    ///
    /// Returns `Some(_)` if the query was not fulfilled, so the context was never used.
    /// Returns `None` if the query was fulfilled.
    pub fn finish(self) -> Option<C> {
        self.context
    }

    /// If the query is expecting `Tag`, call the given function with the [`TypedQuery`] and context.
    /// Returns `Some(R)` value if the query expected `Tag`, otherwise `None`.
    fn downcast_with<Tag: TagFor<L>, R>(
        &mut self,
        f: impl FnOnce(&mut TypedQuery<Tag::Value<'x>, Tag::ArgValue<'x>>, C) -> R,
    ) -> Option<R> {
        self.context.as_ref()?;

        Some(f(self.query.downcast::<Tag>()?, self.context.take()?))
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a value marked by
    /// `Tag`.
    ///
    /// If `Tag` is not expected, the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put<Tag: TagFor<L>>(mut self, f: impl FnOnce(C) -> Tag::Value<'x>) -> Self {
        self.downcast_with::<Tag, _>(|state, cx| state.fulfill(f(cx)));

        self
    }

    /// Attempts to fulfill the query using a function accepting `Tag::ArgValue` and `C` and
    /// returning a value marked by `Tag`.
    ///
    /// If `Tag` is not expected, the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put_with_arg<Tag: TagFor<L>>(
        mut self,
        f: impl FnOnce(Tag::ArgValue<'x>, C) -> Tag::Value<'x>,
    ) -> Self {
        self.downcast_with::<Tag, _>(|state, cx| state.fulfill_with(|arg| f(arg, cx)));

        self
    }

    /// Behaves like [`Self::put_with_arg()`], except that the query is **not** fulfilled if `predicate` returns `false`.
    ///
    /// If `Tag` is not expected or `predicate` returns false,
    /// the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put_where<Tag: TagFor<L>>(
        self,
        predicate: impl FnOnce(&mut Tag::ArgValue<'x>, &mut C) -> bool,
        f: impl FnOnce(Tag::ArgValue<'x>, C) -> Tag::Value<'x>,
    ) -> Self {
        self.try_put_with_arg::<Tag>(|mut arg, mut cx| {
            if predicate(&mut arg, &mut cx) {
                Ok(f(arg, cx))
            } else {
                Err((arg, cx))
            }
        })
    }

    /// Behaves like [`Self::put()`] when the given function returns `Ok(_)`.
    /// When the function returns `Err(context)`, the query is **not** fulfilled and the context will be usable again.
    pub fn try_put<Tag: TagFor<L>>(self, f: impl FnOnce(C) -> Result<Tag::Value<'x>, C>) -> Self {
        self.try_put_with_arg::<Tag>(|arg, cx| f(cx).map_err(|cx| (arg, cx)))
    }

    /// Behaves like [`Self::put_with_arg()`] when the given function returns `Ok(_)`.
    /// When the function returns `Err((arg, context))`, the query is **not** fulfilled and the context will be usable again.
    pub fn try_put_with_arg<Tag: TagFor<L>>(
        mut self,
        f: impl FnOnce(Tag::ArgValue<'x>, C) -> Result<Tag::Value<'x>, (Tag::ArgValue<'x>, C)>,
    ) -> Self {
        self.context = self
            .downcast_with::<Tag, _>(|state, cx| state.try_fulfill_with(|arg| f(arg, cx)))
            .flatten();

        self
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a `T` marked by
    /// [`Value<T>`].
    ///
    /// If the value is not expected, the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put_value<T: 'static>(self, f: impl FnOnce(C) -> T) -> Self {
        self.put::<Value<T>>(f)
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a `&'x T` marked by
    /// [`Ref<Value<T>>`].
    ///
    /// If the reference is not expected, the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put_ref<T: 'static + ?Sized>(self, f: impl FnOnce(C) -> &'x T) -> Self {
        self.put::<Ref<Value<T>>>(f)
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a `&'x mut T` marked by
    /// [`Mut<Value<T>>`].
    ///
    /// If the reference is not expected, the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put_mut<T: 'static + ?Sized>(self, f: impl FnOnce(C) -> &'x mut T) -> Self {
        self.put::<Mut<Value<T>>>(f)
    }

    /// Temporarily add another value to the context for the duration of the call to `block`.
    /// After `block` is finished the new context will be dropped if it hasn't been used.
    fn add_and_drop_context<C2>(
        mut self,
        new_context: C2,
        block: impl for<'q2> FnOnce(QueryUsing<'q2, 'x, (C, C2), L>) -> QueryUsing<'q2, 'x, (C, C2), L>,
    ) -> Self {
        self.context = self
            .context
            .and_then(|cx| block(self.query.using((cx, new_context))).finish())
            .map(|(cx, _)| cx);
        self
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a `&'x T`.
    /// This will supply the `&'x T` marked by [`Ref<Value<T>>`]
    /// as well as the `T` marked by [`Value<T>`]
    /// using `T`'s [`Clone`] implementation.
    ///
    /// Behaves similarly to [`Self::put_ownable()`] but is available when the `alloc` feature is disabled.
    ///
    /// If neither the reference nor the owned value are expected,
    /// the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put_cloneable<T>(self, f: impl FnOnce(C) -> &'x T) -> Self
    where
        T: 'static + Clone,
    {
        self.add_and_drop_context(f, |q| {
            q.put_ref(|(cx, f)| f(cx))
                .put_value(|(cx, f)| f(cx).clone())
        })
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a `&'x T`.
    /// This will supply the `&'x T` marked by [`Ref<Value<T>>`]
    /// as well as the [`T::Owned`][alloc::borrow::ToOwned::Owned] marked by [`Value<T::Owned>`][Value<>]
    /// using `T's` [`ToOwned`][alloc::borrow::ToOwned] implementation.
    ///
    /// If neither the reference nor the owned value are expected,
    /// the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    #[cfg(feature = "alloc")]
    pub fn put_ownable<T>(self, f: impl FnOnce(C) -> &'x T) -> Self
    where
        T: 'static + ?Sized + alloc::borrow::ToOwned,
    {
        self.add_and_drop_context(f, |q| {
            q.put_ref(|(cx, f)| f(cx))
                .put_value(|(cx, f)| f(cx).to_owned())
        })
    }
}
