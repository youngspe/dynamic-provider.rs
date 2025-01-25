use core::{convert::Infallible, mem};

use crate::{
    tag::{MarkerTag, Mut, Ref, TagFor, TagId, Value},
    Lt,
};

struct AlreadyFulfilled;
struct ShouldRecordAttempts;

/// The generic arg to [`QueryGeneric`] that unsizes to [`dyn ErasedQueryState`][ErasedQueryState].
///
/// This is `repr(C)` so that `&mut QueryState<T, Arg, F>` is a valid `&mut QueryState<T, Arg, Empty>`.
#[repr(C)]
struct QueryState<T, Arg, F> {
    value: QueryValue<T, Arg>,
    on_provide_attempt: F,
}

/// Internal state of a query.
#[derive(Default)]
enum QueryValue<T, Arg> {
    /// Invalid state so we can move `Arg` out of `&mut Self`.
    #[default]
    Invalid,
    /// Fulfilled query.
    Value(T),
    /// Unfulfilled query with whatever argument the tag requires.
    Arg(Arg),
}

/// Just a `repr(C)` zero-sized type to stand in place of `F` in [`TypedQuery`].
#[repr(C)]
struct Empty {}

/// Result of successful [`Query::downcast()`] call.
#[repr(transparent)]
struct TypedQuery<T, Arg> {
    inner: QueryGeneric<QueryState<T, Arg, Empty>>,
}

impl<T, Arg> TypedQuery<T, Arg> {
    fn fulfill(&mut self, value: T) {
        if let QueryValue::Arg { .. } = self.inner.state.value {
            self.inner.state.value = QueryValue::Value(value);
            self.inner.tag_id = TagId::of::<MarkerTag<AlreadyFulfilled>>();
        }
    }

    fn fulfill_with(&mut self, f: impl FnOnce(Arg) -> T) {
        self.try_fulfill_with::<Infallible>(|arg| Ok(f(arg)));
    }

    fn try_fulfill_with<R>(&mut self, f: impl FnOnce(Arg) -> Result<T, (Arg, R)>) -> Option<R> {
        let QueryValue::Arg(arg) = mem::take(&mut self.inner.state.value) else {
            return None;
        };
        let out;
        (out, self.inner.state.value) = match f(arg) {
            Ok(value) => (None, QueryValue::Value(value)),
            Err((arg, out)) => (Some(out), QueryValue::Arg(arg)),
        };
        self.inner.tag_id = TagId::of::<MarkerTag<AlreadyFulfilled>>();
        out
    }
}

/// ## Safety
/// It's assumed an implementor is `QueryState<Tag::Value<'x>, Tag::ArgValue<'x>>` for some `Arg: TagFor<L>`
/// and can be downcast back to the concrete type if `Tag` is known.
pub unsafe trait ErasedQueryState<L: Lt> {
    fn record_attempt(&mut self, tag_id: TagId);
}

unsafe impl<L, T, Arg, F> ErasedQueryState<L> for QueryState<T, Arg, F>
where
    L: Lt,
    F: FnMut(TagId),
{
    fn record_attempt(&mut self, tag_id: TagId) {
        (self.on_provide_attempt)(tag_id);
    }
}
/// Generic type meant for unsizing to [Query].
#[repr(C)]
pub struct QueryGeneric<Q: ?Sized> {
    /// Identifies the tag type with which `state` was created.
    /// When the query has been fulfilled, this will be set to `TagId::of::<MarkerTag<AlreadyFulfilled>>()` to indicate no future
    /// downcasts need be attempted.
    tag_id: TagId,
    /// Either a `QueryState` or a `dyn ErasedQueryState` holding the query's internal state
    /// (whether fulfilled or unfulfilled).
    state: Q,
}

impl<T, Arg, F> QueryGeneric<QueryState<T, Arg, F>> {
    fn new<Tag, L>(arg: Arg, on_provide_attempt: F) -> Self
    where
        Tag: TagFor<L, Value = T, ArgValue = Arg>,
        L: Lt,
    {
        Self {
            tag_id: TagId::of::<Tag>(),
            state: QueryState {
                value: QueryValue::Arg(arg),
                on_provide_attempt,
            },
        }
    }
}

/// A type-erased query ready to pass to [`crate::Provide::provide()`].
///
/// Providers may use this type to supply tagged values.
pub type Query<'data, L = ()> = QueryGeneric<dyn ErasedQueryState<L> + 'data>;

impl<L: Lt> Query<'_, L> {
    fn downcast<Tag: TagFor<L>>(&mut self) -> Option<&mut TypedQuery<Tag::Value, Tag::ArgValue>> {
        let tag_id = TagId::of::<Tag>();

        if self.tag_id == TagId::of::<MarkerTag<ShouldRecordAttempts>>() {
            self.state.record_attempt(tag_id);
            return None;
        }

        if self.tag_id == tag_id {
            // SAFETY: `Tag` is the same type used to create this query, so the underlying type should be
            // TypedQuery<Tag::Value, Tag::ArgValue>
            let query =
                unsafe { &mut *(self as *mut Self as *mut TypedQuery<Tag::Value, Tag::ArgValue>) };

            return Some(query);
        }

        None
    }

    /// Returns `true` if the query has been fulfilled and no values will be accepted in the future.
    pub fn is_fulfilled(&self) -> bool {
        self.tag_id == TagId::of::<MarkerTag<AlreadyFulfilled>>()
    }

    /// Returns `true` if this query would accept a value tagged with `Tag`.
    ///
    /// **Note**: this will return `false` if a value tagged with `Tag` _was_ expected and has been
    /// fulfilled, as it will not accept additional values.
    pub fn expects<Tag: TagFor<L>>(&self) -> bool {
        self.tag_id == TagId::of::<Tag>()
    }

    /// Returns the [`TagId`] expected by this query.
    ///
    /// If this query has already been fulfilled, the returned ID is unspecified.
    pub fn expected_tag_id(&self) -> TagId {
        self.tag_id
    }

    /// Attempts to fulfill the query with a value marked with `Tag`.
    pub fn put<Tag: TagFor<L>>(&mut self, value: Tag::Value) -> &mut Self {
        if let Some(state) = self.downcast::<Tag>() {
            state.fulfill(value)
        }

        self
    }

    /// Attempts to fulfill the query with a function returning a value marked with `Tag`.
    ///
    /// The function will not be called if the query does not accept `Tag`.
    pub fn put_with<Tag: TagFor<L>>(
        &mut self,
        f: impl FnOnce(Tag::ArgValue) -> Tag::Value,
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
    pub fn put_where<Tag: TagFor<L>>(
        &mut self,
        predicate: impl FnOnce(&mut Tag::ArgValue) -> bool,
        f: impl FnOnce(Tag::ArgValue) -> Tag::Value,
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
    pub fn try_put<Tag: TagFor<L>>(
        &mut self,
        f: impl FnOnce(Tag::ArgValue) -> Result<Tag::Value, Tag::ArgValue>,
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

    /// Packs a context value of type `C` along with this query.
    ///
    /// The context will be consumed only when fulfilling the query.
    /// If the query is not fulfilled, the context will be returned by [`QueryUsing::finish()`]
    ///
    /// ## Example
    ///
    /// ```
    /// use dynamic_provider::{Lt, Provide, Query};
    ///
    /// #[derive(Debug)]
    /// struct MyProvider {
    ///     x: i32,
    ///     y: String,
    ///     z: Vec<u8>,
    /// }
    ///
    /// impl<'x> Provide<Lt!['x]> for MyProvider {
    ///     fn provide(self, query: &mut Query<'_, Lt!['x]>) -> Option<Self> {
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
    pub fn using<C>(&mut self, context: C) -> QueryUsing<'_, C, L> {
        QueryUsing {
            context: Some(context),
            query: self,
        }
    }
}

impl<'x, L: Lt> Query<'_, Lt!['x, ..L]> {
    /// Attempts to fulfill the query with a `&'x T` marked with [`Ref<Value<T>>`].
    pub fn put_ref<T: ?Sized + 'static>(&mut self, value: &'x T) -> &mut Self {
        self.put::<Ref<Value<T>>>(value)
    }

    /// Attempts to fulfill the query with a `&'x mut T` marked with [`Mut<Value<T>>`].
    pub fn put_mut<T: ?Sized + 'static>(&mut self, value: &'x mut T) -> &mut Self {
        self.put::<Mut<Value<T>>>(value)
    }
}

/// Creates a [`Query`] expecting a value marked with `Tag` and passes it to the given function.
///
/// Returns a pair of:
/// 1. The value of type `R` returned by the given function.
/// 1. `Some(_)` if the query was fulfilled, otherwise `None`
pub fn with_query<Tag: TagFor<L>, L: Lt, R>(
    block: impl FnOnce(&mut Query<'_, L>) -> R,
    arg: Tag::ArgValue,
) -> (R, Option<Tag::Value>)
where {
    let mut query = QueryGeneric::new::<Tag, L>(arg, |_| {});
    let out = block(&mut query as _);

    let value = match query.state.value {
        QueryValue::Value(value) => Some(value),
        _ => None,
    };

    (out, value)
}

/// Creates a [`Query`] not expecting any particular tag, passes it to `block`,
/// and calls `on_provide_attempt()` for every available [`TagId`].
///
/// Returns the value of type `R` returned by `block`.
pub fn with_query_recording_tag_ids<L: Lt, R>(
    block: impl FnOnce(&mut Query<'_, L>) -> R,
    on_provide_attempt: impl FnMut(TagId),
) -> R
where {
    let mut query = QueryGeneric::new::<MarkerTag<ShouldRecordAttempts>, L>((), on_provide_attempt);
    block(&mut query as _)
}

/// Packs a context value of type `C` alongside the query that will be passed to a function
/// fulfilling the query.
///
/// See [`Query::using()`].
#[must_use]
pub struct QueryUsing<'q, C, L: Lt> {
    query: &'q mut Query<'q, L>,
    context: Option<C>,
}

impl<C, L: Lt> QueryUsing<'_, C, L> {
    /// Releases the context value, if still available.
    ///
    /// Returns `Some(_)` if the query was not fulfilled, so the context was never used.
    /// Returns `None` if the query was fulfilled.
    pub fn finish(self) -> Option<C> {
        self.context
    }

    /// Returns `true` if the query has been fulfilled and no values will be accepted in the future.
    pub fn is_fulfilled(&self) -> bool {
        self.query.is_fulfilled()
    }

    /// Returns `true` if this query would accept a value tagged with `Tag`.
    ///
    /// **Note**: this will return `false` if a value tagged with `Tag` _was_ expected and has been
    /// fulfilled, as it will not accept additional values.
    pub fn expects<Tag: TagFor<L>>(&self) -> bool {
        self.query.expects::<Tag>()
    }

    /// Returns the [`TagId`] expected by this query.
    ///
    /// If this query has already been fulfilled, the returned ID is unspecified.
    pub fn expected_tag_id(&self) -> TagId {
        self.query.expected_tag_id()
    }

    /// If the query is expecting `Tag`, call the given function with the [`TypedQuery`] and context.
    /// Returns `Some(R)` value if the query expected `Tag`, otherwise `None`.
    fn downcast_with<Tag: TagFor<L>, R>(
        &mut self,
        f: impl FnOnce(&mut TypedQuery<Tag::Value, Tag::ArgValue>, C) -> R,
    ) -> Option<R> {
        self.context.as_ref()?;

        Some(f(self.query.downcast::<Tag>()?, self.context.take()?))
    }

    /// Attempts to fulfill the query using a function accepting `C` and returning a value marked by
    /// `Tag`.
    ///
    /// If `Tag` is not expected, the function will not be called and the context will not be used.
    /// Does nothing if the query has already been fulfilled.
    pub fn put<Tag: TagFor<L>>(mut self, f: impl FnOnce(C) -> Tag::Value) -> Self {
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
        f: impl FnOnce(Tag::ArgValue, C) -> Tag::Value,
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
        predicate: impl FnOnce(&mut Tag::ArgValue, &mut C) -> bool,
        f: impl FnOnce(Tag::ArgValue, C) -> Tag::Value,
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
    pub fn try_put<Tag: TagFor<L>>(self, f: impl FnOnce(C) -> Result<Tag::Value, C>) -> Self {
        self.try_put_with_arg::<Tag>(|arg, cx| f(cx).map_err(|cx| (arg, cx)))
    }

    /// Behaves like [`Self::put_with_arg()`] when the given function returns `Ok(_)`.
    /// When the function returns `Err((arg, context))`, the query is **not** fulfilled and the context will be usable again.
    pub fn try_put_with_arg<Tag: TagFor<L>>(
        mut self,
        f: impl FnOnce(Tag::ArgValue, C) -> Result<Tag::Value, (Tag::ArgValue, C)>,
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

    /// Temporarily add another value to the context for the duration of the call to `block`.
    /// After `block` is finished the new context will be dropped if it hasn't been used.
    fn add_and_drop_context<C2>(
        mut self,
        new_context: C2,
        block: impl for<'q2> FnOnce(QueryUsing<'q2, (C, C2), L>) -> QueryUsing<'q2, (C, C2), L>,
    ) -> Self {
        self.context = self
            .context
            .and_then(|cx| block(self.query.using((cx, new_context))).finish())
            .map(|(cx, _)| cx);
        self
    }
}

impl<'x, C, L: Lt> QueryUsing<'_, C, Lt!['x, ..L]> {
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
