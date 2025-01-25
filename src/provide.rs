use core::{
    convert::{identity, Infallible},
    marker::PhantomData,
    ptr::NonNull,
};

use crate::{
    query::{with_query, with_query_recording_tag_ids, QueryUsing},
    tag::{Mut, Ref, TagFor, Value},
    Lt, Query, TagId,
};

/// Provides access to values of arbitrary type.
///
/// Requested values are specified using a [`TypeTag`][crate::TypeTag] implementation, or to be more specific, a [`TagFor<L>`]
/// implementation.
pub trait Provide<L: Lt = ()>: Sized {
    /// Supplies the requested value to the given [`Query`], if available.
    ///
    /// Implementations are expected to return `Some(self)` if the query was not fulfilled.
    fn provide(self, query: &mut Query<'_, L>) -> Option<Self>;

    /// Requests the value specified by `Tag`. Returns `Err(self)` if the value was not available.
    ///
    /// `arg` should contain whatever data is needed to request the tagged type.
    /// If no arguments are necessary, this will usually be the unit type `()`.
    fn request<Tag: TagFor<L>>(self, arg: Tag::ArgValue) -> Result<Tag::Value, Self> {
        match with_query::<Tag, L, _>(|q| self.provide(q), arg) {
            (_, Some(value)) => Ok(value),
            (Some(this), None) => Err(this),
            (None, None) => panic!("'self' not returned when 'provide' failed"),
        }
    }

    /// Requests a value of type `T`, marked by the tag [`Value<T>`].
    /// Values of type `T` must not hold any borrowed data.
    fn request_value<T: 'static>(self) -> Result<T, Self> {
        self.request::<Value<T>>(())
    }

    /// Requests a shared reference to a value of type `T`, marked by the tag [`Ref<Value<T>>`].
    /// Values of type `T` must not hold any borrowed data.
    fn request_ref<'x, T: 'static + ?Sized>(self) -> Result<&'x T, Self>
    where
        Ref<Value<T>>: TagFor<L, ArgValue = (), Value = &'x T>,
    {
        self.request::<Ref<Value<T>>>(())
    }

    /// Requests a unique reference to a value of type `T`, marked by the tag [`Mut<Value<T>>`].
    /// Values of type `T` must not hold any borrowed data.
    fn request_mut<'x, T: 'static + ?Sized>(self) -> Result<&'x mut T, Self>
    where
        Mut<Value<T>>: TagFor<L, ArgValue = (), Value = &'x mut T>,
    {
        self.request::<Mut<Value<T>>>(())
    }
}

impl<L: Lt, P: Provide<L>> Provide<L> for Option<P> {
    fn provide(self, query: &mut Query<'_, L>) -> Option<Self> {
        self.map(|this| this.provide(query))
    }

    fn request<Tag: TagFor<L>>(self, arg: Tag::ArgValue) -> Result<Tag::Value, Self> {
        match with_query::<Tag, L, _>(|q| self?.provide(q), arg) {
            (_, Some(value)) => Ok(value),
            (this, None) => Err(this),
        }
    }
}

impl<LTail: Lt, P: ProvideRef<LTail>> ProvideRef<LTail> for &mut Option<P> {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        if let Some(this) = self {
            this.provide_ref(query);
        }
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        if let Some(this) = self {
            this.provide_mut(query);
        }
    }
}

/// Provides access to values of arbitrary type from a reference.
///
/// Requested values are specified using a [`TypeTag`][crate::TypeTag] implementation, or to be more specific, a [`TagFor<LTail>`]
/// implementation.
pub trait ProvideRef<LTail: Lt = ()> {
    /// Supplies the requested value to the given [`Query`] from a shared reference to `Self`, if available.
    ///
    /// The default implementation supplies nothing;
    /// override this method to supply values from a shared reference.
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        let _ = query;
    }

    /// Supplies the requested value to the given [`Query`] from a unique reference to `Self`, if available.
    ///
    /// The default implementation supplies nothing;
    /// override this method to supply values from a shared reference.
    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        let _ = query;
    }
}

impl<'x, P: ?Sized + ProvideRef<LTail>, LTail: Lt> Provide<Lt!['x, ..LTail]> for &'x P {
    fn provide(self, query: &mut Query<'_, Lt!['x, ..LTail]>) -> Option<Self> {
        self.provide_ref(query);

        Some(self)
    }
}

impl<'x, P: ?Sized + ProvideRef<LTail>, LTail: Lt> Provide<Lt!['x, ..LTail]> for &'x mut P {
    fn provide(mut self, query: &mut Query<'_, Lt!['x, ..LTail]>) -> Option<Self> {
        self = provide_mut_with(self, query, |this, q| {
            this.provide_mut(q);
        })
        .1?;

        self = provide_mut_with(self, query, |this, q| {
            this.provide_ref(q);
        })
        .1?;

        Some(self)
    }
}

/// Provides a value given a unique reference to `T`, returning the reference if a value was not supplied.
fn provide_mut_with<'x, T: ?Sized, LTail: Lt, R>(
    in_ref: &'x mut T,
    query: &mut Query<'_, Lt!['x, ..LTail]>,
    f: impl for<'y> FnOnce(&'y mut T, &mut Query<'_, Lt!['y, .. LTail]>) -> R,
) -> (R, Option<&'x mut T>) {
    let mut in_ref_ptr = NonNull::from(&mut *in_ref);

    // SAFETY: in_ref will only be used again if the query is not fulfilled.
    // Since the lifetime of the reference and query are erased, the only way for borrowed data to
    // escape from in_ref is by fulfilling the query.
    let out = unsafe {
        let short_lived_ref = in_ref_ptr.as_mut();
        f(short_lived_ref, query)
    };

    let out_ref = if query.is_fulfilled() {
        // Query has been fulfilled and possibly contains data borrowed from *in_ref, so we must
        // discard in_ref.
        None
    } else {
        // Query has not been fulfilled, so nothing should be borrowed from *in_ref besides in_ref.
        Some(in_ref)
    };

    (out, out_ref)
}

/// Implements [`Provide`] by delegating to a function of type `F`.
///
/// Return type for [`provide_with()`].
#[derive(Clone, Copy)]
pub struct FnProvider<T, F> {
    value: T,
    f: F,
}

impl<T: core::fmt::Debug, F> core::fmt::Debug for FnProvider<T, F> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FnProvider")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<T, F, L: Lt> Provide<L> for FnProvider<T, F>
where
    F: for<'q> FnMut(QueryUsing<'q, T, L>) -> QueryUsing<'q, T, L>,
{
    fn provide(self, query: &mut Query<'_, L>) -> Option<Self> {
        let Self { value, mut f } = self;

        f(query.using(value))
            .finish()
            .map(|value| Self { value, f })
    }
}

impl<T, F> FnProvider<T, F> {
    pub fn into_inner(self) -> T {
        self.value
    }
}

/// Returns a [`Provide`] implementation that delegates to the given function.
///
/// ```
/// use dynamic_provider::{provide_with, Provide};
///
/// let provider = provide_with(
///     String::from("Hello, world!"),
///     |query| query.put_value(|this| this).put_value(Vec::<u8>::from),
/// );
///
/// assert_eq!(Provide::<()>::request_value::<Vec<u8>>(provider).unwrap(), b"Hello, world!");
/// ```
pub fn provide_with<T, F, L: Lt>(value: T, provide: F) -> FnProvider<T, F>
where
    F: for<'q> FnMut(QueryUsing<'q, T, L>) -> QueryUsing<'q, T, L>,
{
    FnProvider { value, f: provide }
}

/// Implements [`ProvideRef`] by delegating to a function of type `F`.
///
/// Return type for [`provide_by_ref_with()`].
#[derive(Clone, Copy)]
pub struct FnRefProvider<T, FRef, FMut> {
    value: T,
    f_ref: FRef,
    f_mut: FMut,
}

impl<T: core::fmt::Debug, FRef, FMut> core::fmt::Debug for FnRefProvider<T, FRef, FMut> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FnRefProvider")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<T, FRef, FMut, L: Lt> ProvideRef<L> for FnRefProvider<T, FRef, FMut>
where
    FRef:
        for<'q, 'x> Fn(QueryUsing<'q, &'x T, Lt!['x, ..L]>) -> QueryUsing<'q, &'x T, Lt!['x, ..L]>,
    FMut: for<'q, 'x> FnMut(
        QueryUsing<'q, &'x mut T, Lt!['x, ..L]>,
    ) -> QueryUsing<'q, &'x mut T, Lt!['x, ..L]>,
{
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..L]>) {
        let Self { value, f_ref, .. } = self;

        let _ = f_ref(query.using(value)).finish();
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, Lt!['this, ..L]>) {
        let Self { value, f_mut, .. } = self;

        let _ = f_mut(query.using(value)).finish();
    }
}

impl<T, FRef, FMut> FnRefProvider<T, FRef, FMut> {
    pub fn into_inner(self) -> T {
        self.value
    }
}

/// Returns a [`ProvideRef`] implementation that delegates to the given function.
///
/// ```
/// use dynamic_provider::{Lt, provide_by_ref_with, Provide};
///
/// let provider = provide_by_ref_with(
///     String::from("Hello, world!"),
///     |query| query.put_value(Clone::clone).put_ref(|s| s.as_bytes()),
///     |query| query,
/// );
///
/// assert_eq!(Provide::<Lt!['_]>::request_ref::<[u8]>(&provider).unwrap(), b"Hello, world!");
/// ```
pub fn provide_by_ref_with<T, FRef, FMut, LTail: Lt>(
    value: T,
    provide_ref: FRef,
    provide_mut: FMut,
) -> FnRefProvider<T, FRef, FMut>
where
    FRef: for<'q, 'x> Fn(
        QueryUsing<'q, &'x T, Lt!['x, ..LTail]>,
    ) -> QueryUsing<'q, &'x T, Lt!['x, ..LTail]>,
    FMut: for<'q, 'x> FnMut(
        QueryUsing<'q, &'x mut T, Lt!['x, ..LTail]>,
    ) -> QueryUsing<'q, &'x mut T, Lt!['x, ..LTail]>,
{
    FnRefProvider {
        value,
        f_ref: provide_ref,
        f_mut: provide_mut,
    }
}

impl<L: Lt> Provide<L> for () {
    fn provide(self, _: &mut Query<'_, L>) -> Option<Self> {
        Some(self)
    }
}

impl<L: Lt> ProvideRef<L> for () {}

impl<L: Lt> ProvideRef<L> for str {
    fn provide_ref<'x>(&'x self, query: &mut Query<'_, Lt!['x, ..L]>) {
        let mut query = query.using(self);

        query = query.put_ref(identity).put_ref(str::as_bytes);

        #[cfg(feature = "alloc")]
        {
            use alloc::borrow::ToOwned;
            query = query
                .put_value(str::to_owned)
                .put_value(|this| this.to_owned().into_boxed_str())
                .put_value(|this| this.as_bytes().to_owned())
                .put_value(|this| this.as_bytes().to_owned().into_boxed_slice());
        }

        query.finish();
    }
}

impl<L: Lt> Provide<L> for Infallible {
    fn provide(self, _: &mut Query<'_, L>) -> Option<Self> {
        match self {}
    }
}

impl<L: Lt> ProvideRef<L> for Infallible {}

enum WhenProviderState<P, Cx, Out> {
    Provider { provider: P, context: Cx },
    Output { out: Out },
}

pub struct HasContext<Cx>(Cx);
pub struct NoContext;

/// Helper function for requesting one of multiple possible values.
pub fn when_provider<L: Lt, P: Provide<L>, Out>(provider: P) -> WhenProvider<L, P, NoContext, Out> {
    WhenProvider {
        state: WhenProviderState::Provider {
            provider,
            context: NoContext,
        },
        _l: PhantomData,
    }
}

/// Return type of [`when_provider()`]
pub struct WhenProvider<L, P, Cx, Out> {
    state: WhenProviderState<P, Cx, Out>,
    _l: PhantomData<L>,
}

macro_rules! impl_when_provider_methods {
    (has_context = $has_context:ident) => {
        /// Handle the case where the type marked by `Tag` is supplied by the provider.
        pub fn has<Tag: TagFor<L>>(
            mut self,
            arg: Tag::ArgValue,
            transform: transform_type!(has_context = $has_context, arg = Tag::Value),
        ) -> Self {
            if let WhenProviderState::Provider {
                provider,
                context: _context,
            } = self.state
            {
                self.state = match provider.request::<Tag>(arg) {
                    Ok(out) => WhenProviderState::Output {
                        out: transform_call!(
                            has_context = $has_context,
                            transform = transform,
                            out = out,
                            context = _context
                        ),
                    },
                    Err(provider) => WhenProviderState::Provider {
                        provider,
                        context: _context,
                    },
                }
            }

            self
        }

        /// Handle the case where a value of type `T`, marked by [`Value<T>`], is supplied by the provider.
        pub fn has_value<T: 'static>(
            self,
            transform: transform_type!(has_context = $has_context, arg = T),
        ) -> Self {
            self.has::<Value<T>>((), transform)
        }
    };
    (ref/has_context = $has_context:ident) => {
        /// Handle the case where a shared reference to a value of type `T`,
        /// marked by [`Ref<Value<T>>`], is supplied by the provider.
        pub fn has_ref<T: 'static + ?Sized>(
            self,
    transform: transform_type!(has_context = $has_context, arg = &'x T),
        ) -> Self {
            self.has::<Ref<Value<T>>>((), transform)
        }

        /// Handle the case where a unique reference to a value of type `T`,
        /// marked by [`Mut<Value<T>>`], is supplied by the provider.
        pub fn has_mut<T: 'static + ?Sized>(
            self,
    transform: transform_type!(has_context = $has_context, arg = &'x mut T),
        ) -> Self {
            self.has::<Mut<Value<T>>>((), transform)
        }
    };
}

macro_rules! transform_type {
    (has_context = true, arg = $Arg:ty) => {
        impl FnOnce($Arg, Cx) -> Out
    };
    (has_context = false, arg = $Arg:ty) => {
        impl FnOnce($Arg) -> Out
    };
}

macro_rules! transform_call {
    (has_context = true, transform = $transform:ident, out = $out:ident, context = $context:ident) => {
        $transform($out, $context.0)
    };
    (has_context = false, transform = $transform:ident, out = $out:ident, context = $context:ident) => {
        $transform($out)
    };
}

impl<L: Lt, P: Provide<L>, Out> WhenProvider<L, P, NoContext, Out> {
    /// Returns the output of the successful request if there is one, or an `Err` containing the original
    /// provider.
    pub fn finish(self) -> Result<Out, P> {
        match self.state {
            WhenProviderState::Provider {
                provider,
                context: NoContext,
            } => Err(provider),
            WhenProviderState::Output { out } => Ok(out),
        }
    }

    /// Returns the output of the successful request if there is one, or the return value of the given
    /// function that accepts the original provider.
    pub fn or_else(self, f: impl FnOnce(P) -> Out) -> Out {
        self.finish().unwrap_or_else(f)
    }

    /// Adds a context value to all subsequent request handlers, which will be passed as the second
    /// callback parameter.
    pub fn with<Cx>(self, context: Cx) -> WhenProvider<L, P, HasContext<Cx>, Out> {
        WhenProvider {
            state: match self.state {
                WhenProviderState::Provider {
                    provider,
                    context: NoContext,
                } => WhenProviderState::Provider {
                    provider,
                    context: HasContext(context),
                },
                WhenProviderState::Output { out } => WhenProviderState::Output { out },
            },
            _l: PhantomData,
        }
    }

    impl_when_provider_methods!(has_context = false);
}

impl<'x, L: Lt, P: Provide<Lt!['x, ..L]>, Out> WhenProvider<Lt!['x, ..L], P, NoContext, Out> {
    impl_when_provider_methods!(ref/has_context = false);
}

impl<L: Lt, P: Provide<L>, Cx, Out> WhenProvider<L, P, HasContext<Cx>, Out> {
    /// Returns the output of the successful request if there is one, or an `Err` containing the original
    /// provider and the context value.
    pub fn finish(self) -> Result<Out, (P, Cx)> {
        match self.state {
            WhenProviderState::Provider {
                provider,
                context: HasContext(context),
            } => Err((provider, context)),
            WhenProviderState::Output { out } => Ok(out),
        }
    }

    /// Returns the output of the successful request if there is one, or the return value of the given
    /// function that accepts the original provider and the context value.
    pub fn or_else(self, f: impl FnOnce(P, Cx) -> Out) -> Out {
        self.finish()
            .unwrap_or_else(|(provider, cx)| f(provider, cx))
    }

    impl_when_provider_methods!(has_context = true);
}

impl<'x, L: Lt, P: Provide<Lt!['x, ..L]>, Cx, Out>
    WhenProvider<Lt!['x, ..L], P, HasContext<Cx>, Out>
{
    impl_when_provider_methods!(ref/has_context = true);
}

/// Given a provider, calls the given callback for each [`TagId`] it provides.
///
/// Returns the original provider.
pub fn for_each_provided_tag_id<L: Lt, P: Provide<L>>(
    provider: P,
    on_provide_attempt: impl FnMut(TagId),
) -> P {
    with_query_recording_tag_ids(|q| provider.provide(q), on_provide_attempt).unwrap()
}

/// Given a provider, creates a collection of [`TagId`]s it provides.
///
#[cfg_attr(
    feature = "alloc",
    doc = r#"
```
use dynamic_provider::{Mut, Ref, TagId, Value};

let provider = String::from("Hello, world!");

let tag_ids: Vec<_> = dynamic_provider::get_provided_tag_ids::<(), _>(&provider);

assert!(tag_ids.contains(&TagId::of::<Ref<Value<str>>>()));
assert!(tag_ids.contains(&TagId::of::<Ref<Value<[u8]>>>()));
assert!(tag_ids.contains(&TagId::of::<Value<String>>()));
```
"#
)]
pub fn get_provided_tag_ids<L: Lt, Out>(provider: impl Provide<L>) -> Out
where
    Out: Default + Extend<TagId>,
{
    let mut out = Out::default();
    for_each_provided_tag_id(provider, |tag_id| out.extend([tag_id]));
    out
}
