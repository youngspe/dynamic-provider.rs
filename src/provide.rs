use core::{convert::Infallible, marker::PhantomData, ptr::NonNull};

use crate::{
    query::{with_query, QueryUsing},
    tag::{Mut, Ref, TagFor, Value},
    Lt, Query,
};

pub trait Provide<'x, LTail: Lt = ()>: Sized {
    fn provide(self, query: &mut Query<'_, 'x, LTail>) -> Option<Self>;

    fn request<Tag: TagFor<LTail>>(self, arg: Tag::ArgValue<'x>) -> Result<Tag::Value<'x>, Self> {
        match with_query::<Tag, LTail, _>(|q| self.provide(q), arg) {
            (_, Some(value)) => Ok(value),
            (Some(this), None) => Err(this),
            (None, None) => panic!("'self' not returned when 'provide' failed"),
        }
    }

    fn request_value<T: 'static>(self) -> Result<T, Self> {
        self.request::<Value<T>>(())
    }

    fn request_ref<T: 'static>(self) -> Result<&'x T, Self> {
        self.request::<Ref<Value<T>>>(())
    }

    fn request_mut<T: 'static>(self) -> Result<&'x mut T, Self> {
        self.request::<Mut<Value<T>>>(())
    }
}

impl<'x, LTail: Lt, P: Provide<'x, LTail>> Provide<'x, LTail> for Option<P> {
    fn provide(self, query: &mut Query<'_, 'x, LTail>) -> Option<Self> {
        self.map(|this| this.provide(query))
    }

    fn request<Tag: TagFor<LTail>>(self, arg: Tag::ArgValue<'x>) -> Result<Tag::Value<'x>, Self> {
        match with_query::<Tag, LTail, _>(|q| self?.provide(q), arg) {
            (_, Some(value)) => Ok(value),
            (this, None) => Err(this),
        }
    }
}

impl<'x, LTail: Lt, P: ProvideRef<LTail>> ProvideRef<LTail> for &mut Option<P> {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, 'this, LTail>) {
        if let Some(this) = self {
            this.provide_ref(query);
        }
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, 'this, LTail>) {
        if let Some(this) = self {
            this.provide_mut(query);
        }
    }
}

pub trait ProvideRef<L: Lt = ()> {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, 'this, L>) {
        let _ = query;
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, 'this, L>) {
        let _ = query;
    }
}

impl<'x, P: ?Sized + ProvideRef<L>, L: Lt> Provide<'x, L> for &'x P {
    fn provide(self, query: &mut Query<'_, 'x, L>) -> Option<Self> {
        self.provide_ref(query);

        Some(self)
    }
}

impl<'x, P: ?Sized + ProvideRef<L>, L: Lt> Provide<'x, L> for &'x mut P {
    fn provide(mut self, query: &mut Query<'_, 'x, L>) -> Option<Self> {
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

fn provide_mut_with<'x, T: ?Sized, L: Lt, R>(
    in_ref: &'x mut T,
    query: &mut Query<'_, 'x, L>,
    f: impl for<'y> FnOnce(&'y mut T, &mut Query<'_, 'y, L>) -> R,
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

pub struct FnProvider<T, F> {
    value: T,
    f: F,
}

impl<'x, T, F, L: Lt> Provide<'x, L> for FnProvider<T, F>
where
    F: for<'q> FnMut(QueryUsing<'q, 'x, T, L>) -> QueryUsing<'q, 'x, T, L>,
{
    fn provide(self, query: &mut Query<'_, 'x, L>) -> Option<Self> {
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

pub fn provide_with_fn<'x, T, F, L: Lt>(value: T, provide: F) -> FnProvider<T, F>
where
    F: for<'q> FnMut(QueryUsing<'q, 'x, T, L>) -> QueryUsing<'q, 'x, T, L>,
{
    FnProvider { value, f: provide }
}

pub struct FnRefProvider<T, FRef, FMut> {
    value: T,
    f_ref: FRef,
    f_mut: FMut,
}

impl<T, FRef, FMut, L: Lt> ProvideRef<L> for FnRefProvider<T, FRef, FMut>
where
    FRef: for<'q, 'x> Fn(QueryUsing<'q, 'x, &'x T, L>) -> QueryUsing<'q, 'x, &'x T, L>,
    FMut: for<'q, 'x> FnMut(QueryUsing<'q, 'x, &'x mut T, L>) -> QueryUsing<'q, 'x, &'x mut T, L>,
{
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, 'this, L>) {
        let Self { value, f_ref, .. } = self;

        let _ = f_ref(query.using(value)).finish();
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, 'this, L>) {
        let Self { value, f_mut, .. } = self;

        let _ = f_mut(query.using(value)).finish();
    }
}

impl<T, FRef, FMut> FnRefProvider<T, FRef, FMut> {
    pub fn into_inner(self) -> T {
        self.value
    }
}

pub fn provide_by_ref_with_fn<T, FRef, FMut, L: Lt>(
    value: T,
    provide_ref: FRef,
    provide_mut: FMut,
) -> FnRefProvider<T, FRef, FMut>
where
    FRef: for<'q, 'x> Fn(QueryUsing<'q, 'x, &'x T, L>) -> QueryUsing<'q, 'x, &'x T, L>,
    FMut: for<'q, 'x> FnMut(QueryUsing<'q, 'x, &'x mut T, L>) -> QueryUsing<'q, 'x, &'x mut T, L>,
{
    FnRefProvider {
        value,
        f_ref: provide_ref,
        f_mut: provide_mut,
    }
}

impl<L: Lt> Provide<'_, L> for () {
    fn provide(self, _: &mut Query<'_, '_, L>) -> Option<Self> {
        Some(self)
    }
}

impl<L: Lt> ProvideRef<L> for () {}

impl<L: Lt> Provide<'_, L> for Infallible {
    fn provide(self, _: &mut Query<'_, '_, L>) -> Option<Self> {
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

pub fn when_provider<'x, L: Lt, P: Provide<'x, L>, Out>(
    provider: P,
) -> WhenProvider<'x, L, P, NoContext, Out> {
    WhenProvider {
        state: WhenProviderState::Provider {
            provider,
            context: NoContext,
        },
        _lt: PhantomData,
        _l: PhantomData,
    }
}

pub struct WhenProvider<'x, L, P, Cx, Out> {
    state: WhenProviderState<P, Cx, Out>,
    _lt: PhantomData<&'x ()>,
    _l: PhantomData<L>,
}

macro_rules! impl_when_provider_methods {
    (has_context = $has_context:ident) => {
        pub fn has<Tag: TagFor<L>>(
            mut self,
            arg: Tag::ArgValue<'x>,
    transform: transform_type!(has_context = $has_context, arg = Tag::Value<'x>),
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

        pub fn has_value<T: 'static>(
            self,
            transform: transform_type!(has_context = $has_context, arg = T),
        ) -> Self {
            self.has::<Value<T>>((), transform)
        }

        pub fn has_ref<T: 'static>(
            self,
    transform: transform_type!(has_context = $has_context, arg = &'x T),
        ) -> Self {
            self.has::<Ref<Value<T>>>((), transform)
        }

        pub fn has_mut<T: 'static>(
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

impl<'x, L: Lt, P: Provide<'x, L>, Out> WhenProvider<'x, L, P, NoContext, Out> {
    pub fn finish(self) -> Result<Out, P> {
        match self.state {
            WhenProviderState::Provider {
                provider,
                context: NoContext,
            } => Err(provider),
            WhenProviderState::Output { out } => Ok(out),
        }
    }

    pub fn with<Cx>(self, context: Cx) -> WhenProvider<'x, L, P, HasContext<Cx>, Out> {
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
            _lt: PhantomData,
            _l: PhantomData,
        }
    }

    impl_when_provider_methods!(has_context = false);
}

impl<'x, L: Lt, P: Provide<'x, L>, Cx, Out> WhenProvider<'x, L, P, HasContext<Cx>, Out> {
    pub fn finish(self) -> Result<Out, (P, Cx)> {
        match self.state {
            WhenProviderState::Provider {
                provider,
                context: HasContext(context),
            } => Err((provider, context)),
            WhenProviderState::Output { out } => Ok(out),
        }
    }

    impl_when_provider_methods!(has_context = true);
}
