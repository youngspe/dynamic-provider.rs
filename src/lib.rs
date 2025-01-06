#![no_std]
#![warn(missing_docs)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "alloc")]
mod feature_alloc;
#[doc(hidden)]
pub mod lt;
mod provide;
mod query;
mod request_macro;
mod tag;
mod tag_macro;

pub use lt::{Lt, LtSignature, TypeFn, TypeFnOf, TypeFnOutput};
pub use provide::{
    provide_by_ref_with, provide_with, when_provider, Provide, ProvideRef, WhenProvider,
};
pub use query::{with_query, Query, QueryUsing};
pub use tag::{Mut, Ref, TagFor, TagId, TypeTag, Value};

/// Dependencies for macros to reference via `$crate::__m::*`.
#[doc(hidden)]
pub mod __m {
    pub use core::{
        convert::Infallible,
        marker::PhantomData,
        result::Result::{self, Err, Ok},
    };

    use crate::{Lt, Mut, Provide, Ref, TagFor, Value};

    pub struct RequestHelper<L, P, Out> {
        state: Result<Out, P>,
        _l: PhantomData<L>,
    }

    impl<'x, L, P, Out> RequestHelper<Lt!['x, ..L], P, Out>
    where
        L: Lt,
        P: Provide<'x, L>,
    {
        pub fn new(provider: P) -> Self {
            Self {
                state: Err(provider),
                _l: PhantomData,
            }
        }

        pub fn from_out(out: Out) -> Self {
            Self {
                state: Ok(out),
                _l: PhantomData,
            }
        }

        pub fn request<Tag: TagFor<L>>(
            self,
            arg: Tag::ArgValue<'x>,
        ) -> Result<Tag::Value<'x>, Self> {
            let Err(p) = self.state else {
                return Err(self);
            };
            p.request::<Tag>(arg).map_err(Self::new)
        }

        pub fn request_value<T: 'static>(self, arg: ()) -> Result<T, Self> {
            self.request::<Value<T>>(arg)
        }

        pub fn request_ref<T: 'static + ?Sized>(self, arg: ()) -> Result<&'x T, Self> {
            self.request::<Ref<Value<T>>>(arg)
        }

        pub fn request_mut<T: 'static + ?Sized>(self, arg: ()) -> Result<&'x mut T, Self> {
            self.request::<Mut<Value<T>>>(arg)
        }

        pub fn finish(self) -> Result<Out, P> {
            self.state
        }
    }
}
