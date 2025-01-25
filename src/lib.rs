#![no_std]
#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

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
#[doc(hidden)]
pub mod type_fn;

pub use lt::Lt;
pub use provide::{
    for_each_provided_tag_id, get_provided_tag_ids, provide_by_ref_with, provide_with,
    when_provider, Provide, ProvideRef, WhenProvider,
};
pub use query::{with_query, with_query_recording_tag_ids, Query, QueryUsing};
pub use tag::{Mut, Ref, TagId, TypeTag, Value};
pub use type_fn::TypeFn;

/// Dependencies for macros to reference via `$crate::__m::*`.
#[doc(hidden)]
pub mod __m {
    pub use core::{
        convert::Infallible,
        marker::PhantomData,
        result::Result::{self, Err, Ok},
    };

    use crate::{tag::TagFor, Lt, Mut, Provide, Ref, Value};

    pub struct RequestHelper<L, P, Out> {
        state: Result<Out, P>,
        _l: PhantomData<L>,
    }

    impl<L, P, Out> RequestHelper<L, P, Out>
    where
        L: Lt,
        P: Provide<L>,
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

        pub fn request<Tag: TagFor<L>>(self, arg: Tag::ArgValue) -> Result<Tag::Value, Self> {
            let Err(p) = self.state else {
                return Err(self);
            };
            p.request::<Tag>(arg).map_err(Self::new)
        }

        pub fn request_value<T: 'static>(
            self,
            arg: <Value<T> as TagFor<L>>::ArgValue,
        ) -> Result<T, Self>
        where
            Value<T>: TagFor<L, Value = T>,
        {
            self.request::<Value<T>>(arg)
        }

        pub fn finish(self) -> Result<Out, P> {
            self.state
        }
    }

    impl<'x, L, P, Out> RequestHelper<Lt!['x, ..L], P, Out>
    where
        L: Lt,
        P: Provide<Lt!['x, ..L]>,
    {
        pub fn request_ref<T: 'static + ?Sized>(self, arg: ()) -> Result<&'x T, Self> {
            self.request::<Ref<Value<T>>>(arg)
        }

        pub fn request_mut<T: 'static + ?Sized>(self, arg: ()) -> Result<&'x mut T, Self> {
            self.request::<Mut<Value<T>>>(arg)
        }
    }
}
