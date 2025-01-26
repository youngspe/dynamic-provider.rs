#![allow(unused_parens)]

/// Requests one of a set of possible values, refs, or tags from a provider.
///
/// If a value is found, its branch will be evaluated and none of the subsequent
/// values will be requested from the provider.
///
/// ## Usage
///
/// ```
/// use dynamic_provider::{define_tag, request, Lt, ProvideRef};
///
/// define_tag! {
///     /// A tag that requires an arg
///     tag TagWithArg: for<'x, 'y, 'arg> &'arg i32 => i32;
///
///     /// A tag that does not require an arg
///     tag TagWithoutArg: for<'x, 'y, 'arg> i32;
/// }
///
/// fn get_number<'x, 'y>(provider: &'x mut dyn for<'arg> ProvideRef<Lt!['y, 'arg]>) -> i32 {
///     let arg_value = 1;
///
///     request! {
///         from provider;
///
///         // This line is optional unless the lifetime list cannot be inferred:
///         in 'x, 'y, '_;
///
///         // Request by tag with arg:
///         tag x: TagWithArg where arg <- &arg_value => x,
///
///         // Request by tag without arg:
///         tag x: TagWithoutArg => x,
///
///         // Request a 'static type:
///         x: i32 => x,
///
///         // Request a unique reference:
///         ref mut x: i32 => *x,
///
///         // Request a shared reference with an inferred type:
///         ref x => *x,
///
///         // When none of the above requests were fulfilled:
///         else => 0,
///     }
/// }
/// ```
///
/// ## Notes
///
/// The `else` branch may also include a pattern that will be bound to the
/// original provider like so:
///
/// ```ignore
/// else provider => todo!()
/// ```
///
/// If there is no `else` branch at all, this macro evaluates to a `Result`
/// where `Ok` contains the output of one of the above branches, and `Err`
/// contains the original provider.
#[macro_export]
macro_rules! request {
    ($($token:tt)*) => {
        $crate::__request_invoke! { $($token)* }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __request_invoke {
    (from $from:expr; in $($rest:tt)*) => {
        $crate::__request_in! { @from = ($from); in $($rest)* }
    };
    (from $from:expr; $($rest:tt)*) => {
        $crate::__request_body! {
            @from = ($from);
            @in = (_);
            @tokens = { $($rest)* };
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __request_in {
    (@from = $from:tt; in $($lt:lifetime),* $(,)?; $($rest:tt)* ) => {
        $crate::__request_body! {
            @from = $from;
            @in = ($crate::Lt![$($lt),*]);
            @tokens = { $($rest)* };
        }
    };
    (@from = $from:tt; in $($lt:lifetime,)* .. $L:ty; $($rest:tt)* ) => {
        $crate::__request_body! {
            @from = $from;
            @in = (Lt![$($lt,)* .. $L]);
            @tokens = { $($rest)* };
        }
    };
    (@from = $from:tt; in $L:ty; $($rest:tt)* ) => {
        $crate::__request_body! {
            @from = $from;
            @in = ($L);
            @tokens = { $($rest)* };
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __request_body {
    {
        @from = ($from:expr);
        @in = ($L:ty);
        @tokens = {
            $($rest:tt)*
        };
    } => {
        {
            let mut _helper = $crate::__m::RequestHelper::<$L, _, _>::new($from);

            $crate::__request_rules! {
                @helper = _helper;
                @tokens = { $($rest)* };
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __request_rules {
    {
        @helper = $helper:ident;
        @tokens = {
            else $pat:pat => $out:expr $(,)?
        };
    } => {
        match $helper.finish() {
            $crate::__m::Ok(_out) => _out,
            $crate::__m::Err($pat) => $out,
        }
    };
    {
        @helper = $helper:ident;
        @tokens = {
            else => $out:expr $(,)?
        };
    } => {
        match $helper.finish() {
            $crate::__m::Ok(_out) => _out,
            $crate::__m::Err(_) => $out,
        }
    };

    {
        @helper = $helper:ident;
        @tokens = {
            else $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = else;
            @helper = $helper;
            @tokens = { $($rest)* };
        }
    };

    {
        @helper = $helper:ident;
        @tokens = {};
    } => {
        $helper.finish()
    };

    {
        @helper = $helper:ident;
        @tokens = {
            ref mut $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = request_mut;
            @helper = $helper;
            @tokens = { $($rest)* };
        }
    };
    {
        @helper = $helper:ident;
        @tokens = {
            ref $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = request_ref;
            @helper = $helper;
            @tokens = { $($rest)* };
        }
    };
    {
        @helper = $helper:ident;
        @tokens = {
            tag $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = request;
            @helper = $helper;
            @tokens = { $($rest)* };
        }
    };
    {
        @helper = $helper:ident;
        @tokens = {
            $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = request_value;
            @helper = $helper;
            @tokens = { $($rest)* };
        }
    };

    {
        @modifier = $method:ident;
        @helper = $helper:ident;
        @tokens = {
            $pat:tt: $Ty:ty
            $(where arg <- $arg:expr)?
            => $out:expr
            $(, $($rest:tt)*)?
        };
    } => {
        $helper = match $helper.$method::<$Ty>(($($arg)?)) {
            $crate::__m::Ok($pat) => $crate::__m::RequestHelper::from_out($out),
            $crate::__m::Err(_helper) => _helper,
        };

        $crate::__request_rules! {
            @helper = $helper;
            @tokens = { $($($rest)*)? };
        }
    };

    {
        @modifier = $modifier:tt;
        @helper = $helper:ident;
        @tokens = {
            $pat:pat => $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = $modifier;
            @helper = $helper;
            @tokens = {
                ($pat): _ => $($rest)*
            };
        }
    };
    {
        @modifier = $modifier:tt;
        @helper = $helper:ident;
        @tokens = {
            $($pat1:ident)* $(::$pat2:ident)* $({$($pat3:tt)*})? $(($($pat4:tt)*))?
            : $Ty:ty
            $(where $key:ident = $value:expr)?
            => $($rest:tt)*
        };
    } => {
        $crate::__request_rules! {
            @modifier = $modifier;
            @helper = $helper;
            @tokens = {
                ( $($pat1)* $(::$pat2)* $({ $($pat3)* })? $(( $($pat4)* ))? )
                : $Ty
                $(where $key = $value)?
                => $($rest)*
            };
        }
    };
}

#[cfg(test)]
mod test {
    use crate::{Lt, ProvideRef, QueryUsing};

    #[test]
    fn macro_usage() {
        crate::define_tag! {
            tag Foo: for<'x, 'y, 'input> &'input i32 => i32;
        }
        fn explicit_lts<'x, 'y>(
            p: &'x mut dyn for<'i> crate::ProvideRef<crate::Lt!['y, 'i]>,
        ) -> i32 {
            let a = 0;
            let b = 1;

            request! {
                from &mut *p;
                in 'x, 'y, '_;
                x: i32 => x,
                tag x: Foo where arg <- &b => x + a,
                ref mut x => *x,
                ref y => *y,
                else => 0,
            }
        }

        fn implicit_lts(p: &mut dyn for<'i> crate::ProvideRef<crate::Lt!['_, 'i]>) -> i32 {
            let a = 0;
            let b = 1;

            request! {
                from &mut *p;
                x: i32 => x,
                tag x: Foo where arg <- &b => x + a,
                ref mut x => *x,
                ref y => *y,
                else => 0,
            }
        }

        fn check_both(p: &mut dyn for<'i> crate::ProvideRef<crate::Lt!['_, 'i]>) -> i32 {
            let out1 = explicit_lts(p);
            let out2 = implicit_lts(p);
            assert_eq!(out1, out2);
            out1
        }

        #[allow(clippy::type_complexity)]
        struct CustomProvide(
            i32,
            for<'q, 'x, 'y, 'z> fn(
                QueryUsing<'q, &'x mut i32, Lt!['x, 'y, 'z]>,
            ) -> QueryUsing<'q, &'x mut i32, Lt!['x, 'y, 'z]>,
        );

        impl ProvideRef<Lt!['_, '_]> for CustomProvide {
            fn provide_mut<'this>(&'this mut self, query: &mut crate::Query<Lt!['this, '_, '_]>) {
                self.1(query.using(&mut self.0)).finish();
            }
        }

        let mut p_value = CustomProvide(1_i32, |q| q.put_value(|x| *x));
        assert_eq!(check_both(&mut p_value), 1);

        let mut p_tag = CustomProvide(1_i32, |q| q.put_with_arg::<Foo>(|arg, x| *arg + *x));
        assert_eq!(check_both(&mut p_tag), 2);

        let mut p_ref = CustomProvide(3_i32, |q| q.put_ref(|x| x));
        assert_eq!(check_both(&mut p_ref), 3);

        let mut p_mut = CustomProvide(4_i32, |q| q.put_mut(|x| x));
        assert_eq!(check_both(&mut p_mut), 4);

        assert_eq!(check_both(&mut ()), 0);
    }
}
