/// Declares one or more [`ResourceTag`][crate::ResourceTag] implementations.
///
/// ```
/// # use dynamic_provider::define_tag;
/// use core::ops::Deref;
///
/// define_tag! {
///     /// Tag describing a numeric status code.
///     pub tag StatusCode: i32;
///
///     /// Tag describing a pair of references to `T` and the target of `U`.
///     pub tag RefPair<T, U>: for<'x> (&'x T, &'x U::Target) where U: Deref;
///
///     /// Tag for retrieving a reference given a string slice.
///     pub tag GetRefByName<T: ?Sized>: for<'data, 'request> &'request str => &'data T;
/// }
/// ```
#[macro_export]
macro_rules! define_tag {
    {} => {};
    {
        $(#$attr:tt)*
        $vis:vis tag $Name:ident $(<$($T:ident),* $(,)?>)? : for<$($lt:lifetime),* $(,)?> $T1:ty $( => $T2:ty )? ;
        $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = {};
            @generics = { $($($T,)*)? };
            @lt_generics_with_bounds = {};
            @generics_with_bounds = { $($T,)* };
            @attrs = { $($attr)* };
            $vis tag $Name<>: for<$($lt),*> $T1 $(=> $T2)?;
        }

        $crate::define_tag! { $($rest)* };
    };
    {
        $(#$attr:tt)*
        $vis:vis tag $Name:ident < $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = {};
            @generics = {};
            @lt_generics_with_bounds = {};
            @generics_with_bounds = {};
            @attrs = { $($attr)* };
            $vis tag $Name < $($rest)*
        }
    };
    {
        $(#$attr:tt)*
        $vis:vis tag $Name:ident : $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = {};
            @generics = {};
            @lt_generics_with_bounds = {};
            @generics_with_bounds = {};
            @attrs = { $($attr)* };
            $vis tag $Name<>: $($rest)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __define_tag {
    // generics

    // read a lifetime arg or type arg without bounds at the end of the arg list
    {
        @lt_generics = { $($lt_gen:tt)* };
        @generics = { $($gen:tt)* };
        @lt_generics_with_bounds = { $($lt_gen_bounds:tt)* };
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident<
            $($lt:lifetime $(: $($lt_b:lifetime $(+)?)*)?)?
            $($T:ident)?
        $(,)? > $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = {};
            @lt_generics = { $($lt_gen)* $($lt,)? };
            @generics = { $($gen)* $($T,)? };
            @lt_generics_with_bounds = { $($lt_gen_bounds)* $($lt $(: $($lt_b +)*)?,)? };
            @generics_with_bounds = { $($gen_bounds)* $($T,)? };
            @attrs = $attrs;
            $vis tag $Name $($rest)*
        }
    };
    // read a const param at the end of the arg list
    {
        @lt_generics = $lt_gen:tt;
        @generics = { $($gen:tt)* };
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident<
            const $T:ident : $T_b:ty
        $(,)? > $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = {}
            @lt_generics = $lt_gen;
            @generics = { $($gen)* $($T,)? };
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = { $($gen_bounds)* $(const $T: $T_b,)? };
            @attrs = $attrs;
            $vis tag $Name $($rest)*
        }
    };
    // read a lifetime arg or type arg without bounds
    {
        @lt_generics = { $($lt_gen:tt)* };
        @generics = { $($gen:tt)* };
        @lt_generics_with_bounds = { $($lt_gen_bounds:tt)* };
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident<
            $($lt:lifetime $(: $($lt_b:lifetime $(+)?)*)?)?
            $($T:ident)?
        , $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = { $($lt_gen)*  $($lt,)? };
            @generics = { $($gen)* $($T,)? };
            @lt_generics_with_bounds = { $($lt_gen_bounds)* $($lt $(: $($lt_b +)*)?,)? };
            @generics_with_bounds = { $($gen_bounds)* $($T,)? };
            @attrs = $attrs;
            $vis tag $Name< $($rest)*
        }
    };
    // read a const param
    {
        @lt_generics = $lt_gen:tt;
        @generics = { $($gen:tt)* };
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident<
            const $T:ident : $T_b:ty
        , $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = $lt_gen;
            @generics = { $($gen)* $T, };
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = { $($gen_bounds)* $(const $T: $T_b,)? };
            @attrs = $attrs;
            $vis tag $Name< $($rest)*
        }
    };
    // read a type arg with bounds
    {
        @lt_generics = $lt_gen:tt;
        @generics = { $($gen:tt)* };
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident< $T:ident: $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = $lt_gen;
            @generics = { $($gen)* $T, };
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = { $($gen_bounds)* $T: };
            @attrs = $attrs;
            $vis tag $Name< : $($rest)*
        }
    };
    // read a type arg bound end at end of arg list
    {
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident< : $(,)? > $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = {};
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = { $($gen_bounds)* , };
            @attrs = $attrs;
            $vis tag $Name $($rest)*
        }
    };
    // read a type arg bound end
    {
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident< : , $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = { $($gen_bounds)* , };
            @attrs = $attrs;
            $vis tag $Name< , $($rest)*
        }
    };
    // read a type arg bound token
    {
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident< : $bound:tt $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = { $($gen_bounds)* $bound };
            @attrs = $attrs;
            $vis tag $Name< : $($rest)*
        }
    };

    // where

    // end of where clause (if any)
    {
        @where = { $($where:tt)* };
        @lt_generics = { $($lt_gen:lifetime,)* };
        @generics = { $($gen:ident,)* };
        @lt_generics_with_bounds = { $($lt_gen_bounds:tt)* };
        @generics_with_bounds = { $($gen_bounds:tt)* };
        @attrs = { $($attr:tt)* };
        $vis:vis tag $Name:ident : for<$($lt:lifetime),* $(,)?> $T1:ty $(=> $T2:ty)? $(where)? ; $($rest:tt)*
    } => {
        $(#$attr)*
        $vis struct $Name< $($lt_gen_bounds)* $($gen_bounds)* > where $($where)* {
            _lt: ( $($crate::__m::PhantomData<& $lt_gen ()>,)* ),
            _p: ( $($crate::__m::PhantomData<$gen>,)* ),
            _infallible: $crate::__m::Infallible,
        }

        impl < $($lt_gen_bounds)* $($gen_bounds)* > $crate::ResourceTag for $Name<$($lt_gen,)* $($gen,)*>
        where
            $($lt_gen: 'static,)*
            $($gen: 'static,)*
            $($where)*
        {
            $crate::__define_tag! { @associated_types for<$($lt),*> $T1 $(=> $T2)? }
        }

        $crate::define_tag! { $($rest)* }
    };

    // read a where clause bound and comma
    {
        @where = { $($where:tt)* };
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = $gen_bounds:tt;
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident : for<$($lt:lifetime),* $(,)?> $T1:ty $(=> $T2:ty)?
        where
            $T:ty : $bound:path ,
        $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = { $($where)* $T: $bound, };
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = $gen_bounds;
            @attrs = $attrs;
            $vis tag $Name : for<$($lt),*> $T1 $(=> $T2)? where $($rest)*
        }
    };
    // read a where clause token
    {
        @where = { $($where:tt)* };
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = $gen_bounds:tt;
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident : for<$($lt:lifetime),* $(,)?> $T1:ty $(=> $T2:ty)? where $where_token:tt
        $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = { $($where)* $where_token };
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = $gen_bounds;
            @attrs = $attrs;
            $vis tag $Name : for<$($lt),*> $T1 $(=> $T2)? where $($rest)*
        }
    };

    // handle a missing for<> qualifier
    {
        @where = $where:tt;
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = $gen_bounds:tt;
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident : $T1:ty $(=> $T2:ty)?
        ;
        $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = $where;
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = $gen_bounds;
            @attrs = $attrs;
            $vis tag $Name : for<> $T1 $(=> $T2)? ; $($rest)*
        }
    };
    {
        @where = $where:tt;
        @lt_generics = $lt_gen:tt;
        @generics = $gen:tt;
        @lt_generics_with_bounds = $lt_gen_bounds:tt;
        @generics_with_bounds = $gen_bounds:tt;
        @attrs = $attrs:tt;
        $vis:vis tag $Name:ident : $T1:ty $(=> $T2:ty)?
        where
        $($rest:tt)*
    } => {
        $crate::__define_tag! {
            @where = $where;
            @lt_generics = $lt_gen;
            @generics = $gen;
            @lt_generics_with_bounds = $lt_gen_bounds;
            @generics_with_bounds = $gen_bounds;
            @attrs = $attrs;
            $vis tag $Name : for<> $T1 $(=> $T2)? where $($rest)*
        }
    };

    { @associated_types for<$($lt:lifetime),*> $In:ty => $Out:ty } => {
        type Arg = $crate::TypeFn![for<$($lt),*> $In];
        type Out = $crate::TypeFn![for<$($lt),*> $Out];
    };
    { @associated_types for<$($lt:lifetime),*> $Out:ty } => {
        type Arg = ();
        type Out = $crate::TypeFn![for<$($lt),*> $Out];
    };
}
