pub trait ImplyBase<T: ?Sized> {
    type Impl: ?Sized;
}

pub trait Implies<T: ?Sized, _U: ?Sized = ()>: ImplyBase<T, Impl = T> {}

impl<X: ?Sized, T: ?Sized> ImplyBase<T> for X {
    type Impl = T;
}

impl<X: ?Sized, T: ?Sized, _U: ?Sized> Implies<T, _U> for X {}
