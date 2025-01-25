use crate::{Lt, Provide, ProvideRef, Query};
use alloc::{boxed::Box, rc::Rc, string::String, sync::Arc};

/// Provides values from a [`Box`].
pub trait ProvideBox<LTail: Lt = ()> {
    fn provide_box<'this>(
        self: Box<Self>,
        query: &mut Query<'_, LTail>,
    ) -> Option<Box<dyn ProvideBox<LTail> + 'this>>
    where
        Self: 'this;
    fn provide_box_send<'this>(
        self: Box<Self>,
        query: &mut Query<'_, LTail>,
    ) -> Option<Box<dyn ProvideBox<LTail> + 'this + Send>>
    where
        Self: 'this + Send;
    fn provide_box_send_sync<'this>(
        self: Box<Self>,
        query: &mut Query<'_, LTail>,
    ) -> Option<Box<dyn ProvideBox<LTail> + 'this + Send + Sync>>
    where
        Self: 'this + Send + Sync;
}

impl<P: Provide<L>, L: Lt> ProvideBox<L> for P {
    fn provide_box<'this>(
        self: Box<Self>,
        query: &mut Query<'_, L>,
    ) -> Option<Box<dyn ProvideBox<L> + 'this>>
    where
        Self: 'this,
    {
        (*self).provide(query).map(|x| Box::new(x) as _)
    }

    fn provide_box_send<'this>(
        self: Box<Self>,
        query: &mut Query<'_, L>,
    ) -> Option<Box<dyn ProvideBox<L> + 'this + Send>>
    where
        Self: 'this + Send,
    {
        (*self).provide(query).map(|x| Box::new(x) as _)
    }

    fn provide_box_send_sync<'this>(
        self: Box<Self>,
        query: &mut Query<'_, L>,
    ) -> Option<Box<dyn ProvideBox<L> + 'this + Send + Sync>>
    where
        Self: 'this + Send + Sync,
    {
        (*self).provide(query).map(|x| Box::new(x) as _)
    }
}

impl<'data, L: Lt + 'data> Provide<L> for Box<dyn ProvideBox<L> + 'data> {
    fn provide(self, query: &mut Query<'_, L>) -> Option<Self> {
        self.provide_box(query)
    }
}

impl<P: ?Sized + ProvideRef<LTail>, LTail: Lt> ProvideRef<LTail> for Box<P> {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        P::provide_ref(self, query);
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        P::provide_mut(self, query);
    }
}

impl<P: ?Sized + ProvideRef<LTail>, LTail: Lt> ProvideRef<LTail> for Rc<P> {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        P::provide_ref(self, query);
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        if let Some(this) = Rc::get_mut(self) {
            P::provide_mut(this, query);
        }
    }
}

impl<P: ?Sized + ProvideRef<LTail>, LTail: Lt> ProvideRef<LTail> for Arc<P> {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        P::provide_ref(self, query);
    }

    fn provide_mut<'this>(&'this mut self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        if let Some(this) = Arc::get_mut(self) {
            P::provide_mut(this, query);
        }
    }
}

impl<L: Lt> Provide<L> for String {
    fn provide(self, query: &mut Query<L>) -> Option<Self> {
        query
            .using(self)
            .put_value(|this| this)
            .put_value(|this| this.into_boxed_str())
            .put_value(|this| this.into_bytes())
            .put_value(|this| this.into_bytes().into_boxed_slice())
            .finish()
    }
}

impl<LTail: Lt> ProvideRef<LTail> for String {
    fn provide_ref<'this>(&'this self, query: &mut Query<'_, Lt!['this, ..LTail]>) {
        str::provide_ref(self, query)
    }
}
