use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::io::{Stdout, Write};
use std::ops::Deref;
use std::rc::Rc;

pub trait Hierarchical: Debug {
    type State: Sized;
    fn new() -> Self
    where
        Self: Sized;
    fn create(&self, state: Self::State) -> Box<dyn Hierarchical<State=Self::State>>;
    fn update_mut(&mut self, cb: Box<dyn FnMut(Self::State) -> Self::State>);
    fn size(&self) -> usize;
    fn print(&self);
    fn visit(&self, visitor: &mut dyn Visitor<State=Self::State>);
}

pub trait Visitor {
    type State: Sized + Debug;
    fn visit(&mut self, state: Self::State);
}

pub fn default<T: Sized>() -> Box<dyn Hierarchical<State=()>> {
    let unit = <HierarchicalWrapper as Hierarchical>::new();
    Box::new(unit)
}

mod private {
    use std::cell::RefCell;
    use std::fmt::{Debug, Formatter, Write};
    use std::rc::Rc;
    use std::sync::Mutex;

    pub(crate) struct HierarchicalPrivate {
        parent: Option<*mut HierarchicalPrivate>,
        pub children: Vec<Rc<RefCell<HierarchicalPrivate>>>,
        pub data: Mutex<()>,
    }

    impl HierarchicalPrivate {
        pub fn new(data: ()) -> Self {
            Self {
                parent: None,
                children: vec![],
                data: Mutex::new(data),
            }
        }
        pub fn add(unit: &RefCell<HierarchicalPrivate>, data: ()) -> Rc<RefCell<HierarchicalPrivate>> {
            let child = Rc::new(RefCell::new(HierarchicalPrivate::new(data)));
            unit.borrow_mut().children.push(Rc::clone(&child));
            child.borrow_mut().parent = Some(unit.as_ptr());
            child
        }
    }

    impl Debug for HierarchicalPrivate {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let mut output = String::new();
            let mut idx = 0;
            for child in &self.children {
                if idx > 0 {
                    output.push_str(", ");
                }
                output.write_fmt(format_args!("\n  {:?}", child.borrow()))?;
                idx += 1;
            }
            if idx > 0 {
                output.push_str("\n");
            }
            f.write_fmt(format_args!("Hierarchical(parent: {:?}, children: [{}])", self.parent, output))
        }
    }
}

struct HierarchicalWrapper {
    inner: Rc<RefCell<private::HierarchicalPrivate>>,
}

impl HierarchicalWrapper {
    fn from(data: &Rc<RefCell<private::HierarchicalPrivate>>) -> Self {
        Self {
            inner: Rc::clone(&data),
        }
    }
}

impl Debug for HierarchicalWrapper {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.inner.borrow()))
    }
}

impl Visitor for Stdout {
    type State = ();

    fn visit(&mut self, state: Self::State) {
        let _ = self.write_fmt(format_args!("{:?}\n", state));
    }
}

impl Hierarchical for HierarchicalWrapper {
    type State = ();

    fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(private::HierarchicalPrivate::new(())))
        }
    }

    fn create(&self, state: Self::State) -> Box<dyn Hierarchical<State=()>> {
        let add = private::HierarchicalPrivate::add(&self.inner, state);
        Box::new(HierarchicalWrapper::from(&add))
    }

    fn update_mut(&mut self, mut cb: Box<dyn FnMut(Self::State) -> Self::State>) {
        let binding = self.inner.borrow_mut();
        let mut data = binding.data.lock().unwrap();
        let ret = cb(*data);
        *data = ret
    }

    fn size(&self) -> usize {
        self.inner.borrow_mut().children.len()
    }

    fn print(&self) {
        self.visit(&mut std::io::stdout() as &mut dyn Visitor<State=()>);
    }

    fn visit(&self, visitor: &mut dyn Visitor<State=()>) {
        let binding = self.inner.borrow();
        let state = binding.data.lock().unwrap();
        visitor.visit(*state);
        // self.inner.borrow().children.iter().for_each(|mut unit| {
        //     let h = &mut HierarchicalImpl::from(unit);
        //     visitor.visit(h as &dyn Hierarchical);
        // })
    }
}

#[cfg(test)]
mod tests {
    use super::{default, Hierarchical};

    #[test]
    fn it_unit() {
        let mut unit = default::<()>();
        let child = unit.create(());
        assert_eq!(unit.size(), 1);
        assert_eq!(child.size(), 0);
        let child = child.create(());
        assert_eq!(child.size(), 0);
        let child = unit.create(());
        assert_eq!(child.size(), 0);
        let child = unit.create(());
        assert_eq!(child.size(), 0);
        unit.print();
        unit.update_mut(Box::new(move |state| {
            state
        }));
        unit.print();
    }
}


