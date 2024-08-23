use std::cell::RefCell;

pub trait Unit {
    fn new() -> Self
    where
        Self: Sized;
    fn add_new_unit(&self) -> Box<dyn Unit>;
    fn size(&self) -> usize;
}

pub fn default() -> Box<dyn Unit> {
    let unit = <UnitImpl as Unit>::new();
    Box::new(unit)
}

mod private {
    use std::cell::RefCell;
    use std::rc::{Rc, Weak};

    #[derive(Clone)]
    pub(crate) struct Unit {
        parent: Option<Weak<RefCell<Unit>>>,
        pub children: Vec<Rc<RefCell<Unit>>>,
    }


    impl Unit {
        pub fn new() -> Self {
            Self {
                parent: None,
                children: vec![],
            }
        }
        pub fn add(unit: &RefCell<Unit>) -> Rc<RefCell<Unit>> {
            let child = Rc::new(RefCell::new(Unit::new()));
            unit.borrow_mut().children.push(Rc::clone(&child));
            let parent: Weak<RefCell<Unit>> = Weak::new();
            parent.upgrade().replace(Rc::new(RefCell::clone(&unit)));
            child.borrow_mut().parent = Some(parent);
            child
        }
    }
}

struct UnitImpl {
    internal: RefCell<private::Unit>,
}

impl UnitImpl {
    fn from(data: &RefCell<private::Unit>) -> Self {
        Self {
            internal: RefCell::clone(&data),
        }
    }
}

impl Unit for UnitImpl {
    fn new() -> Self {
        Self {
            internal: RefCell::new(private::Unit::new())
        }
    }

    fn add_new_unit(&self) -> Box<dyn Unit> {
        let add = private::Unit::add(&self.internal);
        Box::new(UnitImpl::from(add.as_ref()))
    }

    fn size(&self) -> usize {
        self.internal.borrow_mut().children.len()
    }
}

#[cfg(test)]
mod tests {
    use super::{default, Unit};

    #[test]
    fn it_unit() {
        let unit = default();
        let child = unit.add_new_unit();
        assert_eq!(child.size(), 0);
        assert_eq!(unit.size(), 1);
    }
}


