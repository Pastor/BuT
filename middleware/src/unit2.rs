use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::io::{Stdout, Write};
use std::ops::Deref;

pub trait Hierarchical: Debug {
    fn new() -> Self
    where
        Self: Sized;
    fn add_children(&self) -> Box<dyn Hierarchical>;
    fn size(&self) -> usize;
    fn print(&self);
    fn visit(&self, visitor: &mut dyn Visitor);
}

pub trait Visitor {
    fn visit(&mut self, unit: &dyn Hierarchical);
}

pub fn default() -> Box<dyn Hierarchical> {
    let unit = <HierarchicalImpl as Hierarchical>::new();
    Box::new(unit)
}

mod private {
    use std::cell::RefCell;
    use std::fmt::{Debug, Formatter, Write};
    use std::rc::{Rc, Weak};

    #[derive(Clone)]
    pub(crate) struct HierarchicalPrivate {
        parent: Option<Weak<RefCell<HierarchicalPrivate>>>,
        pub children: Vec<Rc<RefCell<HierarchicalPrivate>>>,
    }

    impl HierarchicalPrivate {
        pub fn new() -> Self {
            Self {
                parent: None,
                children: vec![],
            }
        }
        pub fn add(unit: &RefCell<HierarchicalPrivate>) -> Rc<RefCell<HierarchicalPrivate>> {
            let child = Rc::new(RefCell::new(HierarchicalPrivate::new()));
            unit.borrow_mut().children.push(Rc::clone(&child));
            let parent: Weak<RefCell<HierarchicalPrivate>> = Weak::new();
            parent.upgrade().replace(Rc::new(RefCell::clone(&unit)));
            child.borrow_mut().parent = Some(parent);
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

struct HierarchicalImpl {
    inner: RefCell<private::HierarchicalPrivate>,
}

impl HierarchicalImpl {
    fn from(data: &RefCell<private::HierarchicalPrivate>) -> Self {
        Self {
            inner: RefCell::clone(&data),
        }
    }
}

impl Debug for HierarchicalImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.inner.borrow()))
    }
}

impl Visitor for Stdout {
    fn visit(&mut self, unit: &dyn Hierarchical) {
        let _ = self.write_fmt(format_args!("{:?}\n", unit));
    }
}

impl Hierarchical for HierarchicalImpl {
    fn new() -> Self {
        Self {
            inner: RefCell::new(private::HierarchicalPrivate::new())
        }
    }

    fn add_children(&self) -> Box<dyn Hierarchical> {
        let add = private::HierarchicalPrivate::add(&self.inner);
        Box::new(HierarchicalImpl::from(add.as_ref()))
    }

    fn size(&self) -> usize {
        self.inner.borrow_mut().children.len()
    }

    fn print(&self) {
        self.visit(&mut std::io::stdout() as &mut dyn Visitor);
    }

    fn visit(&self, visitor: &mut dyn Visitor) {
        visitor.visit(self);
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
        let unit = default();
        let child = unit.add_children();
        assert_eq!(unit.size(), 1);
        assert_eq!(child.size(), 0);
        let child = child.add_children();
        assert_eq!(child.size(), 0);
        let child = unit.add_children();
        assert_eq!(child.size(), 0);
        let child = unit.add_children();
        assert_eq!(child.size(), 0);
        unit.print();
    }
}


