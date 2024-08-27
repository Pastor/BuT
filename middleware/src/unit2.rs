use crate::unit2::variables::Variable;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::io::{Stdout, Write};
use std::ops::Deref;
use std::rc::Rc;

const PADDING: usize = 2usize;

#[derive(Default, Debug, Clone)]
pub struct UnitState {
    variables: HashMap<String, Variable>,
}

type Data = UnitState;

pub trait VariablesUnit {
    fn get_variable(&self, key: &str) -> Option<Variable>;
    fn put_variable(&self, key: &str, variable: Variable) -> Option<Variable>;
}

pub trait Unit: VariablesUnit + Debug {
    type State: Sized + Default;
    fn new() -> Self
    where
        Self: Sized;
    fn create(&self, state: Self::State) -> Box<dyn Unit<State=Self::State>>;
    fn update(&mut self, cb: Box<dyn FnMut(Self::State) -> Self::State>);
    fn size(&self) -> usize;
    fn print(&self);
    fn visit(&self, pad: usize, visitor: &mut dyn Visitor<State=Self::State>);
}

pub trait Visitor {
    type State: Sized + Debug;
    fn visit_data(&mut self, pad: usize, state: Self::State);
    fn visit_unit(&mut self, pad: usize, unit: Box<dyn Unit<State=Self::State>>);
}

pub fn default<T: Sized>() -> Box<dyn Unit<State=Data>> {
    let unit = <UnitImpl as Unit>::new();
    Box::new(unit)
}

mod variables {
    use std::ops::Add;

    #[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Default)]
    pub(crate) enum Variable {
        #[default]
        None,
        String(String),
        Number(i64),
        Boolean(bool),
    }

    impl Variable {
        pub fn name(&self) -> &'static str {
            match self {
                Variable::None => "none",
                Variable::String(_) => "str",
                Variable::Number(_) => "num",
                Variable::Boolean(_) => "bool",
            }
        }
    }

    impl Add for Variable {
        type Output = Variable;

        fn add(self, rhs: Self) -> Self::Output {
            match (rhs, self) {
                (Variable::Number(n), Variable::Number(v)) => Variable::Number(n + v),
                (_, v) => v
            }
        }
    }

    impl From<bool> for Variable {
        fn from(value: bool) -> Self {
            Self::Boolean(value)
        }
    }

    impl From<i64> for Variable {
        fn from(value: i64) -> Self {
            Self::Number(value)
        }
    }

    impl From<&str> for Variable {
        fn from(value: &str) -> Self {
            Self::String(value.to_string())
        }
    }

    impl From<String> for Variable {
        fn from(value: String) -> Self {
            Self::String(value)
        }
    }

    impl From<()> for Variable {
        fn from(_: ()) -> Self {
            Variable::None
        }
    }
}

mod private {
    use crate::unit2::Data;
    use std::cell::RefCell;
    use std::fmt::{Debug, Formatter, Write};
    use std::rc::Rc;
    use std::sync::Mutex;

    pub(crate) struct UnitPrivate {
        pub parent: Option<*mut UnitPrivate>,
        pub children: Vec<Rc<RefCell<UnitPrivate>>>,
        pub data: Mutex<Data>,
    }

    impl UnitPrivate {
        pub fn new(data: Data) -> Self {
            Self {
                parent: None,
                children: vec![],
                data: Mutex::new(data),
            }
        }
        pub fn add(unit: &RefCell<UnitPrivate>, data: Data) -> Rc<RefCell<UnitPrivate>> {
            let child = Rc::new(RefCell::new(UnitPrivate::new(data)));
            unit.borrow_mut().children.push(Rc::clone(&child));
            child.borrow_mut().parent = Some(unit.as_ptr());
            child
        }
    }

    impl Debug for UnitPrivate {
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
            f.write_fmt(format_args!("Unit(parent: {:?}, children: [{}])", self.parent, output))
        }
    }
}

struct UnitImpl {
    inner: Rc<RefCell<private::UnitPrivate>>,
}

impl UnitImpl {
    fn from(data: &Rc<RefCell<private::UnitPrivate>>) -> Self {
        Self {
            inner: Rc::clone(&data),
        }
    }
}

impl Debug for UnitImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", &self.inner.borrow()))
    }
}

impl Visitor for Stdout {
    type State = Data;

    fn visit_data(&mut self, pad: usize, state: Self::State) {
        let _ = self.write_fmt(format_args!("{:pad$}{:?}\n", ' ', state, pad = pad));
    }

    fn visit_unit(&mut self, pad: usize, child: Box<dyn Unit<State=Self::State>>) {
        // let _ = self.write_fmt(format_args!("{:pad$}{:?}\n", ' ', child, pad = pad));
        child.visit(pad + PADDING, self)
    }
}

impl VariablesUnit for UnitImpl {
    fn get_variable(&self, key: &str) -> Option<Variable> {
        let binding = self.inner.borrow();
        let data = binding.data.lock().unwrap();
        let val = data.variables.get(key);
        if val.is_none() && binding.parent != None {
            drop(data);
            return binding.parent.and_then(|parent| unsafe {
                let state = &*parent;
                let mut data = state.data.lock().unwrap();
                data.variables.get(key).cloned()
            });
        }
        val.cloned()
    }

    fn put_variable(&self, key: &str, variable: Variable) -> Option<Variable> {
        let binding = self.inner.borrow();
        let mut data = binding.data.lock().unwrap();
        data.variables.insert(key.to_string(), variable)
    }
}

impl Unit for UnitImpl {
    type State = Data;

    fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(private::UnitPrivate::new(Self::State::default())))
        }
    }

    fn create(&self, state: Self::State) -> Box<dyn Unit<State=Data>> {
        let add = private::UnitPrivate::add(&self.inner, state);
        Box::new(UnitImpl::from(&add))
    }

    fn update(&mut self, mut update: Box<dyn FnMut(Self::State) -> Self::State>) {
        let binding = self.inner.borrow_mut();
        let mut data = binding.data.lock().unwrap();
        let ret = update(data.clone());
        *data = ret
    }

    fn size(&self) -> usize {
        self.inner.borrow_mut().children.len()
    }

    fn print(&self) {
        self.visit(0usize, &mut std::io::stdout() as &mut dyn Visitor<State=Data>);
    }

    fn visit(&self, pad: usize, visitor: &mut dyn Visitor<State=Data>) {
        let binding = self.inner.borrow();
        let state = binding.data.lock().unwrap();
        visitor.visit_data(pad, state.clone());
        self.inner.borrow().children.iter().for_each(|unit| {
            visitor.visit_unit(pad + PADDING, Box::new(UnitImpl::from(&unit)));
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{default, Data, Unit};
    use crate::unit2::variables::Variable;

    #[test]
    fn it_unit() {
        let mut unit = default::<Data>();
        let child = unit.create(Data::default());
        assert_eq!(unit.size(), 1);
        assert_eq!(child.size(), 0);
        let child = child.create(Data::default());
        assert_eq!(child.size(), 0);
        let child = unit.create(Data::default());
        assert_eq!(child.size(), 0);
        let child = unit.create(Data::default());
        assert_eq!(child.size(), 0);
        unit.update(Box::new(|mut state| {
            state.variables.insert("x".to_owned(), Variable::Number(2));
            state
        }));
        let op = child.get_variable("x");
        assert_eq!(op, Some(Variable::Number(2)));
        child.put_variable("x", Variable::Number(41) + 1.into());
        let op = child.get_variable("x");
        assert_eq!(op, Some(Variable::Number(42)));
        let op = unit.get_variable("x");
        assert_eq!(op, Some(Variable::Number(2)));
        unit.print();
    }
}


