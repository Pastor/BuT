#[cfg(test)]
pub mod tests {
    use crate::diagnostics::Location;
    use crate::semantic::extend::Extend;
    use crate::semantic::{ModelNode, ModelOrigin};
    use std::cell::RefCell;
    use std::collections::BTreeMap;
    use std::rc::Rc;
    pub const SRC: &str = r#"
model A {
    start Start;
}
model B {
    start Start;
}
start Entry = A | B | (A + B) {
    next Next1;
}
state Next1 = A + B + (A | B) {
    next Next2;
}
state Next2 = A + (B | A) + B {
    next Next3;
}
state Next3 = A + (B + A) + B {
    next Next4;
}
state Next4 = A + (B + A) + (B | A) {
    next Next5;
}
state Next5 = (A | B) + (A + B) {
    next Next6;
}
state Next6 = (A | B) + (A + B) + (A | B) {
    next Next7;
}
state Next7 = (A | B) + (A + B) + (A | B) + (A + B) {
    next Next8;
}
state Next8 = (A | B) + (A + B) + (A | B) + (A + B) + (A | B) {
    next Next9;
}
state Next9 = (A | B) + (A + B) + (A | B) + (A + B) + (A | B) + (A + B) {
    next Next10;
}
state Next10 = (A | B) + (A + B) + (A | B) + (A + B) + (A | B) + (A + B) + (A + B);
"#;

    /// Собирает пустой узел модели с именем и родителем - фабрика **для тестов**.
    ///
    /// Место фабрики - в тестовом модуле: там `dead_code` говорит правду, а `pub`
    /// рабочего кода его глушил.
    pub fn model_node(
        name: &str,
        parent: Option<Rc<RefCell<ModelNode>>>,
    ) -> Rc<RefCell<ModelNode>> {
        let model = ModelNode {
            name: Some(name.to_string()),
            loc: Location::Codegen,
            upper: parent.as_ref().map(Rc::downgrade),
            models: Default::default(),
            named_blocks: vec![],
            functions: Default::default(),
            variables: Default::default(),
            parameters: Vec::new(),
            types: Default::default(),
            type_locs: Default::default(),
            raw_type_defs: Default::default(),
            named_block_raw: vec![],
            conditions: Default::default(),
            enums: Default::default(),
            structs: BTreeMap::new(),
            states: BTreeMap::new(),
            implements: Extend::None,
            doc: Vec::new(),
            docs: BTreeMap::new(),
            formulas: Vec::new(),
            address_defs: Vec::new(),
            origin: ModelOrigin::Local,
            clock_hz: None,
        };
        let model = Rc::new(RefCell::new(model));
        if let Some(parent) = &parent {
            parent
                .borrow_mut()
                .models
                .insert(name.to_string(), Rc::clone(&model));
        }
        model
    }
}
