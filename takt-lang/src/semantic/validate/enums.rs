//! Перечисления и битовые значения.
//!
//! Часть модуля `validate` (фича 0027: деление по логике).

use super::*;

/// Проверяет, что инициализатор переменной типа `bit` содержит допустимое значение.
///
/// Тип `bit` принимает только числовые значения `0` или `1`,
/// а также булевы литералы `true` / `false`.
/// Выражения, не являющиеся числовыми литералами (переменные, операции),
/// не проверяются статически.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если числовой литерал не равен 0 или 1.
fn check_bit_variable_value(
    name: &str,
    ty: &TypeNode,
    expr: &ExpressionNode,
    loc: Location,
) -> Result<(), Diagnostic> {
    if *ty == TypeNode::Bit
        && let ExpressionNode::Number(n) = expr
        && *n != 0
        && *n != 1
    {
        return Err(Diagnostic::error(
            loc,
            format!(
                "Переменная '{}' имеет тип bit, но инициализирована значением {} \
                 (допустимые числовые значения: 0 или 1)",
                name, n
            ),
        )
        .with_code("SE-035"));
    }
    Ok(())
}

/// Проверяет начальные значения `bit` у `Simple`- и `Const`-переменных.
///
/// **Порт исключён намеренно (фича 0070).** Инициализатор порта — это его
/// **адрес** (ADR 0020, поле `VariableNode::Port.expr` — «Адрес порта»), а не
/// значение: `in BTN: bit at 0x00100000;` есть адрес, и `check_bit_variable_value`
/// давал бы ложную `SE-035` «плохое значение бита». Адрес порта проверяет и
/// потребляет слой `address_map/resolve.rs` (`SE-052`/`SE-049` и пр.). Проверка
/// значения бита остаётся только там, где инициализатор — действительно значение
/// (`var`/`const`). Прежде порт проверялся наравне с ними — отсюда асимметрия
/// (`u8 := 0xADDR` проходил, `bit := 0xADDR` — нет).
///
/// Рекурсия по вложенным моделям не нужна — [`validate_model`] уже обходит
/// их самостоятельно, вызывая `validate_bit_values` для каждой вложенной модели.
///
/// # Ошибки
///
/// Пробрасывает [`Diagnostic`] из [`check_bit_variable_value`].
pub(super) fn validate_bit_values(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    // Накопление по объявлениям (фича 0151).
    let mut out = Vec::new();
    for var in borrowed.variables.values() {
        match var {
            VariableNode::Simple { name, ty, expr, .. }
            | VariableNode::Const { name, ty, expr, .. } => {
                out.extend(check_bit_variable_value(name, ty, expr, var.loc()).err());
            }
            // Порт: инициализатор — адрес, не значение (фича 0070); не проверяем.
            VariableNode::Port { .. } | VariableNode::Unresolved => {}
        }
    }
    out
}

/// Проверяет, что у каждого перечисления модели есть хотя бы один вариант
/// (`SE-105`, фича 0172).
///
/// ## Мотивация
///
/// Чем должна быть запись `enum E { }` — язык не говорил нигде: отказ давала
/// **грамматика** (`CommaOne<EnumVariant>`), то есть правило языка было
/// свойством квантификатора, а текст сообщения — внутренностью LR-разбора
/// (`SY-002` «нераспознанный токен '}', ожидалось identifier, "X", "F", …»).
/// ADR 0172 принял решение языка: **перечисление обязано иметь хотя бы один
/// вариант**, и высказывает его семантика — на объявлении, а не на первом
/// использовании.
///
/// ⚠️ Отказ стоит именно на **объявлении**. Без него использование пустого
/// перечисления давало бессодержательное `SE-043` «…не является вариантом
/// перечисления (допустимые варианты: )» — про пустой список автору не
/// сообщалось ничего.
///
/// ⚠️ Тип с пустым доменом не имеет представления ни в одной цели: `enum_facts`
/// отдаёт `None`, после чего `c`/`st`/`rust` молча берут 8 бит без знака, а
/// `sv` отвечает `SV-004`. Эта развилка сохраняется — её недостижимость из
/// исходника теперь держит `SE-105`, а не квантификатор грамматики.
///
/// Рекурсия по вложенным моделям не нужна — [`validate_model_all`] обходит их
/// сам (тот же довод, что у [`validate_bit_values`]).
///
/// # Ошибки
///
/// Возвращает по одной [`Diagnostic`] на каждое пустое перечисление
/// (накопление, правило фичи 0151).
pub(super) fn validate_empty_enums(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let borrowed = model.borrow();
    let mut out = Vec::new();
    for en in borrowed.enums.values() {
        if en.variants.is_empty() {
            out.push(
                Diagnostic::declaration_error(
                    en.loc,
                    format!(
                        "перечисление '{}' объявлено без вариантов: \
                         у перечисления обязан быть хотя бы один вариант \
                         (добавьте вариант либо удалите объявление)",
                        en.name
                    ),
                )
                .with_code("SE-105"),
            );
        }
    }
    out
}

/// Проверяет, что все переменные, тип которых — [`TypeNode::Enum`], ссылаются
/// на фактически объявленные перечисления.
///
/// ## Мотивация
///
/// `construct_type` не может проверить существование перечисления на этапе
/// построения дерева, поскольку перечисления и переменные обрабатываются в
/// одном проходе и могут идти в любом порядке. Эта функция выполняется после
/// полного построения дерева, когда `ModelNode::enums` уже заполнена.
///
/// ## Примеры
///
/// ```text
/// // Корректно: Color объявлен выше или ниже переменной
/// enum Color {
///     Red = 0,
///     Green = 1
/// }
/// var c: Color = 0;   // ✓
///
/// // Ошибка: Size не объявлен
/// var s: Size = 0;    // ✗ Ce4: перечисление 'Size' не объявлено
/// ```
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`]-ошибку при первой переменной с необъявленным типом enum.
pub(super) fn validate_enum_type_declarations(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    // Собираем (имя переменной, тип, loc) без удержания заимствования
    let vars: Vec<(String, TypeNode, Location)> = model
        .borrow()
        .variables
        .values()
        .filter_map(|var| match var {
            VariableNode::Simple { name, ty, .. }
            | VariableNode::Const { name, ty, .. }
            | VariableNode::Port { name, ty, .. } => Some((name.clone(), ty.clone(), var.loc())),
            VariableNode::Unresolved => None,
        })
        .collect();

    // Накопление по объявлениям (фича 0151): второе объявление с неизвестным
    // перечислением — самостоятельное нарушение, а не следствие первого.
    let mut out = Vec::new();
    for (var_name, ty, loc) in vars {
        if let TypeNode::Enum(enum_name) = &ty
            && model.borrow().search_enum(enum_name).is_none()
        {
            out.push(
                Diagnostic::declaration_error(
                    loc,
                    format!(
                        "переменная '{}' объявлена с типом '{}', \
                         но перечисление '{}' не найдено в области видимости",
                        var_name, enum_name, enum_name
                    ),
                )
                .with_code("SE-035"),
            );
        }
    }
    out
}

/// Проверяет инициализатор переменной типа enum.
///
/// Если переменная имеет тип `Enum(name)` и инициализирована числовым литералом,
/// числовое значение должно быть допустимым вариантом перечисления.
///
/// # Ошибки
///
/// Возвращает [`Diagnostic`], если числовой литерал не является вариантом перечисления.
fn check_enum_variable_value(
    name: &str,
    ty: &TypeNode,
    expr: &ExpressionNode,
    loc: Location,
    model: &Rc<RefCell<ModelNode>>,
) -> Result<(), Diagnostic> {
    if let TypeNode::Enum(enum_name) = ty
        && let ExpressionNode::Number(n) = expr
        && !is_valid_enum_value(enum_name, *n, model)
    {
        let valid_values: Vec<String> = model
            .borrow()
            .search_enum(enum_name)
            .map(|e| {
                e.variants
                    .iter()
                    .map(|(vn, vv)| format!("{}={}", vn, vv))
                    .collect()
            })
            .unwrap_or_default();
        return Err(Diagnostic::error(
            loc,
            format!(
                "переменная '{}' имеет тип '{}', но инициализирована значением {} \
                 — не является вариантом перечисления (допустимые варианты: {})",
                name,
                enum_name,
                n,
                valid_values.join(", ")
            ),
        )
        .with_code("SE-043"));
    }
    Ok(())
}

/// Проверяет все переменные модели на корректность начальных значений для enum-типов (NI6).
///
/// Аналогично [`validate_bit_values`], проверяет только `Simple`-, `Const`-переменные.
/// Порты не проверяются — адресное значение не является значением перечисления.
///
/// # Ошибки
///
/// Пробрасывает [`Diagnostic`] из [`check_enum_variable_value`].
pub(super) fn validate_enum_values(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    // Собираем данные без удержания заимствования
    let vars: Vec<(String, TypeNode, ExpressionNode, Location)> = model
        .borrow()
        .variables
        .values()
        .filter_map(|var| match var {
            VariableNode::Simple { name, ty, expr, .. }
            | VariableNode::Const { name, ty, expr, .. } => {
                Some((name.clone(), ty.clone(), expr.clone(), var.loc()))
            }
            _ => None,
        })
        .collect();
    // Накопление по объявлениям (фича 0151).
    let mut out = Vec::new();
    for (name, ty, expr, loc) in &vars {
        out.extend(check_enum_variable_value(name, ty, expr, *loc, &model).err());
    }
    out
}

/// Проверяет, является ли числовое значение допустимым для перечисления.
///
/// Возвращает `true`, если значение `n` совпадает с числовым значением
/// хотя бы одного варианта перечисления `enum_name` в контексте модели.
/// Если перечисление не найдено — не блокируем (ошибка другой проверки).
fn is_valid_enum_value(enum_name: &str, n: i128, model: &Rc<RefCell<ModelNode>>) -> bool {
    if let Some(enum_node) = model.borrow().search_enum(enum_name) {
        enum_node.variants.iter().any(|(_, val)| *val == n)
    } else {
        true
    }
}

/// Рекурсивно обходит выражения и проверяет присваивания переменным типа enum (NI6).
fn check_enum_expr(
    expr: &ExpressionNode,
    model: &Rc<RefCell<ModelNode>>,
    out: &mut Vec<Diagnostic>,
) {
    match expr {
        ExpressionNode::Assign(left, right) => {
            if let ExpressionNode::Variable(var_rc) = left.as_ref() {
                let borrowed = var_rc.borrow();
                if let VariableNode::Simple { name, ty, .. }
                | VariableNode::Port { name, ty, .. }
                | VariableNode::Const { name, ty, .. } = &*borrowed
                    && let TypeNode::Enum(enum_name) = ty
                    && let ExpressionNode::Number(n) = right.as_ref()
                    && !is_valid_enum_value(enum_name, *n, model)
                {
                    let var_loc = borrowed.loc();
                    let valid_values: Vec<String> = model
                        .borrow()
                        .search_enum(enum_name)
                        .map(|e| {
                            e.variants
                                .iter()
                                .map(|(vn, vv)| format!("{}={}", vn, vv))
                                .collect()
                        })
                        .unwrap_or_default();
                    out.push(
                        Diagnostic::type_error(
                            var_loc,
                            format!(
                                "присваивание переменной '{}' типа '{}' \
                                 значения {} недопустимо — не является вариантом \
                                 перечисления (допустимые варианты: {})",
                                name,
                                enum_name,
                                n,
                                valid_values.join(", ")
                            ),
                        )
                        .with_code("SE-043"),
                    );
                }
            }
            check_enum_expr(left, model, out);
            check_enum_expr(right, model, out);
        }
        ExpressionNode::Parenthesis(e)
        | ExpressionNode::BitAccess(e, _)
        | ExpressionNode::Not(e)
        | ExpressionNode::BitwiseNot(e)
        | ExpressionNode::UnaryPlus(e)
        | ExpressionNode::Negate(e)
        | ExpressionNode::Cast(e, _) => {
            check_enum_expr(e, model, out);
        }
        ExpressionNode::CodeBlock(e, _) => {
            check_enum_expr(e, model, out);
        }
        ExpressionNode::NamedFunctionBox(e, _) => {
            check_enum_expr(e, model, out);
        }
        ExpressionNode::Power(l, r)
        | ExpressionNode::Multiply(l, r)
        | ExpressionNode::Divide(l, r)
        | ExpressionNode::Modulo(l, r)
        | ExpressionNode::Add(l, r)
        | ExpressionNode::Subtract(l, r)
        | ExpressionNode::ShiftLeft(l, r)
        | ExpressionNode::ShiftRight(l, r)
        | ExpressionNode::BitwiseAnd(l, r)
        | ExpressionNode::BitwiseXor(l, r)
        | ExpressionNode::BitwiseOr(l, r)
        | ExpressionNode::Less(l, r)
        | ExpressionNode::More(l, r)
        | ExpressionNode::LessEqual(l, r)
        | ExpressionNode::MoreEqual(l, r)
        | ExpressionNode::Equal(l, r)
        | ExpressionNode::NotEqual(l, r)
        | ExpressionNode::And(l, r)
        | ExpressionNode::Or(l, r) => {
            check_enum_expr(l, model, out);
            check_enum_expr(r, model, out);
        }
        ExpressionNode::ConditionalOperator(c, t, e) => {
            check_enum_expr(c, model, out);
            check_enum_expr(t, model, out);
            check_enum_expr(e, model, out);
        }
        ExpressionNode::Function(_, args)
        | ExpressionNode::Array(args)
        | ExpressionNode::Initializer(args) => {
            for a in args {
                check_enum_expr(a, model, out);
            }
        }
        _ => {}
    }
}

/// Рекурсивно обходит операторы и проверяет присваивания переменным типа enum (NI6).
fn check_enum_stmt(
    stmt: &StatementNode,
    model: &Rc<RefCell<ModelNode>>,
    out: &mut Vec<Diagnostic>,
) {
    match stmt {
        StatementNode::Expression(expr, _) => check_enum_expr(expr, model, out),
        StatementNode::Block(stmts) => {
            for s in stmts {
                check_enum_stmt(s, model, out);
            }
        }
        StatementNode::If {
            cond, then_, else_, ..
        } => {
            check_enum_expr(cond, model, out);
            check_enum_stmt(then_, model, out);
            if let Some(e) = else_ {
                check_enum_stmt(e, model, out);
            }
        }
        StatementNode::Loop { cond, body, .. } => {
            if let Some(c) = cond {
                check_enum_expr(c, model, out);
            }
            check_enum_stmt(body, model, out);
        }
        StatementNode::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(i) = init {
                check_enum_stmt(i, model, out);
            }
            if let Some(c) = cond {
                check_enum_expr(c, model, out);
            }
            if let Some(s) = step {
                check_enum_expr(s, model, out);
            }
            check_enum_stmt(body, model, out);
        }
        StatementNode::Return(Some(e), _) => check_enum_expr(e, model, out),
        StatementNode::Variable(_, _, Some(e), _) => check_enum_expr(e, model, out),
        _ => {}
    }
}

/// Рекурсивно собирает ошибки NI6 для модели и всех вложенных моделей.
fn collect_enum_type_safety(model: &Rc<RefCell<ModelNode>>, out: &mut Vec<Diagnostic>) {
    // Собираем данные без удержания заимствования
    let vars: Vec<(String, TypeNode, ExpressionNode, Location)> = model
        .borrow()
        .variables
        .values()
        .filter_map(|var| match var {
            VariableNode::Simple { name, ty, expr, .. }
            | VariableNode::Const { name, ty, expr, .. } => {
                Some((name.clone(), ty.clone(), expr.clone(), var.loc()))
            }
            _ => None,
        })
        .collect();

    for (name, ty, expr, loc) in &vars {
        if let TypeNode::Enum(enum_name) = ty
            && let ExpressionNode::Number(n) = expr
            && !is_valid_enum_value(enum_name, *n, model)
        {
            let valid_values: Vec<String> = model
                .borrow()
                .search_enum(enum_name)
                .map(|e| {
                    e.variants
                        .iter()
                        .map(|(vn, vv)| format!("{}={}", vn, vv))
                        .collect()
                })
                .unwrap_or_default();
            out.push(
                Diagnostic::type_error(
                    *loc,
                    format!(
                        "переменная '{}' имеет тип '{}', но инициализирована \
                         значением {} — не является вариантом перечисления \
                         (допустимые варианты: {})",
                        name,
                        enum_name,
                        n,
                        valid_values.join(", ")
                    ),
                )
                .with_code("SE-043"),
            );
        }
    }

    let named_blocks: Vec<StatementNode> = model
        .borrow()
        .named_blocks
        .iter()
        .filter_map(|b| b.statement().cloned())
        .collect();
    for stmt in &named_blocks {
        check_enum_stmt(stmt, model, out);
    }

    let state_blocks: Vec<StatementNode> = model
        .borrow()
        .states
        .values()
        .flat_map(|s| {
            s.named_blocks()
                .iter()
                .filter_map(|b| b.statement().cloned())
                .collect::<Vec<_>>()
        })
        .collect();
    for stmt in &state_blocks {
        check_enum_stmt(stmt, model, out);
    }

    let nested: Vec<Rc<RefCell<ModelNode>>> =
        model.borrow().models.values().map(Rc::clone).collect();
    for m in nested {
        collect_enum_type_safety(&m, out);
    }
}

/// NI6: Возвращает ошибки типобезопасных операций с перечислениями.
///
/// Проверяет, что при присваивании переменной типа enum значение является
/// одним из допустимых вариантов перечисления. Проверяются:
///
/// - Инициализаторы переменных объявленных как `var x: Direction = 0;`
/// - Присваивания в именованных блоках `always`, `enter`, `exit`
///
/// Статически проверяются только числовые литералы. Присваивания через
/// переменные или функции не проверяются.
///
/// # Примеры (Takt)
///
/// ```text
/// // Корректно: 0 — значение варианта North
/// enum Direction {
///     North = 0,
///     South = 1
/// }
/// var dir: Direction = 0;
///
/// // Ошибка NI6: 99 не является вариантом Direction
/// var dir: Direction = 99;
/// ```
///
/// # Возвращаемое значение
///
/// Вектор [`Diagnostic`] уровня `Error` с типом [`TypeError`](crate::diagnostics::ErrorType::TypeError).
/// Пустой вектор означает отсутствие нарушений.
pub fn check_enum_type_safety(model: Rc<RefCell<ModelNode>>) -> Vec<Diagnostic> {
    let mut errors = Vec::new();
    collect_enum_type_safety(&model, &mut errors);
    errors
}
