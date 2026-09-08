//! Такт симуляции - исполнение одного шага [`Unit`].
//!
//! Выделен из `unit/mod.rs`: модуль подошёл к пределу размера (988 строк при лимите
//! 1000 в `scripts/check-module-size.sh`), а правит именно такт. Вынос **чистый**:
//! поведение не меняется, границы - по теме ("что происходит за один такт"), как в
//! `semantic/validate/`.
//!
//! Здесь живёт диспетчеризация такта по форме узла (`Node`/`Parallel`/ `Sequential`),
//! проверка инвариантов и вход в стартовое состояние. Наблюдение значений, композиция
//! юнитов и сбор трасс остались в `mod.rs`.

use super::*;

impl Unit {
    /// Один такт симуляции - **жёсткий** режим (умолчание): нарушение инварианта
    /// останавливает прогон (`Failed`, `SIM-025`), совпадая с `assert()` -> `abort()` в
    /// порождённом C. Публичный контракт; через него идут все потактовые сверки с C и
    /// корпус.
    pub fn tick(&mut self) -> TickResult {
        self.tick_mode(false)
    }

    /// Один такт в **мягком** режиме: нарушение инварианта
    /// **записывается** (в `Node.invariant_violations`) и такт **продолжается**
    /// (иначе состояние не сменится -> ливлок). Осмыслен только для отладки -
    /// сверки с C у него нет (C бы уже упал). Нарушения сливает `runner`
    /// (`take_invariant_violations`).
    pub fn tick_soft(&mut self) -> TickResult {
        self.tick_mode(true)
    }

    fn tick_mode(&mut self, soft: bool) -> TickResult {
        let result = self.tick_body(soft);
        // Счётчик тактов состояния растёт в конце такта (см.
        self.advance_state_ticks();
        result
    }

    fn tick_body(&mut self, soft: bool) -> TickResult {
        if let Err(diagnostic) = self.enter_initial_state() {
            return TickResult::Failed(describe(&diagnostic));
        }
        // Инварианты (охранные формулы) проверяются до `always` - как `assert()` до
        // `switch` в порождённом C. В жёстком режиме нарушение даёт `Failed` и
        // останавливает прогон, в мягком - записывается, и такт продолжается. Ошибка
        // вычисления самого условия нарушением не считается и даёт `Failed` в обоих
        // режимах. У композиции проверяет каждый дочерний узел в своём `tick_mode`.
        if matches!(self.0, UnitKind::Node { .. })
            && let Some(failed) = self.check_guards(soft)
        {
            return failed;
        }
        if let Err(diagnostic) = self.execution("always") {
            return TickResult::Failed(describe(&diagnostic));
        }
        // Периодические блоки `every` - после `always`, до диспетчеризации состояния
        // (как model-level `always`).
        if let Err(diagnostic) = self.execute_every() {
            return TickResult::Failed(describe(&diagnostic));
        }
        // Диспетчеризация по форме без удержания заимствования `self.0`: ветвь вызывает
        // методы, которым нужен `&mut self` (`match &self.0 { ... => self.tick_node()
        // }` дал бы конфликт заимствований).
        if matches!(self.0, UnitKind::None) {
            return TickResult::Terminated;
        }
        if matches!(self.0, UnitKind::Node { .. }) {
            return self.tick_node(soft);
        }
        if matches!(self.0, UnitKind::Parallel { .. }) {
            return self.tick_parallel(soft);
        }
        self.tick_sequential(soft)
    }

    /// Проверяет инварианты модели и текущего состояния. Возвращает `Some(Failed)` при
    /// нарушении (жёсткий режим) или ошибке вычисления, `None` если обязательства
    /// выполнены **или** нарушение записано в мягком режиме. Различает нарушение
    /// (SIM-025) и ошибку самого условия (существующий `SIM-0xx`) - как переходы в
    /// `tick_node`: ошибка условия - `Failed` в **обоих** режимах.
    fn check_guards(&mut self, soft: bool) -> Option<TickResult> {
        let guards: Vec<Guard> = if let UnitKind::Node { guards, state, .. } = &self.0 {
            let mut all = guards.model.clone();
            if let Some(s) = state
                && let Some(sg) = guards.per_state.get(s)
            {
                all.extend(sg.clone());
            }
            all
        } else {
            return None;
        };
        for (pred, name) in &guards {
            match pred.evaluate(self) {
                Ok(true) => {}
                Ok(false) => {
                    let named = name.as_ref().map(|n| format!(" '{n}'")).unwrap_or_default();
                    let details = format!("нарушен инвариант{named} (SIM-025)");
                    if soft {
                        // Мягкий режим: записать и продолжить (не прерывать такт).
                        if let UnitKind::Node {
                            invariant_violations,
                            ..
                        } = &mut self.0
                        {
                            invariant_violations.push(details);
                        }
                    } else {
                        return Some(TickResult::Failed(details));
                    }
                }
                // Ошибка вычисления условия - недостоверность прогона, а не "инвариант
                // ложен": `Failed` в обоих режимах.
                Err(diagnostic) => return Some(TickResult::Failed(describe(&diagnostic))),
            }
        }
        None
    }

    /// Переходы, которые сработали бы на следующем такте при нынешних значениях.
    ///
    /// Условия вычисляются в том же порядке, что на такте, но ни тела, ни переходы не
    /// исполняются: это взгляд вперёд для схемы, а не такт. Ответ - пары "из, в" по
    /// всем активным узлам; ошибка вычисления условия даёт пустой ответ, а не отказ:
    /// её назовёт сам такт.
    pub fn peek_transitions(&mut self) -> Vec<(String, String)> {
        match &self.0 {
            UnitKind::None => Vec::new(),
            UnitKind::Node { state: None, .. } => Vec::new(),
            UnitKind::Node { .. } => {
                let state_name = match &self.0 {
                    UnitKind::Node { state: Some(s), .. } => s.clone(),
                    _ => unreachable!(),
                };
                let implementation = match &self.0 {
                    UnitKind::Node { state_impls, .. } => state_impls.get(&state_name).cloned(),
                    _ => unreachable!(),
                };
                // Незавершённая реализация держит узел: переходы самого узла до её конца
                // не проверяются, смотрятся переходы внутри неё.
                if let Some(inner) = implementation
                    && !inner.borrow().is_terminal()
                {
                    return inner.borrow_mut().peek_transitions();
                }
                let transitions: Vec<(String, Predicate)> = match &self.0 {
                    UnitKind::Node {
                        state_transitions, ..
                    } => state_transitions
                        .get(&state_name)
                        .cloned()
                        .unwrap_or_default(),
                    _ => unreachable!(),
                };
                for (name, pred) in &transitions {
                    match pred.evaluate(self) {
                        Ok(true) => return vec![(state_name, name.clone())],
                        Ok(false) => {}
                        Err(_) => return Vec::new(),
                    }
                }
                Vec::new()
            }
            UnitKind::Parallel { units, .. } => units
                .iter()
                .flat_map(|u| u.borrow_mut().peek_transitions())
                .collect(),
            UnitKind::Sequential { units, index, .. } => units
                .get(*index)
                .map(|u| u.borrow_mut().peek_transitions())
                .unwrap_or_default(),
        }
    }

    fn tick_node(&mut self, soft: bool) -> TickResult {
        // Шаг 1: клонируем имя текущего состояния
        let state_name: String = if let UnitKind::Node { state: Some(s), .. } = &self.0 {
            s.clone()
        } else {
            // state: None - узел не инициализирован или завершён
            return TickResult::Terminated;
        };

        // Шаг 1a: реализация состояния (`state P = A + B { ... }`).
        //
        // Тикается до проверки переходов, и пока она не завершена, переходы не
        // проверяются вовсе. Эталон - порождённый C: `generate_extend_transition`
        // эмитит переход внутри ветви `is_done`.
        //
        // Такт при этом не добавляется: переход берётся на том же такте, на котором
        // реализация завершилась (`Terminated` проваливается ниже, а не выходит с
        // `Processing`).
        let implementation = if let UnitKind::Node { state_impls, .. } = &self.0 {
            state_impls.get(&state_name).cloned()
        } else {
            unreachable!()
        };
        if let Some(inner) = implementation {
            match inner.borrow_mut().tick_mode(soft) {
                TickResult::Processing => return TickResult::Processing,
                // Ошибка внутри реализации есть ошибка узла.
                failed @ TickResult::Failed(_) => return failed,
                TickResult::Terminated => {}
            }
        }

        // Шаг 2: клонируем список переходов (Rc-предикаты)
        let transitions: Vec<(String, Predicate)> = if let UnitKind::Node {
            state_transitions,
            ..
        } = &self.0
        {
            state_transitions
                .get(&state_name)
                .cloned()
                .unwrap_or_default()
        } else {
            unreachable!()
        };

        // Состояние С телом переходов не требует и автомат не завершает: `always` без
        // рёбер работает вечно.
        if transitions.is_empty() && !self.state_has_body(&state_name) {
            // Шаг 2а: уход в терминал - тоже выход из состояния.
            //
            // Цели `c`, `rust`, `st` и `sv` исполняют здесь `exit`, а эталон не
            // исполнял его вовсе: замер 2026-08-23 дал `hits = 1` против `11` у всех
            // четырёх, а на состоянии-композиции - `0` против `1`. Флаг держит "ровно
            // один раз": терминальный узел тикается и дальше, а выходят из состояния
            // однажды - как `state = END` в C.
            let already = matches!(
                &self.0,
                UnitKind::Node {
                    exited_terminal: true,
                    ..
                }
            );
            if !already {
                if let UnitKind::Node {
                    exited_terminal, ..
                } = &mut self.0
                {
                    *exited_terminal = true;
                }
                let exit_fns: Vec<Execution> = if let UnitKind::Node {
                    state_executions, ..
                } = &self.0
                {
                    state_executions
                        .get(&state_name)
                        .and_then(|m| m.get("exit"))
                        .cloned()
                        .unwrap_or_default()
                } else {
                    unreachable!()
                };
                for f in &exit_fns {
                    if let Err(diagnostic) = f(self) {
                        return TickResult::Failed(describe(&diagnostic));
                    }
                }
            }
            return TickResult::Terminated;
        }

        // Шаг 3: ищем первый сработавший переход. Ошибка вычисления условия не
        // означает "условие ложно".
        let mut fired = None;
        for (name, pred) in &transitions {
            match pred.evaluate(self) {
                Ok(true) => {
                    fired = Some((name.clone(), pred.name.clone()));
                    break;
                }
                Ok(false) => {}
                Err(diagnostic) => return TickResult::Failed(describe(&diagnostic)),
            }
        }

        if let UnitKind::Node {
            last_transition, ..
        } = &mut self.0
        {
            *last_transition = None;
        }

        if let Some((next, pred_name)) = fired {
            // Шаг 4: исполнители выхода из текущего состояния
            let exit_fns: Vec<Execution> = if let UnitKind::Node {
                state_executions, ..
            } = &self.0
            {
                state_executions
                    .get(&state_name)
                    .and_then(|m| m.get("exit"))
                    .cloned()
                    .unwrap_or_default()
            } else {
                unreachable!()
            };
            for f in &exit_fns {
                if let Err(diagnostic) = f(self) {
                    return TickResult::Failed(describe(&diagnostic));
                }
            }

            // Шаг 5: исполнители входа в следующее состояние
            let enter_fns: Vec<Execution> = if let UnitKind::Node {
                state_executions, ..
            } = &self.0
            {
                state_executions
                    .get(&next)
                    .and_then(|m| m.get("enter"))
                    .cloned()
                    .unwrap_or_default()
            } else {
                unreachable!()
            };
            for f in &enter_fns {
                if let Err(diagnostic) = f(self) {
                    return TickResult::Failed(describe(&diagnostic));
                }
            }

            // Шаг 6: переход в новое состояние + запись последнего перехода
            //
            // Самопереход отсчёт времени в состоянии не сбрасывает: признак "вход"
            // здесь - смена состояния, ровно как `state != prev_state` в порождённом C.
            //
            // `enter`/`exit` при самопереходе исполняются по-прежнему - это разные
            // вопросы, и разница названа в документе (раздел "Время").
            let changed = state_name != next;
            if let UnitKind::Node {
                state,
                last_transition,
                ..
            } = &mut self.0
            {
                last_transition.replace((state_name, next.clone(), pred_name));
                *state = Some(next);
            }
            // Общий реестр состояний: наблюдатель обязан увидеть новое состояние уже на
            // этом такте - так же, как в порождённом C, где под-модели тикаются по
            // очереди и сосед читает поле после чужого `_tick`.
            self.publish_state();
            // Выдержка `after` отсчитывается от входа в состояние.
            if changed {
                self.mark_state_entry();
            }
        }

        TickResult::Processing
    }

    fn tick_parallel(&mut self, soft: bool) -> TickResult {
        // Тикаем все дочерние и собираем результаты - нельзя прерываться раньше
        let results: Vec<TickResult> = if let UnitKind::Parallel { units, .. } = &self.0 {
            units
                .iter()
                .map(|u| u.borrow_mut().tick_mode(soft))
                .collect()
        } else {
            unreachable!()
        };
        // Ошибка любого из параллельных детей делает шаг недостоверным.
        if let Some(failed) = results
            .iter()
            .find(|r| matches!(r, TickResult::Failed(_)))
            .cloned()
        {
            return failed;
        }
        if results.iter().all(|r| *r == TickResult::Terminated) {
            TickResult::Terminated
        } else {
            TickResult::Processing
        }
    }

    fn tick_sequential(&mut self, soft: bool) -> TickResult {
        let (index, len) = if let UnitKind::Sequential { units, index, .. } = &self.0 {
            (*index, units.len())
        } else {
            unreachable!()
        };
        if index >= len {
            return TickResult::Terminated;
        }
        let child_result = if let UnitKind::Sequential { units, index, .. } = &self.0 {
            units[*index].borrow_mut().tick_mode(soft)
        } else {
            unreachable!()
        };
        match child_result {
            TickResult::Processing => TickResult::Processing,
            // Ошибка ребёнка - ошибка всей последовательности.
            failed @ TickResult::Failed(_) => failed,
            TickResult::Terminated => {
                let mut finished = false;
                if let UnitKind::Sequential { units, index, .. } = &mut self.0 {
                    *index += 1;
                    finished = *index >= units.len();
                }
                // Завершение последнего шага завершает цепочку в тот же такт. Эталон -
                // цель `c`: `X_tick(&step); if (X_is_done(&step)) { ... }` - завершение
                // проверяется на том же такте, что и тик.
                if finished {
                    TickResult::Terminated
                } else {
                    TickResult::Processing
                }
            }
        }
    }

    /// Д5: исполняет `enter` стартового состояния - ровно один раз, до первого `always`
    /// и до проверки переходов.
    ///
    /// Для `Parallel`/`Sequential` вызывать не нужно: их дети получают вызов через
    /// собственный [`Unit::tick`].
    fn enter_initial_state(&mut self) -> Result<(), Diagnostic> {
        let state_name = match &mut self.0 {
            UnitKind::Node {
                entered_initial: true,
                ..
            } => return Ok(()),
            UnitKind::Node {
                entered_initial,
                state,
                ..
            } => {
                *entered_initial = true;
                match state {
                    Some(name) => name.clone(),
                    None => return Ok(()),
                }
            }
            UnitKind::Parallel { .. } | UnitKind::Sequential { .. } | UnitKind::None => {
                return Ok(());
            }
        };
        // Выдержка `after` отсчитывается от входа в состояние - в том числе в
        // Стартовое. Без этой отметки отсчёт шёл бы от начала прогона, и выдержка
        // срабатывала бы раньше, чем у цели `st` со штатным `TON`: тот латчит момент,
        // когда условие стало истинным.
        self.mark_state_entry();
        let enter_fns: Vec<Execution> = match &self.0 {
            UnitKind::Node {
                state_executions, ..
            } => state_executions
                .get(&state_name)
                .and_then(|m| m.get("enter"))
                .cloned()
                .unwrap_or_default(),
            UnitKind::Parallel { .. } | UnitKind::Sequential { .. } | UnitKind::None => vec![],
        };
        for f in &enter_fns {
            f(self)?;
        }
        Ok(())
    }
}
