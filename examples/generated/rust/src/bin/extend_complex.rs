//! Проверка порождённого Rust: модель `extend_complex` (`examples/extend_complex.takt`).
//!
//! Пример проверяет композицию: последовательность `A -> B -> (C | D) -> E`, где
//! `C` и `D` идут параллельно, а `E` сама составная (`D -> F`). Единственный
//! наблюдаемый выход - пара портов `idle`/`work`, которые ставит `B` по входу
//! `wait`; остальное видно только по факту завершения автомата.

use std::cell::RefCell;
use std::rc::Rc;

use takt_generated::extend_complex::{ExtendComplex, Hal, InBitPort, OutBitPort};

/// Записи в выходные порты: (порт, значение).
#[derive(Default)]
struct Trace {
    writes: Vec<(&'static str, bool)>,
}

struct Probe {
    trace: Rc<RefCell<Trace>>,
    wait: bool,
}

impl Hal for Probe {
    fn read_bit(&mut self, port: InBitPort) -> bool {
        match port {
            InBitPort::Wait => self.wait,
        }
    }

    fn write_bit(&mut self, port: OutBitPort, value: bool) {
        let name = match port {
            OutBitPort::Idle => "idle",
            OutBitPort::Work => "work",
        };
        self.trace.borrow_mut().writes.push((name, value));
    }

    /// `extern fn has_flag(v: bool) -> bool` - здесь просто эхо входа.
    fn has_flag(&mut self, v: bool) -> bool {
        v
    }
}

/// Прогоняет автомат до завершения, но не дольше `limit` тактов.
/// Возвращает записи в порты и число потраченных тактов (`None` = не завершился).
fn run(wait: bool, limit: usize) -> (Vec<(&'static str, bool)>, Option<usize>) {
    let trace = Rc::new(RefCell::new(Trace::default()));
    let mut m = ExtendComplex::new(Probe {
        trace: Rc::clone(&trace),
        wait,
    });
    m.init();

    let mut spent = None;
    for i in 0..limit {
        if m.is_done() {
            spent = Some(i);
            break;
        }
        m.tick();
    }
    let writes = trace.borrow().writes.clone();
    (writes, spent)
}

const LIMIT: usize = 40;

fn main() {
    // wait поднят: B объявляет простой.
    let (writes, spent) = run(true, LIMIT);
    assert_eq!(
        writes,
        [("work", false), ("idle", true)],
        "при wait = 1 состояние B снимает work и поднимает idle"
    );
    assert_eq!(
        spent,
        Some(7),
        "цепочка A → B → (C | D) → E завершается за 7 тактов"
    );

    // wait снят: B объявляет работу.
    let (writes, spent) = run(false, LIMIT);
    assert_eq!(
        writes,
        [("idle", false), ("work", true)],
        "при wait = 0 состояние B снимает idle и поднимает work"
    );
    assert_eq!(
        spent,
        Some(7),
        "вход wait на длину цепочки не влияет — он меняет только порты"
    );

    println!("extend_complex: OK (композиция A → B → (C | D) → E завершается за 7 тактов)");
}
