//! Ограничение частоты окном в памяти.

use std::collections::HashMap;
use std::net::IpAddr;
use std::sync::Mutex;
use std::time::{Duration, Instant};

/// Окно попыток по адресу.
#[derive(Debug)]
pub struct Window {
    window: Duration,
    limit: u32,
    /// Адрес -> (начало окна, счёт).
    seen: Mutex<HashMap<IpAddr, (Instant, u32)>>,
}

/// Решение окна: пустить либо подождать.
#[derive(Debug, PartialEq, Eq)]
pub enum Decision {
    /// Пропустить.
    Allow,
    /// Отказать; через сколько секунд окно освободится.
    Wait(u64),
}

impl Window {
    /// Заводит окно.
    pub fn new(window: Duration, limit: u32) -> Self {
        Self {
            window,
            limit,
            seen: Mutex::new(HashMap::new()),
        }
    }

    /// Считает попытку с адреса.
    pub fn check(&self, address: IpAddr) -> Decision {
        self.check_at(address, Instant::now())
    }

    /// То же, но с явным моментом времени - так проверяется в тестах.
    ///
    /// Часы передаются параметром, а не берутся внутри: иначе проверка окна требовала
    /// бы `sleep`, то есть была бы медленной и зыбкой.
    pub fn check_at(&self, address: IpAddr, now: Instant) -> Decision {
        let mut seen = self.seen.lock().expect("окно частоты не отравлено");
        // Просроченные записи убираются на каждом обращении: без этого карта растёт по
        // числу адресов, а не по числу активных.
        seen.retain(|_, (started, _)| now.duration_since(*started) < self.window);
        let entry = seen.entry(address).or_insert((now, 0));
        if now.duration_since(entry.0) >= self.window {
            *entry = (now, 0);
        }
        if entry.1 >= self.limit {
            let left = self.window.saturating_sub(now.duration_since(entry.0));
            return Decision::Wait(left.as_secs().max(1));
        }
        entry.1 += 1;
        Decision::Allow
    }

    /// Сколько адресов помнится сейчас - для проверки, что память не растёт.
    pub fn tracked(&self) -> usize {
        self.seen.lock().expect("окно частоты не отравлено").len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn address(last: u8) -> IpAddr {
        IpAddr::from([10, 0, 0, last])
    }

    #[test]
    fn window_lets_the_limit_through_and_stops_the_next() {
        let window = Window::new(Duration::from_secs(60), 3);
        let now = Instant::now();
        for attempt in 1..=3 {
            assert_eq!(
                window.check_at(address(1), now),
                Decision::Allow,
                "попытка {attempt}"
            );
        }
        assert!(matches!(
            window.check_at(address(1), now),
            Decision::Wait(_)
        ));
    }

    #[test]
    fn refusal_says_how_long_to_wait() {
        let window = Window::new(Duration::from_secs(60), 1);
        let now = Instant::now();
        window.check_at(address(1), now);
        let Decision::Wait(seconds) = window.check_at(address(1), now) else {
            panic!("окно обязано было отказать");
        };
        assert!((1..=60).contains(&seconds), "названо {seconds} с");
    }

    #[test]
    fn addresses_are_counted_apart() {
        let window = Window::new(Duration::from_secs(60), 1);
        let now = Instant::now();
        assert_eq!(window.check_at(address(1), now), Decision::Allow);
        assert_eq!(
            window.check_at(address(2), now),
            Decision::Allow,
            "сосед не при чём"
        );
    }

    #[test]
    fn window_opens_again_when_it_passes() {
        let window = Window::new(Duration::from_secs(60), 1);
        let now = Instant::now();
        window.check_at(address(1), now);
        let later = now + Duration::from_secs(61);
        assert_eq!(window.check_at(address(1), later), Decision::Allow);
    }

    #[test]
    fn memory_does_not_grow_with_every_address_ever_seen() {
        // Карта, из которой ничего не уходит, растёт по числу адресов за всё время
        // работы - то есть становится хранилищем адресов, которого A6 обещал не
        // заводить.
        let window = Window::new(Duration::from_secs(60), 5);
        let now = Instant::now();
        for last in 0..50 {
            window.check_at(address(last), now);
        }
        assert_eq!(window.tracked(), 50);
        window.check_at(address(200), now + Duration::from_secs(61));
        assert_eq!(window.tracked(), 1, "просроченные записи обязаны уходить");
    }
}
