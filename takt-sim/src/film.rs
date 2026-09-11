//! Кадры прогона: цветной вид листа и строка трассы такта.
//!
//! Кадр рисует крейт рисунка по файлу раскладки `.takt-ui` - тот же чертёж, что
//! у страницы; своей раскладки у эталона нет. Бегун кадров не знает: такт отдаёт
//! [`Step`], и лента запоминает из него только подсветку и строку трассы. Кадр
//! рисуется по требованию при записи видео - тексты кадров в памяти не копятся.

use std::io::Write;

use takt_scheme::layout::Layout;
use takt_scheme::run::{Active, Segment, Tick};
use takt_scheme::sheet::DrawSheet;
use takt_scheme::style::View;
use takt_scheme::svg::{Options, svg};
use takt_scheme::video;

use crate::runner::Step;
use takt_lang::diagnostics::lang::keys;
use takt_lang::msg;

/// Пауза между кадрами по умолчанию, миллисекунд.
pub use takt_scheme::video::PAUSE_MS;

/// Ключ корневого листа.
pub const ROOT_SHEET: &str = "/";

/// Лента прогона одного листа.
pub struct Film {
    sheets: Vec<DrawSheet>,
    layout: Layout,
    key: String,
    legend: bool,
    ticks: Vec<(Tick, String)>,
}

impl Film {
    /// Лента листа `sheet` модели с текстом `source` и файлом раскладки `layout`;
    /// `legend` - таблица знаков под листом.
    ///
    /// # Ошибки
    /// Модель не разбирается, файл раскладки не читается либо неполон (отказ
    /// называет неразмещённые узлы по листам), листа с таким ключом нет.
    pub fn new(source: &str, layout: &str, sheet: &str, legend: bool) -> Result<Self, String> {
        let graph = takt_lang::layout::graph_of(source).map_err(|d| d.message.clone())?;
        let layout = takt_scheme::layout::parse(layout).map_err(|e| e.0)?;
        let sheets = takt_scheme::sheet::sheets(&graph, &layout).map_err(|e| e.to_string())?;
        Self::of_sheets(sheets, layout, sheet, legend)
    }

    /// Лента листа `sheet` из уже построенных листов и файла раскладки.
    ///
    /// # Ошибки
    /// Листа с таким ключом нет.
    pub fn of_sheets(
        sheets: Vec<DrawSheet>,
        layout: Layout,
        sheet: &str,
        legend: bool,
    ) -> Result<Self, String> {
        if !sheets.iter().any(|s| s.key == sheet) {
            return Err(msg!(keys::SIM_FILM_NO_SHEET, key = sheet));
        }
        Ok(Self {
            sheets,
            layout,
            key: sheet.to_string(),
            legend,
            ticks: Vec::new(),
        })
    }

    /// Запоминает такт. Шаг без строки трассы (прогон окончен до такта либо такт не
    /// вычислился) кадра не даёт: кадр - это такт, который случился.
    pub fn record(&mut self, step: &Step) {
        if let Some(line) = &step.line {
            self.ticks.push((tick_of(step), line.clone()));
        }
    }

    /// Число кадров - число записанных тактов.
    pub fn frames(&self) -> usize {
        self.ticks.len()
    }

    /// SVG кадра `i` без вшитых шрифтов: растеризатор получает их байтами.
    ///
    /// # Ошибки
    /// Кадра с таким номером нет.
    pub fn frame(&self, i: usize) -> Result<String, String> {
        let (tick, line) = self
            .ticks
            .get(i)
            .ok_or_else(|| msg!(keys::SIM_FILM_NO_FRAME, index = i))?;
        self.draw(Some(tick), Some(line), false)
    }

    /// Лист в последнем такте ленты, без строки трассы: цветной вид картинки.
    /// Лента пуста - лист без подсветки. `fonts` - вшивать ли шрифты: SVG для
    /// читателя их несёт, растеризатору они не нужны.
    ///
    /// # Ошибки
    /// Листа ленты нет в модели.
    pub fn last(&self, fonts: bool) -> Result<String, String> {
        self.draw(self.ticks.last().map(|(tick, _)| tick), None, fonts)
    }

    fn draw(
        &self,
        tick: Option<&Tick>,
        line: Option<&String>,
        fonts: bool,
    ) -> Result<String, String> {
        let options = Options {
            view: View::Run,
            legend: self.legend,
            tick: Some(tick.cloned().unwrap_or_default()),
            trace: line.cloned(),
            fonts,
        };
        svg(&self.key, &self.sheets, &self.layout, &options)
            .ok_or_else(|| msg!(keys::SIM_FILM_NO_SHEET, key = self.key))
    }

    fn tape(&self) -> video::Film<impl Fn(usize) -> Result<String, String> + '_> {
        video::Film {
            count: self.frames(),
            draw: |i| self.frame(i),
        }
    }

    /// GIF ленты: кадр на такт, пауза `pause_ms` между кадрами.
    ///
    /// # Ошибки
    /// Кадров нет либо кадр не кодируется.
    pub fn gif(&self, pause_ms: u32, out: impl Write) -> Result<(), String> {
        video::gif(&self.tape(), pause_ms, out)
    }

    /// MP4 (AV1) ленты: кадр на такт, длительность кадра - `pause_ms`.
    ///
    /// # Ошибки
    /// Кадров нет либо кодировщик отказал.
    pub fn mp4(&self, pause_ms: u32) -> Result<Vec<u8>, String> {
        video::mp4(&self.tape(), pause_ms)
    }
}

/// Подсветка такта: адреса активных состояний и ожидаемые переходы - в той форме,
/// в какой их читает правило подсветки крейта рисунка.
pub fn tick_of(step: &Step) -> Tick {
    Tick {
        active: step
            .active
            .iter()
            .map(|a| Active {
                path: a
                    .path
                    .iter()
                    .map(|s| Segment {
                        owner: s.owner.clone(),
                        step: s.step,
                        model: s.model.clone(),
                    })
                    .collect(),
                model: a.model.clone(),
                state: a.state.clone(),
                done: a.done,
            })
            .collect(),
        next: step.next.clone(),
    }
}
