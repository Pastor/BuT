//! Диагностические сообщения компилятора Takt.
//!
//! Этот модуль содержит типы для описания ошибок, предупреждений и информационных
//! сообщений, возникающих в ходе лексического, синтаксического и семантического
//! анализа.
//!
//! ## Основные типы
//!
//! - [`Level`] - уровень серьёзности сообщения (`Debug`, `Info`, `Warning`, `Error`).
//! - [`ErrorType`] - категория ошибки (синтаксис, типизация, семантика и т.п.).
//! - [`Location`] - позиция в исходном тексте (файл, смещение начала и конца).
//! - [`Diagnostic`] - единица диагностики, объединяющая уровень, тип, сообщение
//!   и список дополнительных замечаний.
//!
//! ## Коды ошибок
//!
//! Каждая диагностика может содержать код вида `XX-YYY`, где:
//! - `LE` - лексические ошибки (лексер)
//! - `SY` - синтаксические ошибки (парсер)
//! - `SE` - семантические ошибки (анализ)
//! - `CC` - ошибки кодогенерации Си
//!
//! ## Конвертации
//!
//! `Diagnostic` реализует `From<&str>` и `From<String>` для удобного создания ошибок из
//! строк. По умолчанию создаётся сообщение уровня `Error` с категорией `SematicError`.

// Пачка диагностик: порядок, уникальность, печать. Вынесено подмодулем - `mod.rs`
// упирается в лимит размера.
mod batch;
// Язык сообщений: носитель, каталоги `messages/*.txt` и подстановка. Модуль публичный -
// язык выбирают CLI, мост и LSP, каждый своим входом.
pub mod lang;
mod position;
pub use batch::{format_compile_error, format_notes, format_warning, normalize};
pub use position::{note_position_prefix, position_prefix};

#[cfg(feature = "ast-serde")]
use serde::{Deserialize, Serialize};
use std::fmt;

/// Уровень серьёзности диагностического сообщения.
#[derive(Clone, Debug, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub enum Level {
    /// Отладочное сообщение - не отображается конечному пользователю.
    Debug,
    /// Информационное сообщение.
    Info,
    /// Предупреждение - код валиден, но может содержать потенциальную проблему.
    Warning,
    /// Ошибка - код содержит нарушение.
    Error,
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Level {
    /// Возвращает строковое представление уровня.
    pub fn as_str(&self) -> &'static str {
        match self {
            Level::Debug => "debug",
            Level::Info => "info",
            Level::Warning => "warning",
            Level::Error => "error",
        }
    }
}

/// Категория диагностического сообщения.
///
/// Помечен `#[non_exhaustive]`: набор категорий ошибок будет пополняться без слома
/// обратной совместимости.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum ErrorType {
    /// Категория не задана.
    None,
    /// Ошибка лексического или синтаксического анализатора.
    ParserError,
    /// Синтаксическая ошибка на уровне языка.
    SyntaxError,
    /// Ошибка объявления (например, неизвестный идентификатор).
    DeclarationError,
    /// Ошибка приведения типов.
    CastError,
    /// Ошибка типизации.
    TypeError,
    /// Предупреждение (не ошибка).
    Warning,
    /// Семантическая ошибка.
    SematicError,
}

/// Вспомогательная заметка, прикреплённая к диагностическому сообщению.
///
/// Используется для указания дополнительного контекста об ошибке (например, место
/// первого объявления при дублировании имени).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Note {
    /// Местоположение в исходном тексте.
    pub loc: Location,
    /// Текст заметки.
    pub message: String,
}

/// Диагностическое сообщение, возникающее в процессе компиляции Takt-программы.
///
/// Каждое сообщение содержит местоположение в исходном тексте, уровень серьёзности,
/// категорию ошибки, основной текст, опциональный код ошибки и список замечаний.
///
/// # Коды ошибок
///
/// Поле [`code`](Diagnostic::code) содержит строку вида `XX-YYY` (например, `LE-001`),
/// однозначно идентифицирующую тип ошибки. Используйте [`Diagnostic::with_code`] для
/// назначения кода существующей диагностике.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic {
    /// Путь файла, к которому относится [`loc`](Diagnostic::loc).
    ///
    /// `None` - путь неизвестен либо неприменим (`Location::Codegen`, `Implicit`,
    /// `Builtin`, `CommandLine`).
    ///
    /// Поле существует потому, что `loc` несёт лишь **номер** файла
    /// ([`Location::Source`]), а таблица номеров ([`FileTable`]) - деталь компиляции и
    /// наружу не выходит. Путь разрешается по номеру там, где таблица ещё жива (внутри
    /// `compile_to_*`), и дальше диагностика самодостаточна: получателю не нужно знать
    /// ни о таблице, ни о номерах.
    pub file: Option<String>,
    /// Местоположение в исходном тексте, к которому относится диагностика.
    pub loc: Location,
    /// Уровень серьёзности сообщения.
    pub level: Level,
    /// Категория ошибки.
    pub ty: ErrorType,
    /// Текст диагностического сообщения.
    pub message: String,
    /// Код ошибки в формате `XX-YYY` (например, `LE-001`, `SE-005`). `None` если код не
    /// назначен.
    pub code: Option<String>,
    /// Вспомогательные заметки.
    pub notes: Vec<Note>,
}

// Конверсии `From<&str> for Diagnostic` больше нет.
//
// Она строила диагностику без кода (`[?]` на печати) и с позицией `Location::Source(0,
// 0, 0)` - то есть "начало первого файла": координата ложная, а не отсутствующая.

impl Diagnostic {
    /// Создаёт отладочное сообщение.
    pub fn debug(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Debug,
            ty: ErrorType::None,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт информационное сообщение.
    pub fn info(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Info,
            ty: ErrorType::None,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт ошибку синтаксического/лексического анализатора.
    pub fn parser_error(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::ParserError,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт синтаксическую ошибку.
    pub fn error(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::SyntaxError,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт ошибку объявления (неизвестный идентификатор и т.д.).
    pub fn declaration_error(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::DeclarationError,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт ошибку приведения типов.
    pub fn cast_error(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::CastError,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт ошибку приведения типов с дополнительной заметкой.
    pub fn cast_error_with_note(
        loc: Location,
        message: String,
        note_loc: Location,
        note: String,
    ) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::CastError,
            loc,
            message,
            code: None,
            notes: vec![Note {
                loc: note_loc,
                message: note,
            }],
        }
    }

    /// Создаёт ошибку типизации.
    pub fn type_error(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::TypeError,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт предупреждение о небезопасном приведении типов.
    pub fn cast_warning(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Warning,
            ty: ErrorType::CastError,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт предупреждение.
    pub fn warning(loc: Location, message: String) -> Self {
        Diagnostic {
            file: None,
            level: Level::Warning,
            ty: ErrorType::Warning,
            loc,
            message,
            code: None,
            notes: Vec::new(),
        }
    }

    /// Создаёт предупреждение с дополнительной заметкой.
    pub fn warning_with_note(
        loc: Location,
        message: String,
        note_loc: Location,
        note: String,
    ) -> Self {
        Diagnostic {
            file: None,
            level: Level::Warning,
            ty: ErrorType::Warning,
            loc,
            message,
            code: None,
            notes: vec![Note {
                loc: note_loc,
                message: note,
            }],
        }
    }

    /// Создаёт предупреждение с набором вспомогательных заметок.
    pub fn warning_with_notes(loc: Location, message: String, notes: Vec<Note>) -> Self {
        Diagnostic {
            file: None,
            level: Level::Warning,
            ty: ErrorType::Warning,
            loc,
            message,
            code: None,
            notes,
        }
    }

    /// Создаёт ошибку с дополнительной заметкой.
    pub fn error_with_note(
        loc: Location,
        message: String,
        note_loc: Location,
        note: String,
    ) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::None,
            loc,
            message,
            code: None,
            notes: vec![Note {
                loc: note_loc,
                message: note,
            }],
        }
    }

    /// Создаёт ошибку с набором вспомогательных заметок.
    pub fn error_with_notes(loc: Location, message: String, notes: Vec<Note>) -> Self {
        Diagnostic {
            file: None,
            level: Level::Error,
            ty: ErrorType::None,
            loc,
            message,
            code: None,
            notes,
        }
    }

    /// Назначает код ошибки и возвращает изменённую диагностику (builder-метод).
    ///
    /// Код должен иметь формат `XX-NNN` (2-4 буквы, дефис, три цифры), например
    /// `LE-001`, `SE-015`, `CC-003`. **Новый код - сперва в реестр**
    /// `docs/diagnostics/README.md` (единый источник истины): возьми следующий
    /// свободный номер в нужном префиксе и добавь строку. Код без записи в реестре
    /// завалит проверка `scripts/check-diagnostic-codes.sh` (в `precheck.sh`).
    ///
    /// # Пример
    ///
    /// ```
    /// use takt_lang::diagnostics::{Diagnostic, Location};
    ///
    /// let d = Diagnostic::error(Location::Builtin, "ошибка".to_string())
    ///     .with_code("SE-001");
    /// assert_eq!(d.code.as_deref(), Some("SE-001"));
    /// ```
    #[must_use]
    pub fn with_code(mut self, code: &str) -> Self {
        self.code = Some(code.to_string());
        self
    }

    /// Добавляет заметку к диагностике.
    ///
    /// Используется для цепочки импорта: ошибка из чужого файла получает заметку
    /// "импортировано здесь" с позицией `import` в **импортирующем** файле. Без неё
    /// редактору не к чему привязать ошибку чужого файла: её собственные координаты
    /// указывают в текст, которого в открытом документе нет.
    pub fn with_note(mut self, loc: Location, message: String) -> Self {
        self.notes.push(Note { loc, message });
        self
    }

    /// Ставит позицию, если своей у диагностики нет.
    ///
    /// Часть отказов рождается там, где координаты неоткуда взять: реестр встроенных
    /// функций спрашивают по строке (`SE-004`), запись карты адресов - по имени порта.
    /// Такие диагностики несут `Location::Builtin` и обязаны получить координату у
    /// Вызывающего - он знает узел.
    ///
    /// Своя позиция не перетирается: чужая верная координата хуже отсутствующей.
    #[must_use]
    pub fn at_if_unset(mut self, loc: Location) -> Self {
        if matches!(self.loc, Location::Builtin) {
            self.loc = loc;
        }
        self
    }

    /// Проставляет путь файла, если он ещё не задан.
    ///
    /// Правильный файл при вложенном импорте (`top -> mid -> deep`) обеспечивает
    /// **не** эта проверка, а настоящий `file_no`: диагностика несёт номер того
    /// файла, где её создали, и реестр разрешает его в путь виновника. Штамповка
    /// при этом одна - в [`parse_and_construct`](crate).
    ///
    /// Проверка `is_none` - защита от повторного штампа: путь, уже проставленный ближе
    /// к источнику, точнее того, что подставит внешний слой.
    pub fn with_file_if_unset(mut self, file: Option<&str>) -> Self {
        if self.file.is_none() {
            self.file = file.map(str::to_string);
        }
        self
    }
}

/// Реестр файлов единицы компиляции: номер (`file_no`) -> путь.
///
/// [`Location::Source`] несёт лишь **номер** файла - этого мало, чтобы назвать
/// пользователю место ошибки. Реестр раздаёт номера при разборе (корневой файл - всегда
/// `0`, импортируемые - по порядку загрузки) и разрешает их обратно в пути.
#[derive(Debug, Clone)]
pub struct FileTable {
    paths: Vec<String>,
}

/// Номер корневого файла единицы компиляции - того, который разбирают, а не
/// импортируют.
///
/// Слот `0` зарезервирован под корень **при любом способе создания реестра** (см.
/// Инвариант держит исправление `0053-01`; на нём стоит файлоосознанный индекс LSP.
pub const ROOT_FILE_NO: u64 = 0;

/// Слот `0` зарезервирован под корень **при любом способе создания реестра**.
///
/// В дереве, построенном через `construct_model` (обёртка с реестром-однодневкой),
/// позиция из импортированного файла становилась неотличима от корневой - ровно тот
/// дефект, который закрывала.
///
/// Дефект прожил латентно, потому что **никто не читал `file_no`** из этой ветки: тесты
/// 0053 проверяли путь `compile_to_*`, а он зовёт `new(filename)`. Читатель появился с
/// (файлоосознанный индекс LSP).
///
/// []:
/// https://github.com/Pastor/Takt/blob/v2/docs/fixes/-file-table-default-collision.md
impl Default for FileTable {
    fn default() -> Self {
        // Пустой путь = "корень неизвестен": слот занят, но `path(0)` честно отвечает
        // `None`, а не выдаёт чужой файл за корневой.
        FileTable {
            paths: vec![String::new()],
        }
    }
}

impl FileTable {
    /// Создаёт реестр, регистрируя корневой файл под номером `0`.
    pub fn new(root: &str) -> Self {
        FileTable {
            paths: vec![root.to_string()],
        }
    }

    /// Регистрирует файл и возвращает его номер.
    ///
    /// Один и тот же путь, загруженный дважды, получает **один** номер: номер
    /// обозначает файл, а не факт загрузки.
    pub fn add(&mut self, path: &str) -> u64 {
        if let Some(i) = self.paths.iter().position(|p| p == path) {
            return i as u64;
        }
        self.paths.push(path.to_string());
        (self.paths.len() - 1) as u64
    }

    /// Путь по номеру; `None` - номер не выдавался этим реестром **либо** путь
    /// неизвестен (слот `0` у реестра, созданного без корня - см.
    pub fn path(&self, file_no: u64) -> Option<&str> {
        self.paths
            .get(file_no as usize)
            .map(String::as_str)
            .filter(|path| !path.is_empty())
    }

    /// Путь для позиции: `None`, если позиция не файловая
    /// (`Codegen`/`Implicit`/`Builtin`/`CommandLine`).
    pub fn path_of(&self, loc: &Location) -> Option<&str> {
        match loc {
            Location::Source(file_no, _, _) => self.path(*file_no as u64),
            _ => None,
        }
    }
}

/// Строка и колонка (с **единицы**) по байтовому смещению в тексте.
///
/// Смещения внутри [`Location`] байтовые и с нуля - это внутреннее представление;
/// человеку показывается нумерация с единицы, как в `rustc`/`gcc`.
///
/// Колонка считается в **символах**, а не в байтах: в `.takt` встречается кириллица
/// (комментарии, строки), и байтовая колонка указывала бы мимо.
pub fn line_column(text: &str, offset: usize) -> (usize, usize) {
    // Смещение приводится к границе символа, а не только к длине текста:
    // иначе срез паникует, и печать диагностики роняет инструмент. Так и было
    // координата записи карты адресов приходила под номером файла
    // модели, попадала внутрь кириллической буквы, и `taktc compile -t st-at
    // --address-map ...` падал с "byte index ... is not a char boundary" - вместо
    // предупреждения пользователь получал panic. Корень (чужой номер файла)
    // устранён там же; здесь - страховка: печать сообщения не вправе падать
    // Никогда, каким бы ни было смещение.
    let mut clamped = offset.min(text.len());
    while clamped > 0 && !text.is_char_boundary(clamped) {
        clamped -= 1;
    }
    let before = &text[..clamped];
    let line = before.matches('\n').count() + 1;
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let column = text[line_start..clamped].chars().count() + 1;
    (line, column)
}

impl Diagnostic {
    /// Возвращает форматированный префикс кода для вывода, например `[SE-001] `. Если
    /// код не задан, возвращает пустую строку.
    pub fn code_prefix(&self) -> String {
        self.code
            .as_deref()
            .map(|c| format!("[{}] ", c))
            .unwrap_or_default()
    }
}

/// Местоположение узла АСД в исходном тексте.
///
/// Вариант [`Source`](Location::Source) хранит номер файла, байтовое смещение начала и
/// байтовое смещение конца (не включительно).
///
/// Поля - `u32`: номера файлов и байтовые смещения `.takt`-исходников с запасом влезают
/// в 32 бита, а `u32x3` даёт варианту 16 байт вместо 32 у `(u64, usize, usize)` - это
/// опускает `Diagnostic` ниже порога 128 байт линта `clippy::result_large_err` (иначе
/// 414 предупреждений на `Result<_, Diagnostic>`). Публичный API методов остаётся в
/// `usize`/`String` - каст локализован в аксессорах, поэтому большинство читающих
/// сайтов не затронуто.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "ast-serde", derive(Serialize, Deserialize))]
pub enum Location {
    /// Встроенный элемент (не относится к пользовательскому коду).
    Builtin,
    /// Элемент, заданный через командную строку компилятора.
    CommandLine,
    /// Неявно сгенерированный элемент.
    Implicit,
    /// Элемент, сгенерированный кодогенератором.
    Codegen,
    /// Элемент в исходном файле: `(файл, начало, конец)`.
    Source(u32, u32, u32),
}

impl Default for Location {
    fn default() -> Self {
        Self::Source(0, 0, 0)
    }
}

/// Вызывается при попытке получить позицию из не-файлового варианта [`Location`].
#[inline(never)]
#[cold]
#[track_caller]
fn not_a_file() -> ! {
    panic!("местоположение не является файловой позицией")
}

impl Location {
    /// Конструирует [`Location::Source`] из "широких" типов вызывающего.
    ///
    /// внутреннее представление - `u32` (см. [`Location`]), но лексер/парсер оперируют
    /// `u64`-номером файла и `usize`-смещениями `@L`/`@R`. Каст локализован здесь:
    /// `Location::source(file_no, l, r)` вместо ручного `Location::Source(file_no as
    /// u32, l as u32, r as u32)` в сотнях мест грамматики. Значения `.takt`-исходников
    /// с запасом влезают в 32 бита.
    #[inline]
    pub fn source(file: u64, start: usize, end: usize) -> Self {
        Location::Source(file as u32, start as u32, end as u32)
    }

    /// Возвращает [`Location`] нулевой длины, указывающий на начало данного диапазона.
    #[inline]
    pub fn begin_range(&self) -> Self {
        match self {
            Location::Source(filename, start, _) => Location::Source(*filename, *start, *start),
            loc => *loc,
        }
    }

    /// Возвращает [`Location`] нулевой длины, указывающий на конец данного диапазона.
    #[inline]
    pub fn end_range(&self) -> Self {
        match self {
            Location::Source(filename, _, end) => Location::Source(*filename, *end, *end),
            loc => *loc,
        }
    }

    /// Возвращает `Some(номер_файла)` для варианта [`Source`](Location::Source), или
    /// `None` для остальных вариантов.
    #[inline]
    pub fn try_file_no(&self) -> Option<String> {
        match self {
            Location::Source(file_no, _, _) => Some(format!("{}", file_no)),
            _ => None,
        }
    }

    /// Возвращает байтовое смещение начала диапазона.
    ///
    /// # Паника
    ///
    /// Паникует, если `self` не является вариантом [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn start(&self) -> usize {
        match self {
            Location::Source(_, start, _) => *start as usize,
            _ => not_a_file(),
        }
    }

    /// Возвращает байтовое смещение конца диапазона (не включительно, exclusive).
    ///
    /// Значение `end` указывает на первый байт **после** последнего символа диапазона,
    /// то есть является exclusive-концом в стиле `start..end` (Rust-срезы, `Range`).
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::diagnostics::Location;
    ///
    /// // Исходный текст: "hello world"
    /// // Токен "hello": байты 0..5
    /// let loc = Location::Source(0, 0, 5);
    /// assert_eq!(loc.end(), 5);
    /// // source[0..5] == "hello" - правильный срез
    /// ```
    ///
    /// # Паника
    ///
    /// Паникует, если `self` не является вариантом [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn end(&self) -> usize {
        match self {
            Location::Source(_, _, end) => *end as usize,
            _ => not_a_file(),
        }
    }

    /// Возвращает байтовое смещение `end + 1`.
    ///
    /// **Важно:** поле `end` в [`Location::Source`] уже является exclusive-концом
    /// (один байт после последнего символа). Этот метод возвращает `end + 1`, то есть
    /// позицию, следующую за exclusive-концом. Используется в редких случаях, когда
    /// нужен инклюзивный диапазон или адресация за пределами диапазона.
    ///
    /// Для большинства задач используйте [`range()`](Location::range) или
    /// [`end()`](Location::end).
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::diagnostics::Location;
    ///
    /// let loc = Location::Source(0, 3, 8); // байты 3..8 (exclusive)
    /// assert_eq!(loc.end(), 8);
    /// assert_eq!(loc.exclusive_end(), 9); // end + 1
    /// ```
    ///
    /// # Паника
    ///
    /// Паникует, если `self` не является вариантом [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn exclusive_end(&self) -> usize {
        self.end() + 1
    }

    /// Устанавливает начало текущего диапазона равным началу `other`.
    ///
    /// # Паника
    ///
    /// Паникует, если любой из операндов не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn use_start_from(&mut self, other: &Location) {
        match (self, other) {
            (Location::Source(_, start, _), Location::Source(_, other_start, _)) => {
                *start = *other_start;
            }
            _ => not_a_file(),
        }
    }

    /// Устанавливает конец текущего диапазона равным концу `other`.
    ///
    /// # Паника
    ///
    /// Паникует, если любой из операндов не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn use_end_from(&mut self, other: &Location) {
        match (self, other) {
            (Location::Source(_, _, end), Location::Source(_, _, other_end)) => {
                *end = *other_end;
            }
            _ => not_a_file(),
        }
    }

    /// Возвращает копию с началом, взятым из `other`.
    ///
    /// # Паника
    ///
    /// Паникует, если любой из операндов не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn with_start_from(mut self, other: &Self) -> Self {
        self.use_start_from(other);
        self
    }

    /// Возвращает копию с концом, взятым из `other`.
    ///
    /// # Паника
    ///
    /// Паникует, если любой из операндов не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn with_end_from(mut self, other: &Self) -> Self {
        self.use_end_from(other);
        self
    }

    /// Возвращает копию с заменённым началом.
    ///
    /// # Паника
    ///
    /// Паникует, если `self` не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn with_start(self, start: usize) -> Self {
        match self {
            Self::Source(no, _, end) => Self::Source(no, start as u32, end),
            _ => not_a_file(),
        }
    }

    /// Возвращает копию с заменённым концом.
    ///
    /// # Паника
    ///
    /// Паникует, если `self` не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn with_end(self, end: usize) -> Self {
        match self {
            Self::Source(no, start, _) => Self::Source(no, start, end as u32),
            _ => not_a_file(),
        }
    }

    /// Преобразует [`Location`] в стандартный диапазон `start..end`.
    ///
    /// # Паника
    ///
    /// Паникует, если `self` не является [`Location::Source`].
    #[track_caller]
    #[inline]
    pub fn range(self) -> std::ops::Range<usize> {
        match self {
            Self::Source(_, start, end) => start as usize..end as usize,
            _ => not_a_file(),
        }
    }

    // -- V5: Безопасный API для не-файловых вариантов Location -----------------

    /// Возвращает `Some(start)` для [`Location::Source`], `None` для остальных.
    ///
    /// Используйте вместо [`start()`](Location::start) там, где Location может быть
    /// [`Builtin`](Location::Builtin), [`CommandLine`](Location::CommandLine),
    /// [`Implicit`](Location::Implicit) или [`Codegen`](Location::Codegen).
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::diagnostics::Location;
    ///
    /// let src = Location::Source(0, 3, 8);
    /// assert_eq!(src.try_start(), Some(3));
    ///
    /// let imp = Location::Implicit;
    /// assert_eq!(imp.try_start(), None);
    /// ```
    #[inline]
    pub fn try_start(&self) -> Option<usize> {
        match self {
            Location::Source(_, start, _) => Some(*start as usize),
            _ => None,
        }
    }

    /// Возвращает `Some(end)` для [`Location::Source`], `None` для остальных.
    ///
    /// Используйте вместо [`end()`](Location::end) там, где Location может быть
    /// не-файловым вариантом.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::diagnostics::Location;
    ///
    /// let src = Location::Source(0, 3, 8);
    /// assert_eq!(src.try_end(), Some(8));
    ///
    /// let cmd = Location::CommandLine;
    /// assert_eq!(cmd.try_end(), None);
    /// ```
    #[inline]
    pub fn try_end(&self) -> Option<usize> {
        match self {
            Location::Source(_, _, end) => Some(*end as usize),
            _ => None,
        }
    }

    /// Возвращает `Some(start..end)` для [`Location::Source`], `None` для остальных.
    ///
    /// Используйте вместо [`range()`](Location::range) там, где Location может быть
    /// [`Builtin`](Location::Builtin), [`Codegen`](Location::Codegen) и т.д.
    ///
    /// # Примеры
    ///
    /// ```
    /// use takt_lang::diagnostics::Location;
    ///
    /// let src = Location::Source(0, 3, 8);
    /// assert_eq!(src.try_range(), Some(3..8));
    ///
    /// let builtin = Location::Builtin;
    /// assert_eq!(builtin.try_range(), None);
    /// ```
    #[inline]
    pub fn try_range(&self) -> Option<std::ops::Range<usize>> {
        match self {
            Location::Source(_, start, end) => Some(*start as usize..*end as usize),
            _ => None,
        }
    }
}

// --- V5: Тесты безопасного API ------------------------------------------------

#[cfg(test)]
mod tests_v5_location {
    use super::Location;

    /// V5: try_start() для Source возвращает начало диапазона.
    #[test]
    fn try_start_source_returns_start() {
        assert_eq!(Location::Source(0, 10, 20).try_start(), Some(10));
    }

    /// V5: try_start() для Implicit возвращает None (не паникует).
    #[test]
    fn try_start_implicit_returns_none() {
        assert_eq!(Location::Implicit.try_start(), None);
    }

    /// V5: try_start() для Builtin возвращает None (не паникует).
    #[test]
    fn try_start_builtin_returns_none() {
        assert_eq!(Location::Builtin.try_start(), None);
    }

    /// V5: try_start() для CommandLine возвращает None (не паникует).
    #[test]
    fn try_start_commandline_returns_none() {
        assert_eq!(Location::CommandLine.try_start(), None);
    }

    /// V5: try_start() для Codegen возвращает None (не паникует).
    #[test]
    fn try_start_codegen_returns_none() {
        assert_eq!(Location::Codegen.try_start(), None);
    }

    /// V5: try_end() для Source возвращает конец диапазона.
    #[test]
    fn try_end_source_returns_end() {
        assert_eq!(Location::Source(0, 3, 8).try_end(), Some(8));
    }

    /// V5: try_end() для Implicit возвращает None (не паникует).
    #[test]
    fn try_end_implicit_returns_none() {
        assert_eq!(Location::Implicit.try_end(), None);
    }

    /// V5: try_range() для Source возвращает диапазон start..end.
    #[test]
    fn try_range_source_returns_range() {
        assert_eq!(Location::Source(0, 3, 8).try_range(), Some(3..8));
    }

    /// V5: try_range() для Builtin возвращает None (не паникует).
    #[test]
    fn try_range_builtin_returns_none() {
        assert_eq!(Location::Builtin.try_range(), None);
    }

    /// V5: try_range() для Codegen возвращает None (не паникует).
    #[test]
    fn try_range_codegen_returns_none() {
        assert_eq!(Location::Codegen.try_range(), None);
    }

    /// V5: Инвариант - start() паникует для Implicit (документированное поведение).
    #[test]
    #[should_panic(expected = "местоположение не является файловой позицией")]
    fn start_implicit_panics_as_documented() {
        let _ = Location::Implicit.start();
    }

    /// V5: Инвариант - end() паникует для Builtin (документированное поведение).
    #[test]
    #[should_panic(expected = "местоположение не является файловой позицией")]
    fn end_builtin_panics_as_documented() {
        let _ = Location::Builtin.end();
    }

    /// V5: Инвариант - range() паникует для Codegen (документированное поведение).
    #[test]
    #[should_panic(expected = "местоположение не является файловой позицией")]
    fn range_codegen_panics_as_documented() {
        let _ = Location::Codegen.range();
    }
}
