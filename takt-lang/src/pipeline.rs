//! Общий конвейер: разбор -> построение дерева -> сбор диагностик.
//!
//! Отвечает на один вопрос: как получить модель - или все её ошибки - из текста.

use crate::diagnostics::{self, Diagnostic};
use crate::{parse, semantic};

/// Проверка "файл в позиции входа исполнения" - наружу для симулятора.
///
/// Правило одно на оба инструмента: цели зовут его ниже, в [`parse_and_construct`], а
/// `takt-sim` - по этому пути. Два ответа на один вход разошлись бы.
///
/// Реэкспорт живёт здесь, а не в `semantic/mod.rs`: тот файл - узаконенный долг реестра
/// размеров (`scripts/module-size-baseline.txt`), и расти ему нельзя даже на строку.
pub use crate::semantic::validate::validate_entry_model;
// Подсказка "кто подключает эту библиотеку" - спутник `validate_entry_model`: её зовут
// те же два потребителя, что и проверку, и оба обязаны отвечать одинаково. Слой
// `semantic::import` наружу крейта закрыт, поэтому доступ к подсказке даётся здесь же,
// рядом с проверкой.
pub use crate::semantic::import::importers::{find_importers, importers_note};

/// Разбирает и строит модель, проставляя диагностике **путь её файла**.
///
/// Общий шаг всех целей. Заведён потому, что путь нужно разрешить там, где
/// [`FileTable`](diagnostics::FileTable) ещё жив: реестр - деталь компиляции и наружу
/// не выходит, а `Location` несёт лишь номер файла. Без этого диагностика из
/// импортированной библиотеки была неотличима от своей - `taktc` печатал обе дословно
/// одинаково. Возвращает **и реестр файлов**: диагностика цели рождается после этого
/// шага, а путь её файла разрешается только реестром. Штамповать имя входного файла
/// вместо реестра **нельзя**: смещения принадлежат файлу диагностики, а он может быть
/// импортированным.
pub(crate) fn parse_and_construct(
    filename: &str,
    source: &str,
    search_paths: &[String],
    options: &crate::generator::GenerateOptions,
) -> Result<Compilation, Diagnostic> {
    let mut files = diagnostics::FileTable::new(filename);

    // Корневой файл - номер 0 (его зарегистрировал `FileTable::new`).
    let (model_ast, source_comments) = parse(source, 0).map_err(|ds| {
        let d = ds.into_iter().next().unwrap();
        stamp_file(d, &files)
    })?;

    let model = semantic::tree::construct_model_with_files(
        &model_ast,
        None,
        search_paths,
        &mut files,
        options.specialize,
    )
    .map_err(|d| stamp_file(d, &files))?;

    // Библиотечный файл (без единого состояния) законен, но входом исполнения быть не
    // может. Проверка стоит здесь, а не в `validate_model_all`: позиция "вход" -
    // свойство вызова, а не модели, и тому же дереву, пришедшему из `import`, она не
    // адресована.
    if let Some(mut d) = semantic::validate::validate_entry_model(&model) {
        // Подсказка "кто эту библиотеку подключает": причину `SE-102` называла и
        // раньше, а следующий шаг автор делал сам - искал импортёра глазами. Поиск
        // живёт здесь, а не в проверке: он ходит по файловой системе, а семантика её не
        // знает (та же граница, по которой сама проверка стоит в конвейере, а не в
        // `validate_model_all`).
        if let Some(note) = semantic::import::importers::importers_note(filename, search_paths) {
            d.notes.push(diagnostics::Note {
                // Позиции у подсказки нет по существу: она говорит о другом файле, и
                // координата в своём была бы ложью. `Location::Codegen` печатается без
                // префикса - на файл указывает сам текст заметки.
                loc: diagnostics::Location::Codegen,
                message: note,
            });
        }
        return Err(stamp_file(d, &files));
    }
    // Подстановка тела функции - тоже после семантики и тоже в конвейере цели: атрибут
    // автора действует всегда, эвристика - по флагу.
    //
    // Эталон этот проход не зовёт **намеренно**: подстановка меняет форму, а не
    // поведение, и сверка "эталон против прошивки" тем и доказывает тождественность.
    // Позови её обе стороны - сверка перестала бы видеть дефект подстановки.
    semantic::inline::inline_functions(
        &model,
        matches!(options.inline, crate::generator::InlinePolicy::Auto),
    );
    // Guard границ массива - по флагу и после семантики: проходу нужен разрешённый тип
    // базы, а печатникам целей о его форме знать не нужно. Место - конвейер, а не
    // стадии: флаг принадлежит вызову цели, и эталон зовёт проход своим
    // (`takt-sim --bounds-check`).
    //
    // Guard идёт после подстановки: защита охраняет то, что напечатается, а решение о
    // подстановке не зависит от того, включена ли защита. Обратный порядок ломает оба
    // правила: эвристика подстановки мерила бы тело, раздутое обёртками, а ветвь отказа
    // приезжала бы в место вызова блоком из двух операторов, из которого цель `rust`
    // печатает отложенное объявление, отвергаемое `clippy` (`needless_late_init`).
    if options.bounds_check {
        semantic::bounds_guard::insert_bounds_guards(&model);
    }
    Ok(Compilation {
        model,
        files,
        comments: std::rc::Rc::new(crate::generator::comments::SourceComments::new(
            source,
            &source_comments,
        )),
    })
}

/// Единица компиляции: построенная модель **и** реестр её файлов.
pub(crate) struct Compilation {
    /// Построенное семантическое дерево.
    pub(crate) model: std::rc::Rc<std::cell::RefCell<semantic::ModelNode>>,
    /// Реестр файлов компиляции: по нему разрешается путь диагностики.
    files: diagnostics::FileTable,
    /// Комментарии автора модели.
    ///
    /// Живут здесь, а не в опциях вызывающего: их знает только тот, кто видел исходник,
    /// а опции приходят снаружи и о тексте не осведомлены.
    comments: std::rc::Rc<crate::generator::comments::SourceComments>,
}

impl Compilation {
    /// Проставляет диагностике путь её файла.
    ///
    /// Метод типа, а не свободная функция с публичным полем: путь ставит владелец
    /// реестра, и это то же решение, что у [`Compilation::emit`]. Нужен он
    /// диагностикам, рождённым вне генерации, - разрешению адресов: его отказ печатался
    /// без координаты.
    pub(crate) fn stamp(&self, diagnostic: Diagnostic) -> Diagnostic {
        stamp_file(diagnostic, &self.files)
    }

    /// Понижения, зависящие от цели, - с путём файла в диагностике.
    ///
    /// Отдельный метод по той же причине, что [`Compilation::stamp`]: отказ разворота
    /// составного порта (`SE-130`) рождается вне генерации, и без стампа печатался бы
    /// без координаты.
    pub(crate) fn lower_for_target(
        &self,
        split: semantic::condition::port_split::PortSplit,
        fold_state_observe: bool,
    ) -> Result<(), Diagnostic> {
        semantic::condition::observe::lower_for_target(&self.model, split, fold_state_observe)
            .map_err(|d| self.stamp(d))
    }

    /// Печатает цель В память, проставляя диагностике путь её файла.
    ///
    /// Тот же отбор вставок и та же печать, что у [`Compilation::emit`]; разница одна -
    /// файлы возвращаются, а не кладутся на диск.
    pub(crate) fn emit_texts(
        &self,
        language: crate::generator::Language,
        options: &crate::generator::GenerateOptions,
    ) -> Result<crate::generator::Output, Diagnostic> {
        crate::semantic::target_block::prune(
            &self.model,
            crate::semantic::target_block::label_of(&language),
        );
        // Комментарии подставляются здесь: вызывающий их не знает, а генератору они
        // нужны - иначе переносить нечего.
        let mut options = options.clone();
        options.comments = Some(self.comments.clone());
        crate::generator::generate_texts(language, &self.model.borrow(), &options)
            .map_err(|d| stamp_file(d, &self.files))
    }
}

/// Первая ошибка списка диагностик (предупреждения пропускаются).
///
/// Живёт здесь, а не у двух вызывающих: разрешение адресов отдаёт смешанный список, и
/// "найти в нём отказ" - одно правило на обе цели с адресами.
pub(crate) fn first_error(diagnostics: &[Diagnostic]) -> Option<Diagnostic> {
    diagnostics
        .iter()
        .find(|d| d.level == diagnostics::Level::Error)
        .cloned()
}

/// Разрешает номер файла диагностики в путь.
pub(crate) fn stamp_file(d: Diagnostic, files: &diagnostics::FileTable) -> Diagnostic {
    let path = files.path_of(&d.loc).map(str::to_string);
    d.with_file_if_unset(path.as_deref())
}

/// **Все** ошибки исходного текста за один прогон.
///
/// Тот же конвейер, что у `compile_to_*` (`parse` -> построение дерева -> проверки), но
/// результат - **список**, а не первая встреченная ошибка. Пустой список означает, что
/// модель строится.
///
/// # Что уже накоплено, а что нет
///
/// - **Разбор** отдаёт все свои ошибки (лексер + парсер) - они и так собирались
///   в `Vec`, но терялись на стыке: вызывающий брал первую.
/// - **Построение дерева** остаётся терминальным: после ошибки дерево неполно, и
///   продолжение дало бы сообщения о следствиях, а не о причинах (решение
///   ).
/// - **Проверки** (`validate`) высказываются все: они идут по готовому дереву и
///   независимы друг от друга.
pub fn collect_compile_diagnostics(
    filename: &str,
    source: &str,
    search_paths: &[String],
    specialize: bool,
) -> Vec<Diagnostic> {
    let mut files = diagnostics::FileTable::new(filename);

    let model_ast = match parse(source, 0) {
        Ok((ast, _)) => ast,
        Err(ds) => {
            let stamped = ds.into_iter().map(|d| stamp_file(d, &files)).collect();
            return diagnostics::normalize(stamped);
        }
    };

    // Стадии построения и проверки разделены намеренно. Слитно (через
    // `construct_model_with_files`) получить всё нельзя - тот вход отдаёт первую ошибку
    // по контракту.
    //
    // С стадии построения тоже отдают **список**: внутри стадий 4-6 (тела блоков,
    // функций, условия рёбер) диагностики накапливаются, между стадиями - нет. Неполное
    // дерево при этом наружу не выходит, поэтому здесь на руках либо готовая модель,
    // либо только диагностики.
    let model = match semantic::stages::construct_stages(
        &model_ast,
        None,
        search_paths,
        &mut files,
        specialize,
    ) {
        Ok(model) => model,
        Err(ds) => {
            let stamped = ds.into_iter().map(|d| stamp_file(d, &files)).collect();
            return diagnostics::normalize(stamped);
        }
    };

    let found = semantic::validate::validate_model_all(model)
        .into_iter()
        .map(|d| stamp_file(d, &files))
        .collect();
    diagnostics::normalize(found)
}
