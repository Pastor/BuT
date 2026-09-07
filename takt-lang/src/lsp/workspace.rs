//! Рабочая область сервера: обход файлов, граф импортов и связывание символов через
//! границу файла.
//!
//! # Что связывается через границу файла
//!
//! Импорт переносит импортёру верхнеуровневые объявления подключённого файла -
//! переменные, типы, структуры, перечисления, функции (усыновление). Такие имена автор
//! пишет голыми, и слой использований помечает их `unresolved`: "имя есть, связать не с
//! чем". Этой пометки и графа импортов достаточно, чтобы найти ссылку.
//!
//! **Имя, введённое самим импортом, - не имя объявления источника.** `import
//! "helper.takt";` вводит имя `Helper`, полученное из **имени файла**, а не из `model
//! Helper` внутри; `import "engine.takt" as Motor;` вводит `Motor`, хотя модель зовётся
//! `Engine`. Совпадение имени файла с именем модели (фикстура `goto56/helper.takt`) -
//! случайность, и связывание "одинаковое имя ⇒ один символ" приписало бы импортёру
//! чужое объявление, то есть дало бы **ложную правку**. Поэтому такие имена принадлежат
//! импортёру, а ссылкой на источник считается лишь **исходное** имя формы `import { A
//! }` / `{ A as B }`.
//!
//! Часть модуля `lsp`.

use crate::parser::ast;
use crate::semantic::usages::{self, UsageKind, UsageTable};
use std::collections::{BTreeMap, BTreeSet};

/// Каталоги, в которые обход не спускается: там не исходники автора.
const SKIPPED_DIRS: [&str; 4] = [".git", "target", "node_modules", ".idea"];

/// Имя с диапазоном в тексте своего файла.
type NamedRange = (String, u32, u32);

/// Одно вхождение имени в рабочей области.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Occurrence {
    /// Путь файла, в котором стоит вхождение.
    pub path: String,
    /// Начало имени (байтовое смещение в тексте этого файла).
    pub start: u32,
    /// Конец имени (эксклюзивно).
    pub end: u32,
    /// Это объявление символа (а не ссылка на него).
    pub declaration: bool,
}

/// Символ под курсором и всё, что о нём знает область.
///
/// Слой **не решает**, отказать ли в переименовании: он отдаёт факты, а политику отказа
/// держит `rename` (одно знание об одном предмете).
#[derive(Debug, Clone)]
pub struct Resolution {
    /// Имя символа.
    pub name: String,
    /// Все вхождения - в файле объявления и у потребителей.
    pub occurrences: Vec<Occurrence>,
    /// Символ виден за пределами своего файла (верхнеуровневое объявление).
    pub exported: bool,
    /// Имя введено самим `import` (алиас либо имя по имени файла): за границей файла
    /// оно не связано ни с каким объявлением источника.
    pub import_binding: bool,
    /// Имя объявлено более чем одним импортированным файлом - какое из них имеется в
    /// виду, область не знает.
    pub ambiguous: bool,
    /// Файлы, которые импортируют объявителя, но не разбираются: вхождения в них могли
    /// остаться незамеченными.
    pub unparsable_consumers: Vec<String>,
}

impl Resolution {
    /// Файлы, которых коснётся переименование.
    pub fn touched_files(&self) -> BTreeSet<&str> {
        self.occurrences.iter().map(|o| o.path.as_str()).collect()
    }
}

/// Разбор одной директивы импорта: путь и имена, которые она вводит либо упоминает.
#[derive(Debug)]
struct ImportSpec {
    /// Путь как написан (разрешается правилами ядра).
    path: ast::ImportPath,
    /// Имена, вводимые импортом в **этот** файл (алиас `as M`).
    bindings: Vec<NamedRange>,
    /// Исходные имена формы `{ A }` / `{ A as B }` - ссылки на объявления подключаемого
    /// файла.
    selected: Vec<NamedRange>,
    /// Индекс подключаемого файла в области; `None` - вне области либо не найден
    /// правилами поиска.
    target: Option<usize>,
}

/// Один файл рабочей области.
#[derive(Debug)]
struct WsFile {
    path: String,
    text: String,
    /// Таблица вхождений; `None` - файл не разбирается.
    table: Option<UsageTable>,
    /// Имена верхнеуровневых объявлений - то, что импорт переносит импортёру.
    exports: BTreeSet<String>,
    /// Директивы импорта этого файла.
    imports: Vec<ImportSpec>,
}

/// Рабочая область: файлы, их таблицы вхождений и граф импортов.
#[derive(Debug)]
pub struct Workspace {
    files: Vec<WsFile>,
}

impl Workspace {
    /// Сканирует область: обходит корни, читает и разбирает каждый `.takt`, разрешает
    /// директивы импорта.
    ///
    /// `overlay` отдаёт текст **открытых** документов: у редактора он свежее того, что
    /// на диске, и правка, построенная по диску, встала бы не туда.
    pub fn scan(
        roots: &[String],
        search_paths: &[String],
        overlay: &dyn Fn(&str) -> Option<String>,
    ) -> Self {
        let mut paths: Vec<String> = Vec::new();
        for root in roots {
            collect_takt_files(std::path::Path::new(root), &mut paths);
        }
        paths.sort();
        paths.dedup();

        let mut files: Vec<WsFile> = paths
            .into_iter()
            .filter_map(|path| {
                let text = match overlay(&path) {
                    Some(t) => t,
                    None => std::fs::read_to_string(&path).ok()?,
                };
                Some(WsFile::new(path, text))
            })
            .collect();

        // Цели импорта проставляются вторым проходом: до конца первого индексы файлов
        // ещё не известны.
        let index: BTreeMap<String, usize> = files
            .iter()
            .enumerate()
            .map(|(i, f)| (normalize(&f.path), i))
            .collect();
        for file in &mut files {
            let dir_paths = search_paths_for(&file.path, search_paths);
            for spec in &mut file.imports {
                spec.target = crate::semantic::import::read_import_file(&dir_paths, &spec.path)
                    .ok()
                    .and_then(|(_, filename)| index.get(&normalize(&filename)).copied());
            }
        }
        Workspace { files }
    }

    /// Число файлов области.
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// Пуста ли область.
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// Текст файла области.
    pub fn text_of(&self, path: &str) -> Option<&str> {
        self.file_index(path).map(|i| self.files[i].text.as_str())
    }

    /// Объявлено ли имя в файле - проверка столкновения перед переименованием.
    pub fn declares_name(&self, path: &str, name: &str) -> bool {
        let Some(i) = self.file_index(path) else {
            return false;
        };
        let Some(table) = self.files[i].table.as_ref() else {
            return false;
        };
        table
            .usages()
            .iter()
            .any(|u| u.kind == UsageKind::Declaration && u.name == name)
    }

    /// Разрешает символ под курсором и собирает его вхождения по всей области.
    ///
    /// `None` - файла нет в области, он не разбирается либо под курсором нет имени.
    pub fn resolve(&self, path: &str, offset: usize) -> Option<Resolution> {
        let origin = self.file_index(path)?;
        let table = self.files[origin].table.as_ref()?;

        // Случай 1: имя связано в своём файле.
        if let Some(usage) = table.usage_at(offset) {
            let name = usage.name.clone();
            let import_binding = self.is_import_binding(origin, usage.start);
            let mut occurrences = self.local_occurrences(origin, usage.symbol);
            let exported = self.files[origin].exports.contains(&name);
            if exported && !import_binding {
                occurrences.extend(self.consumer_occurrences(origin, &name));
            }
            let mut unparsable_consumers = if exported && !import_binding {
                self.unparsable_consumers(&name)
            } else {
                Vec::new()
            };
            // Имя, выбранное импортом (`import { a } from "...";`), объявлено в
            // соседнем файле: к вхождениям импортёра добавляются объявление источника и
            // вхождения у прочих его потребителей. Иначе ответ зависел бы от того, куда
            // поставлен курсор, - на директиву импорта или на имя в теле.
            let mut ambiguous = false;
            if usage.symbol_kind == usages::SymbolKind::Imported {
                let sources = self.sources_of(origin, &name);
                if let Some(&declaring) = sources.first() {
                    occurrences.extend(self.declaration_occurrences(declaring, &name));
                    // Свои вхождения уже собраны выше: у потребителя-импортёра они
                    // пришли из `local_occurrences`, и дубль сделал бы список неверным
                    // по счёту.
                    let own = self.files[origin].path.clone();
                    occurrences.extend(
                        self.consumer_occurrences(declaring, &name)
                            .into_iter()
                            .filter(|o| o.path != own),
                    );
                    unparsable_consumers = self.unparsable_consumers(&name);
                }
                ambiguous = sources.len() > 1;
            }
            return Some(Resolution {
                name,
                occurrences,
                exported,
                import_binding,
                ambiguous,
                unparsable_consumers,
            });
        }

        // Случай 2: имя не связано - оно пришло из импорта либо ниоткуда.
        let unresolved = table.unresolved_at(offset)?;
        let name = unresolved.name.clone();
        let here = Occurrence {
            path: self.files[origin].path.clone(),
            start: unresolved.start,
            end: unresolved.end,
            declaration: false,
        };
        let sources = self.sources_of(origin, &name);
        let Some(&declaring) = sources.first() else {
            // Ни один импортированный файл имени не объявляет: встроенное имя, вариант
            // перечисления или опечатка - связывать не с чем.
            return Some(Resolution {
                name,
                occurrences: vec![here],
                exported: false,
                import_binding: false,
                ambiguous: false,
                unparsable_consumers: Vec::new(),
            });
        };
        let mut occurrences = self.declaration_occurrences(declaring, &name);
        occurrences.extend(self.consumer_occurrences(declaring, &name));
        let unparsable_consumers = self.unparsable_consumers(&name);
        Some(Resolution {
            name,
            occurrences,
            exported: true,
            import_binding: false,
            ambiguous: sources.len() > 1,
            unparsable_consumers,
        })
    }

    fn file_index(&self, path: &str) -> Option<usize> {
        let want = normalize(path);
        self.files.iter().position(|f| normalize(&f.path) == want)
    }

    /// Вхождения символа в его собственном файле.
    fn local_occurrences(&self, file: usize, symbol: usages::SymbolId) -> Vec<Occurrence> {
        let Some(table) = self.files[file].table.as_ref() else {
            return Vec::new();
        };
        table
            .occurrences_of(symbol)
            .into_iter()
            .map(|u| Occurrence {
                path: self.files[file].path.clone(),
                start: u.start,
                end: u.end,
                declaration: u.kind == UsageKind::Declaration,
            })
            .collect()
    }

    /// Стоит ли по смещению имя, введённое самим `import` (алиас `as M`)?
    fn is_import_binding(&self, file: usize, start: u32) -> bool {
        self.files[file]
            .imports
            .iter()
            .flat_map(|i| i.bindings.iter())
            .any(|(_, s, _)| *s == start)
    }

    /// Вхождения символа в файле, где он объявлен (по имени: символ найден через
    /// границу файла, `SymbolId` соседа здесь не пригоден).
    fn declaration_occurrences(&self, file: usize, name: &str) -> Vec<Occurrence> {
        let Some(table) = self.files[file].table.as_ref() else {
            return Vec::new();
        };
        let Some(decl) = table
            .usages()
            .iter()
            .find(|u| u.kind == UsageKind::Declaration && u.name == name)
        else {
            return Vec::new();
        };
        self.local_occurrences(file, decl.symbol)
    }

    /// Вхождения имени во всех файлах, которые (транзитивно) импортируют `declaring`.
    fn consumer_occurrences(&self, declaring: usize, name: &str) -> Vec<Occurrence> {
        let mut out: Vec<Occurrence> = Vec::new();
        for (i, file) in self.files.iter().enumerate() {
            if i == declaring || !self.imports_transitively(i, declaring) {
                continue;
            }
            // Локальное объявление затеняет импортированное: такие вхождения связаны в
            // своём файле и в `unresolved` не попадают.
            if let Some(table) = file.table.as_ref() {
                for u in table.unresolved() {
                    if u.name == name {
                        out.push(Occurrence {
                            path: file.path.clone(),
                            start: u.start,
                            end: u.end,
                            declaration: false,
                        });
                    }
                }
                // Форма `import { a } from "...";` вводит имя, и с вхождения в теле
                // связываются с ним (вид `Imported`) - то есть в `unresolved` их больше
                // нет. Берём их у символа: иначе потребитель этой формы отдал бы только
                // строку импорта, а потребитель формы `import "...";` - все свои
                // вхождения.
                if let Some(decl) = table.usages().iter().find(|u| {
                    u.kind == UsageKind::Declaration
                        && u.name == name
                        && u.symbol_kind == usages::SymbolKind::Imported
                }) {
                    for u in table.occurrences_of(decl.symbol) {
                        if !out
                            .iter()
                            .any(|o| o.start == u.start && o.path == file.path)
                        {
                            out.push(Occurrence {
                                path: file.path.clone(),
                                start: u.start,
                                end: u.end,
                                declaration: false,
                            });
                        }
                    }
                }
            }
            // Исходное имя формы `import { A }` - ссылка на объявление источника;
            // таблица же считает его объявлением (вид `Model`), поэтому берётся отсюда,
            // а не из неё.
            for imp in &file.imports {
                if imp.target != Some(declaring) {
                    continue;
                }
                for (n, start, end) in &imp.selected {
                    if n == name && !out.iter().any(|o| o.start == *start && o.path == file.path) {
                        out.push(Occurrence {
                            path: file.path.clone(),
                            start: *start,
                            end: *end,
                            declaration: false,
                        });
                    }
                }
            }
        }
        out
    }

    /// Файлы, которые могли бы содержать вхождения имени, но не разбираются.
    ///
    /// Граф импортов тут бесполезен **по устройству**: у неразобранного файла АСД нет,
    /// а значит нет и директив импорта - он не числится потребителем ни у кого. Поэтому
    /// подозрение строится по тексту: файл опасен, если в нём есть и слово `import`
    /// (без него чужие имена ему не видны), и само искомое имя (без него правка его не
    /// коснулась бы). Оба признака ошибаются в сторону **лишнего отказа**, а не
    /// пропущенной правки.
    fn unparsable_consumers(&self, name: &str) -> Vec<String> {
        self.files
            .iter()
            .filter(|f| f.table.is_none())
            .filter(|f| f.text.contains("import") && f.text.contains(name))
            .map(|f| f.path.clone())
            .collect()
    }

    /// Импортированные файлы, объявляющие имя (прямо или транзитивно).
    fn sources_of(&self, importer: usize, name: &str) -> Vec<usize> {
        self.reachable_imports(importer)
            .into_iter()
            .filter(|i| self.files[*i].exports.contains(name))
            .collect()
    }

    /// Импортирует ли `from` файл `target` - прямо или через цепочку.
    fn imports_transitively(&self, from: usize, target: usize) -> bool {
        self.reachable_imports(from).contains(&target)
    }

    /// Транзитивное замыкание импортов файла.
    ///
    /// Обход итеративный и с пометками: цикл импорта - ошибка компилятора (`SE-014`),
    /// но сервер работает и с текстом, который её содержит, и зациклиться не вправе.
    fn reachable_imports(&self, from: usize) -> BTreeSet<usize> {
        let mut seen = BTreeSet::new();
        let mut stack = vec![from];
        while let Some(i) = stack.pop() {
            for imp in &self.files[i].imports {
                if let Some(t) = imp.target
                    && seen.insert(t)
                {
                    stack.push(t);
                }
            }
        }
        seen
    }
}

impl WsFile {
    fn new(path: String, text: String) -> Self {
        let parsed = crate::parse(&text, 0).ok();
        let (table, exports, imports) = match parsed.as_ref() {
            Some((ast, _)) => (
                Some(usages::collect_usages(ast)),
                top_level_exports(ast),
                import_specs(ast),
            ),
            None => (None, BTreeSet::new(), Vec::new()),
        };
        WsFile {
            path,
            text,
            table,
            exports,
            imports,
        }
    }
}

/// Имена верхнеуровневых объявлений файла - ровно то, что импорт переносит импортёру
/// (усыновление: переменные, типы, структуры, перечисления, функции).
///
/// Модели сюда не входят: их имя за границей файла не употребляется - импортёр видит
/// имя, полученное из имени файла либо заданное алиасом.
fn top_level_exports(ast: &ast::Model) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for element in &ast.elements {
        let name: Option<&ast::Identifier> = match element {
            ast::ModelElement::Variable(v) => variable_name(v),
            ast::ModelElement::Type(t) => Some(&t.name),
            ast::ModelElement::Struct(s) => s.name.as_ref(),
            ast::ModelElement::Enum(e) => e.name.as_ref(),
            ast::ModelElement::Function(f) => f.name.as_ref(),
            _ => continue,
        };
        if let Some(name) = name {
            out.insert(name.name.clone());
        }
    }
    out
}

/// Имя объявления переменной, порта, константы или параметра.
///
/// Порт в списке экспорта остаётся намеренно: усыновление переносит импортёру **все**
/// `variables` корня, а порт - их частный случай (пример `pid_law.takt` отдаёт
/// применению именно порты `target`/`meas`/`ctrl`).
fn variable_name(define: &ast::VariableDefine) -> Option<&ast::Identifier> {
    match define {
        ast::VariableDefine::Variable { name, .. }
        | ast::VariableDefine::Port { name, .. }
        | ast::VariableDefine::Constant { name, .. }
        | ast::VariableDefine::Parameter { name, .. } => name.as_ref(),
    }
}

/// Разбирает директивы импорта: путь, вводимые имена и ссылки на чужие.
fn import_specs(ast: &ast::Model) -> Vec<ImportSpec> {
    let mut out = Vec::new();
    for element in &ast.elements {
        let ast::ModelElement::Import(def) = element else {
            continue;
        };
        let (path, bindings, selected) = match def {
            // `import "файл";` - имя выводится из имени файла, в тексте его нет.
            ast::ImportDefine::Plain(p, _) => (p.clone(), Vec::new(), Vec::new()),
            // `import "файл" as M;` - `M` принадлежит импортёру.
            ast::ImportDefine::GlobalSymbol(p, alias, _) => (
                p.clone(),
                named_range(alias).into_iter().collect(),
                Vec::new(),
            ),
            // `import { A as B } from "файл";` - `A` ссылается на объявление источника,
            // `B` принадлежит импортёру. Без алиаса имя одно и оно ссылка:
            // переименование источника обязано его задеть.
            ast::ImportDefine::Rename(p, names, _) => {
                let mut bindings = Vec::new();
                let mut selected = Vec::new();
                for (original, alias) in names {
                    selected.extend(named_range(original));
                    if let Some(alias) = alias {
                        bindings.extend(named_range(alias));
                    }
                }
                (p.clone(), bindings, selected)
            }
        };
        out.push(ImportSpec {
            path,
            bindings,
            selected,
            target: None,
        });
    }
    out
}

/// Имя идентификатора с его диапазоном; у порождённого имени позиции нет.
fn named_range(id: &ast::Identifier) -> Option<NamedRange> {
    match id.loc {
        crate::diagnostics::Location::Source(_, start, end) => Some((id.name.clone(), start, end)),
        _ => None,
    }
}

/// Пути поиска импорта для файла: заданные клиентом плюс каталог файла.
///
/// Каталог идёт **в конец** - так же, как в `semantic::tree`
/// (`search_paths_with_importer_dir`): иначе сервер разрешал бы импорт не в тот файл,
/// что компилятор.
fn search_paths_for(file: &str, search_paths: &[String]) -> Vec<String> {
    let mut paths = search_paths.to_vec();
    if let Some(parent) = std::path::Path::new(file).parent() {
        let dir = if parent.as_os_str().is_empty() {
            ".".to_string()
        } else {
            parent.to_string_lossy().into_owned()
        };
        if !paths.contains(&dir) {
            paths.push(dir);
        }
    }
    paths
}

/// Приводит путь к виду, по которому файлы сравниваются между собой.
///
/// Канонизация (`canonicalize`) читает диск и на несуществующем пути отказывает,
/// поэтому сравнение идёт по нормализованному тексту пути: лишние `./` и повторные
/// разделители снимаются, символические ссылки - нет.
fn normalize(path: &str) -> String {
    let mut out: Vec<&str> = Vec::new();
    for part in path.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                out.pop();
            }
            p => out.push(p),
        }
    }
    let joined = out.join("/");
    if path.starts_with('/') {
        format!("/{joined}")
    } else {
        joined
    }
}

/// Рекурсивно собирает `.takt`, пропуская служебные каталоги.
fn collect_takt_files(dir: &std::path::Path, out: &mut Vec<String>) {
    if dir.is_file() {
        if dir.extension().is_some_and(|e| e == "takt") {
            out.push(dir.to_string_lossy().into_owned());
        }
        return;
    }
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let skip = path
                .file_name()
                .is_some_and(|n| SKIPPED_DIRS.iter().any(|s| *s == n));
            if !skip {
                collect_takt_files(&path, out);
            }
        } else if path.extension().is_some_and(|e| e == "takt") {
            out.push(path.to_string_lossy().into_owned());
        }
    }
}
