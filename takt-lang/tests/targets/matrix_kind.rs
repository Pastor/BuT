//! Форма объявления, к которому идёт обращение, - ось матрицы проб.
//!
//! Вынесена из `matrix_probes` по границе ответственности (правило размера модуля,
//! `docs/CODE.md`): там живёт сборка исходника пробы, здесь - знание о том, как
//! выглядят объявление, чтение и запись у каждой формы. Контракт держит `pub(crate)`:
//! потребители берут `Kind` через `super::matrix_kind`.

/// Форма объявления того, к чему идёт обращение.
///
/// Ось заведена потому, что тип меняет и путь печати, и границы целей: массив и
/// структура в порту **разворачиваются по листам**, перечисление в порту `rust` и
/// `st-at` не размещают вовсе, а инициализатор массива от другой переменной цель `c` не
/// выражает.
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Kind {
    /// Скаляр `u8`.
    Scalar,
    /// Массив `[u8; 3]`.
    Array,
    /// Структура из двух полей.
    Struct,
    /// Перечисление из двух вариантов.
    Enum,
}

/// Формы объявления - перебор идёт по ним целиком.
pub(crate) const KINDS: [Kind; 4] = [Kind::Scalar, Kind::Array, Kind::Struct, Kind::Enum];

impl Kind {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Kind::Scalar => "scalar",
            Kind::Array => "array",
            Kind::Struct => "struct",
            Kind::Enum => "enum",
        }
    }

    /// Имя типа в объявлении.
    pub(crate) fn type_name(self) -> &'static str {
        match self {
            Kind::Scalar => "u8",
            Kind::Array => "[u8; 3]",
            Kind::Struct => "Pair",
            Kind::Enum => "Mode",
        }
    }

    /// Объявление типа, если оно нужно.
    pub(crate) fn type_declaration(self) -> &'static str {
        match self {
            Kind::Struct => "struct Pair {\n    lo: u8,\n    hi: u8\n}\n\n",
            Kind::Enum => "enum Mode {\n    Idle = 1,\n    Work = 2\n}\n\n",
            _ => "",
        }
    }

    /// Значение для инициализатора.
    pub(crate) fn literal(self) -> &'static str {
        match self {
            Kind::Scalar => "7",
            Kind::Array => "{4, 5, 6}",
            Kind::Struct => "{4, 5}",
            Kind::Enum => "Work",
        }
    }

    /// Начальное значение объявления в корне.
    pub(crate) fn root_literal(self) -> &'static str {
        match self {
            Kind::Scalar => "3",
            Kind::Array => "{1, 2, 3}",
            Kind::Struct => "{1, 2}",
            Kind::Enum => "Idle",
        }
    }

    /// Оператор, читающий значение `name` и прибавляющий его к `k`.
    pub(crate) fn read_statement(self, name: &str) -> String {
        match self {
            Kind::Scalar => format!("            k := k + {name};\n"),
            Kind::Array => format!("            k := k + {name}[1];\n"),
            // Читаются оба поля: у цели `sv` структурный порт остаётся одним сигналом,
            // и `verilator` под `-Wall` считает непрочитанные биты ошибкой.
            Kind::Struct => format!("            k := k + {name}.hi + {name}.lo;\n"),
            // У перечисления арифметики нет: читается сравнением.
            Kind::Enum => {
                format!(
                    "            if {name} = Work {{\n                k := k + 2;\n            }}\n"
                )
            }
        }
    }

    /// Оператор, читающий одну часть значения.
    pub(crate) fn partial_read_statement(self, name: &str) -> String {
        match self {
            Kind::Struct => format!("            k := k + {name}.hi;\n"),
            Kind::Array => format!("            k := k + {name}[0];\n"),
            // У скаляра и перечисления частей нет: чтение полное.
            _ => self.read_statement(name),
        }
    }

    /// Оператор, пишущий в `name` значение, зависящее от такта.
    pub(crate) fn write_statement(self, name: &str) -> String {
        match self {
            Kind::Scalar => format!("            {name} := k;\n"),
            Kind::Array => format!("            {name}[1] := k;\n"),
            Kind::Struct => format!("            {name}.hi := k;\n"),
            Kind::Enum => format!("            {name} := Work;\n"),
        }
    }
}
