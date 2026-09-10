/**
 * Ключи сборки: опись, разбор строки и сборка обратно.
 *
 * Модуль пока не отдаёт описи ключей: их знает разбор аргументов
 * (`takt-lang/src/compile_cli/`), а ручки "перечисли ключи" у него нет.
 * Опись живёт здесь и **сверяется с исходником разбора** тестом
 * `web/tests/web-tests.mjs` - как сверяются подписи площадок.
 *
 * Без сверки список разошёлся бы с компилятором молча: страница предложила
 * бы ключ, которого нет, либо умолчала о появившемся. Это тот же класс, из-за
 * которого в `web/` запрещён список ключевых слов Takt.
 *
 * Строка остаётся единственной величиной: конструктор её собирает, поле
 * ввода принимает как есть, и обе стороны читают одну и ту же строку. Заведи
 * конструктору своё состояние - и ручная правка строки начала бы теряться.
 */

/** Ключи сборки, применимые из браузера. */
export const FLAGS = [
  {
    key: "--fsm",
    label: "flags.fsm",
    choices: ["switch", "table"],
    fallback: "table",
  },
  { key: "--inline", label: "flags.inline", choices: ["off", "auto"], fallback: "auto" },
  {
    key: "--parameters",
    label: "flags.parameters",
    choices: ["assign", "specialize"],
    fallback: "specialize",
  },
  { key: "--bus", label: "flags.bus", choices: ["apb"], fallback: "apb", only: { apb: ["sv-mmio"] } },
  { key: "--bounds-check", label: "flags.boundsCheck", plain: true },
  { key: "--guard-disable", label: "flags.guardDisable", plain: true, clash: "--guard-enable" },
  { key: "--guard-enable", label: "flags.guardEnable", plain: true, clash: "--guard-disable" },
  { key: "--float-embedded", label: "flags.floatEmbedded", plain: true },
  { key: "--float-width", label: "flags.floatWidth", choices: ["32", "64"], fallback: "32" },
  {
    key: "--float-as-q",
    label: "flags.floatAsQ",
    numbers: [
      { name: "m", fallback: 16, min: 1, max: 32 },
      { name: "n", fallback: 16, min: 0, max: 32 },
    ],
    join: ".",
  },
  { key: "--tick-hz", label: "flags.tickHz", numbers: [{ name: "hz", fallback: 1000, min: 1, max: 1000000 }] },
];

/** Опись ключа по имени. */
export function flag(key) {
  return FLAGS.find((f) => f.key === key) ?? null;
}

/** Принимает ли цель ключ хотя бы в одном значении. */
export function applicable(spec, target) {
  if (!spec.only) return true;
  const values = spec.choices ?? Object.keys(spec.only);
  return values.some((value) => allows(spec, value, target));
}

/** Принимает ли цель ключ в этом значении. */
export function allows(spec, value, target) {
  const list = spec.only?.[value];
  return !list || list.includes(target);
}

/**
 * Разбирает строку ключей в состояние конструктора.
 *
 * Неизвестное сохраняется в `rest` и уезжает обратно в строку: страница не
 * вправе терять то, чего не знает, - ключ мог появиться в компиляторе раньше,
 * чем в описи.
 */
export function parse(line) {
  const chosen = new Map();
  const rest = [];
  for (const piece of String(line ?? "").split(/\s+/).filter(Boolean)) {
    const [name, value] = piece.includes("=") ? split(piece) : [piece, null];
    const spec = flag(name);
    if (!spec) {
      rest.push(piece);
      continue;
    }
    if (spec.plain) {
      chosen.set(name, { on: true });
      continue;
    }
    if (spec.numbers) {
      // Разделитель берётся, только когда чисел несколько: `split("")` рвёт значение
      // посимвольно, и `--tick-hz=500` стало бы `5`.
      const raw = String(value ?? "");
      const parts = spec.numbers.length > 1 ? raw.split(spec.join ?? ".") : [raw];
      const state = { on: true };
      spec.numbers.forEach((n, i) => {
        state[n.name] = parts[i] === undefined || parts[i] === "" ? n.fallback : Number(parts[i]);
      });
      chosen.set(name, state);
      continue;
    }
    chosen.set(name, { on: true, value: value ?? spec.fallback });
  }
  return { chosen, rest };
}

function split(piece) {
  const at = piece.indexOf("=");
  return [piece.slice(0, at), piece.slice(at + 1)];
}

/**
 * Собирает строку из состояния конструктора.
 *
 * Ключ, чьё значение цель не принимает, в строку не идёт: иначе страница
 * обещала бы сборку, которую компилятор отвергнет (`--bus=apb` у
 * `sv`). Неизвестное (`rest`) едет как есть.
 */
export function line({ chosen, rest }, target) {
  const parts = [];
  for (const spec of FLAGS) {
    const state = chosen.get(spec.key);
    if (!state?.on) continue;
    if (!applicable(spec, target)) continue;
    if (spec.plain) {
      parts.push(spec.key);
      continue;
    }
    if (spec.numbers) {
      const value = spec.numbers
        .map((n) => state[n.name] ?? n.fallback)
        .join(spec.join ?? ".");
      parts.push(`${spec.key}=${value}`);
      continue;
    }
    const value = state.value ?? spec.fallback;
    if (!allows(spec, value, target)) continue;
    parts.push(`${spec.key}=${value}`);
  }
  return [...parts, ...(rest ?? [])].join(" ");
}
