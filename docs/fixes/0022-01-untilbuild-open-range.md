# Исправление: открытый верхний диапазон совместимости IDE

> Фича: [../features/0022-intellij-syntax-highlight.md](../features/0022-intellij-syntax-highlight.md) · анализ: [../features/0022-intellij-syntax-highlight.md#анализ](../features/0022-intellij-syntax-highlight.md#анализ) · отчёт: [../features/0022-intellij-syntax-highlight.md#отчёт-о-тестировании](../features/0022-intellij-syntax-highlight.md#отчёт-о-тестировании)

## Проблема

При установке собранного плагина `Lam 0.1.0` в актуальную IDE пользователь получил
ошибку:

> Plugin 'Lam' (version '0.1.0') is not compatible with the current version of the
> IDE, because it requires build 243.* or older but the current build is
> RR-261.26222.73

То есть плагин не ставится в IDE новее сборки 243 (напр. RustRover 261 / 2026.1).
Обнаружено при эксплуатации (реальная установка), на стадии приёмки фичи.

## Причина

Верхняя граница совместимости была задана жёстко: `pluginUntilBuild = 243.*`
(выбрана при 0022-01 под целевую линейку платформы 2024.1.x). Значение попадает в
`<idea-version until-build="243.*">` и **блокирует** установку в любые IDE со
сборкой > 243, хотя плагин собран под 2024.1.x лишь из-за ограничения окружения
(JDK 17), а фактически использует только **давно стабильные** API платформы
(`FileType`, `Language`, `SyntaxHighlighter`, `LexerBase`, `Commenter`,
`PairedBraceMatcher`, `ColorSettingsPage`), совместимые с новыми версиями.

## Решение

Убрана верхняя граница совместимости — `until-build` больше не ограничивает
установку (открытый диапазон, документированный JetBrains способ):

- `gradle.properties`: `pluginUntilBuild =` (пусто); `pluginVersion 0.1.0 → 0.1.1`.
- `build.gradle.kts`: `untilBuild = providers.gradleProperty("pluginUntilBuild").orElse("")`
  — пустое значение → `<idea-version since-build="241" until-build="" />`
  (IntelliJ трактует пустой `until-build` как отсутствие верхней границы).
- `sinceBuild = 241` сохранён (нижняя граница; плагин собран под 2024.1.x).

Сборка под платформу 2024.1.x/JDK 17 не менялась — правка затрагивает только
метаданные диапазона совместимости.

## Проверки

- `./gradlew buildPlugin` → BUILD SUCCESSFUL; в `build/tmp/patchPluginXml/plugin.xml`
  — `<idea-version since-build="241" until-build="" />` (верхняя граница снята),
  `<version>0.1.1</version>`; собран `intellij-lam-0.1.1.zip`.
- `./gradlew test` → **27/27 зелёные** (правка конфигурационная, код не затронут).
- Соответствие критерию приёмки A7 (совместимость) — теперь плагин ставится и в
  IDE новее 243 (RustRover 261 и далее).
