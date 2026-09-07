import org.jetbrains.intellij.platform.gradle.IntelliJPlatformType
import org.jetbrains.intellij.platform.gradle.TestFrameworkType

// Плагин IntelliJ IDEA для языка Takt - подсветка синтаксиса.
// Задача 0022-01 - каркас: FileType/Language, регистрация `.takt`.
// Лексер/highlighter (0022-02) и настройка цветов (0022-03) добавляются поверх.
plugins {
    id("java")
    kotlin("jvm") version "2.4.10"
    id("org.jetbrains.intellij.platform") version "2.18.1"
}

group = providers.gradleProperty("pluginGroup").get()
version = providers.gradleProperty("pluginVersion").get()

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        create(
            IntelliJPlatformType.fromCode(providers.gradleProperty("platformType").get()),
            providers.gradleProperty("platformVersion").get(),
        )
        testFramework(TestFrameworkType.Platform)
        // CLI IntelliJ Plugin Verifier - для задачи verifyPlugin (проверка
        // бинарной совместимости с новыми IDE).
        pluginVerifier()
        // LSP4IJ: зависимость на клиент LSP от Red Hat. Версия
        // фиксируется явно (API LSP4IJ моложе платформенных, риск ADR).
        // Опциональность объявляется в plugin.xml (`<depends optional="true"
        // config-file="takt-lsp4ij.xml">`): плагин ставится и работает без LSP4IJ
        // (лексический слой 0022), семантический слой включается лишь при её наличии.
        plugin("com.redhat.devtools.lsp4ij:0.20.1")
    }
    testImplementation("junit:junit:4.13.2")
    // Тест-фреймворк платформы 2024.2 ожидает opentest4j на classpath
    // (`org.opentest4j.AssertionFailedError`) - при подъёме с 2024.1
    // его перестало подтягивать транзитивно; добавляем явно.
    testImplementation("org.opentest4j:opentest4j:1.3.0")
}

intellijPlatform {
    // Инструментация кода (формы/NLS) не нужна: у плагина нет Java-исходников и форм.
    instrumentCode = false

    pluginConfiguration {
        ideaVersion {
            sinceBuild = providers.gradleProperty("pluginSinceBuild")
            // Открытый верхний диапазон: при пустом `pluginUntilBuild` провайдер
            // становится "отсутствующим" (filter отбрасывает пустое значение), и
            // атрибут until-build в дескрипторе не эмитится вовсе. Прежний
            // `.orElse("")` давал невалидный `until-build=""` (ловится Plugin
            // Verifier). Пустой until-build ⇒ плагин ставится в любые новые IDE.
            untilBuild = providers.gradleProperty("pluginUntilBuild").filter { it.isNotBlank() }
        }
    }

    // Проверка бинарной совместимости с новыми IDE (IntelliJ Plugin Verifier).
    // Открытый until-build обещает работу в свежих сборках - verifier это проверяет
    // на явном спреде релизов IC, которые новее сборочной платформы (2024.1.7).
    // `recommended()` не используем: он подтягивает ещё не вышедшую сборку (2025.3),
    // не резолвимую в релизном репозитории. Список правится по мере выхода IDE.
    pluginVerification {
        // Гасим косметический гайдлайн Marketplace "слово intellij в id плагина"
        // (org.takt.intellij): не влияет на совместимость, переименование id
        // сломало бы идентичность плагина.
        freeArgs = listOf("-mute", "TemplateWordInPluginId")
        // Список задаётся одним провайдером, а не тремя вызовами.
        // В плагине платформы 2.1.0 была строковая форма `ide("IC-2024.3")`; в
        // 2.18.1 её нет, а ближайшая замена `create(type, version)` заводит
        // одну конфигурацию на все версии, и обычный резолв Gradle сводит их к
        // старшей (`idea:ideaIC:2024.3 -> 2025.2` в выводе `dependencies`).
        // Форма ниже проверена прогоном: `verifyPlugin` выполняет три
        // верификации (IC-243, IC-251, IC-252) - покрытие сохранено.
        ides {
            create(providers.provider { listOf("IC-2024.3", "IC-2025.1", "IC-2025.2") })
        }
    }
}

// ── Пусковой JDK: преflight-проверка (перезамер 0224) ─────────────
//
// здесь два разных JDK, И путают именно их.
//
//  * JDK компиляции задаёт `jvmToolchain(21)` ниже. Gradle скачивает его сам
//    (foojay-резолвер в settings.gradle.kts) - об этом заботиться не нужно.
//  * JDK пусковой - тот, на котором работает демон Gradle. Его берут из
//    окружения, и ломался именно он: `jvmToolchain` его не меняет.
//
// причина прежнего отказа была названа неверно (замер 0224). Комментарий
// 0159 винил таблицу версий в `kotlin-gradle-plugin`; стектрейс показывает
// другое - падал встроенный В GRADLE компилятор Kotlin-скриптов, тот, что
// компилирует сам этот файл:
//     org.jetbrains.kotlin.com.intellij.util.lang.JavaVersion.parse
//     <- org.gradle.kotlin.dsl.support.KotlinCompilerKt.compileKotlinScriptToDirectory
// то есть версия `kotlin("jvm")` из `plugins {}` к отказу отношения не имела -
// он приходил до её применения. Проверка независимая: таблица commons-lang3
// даже у Kotlin 2.4.10 обрывается на JAVA_24, значит подъём одного Kotlin JDK 26
// не покрыл бы. Снял ограничение подъём GRADLE (8.10.2 -> 9.7.0) вместе с
// плагином платформы (2.1.0 -> 2.18.1).
//
// границы ниже - замер, А не догадка (прогон `./gradlew test`):
//   17 - работает (нижняя: её же требует Gradle 9);
//   26 - работает (openjdk 26.0.2, тот самый JDK, на котором прежняя связка
//        падала "26.0.2" без объяснений);
//   27+ - не проверено, потому что такого JDK нет. Отсюда предупреждение, а не
//        отказ: запрещать неизмеренное - та же догадка, только с другим знаком.
//
// Числа привязаны к связке Gradle 9.7.0 + IPP 2.18.1 + Kotlin 2.4.10. Подняли
// что-то из неё - перезамерьте границы, а не подвиньте их "на глаз".
run {
    val launcher = JavaVersion.current()
    val lastMeasuredGood = JavaVersion.VERSION_26

    if (launcher > lastMeasuredGood) {
        logger.warn(
            "ВНИМАНИЕ: пусковой JDK $launcher с этой сборкой не проверялся.\n" +
                "  Измерено (фича 0224): 17 и $lastMeasuredGood работают; выше — не проверялось.\n" +
                "  Если сборка упадёт сообщением из одного номера версии " +
                "(`* What went wrong: $launcher`), причина эта: встроенный в Gradle компилятор\n" +
                "  Kotlin-скриптов не разбирает такую версию. Лечение — поднять Gradle " +
                "(`./gradlew wrapper --gradle-version <новее>`)\n" +
                "  либо запустить сборку под JDK $lastMeasuredGood или ниже:\n" +
                "    JAVA_HOME=\$(/usr/libexec/java_home -v $lastMeasuredGood) ./gradlew <задача>\n" +
                "  Сработало или нет — стоит записать: тогда границу можно будет сдвинуть фактом.",
        )
    }
}

kotlin {
    jvmToolchain(21)
}

tasks.test {
    useJUnit()
}
