// Подпроект оснастки IDE для языка Takt. Изолирован от Rust-workspace:
// собственная Gradle-сборка, не входит в cargo/CI Rust.

// Авто-провижининг JDK по toolchain (пересмотр ADR: платформа
// поднята до 2024.2 -> нужен JDK 21). Резолвер foojay скачивает недостающий JDK.
plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}

rootProject.name = "intellij-takt"
