@echo off
REM =============================================================================
REM gen_all.bat — Пакетная генерация кода для всех .but файлов (Windows CMD).
REM
REM Использование:
REM   gen_all.bat <исходная_директория> <директория_назначения> [ОПЦИИ]
REM
REM Аргументы:
REM   %1   Директория с .but файлами (поиск рекурсивный).
REM   %2   Директория назначения для сгенерированного кода.
REM
REM Опции (передаются напрямую в but.exe):
REM   --indent-size N   Отступ в N пробелов.
REM   --indent-tab      Использовать символ табуляции.
REM   -I DIR            Добавить директорию поиска include-файлов.
REM
REM Примечание: для расширенного управления (список генераторов, dry-run и т.д.)
REM используйте gen_all.ps1 (PowerShell):
REM   powershell -ExecutionPolicy Bypass -File gen_all.ps1 <...>
REM
REM Примеры:
REM   gen_all.bat examples gen
REM   gen_all.bat src out --indent-size 2
REM   gen_all.bat src out --indent-tab
REM   gen_all.bat src out -I .\include -I .\lib\types
REM =============================================================================

setlocal EnableDelayedExpansion

REM ── Проверка аргументов ───────────────────────────────────────────────────────
if "%~1"=="" (
    echo [ОШИБКА] Не указана исходная директория.
    echo Использование: gen_all.bat ^<исходная^> ^<назначение^> [ОПЦИИ]
    exit /b 1
)
if "%~2"=="" (
    echo [ОШИБКА] Не указана директория назначения.
    echo Использование: gen_all.bat ^<исходная^> ^<назначение^> [ОПЦИИ]
    exit /b 1
)

set "SRC_DIR=%~f1"
set "DST_DIR=%~f2"

REM Дополнительные аргументы (всё после первых двух позиций)
set "EXTRA_ARGS="
:parse_extra
shift /3
if not "%~3"=="" (
    set "EXTRA_ARGS=!EXTRA_ARGS! %~3"
    goto parse_extra
)

REM ── Проверка исходной директории ─────────────────────────────────────────────
if not exist "%SRC_DIR%\" (
    echo [ОШИБКА] Исходная директория не найдена: %SRC_DIR%
    exit /b 1
)

REM ── Поиск исполняемого файла but ─────────────────────────────────────────────
set "BUT_EXE="

REM 1. В PATH
where but.exe >nul 2>&1
if %errorlevel% == 0 (
    set "BUT_EXE=but.exe"
    goto found_but
)

REM 2. Из директории скрипта — target\release
set "SCRIPT_DIR=%~dp0"
set "REPO_ROOT=%SCRIPT_DIR%.."

if exist "%REPO_ROOT%\target\release\but.exe" (
    set "BUT_EXE=%REPO_ROOT%\target\release\but.exe"
    goto found_but
)

REM 3. Отладочная сборка
if exist "%REPO_ROOT%\target\debug\but.exe" (
    echo [WARN] Используется отладочная сборка.
    set "BUT_EXE=%REPO_ROOT%\target\debug\but.exe"
    goto found_but
)

echo [ОШИБКА] Исполняемый файл 'but.exe' не найден.
echo Установите его через 'cargo install --path cli' или добавьте в PATH.
exit /b 1

:found_but

REM ── Вывод параметров ──────────────────────────────────────────────────────────
echo.
echo BuT -- пакетная генерация кода
echo   Источник    : %SRC_DIR%
echo   Назначение  : %DST_DIR%
echo   Исполняемый : %BUT_EXE%
if not "!EXTRA_ARGS!"=="" echo   Доп. опции  :!EXTRA_ARGS!
echo.

REM ── Создание директории назначения ───────────────────────────────────────────
if not exist "%DST_DIR%\" mkdir "%DST_DIR%"

REM ── Основной цикл ─────────────────────────────────────────────────────────────
set /a COUNT_TOTAL=0
set /a COUNT_OK=0
set /a COUNT_ERR=0
set "FAILED_LIST="

REM Рекурсивный поиск .but файлов
for /r "%SRC_DIR%" %%F in (*.but) do (
    set /a COUNT_TOTAL+=1

    REM Относительный путь от источника
    set "ABS_FILE=%%F"
    set "REL_PATH=!ABS_FILE:%SRC_DIR%\=!"

    REM Убираем расширение .but для создания выходной директории
    set "REL_BASE=!REL_PATH:.but=!"
    set "OUT_DIR=%DST_DIR%\!REL_BASE!"

    echo   [%%F]

    REM Создать выходную директорию
    if not exist "!OUT_DIR!\" mkdir "!OUT_DIR!"

    REM Запустить генерацию всех форматов
    "%BUT_EXE%" "%%F" --gen-c --gen-verilog --gen-st --gen-lc3 --gen-thumb --output-dir "!OUT_DIR!"!EXTRA_ARGS!

    if !errorlevel! == 0 (
        echo   [ОК] !REL_PATH!
        set /a COUNT_OK+=1
    ) else (
        echo   [ОШИБКА] !REL_PATH! 1>&2
        set /a COUNT_ERR+=1
        set "FAILED_LIST=!FAILED_LIST! !REL_PATH!"
    )
)

REM ── Итоговый отчёт ────────────────────────────────────────────────────────────
echo.
echo -- Результат ------------------------------------------
echo   Всего файлов : %COUNT_TOTAL%
echo   Успешно      : %COUNT_OK%

if %COUNT_ERR% gtr 0 (
    echo   С ошибками   : %COUNT_ERR%
    echo   Файлы с ошибками: %FAILED_LIST%
    exit /b 1
) else (
    echo   Все файлы обработаны успешно.
)

endlocal
