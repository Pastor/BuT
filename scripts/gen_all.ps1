# =============================================================================
# gen_all.ps1 — Пакетная генерация кода для всех .but файлов в директории.
#
# Использование:
#   .\gen_all.ps1 <исходная_директория> <директория_назначения> [ОПЦИИ]
#
# Аргументы:
#   SourceDir       Директория с .but файлами (поиск рекурсивный).
#   DestDir         Директория, куда будет помещён сгенерированный код.
#
# Параметры:
#   -IndentSize N   Отступ в N пробелов (по умолчанию: 4).
#   -IndentTab      Использовать символ табуляции вместо пробелов.
#   -Generators     Список генераторов через запятую.
#                   Допустимые значения: c, verilog, st, lc3, thumb, all
#                   По умолчанию: all (все генераторы).
#   -IncludeDir     Массив директорий для поиска include-файлов.
#                   Пример: -IncludeDir @(".\lib", ".\shared\types")
#   -ButExe         Путь к исполняемому файлу but.exe
#                   (по умолчанию: ищется в PATH, затем в target\release).
#   -DryRun         Вывести команды без их выполнения.
#
# Структура выходной директории:
#   <DestDir>\
#     <относительный_путь_файла>\   # Совпадает со структурой источника
#       c\                          # C99 заголовок + исходник
#       verilog\                    # Verilog RTL-модуль
#       st\                         # Structured Text (МЭК 61131-3)
#       lc3\                        # Ассемблер LC-3
#       thumb\                      # Ассемблер ARM Thumb
#
# Примеры:
#   .\gen_all.ps1 .\examples .\gen
#   .\gen_all.ps1 .\src .\out -IndentSize 2
#   .\gen_all.ps1 .\src .\out -IndentTab -Generators c,verilog
#   .\gen_all.ps1 .\src .\out -IncludeDir @(".\include", ".\lib\types")
#   .\gen_all.ps1 .\src .\out -ButExe .\target\release\but.exe
# =============================================================================

param(
    [Parameter(Mandatory = $true, Position = 0)]
    [string]$SourceDir,

    [Parameter(Mandatory = $true, Position = 1)]
    [string]$DestDir,

    [Parameter()]
    [ValidateRange(0, 32)]
    [int]$IndentSize = 4,

    [Parameter()]
    [switch]$IndentTab,

    [Parameter()]
    [string]$Generators = "all",

    [Parameter()]
    [string[]]$IncludeDir = @(),

    [Parameter()]
    [string]$ButExe = "",

    [Parameter()]
    [switch]$DryRun
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# ── Вспомогательные функции ───────────────────────────────────────────────────

function Write-Info  ($msg) { Write-Host "[INFO]    $msg" -ForegroundColor Cyan }
function Write-Ok    ($msg) { Write-Host "[ОК]      $msg" -ForegroundColor Green }
function Write-Warn  ($msg) { Write-Host "[WARN]    $msg" -ForegroundColor Yellow }
function Write-Err   ($msg) { Write-Host "[ОШИБКА]  $msg" -ForegroundColor Red }
function Write-Fatal ($msg) { Write-Err $msg; exit 1 }

# ── Проверка исходной директории ──────────────────────────────────────────────
if (-not (Test-Path -LiteralPath $SourceDir -PathType Container)) {
    Write-Fatal "Исходная директория не найдена: $SourceDir"
}
$SourceDir = (Resolve-Path $SourceDir).Path

# ── Поиск исполняемого файла but ──────────────────────────────────────────────
function Find-ButExe {
    param([string]$Hint)

    # 1. Явно указанный путь
    if ($Hint -ne "") {
        if (Test-Path -LiteralPath $Hint) { return $Hint }
        Write-Fatal "Исполняемый файл не найден: $Hint"
    }
    # 2. В PATH
    $inPath = Get-Command "but" -ErrorAction SilentlyContinue
    if ($inPath) { return $inPath.Source }

    # 3. Из корня репозитория (рядом со скриптом)
    $scriptRoot = Split-Path -Parent $PSCommandPath
    $repoRoot   = Split-Path -Parent $scriptRoot

    $releaseBin = Join-Path $repoRoot "target\release\but.exe"
    if (Test-Path $releaseBin) { return $releaseBin }

    $debugBin = Join-Path $repoRoot "target\debug\but.exe"
    if (Test-Path $debugBin) {
        Write-Warn "Используется отладочная сборка: $debugBin"
        return $debugBin
    }

    Write-Fatal "Исполняемый файл 'but.exe' не найден.`nУстановите его через 'cargo install --path cli' или укажите путь через -ButExe."
}

$ButExe = Find-ButExe -Hint $ButExe

# ── Формирование флагов генераторов ──────────────────────────────────────────
function Build-GenFlags {
    param([string]$GenList)

    if ($GenList -eq "all") {
        return @("--gen-c", "--gen-verilog", "--gen-st", "--gen-lc3", "--gen-thumb")
    }

    $flags = @()
    foreach ($g in ($GenList -split ",")) {
        $g = $g.Trim()
        switch ($g) {
            "c"       { $flags += "--gen-c" }
            "verilog" { $flags += "--gen-verilog" }
            "st"      { $flags += "--gen-st" }
            "lc3"     { $flags += "--gen-lc3" }
            "thumb"   { $flags += "--gen-thumb" }
            default   { Write-Warn "Неизвестный генератор: '$g' (допустимые: c,verilog,st,lc3,thumb,all)" }
        }
    }

    if ($flags.Count -eq 0) {
        Write-Fatal "Не указан ни один допустимый генератор"
    }
    return $flags
}

$GenFlags = Build-GenFlags -GenList $Generators

# Аргументы отступа
$IndentArgs = @()
if ($IndentTab) {
    $IndentArgs = @("--indent-tab")
} elseif ($IndentSize -ne 4) {
    # Передаём явно только при отклонении от значения по умолчанию
    $IndentArgs = @("--indent-size", "$IndentSize")
}

# ── Заголовок ─────────────────────────────────────────────────────────────────
Write-Host ""
Write-Host "BuT — пакетная генерация кода" -ForegroundColor White -BackgroundColor DarkBlue
Write-Host "  Источник    : $SourceDir"
Write-Host "  Назначение  : $DestDir"
Write-Host "  Генераторы  : $Generators"
$indentDesc = if ($IndentTab) { "табуляция" } elseif ($IndentArgs.Count -gt 0) { "$IndentSize пробела(ов)" } else { "по умолчанию (4 пробела)" }
Write-Host "  Отступ      : $indentDesc"
if ($IncludeDir.Count -gt 0) {
    Write-Host "  Include-пути: $($IncludeDir -join ', ')"
}
Write-Host "  Исполняемый : $ButExe"
if ($DryRun) { Write-Host "  [dry-run] команды не выполняются" -ForegroundColor Yellow }
Write-Host ""

# ── Основной цикл ─────────────────────────────────────────────────────────────
$CountTotal = 0
$CountOk    = 0
$CountErr   = 0
$FailedFiles = [System.Collections.Generic.List[string]]::new()

# Найти все .but файлы рекурсивно
$butFiles = Get-ChildItem -LiteralPath $SourceDir -Recurse -Filter "*.but" |
    Where-Object { -not $_.PSIsContainer } |
    Sort-Object FullName

foreach ($file in $butFiles) {
    $CountTotal++

    # Относительный путь без расширения
    $relPath    = $file.FullName.Substring($SourceDir.Length).TrimStart('\', '/')
    $relBase    = [System.IO.Path]::ChangeExtension($relPath, $null).TrimEnd('.')
    $outDir     = Join-Path $DestDir $relBase

    Write-Host "  ▶ $relPath" -ForegroundColor Cyan

    # Собираем аргументы команды
    # Добавляем флаги путей поиска include-файлов
    $includeArgs = @()
    foreach ($dir in $IncludeDir) {
        $includeArgs += @("-I", $dir)
    }
    $cmdArgs = @($file.FullName) + $GenFlags + @("--output-dir", $outDir) + $IndentArgs + $includeArgs

    if ($DryRun) {
        Write-Host "    [dry-run] $ButExe $($cmdArgs -join ' ')" -ForegroundColor Yellow
        $CountOk++
        continue
    }

    # Создать выходную директорию
    New-Item -ItemType Directory -Path $outDir -Force | Out-Null

    # Выполнить генерацию
    try {
        $output = & $ButExe @cmdArgs 2>&1
        $exitCode = $LASTEXITCODE
        if ($exitCode -eq 0) {
            Write-Ok "$relPath → $outDir"
            $CountOk++
        } else {
            Write-Err "Ошибка (код $exitCode) при обработке: $relPath"
            $output | ForEach-Object { Write-Host "    $_" -ForegroundColor Red }
            $FailedFiles.Add($relPath)
            $CountErr++
        }
    } catch {
        Write-Err "Исключение при обработке '$relPath': $_"
        $FailedFiles.Add($relPath)
        $CountErr++
    }
}

# ── Итоговый отчёт ────────────────────────────────────────────────────────────
Write-Host ""
Write-Host "── Результат ───────────────────────────────────────" -ForegroundColor White
Write-Host "  Всего файлов : $CountTotal"
Write-Host "  Успешно      : $CountOk" -ForegroundColor Green
if ($CountErr -gt 0) {
    Write-Host "  С ошибками   : $CountErr" -ForegroundColor Red
    Write-Host "  Файлы с ошибками:" -ForegroundColor Red
    foreach ($f in $FailedFiles) {
        Write-Host "    ✗ $f" -ForegroundColor Red
    }
    exit 1
} else {
    Write-Host "  Все файлы обработаны успешно." -ForegroundColor Green
}
