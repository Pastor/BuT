#!/bin/sh
# Тело релиза: что вошло с предыдущей помеченной версии.
#
set -eu

VERSION="${1:-}"
REV="${2:-HEAD}"
[ -n "$VERSION" ] || { echo "Использование: $0 ВЕРСИЯ [РЕВИЗИЯ]" >&2; exit 2; }

# Предыдущий тег версии: самый старший `vX.Y.Z`, строго меньший текущего и
# достижимый из ревизии. `sort -V`, а не лексикографическая сортировка:
# иначе `v0.9.0` окажется старше `v0.10.0`.
PREV=""
for tag in $(git tag -l 'v*' | sort -V); do
    [ "$tag" = "v$VERSION" ] && continue
    git merge-base --is-ancestor "$tag^{commit}" "$REV" 2>/dev/null || continue
    NEWEST="$(printf '%s\n%s\n' "${tag#v}" "$VERSION" | sort -V | tail -n1)"
    [ "$NEWEST" = "$VERSION" ] || continue
    PREV="$tag"
done

printf '## Версия языка %s\n\n' "$VERSION"

if [ -n "$PREV" ]; then
    printf 'Изменения с %s:\n\n' "$PREV"
    RANGE="$PREV..$REV"
else
    # Первый релиз: диапазона нет, берём всю историю до ревизии.
    printf 'Изменения:\n\n'
    RANGE="$REV"
fi

# Заголовки коммитов. Они несут номер фичи, поэтому список
# читается как перечень сделанного, а не как выгрузка git.
git log --no-merges --format='- %s' "$RANGE"

printf '\nПолный журнал изменений — [CHANGES.md](CHANGES.md).\n'
