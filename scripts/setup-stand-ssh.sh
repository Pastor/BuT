#!/usr/bin/env bash
#
# Доступ к стенду по ключу: сгенерировать ключ, прописать хост в
# `~/.ssh/config` и положить открытую часть на сервер.
#
# Пароль вводит человек - скрипт его не спрашивает, не читает и никуда не
# сохраняет: `ssh-copy-id` спросит сам, напрямую с терминала.
#
# Идемпотентен: существующий ключ не перезаписывается, запись в конфиге не
# дублируется, повторный запуск лишь досылает ключ.
#
# Использование:
#   scripts/setup-stand-ssh.sh [пользователь@]хост [короткое-имя]
#
# Примеры:
#   scripts/setup-stand-ssh.sh pastor@pastor.axatel.ru takt-stand
#   scripts/setup-stand-ssh.sh pastor.axatel.ru
set -euo pipefail

target="${1:-}"
if [[ -z "$target" ]]; then
  echo "укажите хост стенда: $0 [пользователь@]хост [короткое-имя]" >&2
  exit 1
fi

if [[ "$target" == *@* ]]; then
  user="${target%%@*}"
  host="${target#*@}"
else
  user="$(id -un)"
  host="$target"
fi
alias_name="${2:-takt-stand}"

KEY="$HOME/.ssh/id_ed25519_takt"
CONFIG="$HOME/.ssh/config"

mkdir -p "$HOME/.ssh"
chmod 700 "$HOME/.ssh"

if [[ -f "$KEY" ]]; then
  echo "ключ уже есть: $KEY (не трогаем)"
else
  # Свой ключ, а не общий `id_rsa`: ключ стенда должно быть можно отозвать,
  # не отзывая всё остальное.
  ssh-keygen -t ed25519 -f "$KEY" -N '' -C "takt-stand $(id -un)@$(hostname)"
  echo "ключ создан: $KEY"
fi

if grep -qE "^Host[[:space:]]+$alias_name\$" "$CONFIG" 2>/dev/null; then
  echo "запись 'Host $alias_name' в $CONFIG уже есть (не дублируем)"
else
  {
    echo ""
    echo "# Стенд Takt (scripts/setup-stand-ssh.sh, фича 0531)"
    echo "Host $alias_name"
    echo "    HostName $host"
    echo "    User $user"
    echo "    IdentityFile $KEY"
    echo "    IdentitiesOnly yes"
  } >> "$CONFIG"
  chmod 600 "$CONFIG"
  echo "запись 'Host $alias_name' добавлена в $CONFIG"
fi

echo
echo "Сейчас ssh-copy-id спросит пароль — вводите его прямо ему."
ssh-copy-id -i "$KEY.pub" "$user@$host"

echo
echo "Проверка:"
ssh -o BatchMode=yes "$alias_name" 'echo "  вход по ключу работает: $(hostname)"'
echo "Готово. Выкатка: scripts/deploy-stand.sh $alias_name"
