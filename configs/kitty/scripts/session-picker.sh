#!/usr/bin/env zsh
set -euo pipefail

PROJECTS_DIR="${PROJECTS_DIR:-$HOME/dev}"
SCRATCH_DIR="${SCRATCH_DIR:-$HOME/scratch}"
scratch_option="scratch"

die() {
  print -u2 "$1"
  print -u2 "Press Enter to close..."
  read -r _ </dev/tty 2>/dev/null || true
  exit 1
}

command -v fzf >/dev/null 2>&1 || die "fzf is required"
command -v kitten >/dev/null 2>&1 || die "kitten is required"
command -v jq >/dev/null 2>&1 || die "jq is required"

projects=$(
  find "$PROJECTS_DIR" -maxdepth 3 -name ".git" -type d 2>/dev/null \
    | sed 's|/.git$||' \
    | sed "s|^$PROJECTS_DIR/||" \
    | sort
)

set +e
selected=$(printf "%s\n%s\n" "$scratch_option" "$projects" | awk 'NF' | fzf --prompt "Session: ")
fzf_status=$?
set -e

if [ "$fzf_status" -eq 130 ] || [ "$fzf_status" -eq 1 ]; then
  exit 0
fi

if [ "$fzf_status" -ne 0 ]; then
  die "fzf failed with exit code $fzf_status"
fi

[ -z "$selected" ] && exit 0

if [ "$selected" = "$scratch_option" ]; then
  session_name="scratch"
  target_dir="$SCRATCH_DIR"
  mkdir -p "$target_dir"
else
  session_name=$(basename "$selected" | tr . _)
  target_dir="$PROJECTS_DIR/$selected"
fi

match_session="var:kitty_session_name=$session_name"
kitty_state=$(kitten @ ls) || die "Unable to query Kitty windows. Is remote control enabled?"

existing_window_id=$(printf "%s" "$kitty_state" | jq -r --arg session "$session_name" '
  [
    .[].tabs[].windows[]?
    | select(.user_vars.kitty_session_primary == $session)
    | .id
  ][0] // empty
')

if [ -z "$existing_window_id" ]; then
  existing_window_id=$(printf "%s" "$kitty_state" | jq -r --arg session "$session_name" '
    [
      .[].tabs[].windows[]?
      | select(.user_vars.kitty_session_name == $session)
      | .id
    ][0] // empty
  ')
fi

if [ -n "$existing_window_id" ]; then
  kitten @ focus-window --match "id:$existing_window_id"
  exit 0
fi

common_args=(
  --cwd "$target_dir"
  --var "kitty_session_name=$session_name"
  --env "KITTY_SESSION_NAME=$session_name"
)

login_shell_cmd() {
  local command="$1"
  printf 'cd %q && exec %s' "$target_dir" "$command"
}

window_id=$(
  kitten @ launch \
    --type os-window \
    --os-window-title "$session_name" \
    --tab-title "$session_name: Neovim" \
    --title Neovim \
    --var "kitty_session_primary=$session_name" \
    "${common_args[@]}" \
    zsh -lic "$(login_shell_cmd nvim)"
)

kitten @ launch \
  --type tab \
  --match "window_id:$window_id" \
  --tab-title Zsh \
  --title Zsh \
  "${common_args[@]}" \
  zsh -lic "$(login_shell_cmd zsh)" >/dev/null

kitten @ launch \
  --type tab \
  --match "window_id:$window_id" \
  --tab-title Agent \
  --title Agent \
  "${common_args[@]}" \
  zsh -lic "$(login_shell_cmd opencode)" >/dev/null

kitten @ launch \
  --type tab \
  --match "window_id:$window_id" \
  --tab-title Diff \
  --title Diff \
  "${common_args[@]}" \
  zsh -lic "$(login_shell_cmd 'nvim +DiffviewOpen')" >/dev/null

kitten @ set-tab-title --match "$match_session" "$session_name"
kitten @ focus-window --match "id:$window_id"
