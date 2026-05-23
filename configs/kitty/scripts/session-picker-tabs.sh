#!/usr/bin/env zsh
set -euo pipefail

PROJECTS_DIR="${PROJECTS_DIR:-$HOME/dev}"
SCRATCH_DIR="${SCRATCH_DIR:-$HOME/scratch}"
scratch_option="scratch"

fzf_options=(
  --prompt "Project: "
  --layout reverse
  --border rounded
)

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
selected=$(printf "%s\n%s\n" "$scratch_option" "$projects" | awk 'NF' | fzf "${fzf_options[@]}")
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

existing_tab_id=$(printf "%s" "$kitty_state" | jq -r --arg session "$session_name" '
  [
    .[].tabs[]? as $tab
    | select(any($tab.windows[]?; .user_vars.kitty_session_primary == $session))
    | $tab.id
  ][0] // empty
')

if [ -z "$existing_tab_id" ]; then
  existing_tab_id=$(printf "%s" "$kitty_state" | jq -r --arg session "$session_name" '
    [
      .[].tabs[]? as $tab
      | select(any($tab.windows[]?; .user_vars.kitty_session_name == $session))
      | $tab.id
    ][0] // empty
  ')
fi

if [ -n "$existing_tab_id" ]; then
  kitten @ focus-tab --match "id:$existing_tab_id"
  exit 0
fi

common_args=(
  --cwd "$target_dir"
  --var "kitty_session_name=$session_name"
  --env "KITTY_SESSION_NAME=$session_name"
)

window_id=$(
  kitten @ launch \
    --type tab \
    --tab-title "$session_name" \
    --title Neovim \
    --var "kitty_session_primary=$session_name" \
    "${common_args[@]}"
)

kitten @ set-tab-title --match "$match_session" "$session_name"
kitten @ focus-window --match "id:$window_id"
