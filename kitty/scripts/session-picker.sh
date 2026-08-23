#!/usr/bin/env bash
set -euo pipefail

# Ensure PATH includes common locations for tools
PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.local/bin:$PATH"

# Mode parameter: 'tab' or 'os-window' (default: os-window)
MODE="${1:-os-window}"

PROJECTS_DIR="${PROJECTS_DIR:-$HOME/dev}"

die() {
  echo "$1" >&2
  echo "Press Enter to close..." >&2
  read -r _ </dev/tty 2>/dev/null || true
  exit 1
}

command -v fzf >/dev/null 2>&1 || die "fzf is required"
command -v kitten >/dev/null 2>&1 || die "kitten is required"
command -v jq >/dev/null 2>&1 || die "jq is required"

if [[ "$MODE" != "tab" && "$MODE" != "os-window" ]]; then
  die "Invalid mode: $MODE (expected: tab or os-window)"
fi

# Get the project list.
projects=""
if [ -d "$PROJECTS_DIR" ]; then
  projects=$(
    while IFS= read -r git_path; do
      project_path="${git_path%/.git}"
      printf '%s\n' "${project_path#"$PROJECTS_DIR"/}"
    done < <(find "$PROJECTS_DIR" -maxdepth 3 -name ".git" 2>/dev/null)
  )
  projects=$(printf '%s\n' "$projects" | sort)
fi

set +e
selected=$(printf '%s\n' "$projects" | awk 'NF' | fzf)
fzf_status=$?
set -e

if [ "$fzf_status" -eq 130 ] || [ "$fzf_status" -eq 1 ]; then
  exit 0
fi

if [ "$fzf_status" -ne 0 ]; then
  die "fzf failed with exit code $fzf_status"
fi

[ -z "$selected" ] && exit 0

# Keep the full relative path so same-named projects do not collide.
session_name=$(printf '%s' "$selected" | tr '/.:' '_')
target_dir="$PROJECTS_DIR/$selected"

kitty_state=$(kitten @ ls) || die "Unable to query Kitty windows. Is remote control enabled?"

if [[ "$MODE" == "tab" ]]; then
  # Tab mode: look for existing tabs
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
else
  # OS Window mode: look for existing windows
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
fi

common_args=(
  --cwd "$target_dir"
  --var "kitty_session_name=$session_name"
  --env "KITTY_SESSION_NAME=$session_name"
)

if [[ "$MODE" == "tab" ]]; then
  # Create new tab
  window_id=$(
    kitten @ launch \
      --type tab \
      --tab-title "$session_name" \
      --title Neovim \
      --var "kitty_session_primary=$session_name" \
      "${common_args[@]}"
  )
else
  # Create new OS window
  window_id=$(
    kitten @ launch \
      --type os-window \
      --os-window-title "$session_name" \
      --tab-title "1.$session_name" \
      --title "$session_name" \
      --var "kitty_session_primary=$session_name" \
      "${common_args[@]}"
  )
fi

kitten @ focus-window --match "id:$window_id"
