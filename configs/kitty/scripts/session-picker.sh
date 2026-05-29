#!/usr/bin/env bash
set -euo pipefail

# Ensure PATH includes common locations for tools
PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.local/bin:$PATH"

# Mode parameter: 'tab' or 'os-window' (default: os-window)
MODE="${1:-os-window}"

PROJECTS_DIR="${PROJECTS_DIR:-$HOME/dev}"
SCRATCH_DIR="${SCRATCH_DIR:-$HOME/scratch}"
scratch_option=" scratch"

# Set icon and title based on mode
if [[ "$MODE" == "tab" ]]; then
  icon="󰚢"
  title="Tab"
else
  icon="󰣇"
  title="OS Window"
fi

# Get colors from Kitty's current colorscheme
get_kitty_color() {
  kitten @ get-colors 2>/dev/null | grep "^$1 " | awk '{print $2}' | tr -d '\r'
}

# Fetch Kitty colors dynamically
bg=$(get_kitty_color background)
bg=${bg:-#1e1e2e}

fg=$(get_kitty_color foreground)
fg=${fg:-#cdd6f4}

cursor=$(get_kitty_color cursor)
cursor=${cursor:-#f5c2e7}

color0=$(get_kitty_color color0)
color0=${color0:-#313244}

color4=$(get_kitty_color color4)
color4=${color4:-#89b4fa}

color5=$(get_kitty_color color5)
color5=${color5:-#cba6f7}

color6=$(get_kitty_color color6)
color6=${color6:-#b4befe}

color7=$(get_kitty_color color7)
color7=${color7:-#a6adc8}

color2=$(get_kitty_color color2)
color2=${color2:-#a6e3a1}

color1=$(get_kitty_color color1)
color1=${color1:-#f38ba8}

fzf_options=(
  # --prompt "  "
  # --height "50%"
  # --min-height 40
  --layout reverse
  # --border none
  # --margin "25%,15%"
  # --padding "2"
  # --info inline-right
  # --pointer "▶"
  # --marker "✓"
  --color "bg:${bg},bg+:${color0},fg:${fg},fg+:${fg}"
  --color "hl:${color4},hl+:${color5},info:${color7}"
  --color "prompt:${color4},pointer:${color5},marker:${color2}"
  --color "border:${color0},gutter:${bg}"
  --color "header:${color6},spinner:${cursor}"
)

die() {
  echo "$1" >&2
  echo "Press Enter to close..." >&2
  read -r _ </dev/tty 2>/dev/null || true
  exit 1
}

command -v fzf >/dev/null 2>&1 || die "fzf is required"
command -v kitten >/dev/null 2>&1 || die "kitten is required"
command -v jq >/dev/null 2>&1 || die "jq is required"

# Get project list with icons
projects=$(
  find "$PROJECTS_DIR" -maxdepth 3 -name ".git" -type d 2>/dev/null \
    | sed 's|/.git$||' \
    | sed "s|^$PROJECTS_DIR/||" \
    | sort \
    | sed "s/^/${icon} /"
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

# Strip icon prefix for processing
selected_clean="${selected#* }"

if [ "$selected" = "$scratch_option" ]; then
  session_name="scratch"
  target_dir="$SCRATCH_DIR"
  mkdir -p "$target_dir"
else
  session_name=$(basename "$selected_clean" | tr . _)
  target_dir="$PROJECTS_DIR/$selected_clean"
fi

match_session="var:kitty_session_name=$session_name"
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
      --tab-title "vim" \
      --title "$session_name" \
      --var "kitty_session_primary=$session_name" \
      "${common_args[@]}"
  )
  
  kitten @ launch \
    --type tab \
    --tab-title "shell" \
    --title "$session_name" \
    --var "kitty_session_primary=$session_name" \
    "${common_args[@]}"

  kitten @ launch \
    --type tab \
    --tab-title "agent" \
    --title "$session_name" \
    --var "kitty_session_primary=$session_name" \
    "${common_args[@]}"
fi

kitten @ focus-window --match "id:$window_id"
