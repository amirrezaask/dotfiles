#!/usr/bin/env bash
set -euo pipefail

PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.local/bin:$PATH"

MODE="${1:-window}" # window|tab

PROJECTS_DIR="${PROJECTS_DIR:-$HOME/dev}"
SCRATCH_DIR="${SCRATCH_DIR:-$HOME/scratch}"
scratch_option="scratch"

die() {
  echo "$1" >&2
  exit 1
}

command -v fzf >/dev/null 2>&1 || die "fzf is required"
command -v osascript >/dev/null 2>&1 || die "osascript is required"

projects=$(
  find "$PROJECTS_DIR" -maxdepth 3 -name ".git" -type d 2>/dev/null \
    | sed 's|/.git$||' \
    | sed "s|^$PROJECTS_DIR/||" \
    | sort
)

set +e
selected=$(printf "%s\n%s\n" "$scratch_option" "$projects" | awk 'NF' | fzf --prompt "Ghostty Session: " --layout reverse)
fzf_status=$?
set -e

if [ "$fzf_status" -eq 130 ] || [ "$fzf_status" -eq 1 ]; then
  exit 0
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

if [ ! -d "$target_dir" ]; then
  die "Target directory does not exist: $target_dir"
fi

if [[ "$MODE" != "window" && "$MODE" != "tab" ]]; then
  die "Invalid mode: $MODE (expected: window or tab)"
fi

# Try to focus an existing terminal whose working directory is inside target_dir.
# This mirrors session reuse behavior from tmux/kitty, without requiring extra daemons.
focus_output=$(
  osascript - "$target_dir" <<'APPLESCRIPT' 2>/dev/null
on run argv
  set targetDir to item 1 of argv
  set foundId to ""

  tell application "Ghostty"
    repeat with t in terminals
      try
        set wd to working directory of t
        if wd starts with targetDir then
          focus t
          set foundId to (id of t) as text
          exit repeat
        end if
      end try
    end repeat
  end tell

  return foundId
end run
APPLESCRIPT
) || true

if [ -n "$focus_output" ]; then
  exit 0
fi

if [ "$MODE" = "tab" ]; then
  osascript - "$target_dir" "$session_name" <<'APPLESCRIPT'
on run argv
  set targetDir to item 1 of argv
  set sessionName to item 2 of argv

  tell application "Ghostty"
    activate
    set cfg to new surface configuration
    set initial working directory of cfg to targetDir

    if (count windows) = 0 then
      set win to new window with configuration cfg
      set t1 to selected tab of win
      try
        set name of t1 to sessionName
      end try
      focus (focused terminal of t1)
    else
      set win to front window
      set t1 to new tab in win with configuration cfg
      try
        set name of t1 to sessionName
      end try
      select tab t1
      focus (focused terminal of t1)
    end if
  end tell
end run
APPLESCRIPT
else
  osascript - "$target_dir" "$session_name" <<'APPLESCRIPT'
on run argv
  set targetDir to item 1 of argv
  set sessionName to item 2 of argv

  tell application "Ghostty"
    activate
    set cfg to new surface configuration
    set initial working directory of cfg to targetDir

    set win to new window with configuration cfg
    set tab1 to selected tab of win
    set tab2 to new tab in win with configuration cfg
    set tab3 to new tab in win with configuration cfg

    try
      set name of tab1 to "1." & sessionName
      set name of tab2 to "2." & sessionName
      set name of tab3 to "3." & sessionName
    end try

    select tab tab1
    focus (focused terminal of tab1)
  end tell
end run
APPLESCRIPT
fi
