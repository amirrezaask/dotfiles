#!/usr/bin/env bash
PROJECTS_DIR="$HOME/dev"

selected=$(find "$PROJECTS_DIR" -maxdepth 3 -name ".git" -type d 2>/dev/null | sed 's|/.git$||' | sed "s|^$PROJECTS_DIR/||" | fzf --reverse --prompt "Project: ")
[ -z "$selected" ] && exit 0

session_name=$(basename "$selected" | tr . _)

if tmux has-session -t="$session_name" 2>/dev/null; then
  tmux switch-client -t="$session_name"
else
  tmux new-session -ds "$session_name" -n Code -c "$PROJECTS_DIR/$selected"
  tmux send-keys -t "$session_name:Code" "nvim" Enter
  tmux new-window -t "$session_name" -n Agent -c "$PROJECTS_DIR/$selected"
  tmux send-keys -t "$session_name:Agent" "opencode" Enter
  tmux new-window -t "$session_name" -n Shell -c "$PROJECTS_DIR/$selected"
  tmux select-window -t "$session_name:1"
  tmux switch-client -t="$session_name"
fi
