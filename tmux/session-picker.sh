#!/usr/bin/env bash

set -euo pipefail

PROJECTS_DIR="${PROJECTS_DIR:-$HOME/dev}"
SCRATCH_DIR="${SCRATCH_DIR:-$HOME/scratch}"

command -v fzf >/dev/null 2>&1 || {
	printf 'fzf is required\n' >&2
	exit 1
}
command -v tmux >/dev/null 2>&1 || {
	printf 'tmux is required\n' >&2
	exit 1
}

projects=""
if [[ -d "$PROJECTS_DIR" ]]; then
	projects=$(
		while IFS= read -r git_path; do
			project_path="${git_path%/.git}"
			printf '%s\n' "${project_path#"$PROJECTS_DIR"/}"
		done < <(find "$PROJECTS_DIR" -maxdepth 3 -name .git 2>/dev/null)
	)
	projects=$(printf '%s\n' "$projects" | sort)
fi

set +e
selected=$(printf 'scratch\n%s\n' "$projects" | awk 'NF' | fzf --prompt 'Session: ' --layout reverse)
fzf_status=$?
set -e

if [[ "$fzf_status" -eq 1 || "$fzf_status" -eq 130 || -z "$selected" ]]; then
	exit 0
elif [[ "$fzf_status" -ne 0 ]]; then
	printf 'fzf failed with exit code %s\n' "$fzf_status" >&2
	exit "$fzf_status"
fi

if [[ "$selected" == scratch ]]; then
	session_name="scratch"
	target_dir="$SCRATCH_DIR"
	mkdir -p "$target_dir"
else
	# The full relative path avoids collisions such as work/api and personal/api.
	session_name=$(printf '%s' "$selected" | tr '/.:' '_')
	target_dir="$PROJECTS_DIR/$selected"
fi

if tmux has-session -t="$session_name" 2>/dev/null; then
	tmux switch-client -t="$session_name"
else
	tmux new-session -ds "$session_name" -c "$target_dir"
	tmux switch-client -t="$session_name"
fi
