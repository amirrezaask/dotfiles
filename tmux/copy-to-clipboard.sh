#!/usr/bin/env bash

set -euo pipefail

if command -v pbcopy >/dev/null 2>&1; then
	exec pbcopy
elif command -v wl-copy >/dev/null 2>&1; then
	exec wl-copy
elif command -v xclip >/dev/null 2>&1; then
	exec xclip -in -selection clipboard
elif command -v xsel >/dev/null 2>&1; then
	exec xsel --clipboard --input
fi

# Consume stdin so tmux's copy operation still completes predictably.
cat >/dev/null
printf 'No clipboard command found (tried pbcopy, wl-copy, xclip, and xsel).\n' >&2
exit 127
