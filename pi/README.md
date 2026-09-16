# Pi extensions

## `image-attachments`

`image-attachments.ts` shows a compact thumbnail below the editor whenever the prompt contains a local PNG, JPEG, GIF, WebP, or AVIF path. It understands quoted paths, shell-escaped paths from terminal drag-and-drop, `@path` references, `file://` URLs, and Markdown image references. Deleting the path removes its preview.

The extension previews up to six images and skips thumbnail decoding for files larger than 20 MB. The path remains in the editor and is submitted normally. Terminal image rendering requires a supported terminal such as Ghostty, Kitty, iTerm2, WezTerm, Warp, or another terminal implementing a supported graphics protocol.

Run `./sync`, then use `/reload` in Pi.

## `codex-mux`

`codex-mux` exposes multiple independent OpenAI Codex OAuth account slots and an `openai-codex-mux` provider that picks the account with the most remaining cached usage. It tracks Codex limits with delayed background requests and shows each account on its own line below the editor. It does not replace the footer, scan session history while rendering, poll continuously, or spawn `codex app-server`.

Run `./sync`, then `/reload`. Log in with `/login openai-codex-account-1` and `/login openai-codex-account-2`, select an `openai-codex-mux` model, and use `/codex-accounts` for status and controls.

## `pierre-changes`

`pierre-changes` records net file changes at the end of each completed Pi turn and opens a local report with Pierre’s file tree and split diff renderer. Install its npm dependencies from `pi/pierre-changes`, then run `./sync`. Use `/changes` to reopen the latest report; set `PI_PIERRE_CHANGES_AUTO_OPEN=0` to disable automatic browser opening.
