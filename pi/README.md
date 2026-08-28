# Pi extensions

## `consent`

`consent.ts` asks for confirmation before sending a user prompt to an expensive model. It recognizes Opus and Sol models by name and also gates models whose published input/output/cache rate reaches the configured threshold.

Run `./sync` to link it into `~/.pi/agent/extensions/`, then reload Pi with `/reload`.

Environment overrides:

- `PI_CONSENT_COST_THRESHOLD` — maximum published rate in USD per million tokens before consent is required. Defaults to `10`.
- `PI_CONSENT_MODEL_PATTERNS` — comma-separated model-name patterns. Defaults to `opus,sol`.

In non-interactive modes, the extension blocks expensive prompts because no user can provide consent.

## `image-attachments`

`image-attachments.ts` shows a compact thumbnail below the editor whenever the prompt contains a local PNG, JPEG, GIF, WebP, or AVIF path. It understands quoted paths, shell-escaped paths from terminal drag-and-drop, `@path` references, `file://` URLs, and Markdown image references. Deleting the path removes its preview.

The extension previews up to six images and skips thumbnail decoding for files larger than 20 MB. The path remains in the editor and is submitted normally. Terminal image rendering requires a supported terminal such as Ghostty, Kitty, iTerm2, WezTerm, Warp, or another terminal implementing a supported graphics protocol.

Run `./sync`, then use `/reload` in Pi.

## `pierre-changes`

`pierre-changes` records net file changes at the end of each completed Pi turn and opens a local report with Pierre’s file tree and split diff renderer. Install its npm dependencies from `pi/pierre-changes`, then run `./sync`. Use `/changes` to reopen the latest report; set `PI_PIERRE_CHANGES_AUTO_OPEN=0` to disable automatic browser opening.
