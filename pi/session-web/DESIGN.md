# Pi Session Web — Design Record

## Design Intent

A live coding ledger rather than a consumer chat app. Conversation and agent activity share one calm timeline, with context available only when requested.

## Visual World

- Paper-white or charcoal work surfaces, selected by `prefers-color-scheme`.
- Hairline dividers and small tonal steps instead of stacked cards.
- Compact square controls with consistent 36 px targets.
- Restrained green used for live state, progress, links, and focus—not decoration.
- Locally bundled Geist Variable for interface and message copy; Geist Mono Variable for paths, code, commands, and token counts.
- A single centered transcript column capped at 760 px for readable Markdown.

## Screen Anatomy

1. A 48 px fixed title bar holds product identity, session title, connection state, sharing, and context access. Explicit Pi session names take precedence; otherwise the first user prompt becomes a compact title instead of leaving the interface at “New session.”
2. The centered timeline holds user/assistant messages and grouped tool activity.
3. The composer remains anchored at the bottom, with image attachment, active model, send state, and keyboard guidance.
4. The optional right inspector reports context-window usage, system prompt, and active context entries.

A persistent left sidebar was considered from the reference layout and deliberately removed after review. It repeated project/model/usage information and reduced transcript space without adding a necessary navigation task.

## Markdown

Message text uses semantic Markdown with GFM support:

- headings retain clear scale and hierarchy;
- bold and emphasis remain visible in both color schemes;
- lists, blockquotes, tables, links, rules, and inline code have dedicated treatment;
- fenced code loads a narrowed Shiki highlighter on demand, with light and dark GitHub themes.

## Tool Activity

Tool traffic is grouped between conversational turns. The collapsed row states the number of calls and completion status. Each expanded call translates machine arguments into a task-shaped summary:

- `read`, `write`, and `edit` emphasize the file;
- `bash` emphasizes the first command and renders shell input;
- code search and file search emphasize query and scope;
- web tools emphasize query or URL;
- unknown tools receive humanized names and key/value fields.

Full output stays available in a nested disclosure with syntax-oriented formatting. Errors and running calls have distinct semantic statuses.

## Responsive Behavior

The transcript and composer use the full canvas at tablet and mobile widths. At 390 px:

- the title truncates without moving actions off-screen;
- no horizontal page overflow is allowed;
- tool summaries collapse status text to icons;
- the context inspector becomes a near-full-width overlay;
- the composer stays pinned to the viewport bottom.

## Interaction and Accessibility

- Every icon-only button has an accessible name and native tooltip.
- Native `details`/`summary` controls provide keyboard-accessible tool disclosure.
- Context-panel close returns focus to its trigger and Escape closes the panel.
- Focus rings use a high-contrast semantic token.
- Motion is brief and functional, with `prefers-reduced-motion` disabling transitions and animated scrolling.
- Errors use text and icons in addition to color.

## Assets and Provenance

No shipping raster imagery or generated decorative artwork was introduced. User attachments are runtime session data and are not repository assets.

The visual direction is pinned to the user-supplied T3 Code screenshot:

`/var/folders/lx/409xqb9n18sf6hd_y81h33r00000gn/T/TemporaryItems/NSIRD_screencaptureui_HHyN4T/Screenshot 2026-09-22 at 18.24.51.png`

The implementation borrows its compact harness character, neutral surfaces, restrained controls, and optional context panel while retaining Pi-specific identity and behavior.

## Verification Record

Verified on 2026-09-22 with a representative intercepted session fixture in headless Google Chrome:

- 1440 × 960 in system light mode;
- 1440 × 960 in system dark mode;
- 390 × 844 mobile viewport in both modes;
- placeholder `New Session` names replaced by a compact title derived from the first user prompt;
- bundled Geist Sans and Geist Mono reported loaded through the browser FontFaceSet API;
- Markdown heading and bold output present;
- fenced code highlighted after lazy module load;
- grouped tool disclosure and nested call details opened successfully;
- context inspector opened and closed successfully;
- prompt submit cleared the composer after a successful response;
- body width matched viewport width on desktop and mobile;
- no page exceptions were reported.

The Vite-only fixture run expectedly logged a failed root WebSocket connection because it did not run the extension server; it produced no page exception and did not affect the exercised interface. The real extension supplies the WebSocket endpoint.

Screenshots from that run were written outside the repository at `/tmp/session-web-light.png`, `/tmp/session-web-dark.png`, and `/tmp/session-web-dark-mobile.png`.
