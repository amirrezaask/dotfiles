# Product

<!-- impeccable:product-schema 1 -->

## Platform

web

## Users

Pi coding-agent users who want to watch and steer the current local session from a browser while work is in progress.

## Product Purpose

Session Web is a private browser companion for the active Pi session. It mirrors the conversation and tool activity live, accepts text and image prompts, and exposes the context Pi can currently see.

## Positioning

It is not a separate chat client or session archive: it is a live, local view onto the exact Pi runtime already running in the terminal.

## Operating Context

The interface runs beside a terminal during coding work. Users scan long Markdown responses, inspect file and command activity, attach screenshots, submit steering prompts, and occasionally inspect context-window contents and usage.

## Capabilities and Constraints

- Serves from the extension on a system-assigned port and binds to `127.0.0.1` by default.
- Streams session updates over WebSocket and falls back to snapshot requests.
- Sends prompts and image attachments into the current Pi session.
- Shows only the current session; it does not manage a session library.
- Tool calls must be summarized as readable activity, not exposed as raw JSON.
- Message content must render as Markdown, including headings, emphasis, lists, tables, links, and fenced code.
- The visual theme follows the operating system's light or dark preference.
- If Pi has no explicit session name, the first user prompt supplies a compact display and browser title.
- Interface and code typography use locally bundled Geist variable fonts for consistent rendering across platforms.

## Brand Commitments

The product name is **Pi Session Web**. The interface should share the compact, restrained coding-workspace character of the supplied T3 Code reference and DeepSeek-style harnesses without copying either brand literally. Preserve Pi's plain, direct language and π mark. Keep the transcript full-width rather than reserving space for a persistent left sidebar; session context belongs in the optional inspector.

## Evidence on Hand

- Existing extension and browser client under `pi/session-web/`.
- User-supplied T3 Code interface screenshot: `/var/folders/lx/409xqb9n18sf6hd_y81h33r00000gn/T/TemporaryItems/NSIRD_screencaptureui_HHyN4T/Screenshot 2026-09-22 at 18.24.51.png`.
- No testimonials, benchmark claims, or remote-service claims are available and none should be invented.

## Product Principles

1. Make ongoing agent work legible at a glance.
2. Put conversation first and harness detail one layer deeper.
3. Translate machine-shaped data into human-readable activity.
4. Remain dense enough for coding work without shrinking controls below comfortable targets.
5. Feel native in both system light and dark modes.

## Accessibility & Inclusion

Keyboard navigation, visible focus, semantic controls, reduced-motion support, sufficient contrast, and responsive use down to a narrow mobile viewport are required.
