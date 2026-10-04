# `router-usage`

A Pi extension that shows your Token Router subscription (sub) usage below the editor, the same way `codex-mux` shows Codex account usage.

It reads `GET /api/usage` from the router (`http://127.0.0.1:4000` by default) and renders one line per sub that has usage data:

```text
Router subs
  OpenCode Free  12% 5h (resets in 3.4h) · free
! comradealeximov@atomicmail.io  100% wk (resets in 21.5h) · prolite · limit reached
  comradeamirov@atomicmail.io  73% wk (resets in 5.8d) · prolite
```

Lines for subs that hit a rate limit or are in a router cooldown are marked with a `!` and colored. Disabled subs are dimmed.

## Behavior

- Refreshes the snapshot 1.5s after session start and every 15s while a session is active (a plain read of the router's SQLite-backed endpoint — no upstream calls).
- Re-reads after each settled agent turn when the snapshot is older than 60s.
- Keeps the last good snapshot in `~/.pi/agent/router-usage-cache.json`, so the widget still renders if the router is down (marked `stale`).

## Commands

```text
/router-usage            # list every sub and its usage (including subs without data)
/router-usage refresh    # force-refresh usage upstream for all subs, then re-read
```

`refresh` calls `GET /api/subs/:id/usage` for each sub (subs whose provider lacks usage monitoring fail harmlessly) and then re-reads `GET /api/usage`. The router's own poller runs every 5 minutes (`ROUTER_USAGE_POLL_MS`), so the widget normally just mirrors it.

## Configuration

`~/.pi/agent/router-usage.json`:

```json
{ "version": 1, "baseUrl": "http://127.0.0.1:4000" }
```

Without a config file, the base URL comes from `$ROUTER_URL`, then `http://127.0.0.1:4000`. The management API is unauthenticated, so keep the router bound to localhost.

## Tests

```bash
npm test   # node --test usage.test.ts
```
