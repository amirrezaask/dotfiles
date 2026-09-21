# `codex-mux`

A Pi extension for using multiple OpenAI Codex OAuth subscriptions through one model provider.

## Setup

The extension creates two OAuth-only account slots on first run:

- `openai-codex-account-1`
- `openai-codex-account-2`

Log in to each slot separately:

```text
/login openai-codex-account-1
/login openai-codex-account-2
```

Then select a model under `openai-codex-mux`. The mux chooses the logged-in account with the lowest cached Codex usage. The built-in `openai-codex` provider is disabled so requests always go through the mux. Use `/codex-fast on|off` to control Codex priority/fast mode for subsequent requests. The account widget shows one email and its usage per line below the editor, including the relative reset time for each limit (for example, `23% 5h (resets in 2.4h)`).

Use `/codex-accounts` to open an interactive account picker. The widget marks the account active when the session starts and updates it after each request. The last active account is persisted and receives the first request in the next session. Choosing an email pins it and persists it as the next session's starting account; choose **Automatic** to let the mux select by remaining usage. Management forms:

```text
/codex-accounts refresh
/codex-accounts add Work
/codex-accounts use person@example.com
/codex-accounts use auto
```

Account slot metadata is stored in `~/.pi/agent/codex-mux.json`. OAuth credentials remain in Pi's normal auth store, keyed by the account provider ID. Usage is cached in `~/.pi/agent/codex-mux-usage.json`.

## Performance

This intentionally does not install or invoke `pi-usage`. It calls the same Codex usage endpoint directly in delayed/background refreshes and refreshes usage every 15 seconds while a session is active. The active account is highlighted in the widget using in-memory state; account auth checks and formatting happen only when the widget is updated, never inside its render loop. Usage cache writes are batched once per refresh batch. It does not replace Pi's footer, scan session history during renders, or spawn `codex app-server` as a fallback.
