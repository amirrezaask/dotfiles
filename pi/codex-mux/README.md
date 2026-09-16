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

Then select a model under `openai-codex-mux`. The mux chooses the logged-in account with the lowest cached Codex usage. The account widget shows one email and its usage per line below the editor.

Use `/codex-accounts` to inspect the slots and usage. Other forms:

```text
/codex-accounts refresh
/codex-accounts add Work
/codex-accounts use person@example.com
/codex-accounts use auto
```

Account slot metadata is stored in `~/.pi/agent/codex-mux.json`. OAuth credentials remain in Pi's normal auth store, keyed by the account provider ID. Usage is cached in `~/.pi/agent/codex-mux-usage.json`.

## Performance

This intentionally does not install or invoke `pi-usage`. It calls the same Codex usage endpoint directly, but only in delayed/background refreshes. It does not replace Pi's footer, scan session history during renders, poll continuously, or spawn `codex app-server` as a fallback.
