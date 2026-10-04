# Pi Session Web

A project-local Pi extension that serves a private, live browser view of the currently running session.

## What it does

- Starts an HTTP/WebSocket server on a system-assigned free port when a session starts.
- Serves the React + Tailwind + shadcn-style client from the extension itself.
- Streams session updates to connected browsers over WebSocket.
- Accepts prompts from the browser and sends them into the current Pi session.
- Shows the local URL in a widget below Pi's prompt editor.
- Shows the URL once per session. Use `/session-web open` to open it in a browser, or `/session-web` to show it again.
- Binds to `127.0.0.1` by default and uses a system-assigned free port.

## Install

From this repository:

```bash
cd pi/session-web
npm install
npm run build
```

Then link it with the repository's sync script:

```bash
./sync --force
```

Or load it directly for a quick test:

```bash
pi -e ./pi/session-web/index.ts
```

The extension is linked to `~/.pi/agent/extensions/pi-session-web` by `sync`.

## Network access

The default server is local-only (`127.0.0.1`). To expose it to your local network, start Pi with:

```bash
PI_SESSION_WEB_HOST=0.0.0.0 pi
```

The widget then prints the machine's local IPv4 address.

## Development

The extension serves `client/dist` when it exists, and falls back to `client/` for development. Build after client changes:

```bash
npm run build
```
