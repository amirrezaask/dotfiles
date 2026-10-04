import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { createServer, type IncomingMessage, type Server, type ServerResponse } from "node:http";
import { readFile, readdir, realpath, stat } from "node:fs/promises";
import { existsSync } from "node:fs";
import { basename, dirname, extname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { networkInterfaces } from "node:os";
import { spawn } from "node:child_process";
import { WebSocketServer, type WebSocket } from "ws";

const WIDGET_KEY = "pi-session-web";
const DEFAULT_HOST = "127.0.0.1";
const MAX_BODY_BYTES = 16 * 1024 * 1024;
const MAX_ATTACHMENTS = 10;
const MAX_IMAGE_BYTES = 8 * 1024 * 1024;
const MAX_TEXT_FILE_BYTES = 2 * 1024 * 1024;
const MAX_WORKSPACE_FILES = 20_000;
const WORKSPACE_CACHE_MS = 8_000;
const SKIPPED_DIRECTORIES = new Set([".git", ".next", ".cache", "coverage", "dist", "build", "node_modules", "vendor"]);
const TEXT_EXTENSIONS = new Set([
  ".c", ".cc", ".conf", ".cpp", ".css", ".csv", ".env", ".go", ".graphql", ".h", ".hpp", ".html",
  ".ini", ".java", ".js", ".json", ".jsx", ".kt", ".less", ".log", ".lua", ".md", ".mdx", ".mjs",
  ".php", ".properties", ".py", ".rb", ".rs", ".scss", ".sh", ".sql", ".svelte", ".svg", ".toml", ".ts",
  ".tsx", ".txt", ".vue", ".xml", ".yaml", ".yml", ".zsh",
]);
const extensionDir = dirname(fileURLToPath(import.meta.url));
const clientDistDir = join(extensionDir, "client", "dist");
const clientDevDir = join(extensionDir, "client");

interface SessionWebToolCall {
  id?: string;
  name: string;
  arguments?: unknown;
}

interface SessionWebImage {
  type: "image";
  data: string;
  mimeType: string;
}

interface SessionWebFile {
  name: string;
  path?: string;
  mimeType?: string;
  size?: number;
}

interface SessionWebCommand {
  name: string;
  description?: string;
  source: "extension" | "prompt" | "skill";
  sourceInfo: {
    path: string;
    source: string;
    scope: "user" | "project" | "temporary";
    origin: "package" | "top-level";
    baseDir?: string;
  };
}

interface SessionWebMessage {
  role: string;
  timestamp?: number;
  content?: unknown;
  toolName?: string;
  toolCallId?: string;
  toolCalls?: SessionWebToolCall[];
  images?: SessionWebImage[];
  files?: SessionWebFile[];
  isError?: boolean;
  [key: string]: unknown;
}

interface SessionWebEntry {
  type: string;
  id?: string;
  parentId?: string | null;
  timestamp?: string;
  message?: SessionWebMessage;
  summary?: string;
  [key: string]: unknown;
}

interface SessionWebAttachment {
  data: string;
  mimeType: string;
}

interface SessionContextItem {
  id?: string;
  type: string;
  timestamp?: string;
  role?: string;
  label: string;
  preview: string;
  content: string;
  images?: SessionWebAttachment[];
}

interface SessionSnapshot {
  session: {
    id?: string;
    name?: string;
    cwd?: string;
    file?: string;
    model?: string;
    mode?: string;
  };
  context: {
    tokens: number | null;
    contextWindow: number;
    percent: number | null;
    systemPrompt?: string;
    entries: SessionContextItem[];
  };
  entries: SessionWebEntry[];
  commands: SessionWebCommand[];
  updatedAt: string;
}

interface ClientAttachment {
  name?: string;
  path?: string;
  data?: string;
  mimeType?: string;
  size?: number;
}

interface ClientMessage {
  type: "prompt" | "ping";
  text?: string;
  attachments?: ClientAttachment[];
  /** Kept for compatibility with an already-open pre-upgrade client. */
  images?: Array<{ data: string; mimeType: string }>;
}

interface WorkspaceFile {
  path: string;
  name: string;
  directory: string;
  mimeType: string;
  size: number;
}

function getLocalAddress(): string {
  const interfaces = networkInterfaces();
  for (const entries of Object.values(interfaces)) {
    for (const entry of entries ?? []) {
      if (entry.family === "IPv4" && !entry.internal) return entry.address;
    }
  }
  return DEFAULT_HOST;
}

function imagesFromContent(content: unknown): SessionWebImage[] {
  if (!Array.isArray(content)) return [];
  return content.flatMap((part) => {
    if (!part || typeof part !== "object") return [];
    const value = part as Record<string, unknown>;
    if (value.type !== "image" || typeof value.data !== "string" || typeof value.mimeType !== "string") return [];
    return [{ type: "image", data: value.data, mimeType: value.mimeType }];
  });
}

function contentToText(content: unknown): string {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return content
    .map((part) => {
      if (!part || typeof part !== "object") return "";
      const value = part as Record<string, unknown>;
      if (value.type === "text" && typeof value.text === "string") return value.text;
      if (value.type === "thinking" && typeof value.thinking === "string") return value.thinking;
      return "";
    })
    .filter(Boolean)
    .join("\n");
}

function toolCallsFromContent(content: unknown): SessionWebToolCall[] {
  if (!Array.isArray(content)) return [];
  return content.flatMap((part) => {
    if (!part || typeof part !== "object") return [];
    const value = part as Record<string, unknown>;
    if (value.type !== "toolCall" || typeof value.name !== "string") return [];
    return [{ id: typeof value.id === "string" ? value.id : undefined, name: value.name, arguments: value.arguments }];
  });
}

function filesFromText(content: string): SessionWebFile[] {
  const files: SessionWebFile[] = [];
  const expression = /<file name="([^"]+)"(?: path="([^"]+)")?[^>]*>/g;
  for (const match of content.matchAll(expression)) {
    const path = match[2] || match[1];
    if (!files.some((file) => file.path === path)) files.push({ name: basename(match[1]), path });
  }
  return files;
}

function stripEmbeddedFiles(content: string): string {
  return content.replace(/\n?<file name="[^"]+"(?: path="[^"]+")?[^>]*>[\s\S]*?<\/file>\n?/g, "\n").trim();
}

function normalizeEntry(entry: SessionWebEntry): SessionWebEntry {
  if (!entry.message) return entry;
  const toolCalls = toolCallsFromContent(entry.message.content);
  const images = imagesFromContent(entry.message.content);
  const content = contentToText(entry.message.content);
  const files = entry.message.role === "user" ? filesFromText(content) : [];
  return {
    ...entry,
    message: {
      role: entry.message.role,
      timestamp: entry.message.timestamp,
      content: files.length > 0 ? stripEmbeddedFiles(content) : content,
      toolName: entry.message.toolName,
      toolCallId: entry.message.toolCallId,
      toolCalls,
      images,
      files,
      isError: entry.message.isError,
    },
  };
}

function contextItem(entry: SessionWebEntry): SessionContextItem {
  const normalized = normalizeEntry(entry);
  const message = normalized.message;
  const role = message?.role;
  const label = entry.type === "compaction"
    ? "Compaction summary"
    : role === "toolResult"
      ? message?.toolName || "Tool result"
      : role === "assistant"
        ? "Pi"
        : role === "user"
          ? "You"
          : entry.type.replaceAll("_", " ");
  const messageContent = typeof message?.content === "string" ? message.content : "";
  const toolContent = message?.toolCalls?.map((call) => `${call.name}\n${call.arguments === undefined ? "" : JSON.stringify(call.arguments, null, 2)}`).join("\n\n") ?? "";
  const content = [messageContent, entry.summary, toolContent].filter(Boolean).join("\n\n").slice(0, 20000);
  return { id: entry.id, type: entry.type, timestamp: entry.timestamp, role, label, preview: content.slice(0, 180), content, images: message?.images };
}

async function readStatic(pathname: string): Promise<{ body: Buffer; contentType: string } | undefined> {
  const root = existsSync(join(clientDistDir, "index.html")) ? clientDistDir : clientDevDir;
  const requested = pathname === "/" ? "index.html" : pathname.replace(/^\//, "");
  const file = join(root, requested);
  if (!file.startsWith(root)) return undefined;
  try {
    const body = await readFile(file);
    const ext = file.slice(file.lastIndexOf(".") + 1);
    const contentType = ext === "html" ? "text/html; charset=utf-8" : ext === "js" ? "text/javascript; charset=utf-8" : ext === "css" ? "text/css; charset=utf-8" : "application/octet-stream";
    return { body, contentType };
  } catch {
    if (pathname !== "/") return readStatic("/");
    return undefined;
  }
}

async function readJsonBody(request: IncomingMessage): Promise<unknown> {
  let size = 0;
  const chunks: Buffer[] = [];
  for await (const chunk of request) {
    const buffer = Buffer.from(chunk);
    size += buffer.length;
    if (size > MAX_BODY_BYTES) throw new Error("Request body is too large");
    chunks.push(buffer);
  }
  return JSON.parse(Buffer.concat(chunks).toString("utf8"));
}

function writeJson(response: ServerResponse, status: number, value: unknown): void {
  const body = JSON.stringify(value);
  response.writeHead(status, {
    "content-type": "application/json; charset=utf-8",
    "cache-control": "no-store",
    "access-control-allow-origin": "*",
  });
  response.end(body);
}

function isPathInside(path: string, root: string): boolean {
  const pathFromRoot = relative(root, path);
  return pathFromRoot === "" || (!pathFromRoot.startsWith(`..${sep}`) && pathFromRoot !== "..");
}

function isTextFile(name: string, mimeType = ""): boolean {
  return mimeType.startsWith("text/") || TEXT_EXTENSIONS.has(extname(name).toLowerCase());
}

function escapeXmlAttribute(value: string): string {
  return value.replaceAll("&", "&amp;").replaceAll('"', "&quot;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
}

function mimeTypeForPath(path: string): string {
  const extension = extname(path).toLowerCase();
  if ([".png"].includes(extension)) return "image/png";
  if ([".jpg", ".jpeg"].includes(extension)) return "image/jpeg";
  if (extension === ".gif") return "image/gif";
  if (extension === ".webp") return "image/webp";
  if (extension === ".svg") return "image/svg+xml";
  if (extension === ".json") return "application/json";
  if (extension === ".pdf") return "application/pdf";
  return isTextFile(path) ? "text/plain" : "application/octet-stream";
}

async function listWorkspaceFiles(cwd: string): Promise<WorkspaceFile[]> {
  const root = await realpath(cwd);
  const files: WorkspaceFile[] = [];
  const pending = [root];
  while (pending.length > 0 && files.length < MAX_WORKSPACE_FILES) {
    const directory = pending.pop();
    if (!directory) break;
    let entries;
    try {
      entries = await readdir(directory, { withFileTypes: true });
    } catch {
      continue;
    }
    entries.sort((a, b) => a.name.localeCompare(b.name));
    for (const entry of entries) {
      if (entry.name.startsWith(".") || SKIPPED_DIRECTORIES.has(entry.name)) continue;
      const absolutePath = join(directory, entry.name);
      if (entry.isDirectory()) {
        pending.push(absolutePath);
        continue;
      }
      if (!entry.isFile()) continue;
      try {
        const fileStats = await stat(absolutePath);
        const path = relative(root, absolutePath).split(sep).join("/");
        files.push({ path, name: entry.name, directory: dirname(path) === "." ? "" : dirname(path), mimeType: mimeTypeForPath(path), size: fileStats.size });
        if (files.length >= MAX_WORKSPACE_FILES) break;
      } catch {
        // A file may disappear while the workspace is being indexed.
      }
    }
  }
  return files;
}

async function buildAttachmentContent(attachments: ClientAttachment[], cwd: string): Promise<{ text: string; images: SessionWebImage[] }> {
  if (attachments.length > MAX_ATTACHMENTS) throw new Error(`Attach at most ${MAX_ATTACHMENTS} files at a time.`);
  const root = await realpath(cwd);
  let text = "";
  const images: SessionWebImage[] = [];

  for (const attachment of attachments) {
    if (attachment.path) {
      const absolutePath = resolve(root, attachment.path);
      let canonicalPath: string;
      try {
        canonicalPath = await realpath(absolutePath);
      } catch {
        throw new Error(`${attachment.path} is no longer available.`);
      }
      if (!isPathInside(canonicalPath, root)) throw new Error("Workspace attachments must stay inside the current project.");
      const fileStats = await stat(canonicalPath);
      if (!fileStats.isFile()) throw new Error(`${attachment.path} is not a file.`);
      const mimeType = attachment.mimeType || mimeTypeForPath(canonicalPath);
      const displayName = escapeXmlAttribute(basename(canonicalPath));
      const relativePath = escapeXmlAttribute(relative(root, canonicalPath).split(sep).join("/"));
      if (mimeType.startsWith("image/")) {
        if (fileStats.size > MAX_IMAGE_BYTES) throw new Error(`${relativePath} is larger than 8 MB.`);
        images.push({ type: "image", data: (await readFile(canonicalPath)).toString("base64"), mimeType });
        text += `<file name="${displayName}" path="${relativePath}"></file>\n`;
      } else {
        if (!isTextFile(canonicalPath, mimeType)) throw new Error(`${relativePath} is not a supported text or image file.`);
        if (fileStats.size > MAX_TEXT_FILE_BYTES) throw new Error(`${relativePath} is larger than 2 MB.`);
        text += `<file name="${displayName}" path="${relativePath}">\n${await readFile(canonicalPath, "utf8")}\n</file>\n`;
      }
      continue;
    }

    const data = typeof attachment.data === "string" ? attachment.data : "";
    const mimeType = typeof attachment.mimeType === "string" ? attachment.mimeType : "";
    const name = (attachment.name || "attachment").replace(/[\\/]/g, "_");
    if (!data) throw new Error(`${name} could not be read.`);
    const byteLength = Buffer.byteLength(data, "base64");
    if (mimeType.startsWith("image/")) {
      if (byteLength > MAX_IMAGE_BYTES) throw new Error(`${name} is larger than 8 MB.`);
      images.push({ type: "image", data, mimeType });
      text += `<file name="${escapeXmlAttribute(name)}"></file>\n`;
    } else {
      if (!isTextFile(name, mimeType)) throw new Error(`${name} is not a supported text or image file.`);
      if (byteLength > MAX_TEXT_FILE_BYTES) throw new Error(`${name} is larger than 2 MB.`);
      const content = Buffer.from(data, "base64").toString("utf8").replace(/^\uFEFF/, "");
      text += `<file name="${escapeXmlAttribute(name)}">\n${content}\n</file>\n`;
    }
  }
  return { text, images };
}

function openBrowser(url: string): void {
  const command = process.platform === "darwin" ? "open" : process.platform === "win32" ? "cmd" : "xdg-open";
  const args = process.platform === "win32" ? ["/c", "start", "", url] : [url];
  const child = spawn(command, args, { detached: true, stdio: "ignore" });
  child.unref();
}

export default function sessionWeb(pi: ExtensionAPI) {
  let server: Server | undefined;
  let websocketServer: WebSocketServer | undefined;
  let webSockets: WebSocket[] = [];
  let sessionContext: ExtensionContext | undefined;
  let url = "";
  let port: number | undefined;
  let openedForSession = false;
  let workspaceFileCache: { cwd: string; expiresAt: number; files: WorkspaceFile[] } | undefined;

  const snapshot = (): SessionSnapshot => {
    const ctx = sessionContext;
    const entries = (ctx?.sessionManager.getEntries() ?? []) as unknown as SessionWebEntry[];
    const usage = ctx?.getContextUsage();
    const activeContextEntries = (ctx?.sessionManager.buildContextEntries() ?? []) as unknown as SessionWebEntry[];
    return {
      session: {
        id: ctx?.sessionManager.getSessionId(),
        name: ctx?.sessionManager.getSessionName(),
        cwd: ctx?.cwd,
        file: ctx?.sessionManager.getSessionFile(),
        model: ctx?.model ? `${ctx.model.provider}/${ctx.model.id}` : undefined,
        mode: ctx?.mode,
      },
      context: {
        tokens: usage?.tokens ?? null,
        contextWindow: usage?.contextWindow ?? 0,
        percent: usage?.percent ?? null,
        systemPrompt: ctx?.getSystemPrompt()?.slice(0, 16000),
        entries: activeContextEntries.map(contextItem),
      },
      entries: entries.map(normalizeEntry),
      commands: pi.getCommands() as SessionWebCommand[],
      updatedAt: new Date().toISOString(),
    };
  };

  const broadcast = (event: string): void => {
    const message = JSON.stringify({ event, snapshot: snapshot() });
    webSockets = webSockets.filter((socket) => socket.readyState === socket.OPEN);
    for (const socket of webSockets) socket.send(message);
  };

  const updateWidget = (ctx: ExtensionContext): void => {
    if (!ctx.hasUI || !url) return;
    const host = url.replace(/^https?:\/\//, "").split(":")[0];
    const lines = [
      ctx.ui.theme.fg("accent", "◉ session web"),
      `${ctx.ui.theme.fg("muted", "local")}  ${ctx.ui.theme.fg("accent", url)}`,
      ctx.ui.theme.fg("dim", `live session mirror · ${host === "127.0.0.1" ? "this machine only" : "local network"}`),
    ];
    ctx.ui.setWidget(WIDGET_KEY, lines, { placement: "belowEditor" });
  };

  const stopServer = async (): Promise<void> => {
    for (const socket of webSockets) socket.close();
    webSockets = [];
    websocketServer?.close();
    websocketServer = undefined;
    if (!server) return;
    await new Promise<void>((resolve) => server?.close(() => resolve()));
    server = undefined;
    port = undefined;
    url = "";
    workspaceFileCache = undefined;
  };

  const startServer = async (ctx: ExtensionContext): Promise<void> => {
    sessionContext = ctx;
    const host = process.env.PI_SESSION_WEB_HOST || DEFAULT_HOST;

    server = createServer(async (request, response) => {
      const requestUrl = new URL(request.url ?? "/", `http://${host}:${port ?? 0}`);
      if (requestUrl.pathname === "/health") {
        writeJson(response, 200, { ok: true, port, updatedAt: new Date().toISOString() });
        return;
      }
      if (requestUrl.pathname === "/api/session") {
        writeJson(response, 200, snapshot());
        return;
      }
      if (requestUrl.pathname === "/api/files") {
        try {
          const cwd = sessionContext?.cwd;
          if (!cwd) throw new Error("The workspace is not available yet.");
          if (!workspaceFileCache || workspaceFileCache.cwd !== cwd || workspaceFileCache.expiresAt < Date.now()) {
            workspaceFileCache = { cwd, expiresAt: Date.now() + WORKSPACE_CACHE_MS, files: await listWorkspaceFiles(cwd) };
          }
          const query = (requestUrl.searchParams.get("q") || "").trim().toLowerCase();
          const files = query
            ? workspaceFileCache.files.filter((file) => file.path.toLowerCase().includes(query)).slice(0, 80)
            : workspaceFileCache.files.slice(0, 80);
          writeJson(response, 200, { files, truncated: workspaceFileCache.files.length >= MAX_WORKSPACE_FILES });
        } catch (error) {
          writeJson(response, 500, { error: error instanceof Error ? error.message : "Unable to list workspace files" });
        }
        return;
      }
      if (requestUrl.pathname === "/api/prompt" && request.method === "OPTIONS") {
        response.writeHead(204, {
          "access-control-allow-origin": "*",
          "access-control-allow-methods": "GET,POST,OPTIONS",
          "access-control-allow-headers": "content-type",
        });
        response.end();
        return;
      }
      if (requestUrl.pathname === "/api/prompt" && request.method === "POST") {
        try {
          const body = (await readJsonBody(request)) as Partial<ClientMessage>;
          const text = typeof body.text === "string" ? body.text.trim() : "";
          const attachments: ClientAttachment[] = Array.isArray(body.attachments)
            ? body.attachments
            : (body.images ?? []).map((image, index) => ({ ...image, name: `image-${index + 1}` }));
          if (body.type !== "prompt" || (!text && attachments.length === 0)) {
            writeJson(response, 400, { error: "A prompt or file attachment is required" });
            return;
          }
          const currentContext = sessionContext;
          const attachmentContent = await buildAttachmentContent(attachments, currentContext?.cwd ?? process.cwd());
          const fullText = [text, attachmentContent.text.trim()].filter(Boolean).join("\n\n");
          const content = attachmentContent.images.length > 0
            ? [...(fullText ? [{ type: "text" as const, text: fullText }] : []), ...attachmentContent.images]
            : fullText;
          pi.sendUserMessage(content, {
            ...(currentContext && !currentContext.isIdle() ? { deliverAs: "steer" as const } : {}),
            expandPromptTemplates: true,
          });
          writeJson(response, 202, { ok: true });
          broadcast("prompt");
        } catch (error) {
          writeJson(response, 400, { error: error instanceof Error ? error.message : "Invalid request" });
        }
        return;
      }
      const staticFile = await readStatic(requestUrl.pathname);
      if (!staticFile) {
        writeJson(response, 404, { error: "Not found" });
        return;
      }
      response.writeHead(200, { "content-type": staticFile.contentType, "cache-control": "no-cache" });
      response.end(staticFile.body);
    });

    websocketServer = new WebSocketServer({ server });
    websocketServer.on("connection", (socket) => {
      webSockets.push(socket);
      socket.send(JSON.stringify({ event: "connected", snapshot: snapshot() }));
      socket.on("close", () => {
        webSockets = webSockets.filter((candidate) => candidate !== socket);
      });
      socket.on("message", (raw) => {
        try {
          const message = JSON.parse(raw.toString()) as ClientMessage;
          if (message.type === "ping") socket.send(JSON.stringify({ event: "pong" }));
        } catch {}
      });
    });

    await new Promise<void>((resolve, reject) => {
      server?.once("error", reject);
      server?.listen(0, host, () => resolve());
    });
    const addressInfo = server.address();
    if (!addressInfo || typeof addressInfo === "string") throw new Error("Session web did not receive a local port");
    port = addressInfo.port;
    const address = host === DEFAULT_HOST ? DEFAULT_HOST : getLocalAddress();
    url = `http://${address}:${port}/`;
    updateWidget(ctx);
    if (!openedForSession && ctx.hasUI) {
      openedForSession = true;
      ctx.ui.notify(`Session web is live at ${url}`, "info");
    }
  };

  const refresh = (event: string, ctx: ExtensionContext): void => {
    sessionContext = ctx;
    updateWidget(ctx);
    broadcast(event);
  };

  pi.registerCommand("session-web", {
    description: "Open or show the live web UI for the current session",
    handler: async (args, ctx) => {
      if (!server) await startServer(ctx);
      if (args.trim() === "open" && url) openBrowser(url);
      if (url) ctx.ui.notify(`Session web: ${url}`, "info");
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    openedForSession = false;
    await stopServer();
    await startServer(ctx);
  });

  for (const event of ["message_end", "tool_execution_end", "agent_settled", "session_info_changed"] as const) {
    pi.on(event, (_event, ctx) => refresh(event, ctx));
  }

  pi.on("session_shutdown", async (_event, ctx) => {
    ctx.ui.setWidget(WIDGET_KEY, undefined);
    await stopServer();
    sessionContext = undefined;
  });
}
