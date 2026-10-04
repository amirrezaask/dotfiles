import {
  StrictMode,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
  type ChangeEvent,
  type ClipboardEvent,
  type Dispatch,
  type KeyboardEvent,
  type ReactNode,
  type SetStateAction,
} from "react";
import { createRoot } from "react-dom/client";
import { motion, useReducedMotion } from "motion/react";
import Markdown from "react-markdown";
import remarkGfm from "remark-gfm";
import {
  ArrowDown,
  ArrowUp,
  Braces,
  Check,
  CheckCircle2,
  ChevronDown,
  CircleAlert,
  Code2,
  Copy,
  File,
  FileCode2,
  FilePenLine,
  FileSearch,
  Files,
  FolderSearch,
  Globe2,
  Image,
  LoaderCircle,
  Paperclip,
  PanelRight,
  Search,
  Sparkles,
  Terminal,
  UserRound,
  Wrench,
  X,
  type LucideIcon,
} from "lucide-react";
import {
  Attachment,
  AttachmentContent,
  AttachmentDescription,
  AttachmentGroup,
  AttachmentImage,
  AttachmentMedia,
  AttachmentTitle,
} from "./components/ui/attachment";
import { Badge } from "./components/ui/badge";
import { Button } from "./components/ui/button";
import { Command, CommandEmpty, CommandGroup, CommandItem, CommandList, CommandSeparator } from "./components/ui/command";
import { Marker, MarkerContent, MarkerIcon } from "./components/ui/marker";
import { cn } from "./lib/utils";
import "./styles.css";

type ToolCall = { id?: string; name: string; arguments?: unknown };
type ComposerAttachment = { id: string; data?: string; mimeType: string; name: string; size: number; path?: string; source: "local" | "workspace" };
type WorkspaceFile = { path: string; name: string; directory: string; mimeType: string; size: number };
type CommandInfo = { name: string; description?: string; source: "extension" | "prompt" | "skill"; sourceInfo: { scope: "user" | "project" | "temporary" } };
type AutocompleteTrigger = { kind: "command" | "file"; start: number; end: number; query: string };

type Entry = {
  type: string;
  id?: string;
  timestamp?: string;
  summary?: string;
  message?: {
    role: string;
    timestamp?: number;
    content?: string;
    toolName?: string;
    toolCallId?: string;
    toolCalls?: ToolCall[];
    images?: Array<{ data: string; mimeType: string; name?: string; size?: number }>;
    files?: Array<{ name: string; path?: string; mimeType?: string; size?: number }>;
    isError?: boolean;
  };
};

type ContextItem = {
  id?: string;
  type: string;
  timestamp?: string;
  role?: string;
  label: string;
  preview: string;
};

type Snapshot = {
  session: { id?: string; name?: string; cwd?: string; file?: string; model?: string; mode?: string };
  context: { tokens: number | null; contextWindow: number; percent: number | null; systemPrompt?: string; entries: ContextItem[] };
  entries: Entry[];
  commands: CommandInfo[];
  updatedAt: string;
};

type ToolRow = { key: string; call?: ToolCall; result?: Entry };
type DisplayItem = { kind: "message"; entry: Entry } | { kind: "tools"; entries: Entry[] };
type CodePreview = { code: string; language: string };
type ThemeName = "github-dark" | "github-light";
type CodeHighlighter = (code: string, language: string, theme: ThemeName) => Promise<string>;

const initialSnapshot: Snapshot = {
  session: {},
  context: { tokens: null, contextWindow: 0, percent: null, entries: [] },
  entries: [],
  commands: [],
  updatedAt: new Date().toISOString(),
};

const markdownPlugins = [remarkGfm];
const languageAliases: Record<string, string> = {
  bash: "shellscript",
  sh: "shellscript",
  zsh: "shellscript",
  js: "javascript",
  ts: "typescript",
  md: "markdown",
  yml: "yaml",
  py: "python",
  rb: "ruby",
  rs: "rust",
};
const highlightedCodeCache = new Map<string, Promise<string | undefined>>();
let codeHighlighterLoader: Promise<CodeHighlighter> | undefined;

function timeLabel(timestamp?: number | string) {
  if (!timestamp) return "";
  const date = new Date(timestamp);
  if (Number.isNaN(date.getTime())) return "";
  return new Intl.DateTimeFormat(undefined, { hour: "numeric", minute: "2-digit" }).format(date);
}

function timestampValue(timestamp?: number | string) {
  if (!timestamp) return undefined;
  const date = new Date(timestamp);
  return Number.isNaN(date.getTime()) ? undefined : date.toISOString();
}

function formatTokens(value: number) {
  return new Intl.NumberFormat(undefined, { notation: "compact", maximumFractionDigits: 1 }).format(value);
}

function modelLabel(model?: string) {
  if (!model) return "Waiting for model";
  const id = model.split("/").at(-1) ?? model;
  return id.replace(/^claude-/, "Claude ").replace(/^gpt-/, "GPT ").replaceAll("-", " ");
}

function compactTitle(value: string) {
  const plainText = value
    .replace(/```[\s\S]*?```/g, " code ")
    .replace(/!\[[^\]]*\]\([^)]*\)/g, "")
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1")
    .replace(/^[\s>#+-]+/gm, "")
    .replace(/[`*_~]/g, "")
    .replace(/\s+/g, " ")
    .trim();
  if (plainText.length <= 58) return plainText;
  const shortened = plainText.slice(0, 58).replace(/\s+\S*$/, "").trim();
  return `${shortened || plainText.slice(0, 58).trim()}…`;
}

function sessionTitle(name: string | undefined, entries: Entry[], cwd?: string) {
  const explicitName = name?.trim();
  if (explicitName && !/^(?:new|untitled)(?:\s+session)?$/i.test(explicitName)) return explicitName;
  const firstUserMessage = entries.find((entry) => entry.message?.role === "user")?.message;
  const promptTitle = compactTitle(firstUserMessage?.content ?? "");
  if (promptTitle) return promptTitle;
  if ((firstUserMessage?.images?.length ?? 0) > 0) return "Image review";
  return cwd?.split("/").filter(Boolean).at(-1) || "Pi session";
}

function loadCodeHighlighter(): Promise<CodeHighlighter> {
  codeHighlighterLoader ??= import("./lib/highlight").then((module) => module.codeToHtml);
  return codeHighlighterLoader;
}

function highlightCode(code: string, language: string, theme: ThemeName): Promise<string | undefined> {
  const key = `${theme}:${language}:${code}`;
  const cached = highlightedCodeCache.get(key);
  if (cached) return cached;
  const highlighted = loadCodeHighlighter()
    .then((codeToHtml) => codeToHtml(code, language, theme))
    .catch(() => undefined);
  highlightedCodeCache.set(key, highlighted);
  if (highlightedCodeCache.size > 120) highlightedCodeCache.delete(highlightedCodeCache.keys().next().value as string);
  return highlighted;
}

function useSystemTheme(): ThemeName {
  const [theme, setTheme] = useState<ThemeName>(() => window.matchMedia("(prefers-color-scheme: dark)").matches ? "github-dark" : "github-light");
  useEffect(() => {
    const media = window.matchMedia("(prefers-color-scheme: dark)");
    const update = () => setTheme(media.matches ? "github-dark" : "github-light");
    update();
    media.addEventListener("change", update);
    return () => media.removeEventListener("change", update);
  }, []);
  return theme;
}

function ShikiCodeBlock({ code, language, className }: CodePreview & { className?: string }) {
  const theme = useSystemTheme();
  const [highlighted, setHighlighted] = useState<string>();

  useEffect(() => {
    let active = true;
    setHighlighted(undefined);
    void highlightCode(code, language || "text", theme).then((html) => {
      if (active) setHighlighted(html);
    });
    return () => { active = false; };
  }, [code, language, theme]);

  return highlighted
    ? <div className={cn("code-block", className)} dangerouslySetInnerHTML={{ __html: highlighted }} />
    : <pre className={cn("code-block", className)}><code>{code}</code></pre>;
}

function MarkdownContent({ content, className }: { content: string; className?: string }) {
  return (
    <div className={cn("markdown", className)}>
      <Markdown
        remarkPlugins={markdownPlugins}
        components={{
          a: ({ href, children, ...props }) => <a href={href} target="_blank" rel="noreferrer" {...props}>{children}</a>,
          pre: ({ children }) => <>{children}</>,
          code: ({ className: codeClassName, children, ...props }) => {
            const match = /language-([^\s]+)/.exec(codeClassName ?? "");
            const code = String(children).replace(/\n$/, "");
            return match
              ? <ShikiCodeBlock code={code} language={languageAliases[match[1]] || match[1]} />
              : <code className={codeClassName} {...props}>{children}</code>;
          },
        }}
      >
        {content}
      </Markdown>
    </div>
  );
}

function App() {
  const reduceMotion = useReducedMotion();
  const [snapshot, setSnapshot] = useState(initialSnapshot);
  const [snapshotError, setSnapshotError] = useState<string>();
  const [connected, setConnected] = useState(false);
  const [draft, setDraft] = useState("");
  const [sending, setSending] = useState(false);
  const [copied, setCopied] = useState(false);
  const [contextOpen, setContextOpen] = useState(false);
  const [attachments, setAttachments] = useState<ComposerAttachment[]>([]);
  const [attachmentError, setAttachmentError] = useState<string>();
  const contextButtonRef = useRef<HTMLButtonElement>(null);
  const bottomRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLTextAreaElement>(null);
  const fileInputRef = useRef<HTMLInputElement>(null);

  const applyEvent = useCallback((event: { snapshot?: Snapshot }) => {
    if (event.snapshot) {
      setSnapshot(event.snapshot);
      setSnapshotError(undefined);
    }
  }, []);

  useEffect(() => {
    let active = true;
    const fetchSnapshot = async () => {
      try {
        const response = await fetch("/api/session");
        if (!response.ok) throw new Error("Session snapshot unavailable");
        if (active) applyEvent({ snapshot: await response.json() as Snapshot });
      } catch {
        if (active) setSnapshotError("Unable to load this session. Refresh the page to try again.");
      }
    };
    void fetchSnapshot();
    const socket = new WebSocket(`${location.protocol === "https:" ? "wss" : "ws"}://${location.host}/`);
    socket.addEventListener("open", () => setConnected(true));
    socket.addEventListener("error", () => setConnected(false));
    socket.addEventListener("close", () => setConnected(false));
    socket.addEventListener("message", (message) => {
      try { applyEvent(JSON.parse(message.data as string)); } catch { /* malformed server event */ }
    });
    return () => { active = false; socket.close(); };
  }, [applyEvent]);

  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: reduceMotion ? "auto" : "smooth" });
  }, [snapshot.entries.length, reduceMotion]);

  const messages = useMemo(
    () => snapshot.entries.filter((entry) => ["user", "assistant", "toolResult"].includes(entry.message?.role ?? "") || entry.type === "compaction"),
    [snapshot.entries],
  );
  const toolCallsById = useMemo(
    () => new Map(messages.flatMap((entry) => entry.message?.toolCalls ?? []).filter((call) => call.id).map((call) => [call.id as string, call])),
    [messages],
  );
  const displayItems = useMemo(() => buildDisplayItems(messages), [messages]);
  const title = useMemo(
    () => sessionTitle(snapshot.session.name, messages, snapshot.session.cwd),
    [snapshot.session.name, snapshot.session.cwd, messages],
  );

  useEffect(() => { document.title = title; }, [title]);

  async function submitPrompt() {
    const text = draft.trim();
    if ((!text && attachments.length === 0) || sending) return;
    setSending(true);
    setAttachmentError(undefined);
    try {
      const response = await fetch("/api/prompt", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          type: "prompt",
          text: text || "Please inspect the attached files.",
          attachments: attachments.map(({ data, mimeType, name, path, size }) => ({ data, mimeType, name, path, size })),
        }),
      });
      if (!response.ok) {
        setAttachmentError("Message was not sent. Check the session connection and try again.");
        return;
      }
      setDraft("");
      setAttachments([]);
    } catch {
      setAttachmentError("Message was not sent. Check the session connection and try again.");
    } finally {
      setSending(false);
      inputRef.current?.focus();
    }
  }

  async function copyUrl() {
    try {
      await navigator.clipboard.writeText(window.location.href);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1400);
    } catch {
      setSnapshotError("The session URL could not be copied. Copy it from the address bar instead.");
    }
  }

  function closeContext() {
    setContextOpen(false);
    window.requestAnimationFrame(() => contextButtonRef.current?.focus());
  }

  return (
    <div className="app-shell">
      <TopBar
        title={title}
        connected={connected}
        copied={copied}
        contextOpen={contextOpen}
        contextButtonRef={contextButtonRef}
        onCopy={() => void copyUrl()}
        onToggleContext={() => setContextOpen((value) => !value)}
      />

      <div className="workspace">
        <main id="session-messages" aria-label="Session messages" className="conversation">
          <div className="conversation-column">
            {snapshotError ? <div role="alert" className="inline-alert"><CircleAlert /> <span>{snapshotError}</span></div> : null}
            {displayItems.length === 0
              ? <EmptyState onFocus={() => inputRef.current?.focus()} />
              : <div className="timeline">{displayItems.map((item, index) => item.kind === "tools"
                ? <ToolActivity key={`tools-${index}`} entries={item.entries} index={index} reduceMotion={!!reduceMotion} toolCallsById={toolCallsById} />
                : <MessageItem key={item.entry.id || `${item.entry.type}-${index}`} entry={item.entry} index={index} reduceMotion={!!reduceMotion} />)}</div>}
            <div ref={bottomRef} />
          </div>
        </main>

        {contextOpen ? <button type="button" className="mobile-scrim context-scrim" aria-label="Close context window" onClick={closeContext} /> : null}
        {contextOpen ? <ContextPanel context={snapshot.context} onClose={closeContext} /> : null}
      </div>

      <Composer
        draft={draft}
        setDraft={setDraft}
        attachments={attachments}
        setAttachments={setAttachments}
        sending={sending}
        error={attachmentError}
        setError={setAttachmentError}
        inputRef={inputRef}
        fileInputRef={fileInputRef}
        model={snapshot.session.model}
        commands={snapshot.commands}
        cwd={snapshot.session.cwd}
        onSubmit={() => void submitPrompt()}
      />
    </div>
  );
}

function TopBar({
  title,
  connected,
  copied,
  contextOpen,
  contextButtonRef,
  onCopy,
  onToggleContext,
}: {
  title: string;
  connected: boolean;
  copied: boolean;
  contextOpen: boolean;
  contextButtonRef: React.RefObject<HTMLButtonElement | null>;
  onCopy: () => void;
  onToggleContext: () => void;
}) {
  return (
    <header className="topbar">
      <div className="topbar-brand">
        <div aria-hidden="true" className="pi-mark">π</div>
        <span className="product-name">Session Web</span>
      </div>
      <div className="topbar-session">
        <span className="topbar-title" title={title}>{title}</span>
        <span className={cn("connection", connected ? "is-connected" : "is-reconnecting")} role="status" aria-live="polite">
          <span aria-hidden="true" className="connection-dot" />
          {connected ? "Live" : "Reconnecting"}
        </span>
      </div>
      <div className="topbar-actions">
        <Button variant="ghost" size="icon" onClick={onCopy} aria-label={copied ? "Session URL copied" : "Copy session URL"} title="Copy session URL">
          {copied ? <Check /> : <Copy />}
        </Button>
        <Button ref={contextButtonRef} variant="ghost" size="icon" onClick={onToggleContext} aria-expanded={contextOpen} aria-controls="session-context" aria-label={contextOpen ? "Hide context window" : "Show context window"} title="Show context window">
          <PanelRight />
        </Button>
      </div>
    </header>
  );
}

function EmptyState({ onFocus }: { onFocus: () => void }) {
  return (
    <section aria-labelledby="empty-state-heading" className="empty-state">
      <div aria-hidden="true" className="empty-glyph">π</div>
      <h2 id="empty-state-heading">Ready when you are</h2>
      <p>Messages, edits, commands, and results from this Pi session will appear here.</p>
      <Button variant="outline" size="sm" onClick={onFocus}>Message Pi</Button>
    </section>
  );
}

function Composer({
  draft,
  setDraft,
  attachments,
  setAttachments,
  sending,
  error,
  setError,
  inputRef,
  fileInputRef,
  model,
  commands,
  cwd,
  onSubmit,
}: {
  draft: string;
  setDraft: Dispatch<SetStateAction<string>>;
  attachments: ComposerAttachment[];
  setAttachments: Dispatch<SetStateAction<ComposerAttachment[]>>;
  sending: boolean;
  error?: string;
  setError: Dispatch<SetStateAction<string | undefined>>;
  inputRef: React.RefObject<HTMLTextAreaElement | null>;
  fileInputRef: React.RefObject<HTMLInputElement | null>;
  model?: string;
  commands: CommandInfo[];
  cwd?: string;
  onSubmit: () => void;
}) {
  const [trigger, setTrigger] = useState<AutocompleteTrigger>();
  const [workspaceFiles, setWorkspaceFiles] = useState<WorkspaceFile[]>([]);
  const [filesLoading, setFilesLoading] = useState(false);
  const [filesError, setFilesError] = useState<string>();
  const [dragDepth, setDragDepth] = useState(0);
  const [dragHasFiles, setDragHasFiles] = useState(false);
  const [selectedValue, setSelectedValue] = useState("");
  const dragActive = dragDepth > 0 && dragHasFiles;
  const canSend = draft.trim().length > 0 || attachments.length > 0;
  const filteredCommands = useMemo(() => filterCommands(commands, trigger?.kind === "command" ? trigger.query : ""), [commands, trigger]);
  const visibleFiles = useMemo(() => filterWorkspaceFiles(workspaceFiles, trigger?.kind === "file" ? trigger.query : ""), [workspaceFiles, trigger]);
  const popupOpen = Boolean(trigger);
  const commandGroups = useMemo(() => groupCommands(filteredCommands), [filteredCommands]);

  useEffect(() => {
    if (trigger?.kind !== "file") return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => {
      setFilesLoading(true);
      setFilesError(undefined);
      void fetch(`/api/files?q=${encodeURIComponent(trigger.query)}`, { signal: controller.signal })
        .then(async (response) => {
          if (!response.ok) throw new Error("Workspace files are unavailable.");
          const body = await response.json() as { files?: WorkspaceFile[] };
          setWorkspaceFiles(body.files ?? []);
        })
        .catch((fetchError: unknown) => {
          if (!controller.signal.aborted) setFilesError(fetchError instanceof Error ? fetchError.message : "Workspace files are unavailable.");
        })
        .finally(() => { if (!controller.signal.aborted) setFilesLoading(false); });
    }, workspaceFiles.length === 0 ? 0 : 90);
    return () => { window.clearTimeout(timer); controller.abort(); };
  }, [trigger?.kind, trigger?.query]);

  useEffect(() => {
    if (!trigger) return;
    const first = trigger.kind === "command" ? filteredCommands[0] && commandValue(filteredCommands[0]) : visibleFiles[0] && fileValue(visibleFiles[0]);
    setSelectedValue(first || "");
  }, [trigger?.kind, trigger?.query, filteredCommands, visibleFiles]);

  useEffect(() => {
    if (trigger?.kind !== "file") setWorkspaceFiles([]);
  }, [cwd, trigger?.kind]);

  useEffect(() => {
    let depth = 0;
    const includesFiles = (event: globalThis.DragEvent) => Array.from(event.dataTransfer?.types ?? []).some((type) => type.toLowerCase() === "files");
    const enter = (event: globalThis.DragEvent) => {
      if (!includesFiles(event)) return;
      event.preventDefault();
      event.stopPropagation();
      depth += 1;
      setDragHasFiles(true);
      setDragDepth(depth);
    };
    const over = (event: globalThis.DragEvent) => {
      if (!includesFiles(event) || !event.dataTransfer) return;
      event.preventDefault();
      event.stopPropagation();
      event.dataTransfer.dropEffect = "copy";
    };
    const leave = (event: globalThis.DragEvent) => {
      if (!includesFiles(event)) return;
      event.preventDefault();
      event.stopPropagation();
      depth = Math.max(0, depth - 1);
      setDragDepth(depth);
      if (depth === 0) setDragHasFiles(false);
    };
    const drop = (event: globalThis.DragEvent) => {
      if (!includesFiles(event)) return;
      event.preventDefault();
      event.stopPropagation();
      depth = 0;
      setDragDepth(0);
      setDragHasFiles(false);
      if (event.dataTransfer?.files.length) readFiles(event.dataTransfer.files, setAttachments, setError);
    };
    document.addEventListener("dragenter", enter, true);
    document.addEventListener("dragover", over, true);
    document.addEventListener("dragleave", leave, true);
    document.addEventListener("drop", drop, true);
    return () => {
      document.removeEventListener("dragenter", enter, true);
      document.removeEventListener("dragover", over, true);
      document.removeEventListener("dragleave", leave, true);
      document.removeEventListener("drop", drop, true);
    };
  }, [setAttachments, setError]);

  useEffect(() => {
    const textarea = inputRef.current;
    if (!textarea) return;
    textarea.style.height = "0px";
    textarea.style.height = `${Math.min(textarea.scrollHeight, 168)}px`;
  }, [draft, inputRef]);

  function updateTrigger(value: string, cursor: number) {
    const nextTrigger = autocompleteTrigger(value, cursor);
    setTrigger(nextTrigger);
    setSelectedValue("");
  }

  function updateDraft(event: ChangeEvent<HTMLTextAreaElement>) {
    setDraft(event.target.value);
    updateTrigger(event.target.value, event.target.selectionStart);
  }

  function completeCommand(command: CommandInfo) {
    if (!trigger) return;
    const hasArguments = draft.slice(trigger.end).trimStart().length > 0;
    const suffix = hasArguments ? "" : " ";
    replaceTrigger(draft, trigger, `/${command.name}${suffix}`, setDraft, inputRef);
    setTrigger(undefined);
  }

  function attachWorkspaceFile(file: WorkspaceFile) {
    setAttachments((items) => appendUniqueAttachments(items, [{ id: `workspace:${file.path}`, path: file.path, name: file.name, size: file.size, mimeType: file.mimeType, source: "workspace" }], setError));
    if (trigger) replaceTrigger(draft, trigger, "", setDraft, inputRef);
    setTrigger(undefined);
  }

  function handleKeyDown(event: KeyboardEvent<HTMLTextAreaElement>) {
    if (popupOpen) {
      if (event.key === "Escape") {
        event.preventDefault();
        setTrigger(undefined);
        return;
      }
      if (event.key === "ArrowDown" || event.key === "ArrowUp") {
        event.preventDefault();
        moveCommandSelection(event.key === "ArrowDown" ? 1 : -1);
        return;
      }
      if (event.key === "Enter" && selectedValue) {
        event.preventDefault();
        const command = filteredCommands.find((item) => commandValue(item) === selectedValue);
        const file = visibleFiles.find((item) => fileValue(item) === selectedValue);
        if (command) completeCommand(command);
        else if (file) attachWorkspaceFile(file);
        return;
      }
    }
    if (event.key === "Enter" && !event.shiftKey && !event.nativeEvent.isComposing) {
      event.preventDefault();
      onSubmit();
    }
  }

  function moveCommandSelection(delta: number) {
    const values = trigger?.kind === "command" ? filteredCommands.map(commandValue) : visibleFiles.map(fileValue);
    if (values.length === 0) return;
    const currentIndex = Math.max(0, values.indexOf(selectedValue));
    const nextIndex = selectedValue ? (currentIndex + delta + values.length) % values.length : delta > 0 ? 0 : values.length - 1;
    setSelectedValue(values[nextIndex]);
    window.requestAnimationFrame(() => document.querySelector<HTMLElement>(`[cmdk-item][data-value="${CSS.escape(values[nextIndex])}"]`)?.scrollIntoView({ block: "nearest" }));
  }

  return (
    <footer
      className={cn("composer-dock", dragActive && "is-dragging")}
      aria-label="Message composer"
    >
      <form onSubmit={(event) => { event.preventDefault(); onSubmit(); }} className="composer-form" aria-busy={sending}>
        {trigger ? (
          <AutocompleteMenu
            trigger={trigger}
            commandGroups={commandGroups}
            files={visibleFiles}
            loading={filesLoading}
            error={filesError}
            selectedValue={selectedValue}
            setSelectedValue={setSelectedValue}
            onCommand={completeCommand}
            onFile={attachWorkspaceFile}
          />
        ) : null}
        <div className="composer">
          {attachments.length > 0 ? (
            <AttachmentGroup className="composer-attachments" aria-label="Attached files">
              {attachments.map((attachment) => <ComposerAttachmentItem key={attachment.id} attachment={attachment} onRemove={() => setAttachments((items) => items.filter((item) => item.id !== attachment.id))} />)}
            </AttachmentGroup>
          ) : null}
          <textarea
            ref={inputRef}
            value={draft}
            onChange={updateDraft}
            onClick={(event) => updateTrigger(event.currentTarget.value, event.currentTarget.selectionStart)}
            onKeyUp={(event) => { if (["ArrowLeft", "ArrowRight", "Home", "End"].includes(event.key)) updateTrigger(event.currentTarget.value, event.currentTarget.selectionStart); }}
            onKeyDown={handleKeyDown}
            onPaste={(event) => handlePaste(event, setAttachments, setError)}
            placeholder="Message Pi, use /commands, or @mention files"
            aria-label="Message Pi"
            aria-autocomplete="list"
            aria-controls={popupOpen ? "composer-autocomplete" : undefined}
            aria-expanded={popupOpen}
            aria-describedby={error ? "composer-error" : "composer-hint"}
            rows={1}
          />
          <input ref={fileInputRef} type="file" multiple hidden onChange={(event) => { readFiles(event.target.files, setAttachments, setError); event.currentTarget.value = ""; }} />
          <div className="composer-toolbar">
            <div className="composer-tools">
              <Button type="button" variant="ghost" size="icon" onClick={() => fileInputRef.current?.click()} aria-label="Attach files" title="Attach files"><Paperclip /></Button>
              <span className="model-chip"><Code2 /> {modelLabel(model)}</span>
            </div>
            <Button type="submit" size="icon" disabled={!canSend || sending} aria-label={sending ? "Sending message" : "Send message"} title="Send message">
              {sending ? <LoaderCircle className="spin" /> : <ArrowUp />}
            </Button>
          </div>
        </div>
        {error ? <p id="composer-error" role="alert" className="composer-error">{error}</p> : null}
        <p id="composer-hint" className="composer-hint"><span>/</span> commands <span>·</span> <span>@</span> files <span>·</span> Enter to send <span>·</span> Shift + Enter for a new line</p>
      </form>
      {dragActive ? (
        <div className="drop-overlay" aria-hidden="true">
          <span className="drop-overlay-icon"><Paperclip /></span>
          <strong>Drop files to attach</strong>
          <span>Images up to 8 MB · text files up to 2 MB</span>
        </div>
      ) : null}
    </footer>
  );
}

function AutocompleteMenu({
  trigger,
  commandGroups,
  files,
  loading,
  error,
  selectedValue,
  setSelectedValue,
  onCommand,
  onFile,
}: {
  trigger: AutocompleteTrigger;
  commandGroups: Array<{ label: string; items: CommandInfo[] }>;
  files: WorkspaceFile[];
  loading: boolean;
  error?: string;
  selectedValue: string;
  setSelectedValue: Dispatch<SetStateAction<string>>;
  onCommand: (command: CommandInfo) => void;
  onFile: (file: WorkspaceFile) => void;
}) {
  const isCommand = trigger.kind === "command";
  const itemsAvailable = isCommand ? commandGroups.some((group) => group.items.length > 0) : files.length > 0;
  return (
    <div id="composer-autocomplete" className="autocomplete-popover" role="presentation">
      <Command value={selectedValue} onValueChange={setSelectedValue} shouldFilter={false} loop>
        <div className="autocomplete-heading">
          <span>{isCommand ? <Terminal /> : <Files />}{isCommand ? "Commands" : "Files"}</span>
          <kbd>esc</kbd>
        </div>
        <CommandList>
          {loading ? <div className="autocomplete-state"><LoaderCircle className="spin" /> Indexing workspace…</div> : null}
          {error ? <div className="autocomplete-state is-error"><CircleAlert /> {error}</div> : null}
          {!loading && !error && !itemsAvailable ? <CommandEmpty>{isCommand ? `No command matches “${trigger.query}”` : `No file matches “${trigger.query}”`}</CommandEmpty> : null}
          {isCommand ? commandGroups.map((group, groupIndex) => (
            <div key={group.label}>
              {groupIndex > 0 ? <CommandSeparator /> : null}
              <CommandGroup heading={group.label}>
                {group.items.map((command) => (
                  <CommandItem key={`${command.source}:${command.name}`} value={commandValue(command)} onSelect={() => onCommand(command)}>
                    <span className="autocomplete-icon"><CommandIcon source={command.source} /></span>
                    <span className="autocomplete-copy"><strong>/{command.name}</strong><small>{command.description || commandSourceLabel(command)}</small></span>
                    <Badge>{command.source === "prompt" ? "prompt" : command.source}</Badge>
                  </CommandItem>
                ))}
              </CommandGroup>
            </div>
          )) : !loading && !error ? (
            <CommandGroup heading={trigger.query ? "Matching files" : "Workspace files"}>
              {files.map((file) => (
                <CommandItem key={file.path} value={fileValue(file)} onSelect={() => onFile(file)}>
                  <span className="autocomplete-icon"><FileTypeIcon file={file} /></span>
                  <span className="autocomplete-copy"><strong>{file.name}</strong><small>{file.directory || "Project root"}</small></span>
                  <span className="autocomplete-size">{formatBytes(file.size)}</span>
                </CommandItem>
              ))}
            </CommandGroup>
          ) : null}
        </CommandList>
        <div className="autocomplete-footer"><span><kbd>↑</kbd><kbd>↓</kbd> navigate</span><span><kbd>↵</kbd> select</span>{!isCommand ? <span>from current project</span> : null}</div>
      </Command>
    </div>
  );
}

function ComposerAttachmentItem({ attachment, onRemove }: { attachment: ComposerAttachment; onRemove: () => void }) {
  const imageSource = attachment.data && attachment.mimeType.startsWith("image/") ? `data:${attachment.mimeType};base64,${attachment.data}` : undefined;
  return (
    <Attachment state="done" className="composer-attachment">
      <AttachmentMedia variant={imageSource ? "image" : "icon"}>
        {imageSource ? <AttachmentImage src={imageSource} alt="" /> : <FileTypeIcon file={attachment} />}
      </AttachmentMedia>
      <AttachmentContent>
        <AttachmentTitle title={attachment.path || attachment.name}>{attachment.name}</AttachmentTitle>
        <AttachmentDescription>{attachment.path ? `${attachment.path} · ` : ""}{formatBytes(attachment.size)}</AttachmentDescription>
      </AttachmentContent>
      <Button type="button" variant="ghost" size="icon" onClick={onRemove} aria-label={`Remove ${attachment.name}`}><X /></Button>
    </Attachment>
  );
}

function CommandIcon({ source }: { source: CommandInfo["source"] }) {
  if (source === "skill") return <Sparkles />;
  if (source === "prompt") return <FilePenLine />;
  return <Terminal />;
}

function FileTypeIcon({ file }: { file: { name: string; mimeType: string } }) {
  if (file.mimeType.startsWith("image/")) return <Image />;
  if (/\.(?:c|cc|cpp|css|go|html|java|js|jsx|lua|php|py|rb|rs|sh|sql|svelte|ts|tsx|vue)$/i.test(file.name)) return <FileCode2 />;
  return <File />;
}

function formatBytes(value?: number) {
  if (value === undefined) return "File";
  if (value < 1024) return `${value} B`;
  if (value < 1024 * 1024) return `${Math.round(value / 1024)} KB`;
  return `${(value / (1024 * 1024)).toFixed(1)} MB`;
}

function readFiles(files: FileList | null, setAttachments: Dispatch<SetStateAction<ComposerAttachment[]>>, setError: Dispatch<SetStateAction<string | undefined>>) {
  if (!files || files.length === 0) return;
  const selected = Array.from(files);
  if (selected.length > 10) {
    setError("Attach at most 10 files at a time.");
    return;
  }
  const invalid = selected.find((file) => !isSupportedLocalFile(file));
  const oversized = selected.find((file) => file.size > (file.type.startsWith("image/") ? 8 : 2) * 1024 * 1024);
  if (invalid) setError(`${invalid.name} is not a supported text or image file.`);
  else if (oversized) setError(`${oversized.name} is larger than ${oversized.type.startsWith("image/") ? "8" : "2"} MB.`);
  else setError(undefined);

  for (const [index, file] of selected.filter((item) => isSupportedLocalFile(item) && item.size <= (item.type.startsWith("image/") ? 8 : 2) * 1024 * 1024).entries()) {
    const reader = new FileReader();
    reader.addEventListener("load", () => {
      const result = typeof reader.result === "string" ? reader.result.split(",")[1] : undefined;
      if (!result) {
        setError(`Could not read ${file.name}.`);
        return;
      }
      const attachment: ComposerAttachment = {
        id: `local:${file.name}:${file.size}:${file.lastModified}:${index}`,
        data: result,
        mimeType: file.type || "text/plain",
        name: file.name,
        size: file.size,
        source: "local",
      };
      setAttachments((items) => appendUniqueAttachments(items, [attachment], setError));
    });
    reader.addEventListener("error", () => setError(`Could not read ${file.name}.`));
    reader.readAsDataURL(file);
  }
}

function handlePaste(event: ClipboardEvent<HTMLTextAreaElement>, setAttachments: Dispatch<SetStateAction<ComposerAttachment[]>>, setError: Dispatch<SetStateAction<string | undefined>>) {
  if (event.clipboardData.files.length === 0) return;
  event.preventDefault();
  readFiles(event.clipboardData.files, setAttachments, setError);
}

function appendUniqueAttachments(current: ComposerAttachment[], next: ComposerAttachment[], setError: Dispatch<SetStateAction<string | undefined>>) {
  const unique = next.filter((attachment) => !current.some((item) => item.id === attachment.id));
  const combined = [...current, ...unique];
  if (combined.length > 10) {
    setError("Attach at most 10 files at a time.");
    return combined.slice(0, 10);
  }
  return combined;
}

function isSupportedLocalFile(file: File) {
  return file.type.startsWith("image/") || file.type.startsWith("text/") || /\.(?:c|cc|conf|cpp|css|csv|env|go|graphql|h|hpp|html|ini|java|js|json|jsx|kt|less|log|lua|md|mdx|mjs|php|properties|py|rb|rs|scss|sh|sql|svelte|svg|toml|ts|tsx|txt|vue|xml|yaml|yml|zsh)$/i.test(file.name);
}

function autocompleteTrigger(value: string, cursor: number): AutocompleteTrigger | undefined {
  const before = value.slice(0, cursor);
  const command = before.match(/^\s*\/([^\s]*)$/);
  if (command) return { kind: "command", start: before.lastIndexOf("/"), end: cursor, query: command[1] };
  const file = before.match(/(?:^|\s)@([^\s@]*)$/);
  if (file) return { kind: "file", start: before.lastIndexOf("@"), end: cursor, query: file[1] };
  return undefined;
}

function replaceTrigger(value: string, trigger: AutocompleteTrigger, replacement: string, setDraft: Dispatch<SetStateAction<string>>, inputRef: React.RefObject<HTMLTextAreaElement | null>) {
  const next = `${value.slice(0, trigger.start)}${replacement}${value.slice(trigger.end)}`;
  const cursor = trigger.start + replacement.length;
  setDraft(next);
  window.requestAnimationFrame(() => {
    inputRef.current?.focus();
    inputRef.current?.setSelectionRange(cursor, cursor);
  });
}

function commandValue(command: CommandInfo) {
  return `command:${command.source}:${command.name}`;
}

function fileValue(file: WorkspaceFile) {
  return `file:${file.path}`;
}

function filterCommands(commands: CommandInfo[], query: string) {
  const normalized = query.toLowerCase();
  return commands
    .filter((command) => !normalized || command.name.toLowerCase().includes(normalized) || command.description?.toLowerCase().includes(normalized))
    .sort((a, b) => {
      const aStarts = a.name.toLowerCase().startsWith(normalized) ? 0 : 1;
      const bStarts = b.name.toLowerCase().startsWith(normalized) ? 0 : 1;
      return aStarts - bStarts || a.name.localeCompare(b.name);
    })
    .slice(0, 36);
}

function filterWorkspaceFiles(files: WorkspaceFile[], query: string) {
  const normalized = query.toLowerCase();
  return files
    .filter((file) => !normalized || file.path.toLowerCase().includes(normalized))
    .sort((a, b) => {
      const aName = a.name.toLowerCase();
      const bName = b.name.toLowerCase();
      const aStarts = aName.startsWith(normalized) ? 0 : 1;
      const bStarts = bName.startsWith(normalized) ? 0 : 1;
      return aStarts - bStarts || a.path.length - b.path.length || a.path.localeCompare(b.path);
    })
    .slice(0, 60);
}

function groupCommands(commands: CommandInfo[]) {
  const groups: Array<{ source: CommandInfo["source"]; label: string }> = [
    { source: "extension", label: "Actions" },
    { source: "prompt", label: "Prompt templates" },
    { source: "skill", label: "Skills" },
  ];
  return groups.map((group) => ({ label: group.label, items: commands.filter((command) => command.source === group.source) })).filter((group) => group.items.length > 0);
}

function commandSourceLabel(command: CommandInfo) {
  const scope = command.sourceInfo.scope === "project" ? "project" : command.sourceInfo.scope === "user" ? "personal" : "session";
  return `${scope} ${command.source}`;
}

function buildDisplayItems(entries: Entry[]): DisplayItem[] {
  const items: DisplayItem[] = [];
  let toolEntries: Entry[] = [];
  const flushTools = () => {
    if (toolEntries.length > 0) items.push({ kind: "tools", entries: toolEntries });
    toolEntries = [];
  };

  for (const entry of entries) {
    const role = entry.message?.role;
    const hasToolCalls = (entry.message?.toolCalls?.length ?? 0) > 0;
    const hasVisibleContent = Boolean(entry.message?.content || entry.summary || entry.message?.images?.length);
    if (role === "toolResult") {
      toolEntries.push(entry);
      continue;
    }
    if (role === "assistant" && hasToolCalls) {
      if (hasVisibleContent) {
        flushTools();
        items.push({ kind: "message", entry });
      }
      toolEntries.push(entry);
      continue;
    }
    flushTools();
    items.push({ kind: "message", entry });
  }
  flushTools();
  return items;
}

function MessageItem({ entry, index, reduceMotion }: { entry: Entry; index: number; reduceMotion: boolean }) {
  const role = entry.message?.role;
  const isUser = role === "user";
  const isCompaction = entry.type === "compaction";
  const content = entry.message?.content || entry.summary || "";
  const images = entry.message?.images ?? [];
  const files = entry.message?.files ?? [];
  const label = isCompaction ? "Context summary" : isUser ? "You" : "Pi";
  const timestamp = entry.message?.timestamp || entry.timestamp;

  return (
    <motion.article
      initial={reduceMotion ? false : { opacity: 0, transform: "translateY(4px)" }}
      animate={{ opacity: 1, transform: "translateY(0)" }}
      transition={{ duration: 0.16, delay: reduceMotion ? 0 : Math.min(index * 0.02, 0.1), ease: [0.23, 1, 0.32, 1] }}
      className={cn("message", isUser && "message-user", isCompaction && "message-compaction")}
      aria-label={`${label} message`}
    >
      <div className="message-avatar" aria-hidden="true">{isUser ? <UserRound /> : "π"}</div>
      <div className="message-body">
        <div className="message-meta">
          <strong>{label}</strong>
          {timestamp ? <time dateTime={timestampValue(timestamp)}>{timeLabel(timestamp)}</time> : null}
        </div>
        <div className="message-content">
          {content ? <MarkdownContent content={content} /> : images.length === 0 && files.length === 0 ? <p className="muted">No text content</p> : null}
          {files.length > 0 ? (
            <AttachmentGroup className="message-files" aria-label="Attached files">
              {files.map((file, fileIndex) => (
                <Attachment key={`${file.path || file.name}-${fileIndex}`} state="done">
                  <AttachmentMedia><FileTypeIcon file={{ name: file.name, mimeType: file.mimeType || "text/plain" }} /></AttachmentMedia>
                  <AttachmentContent><AttachmentTitle>{file.name}</AttachmentTitle><AttachmentDescription>{file.path || "Attached file"}</AttachmentDescription></AttachmentContent>
                </Attachment>
              ))}
            </AttachmentGroup>
          ) : null}
          {images.length > 0 ? (
            <AttachmentGroup className="message-attachments">
              {images.map((image, imageIndex) => (
                <Attachment key={`${image.mimeType}-${imageIndex}`} state="done" orientation="vertical" className="message-attachment">
                  <AttachmentMedia variant="image"><AttachmentImage src={`data:${image.mimeType};base64,${image.data}`} alt={`Attached image ${imageIndex + 1}`} loading="lazy" /></AttachmentMedia>
                  <AttachmentContent><AttachmentTitle>Attached image</AttachmentTitle><AttachmentDescription>{image.mimeType}</AttachmentDescription></AttachmentContent>
                </Attachment>
              ))}
            </AttachmentGroup>
          ) : null}
        </div>
      </div>
    </motion.article>
  );
}

function prettyToolOutput(content: string): CodePreview {
  const fenced = content.match(/^\s*```([^\n`]*)\n([\s\S]*?)\n?```\s*$/);
  if (fenced) {
    const rawLanguage = fenced[1].trim().toLowerCase();
    return { code: fenced[2], language: languageAliases[rawLanguage] || rawLanguage || "text" };
  }
  try {
    return { code: JSON.stringify(JSON.parse(content), null, 2), language: "json" };
  } catch {
    return { code: content, language: "text" };
  }
}

function getArgumentRecord(value: unknown): Record<string, unknown> | undefined {
  return value && typeof value === "object" && !Array.isArray(value) ? value as Record<string, unknown> : undefined;
}

function stringValue(value: unknown) {
  return typeof value === "string" ? value : undefined;
}

function shortCommand(command: string) {
  const firstLine = command.trim().split("\n")[0] || "Run command";
  return firstLine.length > 90 ? `${firstLine.slice(0, 87)}…` : firstLine;
}

function pathName(path?: string) {
  if (!path) return undefined;
  return path.split("/").filter(Boolean).at(-1) || path;
}

function humanizeToolName(name: string) {
  return name.replace(/^functions\./, "").replaceAll("_", " ").replace(/\b\w/g, (letter) => letter.toUpperCase());
}

function describeTool(call: ToolCall): { Icon: LucideIcon; action: string; subject?: string; meta?: string; argumentsBody?: ReactNode } {
  const name = call.name.replace(/^functions\./, "");
  const args = getArgumentRecord(call.arguments);
  const path = stringValue(args?.path);
  const command = stringValue(args?.command);
  const query = stringValue(args?.query) || (Array.isArray(args?.queries) ? stringValue(args.queries[0]) : undefined);
  const pattern = stringValue(args?.pattern);

  if (name === "read") return { Icon: FileSearch, action: "Read", subject: pathName(path), meta: path, argumentsBody: path ? <ToolFields rows={[{ label: "File", value: path }, { label: "Range", value: rangeLabel(args) }]} /> : undefined };
  if (name === "write") return { Icon: FilePenLine, action: "Wrote", subject: pathName(path), meta: path, argumentsBody: <ToolFields rows={[{ label: "File", value: path }, { label: "Content", value: `${stringValue(args?.content)?.length ?? 0} characters` }]} /> };
  if (name === "edit") return { Icon: FilePenLine, action: "Edited", subject: pathName(path), meta: path, argumentsBody: <ToolFields rows={[{ label: "File", value: path }, { label: "Changes", value: Array.isArray(args?.edits) ? `${args.edits.length} replacement${args.edits.length === 1 ? "" : "s"}` : undefined }]} /> };
  if (name === "bash") return { Icon: Terminal, action: "Ran command", subject: command ? shortCommand(command) : undefined, argumentsBody: command ? <ShikiCodeBlock code={command} language="shellscript" /> : undefined };
  if (name === "ffgrep") return { Icon: Search, action: "Searched code", subject: pattern, meta: stringValue(args?.path), argumentsBody: <ToolFields rows={[{ label: "Query", value: pattern }, { label: "In", value: stringValue(args?.path) || "workspace" }]} /> };
  if (name === "fffind") return { Icon: FolderSearch, action: "Found files", subject: pattern, meta: stringValue(args?.path), argumentsBody: <ToolFields rows={[{ label: "Pattern", value: pattern }, { label: "In", value: stringValue(args?.path) || "workspace" }]} /> };
  if (name === "web_search") return { Icon: Globe2, action: "Searched the web", subject: query, argumentsBody: query ? <ToolFields rows={[{ label: "Query", value: query }]} /> : undefined };
  if (name === "fetch_content") return { Icon: Globe2, action: "Fetched content", subject: stringValue(args?.url) || (Array.isArray(args?.urls) ? stringValue(args.urls[0]) : undefined), argumentsBody: <ToolFields rows={[{ label: "URL", value: stringValue(args?.url) || (Array.isArray(args?.urls) ? args.urls.join("\n") : undefined) }]} /> };
  if (name === "multi_tool_use.parallel") return { Icon: Braces, action: "Ran tools in parallel", subject: Array.isArray(args?.tool_uses) ? `${args.tool_uses.length} calls` : undefined, argumentsBody: <ToolFields rows={(Array.isArray(args?.tool_uses) ? args.tool_uses : []).flatMap((value) => { const item = getArgumentRecord(value); return item ? [{ label: "Tool", value: stringValue(item.recipient_name) }] : []; })} /> };
  return { Icon: Wrench, action: humanizeToolName(name), argumentsBody: <ToolFields rows={friendlyArgumentRows(args)} /> };
}

function rangeLabel(args?: Record<string, unknown>) {
  const offset = typeof args?.offset === "number" ? args.offset : undefined;
  const limit = typeof args?.limit === "number" ? args.limit : undefined;
  if (offset === undefined && limit === undefined) return undefined;
  return `${offset ?? 1}${limit ? `–${(offset ?? 1) + limit - 1}` : "+"}`;
}

function friendlyArgumentRows(args?: Record<string, unknown>): Array<{ label: string; value?: string }> {
  if (!args) return [];
  return Object.entries(args).slice(0, 8).map(([key, value]) => ({
    label: key.replaceAll("_", " "),
    value: typeof value === "string" ? value : typeof value === "number" || typeof value === "boolean" ? String(value) : Array.isArray(value) ? `${value.length} items` : value ? "Configured" : "None",
  }));
}

function ToolFields({ rows }: { rows: Array<{ label: string; value?: string }> }) {
  const visible = rows.filter((row) => row.value);
  if (visible.length === 0) return null;
  return (
    <dl className="tool-fields">
      {visible.map((row, index) => <div key={`${row.label}-${index}`}><dt>{row.label}</dt><dd>{row.value}</dd></div>)}
    </dl>
  );
}

function buildToolRows(entries: Entry[], toolCallsById: Map<string, ToolCall>): ToolRow[] {
  const calls = entries.flatMap((entry, entryIndex) => (entry.message?.toolCalls ?? []).map((call, callIndex) => ({ call, key: call.id || `${entryIndex}-${call.name}-${callIndex}` })));
  const results = entries.filter((entry) => entry.message?.role === "toolResult");
  const rows: ToolRow[] = calls.map(({ call, key }) => ({ key, call }));
  const matchedResults = new Set<Entry>();

  for (const result of results) {
    const toolCallId = result.message?.toolCallId;
    let row = toolCallId ? rows.find((candidate) => candidate.call?.id === toolCallId) : undefined;
    if (!row && toolCallId) {
      const linkedCall = toolCallsById.get(toolCallId);
      if (linkedCall) {
        row = { key: `linked-${toolCallId}`, call: linkedCall };
        rows.push(row);
      }
    }
    if (row) {
      row.result = result;
      matchedResults.add(result);
    }
  }
  for (const [index, result] of results.filter((item) => !matchedResults.has(item)).entries()) {
    rows.push({ key: result.id || `orphan-${index}`, result });
  }
  return rows;
}

function ToolActivity({ entries, index, reduceMotion, toolCallsById }: { entries: Entry[]; index: number; reduceMotion: boolean; toolCallsById: Map<string, ToolCall> }) {
  const [open, setOpen] = useState(false);
  const rows = buildToolRows(entries, toolCallsById);
  const completed = rows.filter((row) => row.result && !row.result.message?.isError).length;
  const errors = rows.filter((row) => row.result?.message?.isError).length;
  const waiting = rows.length - completed - errors;
  const timestamps = entries.map((entry) => entry.message?.timestamp || entry.timestamp).filter(Boolean);
  const timestamp = timestamps.at(-1);
  const status = errors > 0 ? `${errors} failed` : waiting > 0 ? `${waiting} running` : "Complete";

  return (
    <motion.details
      initial={reduceMotion ? false : { opacity: 0 }}
      animate={{ opacity: 1 }}
      transition={{ duration: 0.14, delay: Math.min(index * 0.02, 0.08) }}
      className="tool-activity"
      open={open}
      onToggle={(event) => setOpen(event.currentTarget.open)}
    >
      <summary>
        <span className="tool-summary-icon" aria-hidden="true"><Wrench /></span>
        <span className="tool-summary-copy"><strong>{rows.length} tool {rows.length === 1 ? "call" : "calls"}</strong><span>{status}</span></span>
        {timestamp ? <time dateTime={timestampValue(timestamp)}>{timeLabel(timestamp)}</time> : null}
        <ChevronDown className="tool-summary-chevron" aria-hidden="true" />
      </summary>
      <div className="tool-list">
        {rows.map((row) => <ToolCallRow key={row.key} row={row} />)}
      </div>
    </motion.details>
  );
}

function ToolCallRow({ row }: { row: ToolRow }) {
  const [open, setOpen] = useState(false);
  const description = row.call ? describeTool(row.call) : { Icon: Terminal, action: "Tool output" };
  const { Icon } = description;
  const resultText = row.result?.message?.content || row.result?.summary || "";
  const resultPreview = resultText ? prettyToolOutput(resultText) : undefined;
  const isError = Boolean(row.result?.message?.isError);
  const isWaiting = Boolean(row.call && !row.result);

  return (
    <details className={cn("tool-row", isError && "is-error")} open={open} onToggle={(event) => setOpen(event.currentTarget.open)}>
      <summary>
        <span className="tool-row-icon" aria-hidden="true"><Icon /></span>
        <span className="tool-row-copy"><strong>{description.action}</strong>{description.subject ? <span title={description.meta}>{description.subject}</span> : null}</span>
        <ToolStatus error={isError} waiting={isWaiting} />
        <ChevronDown className="tool-row-chevron" aria-hidden="true" />
      </summary>
      <div className="tool-detail">
        {description.meta && description.meta !== description.subject ? <p className="tool-path">{description.meta}</p> : null}
        {description.argumentsBody ? <section aria-label="Tool input"><h4>Input</h4>{description.argumentsBody}</section> : null}
        {row.result ? <section aria-label="Tool output"><h4>Output</h4>{resultPreview ? <ShikiCodeBlock {...resultPreview} /> : <p className="muted">No output</p>}</section> : null}
      </div>
    </details>
  );
}

function ToolStatus({ error, waiting }: { error: boolean; waiting: boolean }) {
  if (error) return <span className="tool-status is-error"><CircleAlert /> Failed</span>;
  if (waiting) return <span className="tool-status"><LoaderCircle className="spin" /> Running</span>;
  return <span className="tool-status"><CheckCircle2 /> Done</span>;
}

function ContextPanel({ context, onClose }: { context: Snapshot["context"]; onClose: () => void }) {
  const percent = context.percent ?? (context.contextWindow && context.tokens ? context.tokens / context.contextWindow * 100 : null);
  const closeRef = useRef<HTMLButtonElement>(null);
  useEffect(() => { closeRef.current?.focus(); }, []);

  return (
    <aside id="session-context" aria-labelledby="context-heading" tabIndex={-1} onKeyDown={(event) => { if (event.key === "Escape") onClose(); }} className="context-panel">
      <div className="context-heading-row">
        <div><h2 id="context-heading">Context</h2><p>What Pi can currently see</p></div>
        <Button ref={closeRef} variant="ghost" size="icon" onClick={onClose} aria-label="Close context window"><X /></Button>
      </div>
      <div className="context-usage">
        <div className="context-usage-row"><span>Usage</span><strong>{context.tokens == null ? "—" : formatTokens(context.tokens)} <small>/ {formatTokens(context.contextWindow)}</small></strong></div>
        <div role="progressbar" aria-label="Context window usage" aria-valuemin={0} aria-valuemax={100} aria-valuenow={percent == null ? undefined : Math.round(percent)} aria-valuetext={percent == null ? "Waiting for usage" : `${Math.round(percent)}% used`} className="usage-track"><span style={{ width: `${Math.min(percent ?? 0, 100)}%` }} /></div>
        <p>{percent == null ? "Waiting for usage" : `${Math.round(percent)}% used`} <span>·</span> {context.entries.length} items</p>
      </div>
      <div className="context-content">
        {context.systemPrompt ? (
          <details className="system-prompt">
            <summary><Terminal /> System prompt <ChevronDown /></summary>
            <pre>{context.systemPrompt}</pre>
          </details>
        ) : null}
        <h3>Included in context</h3>
        <div className="context-list">
          {context.entries.length === 0 ? <p className="muted">Context is not available yet.</p> : context.entries.map((item, index) => (
            <div key={item.id || `${item.type}-${index}`} className="context-item">
              <div><span aria-hidden="true" className="context-item-dot" /><strong>{item.label}</strong><Badge>{item.role || item.type}</Badge></div>
              {item.preview ? <p>{item.preview}</p> : null}
            </div>
          ))}
        </div>
      </div>
    </aside>
  );
}

createRoot(document.getElementById("root")!).render(<StrictMode><App /></StrictMode>);
