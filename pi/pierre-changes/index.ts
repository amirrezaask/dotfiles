import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { CONFIG_DIR_NAME } from "@earendil-works/pi-coding-agent";
import { createHash } from "node:crypto";
import { mkdir, readdir, readFile, stat, writeFile } from "node:fs/promises";
import { createReadStream } from "node:fs";
import { basename, join, relative, resolve, sep } from "node:path";

const CUSTOM_TYPE = "pierre-changes";
const MAX_FILES = 10_000;
const MAX_TEXT_BYTES = 2 * 1024 * 1024;
const MAX_TOTAL_TEXT_BYTES = 50 * 1024 * 1024;
const REPORT_DIR_NAME = "pierre-changes";
const CLIENT_ENTRY = require.resolve("./client.ts");

const IGNORED_DIRECTORIES = new Set([
	".git",
	"node_modules",
	".next",
	".turbo",
	".cache",
	"__pycache__",
	".venv",
	"venv",
]);

type Snapshot = {
	path: string;
	contents?: string;
	size: number;
	hash: string;
	binary: boolean;
	mode: number;
};

type FileChange = {
	path: string;
	previousPath?: string;
	kind: "added" | "modified" | "deleted" | "renamed";
	before: Snapshot | null;
	after: Snapshot | null;
};

type TurnRecord = {
	turnIndex: number;
	startedAt: string;
	completedAt: string;
	changes: FileChange[];
};

type PersistedSnapshot = Omit<Snapshot, "path">;
type PersistedChange = Omit<FileChange, "before" | "after"> & {
	before: PersistedSnapshot | null;
	after: PersistedSnapshot | null;
};

type PersistedTurn = Omit<TurnRecord, "changes"> & { changes: PersistedChange[] };


let turnStart: Map<string, Snapshot> | undefined;
let activeTurnStartedAt: string | undefined;
let turns: TurnRecord[] = [];
let bundlePromise: Promise<string> | undefined;
let reportDirty = false;

function isInside(parent: string, child: string): boolean {
	const parentWithSeparator = parent.endsWith(sep) ? parent : `${parent}${sep}`;
	return child === parent || child.startsWith(parentWithSeparator);
}

function toRelativePath(cwd: string, absolutePath: string): string | undefined {
	const path = relative(cwd, absolutePath);
	if (!path || path.startsWith("..") || path.includes(`${sep}..${sep}`)) return undefined;
	return path.split(sep).join("/");
}

function isGeneratedReportPath(path: string): boolean {
	const reportRoot = `${CONFIG_DIR_NAME}/${REPORT_DIR_NAME}`;
	return path === reportRoot || path.startsWith(`${reportRoot}/`);
}

async function gitFiles(cwd: string, pi: ExtensionAPI): Promise<string[] | undefined> {
	const rootResult = await pi.exec("git", ["-C", cwd, "rev-parse", "--show-toplevel"], {
		timeout: 2_000,
	});
	const repoRoot = rootResult.stdout.trim();
	if (!repoRoot || rootResult.code !== 0) return undefined;

	const filesResult = await pi.exec(
		"git",
		["-C", cwd, "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--full-name"],
		{ timeout: 5_000 },
	);
	if (filesResult.code !== 0) return undefined;

	const paths: string[] = [];
	for (const file of filesResult.stdout.split("\0")) {
		if (!file) continue;
		const absolutePath = resolve(repoRoot, file);
		const relativePath = isInside(cwd, absolutePath) ? relative(cwd, absolutePath) : undefined;
		const normalizedPath = relativePath?.split(sep).join("/");
		if (
			normalizedPath &&
			!isGeneratedReportPath(normalizedPath) &&
			!normalizedPath.split("/").some((segment) => IGNORED_DIRECTORIES.has(segment))
		) {
			paths.push(normalizedPath);
		}
		if (paths.length >= MAX_FILES) break;
	}
	return paths;
}

async function walkFiles(cwd: string): Promise<string[]> {
	const paths: string[] = [];

	async function walk(directory: string): Promise<void> {
		if (paths.length >= MAX_FILES) return;
		let entries;
		try {
			entries = await readdir(directory, { withFileTypes: true });
		} catch {
			return;
		}

		for (const entry of entries) {
			if (paths.length >= MAX_FILES) return;
			const absolutePath = join(directory, entry.name);
			const relativePath = toRelativePath(cwd, absolutePath);
			if (
				entry.isDirectory() &&
				(IGNORED_DIRECTORIES.has(entry.name) || (relativePath != null && isGeneratedReportPath(relativePath)))
			) {
				continue;
			}
			if (entry.isDirectory()) {
				await walk(absolutePath);
			} else if (entry.isFile() || entry.isSymbolicLink()) {
				if (relativePath && !isGeneratedReportPath(relativePath)) paths.push(relativePath);
			}
		}
	}

	await walk(cwd);
	return paths;
}

function isBinary(buffer: Buffer): boolean {
	const sampleLength = Math.min(buffer.length, 8_192);
	for (let index = 0; index < sampleLength; index += 1) {
		if (buffer[index] === 0) return true;
	}
	return false;
}

async function hashLargeFile(absolutePath: string): Promise<{ hash: string; binary: boolean }> {
	const hash = createHash("sha256");
	let binary = false;
	let inspected = 0;
	const stream = createReadStream(absolutePath);
	for await (const chunk of stream) {
		const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
		hash.update(buffer);
		if (inspected < 8_192) {
			const sample = buffer.subarray(0, 8_192 - inspected);
			if (isBinary(sample)) binary = true;
			inspected += sample.length;
		}
	}
	return { hash: hash.digest("hex"), binary };
}

async function snapshotFile(cwd: string, path: string, textBytes: { value: number }): Promise<Snapshot | undefined> {
	const absolutePath = resolve(cwd, path);
	try {
		const fileStat = await stat(absolutePath);
		if (!fileStat.isFile()) return undefined;
		const mode = fileStat.mode & 0o7777;

		if (fileStat.size <= MAX_TEXT_BYTES && textBytes.value + fileStat.size <= MAX_TOTAL_TEXT_BYTES) {
			const buffer = await readFile(absolutePath);
			const binary = isBinary(buffer);
			textBytes.value += fileStat.size;
			return {
				path,
				contents: binary ? undefined : buffer.toString("utf8"),
				size: fileStat.size,
				hash: createHash("sha256").update(buffer).digest("hex"),
				binary,
				mode,
			};
		}

		const result = await hashLargeFile(absolutePath);
		return { path, size: fileStat.size, mode, ...result };
	} catch {
		return undefined;
	}
}

async function snapshotWorkspace(cwd: string, pi: ExtensionAPI): Promise<Map<string, Snapshot>> {
	const files = (await gitFiles(cwd, pi)) ?? (await walkFiles(cwd));
	const snapshot = new Map<string, Snapshot>();
	const textBytes = { value: 0 };
	for (const path of files) {
		const file = await snapshotFile(cwd, path, textBytes);
		if (file) snapshot.set(path.split(sep).join("/"), file);
	}
	return snapshot;
}

function sameSnapshot(before: Snapshot | undefined, after: Snapshot | undefined): boolean {
	if (!before || !after) return before === after;
	return before.hash === after.hash && before.mode === after.mode;
}

function classifyChange(before: Snapshot | undefined, after: Snapshot | undefined): FileChange["kind"] {
	if (!before) return "added";
	if (!after) return "deleted";
	return "modified";
}

function detectRenames(changes: FileChange[]): FileChange[] {
	const additions = changes.filter((change) => change.kind === "added" && change.after);
	const deletions = changes.filter((change) => change.kind === "deleted" && change.before);
	const consumed = new Set<FileChange>();
	const renamed = new Set<FileChange>();

	for (const deletion of deletions) {
		const match = additions.find(
			(addition) =>
				!consumed.has(addition) &&
				addition.after?.hash === deletion.before?.hash &&
				addition.after?.size === deletion.before?.size,
		);
		if (!match) continue;
		consumed.add(match);
		renamed.add(deletion);
		renamed.add(match);
		match.kind = "renamed";
		match.previousPath = deletion.path;
	}

	return changes.filter((change) => !renamed.has(change) || change.kind === "renamed");
}

function diffSnapshots(before: Map<string, Snapshot>, after: Map<string, Snapshot>): FileChange[] {
	const paths = new Set([...before.keys(), ...after.keys()]);
	const changes: FileChange[] = [];
	for (const path of paths) {
		const oldFile = before.get(path);
		const newFile = after.get(path);
		if (sameSnapshot(oldFile, newFile)) continue;
		changes.push({
			path,
			kind: classifyChange(oldFile, newFile),
			before: oldFile ?? null,
			after: newFile ?? null,
		});
	}
	return detectRenames(changes).sort((left, right) => left.path.localeCompare(right.path));
}

function persistedSnapshot(snapshot: Snapshot | null): PersistedSnapshot | null {
	if (!snapshot) return null;
	return {
		contents: snapshot.contents,
		size: snapshot.size,
		hash: snapshot.hash,
		binary: snapshot.binary,
		mode: snapshot.mode,
	};
}

function persistedChange(change: FileChange): PersistedChange {
	return {
		path: change.path,
		previousPath: change.previousPath,
		kind: change.kind,
		before: persistedSnapshot(change.before),
		after: persistedSnapshot(change.after),
	};
}

function restoreSnapshot(path: string, snapshot: PersistedSnapshot | null): Snapshot | null {
	if (!snapshot) return null;
	return { path, ...snapshot };
}

function restoreChange(change: PersistedChange): FileChange {
	return {
		path: change.path,
		previousPath: change.previousPath,
		kind: change.kind,
		before: restoreSnapshot(change.previousPath ?? change.path, change.before),
		after: restoreSnapshot(change.path, change.after),
	};
}

function restoreTurns(ctx: ExtensionContext): TurnRecord[] {
	const restored: TurnRecord[] = [];
	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type !== "custom" || entry.customType !== CUSTOM_TYPE) continue;
		const data = entry.data as { version?: number; kind?: string; turn?: PersistedTurn } | undefined;
		if (data?.version !== 1 || data.kind !== "turn" || !data.turn) continue;
		restored.push({
			turnIndex: data.turn.turnIndex,
			startedAt: data.turn.startedAt,
			completedAt: data.turn.completedAt,
			changes: data.turn.changes.map(restoreChange),
		});
	}
	return restored;
}

function aggregateChanges(records: TurnRecord[]): FileChange[] {
	const aggregate = new Map<string, FileChange>();
	for (const record of records) {
		for (const change of record.changes) {
			const existing = aggregate.get(change.path);
			if (!existing) {
				aggregate.set(change.path, {
					...change,
					before: change.before,
					after: change.after,
				});
				continue;
			}
			existing.after = change.after;
			existing.kind = classifyChange(existing.before ?? undefined, existing.after ?? undefined);
		}
	}
	return [...aggregate.values()]
		.filter((change) => !sameSnapshot(change.before ?? undefined, change.after ?? undefined))
		.sort((left, right) => left.path.localeCompare(right.path));
}

async function buildClientBundle(): Promise<string> {
	const esbuild = await import("esbuild");
	const result = await esbuild.build({
		entryPoints: [CLIENT_ENTRY],
		bundle: true,
		format: "iife",
		platform: "browser",
		target: "es2022",
		minify: true,
		legalComments: "none",
		write: false,
	});
	const output = result.outputFiles?.[0]?.text;
	if (!output) throw new Error("Pierre report bundle was empty");
	return output;
}

function safeJson(value: unknown): string {
	return JSON.stringify(value)
		.replaceAll("<", "\\u003c")
		.replaceAll(">", "\\u003e")
		.replaceAll("&", "\\u0026")
		.replaceAll("\u2028", "\\u2028")
		.replaceAll("\u2029", "\\u2029");
}

function sessionTitle(pi: ExtensionAPI, ctx: ExtensionContext): string {
	const namedSession = pi.getSessionName() ?? ctx.sessionManager.getSessionName();
	if (namedSession?.trim()) return namedSession.trim();

	for (const entry of ctx.sessionManager.getBranch()) {
		if (entry.type !== "message" || entry.message.role !== "user") continue;
		const content = entry.message.content;
		const text = typeof content === "string"
			? content
			: content
				.filter((block): block is { type: "text"; text: string } => block.type === "text")
				.map((block) => block.text)
				.join(" ");
		const firstLine = text.split(/\r?\n/).find((line) => line.trim())?.trim();
		if (firstLine) return firstLine.length > 80 ? `${firstLine.slice(0, 77)}…` : firstLine;
	}

	return basename(ctx.cwd) || "Untitled session";
}

function reportData(pi: ExtensionAPI, ctx: ExtensionContext): { cwd: string; sessionId: string; sessionTitle: string; generatedAt: string; turns: PersistedTurn[]; files: PersistedChange[] } {
	return {
		cwd: ctx.cwd,
		sessionId: ctx.sessionManager.getSessionId(),
		sessionTitle: sessionTitle(pi, ctx),
		generatedAt: new Date().toISOString(),
		turns: turns.map((turn) => ({
			turnIndex: turn.turnIndex,
			startedAt: turn.startedAt,
			completedAt: turn.completedAt,
			changes: turn.changes.map(persistedChange),
		})),
		files: aggregateChanges(turns).map(persistedChange),
	};
}

function htmlDocument(data: ReturnType<typeof reportData>, bundle: string): string {
	const bundleForHtml = bundle.replaceAll("</script", "<\\/script");
	return `<!doctype html>
<html lang="en">
<head>
	<meta charset="utf-8">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<meta name="color-scheme" content="dark">
	<title>${escapeHtml(data.sessionTitle)}</title>
	<style>
		:root { color-scheme: dark; font-family: Geist, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; background: #0b0d10; color: #e7e9ee; }
		* { box-sizing: border-box; }
		body { margin: 0; min-width: 760px; height: 100vh; overflow: hidden; background: radial-gradient(circle at 70% -20%, #1c2738 0, transparent 38%), #0b0d10; }
		button, select { font: inherit; }
		.app { display: grid; grid-template-rows: 72px minmax(0, 1fr); height: 100vh; }
		.topbar { display: flex; align-items: center; justify-content: space-between; gap: 24px; padding: 0 28px; border-bottom: 1px solid #222832; background: rgba(11, 13, 16, .78); backdrop-filter: blur(18px); }
		.brand { display: flex; align-items: center; gap: 13px; min-width: 0; }
		.mark { display: grid; place-items: center; width: 34px; height: 34px; border: 1px solid #4f8cff; border-radius: 10px; color: #9dc1ff; background: #15243d; box-shadow: 0 0 28px rgba(67, 131, 255, .2); font-weight: 700; }
		h1 { margin: 0; font-size: 15px; letter-spacing: -.02em; }
		.subtitle { margin-top: 4px; color: #7e8796; font: 11px/1.2 "Geist Mono", monospace; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 42vw; }
		.controls { display: flex; align-items: center; gap: 14px; }
		.stats { display: flex; gap: 12px; color: #aab2c0; font: 11px "Geist Mono", monospace; }
		.stats span { padding: 6px 9px; border: 1px solid #27303c; border-radius: 7px; background: #12161d; }
		.stats .plus { color: #62d991; }
		.stats .minus { color: #ff7d87; }
		select { color: #d9deea; border: 1px solid #364253; border-radius: 7px; padding: 8px 30px 8px 10px; background: #151b24; outline: none; }
		select:focus { border-color: #6b9fff; box-shadow: 0 0 0 3px rgba(91, 145, 255, .18); }
		.workspace { display: grid; grid-template-columns: 292px minmax(0, 1fr); min-height: 0; }
		.sidebar { display: grid; grid-template-rows: 53px minmax(0, 1fr); min-height: 0; border-right: 1px solid #222832; background: rgba(13, 16, 21, .92); }
		.sidebar-head { display: flex; align-items: center; justify-content: space-between; padding: 0 17px; border-bottom: 1px solid #202630; color: #aeb7c7; font-size: 12px; font-weight: 600; }
		#file-count { color: #68758a; font: 10px "Geist Mono", monospace; }
		#file-tree { min-height: 0; height: 100%; padding: 8px 6px; }
		.main { display: grid; grid-template-rows: 58px minmax(0, 1fr); min-width: 0; min-height: 0; }
		.file-toolbar { display: flex; align-items: center; gap: 13px; padding: 0 24px; border-bottom: 1px solid #222832; background: rgba(15, 18, 24, .68); }
		#selected-file { color: #edf0f7; font: 600 12px "Geist Mono", monospace; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
		#selected-kind { padding: 3px 7px; border: 1px solid #315a9e; border-radius: 999px; color: #a5c5ff; background: #14243d; font: 10px "Geist Mono", monospace; }
		#selected-meta { margin-left: auto; color: #6f7b8e; font: 10px "Geist Mono", monospace; }
		#file-diff { min-height: 0; overflow: auto; background: #10141a; }
		.pierre-diff-wrapper { min-height: 100%; }
		.unavailable { max-width: 520px; margin: 72px auto; padding: 24px; border: 1px solid #2e3744; border-radius: 12px; background: #151a22; color: #c6cedb; }
		.unavailable strong { font-size: 14px; }
		.unavailable p { margin: 9px 0 0; color: #7c8798; font: 12px/1.5 "Geist Mono", monospace; }
		.empty { display: grid; place-items: center; height: 100%; color: #748095; text-align: center; }
		.empty[hidden] { display: none; }
		.empty-inner { max-width: 310px; }
		.empty-icon { margin-bottom: 14px; color: #5f9aff; font-size: 28px; }
		.empty h2 { margin: 0 0 7px; color: #cbd3e2; font-size: 15px; }
		.empty p { margin: 0; font: 11px/1.6 "Geist Mono", monospace; }
		footer { position: fixed; right: 18px; bottom: 12px; color: #4e596b; font: 10px "Geist Mono", monospace; pointer-events: none; }
		@media (max-width: 980px) { .topbar { padding: 0 16px; } .stats { display: none; } .workspace { grid-template-columns: 250px minmax(0, 1fr); } }
	</style>
</head>
<body>
	<div class="app">
		<header class="topbar">
			<div class="brand">
				<div class="mark">Δ</div>
				<div><h1>${escapeHtml(data.sessionTitle)}</h1><div class="subtitle" id="project"></div></div>
			</div>
			<div class="controls">
				<div class="stats"><span id="file-count">0 files</span><span id="turn-count">0 turns</span><span class="plus" id="additions">0 added</span><span class="minus" id="deletions">0 deleted</span></div>
				<select id="scope" aria-label="Change scope"><option value="session">Session snapshot</option></select>
			</div>
		</header>
		<div class="workspace">
			<aside class="sidebar"><div class="sidebar-head"><span>Changed files</span><span id="sidebar-file-count">0 files</span></div><div id="file-tree"></div></aside>
			<main class="main">
				<div class="file-toolbar"><span id="selected-file">No file changes</span><span id="selected-kind"></span><span id="selected-meta"></span></div>
				<div id="file-diff"><div class="empty" id="empty-state"><div class="empty-inner"><div class="empty-icon">✦</div><h2>No net file changes</h2><p>Completed turns are recorded here. Choose a turn above to inspect its changes.</p></div></div></div>
			</main>
		</div>
	</div>
	<footer>Generated <span id="generated"></span></footer>
	<script>window.__PI_PIERRE_CHANGES__ = ${safeJson(data)};</script>
	<script>${bundleForHtml}</script>
</body>
</html>`;
}

function escapeHtml(value: string): string {
	return value.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
}

async function writeReport(pi: ExtensionAPI, ctx: ExtensionContext): Promise<string> {
	if (!bundlePromise) bundlePromise = buildClientBundle();
	const [bundle] = await Promise.all([bundlePromise]);
	const data = reportData(pi, ctx);
	const reportDirectory = join(ctx.cwd, CONFIG_DIR_NAME, REPORT_DIR_NAME, data.sessionId);
	await mkdir(reportDirectory, { recursive: true });
	const reportPath = join(reportDirectory, "latest.html");
	await writeFile(reportPath, htmlDocument(data, bundle), "utf8");
	return reportPath;
}

async function openReport(pi: ExtensionAPI, ctx: ExtensionContext, reportPath: string): Promise<void> {
	if (ctx.mode !== "tui" || process.env.PI_PIERRE_CHANGES_AUTO_OPEN === "0") return;
	const command = process.platform === "darwin" ? "open" : process.platform === "win32" ? "cmd" : "xdg-open";
	const args = process.platform === "win32" ? ["/c", "start", "", reportPath] : [reportPath];
	const result = await pi.exec(command, args, { timeout: 5_000 });
	if (result.code !== 0) ctx.ui.notify(`Could not open Pierre report: ${result.stderr || result.stdout}`, "warning");
}

async function showReport(pi: ExtensionAPI, ctx: ExtensionContext, open: boolean): Promise<void> {
	if (turns.length === 0) {
		ctx.ui.notify("No completed turns with file changes yet.", "info");
		return;
	}
	try {
		const reportPath = await writeReport(pi, ctx);
		if (open) await openReport(pi, ctx, reportPath);
		ctx.ui.notify(`Pierre report: ${reportPath}`, "info");
	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		ctx.ui.notify(`Pierre changes report failed: ${message}`, "error");
	}
}

export default function pierreChanges(pi: ExtensionAPI): void {
	pi.on("session_start", async (_event, ctx) => {
		turns = restoreTurns(ctx);
		turnStart = await snapshotWorkspace(ctx.cwd, pi);
		activeTurnStartedAt = undefined;
		reportDirty = false;
	});

	pi.on("session_tree", async (_event, ctx) => {
		turns = restoreTurns(ctx);
		turnStart = await snapshotWorkspace(ctx.cwd, pi);
		activeTurnStartedAt = undefined;
		reportDirty = false;
	});

	pi.on("turn_start", async (event, ctx) => {
		activeTurnStartedAt = new Date(event.timestamp).toISOString();
		turnStart = await snapshotWorkspace(ctx.cwd, pi);
	});

	pi.on("turn_end", async (event, ctx) => {
		if (!turnStart) return;
		const started = turnStart;
		turnStart = await snapshotWorkspace(ctx.cwd, pi);
		const changes = diffSnapshots(started, turnStart);
		if (changes.length === 0) return;

		const turn: TurnRecord = {
			turnIndex: event.turnIndex,
			startedAt: activeTurnStartedAt ?? new Date().toISOString(),
			completedAt: new Date().toISOString(),
			changes,
		};
		turns.push(turn);
		reportDirty = true;
		pi.appendEntry(CUSTOM_TYPE, { version: 1, kind: "turn", turn: { ...turn, changes: changes.map(persistedChange) } });
	});

	// Wait for agent_settled so a prompt that needs several tool turns opens one
	// report containing the complete set of changes instead of one tab per loop.
	pi.on("agent_settled", async (_event, ctx) => {
		if (reportDirty) {
			await showReport(pi, ctx, true);
			reportDirty = false;
		}
	});

	pi.registerCommand("changes", {
		description: "Open the Pierre report for file changes in this session",
		handler: async (_args, ctx) => {
			await showReport(pi, ctx, true);
		},
	});
}
