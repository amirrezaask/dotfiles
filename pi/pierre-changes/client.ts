import { FileDiff } from "@pierre/diffs";
import { FileTree } from "@pierre/trees";

type Snapshot = {
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

type ReportData = {
	cwd: string;
	sessionId: string;
	sessionTitle: string;
	generatedAt: string;
	turns: TurnRecord[];
	files: FileChange[];
};

declare global {
	interface Window {
		__PI_PIERRE_CHANGES__: ReportData;
	}
}

const data = window.__PI_PIERRE_CHANGES__;
const treeMount = document.querySelector<HTMLElement>("#file-tree");
const diffMount = document.querySelector<HTMLElement>("#file-diff");
const scopeSelect = document.querySelector<HTMLSelectElement>("#scope");
const fileCount = document.querySelector<HTMLElement>("#file-count");
const sidebarFileCount = document.querySelector<HTMLElement>("#sidebar-file-count");
const turnCount = document.querySelector<HTMLElement>("#turn-count");
const additions = document.querySelector<HTMLElement>("#additions");
const deletions = document.querySelector<HTMLElement>("#deletions");
const selectedFile = document.querySelector<HTMLElement>("#selected-file");
const selectedKind = document.querySelector<HTMLElement>("#selected-kind");
const selectedMeta = document.querySelector<HTMLElement>("#selected-meta");
const emptyState = document.querySelector<HTMLElement>("#empty-state");

let tree: FileTree | undefined;
let diff: FileDiff | undefined;
let currentChanges: FileChange[] = [];

function formatBytes(bytes: number): string {
	if (bytes < 1024) return `${bytes} B`;
	if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
	return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

function kindLabel(kind: FileChange["kind"]): string {
	return {
		added: "Added",
		modified: "Modified",
		deleted: "Deleted",
		renamed: "Renamed",
	}[kind];
}

function statusForChange(kind: FileChange["kind"]): "added" | "deleted" | "modified" | "renamed" {
	return kind === "added" || kind === "deleted" || kind === "renamed" ? kind : "modified";
}

function changesForScope(scope: string): FileChange[] {
	if (scope === "session") return data.files;
	const index = Number.parseInt(scope, 10);
	return data.turns[index]?.changes ?? [];
}

function renderStats(changes: FileChange[]): void {
	const added = changes.filter((change) => change.kind === "added").length;
	const removed = changes.filter((change) => change.kind === "deleted").length;
	const modified = changes.filter((change) => change.kind === "modified").length;
	const renamed = changes.filter((change) => change.kind === "renamed").length;

	const fileLabel = `${changes.length} ${changes.length === 1 ? "file" : "files"}`;
	fileCount!.textContent = fileLabel;
	if (sidebarFileCount) sidebarFileCount.textContent = fileLabel;
	turnCount!.textContent = `${data.turns.length} ${data.turns.length === 1 ? "turn" : "turns"}`;
	additions!.textContent = `${added} added`;
	deletions!.textContent = `${removed} deleted`;
	selectedMeta!.textContent = `${modified} modified · ${renamed} renamed`;
}

function setText(selector: HTMLElement | null, text: string): void {
	if (selector) selector.textContent = text;
}

function renderUnavailable(change: FileChange): void {
	diffMount!.replaceChildren();
	const card = document.createElement("div");
	card.className = "unavailable";
	const title = document.createElement("strong");
	title.textContent = change.before?.binary || change.after?.binary
		? "Binary file change"
		: "Text preview unavailable";
	const detail = document.createElement("p");
	detail.textContent = "The change was recorded, but this report keeps only metadata for binary or oversized content.";
	card.append(title, detail);
	diffMount!.append(card);
}

function renderDiff(change: FileChange | undefined): void {
	if (diff) {
		diff.cleanUp();
		diff = undefined;
	}
	diffMount!.replaceChildren();

	if (!change) {
		if (emptyState) diffMount!.append(emptyState);
		setText(selectedFile, "No file changes in this view");
		setText(selectedKind, "");
		setText(selectedMeta, "");
		emptyState!.hidden = false;
		return;
	}

	emptyState!.hidden = true;
	setText(selectedFile, change.path);
	setText(selectedKind, kindLabel(change.kind));
	const beforeSize = change.before ? formatBytes(change.before.size) : "—";
	const afterSize = change.after ? formatBytes(change.after.size) : "—";
	setText(selectedMeta, `${beforeSize} → ${afterSize}`);

	const oldContents = change.before?.contents;
	const newContents = change.after?.contents;
	const needsPreview =
		change.before?.binary ||
		change.after?.binary ||
		(change.before != null && oldContents === undefined) ||
		(change.after != null && newContents === undefined);
	if (needsPreview) {
		renderUnavailable(change);
		return;
	}

	const wrapper = document.createElement("div");
	wrapper.className = "pierre-diff-wrapper";
	diffMount!.append(wrapper);
	diff = new FileDiff({
		themeType: "dark",
		diffStyle: "split",
		overflow: "scroll",
		hunkSeparators: "line-info",
		lineDiffType: "word-alt",
		stickyHeader: true,
		unsafeCSS: `
			:host { --diffs-font-size: 13px; --diffs-line-height: 21px; }
			[data-diffs-header="default"] { position: sticky; top: 0; }
		`,
	});
	const oldFile = change.before
		? { name: change.previousPath ?? change.path, contents: oldContents ?? "" }
		: null;
	const newFile = change.after
		? { name: change.path, contents: newContents ?? "" }
		: null;
	if (oldFile && newFile) {
		diff.render({ containerWrapper: wrapper, oldFile, newFile });
	} else if (oldFile) {
		diff.render({ containerWrapper: wrapper, oldFile, newFile: null });
	} else if (newFile) {
		diff.render({ containerWrapper: wrapper, oldFile: null, newFile });
	}
}

function renderTree(changes: FileChange[]): void {
	currentChanges = changes;
	tree?.cleanUp();
	tree = undefined;
	treeMount!.replaceChildren();
	renderStats(changes);

	if (changes.length === 0) {
		emptyState!.hidden = false;
		renderDiff(undefined);
		return;
	}

	emptyState!.hidden = true;
	const paths = changes.map((change) => change.path);
	const firstPath = paths[0];
	tree = new FileTree({
		paths,
		initialExpansion: "open",
		initialSelectedPaths: firstPath ? [firstPath] : [],
		search: true,
		unsafeCSS: `
			:host {
				--trees-bg-override: #0d1015;
				--trees-bg-muted-override: #1a2433;
				--trees-fg-override: #c7d0df;
				--trees-fg-muted-override: #7c899e;
				--trees-border-color-override: #27303c;
				--trees-input-bg-override: #151b24;
				--trees-selected-bg-override: #1d3657;
				--trees-selected-fg-override: #e2edff;
				--trees-accent-override: #6b9fff;
				--trees-font-family: "Geist Mono", ui-monospace, monospace;
				--trees-font-size: 12px;
			}
		`,
		gitStatus: changes.map((change) => ({
			path: change.path,
			status: statusForChange(change.kind),
		})),
		onSelectionChange: (selectedPaths) => {
			const path = tree?.getFocusedPath() ?? selectedPaths[0];
			if (path) renderDiff(currentChanges.find((change) => change.path === path));
		},
	});
	tree.render({ containerWrapper: treeMount! });
	renderDiff(changes.find((change) => change.path === firstPath));
}

function updateScope(): void {
	renderTree(changesForScope(scopeSelect?.value ?? "session"));
}

if (scopeSelect) {
	scopeSelect.addEventListener("change", updateScope);
}

for (const [index, turn] of data.turns.entries()) {
	const option = document.createElement("option");
	option.value = String(index);
	option.textContent = `Turn ${turn.turnIndex + 1} · ${turn.changes.length} ${turn.changes.length === 1 ? "file" : "files"}`;
	scopeSelect?.append(option);
}

setText(document.querySelector<HTMLElement>("#project"), data.cwd);
setText(document.querySelector<HTMLElement>("#generated"), new Date(data.generatedAt).toLocaleString());
updateScope();
