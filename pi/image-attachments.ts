import { existsSync, readFileSync, realpathSync, statSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { basename, extname, isAbsolute, resolve } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Image, truncateToWidth, type Component } from "@earendil-works/pi-tui";

const WIDGET_KEY = "image-attachments";
const IMAGE_EXTENSIONS = new Set([".avif", ".gif", ".jpeg", ".jpg", ".png", ".webp"]);
const MAX_ATTACHMENTS = 6;
const MAX_IMAGE_BYTES = 20 * 1024 * 1024;

const MIME_TYPES: Record<string, string> = {
	".avif": "image/avif",
	".gif": "image/gif",
	".jpeg": "image/jpeg",
	".jpg": "image/jpeg",
	".png": "image/png",
	".webp": "image/webp",
};

interface Attachment {
	path: string;
	mimeType: string;
	size: number;
	mtimeMs: number;
}

/** Split editor text like a shell, preserving paths escaped or quoted by terminal drag-and-drop. */
export function editorWords(text: string): string[] {
	const words: string[] = [];
	let word = "";
	let quote: "'" | '"' | undefined;

	const push = () => {
		if (word) words.push(word);
		word = "";
	};

	for (let index = 0; index < text.length; index += 1) {
		const character = text[index]!;
		if (quote) {
			if (character === quote) quote = undefined;
			else if (character === "\\" && quote === '"' && index + 1 < text.length) word += text[++index];
			else word += character;
			continue;
		}

		if (character === "'" || character === '"') quote = character;
		else if (character === "\\" && index + 1 < text.length) word += text[++index];
		else if (/\s/.test(character)) push();
		else word += character;
	}
	push();
	return words;
}

function unwrapReference(value: string): string {
	let result = value.trim();
	const markdownTarget = result.match(/!?\[[^\]]*\]\((.+)\)$/)?.[1];
	if (markdownTarget) result = markdownTarget;

	result = result.replace(/^[([{<]+/, "").replace(/[\])}>.,;:!?]+$/, "");
	if (result.startsWith("@")) result = result.slice(1);
	return result;
}

function referencePath(value: string, cwd: string): string | undefined {
	let candidate = unwrapReference(value);
	if (!candidate || /^https?:\/\//i.test(candidate)) return undefined;

	try {
		if (candidate.startsWith("file://")) candidate = fileURLToPath(candidate);
	} catch {
		return undefined;
	}

	if (candidate === "~") candidate = process.env.HOME ?? candidate;
	else if (candidate.startsWith("~/") && process.env.HOME) candidate = resolve(process.env.HOME, candidate.slice(2));

	const absolutePath = isAbsolute(candidate) ? candidate : resolve(cwd, candidate);
	if (!IMAGE_EXTENSIONS.has(extname(absolutePath).toLowerCase()) || !existsSync(absolutePath)) return undefined;

	try {
		const stats = statSync(absolutePath);
		return stats.isFile() ? realpathSync(absolutePath) : undefined;
	} catch {
		return undefined;
	}
}

export function imageReferences(text: string, cwd: string): string[] {
	const paths = new Set<string>();
	for (const word of editorWords(text)) {
		const path = referencePath(word, cwd);
		if (path) paths.add(path);
		if (paths.size === MAX_ATTACHMENTS) break;
	}
	return [...paths];
}

function describeBytes(bytes: number): string {
	if (bytes < 1024) return `${bytes} B`;
	if (bytes < 1024 * 1024) return `${Math.round(bytes / 1024)} KB`;
	return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
}

export default function imageAttachments(pi: ExtensionAPI) {
	pi.on("session_start", (_event, ctx) => {
		if (ctx.mode !== "tui") return;

		ctx.ui.setWidget(
			WIDGET_KEY,
			(_tui, theme) => {
				let attachments: Attachment[] = [];
				let previousSignature = "";
				const images = new Map<string, Image>();

				const refresh = () => {
					const next = imageReferences(ctx.ui.getEditorText(), ctx.cwd).flatMap((path): Attachment[] => {
						try {
							const stats = statSync(path);
							return [{
								path,
								mimeType: MIME_TYPES[extname(path).toLowerCase()] ?? "image/png",
								size: stats.size,
								mtimeMs: stats.mtimeMs,
							}];
						} catch {
							return [];
						}
					});
					const signature = next.map(({ path, size, mtimeMs }) => `${path}:${size}:${mtimeMs}`).join("|");
					if (signature === previousSignature) return;

					const stalePaths = new Set(
						attachments
							.filter((previous) => {
								const current = next.find((attachment) => attachment.path === previous.path);
								return !current || current.size !== previous.size || current.mtimeMs !== previous.mtimeMs;
							})
							.map(({ path }) => path),
					);
					attachments = next;
					previousSignature = signature;
					for (const path of images.keys()) {
						if (stalePaths.has(path) || !attachments.some((attachment) => attachment.path === path)) {
							images.delete(path);
						}
					}
				};

				const component: Component = {
					render(width: number): string[] {
						refresh();
						if (attachments.length === 0 || width <= 0) return [];

						const lines: string[] = [];
						for (const attachment of attachments) {
							const label = `▣ ${basename(attachment.path)} · ${describeBytes(attachment.size)}`;
							lines.push(truncateToWidth(theme.fg("muted", label), width));

							if (attachment.size > MAX_IMAGE_BYTES) {
								lines.push(truncateToWidth(theme.fg("warning", "  Preview skipped: image is larger than 20 MB"), width));
								continue;
							}

							let image = images.get(attachment.path);
							if (!image) {
								try {
									image = new Image(readFileSync(attachment.path).toString("base64"), attachment.mimeType, {
										fallbackColor: (text) => theme.fg("dim", text),
									}, {
										maxWidthCells: Math.min(28, width),
										maxHeightCells: 8,
										filename: basename(attachment.path),
									});
									images.set(attachment.path, image);
								} catch {
									lines.push(truncateToWidth(theme.fg("warning", "  Preview unavailable"), width));
									continue;
								}
							}
							lines.push(...image.render(width));
						}
						return lines;
					},
					invalidate() {
						previousSignature = "";
						for (const image of images.values()) image.invalidate();
					},
				};

				return component;
			},
			{ placement: "belowEditor" },
		);
	});
}
