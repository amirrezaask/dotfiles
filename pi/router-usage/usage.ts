export interface UsageWindow {
	id: "primary" | "secondary" | string;
	/** 0–100. */
	usedPercent: number;
	windowSeconds?: number | null;
	/** Epoch ms. */
	resetAt?: number | null;
}

export interface SubUsage {
	provider: string;
	/** Epoch ms when the router fetched this snapshot. */
	fetchedAt: number;
	plan?: string | null;
	allowed?: boolean;
	limitReached?: boolean;
	windows: UsageWindow[];
}

export interface SubUsageRow {
	subId: string;
	name: string;
	type: string;
	enabled: boolean;
	cooldownUntil?: number | null;
	usage?: SubUsage | null;
}

interface CacheFile {
	version: 1;
	capturedAt: number;
	rows: SubUsageRow[];
}

function parseUsage(value: unknown): SubUsage | null {
	if (!value || typeof value !== "object" || Array.isArray(value)) return null;
	const usage = value as Record<string, unknown>;
	if (typeof usage.fetchedAt !== "number" || !Number.isFinite(usage.fetchedAt)) return null;

	const windows: UsageWindow[] = [];
	if (Array.isArray(usage.windows)) {
		for (const entry of usage.windows) {
			if (!entry || typeof entry !== "object" || Array.isArray(entry)) continue;
			const window = entry as Record<string, unknown>;
			if (typeof window.usedPercent !== "number" || !Number.isFinite(window.usedPercent)) continue;
			windows.push({
				id: typeof window.id === "string" ? window.id : "primary",
				usedPercent: Math.min(100, Math.max(0, window.usedPercent)),
				windowSeconds: typeof window.windowSeconds === "number" ? window.windowSeconds : null,
				resetAt: typeof window.resetAt === "number" ? window.resetAt : null,
			});
		}
	}

	return {
		provider: typeof usage.provider === "string" ? usage.provider : "unknown",
		fetchedAt: usage.fetchedAt,
		plan: typeof usage.plan === "string" ? usage.plan : null,
		allowed: typeof usage.allowed === "boolean" ? usage.allowed : undefined,
		limitReached: typeof usage.limitReached === "boolean" ? usage.limitReached : undefined,
		windows,
	};
}

/** Parse a `GET /api/usage` payload (or the local cache file) into rows. */
export function parseSnapshot(value: unknown): SubUsageRow[] {
	const data = (value as { data?: unknown } | null)?.data;
	if (!Array.isArray(data)) return [];

	const rows: SubUsageRow[] = [];
	for (const entry of data) {
		if (!entry || typeof entry !== "object" || Array.isArray(entry)) continue;
		const row = entry as Record<string, unknown>;
		if (typeof row.subId !== "string" || typeof row.name !== "string") continue;
		rows.push({
			subId: row.subId,
			name: row.name,
			type: typeof row.type === "string" ? row.type : "unknown",
			enabled: row.enabled !== false,
			cooldownUntil: typeof row.cooldownUntil === "number" ? row.cooldownUntil : null,
			usage: parseUsage(row.usage),
		});
	}
	return rows;
}

export function parseCache(value: unknown): { capturedAt: number; rows: SubUsageRow[] } | undefined {
	if (!value || typeof value !== "object" || Array.isArray(value)) return undefined;
	const data = value as Partial<CacheFile>;
	if (data.version !== 1 || typeof data.capturedAt !== "number" || !Array.isArray(data.rows)) return undefined;
	return { capturedAt: data.capturedAt, rows: parseSnapshot({ data: data.rows }) };
}

/** True when the provider reports no more requests are allowed right now. */
export function isBlocked(usage: SubUsage | null | undefined): boolean {
	if (!usage) return false;
	return usage.limitReached === true || usage.allowed === false;
}

/** Blocked by a limit, or cooled down by the router after a quota error. */
export function isUnavailable(row: SubUsageRow, now = Date.now()): boolean {
	if (isBlocked(row.usage)) return true;
	return typeof row.cooldownUntil === "number" && row.cooldownUntil > now;
}

function windowLabel(window: UsageWindow): string {
	const seconds = window.windowSeconds;
	if (seconds === 18_000) return "5h";
	if (seconds === 604_800) return "wk";
	if (typeof seconds === "number" && seconds > 0) {
		if (seconds % 86_400 === 0) return `${seconds / 86_400}d`;
		if (seconds % 3_600 === 0) return `${seconds / 3_600}h`;
		if (seconds % 60 === 0) return `${seconds / 60}m`;
	}
	return window.id === "secondary" ? "weekly" : "primary";
}

function resetLabel(resetAt: number, now: number): string {
	const seconds = (resetAt - now) / 1000;
	if (seconds <= 0) return "now";
	if (seconds < 60) return `${Math.max(1, Math.round(seconds))}s`;
	if (seconds < 3_600) return `${Math.round(seconds / 60)}m`;
	if (seconds < 86_400) return `${Math.round(seconds / 360) / 10}h`;
	return `${Math.round(seconds / 8_640) / 10}d`;
}

function compactWindow(window: UsageWindow, now: number): string {
	const reset = window.resetAt ? resetLabel(window.resetAt, now) : undefined;
	const suffix = reset ? (reset === "now" ? " (resets now)" : ` (resets in ${reset})`) : "";
	return `${Math.round(window.usedPercent)}% ${windowLabel(window)}${suffix}`;
}

export function compactUsage(usage: SubUsage | null | undefined, now = Date.now()): string {
	if (!usage) return "?";
	if (usage.windows.length === 0) {
		return isBlocked(usage) ? "limit reached" : usage.plan ?? "ok";
	}
	const parts = usage.windows.map((window) => compactWindow(window, now));
	let text = parts.join(" / ");
	if (usage.plan) text += ` · ${usage.plan}`;
	if (isBlocked(usage)) text += " · limit reached";
	return text;
}
