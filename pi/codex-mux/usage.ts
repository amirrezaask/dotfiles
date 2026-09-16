export const CODEX_USAGE_URL = "https://chatgpt.com/backend-api/wham/usage";

export interface UsageWindow {
	usedPercent: number;
	resetsAt?: number;
	windowMinutes?: number;
}

export interface UsageSnapshot {
	limitId: string;
	limitName?: string;
	primary?: UsageWindow;
	secondary?: UsageWindow;
}

export interface AccountUsage {
	capturedAt: number;
	planType?: string;
	email?: string;
	snapshots: UsageSnapshot[];
}

interface CacheFile {
	version: 1;
	accounts: Record<string, AccountUsage>;
}

function record(value: unknown, label: string): Record<string, unknown> {
	if (!value || typeof value !== "object" || Array.isArray(value)) {
		throw new Error(`${label} was not an object`);
	}
	return value as Record<string, unknown>;
}

function number(value: unknown): number | undefined {
	if (typeof value === "number" && Number.isFinite(value)) return value;
	if (typeof value === "string" && value.trim()) {
		const parsed = Number(value);
		if (Number.isFinite(parsed)) return parsed;
	}
	return undefined;
}

function string(value: unknown): string | undefined {
	return typeof value === "string" ? value : undefined;
}

function parseWindow(value: unknown): UsageWindow | undefined {
	if (value == null) return undefined;
	const data = record(value, "rate-limit window");
	const usedPercent = number(data.used_percent);
	if (usedPercent === undefined) return undefined;
	const windowSeconds = number(data.limit_window_seconds);
	return {
		usedPercent: Math.min(100, Math.max(0, usedPercent)),
		resetsAt: number(data.reset_at),
		windowMinutes: windowSeconds && windowSeconds > 0 ? Math.ceil(windowSeconds / 60) : undefined,
	};
}

function parseSnapshot(limitId: string, limitName: string | undefined, value: unknown): UsageSnapshot | undefined {
	if (value == null) return undefined;
	const data = record(value, "rate limit");
	const primary = parseWindow(data.primary_window);
	const secondary = parseWindow(data.secondary_window);
	if (!primary && !secondary) return undefined;
	return { limitId, limitName, primary, secondary };
}

export function parseUsagePayload(payload: unknown, capturedAt = Date.now()): AccountUsage {
	const data = record(payload, "Codex usage response");
	const snapshots: UsageSnapshot[] = [];
	const base = parseSnapshot("codex", undefined, data.rate_limit);
	if (base) snapshots.push(base);

	if (Array.isArray(data.additional_rate_limits)) {
		for (const value of data.additional_rate_limits) {
			const limit = record(value, "additional rate limit");
			const id = string(limit.metered_feature) ?? string(limit.limit_name);
			if (!id) continue;
			const snapshot = parseSnapshot(id, string(limit.limit_name), limit.rate_limit);
			if (snapshot) snapshots.push(snapshot);
		}
	}

	if (snapshots.length === 0) throw new Error("Codex returned no rate-limit windows");
	return { capturedAt, planType: string(data.plan_type), snapshots };
}

export function parseCache(value: unknown): Record<string, AccountUsage> {
	if (!value || typeof value !== "object") return {};
	const data = value as Partial<CacheFile>;
	if (data.version !== 1 || !data.accounts || typeof data.accounts !== "object") return {};
	return data.accounts;
}

export function usageScore(usage: AccountUsage | undefined, now = Date.now()): number {
	if (!usage) return 70;
	const snapshot = usage.snapshots.find((item) => item.limitId === "codex") ?? usage.snapshots[0];
	if (!snapshot) return 70;
	const windows = [snapshot.primary, snapshot.secondary].filter((window): window is UsageWindow => !!window);
	if (windows.some((window) => window.usedPercent >= 100 && (!window.resetsAt || window.resetsAt * 1000 > now))) {
		return Number.POSITIVE_INFINITY;
	}
	return Math.max(0, ...windows.map((window) => window.usedPercent));
}

function windowLabel(window: UsageWindow): string {
	const minutes = window.windowMinutes;
	if (!minutes) return "limit";
	if (minutes % (24 * 60) === 0) return `${minutes / (24 * 60)}d`;
	if (minutes % 60 === 0) return `${minutes / 60}h`;
	return `${minutes}m`;
}

export function compactUsage(usage: AccountUsage | undefined): string {
	if (!usage) return "?";
	const snapshot = usage.snapshots.find((item) => item.limitId === "codex") ?? usage.snapshots[0];
	if (!snapshot) return "?";
	const parts: string[] = [];
	if (snapshot.primary) parts.push(`${Math.round(snapshot.primary.usedPercent)}% ${windowLabel(snapshot.primary)}`);
	if (snapshot.secondary) parts.push(`${Math.round(snapshot.secondary.usedPercent)}% ${windowLabel(snapshot.secondary)}`);
	return parts.join(" / ") || "?";
}

function tokenPayload(token: string): Record<string, unknown> | undefined {
	try {
		const encoded = token.split(".")[1];
		if (!encoded) return undefined;
		return JSON.parse(Buffer.from(encoded, "base64url").toString("utf8")) as Record<string, unknown>;
	} catch {
		return undefined;
	}
}

export function accountIdFromToken(token: string): string | undefined {
	const auth = tokenPayload(token)?.["https://api.openai.com/auth"] as Record<string, unknown> | undefined;
	const accountId = auth?.chatgpt_account_id;
	return typeof accountId === "string" && accountId ? accountId : undefined;
}

export function emailFromToken(token: string): string | undefined {
	const profile = tokenPayload(token)?.["https://api.openai.com/profile"] as Record<string, unknown> | undefined;
	const email = profile?.email;
	return typeof email === "string" && email ? email : undefined;
}
