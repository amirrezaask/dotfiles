import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { compactUsage, isUnavailable, parseCache, parseSnapshot, type SubUsageRow } from "./usage.ts";

const WIDGET_KEY = "router-usage";
const CONFIG_PATH = join(homedir(), ".pi", "agent", "router-usage.json");
const CACHE_PATH = join(homedir(), ".pi", "agent", "router-usage-cache.json");
const DEFAULT_BASE_URL = "http://127.0.0.1:4000";
const STARTUP_DELAY_MS = 1_500;
const USAGE_REFRESH_INTERVAL_MS = 15_000;
const POST_TURN_MAX_AGE_MS = 60_000;
const REQUEST_TIMEOUT_MS = 8_000;

interface Config {
	version: 1;
	baseUrl: string;
}

function readConfig(): Config {
	try {
		const parsed = JSON.parse(readFileSync(CONFIG_PATH, "utf8")) as Partial<Config>;
		if (typeof parsed.baseUrl === "string" && parsed.baseUrl.trim()) {
			return { version: 1, baseUrl: parsed.baseUrl.trim().replace(/\/+$/, "") };
		}
	} catch {}
	const baseUrl = (process.env["ROUTER_URL"] ?? DEFAULT_BASE_URL).trim().replace(/\/+$/, "");
	return { version: 1, baseUrl };
}

function writeJson(path: string, value: unknown): void {
	mkdirSync(dirname(path), { recursive: true });
	const temporary = `${path}.${process.pid}.tmp`;
	writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`, { mode: 0o600 });
	renameSync(temporary, path);
}

export default function routerUsage(pi: ExtensionAPI) {
	const config = readConfig();
	let rows: SubUsageRow[] = [];
	let capturedAt = 0;
	let lastError: string | undefined;
	let uiContext: ExtensionContext | undefined;
	let startupTimer: NodeJS.Timeout | undefined;
	let usageRefreshTimer: NodeJS.Timeout | undefined;
	let alive = false;
	let inFlight: Promise<void> | undefined;

	try {
		const cache = parseCache(JSON.parse(readFileSync(CACHE_PATH, "utf8")));
		if (cache) {
			rows = cache.rows;
			capturedAt = cache.capturedAt;
		}
	} catch {}

	const saveCache = () => {
		if (capturedAt === 0) return;
		writeJson(CACHE_PATH, { version: 1, capturedAt, rows });
	};

	const updateDisplay = (ctx: ExtensionContext) => {
		const withUsage = rows.filter((row) => row.usage);
		const lines = [ctx.ui.theme.fg("muted", "Router subs")];
		if (withUsage.length === 0) {
			lines.push(
				lastError
					? ctx.ui.theme.fg("warning", `router unreachable at ${config.baseUrl} (${lastError})`)
					: ctx.ui.theme.fg("dim", "waiting for usage data…"),
			);
		} else {
			for (const row of withUsage) {
				const unavailable = isUnavailable(row);
				const marker = unavailable ? ctx.ui.theme.fg("warning", "!") : " ";
				const name = row.enabled ? row.name : ctx.ui.theme.fg("dim", row.name);
				const usage = ctx.ui.theme.fg(unavailable ? "error" : "dim", compactUsage(row.usage));
				lines.push(`${marker} ${name}  ${usage}`);
			}
			if (lastError) lines.push(ctx.ui.theme.fg("dim", `stale — ${lastError}`));
		}
		ctx.ui.setWidget(WIDGET_KEY, lines, { placement: "belowEditor" });
	};

	const apiGet = async (path: string): Promise<unknown> => {
		const controller = new AbortController();
		const timeout = setTimeout(() => controller.abort(), REQUEST_TIMEOUT_MS);
		try {
			const response = await fetch(`${config.baseUrl}${path}`, {
				headers: { accept: "application/json" },
				signal: controller.signal,
			});
			if (!response.ok) throw new Error(`${path} returned ${response.status}`);
			return await response.json();
		} finally {
			clearTimeout(timeout);
		}
	};

	/** Read the router's latest persisted usage snapshot (`GET /api/usage`). */
	const fetchUsage = async (force = false): Promise<void> => {
		if (!force && Date.now() - capturedAt < POST_TURN_MAX_AGE_MS) return;
		if (inFlight) return inFlight;

		const task = (async () => {
			try {
				rows = parseSnapshot(await apiGet("/api/usage"));
				capturedAt = Date.now();
				lastError = undefined;
				saveCache();
			} catch (error) {
				lastError = error instanceof Error ? error.message : String(error);
			} finally {
				inFlight = undefined;
			}
			if (alive && uiContext) updateDisplay(uiContext);
		})();
		inFlight = task;
		return task;
	};

	/**
	 * Force-refresh every sub's usage upstream (`GET /subs/:id/usage`), then
	 * re-read the snapshot. Subs whose provider lacks usage monitoring fail
	 * harmlessly and keep their last snapshot.
	 */
	const probeUsage = async (): Promise<void> => {
		try {
			const payload = (await apiGet("/api/subs")) as { data?: Array<{ id?: unknown }> };
			const ids = (Array.isArray(payload?.data) ? payload.data : [])
				.map((sub) => (typeof sub?.id === "string" ? sub.id : undefined))
				.filter((id): id is string => !!id);
			await Promise.allSettled(ids.map((id) => apiGet(`/api/subs/${id}/usage`)));
		} catch {}
		await fetchUsage(true);
	};

	pi.registerCommand("router-usage", {
		description: "Show router subscription usage, or force-refresh it",
		handler: async (args, ctx) => {
			const action = args.trim().toLowerCase() || "show";
			if (action === "refresh") {
				ctx.ui.notify("Refreshing router usage…", "info");
				await probeUsage();
				ctx.ui.notify(
					`Router usage refreshed — ${rows.filter((row) => row.usage).length}/${rows.length} subs with data.`,
					"info",
				);
				updateDisplay(ctx);
				return;
			}
			if (action !== "show") {
				ctx.ui.notify("Usage: /router-usage [show|refresh]", "warning");
				return;
			}
			const lines = rows.length
				? rows.map((row) => `${row.name}: ${row.usage ? compactUsage(row.usage) : "no usage data"}`)
				: ["No subs found — is the router running?"];
			ctx.ui.notify(
				[
					`Router subs (${config.baseUrl})`,
					...lines,
					...(lastError ? [`stale — ${lastError}`] : []),
				],
				"info",
			);
			updateDisplay(ctx);
		},
	});

	pi.on("session_start", async (_event, ctx) => {
		alive = true;
		uiContext = ctx;
		updateDisplay(ctx);
		startupTimer = setTimeout(() => {
			if (alive) void fetchUsage(true);
		}, STARTUP_DELAY_MS);
		startupTimer.unref?.();
		usageRefreshTimer = setInterval(() => {
			if (alive) void fetchUsage(true);
		}, USAGE_REFRESH_INTERVAL_MS);
		usageRefreshTimer.unref?.();
	});

	pi.on("agent_settled", (_event, ctx) => {
		updateDisplay(ctx);
		if (Date.now() - capturedAt < POST_TURN_MAX_AGE_MS) return;
		void fetchUsage(true);
	});

	pi.on("session_shutdown", (_event, ctx) => {
		alive = false;
		if (startupTimer) clearTimeout(startupTimer);
		if (usageRefreshTimer) clearInterval(usageRefreshTimer);
		startupTimer = undefined;
		usageRefreshTimer = undefined;
		uiContext = undefined;
		saveCache();
		ctx.ui.setWidget(WIDGET_KEY, undefined);
	});
}
