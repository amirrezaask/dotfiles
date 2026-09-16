import { createAssistantMessageEventStream, getModels, streamOpenAICodexResponses, type Api, type AssistantMessage, type Context, type Model, type Provider, type SimpleStreamOptions } from "@earendil-works/pi-ai/compat";
import type { ExtensionAPI, ModelRegistry, ProviderModelConfig } from "@earendil-works/pi-coding-agent";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { accountIdFromToken, CODEX_USAGE_URL, compactUsage, emailFromToken, parseCache, parseUsagePayload, usageScore, type AccountUsage } from "./usage.ts";

const MUX_PROVIDER = "openai-codex-mux";
const CODEX_BASE_URL = "https://chatgpt.com/backend-api";
const ACCOUNT_PREFIX = "openai-codex-account-";
const STATUS_KEY = "codex-mux";
const PLACEHOLDER_API_KEY = "__codex_mux_resolves_account_auth__";
const CONFIG_PATH = join(homedir(), ".pi", "agent", "codex-mux.json");
const CACHE_PATH = join(homedir(), ".pi", "agent", "codex-mux-usage.json");
const CACHE_MAX_AGE_MS = 3 * 60_000;
const POST_TURN_MAX_AGE_MS = 60_000;
const REQUEST_TIMEOUT_MS = 8_000;

interface AccountSlot {
	id: string;
	label: string;
}

interface Config {
	version: 1;
	accounts: AccountSlot[];
}

function defaultConfig(): Config {
	return {
		version: 1,
		accounts: [
			{ id: "1", label: "Account 1" },
			{ id: "2", label: "Account 2" },
		],
	};
}

function readConfig(): Config {
	try {
		const parsed = JSON.parse(readFileSync(CONFIG_PATH, "utf8")) as Partial<Config>;
		if (parsed.version === 1 && Array.isArray(parsed.accounts) && parsed.accounts.length > 0) {
			const accounts = parsed.accounts.flatMap((account): AccountSlot[] =>
				typeof account?.id === "string" && typeof account.label === "string"
					? [{ id: account.id, label: account.label }]
					: [],
			);
			if (accounts.length > 0) return { version: 1, accounts };
		}
	} catch {}
	return defaultConfig();
}

function writeJson(path: string, value: unknown): void {
	mkdirSync(dirname(path), { recursive: true });
	const temporary = `${path}.${process.pid}.tmp`;
	writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`, { mode: 0o600 });
	renameSync(temporary, path);
}

function providerId(slot: AccountSlot): string {
	return `${ACCOUNT_PREFIX}${slot.id}`;
}

function toModelConfig(model: Model<"openai-codex-responses">): ProviderModelConfig {
	return {
		id: model.id,
		name: model.name,
		baseUrl: model.baseUrl,
		reasoning: model.reasoning,
		thinkingLevelMap: model.thinkingLevelMap,
		input: model.input,
		cost: model.cost,
		contextWindow: model.contextWindow,
		maxTokens: model.maxTokens,
		headers: model.headers,
		compat: model.compat,
	};
}

function endWithError(
	stream: ReturnType<typeof createAssistantMessageEventStream>,
	modelId: string,
	message: string,
	options?: SimpleStreamOptions,
): void {
	const error: AssistantMessage = {
		role: "assistant",
		content: [],
		api: "openai-codex-responses",
		provider: MUX_PROVIDER,
		model: modelId,
		usage: {
			input: 0,
			output: 0,
			cacheRead: 0,
			cacheWrite: 0,
			totalTokens: 0,
			cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, total: 0 },
		},
		stopReason: options?.signal?.aborted ? "aborted" : "error",
		errorMessage: message,
		timestamp: Date.now(),
	};
	stream.push({ type: "error", reason: error.stopReason === "aborted" ? "aborted" : "error", error });
	stream.end(error);
}

export default function codexMux(pi: ExtensionAPI) {
	const codexModels = getModels("openai-codex") as Model<"openai-codex-responses">[];
	const modelConfigs = codexModels.map(toModelConfig);
	let config = readConfig();
	let modelRegistry: ModelRegistry | undefined;
	let baseProvider: Provider | undefined;
	let usageByAccount: Record<string, AccountUsage> = {};
	let pinnedAccount: string | undefined;
	let lastSelectedAccount: string | undefined;
	let roundRobinOffset = 0;
	let startupTimer: NodeJS.Timeout | undefined;
	let alive = false;
	const refreshing = new Map<string, Promise<void>>();

	try {
		usageByAccount = parseCache(JSON.parse(readFileSync(CACHE_PATH, "utf8")));
	} catch {}

	const registerAccount = (slot: AccountSlot) => {
		if (!baseProvider) throw new Error("The built-in OpenAI Codex provider is unavailable");
		pi.registerProvider({
			...baseProvider,
			id: providerId(slot),
			name: `OpenAI Codex · ${slot.label}`,
			getModels: () => [],
		});
	};

	const displayName = (slot: AccountSlot): string => usageByAccount[providerId(slot)]?.email ?? slot.label;

	const updateStatus = (ctx: { ui: { setStatus(key: string, value: string | undefined): void } }) => {
		const configured = config.accounts.filter((slot) => modelRegistry?.getProviderAuthStatus(providerId(slot)));
		if (configured.length === 0) {
			ctx.ui.setStatus(STATUS_KEY, undefined);
			return;
		}
		const summary = configured
			.map((slot) => `${displayName(slot)}:${compactUsage(usageByAccount[providerId(slot)]).split(" /")[0]}`)
			.join(" ");
		const selectedSlot = config.accounts.find((slot) => providerId(slot) === lastSelectedAccount);
		ctx.ui.setStatus(STATUS_KEY, `codex ${summary}${selectedSlot ? ` →${displayName(selectedSlot)}` : ""}`);
	};

	const saveUsage = () => writeJson(CACHE_PATH, { version: 1, accounts: usageByAccount });

	const refreshAccount = async (slot: AccountSlot, force = false): Promise<void> => {
		const id = providerId(slot);
		const cached = usageByAccount[id];
		if (!force && cached && Date.now() - cached.capturedAt < CACHE_MAX_AGE_MS) return;
		const existing = refreshing.get(id);
		if (existing) return existing;

		const task = (async () => {
			if (!modelRegistry) return;
			const resolved = await modelRegistry.getProviderAuth(id);
			const apiKey = resolved?.auth.apiKey;
			if (!apiKey) return;
			const headers = new Headers();
			for (const [name, value] of Object.entries(resolved.auth.headers ?? {})) {
				if (typeof value === "string") headers.set(name, value);
			}
			headers.set("Authorization", `Bearer ${apiKey}`);
			headers.set("Accept", "application/json");
			headers.set("OpenAI-Beta", "codex-1");
			headers.set("User-Agent", "pi-codex-mux");
			const accountId = accountIdFromToken(apiKey);
			if (accountId) headers.set("ChatGPT-Account-ID", accountId);

			const controller = new AbortController();
			const timeout = setTimeout(() => controller.abort(), REQUEST_TIMEOUT_MS);
			try {
				const response = await fetch(CODEX_USAGE_URL, { headers, signal: controller.signal });
				if (!response.ok) throw new Error(`usage endpoint returned ${response.status}`);
				usageByAccount = {
					...usageByAccount,
					[id]: { ...parseUsagePayload(await response.json()), email: emailFromToken(apiKey) },
				};
				saveUsage();
			} finally {
				clearTimeout(timeout);
			}
		})().finally(() => refreshing.delete(id));
		refreshing.set(id, task);
		return task;
	};

	const refreshAccounts = async (slots = config.accounts, force = false): Promise<void> => {
		await Promise.allSettled(slots.map((slot) => refreshAccount(slot, force)));
	};

	const orderedAccounts = (): AccountSlot[] => {
		if (pinnedAccount) {
			const pinned = config.accounts.find((slot) => providerId(slot) === pinnedAccount);
			if (pinned) return [pinned];
		}
		const rotated = config.accounts.map((_, index) => config.accounts[(index + roundRobinOffset) % config.accounts.length]!);
		roundRobinOffset = (roundRobinOffset + 1) % Math.max(1, config.accounts.length);
		return rotated.sort(
			(a, b) => usageScore(usageByAccount[providerId(a)]) - usageScore(usageByAccount[providerId(b)]),
		);
	};

	const streamMux = (model: Model<Api>, context: Context, options?: SimpleStreamOptions) => {
		const outer = createAssistantMessageEventStream();
		void (async () => {
			if (!modelRegistry) throw new Error("Codex mux session is not initialized");
			const codexModel = codexModels.find((candidate) => candidate.id === model.id);
			if (!codexModel) throw new Error(`Underlying OpenAI Codex model not found: ${model.id}`);

			const authErrors: string[] = [];
			for (const slot of orderedAccounts()) {
				const id = providerId(slot);
				try {
					const resolved = await modelRegistry.getProviderAuth(id);
					const apiKey = resolved?.auth.apiKey;
					if (!apiKey) continue;
					lastSelectedAccount = id;
					const inner = streamOpenAICodexResponses(codexModel, context, {
						...options,
						apiKey,
						headers: { ...resolved.auth.headers, ...options?.headers },
					});
					for await (const event of inner) outer.push(event);
					outer.end();
					return;
				} catch (error) {
					authErrors.push(`${displayName(slot)}: ${error instanceof Error ? error.message : String(error)}`);
				}
			}
			throw new Error(
				authErrors.length > 0
					? `No Codex account could authenticate (${authErrors.join("; ")})`
					: `No Codex accounts are logged in. Run /login ${providerId(config.accounts[0]!)}`,
			);
		})().catch((error: unknown) => {
			endWithError(outer, model.id, error instanceof Error ? error.message : String(error), options);
		});
		return outer;
	};

	if (modelConfigs.length > 0) {
		pi.registerProvider(MUX_PROVIDER, {
			name: "OpenAI Codex · Auto",
			baseUrl: CODEX_BASE_URL,
			apiKey: PLACEHOLDER_API_KEY,
			api: "openai-codex-mux-responses",
			models: modelConfigs,
			streamSimple: streamMux,
		});
	}

	pi.registerCommand("codex-accounts", {
		description: "Show, add, refresh, or select Codex subscription accounts",
		handler: async (args, ctx) => {
			const [action = "show", ...rest] = args.trim().split(/\s+/).filter(Boolean);
			if (action === "add") {
				const nextId = String(Math.max(0, ...config.accounts.map((slot) => Number(slot.id) || 0)) + 1);
				const slot = { id: nextId, label: rest.join(" ") || `Account ${nextId}` };
				config = { ...config, accounts: [...config.accounts, slot] };
				writeJson(CONFIG_PATH, config);
				registerAccount(slot);
				ctx.ui.notify(`Added ${slot.label}. Run /login ${providerId(slot)}.`, "info");
				return;
			}
			if (action === "use") {
				const requested = rest[0];
				if (!requested || requested === "auto") {
					pinnedAccount = undefined;
					ctx.ui.notify("Codex account selection set to automatic.", "info");
					return;
				}
				const normalized = requested.toLowerCase();
				const slot = config.accounts.find((candidate) =>
					candidate.id === requested ||
					providerId(candidate) === requested ||
					displayName(candidate).toLowerCase() === normalized,
				);
				if (!slot) {
					ctx.ui.notify(`Unknown Codex account: ${requested}`, "warning");
					return;
				}
				pinnedAccount = providerId(slot);
				ctx.ui.notify(`Pinned Codex mux to ${displayName(slot)} for this session.`, "info");
				return;
			}
			if (action !== "show" && action !== "refresh") {
				ctx.ui.notify("Usage: /codex-accounts [show|refresh|add <label>|use <id|email|auto>]", "warning");
				return;
			}
			if (action === "refresh") await refreshAccounts(config.accounts, true);
			const lines = config.accounts.map((slot) => {
				const id = providerId(slot);
				const configured = !!ctx.modelRegistry.getProviderAuthStatus(id);
				return `${displayName(slot)}: ${configured ? compactUsage(usageByAccount[id]) : `not logged in — /login ${id}`}`;
			});
			ctx.ui.notify(["OpenAI Codex accounts", ...lines, `Mux provider: ${MUX_PROVIDER}`].join("\n"), "info");
			updateStatus(ctx);
		},
	});

	pi.on("session_start", (_event, ctx) => {
		alive = true;
		modelRegistry = ctx.modelRegistry;
		baseProvider = ctx.modelRegistry.getProvider("openai-codex");
		if (!baseProvider) {
			ctx.ui.notify("Codex mux could not find Pi's built-in OpenAI Codex provider.", "error");
			return;
		}
		for (const slot of config.accounts) registerAccount(slot);
		updateStatus(ctx);
		startupTimer = setTimeout(() => {
			if (!alive) return;
			void refreshAccounts().then(() => alive && updateStatus(ctx));
		}, 1_500);
		startupTimer.unref?.();
	});

	pi.on("agent_settled", (_event, ctx) => {
		if (!lastSelectedAccount) return;
		const slot = config.accounts.find((candidate) => providerId(candidate) === lastSelectedAccount);
		if (!slot) return;
		const cached = usageByAccount[lastSelectedAccount];
		if (cached && Date.now() - cached.capturedAt < POST_TURN_MAX_AGE_MS) return;
		void refreshAccount(slot).then(() => alive && updateStatus(ctx));
	});

	pi.on("session_shutdown", (_event, ctx) => {
		alive = false;
		if (startupTimer) clearTimeout(startupTimer);
		startupTimer = undefined;
		ctx.ui.setStatus(STATUS_KEY, undefined);
	});
}
