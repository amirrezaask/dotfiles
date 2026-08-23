/**
 * Ask for confirmation before a user prompt is sent to an expensive model.
 *
 * The gate runs on `input`, so one consent is requested per user prompt and
 * automatic retries/tool turns do not produce additional prompts.
 */
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const DEFAULT_COST_THRESHOLD = 10;
const DEFAULT_MODEL_PATTERNS = ["opus", "sol"];

function configuredModelPatterns(): string[] {
	const configured = process.env.PI_CONSENT_MODEL_PATTERNS;
	if (configured === undefined) return DEFAULT_MODEL_PATTERNS;
	return configured
		.split(",")
		.map((pattern) => pattern.trim().toLowerCase())
		.filter(Boolean);
}

function configuredCostThreshold(): number {
	const configured = Number.parseFloat(process.env.PI_CONSENT_COST_THRESHOLD ?? "");
	return Number.isFinite(configured) && configured >= 0 ? configured : DEFAULT_COST_THRESHOLD;
}

function matchesModelPattern(modelText: string, pattern: string): boolean {
	const escaped = pattern.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
	return new RegExp(`\\b${escaped}\\b`, "i").test(modelText);
}

function numericRates(model: {
	cost?: {
		input?: number;
		output?: number;
		cacheRead?: number;
		cacheWrite?: number;
		tiers?: Array<{
			input?: number;
			output?: number;
			cacheRead?: number;
			cacheWrite?: number;
		}>;
	};
}): number[] {
	const rates = [
		model.cost?.input,
		model.cost?.output,
		model.cost?.cacheRead,
		model.cost?.cacheWrite,
		...(model.cost?.tiers ?? []).flatMap((tier) => [
			tier.input,
			tier.output,
			tier.cacheRead,
			tier.cacheWrite,
		]),
	];

	return rates.filter((rate): rate is number => typeof rate === "number" && Number.isFinite(rate));
}

function formatRate(rate: number | undefined): string {
	return rate === undefined ? "unknown" : `$${rate}/1M tokens`;
}

export default function consent(pi: ExtensionAPI) {
	const modelPatterns = configuredModelPatterns();
	const costThreshold = configuredCostThreshold();

	pi.on("input", async (event, ctx) => {
		// Messages sent by another extension are not direct user submissions.
		if (event.source === "extension" || !event.text.trim()) {
			return { action: "continue" };
		}

		const model = ctx.model;
		if (!model) return { action: "continue" };

		const modelText = `${model.id} ${model.name}`.toLowerCase();
		const matchingPattern = modelPatterns.find((pattern) => matchesModelPattern(modelText, pattern));
		const rates = numericRates(model);
		const highestRate = rates.length > 0 ? Math.max(...rates) : undefined;
		const expensiveByCost = highestRate !== undefined && highestRate > 0 && highestRate >= costThreshold;

		if (!matchingPattern && !expensiveByCost) {
			return { action: "continue" };
		}

		const modelLabel = `${model.provider}/${model.name || model.id}`;
		if (!ctx.hasUI) {
			// There is no user available to give consent in print/JSON mode.
			console.error(`[consent] blocked prompt for ${modelLabel}: confirmation requires a UI`);
			return { action: "handled" };
		}

		const reason = matchingPattern
			? `matches the model rule “${matchingPattern}”`
			: `has a published rate up to ${formatRate(highestRate)}`;
		const confirmed = await ctx.ui.confirm(
			"Expensive model consent",
			[
				`Send this prompt to ${modelLabel}?`,
				`This model ${reason}.`,
				`Input: ${formatRate(model.cost?.input)} · Output: ${formatRate(model.cost?.output)}`,
				`Prompt length: ${event.text.length.toLocaleString()} characters.`,
			].join("\n"),
		);

		if (!confirmed) {
			ctx.ui.notify("Prompt cancelled; no request was sent.", "info");
			return { action: "handled" };
		}

		return { action: "continue" };
	});
}
