import assert from "node:assert/strict";
import test from "node:test";
import { accountIdFromToken, compactUsage, emailFromToken, parseUsagePayload, usageScore } from "./usage.ts";

function token(payload: unknown): string {
	return `x.${Buffer.from(JSON.stringify(payload)).toString("base64url")}.x`;
}

test("parses Codex rolling limits", () => {
	const usage = parseUsagePayload({
		plan_type: "pro",
		rate_limit: {
			primary_window: { used_percent: 23.4, limit_window_seconds: 18_000, reset_at: 2_000_000_000 },
			secondary_window: { used_percent: 67, limit_window_seconds: 604_800, reset_at: 2_000_000_001 },
		},
	});
	assert.equal(usage.planType, "pro");
	assert.equal(
		compactUsage(usage, 1_999_996_400_000),
		"23% 5h (resets in 1h) / 67% 7d (resets in 1h)",
	);
	assert.equal(usageScore(usage), 67);
});

test("uses the provider's actual window duration", () => {
	const usage = parseUsagePayload({
		rate_limit: {
			primary_window: { used_percent: 42, limit_window_seconds: 604_800 },
		},
	});
	assert.equal(compactUsage(usage), "42% 7d");
});

test("shows compact relative reset times", () => {
	const now = 1_700_000_000_000;
	const usage = parseUsagePayload({
		rate_limit: {
			primary_window: { used_percent: 42, limit_window_seconds: 18_000, reset_at: now / 1000 + 90 * 60 },
			secondary_window: { used_percent: 8, limit_window_seconds: 604_800, reset_at: now / 1000 - 1 },
		},
	});
	assert.equal(compactUsage(usage, now), "42% 5h (resets in 1.5h) / 8% 7d (resets now)");
});

test("exhausted accounts sort last until reset", () => {
	const usage = parseUsagePayload({
		rate_limit: {
			primary_window: { used_percent: 100, reset_at: Math.ceil(Date.now() / 1000) + 60 },
		},
	});
	assert.equal(usageScore(usage), Number.POSITIVE_INFINITY);
});

test("extracts account metadata from the OAuth access token", () => {
	const access = token({
		"https://api.openai.com/auth": { chatgpt_account_id: "acct_123" },
		"https://api.openai.com/profile": { email: "person@example.com" },
	});
	assert.equal(accountIdFromToken(access), "acct_123");
	assert.equal(emailFromToken(access), "person@example.com");
	assert.equal(accountIdFromToken("invalid"), undefined);
	assert.equal(emailFromToken("invalid"), undefined);
});
