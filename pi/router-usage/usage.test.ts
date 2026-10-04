import assert from "node:assert/strict";
import test from "node:test";
import { compactUsage, isBlocked, isUnavailable, parseCache, parseSnapshot } from "./usage.ts";

const snapshot = {
	data: [
		{
			subId: "sub_1",
			name: "codex-a@example.com",
			type: "chatgpt-oauth",
			enabled: true,
			cooldownUntil: null,
			usage: {
				provider: "chatgpt-oauth",
				fetchedAt: 1_790_038_518_133,
				plan: "prolite",
				allowed: false,
				limitReached: true,
				windows: [{ id: "primary", usedPercent: 100, windowSeconds: 604_800, resetAt: 1_790_117_475_000 }],
			},
		},
		{
			subId: "sub_2",
			name: "opencode-free",
			type: "opencode-free",
			enabled: false,
			cooldownUntil: null,
			usage: null,
		},
	],
};

test("parses the router /api/usage payload", () => {
	const rows = parseSnapshot(snapshot);
	assert.equal(rows.length, 2);
	assert.equal(rows[0]!.name, "codex-a@example.com");
	assert.equal(rows[0]!.usage?.plan, "prolite");
	assert.equal(rows[0]!.usage?.windows[0]?.usedPercent, 100);
	assert.equal(rows[1]!.usage, null);
	assert.equal(rows[1]!.enabled, false);
});

test("tolerates malformed payloads", () => {
	assert.deepEqual(parseSnapshot(null), []);
	assert.deepEqual(parseSnapshot({ data: "nope" }), []);
	assert.deepEqual(parseSnapshot({ data: [{ name: "missing subId" }] }), []);
	const [row] = parseSnapshot({ data: [{ subId: "s", name: "n", usage: { fetchedAt: "soon" } }] });
	assert.equal(row?.usage, null);
});

test("formats usage like the codex-mux widget", () => {
	const now = 1_790_000_000_000;
	const rows = parseSnapshot(snapshot);
	assert.equal(
		compactUsage(rows[0]!.usage, now),
		"100% wk (resets in 1.4d) · prolite · limit reached",
	);
	assert.equal(compactUsage(null), "?");
});

test("compact relative reset times scale correctly", () => {
	const now = 1_700_000_000_000;
	const usage = (resetAt: number | null) => ({
		provider: "x",
		fetchedAt: now,
		windows: [{ id: "primary", usedPercent: 42, windowSeconds: 18_000, resetAt }],
	});
	assert.equal(compactUsage(usage(now + 90 * 60_000), now), "42% 5h (resets in 1.5h)");
	assert.equal(compactUsage(usage(now + 45 * 60_000), now), "42% 5h (resets in 45m)");
	assert.equal(compactUsage(usage(now - 1_000), now), "42% 5h (resets now)");
	assert.equal(compactUsage(usage(null), now), "42% 5h");
});

test("empty windows fall back to plan or ok", () => {
	const now = Date.now();
	assert.equal(compactUsage({ provider: "x", fetchedAt: now, windows: [] }), "ok");
	assert.equal(compactUsage({ provider: "x", fetchedAt: now, plan: "free", windows: [] }), "free");
	assert.equal(
		compactUsage({ provider: "x", fetchedAt: now, allowed: false, windows: [] }, now),
		"limit reached",
	);
});

test("detects limits and cooldowns", () => {
	const now = 1_000;
	const rows = parseSnapshot(snapshot);
	assert.equal(isBlocked(rows[0]!.usage), true);
	assert.equal(isUnavailable(rows[0]!, now), true);
	assert.equal(isBlocked(rows[1]!.usage), false);
	assert.equal(isUnavailable({ ...rows[1]!, cooldownUntil: now + 1 }, now), true);
	assert.equal(isUnavailable({ ...rows[1]!, cooldownUntil: now - 1 }, now), false);
});

test("round-trips through the cache file shape", () => {
	const cache = parseCache({ version: 1, capturedAt: 42, rows: snapshot.data });
	assert.equal(cache?.capturedAt, 42);
	assert.equal(cache?.rows.length, 2);
	assert.equal(parseCache({ version: 2 }), undefined);
	assert.equal(parseCache("junk"), undefined);
});
