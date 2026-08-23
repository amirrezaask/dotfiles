# Pi extensions

## `consent`

`consent.ts` asks for confirmation before sending a user prompt to an expensive model. It recognizes Opus and Sol models by name and also gates models whose published input/output/cache rate reaches the configured threshold.

Run `./sync` to link it into `~/.pi/agent/extensions/`, then reload Pi with `/reload`.

Environment overrides:

- `PI_CONSENT_COST_THRESHOLD` — maximum published rate in USD per million tokens before consent is required. Defaults to `10`.
- `PI_CONSENT_MODEL_PATTERNS` — comma-separated model-name patterns. Defaults to `opus,sol`.

In non-interactive modes, the extension blocks expensive prompts because no user can provide consent.
