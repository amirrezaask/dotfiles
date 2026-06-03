# Vercel theme palette

Canonical reference for the **Vercel** color system used across this dotfiles repo.  
Based on [Vercel docs](https://vercel.com/docs) / [Geist](https://vercel.com/font) styling and Shiki themes **`github-dark-default`** (dark) and **`github-light-default`** (light).

**For agents:** Use the token tables below as the single source of truth when adding or updating themes. Do not invent new accent colors unless the user asks — keep pure black / white backgrounds and map syntax roles to the named tokens.

---

## Typography

| Role | Font |
|------|------|
| Code (terminal, editors) | **Geist Mono** |
| UI / prose (optional) | **Geist Sans** |

Install (macOS):

```bash
brew install --cask font-geist-mono
brew install --cask font-geist   # optional, UI only
```

---

## Design principles

1. **Backgrounds:** Dark mode uses **pure black** `#000000`, not GitHub’s `#0d1117`. Light mode uses **pure white** `#ffffff`.
2. **Syntax:** Match Vercel docs code blocks — pink keywords, purple functions, blue variables/properties, green strings & HTML tags, blue React components (PascalCase).
3. **Surfaces:** Use `surface` / `border` only for subtle UI chrome (line highlight, gutters, markdown blocks). Floating UI in Neovim should match `bg` (transparent or pure black).
4. **Source theme:** When in doubt, check Shiki `github-dark-default` / `github-light-default` token colors.

---

## Dark palette

| Token | Hex | Use |
|-------|-----|-----|
| `bg` | `#000000` | Editor, terminal, browser chrome, panels |
| `fg` | `#e6edf3` | Default text, punctuation |
| `muted` | `#8b949e` | Comments, hints, inactive UI text |
| `dim` | `#6e7681` | Secondary UI |
| `subtle` | `#484f58` | Line numbers, dim chrome |
| `border` | `#30363d` | Borders, selection (solid), widgets |
| `surface` | `#0d1117` | Line highlight alt, code blocks (non-float) |
| `surface_alt` | `#161b22` | Incognito / secondary surfaces |
| `cursorline` | `#111111` | Active line, tab lift (Chrome) |
| `selection` | `#333333` | Selection background |
| `cursor` | `#ffffff` | Caret |
| `on_accent` | `#000000` | Text on colored badges (diagnostics) |

### Syntax (dark)

| Token | Hex | TextMate / Tree-sitter role |
|-------|-----|----------------------------|
| `pink` | `#ff7b72` | Keywords, `export`, `const`, `await`, `return` |
| `purple` | `#d2a8ff` | Functions, methods, JSX attributes (`key`, `fallback`) |
| `blue` | `#79c0ff` | Variables (members), properties, constants, React components, links |
| `green` | `#7ee787` | Strings, HTML tags (`div`, `ul`), diff add |
| `string_alt` | `#a5d6ff` | Regex, escapes, markdown links |
| `orange` | `#ffa657` | Types, general `variable` bindings, diff change |
| `red` | `#ffa198` | Errors, diff delete, invalid |

### ANSI → token (Kitty dark)

| ANSI | Hex | Maps to |
|------|-----|---------|
| 0 | `#000000` | `bg` |
| 1 | `#ff7b72` | `pink` |
| 2 | `#7ee787` | `green` |
| 3 | `#ffa657` | `orange` |
| 4 | `#79c0ff` | `blue` |
| 5 | `#d2a8ff` | `purple` |
| 6 | `#a5d6ff` | `string_alt` |
| 7 | `#e6edf3` | `fg` |
| 8 | `#8b949e` | `muted` |
| 15 | `#ffffff` | bright white |

### Chrome dark (RGB)

| UI | RGB | Hex |
|----|-----|-----|
| Frame / inactive tabs bg | `0, 0, 0` | `#000000` |
| Toolbar / active tab bg | `17, 17, 17` | `#111111` |
| Active tab text | `255, 255, 255` | `#ffffff` |
| Inactive tab text | `139, 148, 158` | `#8b949e` |
| Tab indicator | `121, 192, 255` | `#79c0ff` |

---

## Light palette

| Token | Hex | Use |
|-------|-----|-----|
| `bg` | `#ffffff` | Editor, terminal, browser chrome |
| `fg` | `#1f2328` | Default text, punctuation |
| `muted` | `#6e7781` | Comments, hints |
| `dim` | `#57606a` | Secondary UI |
| `subtle` | `#8c959f` | Line numbers |
| `border` | `#d0d7de` | Borders, selection |
| `surface` | `#f6f8fa` | Line highlight, inactive tabs |
| `surface_alt` | `#eaeef2` | Secondary surfaces |
| `cursorline` | `#f6f8fa` | Active line |
| `selection` | `#dddddd` | Selection |
| `cursor` | `#1f2328` | Caret |
| `on_accent` | `#ffffff` | Text on colored badges |

### Syntax (light)

| Token | Hex | Role |
|-------|-----|------|
| `pink` | `#cf222e` | Keywords |
| `purple` | `#8250df` | Functions, JSX attributes |
| `blue` | `#0550ae` | Constants, properties, React components |
| `green` | `#116329` | HTML tags, diff add |
| `string` | `#0a3069` | Strings |
| `string_alt` | `#0550ae` | Links, regex |
| `orange` | `#953800` | Types, variables |
| `red` | `#82071e` | Errors |

### ANSI → token (Kitty light)

| ANSI | Hex | Maps to |
|------|-----|---------|
| 0 | `#ffffff` | `bg` |
| 1 | `#cf222e` | `pink` |
| 2 | `#116329` | `green` |
| 3 | `#953800` | `orange` |
| 4 | `#0550ae` | `blue` |
| 5 | `#8250df` | `purple` |
| 6 | `#0a3069` | `string` |
| 7 | `#1f2328` | `fg` |
| 8 | `#6e7781` | `muted` |

### Chrome light (RGB)

| UI | RGB | Hex |
|----|-----|-----|
| Frame / editor | `255, 255, 255` | `#ffffff` |
| Inactive tabs bg | `246, 248, 250` | `#f6f8fa` |
| Active tab text | `31, 35, 40` | `#1f2328` |
| Inactive tab text | `110, 119, 129` | `#6e7781` |
| Tab indicator | `9, 105, 218` | `#0969da` |

---

## Syntax mapping (all editors)

Use this table when adding highlight rules to a new tool:

| Role | Dark | Light |
|------|------|-------|
| Comment | `muted` | `muted` |
| Keyword / storage | `pink` | `pink` |
| Function / method | `purple` | `purple` |
| String | `green` (dark) / `string` (light) | `#7ee787` / `#0a3069` |
| Number / boolean / constant | `blue` | `blue` |
| Type / class | `orange` | `orange` |
| Variable (general) | `orange` | `orange` |
| Variable / parameter (plain) | `fg` | `fg` |
| Property / member | `blue` | `blue` |
| HTML tag (lowercase) | `green` | `green` |
| React component (PascalCase) | `blue` | `blue` |
| JSX attribute name | `purple` | `purple` |
| Import identifier | `fg` | `fg` |
| Punctuation / operator | `fg` | `fg` |

**Neovim TSX:** PascalCase tags use `@constructor` → `blue`; lowercase tags use `@tag` → `green`. Queries: `configs/nvim/queries/{tsx,jsx}/highlights.scm`.

---

## Implementations in this repo

| Program | Dark | Light | Notes |
|---------|------|-------|-------|
| **Kitty** | `configs/kitty/themes/vercel.conf` | `vercel-light.conf` | `include` in `kitty.conf` |
| **Ghostty** | Geist Mono + theme via `set-theme` | same | `configs/ghostty/config` |
| **Neovim** | `configs/nvim/colors/vercel.lua` | auto (`vim.o.background`) | `:colorscheme vercel` |
| **Sublime** | `configs/sublime/vercel.sublime-color-scheme` | `vercel-light.sublime-color-scheme` | `dark_color_scheme` / `light_color_scheme` in Preferences |
| **VS Code / Cursor** | `configs/code/vercel-theme/themes/vercel-dark.json` | `vercel-light.json` | Extension: `dotfiles.vercel-theme-1.0.0` |
| **Chrome** | `configs/chrome/vercel-theme/` | `vercel-theme-light/` | Load unpacked in `chrome://extensions` |

### Switching themes

```bash
# Kitty + Neovim (from repo root)
./set-theme vercel          # dark
./set-theme vercel-light    # light (Neovim still uses `vercel` + background)
```

Environment overrides:

- `NVIM_THEME=vercel` — Neovim colorscheme name (always `vercel`; palette follows `'background'`)
- `NVIM_TRANSPARENCY=true` — Neovim `Normal` bg `NONE` (terminal black shows through)

---

## Adding a new program (agent checklist)

1. Read **Syntax mapping** and pick hex from **Dark** or **Light** tables — do not duplicate hex values with typos.
2. Keep editor/terminal **background** = `bg` (`#000000` / `#ffffff`).
3. Map UI chrome to `border`, `surface`, `muted` — not random grays.
4. Put new theme files under `configs/<app>/` and add a row to **Implementations** above.
5. If the tool supports semantic tokens, mirror the **Syntax (dark)** table in `semanticTokenColors` (see VS Code theme).
6. Prefer **Geist Mono** for monospace UI.

---

## Changelog

| Date | Notes |
|------|-------|
| 2026-06-03 | Initial palette doc; themes for Kitty, Neovim, Chrome, Sublime, VS Code |
