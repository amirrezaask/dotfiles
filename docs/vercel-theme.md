# Vercel theme palette

Canonical reference for the **Vercel** color system used across this dotfiles repo.  
Syntax and UI colors follow [gantoreno/vscode-vercel](https://github.com/gantoreno/vscode-vercel) (Vercel’s editor theme), not GitHub’s `github-dark-default`.

**For agents:** Use the token tables below as the single source of truth when adding or updating themes. Terminal/chrome backgrounds stay **pure black** (dark) / **white** (light editor); editor surface in VS Code uses `#0a0a0a` / `#fafafa` chrome per upstream.

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

1. **Terminal background:** Dark Kitty uses **pure black** `#000000`; light uses **white** `#ffffff`.
2. **Syntax:** Pink keywords, purple functions, blue constants/types/properties/components, green strings & HTML tags, default variables in **foreground** (not orange).
3. **Surfaces:** `surface` `#0a0a0a` (dark editor) / `#fafafa` (light chrome); borders `#242424` / `#333333` (dark), `#ebebeb` / `#cccccc` (light).
4. **Source theme:** When in doubt, diff against `themes/vercel/vscode/themes/vercel-dark.json` in this repo (upstream: [gantoreno/vscode-vercel](https://github.com/gantoreno/vscode-vercel)).

---

## Dark palette

| Token | Hex | Use |
|-------|-----|-----|
| `bg` | `#000000` | Terminal, browser chrome, panels |
| `fg` | `#ededed` | Default text, variables, punctuation |
| `muted` | `#a1a1a1` | Comments, hints, inactive UI text |
| `dim` | `#878787` | Inactive line numbers, secondary UI |
| `subtle` | `#676767` | Dim chrome |
| `border` | `#333333` | Borders, widgets |
| `surface` | `#0a0a0a` | Editor background (VS Code), code blocks |
| `surface_alt` | `#242424` | Tab borders, secondary surfaces |
| `cursorline` | `#1a1a1a` | Active line (~`#ffffff1a` on `#0a0a0a`) |
| `selection` | `#333333` | Selection background |
| `cursor` | `#ededed` | Caret |
| `on_accent` | `#000000` | Text on colored badges |

### Syntax (dark)

| Token | Hex | TextMate / Tree-sitter role |
|-------|-----|----------------------------|
| `pink` | `#f05b8d` | Keywords, storage, operators (word) |
| `purple` | `#b675f1` | Functions, methods, JSX attributes |
| `blue` | `#62a6ff` | Constants, types, properties, React components, links, regex |
| `green` | `#58c760` | Strings, HTML tags |
| `string_alt` | `#62a6ff` | Regex, escapes (same as blue in upstream) |
| `orange` | `#f99902` | Warnings, diff change, markdown list markers |
| `red` | `#f56464` | Errors, terminal red |

### ANSI → token (Kitty dark)

| ANSI | Hex | Maps to |
|------|-----|---------|
| 0 | `#000000` | `bg` |
| 1 | `#f56464` | `red` |
| 2 | `#58c760` | `green` |
| 3 | `#f99902` | `orange` |
| 4 | `#62a6ff` | `blue` |
| 5 | `#b675f1` | `purple` |
| 6 | `#14cbb7` | cyan |
| 7 | `#ededed` | `fg` |
| 8 | `#676767` | `subtle` |
| 15 | `#ededed` | bright white |

---

## Light palette

| Token | Hex | Use |
|-------|-----|-----|
| `bg` | `#ffffff` | Editor, terminal |
| `fg` | `#171717` | Default text, variables |
| `muted` | `#666666` | Comments, hints |
| `dim` | `#a8a8a8` | Line numbers |
| `subtle` | `#bbbbbb` | Ignored git, dim chrome |
| `border` | `#cccccc` | Borders |
| `surface` | `#fafafa` | Chrome, inactive tabs |
| `surface_alt` | `#ebebeb` | Section borders |
| `cursorline` | `#f5f5f5` | Active line (~`#0000001a` on white) |
| `selection` | `#cccccc` | Selection |
| `cursor` | `#171717` | Caret |
| `on_accent` | `#fafafa` | Text on colored badges |

### Syntax (light)

| Token | Hex | Role |
|-------|-----|------|
| `pink` | `#b32c62` | Keywords |
| `purple` | `#7200c4` | Functions, JSX attributes |
| `blue` | `#005ee9` | Constants, types, properties, components |
| `green` | `#397c3b` | Strings, HTML tags |
| `string_alt` | `#005ee9` | Links, regex |
| `orange` | `#9e5200` | Warnings, diff change |
| `red` | `#c62128` | Errors |

### ANSI → token (Kitty light)

| ANSI | Hex | Maps to |
|------|-----|---------|
| 0 | `#171717` | dark fg / bright black context |
| 1 | `#c62128` | `red` |
| 2 | `#397c3b` | `green` |
| 3 | `#9e5200` | `orange` |
| 4 | `#005ee9` | `blue` |
| 5 | `#7200c4` | `purple` |
| 6 | `#027d70` | cyan |
| 7 | `#171717` | `fg` |
| 8 | `#666666` | `muted` |
| 15 | `#fafafa` | bright white |

---

## Syntax mapping (all editors)

| Role | Dark | Light |
|------|------|-------|
| Comment | `muted` | `muted` |
| Keyword / storage | `pink` | `pink` |
| Function / method | `purple` | `purple` |
| String | `green` | `green` |
| Number / boolean / constant | `blue` | `blue` |
| Type / class / interface | `blue` | `blue` |
| Variable (general) | `fg` | `fg` |
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

Canonical files live under **`themes/vercel/`** (see [themes/README.md](../themes/README.md)). App configs use **symlinks** at the paths below so Kitty/Neovim/`set-theme` keep working unchanged.

| Program | Canonical path | Symlink (for apps) | Notes |
|---------|----------------|-------------------|-------|
| **Kitty** | `themes/vercel/kitty/vercel.conf` | `configs/kitty/themes/vercel.conf` | `vercel-light.conf`; `include` in `kitty.conf` |
| **Neovim** | `themes/vercel/nvim/vercel.lua` | `configs/nvim/colors/vercel.lua` | Light via `vim.o.background`; `:colorscheme vercel` |
| **VS Code / Cursor** | `themes/vercel/vscode/themes/vercel-dark.json` | `configs/code/vercel-theme` | Extension package in `themes/vercel/vscode/` |
| **JetBrains IDEs** | `themes/vercel/jetbrains/` | — | Plugin: `./gradlew buildPlugin` → install ZIP from `build/distributions/` |
| **Chrome** | `themes/vercel/chrome/vercel-theme/` | `configs/chrome/vercel-theme` | `vercel-theme-light/` for light |
| **Sublime** | `configs/sublime/vercel.sublime-color-scheme` | — | *(still in configs; GitHub palette — update separately if needed)* |

### Switching themes

```bash
./set-theme vercel          # dark
./set-theme vercel-light    # light
```

Environment overrides:

- `NVIM_THEME=vercel` — Neovim colorscheme name (palette follows `'background'`)
- `NVIM_TRANSPARENCY=true` — Neovim `Normal` bg `NONE`

---

## Changelog

| Date | Notes |
|------|-------|
| 2026-06-03 | Add JetBrains Platform plugin (dark + light) |
| 2026-06-03 | Align Kitty, Neovim, VS Code with gantoreno/vscode-vercel palette |
| 2026-06-03 | Initial palette doc (GitHub-based); superseded by row above |
