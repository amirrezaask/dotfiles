# Themes

Canonical theme assets live here, separate from app configs under `configs/`.

App-specific paths use **symlinks** so existing tools (`set-theme`, Kitty `include`, Neovim `:colorscheme`) keep working without path changes.

## Layout

```
themes/
  vercel/
    kitty/          # vercel.conf, vercel-light.conf
    ghostty/        # vercel, vercel-light
    nvim/           # vercel.lua, vercel-light.lua
    vscode/         # VS Code / Cursor extension (package.json, themes/*.json, .vsix)
    jetbrains/      # IntelliJ Platform plugin (.zip)
    chrome/         # unpacked extensions + .crx / .pem
```

## Symlinks (into `configs/`)

| App | Symlink | Target |
|-----|---------|--------|
| Kitty | `configs/kitty/themes/vercel*.conf` | `themes/vercel/kitty/` |
| Ghostty | `configs/ghostty/themes/vercel*` | `themes/vercel/ghostty/` |
| Neovim | `configs/nvim/colors/vercel*.lua` | `themes/vercel/nvim/` |
| VS Code | `configs/code/vercel-theme` | `themes/vercel/vscode/` |
| Chrome | `configs/chrome/vercel-theme*` | `themes/vercel/chrome/` |

Palette reference: **[docs/vercel-theme.md](../docs/vercel-theme.md)**.
