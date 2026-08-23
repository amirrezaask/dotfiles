# Dotfiles

Personal configuration for Neovim, Ghostty, Kitty, Alacritty, tmux, Fish, Zsh, VS Code/Cursor, Sublime Text, Git, Herdr, and Pi.

## Install

```sh
./sync
```

`sync` creates absolute symlinks into the standard config locations. It is idempotent and refuses to replace an existing file or directory unless explicitly requested:

```sh
./sync --force
```

Use `./sync --with-platform` to install Rust/eza and the configured Zsh plugins. Other optional tools used by these configs include `fzf`, `fd`, `rg`, `jq`, `starship`, `nvim`, and `hunk`.

## Checks

```sh
bash -n sync ghostty/scripts/session-picker.sh kitty/scripts/session-picker.sh \
  tmux/session-picker.sh tmux/copy-to-clipboard.sh
fish -n fish/config.fish fish/conf.d/vite-plus.fish
zsh -n zsh/.zshrc
ghostty +validate-config
nvim --headless -u nvim/init.lua '+qa'
```

The Pi extension under `pi/` has its own usage notes in [`pi/README.md`](pi/README.md). The Vercel palette reference lives in [`themes/vercel/README.md`](themes/vercel/README.md).
