# Dotfiles

Personal configuration for Neovim, Ghostty, Kitty, Alacritty, tmux, Fish, Nushell, Zsh, VS Code/Cursor, Sublime Text, Git, Herdr, and Pi.

## Install

```sh
./sync
```

`sync` creates absolute symlinks into the standard config locations. It is idempotent and refuses to replace an existing file or directory unless explicitly requested:

```sh
./sync --force
```

Use `./sync --with-platform` to install Rust/eza and the configured Zsh plugins. Other optional tools used by these configs include `fzf`, `fd`, `rg`, `jq`, `starship`, `nvim`, and `hunk`.

## Nushell

The prompt shows the current directory and Git branch. Use Tab for fuzzy path completion; install `carapace` (`brew install carapace` on macOS) for command flags and arguments, including Git aliases.

Press Ctrl-R to search history from the current directory, or Ctrl-Alt-R to search all directories. Both menus show unique commands in order of their most recent use. Run `reload` after changing the configuration.

`wip` stages changes, commits with the message `wip`, and pushes the current branch. It stops and reports an error if any step fails.

## Checks

```sh
bash -n sync ghostty/scripts/session-picker.sh kitty/scripts/session-picker.sh \
  tmux/session-picker.sh tmux/copy-to-clipboard.sh
fish -n fish/config.fish fish/conf.d/vite-plus.fish
nu --env-config nushell/env.nu --config nushell/config.nu --commands 'exit'
nu --env-config nushell/env.nu --config nushell/config.nu --commands 'source nushell/tests.nu'
zsh -n zsh/.zshrc
ghostty +validate-config
nvim --headless -u nvim/init.lua '+qa'
```

The Pi extension under `pi/` has its own usage notes in [`pi/README.md`](pi/README.md). The Vercel palette reference lives in [`themes/vercel/README.md`](themes/vercel/README.md).
