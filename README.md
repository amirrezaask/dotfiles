# Dotfiles
My personal computing experience.

## Installation
```bash
git clone https://github.com/amirreza/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./sync
```

### Supported Platforms
- [Ubuntu](https://ubuntu.com/)
- [Fedora](https://fedoraproject.org/)
- [macOS](https://www.apple.com/os/macos/)

## Theme Management

This dotfiles repository includes a theme synchronization function that keeps your terminal (Ghostty) and Neovim in sync.

### Changing Themes

Use the `theme` function in zsh to change themes:

```bash
theme <theme-name>
theme --list
```

### Available Themes

- `gruvbox` - Gruvbox Dark Hard (default)
- `everforest` - Everforest Dark Hard
- `tokyonight` - Tokyo Night
- `poimandres` - Poimandres

### List Available Themes

```bash
theme --list
```

The function automatically updates:
- Ghostty terminal configuration (`configs/ghostty/config`)
- Neovim configuration (`configs/nvim/init.lua`)
- VSCode configuration (`configs/code/settings.json`)
- Cursor configuration (`configs/cursor/settings.json`)

The function also automatically installs the required theme extensions in VSCode and Cursor using their command-line interfaces (`code --install-extension` and `cursor --install-extension`).

**Note:** After changing themes, restart Ghostty, Neovim, VSCode, and Cursor for changes to take effect. The `code` and `cursor` CLI commands must be installed for extension installation to work.

## Screenshots

![Ubuntu](https://raw.githubusercontent.com/amirrezaask/dotfiles/refs/heads/master/.screenshots/screenshot1.png)
![Fedora](https://raw.githubusercontent.com/amirrezaask/dotfiles/refs/heads/master/.screenshots/screenshot-fedora.png)
![Nord](https://raw.githubusercontent.com/amirrezaask/dotfiles/refs/heads/master/.screenshots/nord.png)
![macOS](https://raw.githubusercontent.com/amirrezaask/dotfiles/refs/heads/master/.screenshots/macos.png)

