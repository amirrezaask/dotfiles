#!/usr/bin/env python3
"""
Theme synchronization script for dotfiles
Updates themes across ghostty, neovim, VSCode, and Cursor
"""

import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
from tempfile import NamedTemporaryFile

# Theme mappings dictionary
THEMES = {
    "gruvbox": {
        "ghostty": "Gruvbox Dark Hard",
        "nvim": "gruvbox",
        "vscode": {
            "theme_name": "Gruvbox Dark Hard",
            "extension_id": "jdinhlife.gruvbox"
        },
        "cursor": {
            "theme_name": "Gruvbox Dark Hard",
            "extension_id": "jdinhlife.gruvbox"
        }
    },
    "everforest": {
        "ghostty": "Everforest Dark Hard",
        "nvim": "everforest",
        "vscode": {
            "theme_name": "Everforest Dark",
            "extension_id": "sainnhe.everforest"
        },
        "cursor": {
            "theme_name": "Everforest Dark",
            "extension_id": "sainnhe.everforest"
        }
    },
    "tokyonight": {
        "ghostty": "Tokyo Night",
        "nvim": "tokyonight",
        "vscode": {
            "theme_name": "Tokyo Night",
            "extension_id": "enkia.tokyo-night"
        },
        "cursor": {
            "theme_name": "Tokyo Night",
            "extension_id": "enkia.tokyo-night"
        }
    },
    "poimandres": {
        "ghostty": "poimandres",
        "nvim": "poimandres",
        "vscode": {
            "theme_name": "Poimandres",
            "extension_id": "olivercederborg.poimandres-theme"
        },
        "cursor": {
            "theme_name": "poimandres",
            "extension_id": "flvffy.poimandres"
        }
    }
}

SUPPORTED_THEMES = list(THEMES.keys())


def find_dotfiles_dir():
    """Find the dotfiles directory by following ghostty config symlink"""
    # Check DOTFILES_DIR environment variable first
    if os.environ.get("DOTFILES_DIR"):
        dotfiles_dir = Path(os.environ["DOTFILES_DIR"])
        if (dotfiles_dir / "configs" / "ghostty" / "config").exists():
            return dotfiles_dir
    
    # Try to find by following ghostty directory symlink
    ghostty_paths = [
        Path.home() / ".config" / "ghostty",
    ]
    
    for ghostty_dir in ghostty_paths:
        if ghostty_dir.exists() or ghostty_dir.is_symlink():
            try:
                # Resolve symlink to get actual path (e.g., /path/to/dotfiles/configs/ghostty)
                resolved = ghostty_dir.resolve()
                # Work backwards: resolved path is configs/ghostty
                # So dotfiles_dir is resolved.parent.parent
                if "configs" in resolved.parts and "ghostty" in resolved.parts:
                    # Find the index of "configs" and go up one level
                    configs_index = resolved.parts.index("configs")
                    dotfiles_dir = Path(*resolved.parts[:configs_index])
                    if (dotfiles_dir / "configs" / "ghostty" / "config").exists():
                        return dotfiles_dir
            except (OSError, ValueError):
                # If symlink is broken or path issues, continue
                continue
    
    # Fallback: Check common locations
    for path in [
        Path.home() / "dev" / "dotfiles",
        Path.home() / ".dotfiles",
        Path.cwd()
    ]:
        if (path / "configs" / "ghostty" / "config").exists():
            return path
    
    return None


def update_ghostty_config(config_path, theme_name):
    """Update ghostty config file"""
    if not config_path.exists():
        print(f"Error: Ghostty config not found at {config_path}", file=sys.stderr)
        return False
    
    content = config_path.read_text()
    
    # Replace existing theme line or add new one
    if re.search(r"^theme\s*=", content, re.MULTILINE):
        content = re.sub(
            r"^theme\s*=.*$",
            f"theme = {theme_name}",
            content,
            flags=re.MULTILINE
        )
    else:
        # Add after maximize line or at the end
        if re.search(r"^maximize=", content, re.MULTILINE):
            content = re.sub(
                r"^(maximize=.*)$",
                rf"\1\ntheme = {theme_name}",
                content,
                flags=re.MULTILINE
            )
        else:
            content += f"\ntheme = {theme_name}\n"
    
    config_path.write_text(content)
    print(f"✓ Updated ghostty theme to: {theme_name}")
    return True


def update_nvim_config(config_path, theme_name):
    """Update neovim config file"""
    if not config_path.exists():
        print(f"Error: Neovim config not found at {config_path}", file=sys.stderr)
        return False
    
    content = config_path.read_text()
    
    # Replace colorscheme line
    content = re.sub(
        r'^vim\.cmd\.colorscheme\(.*?\)',
        f'vim.cmd.colorscheme("{theme_name}")',
        content,
        flags=re.MULTILINE
    )
    
    config_path.write_text(content)
    print(f"✓ Updated neovim theme to: {theme_name}")
    return True


def update_json_config(config_path, theme_name):
    """Update JSON config file (VSCode/Cursor)"""
    if not config_path.exists():
        print(f"Error: Config not found at {config_path}", file=sys.stderr)
        return False
    
    try:
        # Read and parse JSON
        content = config_path.read_text()
        # Remove trailing commas before parsing
        content = re.sub(r',(\s*[}\]])', r'\1', content)
        config = json.loads(content)
        
        # Update theme
        config["workbench.colorTheme"] = theme_name
        
        # Write back
        config_path.write_text(json.dumps(config, indent=2) + "\n")
        return True
    except (json.JSONDecodeError, KeyError) as e:
        print(f"Error updating JSON config: {e}", file=sys.stderr)
        return False


def is_theme_installed(app, theme):
    """Check if theme is installed (flag file exists)"""
    flag_file = Path.home() / f".{app}-theme-{theme}-installed"
    return flag_file.exists()


def mark_theme_installed(app, theme):
    """Mark theme as installed (create flag file)"""
    flag_file = Path.home() / f".{app}-theme-{theme}-installed"
    flag_file.touch()
    return True


def install_extension(app, extension_id, theme):
    """Install extension using app CLI"""
    app_cmd = "code" if app == "vscode" else "cursor"
    
    if not shutil.which(app_cmd):
        return False
    
    # Check if already installed
    app_display = "VSCode" if app == "vscode" else "Cursor"
    if is_theme_installed(app, theme):
        print(f"  {app_display} extension already installed: {extension_id}")
        return True
    
    # Install extension
    print(f"  Installing {app_display} extension: {extension_id}")
    result = subprocess.run(
        [app_cmd, "--install-extension", extension_id, "--force"],
        capture_output=True,
        text=True
    )
    
    if result.returncode == 0:
        mark_theme_installed(app, theme)
        return True
    
    return False


def update_vscode(dotfiles_dir, theme_data, theme_key):
    """Update VSCode config and install extension"""
    config_path = dotfiles_dir / "configs" / "code" / "settings.json"
    theme_name = theme_data["vscode"]["theme_name"]
    extension_id = theme_data["vscode"]["extension_id"]
    
    if not update_json_config(config_path, theme_name):
        return False
    
    print(f"✓ Updated VSCode theme to: {theme_name}")
    install_extension("vscode", extension_id, theme_key)
    return True


def update_cursor(dotfiles_dir, theme_data, theme_key):
    """Update Cursor config and install extension"""
    config_path = dotfiles_dir / "configs" / "cursor" / "settings.json"
    theme_name = theme_data["cursor"]["theme_name"]
    extension_id = theme_data["cursor"]["extension_id"]
    
    if not update_json_config(config_path, theme_name):
        return False
    
    print(f"✓ Updated Cursor theme to: {theme_name}")
    install_extension("cursor", extension_id, theme_key)
    return True


def list_themes():
    """List available themes"""
    print("Available themes:")
    for theme in SUPPORTED_THEMES:
        print(f"  - {theme}")


def main():
    """Main function"""
    if len(sys.argv) > 1 and sys.argv[1] in ["--list", "-l"]:
        list_themes()
        return 0
    
    if len(sys.argv) < 2:
        list_themes()
        return 0
    
    theme = sys.argv[1]
    
    # Validate theme
    if theme not in SUPPORTED_THEMES:
        print(f"Error: '{theme}' is not a supported theme.", file=sys.stderr)
        print("", file=sys.stderr)
        list_themes()
        return 1
    
    # Find dotfiles directory
    dotfiles_dir = find_dotfiles_dir()
    if not dotfiles_dir:
        print("Error: Could not find dotfiles directory", file=sys.stderr)
        print("Please ensure your dotfiles are in ~/dev/dotfiles or set DOTFILES_DIR environment variable", file=sys.stderr)
        return 1
    
    theme_data = THEMES[theme]
    
    print(f"Changing theme to: {theme}")
    print("")
    
    # Update all configs
    success = True
    
    # Ghostty
    ghostty_config = dotfiles_dir / "configs" / "ghostty" / "config"
    if not update_ghostty_config(ghostty_config, theme_data["ghostty"]):
        success = False
    
    # Neovim
    nvim_config = dotfiles_dir / "configs" / "nvim" / "init.lua"
    if not update_nvim_config(nvim_config, theme_data["nvim"]):
        success = False
    
    # VSCode
    if not update_vscode(dotfiles_dir, theme_data, theme):
        success = False
    
    # Cursor
    if not update_cursor(dotfiles_dir, theme_data, theme):
        success = False
    
    if success:
        print("")
        print(f"✓ Theme changed to: {theme}")
        print("")
        print("Note: Restart ghostty, neovim, VSCode, and Cursor for changes to take effect.")
        return 0
    else:
        return 1


if __name__ == "__main__":
    sys.exit(main())

