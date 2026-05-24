# Sublime Text Project Picker

A Sublime Text plugin inspired by the kitty session-picker.sh script.

## Features

- **Project Picker** (`Command+Ctrl+P`): Shows a quick panel with all git projects from `~/dev`.
  - If a project is already open in a Sublime window, it will focus that window
  - If not open, it creates a new window with the project
  - First option is always "scratch" (opens `~/scratch`)

- **Project Picker (Current Window)** (`Command+Ctrl+Shift+P`): Same as above but opens in the current window instead of creating a new one

## Commands

The following commands are available in the Command Palette:

- `Project Picker` - Shows the project picker dialog
- `Project Picker: Current Window` - Shows the picker to open in current window
- `Project Picker: Scratch` - Quick access to scratch directory

## How It Works

1. Scans `~/dev` for git repositories (up to depth 3)
2. Shows a sorted list in Sublime's quick panel
3. Checks if any open window already has that project/folder
4. If found, brings that window to front
5. If not found, creates a new window with the project folder

## Configuration

The plugin looks for projects in `~/dev` and uses `~/scratch` for the scratch directory. These paths can be modified by editing the plugin file (`project_picker.py`).

## Key Bindings

- `Command+Ctrl+P` - Open project picker (new window)
- `Command+Ctrl+Shift+P` - Open project picker (current window)

Edit `Default (OSX).sublime-keymap` to customize these bindings.
