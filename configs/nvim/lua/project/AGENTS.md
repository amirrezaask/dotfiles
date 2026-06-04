# Neovim `project` plugin — agent rebuild spec

Use this document to **reimplement or refactor** the per-project context plugin at `configs/nvim/lua/project.lua` (module `require("project")`). The implementation is a **single Lua file** (~1.6k lines); keep it self-contained unless the user asks to split it.

## Product summary

Multi-repo Neovim workflow for a fixed `projects_dir` (e.g. `~/dev`) plus a `scratch_dir`:

- **Auto project context** on buffer focus: detect git-style root, `cd` to root, hide other projects’ buffers from `:buffers`.
- **Explicit switch** (`:P` / `vim.ui.select`): save leaving project, `cd`, open last file + cursor, restore jumplist, isolate buffers.
- **Persist** last file, cursor, jumplist to `stdpath("data")/project-state.json` — **in memory at runtime**, flush periodically and on exit.
- **Per-project terminal** with layout config (`:Pt`).

Paths outside `projects_dir` / `scratch_dir` (e.g. Go stdlib) must **not** become managed projects.

## File layout

| Path | Role |
|------|------|
| `configs/nvim/lua/project.lua` | Entire plugin (`return M`) |
| `configs/nvim/lua/project/AGENTS.md` | This spec |
| `configs/nvim/init.lua` | `require("project").setup { ... }` + keymaps |

Runtime state file: `vim.fn.stdpath("data") .. "/project-state.json"`.

## Integration (init.lua)

```lua
local project = require("project")

project.setup {
  projects_dir = os.getenv("PROJECTS_DIR") or vim.fn.expand("~/dev"),
  scratch_dir = os.getenv("SCRATCH_DIR") or vim.fn.expand("~/scratch"),
  max_depth = 3,
}

vim.keymap.set("n", "<leader>p", project.switch, { desc = "Switch project" })
vim.keymap.set("n", "<leader>ft", project.terminal_toggle, { desc = "Toggle project terminal" })
```

User may use **Snacks.nvim** (`picker.ui_select = true`), which replaces `vim.ui.select`. Picker callback is already `vim.schedule`’d by Snacks; still wrap `on_root_change` in `vim.schedule` when called from the select callback.

## Configuration (`project.Config`)

| Field | Default | Purpose |
|-------|---------|---------|
| `projects_dir` | `~/dev` | Scan for git repos |
| `scratch_dir` | `~/scratch` | Always listed first in picker; always managed |
| `max_depth` | `3` | Max depth for discovery (`depth = max_depth + 1` for fd/find) |
| `store_path` | nil | Override JSON path |
| `terminal` | split botright 15 | See `TerminalConfig` |

## Managed root rules (`M.is_managed_root`)

1. **Scratch**: `scratch_dir` and any path under `scratch_dir/`.
2. **Repo under projects_dir**: path must be under `projects_dir/`, and `vim.fs.root(root, root_patterns)` must equal `root` (normalized).

`root_patterns`: `.git`, `package.json`, `go.mod`, `Cargo.toml`.

**Never** treat `…/repo/.git` as the project root.

## Project discovery (must NOT use `vim.fs.find` in Lua)

Scan is **slow** in Lua on large trees. Use shell:

1. **`fd`** if executable: `fd -H -t d -d <depth> -g '.git' <dir>`
   - Use **`-g '.git'`** — plain `.git` matches `.github`.
2. Else **`find`**: `find <dir> -name .git -type d -maxdepth <depth>`

Convert each hit with `git_dir_to_root`:

- Strip trailing slashes from path.
- If path ends with `/.git`, parent via `fnamemodify(..., ":h")`.
- **Critical**: `fnamemodify('…/repo/.git/', ':h')` returns `…/repo/.git` — strip `/` before `:h`.

Dedupe roots, sort (scratch first, then case-insensitive). Picker labels via `project_label` (relative path under `projects_dir`, or `"scratch"`).

## State model

### On disk (`project-state.json`)

```json
{
  "version": 1,
  "projects": {
    "/abs/path/to/repo": {
      "last_file": "/abs/path/file.go",
      "cursor": { "lnum": 42, "col": 0 },
      "jumplist": {
        "pos": 3,
        "entries": [{ "file": "...", "lnum": 10, "col": 0, "coladd": 0 }],
        "jumps_blob": "<base64 mpack of nvim_get_context().jumps>"
      }
    }
  }
}
```

### In memory

- `state` — loaded once in `setup` via `M.load_state()`.
- `state_dirty` — set on `M.set_project`; cleared on successful `M.persist_state`.
- **Do not** read JSON on every `get_project` / picker open.

### Persistence triggers

| When | Action |
|------|--------|
| `setup` after `load_state` | `persist_state(true)` if migration/repair marked dirty |
| `vim.uv` timer every `PERSIST_INTERVAL_MS` (5000) | `persist_state(false)` |
| `VimLeavePre` | `save_state(active_root)` then `persist_state(true)` |
| `:ProjectSave` | `save_state` + `persist_state(true)` |

**Fast events**: if `vim.in_fast_event()`, reschedule `persist_state` with `vim.schedule` (timer callbacks are fast events — `mkdir` / `writefile` fail with E5560 otherwise).

### Load / migrate

1. Read `project-state.json`; repair Neovim 0.12 indent bug (`repair_json_twos_indent` — literal `"2"` per level).
2. Else migrate legacy `project-last-files.json` (map of root → path or entry).
3. `prune_unmanaged_projects` on load only (not every flush — `is_managed_root` is expensive).
4. `normalize_entry` / `normalize_jumplist` on read.

## Jumplist

### Capture (`M.capture_jumplist`)

- **entries** + **pos**: from `vim.fn.getjumplist()`, only filereadable files, map jump index → filtered pos.
- **jumps_blob**: `vim.base64.encode(vim.mpack.encode(vim.api.nvim_get_context({ types = { "jumps" } }).jumps))`.

Return nil only if both are empty.

### Restore (`M.restore_jumplist(jumplist, opts)`)

Priority:

1. **`jumps_blob`** → decode mpack → `clearjumps` → `nvim_load_context({ jumps = chunks })`. Fast; use for project switch.
2. **Legacy `jumps_b64`** → single blob `nvim_load_context`.
3. **entries** → slow path: `keepjumps edit` each file; if `opts.stay_put`, end on `opts.file` + `apply_cursor`.

After blob restore with `stay_put`: re-`apply_cursor` (load_context does not move buffer). Without `stay_put`: `restore_jump_position(jumplist.pos)`.

**Do not** no-op `stay_put` — that leaves the previous project’s jumplist.

### Debounced jumplist write

`CursorHold` / `CursorHoldI` → `persist_jumplist` (debounce `JUMPLIST_DEBOUNCE_MS` 1500). Skip when `lock`.

## Concurrency (`lock`)

`lock` blocks re-entrant `on_root_change`, `chdir_to_buffer`, `record`, `persist_jumplist` during switch.

**Switch sequence** (`M.on_root_change`):

1. Bail if `lock`, same root, or unmanaged `to_root` (notify).
2. `lock = true`; `pcall` sync work:
   - `save_state(from_root)` if leaving
   - `active_root = to_root`; `cd` if needed
   - `isolate_buffers(to_root, { close_wins = true })` **synchronously** (prevents BufEnter from reverting cwd before other wins close)
   - If `open_last`: `edit` last file or `enew`, `apply_cursor`
3. `lock = false` **before** scheduled jumplist restore
4. `vim.schedule` → `restore_jumplist` only (no second isolate pass required if already closed)

`:P` callback: `vim.schedule(function() M.on_root_change(active_root, choice.project, { open_last = true }) end)`.

## Buffer / project tagging

- `M.path_root(bufnr)` — filesystem root via `root_patterns`.
- `M.buffer_root(bufnr)` — `vim.b[bufnr].project_root` if managed, else path root.
- `M.assign_buffer_project` — terminals / empty buffers inherit `active_root`.
- `M.isolate_buffers(root)` — set `buflisted` only for buffers in `root`; optionally close windows showing other managed roots.
- `M.should_track` — normal buftype, named readable file (no `://`).

Autocmds (`augroup project`, `clear = true`):

| Event | Behavior |
|-------|----------|
| `BufNew`, `TermOpen` | `assign_buffer_project` |
| `BufDelete` | clear `primary_terminal[buf]` |
| `BufEnter`, `BufWinEnter` | if not `lock`: on `BufEnter` → `chdir_to_buffer`, `record`; then `isolate_buffers(active_root)` |
| `VimLeavePre` | save + persist |
| `CursorHold`, `CursorHoldI` | `persist_jumplist(active_root)` |
| `VimEnter` once | `schedule(restore_startup)` |

`restore_startup`: detect initial managed root; if `argc(-1) == 0`, `on_root_change(nil, root, { open_last = true })`.

`chdir_to_buffer`: if buffer root ≠ cwd/active, `on_root_change(..., { open_last = false })` (no file jump).

`record`: update `last_file` + `cursor` in memory when path/cursor changes (skip if unchanged).

## User commands

| Command | Alias | Action |
|---------|-------|--------|
| `ProjectSwitch` | `P` | `M.switch` — `vim.ui.select` on `project_picker_items()` |
| `ProjectSave` | — | `save_state` + `persist_state(true)` |
| `ProjectStatus` | — | notify root, state path, terminal config |
| `ProjectTerm` | `Pt` | `terminal_cmd` (bang = new); args: toggle/new/show, layout |

## Per-project terminal

- Tag: `vim.b[buf].project_terminal = true`, `project_root = root`.
- `primary_terminal[root]` — preferred buf; find or create.
- Layout: `split` / `vsplit` / `tab` / `float`; `open_split_window` handles `splitkeep=screen` (find new win if focus stuck on editor).
- `termopen(vim.o.shell, { cwd = root })`.
- Toggle: hide terminal win or show/create; if only one win, `edit` last file or `enew`.

## Public API surface (for tests / extensions)

`setup`, `switch`, `load_state`, `persist_state`, `get_project`, `set_project`, `is_managed_root`, `list_projects`, `on_root_change`, `save_state`, `current_root`, terminal helpers, `restore_startup`.

## Pitfalls (regression checklist)

| Bug | Cause | Fix |
|-----|-------|-----|
| `:P` does nothing | Picker roots are `…/.git` paths | `git_dir_to_root` + `fd -g '.git'` |
| `:P` silent fail | `is_managed_root` false on bad paths | Notify + correct discovery |
| Switch reverts instantly | `lock` false while old project windows live | `isolate_buffers(close_wins=true)` before releasing `lock` |
| E5560 on persist | `mkdir` in timer | `vim.in_fast_event()` → `vim.schedule` |
| Wrong jumplist after `:P` | `stay_put` returned early | Restore via `jumps_blob` / `nvim_load_context` |
| Multi-second `:P` | `vim.fs.find` for repos | `fd` / `find` only |
| Stuck `lock` | error before `lock = false` | `pcall` sync block; always clear `lock` |

## Style / constraints

- LuaCATS `---@` types for config and state.
- Module table `M`; file-local helpers for paths, JSON, git scan, terminal internals.
- Minimal scope: no extra dependencies beyond Neovim + `fd` (optional).
- Match existing naming and section comments (`-- ---------------------------------------------------------------------------`).
- Do not commit secrets; state file lives under `stdpath("data")`.

## Rebuild workflow for agents

1. Read this spec and current `project.lua` if present.
2. Implement bottom-up: paths → JSON → discovery → roots → jumplist → switch → autocmds → terminal → commands.
3. Verify: headless `require("project").setup(...); #M.list_projects(); all pass `is_managed_root`**.
4. Manual: `:P` between two repos; `Ctrl-o` jumplist per project; `:ProjectSave`; restart Neovim.

## Constants

```lua
M.STORE_VERSION = 1
M.DEFAULT_STORE_NAME = "project-state.json"
M.LEGACY_STORE_NAME = "project-last-files.json"
M.JUMPLIST_DEBOUNCE_MS = 1500
M.PERSIST_INTERVAL_MS = 5000
```
