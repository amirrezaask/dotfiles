# Pierre changes

A Pi extension that records net file changes made during completed agent turns and opens a local HTML report powered by [`@pierre/diffs`](https://github.com/pierrecomputer/pierre/tree/main/packages/diffs) and [`@pierre/trees`](https://github.com/pierrecomputer/pierre/tree/main/packages/trees).

## Install

From this repository:

```bash
cd pi/pierre-changes
npm install
cd ../..
./sync
```

Then start or reload Pi. The report opens automatically after a settled turn in interactive mode. Use `/changes` to reopen it.

Reports are written under the current project’s `.pi/pierre-changes/<session-id>/latest.html`; the HTML is self-contained and can be opened later. Turn records are also persisted in the Pi session as custom entries.

Set `PI_PIERRE_CHANGES_AUTO_OPEN=0` to record and generate reports without launching a browser.

## What is recorded

- Tracked and non-ignored untracked files in Git repositories.
- All files except common dependency/cache directories in non-Git directories.
- Added, modified, deleted, and pure-renamed files.
- Text contents, hashes, sizes, binary status, and file mode for each net turn change.

Binary and very large files remain recorded in the tree but are shown as metadata instead of being rendered as a text diff.
