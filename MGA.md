# Repo rules for emacs-settings

This repository stores the Emacs configuration loaded from `~/.emacs`.
Every feature lives inside its own folder under `~/emacs-settings/`.

## Adding a feature

1. **Update an existing feature folder first.** If the new behavior fits
   the intent of a folder that already exists — for example `editing/`,
   `modes/`, `copilot/`, or `agent-terminal/` — add or edit the files in
   that folder instead of creating a new one.
2. **Create a new folder only when no existing folder can contain the
   intent.** Name the folder after the feature and put the feature's
   `.el` file(s) inside it, for example
   `~/emacs-settings/<new-feature>/<new-feature>.el`.
3. **No individual feature files at the root** of `~/emacs-settings/`.
   Every feature must be contained in a feature folder. Root-level files
   are limited to repository metadata and legacy files (`MGA.md`,
   `README.md`, the old `.emacs`, and similar).
4. **Keep `~/.emacs` a loader.** Wire new or changed features into
   `~/.emacs` with a `(load-file "~/emacs-settings/<feature>/...")`
   entry, preferably with a short description/instructions comment.
5. **Copy, never move, referenced files.** Existing referenced files and
   folders (`lib/`, `copilot_el/`, `themes/`, ...) stay where they are;
   a feature folder may contain copies.
6. **Additions only.** Do not delete or restructure existing folders or
   files unless the user explicitly asks for it.

## Feature folder map

Use this map when deciding whether an addition belongs in an existing
folder.

| Folder | Responsibility |
|---|---|
| `packages/` | MELPA setup, `package-initialize` |
| `theme/` | Theme loading and default face |
| `legacy-libs/` | Loaders for the libraries in `lib/` |
| `modes/` | `auto-mode-alist`, indentation, mode hooks |
| `editing/` | Ido, buffer/line editing commands, keybindings |
| `code-generation/` | `snake-to-camel`, `getset` |
| `diff-mode/` | Diff-mode face settings |
| `copilot/` | Copilot configuration (implementation in `copilot_el/`) |
| `git-graph/` | `git-graph` command |
| `agent-terminal/` | Agent terminal (copy of the `~/.emacs.d` original) |
