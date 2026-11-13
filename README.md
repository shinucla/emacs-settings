# emacs-settings

Personal Emacs configuration.

`~/.emacs` is a small loader: each feature's code lives in its own folder
in this repository, and `~/.emacs` loads those files by absolute path.
The repository is therefore expected to live at `~/emacs-settings`.

## Layout

| Path | Purpose |
|---|---|
| `.emacs` | Tracked copy of the init file that is installed as `~/.emacs` |
| `MGA.md` | Rules for adding or changing features |
| `packages/` | MELPA setup and `package-initialize` |
| `theme/` | Theme loading and default face |
| `legacy-libs/` | Loaders for the libraries in `lib/` |
| `modes/` | `auto-mode-alist`, indentation, mode hooks |
| `editing/` | Ido, buffer/line editing commands, keybindings |
| `code-generation/` | `snake-to-camel`, `getset` |
| `diff-mode/` | Diff-mode face settings |
| `copilot/` | GitHub Copilot configuration |
| `git-graph/` | `git-graph` command |
| `agent-terminal/` | Agentic CLI terminals on top of Eat |
| `lib/` | Legacy libraries loaded by `legacy-libs/` |
| `themes/` | Theme files |
| `copilot_el/` | Copilot implementation (Git submodule) |

## Requirements

- Emacs 28.1 or newer
- Git
- Node.js 18+ (Copilot language server)
- MELPA packages, including at least:
  `eat`, `compat`, `yaml-mode`, `lua-mode`, `markdown-mode`, `csv-mode`,
  and Copilot's dependencies `jsonrpc`, `f`, `s`, `dash`, `editorconfig`
- `vterm` is optional; it is only needed for `C-c A`

## Setup on a new machine

Clone with the submodule and enable recursive pulls:

```bash
git clone --recurse-submodules git@github-shin:shinucla/emacs-settings.git \
  ~/emacs-settings
cd ~/emacs-settings
git config submodule.recurse true
cp .emacs ~/.emacs
```

If the repository was cloned without submodules:

```bash
cd ~/emacs-settings
git submodule update --init --recursive
git config submodule.recurse true
```

Then install the Emacs packages, either from the package menu or with:

```
M-x package-refresh-contents
M-x package-install-selected-packages
```

Copilot needs its server installed and an authenticated account:

```
M-x copilot-install-server
M-x copilot-login
```

See `agent-terminal/agent-terminal.el` for Eat/vterm setup notes.

## Submodules

`copilot_el/` is a Git submodule pinned to a commit of
`github.com/copilot-emacs/copilot.el`. The superproject records only the
commit, so the contents must be checked out separately.

With `submodule.recurse true` set in this repository (see above), a plain
`git pull` also updates submodules to the recorded commit:

```bash
cd ~/emacs-settings
git pull                      # pulls and syncs copilot_el
git pull --no-recurse-submodules   # skip submodules once
```

Other operations:

```bash
# After a pull that did not recurse, or on a fresh clone
git submodule update --init --recursive

# Advance copilot_el to the latest upstream commit, then record it
git submodule update --remote copilot_el
git add copilot_el
git commit -m "Update copilot.el submodule"
```

To add another submodule:

```bash
cd ~/emacs-settings
git submodule add <repository-url> <path>
git commit -m "Add <name> submodule"
git push
```

Note: `.gitmodules` currently uses the `github-shin` SSH alias
(`Host github-shin` in `~/.ssh/config`, key `id_rsa_github`). On a machine
without that alias, either define it or change the URL to the canonical
`git@github.com:copilot-emacs/copilot.el.git` (or the HTTPS URL).

## Adding features

See `MGA.md` for the full rules. In short:

1. Update an existing feature folder if the new behavior fits its intent.
2. Create a new folder only when no existing folder can contain it.
3. No individual feature files at the repository root.
4. Keep `~/.emacs` a loader; add a `(load-file ...)` entry for the feature.
5. Copy, never move, referenced files.
6. Additions only: do not delete or restructure existing folders unless asked.
