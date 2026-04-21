# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository purpose

This is a personal Emacs configuration, intended to be cloned to `~/.emacs.d/` (or equivalent) and bootstrapped via `install.sh`. It targets **macOS only** (install.sh checks `$OSTYPE` and exits otherwise) and expects Emacs 24.4 or higher.

## Bootstrap / install

- `./install.sh` — one-time bootstrap. Installs Homebrew packages (ispell, cask, plantuml, pandoc, pyenv, etc.), sets up Python 2.7.10 via pyenv, installs Python packages for elpy/jedi, and clones `anshulverma/mural` to `~/.mural`. Run this on a fresh machine; re-running is idempotent but slow.
- `./install-font.sh` — installs the bundled fonts (`Symbola`, `source-code-pro`) under `fonts/`.
- There is no test suite, lint target, or build step — changes are verified by launching Emacs.

## Entry point and load order

`init.el` is the only top-level entry point. It does three things:

1. Calls `package-initialize` (required before `custom-set-variables`).
2. Adds `src/` and `lib/` (plus `lib/jmax/`) to `load-path`.
3. `(require 'av-packages)` then `(require 'av-setup)`.

### `src/av-packages.el`
- Declares the package archives (elpy, org, gnu, melpa-stable, melpa) and sets `package-user-dir` to `lib/elpa` (not the default `~/.emacs.d/elpa`).
- Contains the authoritative `av/packages` list — **adding a new ELPA package means adding a symbol here**, not editing the `Cask` file. (The `Cask` file exists but `cask-initialize` runs after `package.el` has already installed everything; Cask is secondary.)
- Also force-installs a specific `highlight-indentation-0.6.0` from the elpy archive as a workaround.

### `src/av-setup.el`
- Loads `~/.emacs.local.el` if present (per-machine overrides — see below).
- **Auto-loads every `.el` file under `src/custom/` recursively**, filtered to exclude `flycheck_*` temp files. `f--files` walks the tree; load order follows the lexicographic sort of full paths, which is why directories and files are prefixed with numbers.

### Numeric prefix convention in `src/custom/`

The numbers define load phases — **do not rename files without accounting for dependencies on earlier phases**:

- `02-*` — foundation (appearance, environment, frame, global vars, macros)
- `03-*` — mode setups that can load early (helm, ido, magit, projectile, etc.)
- `04-*` — phase-2 modes that depend on phase-1 being ready (company, helm-projectile)
- `05-*` — larger feature areas (org, editor, ibuffer, ox backends)
- `06-*` — feature-specific subtrees (auto-mode, calendar, communication/, org-plus/, programming/)
- `07-*` — interactivity layer (hydras, key-bindings, which-key, smartparens)
- `08-*` — color overrides applied after themes
- `101-*` — post-init (look-and-feel tweaks that must come last)

Subdirectories (e.g. `06-org-plus/`, `06-programming/`) are walked too; their files also load in lexicographic order within the parent phase.

## Per-machine customization

Users override defaults by creating `~/.emacs.local.el`, which is loaded early in `av-setup.el` (before the `custom/` tree). Well-known knobs, all read with `boundp` checks:

- `av/theme` — theme symbol (default `'leuven`, see `02-appearance.el`)
- `av/face-height` — default font height (default 144)
- `av/org-base-dir` — location of org agenda files and `diary`
- `av/blog-dir`, `av/blog-site-domain`, `av/blog-disqus-username`, `av/blog-google-analytics-id` — consumed in `05-org.el` to configure `org-page`

When adding new tunables, follow this pattern: define a `defvar av/...` with a default, then gate behavior on `(boundp 'av/...)` or `(and (boundp ...) (f-exists? ...))` so the config still works without a local file.

## Library code under `lib/`

Code in `lib/` is **third-party and should not be modified** beyond minor patches (see README.md §License). `lib/elpa/` is the `package-user-dir` — it is managed by `package.el` and regenerated on install; don't edit it by hand. `lib/jmax/`, `lib/helm/`, `lib/predictive/`, and `snippets/AndreaCrotti/` are git submodules (see `.gitmodules`) — update them with `git submodule update --remote` rather than editing in place.

## `init.el` custom block

The `custom-set-variables` block at the bottom of `init.el` is managed by Emacs Customize. It contains `package-selected-packages`, `org-agenda-files` with **hard-coded absolute paths to the author's Dropbox**, theme safe-hashes, etc. Prefer editing via `M-x customize` or moving settings into `src/custom/` files; treat manual edits to this block carefully — Emacs will rewrite the whole block on next customize save.

## Notes for making changes

- A new package goes in `av/packages` in `src/av-packages.el`, plus (optionally) a `setup-<pkg>.el` under the appropriate `src/custom/0N-*` directory.
- A new config area gets a new numbered file; match the existing phase or introduce a new one carefully.
- Files are expected to end with `(provide 'FEATURE)` matching the filename and a `;;; FILE.el ends here` footer — the existing files establish this convention and flycheck will complain otherwise.
- `custom/` files can assume `av-util`, `f`, `s`, `dash` are loaded (required by `av-setup.el`).
