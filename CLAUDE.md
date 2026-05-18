# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository purpose

Personal Emacs configuration. Cloned into a working directory and linked into `~/.emacs.d` by `install.sh`. Targets macOS on Homebrew, though the Emacs side is portable (the CI workflow exercises Linux too). Minimum Emacs version is **27.1** (enforced by `init.el`; CI covers 28.2 / 29.4 / 30.2 / snapshot).

## Bootstrap

- `./install.sh` — idempotent; checks for Homebrew and Emacs, installs a small set of external binaries (`ispell`, `pandoc`, `graphviz`, `plantuml`, `gnuplot`), and symlinks the repo into `~/.emacs.d` if nothing is there. Elisp packages are installed by `package.el` on first Emacs launch (see `src/av-packages.el`).
- `./install-font.sh` — installs the bundled fonts.
- No test suite; `.github/workflows/ci.yml` runs `emacs -Q --batch -l init.el` across Emacs 28–snapshot on every push and weekly cron.

## Entry points and load order

1. **`early-init.el`** (Emacs 27+) — pre-UI: bumps `gc-cons-threshold`, zeros `file-name-handler-alist` for startup, disables automatic `package-initialize`, strips UI chrome from `default-frame-alist`. Restores sane values on `emacs-startup-hook`.
2. **`init.el`** — sets `av-basedir`/`av-src-dir`/`av-lib-dir`, adds them to `load-path`, requires `av-packages` then `av-setup`, then loads `custom.el` (where Customize saves state, so it doesn't fight hand edits).
3. **`src/av-packages.el`** — the **authoritative** package list is `av/packages`. Adds archives (GNU + NonGNU + MELPA + melpa-stable, all HTTPS), sets `package-archive-priorities` (melpa > melpa-stable, since many packages only cut rolling releases), and installs anything missing. Cask is **not** used.
4. **`src/av-setup.el`** — requires `use-package`, then recursively loads every `.el` under `src/custom/` in lexicographic order, filtering out `flycheck_*` temp files.

### `src/custom/` numeric-prefix phases

Load order matters; do not rename without checking dependencies:

- `02-*` — foundation (appearance, env, frame, global, macros)
- `03-*` — mode setup: helm (the completion stack), projectile, magit, yas-snippet, etc.
- `04-*` — phase-2 modes that depend on phase-1 (company, helm-projectile)
- `05-*` — feature areas (org, editor, ibuffer, ox backends)
- `06-*` — subtrees (`06-org-plus/`, `06-programming/`, `06-communication/`)
- `07-*` — interactivity (hydras, key bindings, which-key, smartparens)
- `08-*` — color overrides (after theme)
- `101-*` — post-init look-and-feel

## Completion stack

**Helm**, via `src/custom/03-modes/setup-helm.el`. The previous dual ido+helm setup was removed because modern `helm-mode` refuses to coexist with `ido-everywhere`. `flx` and `smex` are kept as backing scorers/ranking for helm-flx and helm-smex, but they don't provide their own UI.

## Python

Modern minimal stack (Stage 3.4 rewrite): built-in `python.el` + `eglot` (Emacs 29+ built-in) + `apheleia` for format-on-save. Eglot starts automatically when `pyright`/`basedpyright`/`ruff` is on `$PATH`; otherwise python buffers work without LSP. `apheleia` prefers `ruff` then `black`. No `elpy`, `python-mode.el`, `py-autopep8`, `company-jedi`, or `ein`.

Install LSP/formatters yourself: `pipx install pyright ruff black`.

## Per-machine customization

Users override defaults via `~/.emacs.local.el`, loaded in `av-setup.el` before the `custom/` tree. Known knobs, all read with `boundp` checks:

- `av/theme` — theme symbol (default `'leuven`)
- `av/face-height` — default font height (default 144)
- `av/org-base-dir` — location of org agenda files and `diary`
- `av/user-email` — IRC email userid
- `av/proxy` — forward proxy URL for package downloads (e.g., `"http://fwdproxy:8080"`). Alternatively set `https_proxy`/`http_proxy` env vars; `av/proxy` takes precedence. Supports `no_proxy`/`NO_PROXY` env var.
- `av/blog-*` — (historical — `org-page` was removed in Stage 2.1)

The env var `AV_SKIP_PACKAGES=1` skips all package installation at startup (useful on hosts with no internet access). Install packages later with `AV_SKIP_PACKAGES= emacs --batch -l init.el`.

When adding new tunables: `defvar av/... default` + `(boundp 'av/...)` guard.

## Library code under `lib/`

Everything here is third-party or historical and should not be modified beyond minor patches. Most vendored copies were deleted during the 2026 modernization (`linum`, `linum-off`, `dired+`, `fill-column-indicator`, `column-marker`, `emacs-type`, `highlight-chars`, `popup-select-window`) in favor of modern built-ins. What remains:

- `lib/elpa/` — `package-user-dir`; managed by `package.el`, regenerated on install, gitignored.
- `lib/jmax/` — John Kitchin's macros; submodule for criticmarkup-emacs and org-ref forks. Not live-loaded.
- `lib/predictive/` — optional submodule; `setup-predictive.el` guards the require with `file-exists-p`.
- `lib/helm-projectile.el`, `lib/hide-mode-line.el`, `lib/ox-pandoc.el` — still required by setup.

## Notes for making changes

- A new package goes in `av/packages` (`src/av-packages.el`), plus a `setup-<pkg>.el` under `src/custom/` at the appropriate phase.
- New blocks should prefer `(use-package FOO :defer t :config ...)` — `use-package` is available globally after `av-setup.el`. Do **not** set `use-package-always-defer t`; it breaks `:config` blocks that do setup work.
- Files are expected to end with `(provide 'FEATURE)` matching the filename, and start with a `lexical-binding: t` cookie on the first line.
- `custom/` files can assume `av-util`, `f`, `s`, `dash`, `use-package`, `validate` are already loaded.
- When upgrading across an Emacs major, watch for: `defadvice` → `advice-add`, `loop`/`every`/etc → `cl-`prefixed, deprecated package archives (HTTP → HTTPS), packages dropped from MELPA.
