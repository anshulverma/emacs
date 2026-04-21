# emacs

Personal Emacs configuration. macOS + Homebrew and Debian/Ubuntu Linux (incl. GitHub Codespaces). Emacs 27.1 or newer (CI covers 28.2, 29.4, 30.2, and snapshot).

[![CI](https://github.com/anshulverma/emacs/actions/workflows/ci.yml/badge.svg)](https://github.com/anshulverma/emacs/actions/workflows/ci.yml)

## Install

```sh
git clone https://github.com/anshulverma/emacs.git ~/workspace/emacs
cd ~/workspace/emacs
./install.sh
```

`install.sh` is idempotent. It:

- detects macOS or Debian/Ubuntu,
- installs the external binaries this config shells out to (via Homebrew or apt): `ispell`/`aspell`, `pandoc`, `graphviz`, `plantuml`, `gnuplot`,
- on Linux, installs `emacs-nox` if Emacs isn't already present,
- symlinks the checkout into `~/.emacs.d` if nothing is there.

### GitHub Codespaces

`./install.sh` works out of the box in a Codespace. To get Claude features (see below) working, add your Anthropic key as a Codespace secret named `ANTHROPIC_API_KEY` — `gptel` picks it up automatically.

If `~/.emacs.d` already exists, `install.sh` leaves it alone. Move it aside first:

```sh
mv ~/.emacs.d ~/.emacs.d.bak.$(date +%Y%m%d)
./install.sh
```

Elisp packages are installed by `package.el` on the first launch (takes a few minutes the first time — subsequent launches are fast).

The submodules (`snippets/AndreaCrotti`, `lib/predictive`) are optional; clone without `--recursive` and the config degrades gracefully.

## Layout

- `early-init.el` — pre-UI setup (GC tuning, UI chrome stripped before the frame is drawn). Emacs 27+.
- `init.el` — bootstraps `av-packages` + `av-setup`, then loads `custom.el`.
- `custom.el` — Customize's auto-generated state, kept separate so it can't fight hand-edits.
- `src/av-packages.el` — the authoritative package list (`av/packages`). Edit here to add/remove packages.
- `src/av-setup.el` — loads `use-package`, then walks `src/custom/` in load order.
- `src/custom/NN-*.el` — feature-specific setup, ordered by the numeric prefix (02 = foundation … 101 = post-init).
- `lib/` — third-party / historical helpers. Most vendored copies were deleted in the 2026 cleanup in favor of modern built-ins.

## Local customizations

Create `~/.emacs.local.el`; it loads before any `src/custom/*.el` file, so values set there win.

### Theme

```lisp
(setq av/theme 'zenburn)    ; default 'leuven
```

### Font size

```lisp
(setq av/face-height 180)   ; default 144
```

### Org agenda / diary

```lisp
(setq av/org-base-dir "~/Dropbox/org")
```

(Expects a file named `diary` in that directory.)

### IRC email (used by ERC setup)

```lisp
(setq av/user-email "you@example.com")
```

## External binaries the config expects

Installed by `install.sh`:

- **ispell** — flyspell backend
- **pandoc** — `ox-pandoc` export + fallback for `markdown-command`
- **graphviz** — `dot` for org-babel diagrams
- **plantuml** — UML diagrams in org-babel
- **gnuplot** — org-babel plots

Optional, install yourself:

- **Python LSP/formatter** — `pipx install pyright ruff black`. Eglot starts automatically when one of these is on `$PATH`; apheleia formats on save with `ruff` or `black`.
- **Scala** — install Metals via `coursier install metals` if you want LSP.
- **Claude Code CLI** — `npm install -g @anthropic-ai/claude-code`. When the `claude` binary is on `$PATH` and Emacs is 29+, `claude-code-ide.el` self-installs via `package-vc` on next launch.

## Claude integration

Two complementary packages are wired up:

- **`gptel`** — provider-agnostic LLM client; pre-configured with Claude as the default backend.
  - `C-c a c` — open a chat buffer
  - `C-c a s` — send the region/buffer
  - `C-c a m` — transient menu
  - API key: `ANTHROPIC_API_KEY` env var wins; otherwise `auth-source` is read (`machine api.anthropic.com login apikey password sk-...` in `~/.authinfo` or `~/.authinfo.gpg`).
  - Override the default model from `~/.emacs.local.el`: `(setq av/claude-model 'claude-opus-4-7)`.

- **`claude-code-ide.el`** — deep integration with the Claude Code CLI via MCP (xref, project, tree-sitter tools exposed to Claude; ediff-style suggestions in-buffer).
  - `C-c c` — open the claude-code-ide menu
  - Requires Emacs 29+ and the `claude` CLI on `$PATH`; uses `eat` as the terminal backend (pure-elisp, works in any TTY).
  - Self-installed via `package-vc-install` on first launch when the prerequisites are present; silently skipped otherwise.

## Troubleshooting

Start Emacs with `--debug-init` to get a full backtrace for any startup error.

### `Expected printf output from shell, but got …`

Your shell rc (e.g. `.zshrc`) is printing something — often a fancy prompt — in non-interactive subshells, which pollutes what `exec-path-from-shell` expects. The config already runs the shell with `-l` (login only, no `.zshrc`) and wraps the call in `with-demoted-errors`, so this should not abort init. If it recurs, move `PATH` exports from `.zshrc` to `.zshenv` and keep rc files silent when stdout isn't a tty.

### `Symbol's function definition is void: …`

A vendored library (under `lib/`) is calling a function that got removed in a recent Emacs. Usually a one-line rename fix — check `lib/` for the symbol and either update the call, delete the unused library, or replace it with a built-in equivalent.

### A package fails to install

`package.el` pulls from GNU ELPA + NonGNU ELPA + MELPA + melpa-stable. If a package has been pulled from all four (has happened to `helm-swoop`, `helm-themes`, `ensime`), prune it from `src/av-packages.el` and strip the matching `src/custom/` setup. The weekly CI cron catches these before they bite.

### Starting clean

```sh
rm -rf lib/elpa/             # force a re-download of every ELPA package
rm ~/.emacs.d/custom.el      # drop Customize state
```

## Licensing

Code under `lib/` is third-party — upstream authors are credited in each file. The rest of the config is the author's own.

The bundled font `source-code-pro` is from <https://github.com/adobe-fonts/source-code-pro>.
