# emacs

Personal Emacs configuration. macOS + Homebrew, Emacs 27.1 or newer (CI covers 28.2, 29.4, 30.2, and snapshot).

[![CI](https://github.com/anshulverma/emacs/actions/workflows/ci.yml/badge.svg)](https://github.com/anshulverma/emacs/actions/workflows/ci.yml)

## Install

```sh
git clone --recursive git@github.com:anshulverma/emacs.git ~/workspace/emacs
cd ~/workspace/emacs
./install.sh
```

`install.sh` is idempotent. It:

- checks for Homebrew and Emacs,
- installs the external binaries this config shells out to (`ispell`, `pandoc`, `graphviz`, `plantuml`, `gnuplot`),
- symlinks the checkout into `~/.emacs.d` if nothing is there.

Elisp packages are installed by `package.el` on the first launch (takes a few minutes the first time — subsequent launches are fast).

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

## Licensing

Code under `lib/` is third-party — upstream authors are credited in each file. The rest of the config is the author's own.

The bundled font `source-code-pro` is from <https://github.com/adobe-fonts/source-code-pro>.
