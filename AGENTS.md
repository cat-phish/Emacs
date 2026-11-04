# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A personal, Org-first, Evil-based Emacs config built on Kickstart.emacs (MiniApollo) and inspired by Doom Emacs. There is no build system and no tests. The config itself is the product.

## Literate config: edit `init.org`, not `init.el`

- **`init.org` is the source of truth.** `init.el` and `README.md` are generated from it. Never hand-edit `init.el`: the next tangle overwrites the change.
- The file header sets `header-args:emacs-lisp :tangle ./init.el`, so every `emacs-lisp` src block is tangled unless it is marked `:tangle no`. Example and disabled blocks use `:tangle no`, or their code is commented out.
- `README.md` is an `org-md` export of `init.org` with source blocks omitted (`:exports none`). Headings tagged `:noexport:` are left out.
- On save inside `user-emacs-directory`, `start/org-babel-tangle-config` tangles `init.el`, exports `README.md`, and runs `package-quickstart-refresh`. **That hook does not fire for this checkout** (`~/code/emacs-config`). This repo is deployed to `~/.config/emacs` through Nix home-manager as read-only symlinks into `/nix/store`, so after editing `init.org` you must regenerate the outputs yourself:

```sh
# Tangle init.org -> init.el
emacs --batch -l org --eval '(org-babel-tangle-file "init.org")'

# Regenerate README.md (optional; mirrors what the save hook does)
emacs --batch -l org -l ox-md init.org --eval '(let ((org-export-with-toc nil) (org-export-with-author nil) (org-export-with-date nil) (org-export-with-broken-links t)) (org-md-export-to-markdown))'

# Sanity-check that the tangled init loads (needs packages already installed in ~/.config/emacs/elpa)
emacs --batch --init-directory ~/.config/emacs -l init.el
```

Commit `init.org` together with the regenerated `init.el` (and `README.md` if prose changed). Changes reach the live config only after a home-manager rebuild.

## Structure of `init.org`

Top-level headings run in load order, and that order matters:

1. **Core Setup & Performance**: GC threshold, the auto-tangle hook, package.el + `use-package-always-ensure t` (built-in packages need `:ensure nil`), `package-quickstart`, defaults, and **cross-platform path constants**.
2. **Keybindings (Load First)**: Evil plus general.el. The leader is `SPC`, defined with `start/leader-keys`. These load early so keys still work if something later breaks.
3. Appearance → DWIM helper functions (custom Org editing commands) → Org Mode (agenda, roam, super-agenda, ox-icalendar export, etc.) → Development → Version Control → Completion (Corfu/Cape/Orderless/Vertico) → Navigation → Utilities → Runtime Performance.

Conventions:
- Custom functions and variables use the `start/` prefix.
- Org file locations come from the constants in "Cross-platform paths" (`start/org-root`, `start/org-tasks-file`, `start/org-inbox-file`, etc.). Org data lives outside this repo, in `~/org/` on Linux and `C:/Users/Jordan/org/` on Windows. Use these constants rather than hardcoding paths.
- Dev tooling (Eglot, tree-sitter, yasnippet, Eat, etc.) is gated by `:if start/enable-dev-features` or `(when start/enable-dev-features ...)`. That flag is false on Windows, so new programming-related packages should use the same gate.
- `custom-file` is `custom-vars.el` (gitignored). `custom-vars.md` and `bookmarks.md` are stray Emacs data files written by the config, not documentation.

## Other files

- `early-init.el`: hand-written, not tangled. It holds frame-sizing fixes for the niri compositor.
- `lisp/`: added to `load-path`. `flash-emacs.el` is a vendored flash.nvim-style jump package that `init.org` loads with `(require 'flash-emacs)`. `start-multiFileExample.*` is a Kickstart example and is not loaded.
- `scripts/`: one-off maintenance tools for the external Org files. `fix_duplicate_ids.py` and `fix_double_dl.py` operate on `~/org/main/{Tasks,Inbox}.org`. `org-caldav-sync.el` is a standalone batch script for running org-caldav in Docker (`emacs --batch --load scripts/org-caldav-sync.el`). The in-config org-caldav block is currently commented out in favor of ox-icalendar export to `~/org/calendars/Org-Mode.ics`.
