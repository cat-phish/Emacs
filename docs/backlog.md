# Emacs Configuration Backlog

This backlog tracks bugs, code hygiene improvements, performance optimizations, and potential enhancements identified across `init.org`, `early-init.el`, `lisp/`, and `scripts/`.

> **Note:** As a literate configuration, all changes to Emacs Lisp configuration must be made in `init.org` (never directly in `init.el`), followed by re-tangling `init.el` and exporting `README.md`.

---

## 1. Bugs & Broken Blocks (High Priority)

- [ ] **Remove stray `sh-test` symbol in `corfu` `:config` (`init.org` line 2777)**
  - Inside `(use-package corfu :config ...)`, there is a bare `sh-test` symbol on line 2777 (tangled into `init.el` line 2113). When `corfu` loads and evaluates its `:config` block, evaluating the unbound variable `sh-test` signals a `(void-variable sh-test)` error, aborting the rest of the `:config` block (`C-n`/`C-p`/`TAB` bindings and `corfu-popupinfo-mode`).
- [ ] **Fix malformed `#+end_src>` tags preventing blocks from tangling (`init.org` lines 3023, 3044, 3062)**
  - Three source blocks in `init.org` close with `#+end_src>` instead of `#+end_src`:
    1. **Rainbow-Mode** (`init.org` lines 3016–3023) — Also missing a closing parenthesis `)` for `(use-package rainbow-mode ...)` and marked `FIXME:` in the heading.
    2. **PDF Tools** (`init.org` lines 3026–3044) — Entire `pdf-tools` configuration is silently skipped during tangling.
    3. **Super Save** (`init.org` lines 3050–3062) — Marked `:disabled t`, but the malformed block delimiter still breaks Org block parsing/export.
- [ ] **Resolve `C-s` keybinding and completion framework conflict (`vertico`/`consult` vs. `ivy`/`swiper`)**
  - Both `vertico` (lines 2844–2846) and `ivy` (`(ivy-mode 1)` on lines 2895–2911) are enabled simultaneously, competing over minibuffer completion.
  - Furthermore, line 845 binds `C-s` in `(normal insert visual emacs)` states to save the buffer and return to normal state, while `ivy` binds `"C-s" . swiper` in `global-map` (line 2897). Decide whether `ivy`/`swiper` is still needed alongside `vertico` + `consult-line` (`SPC s l`), and remove `ivy` if redundant.
- [ ] **Fix broken `:load-path` for `grease` (`init.org` line 2917)**
  - `grease` points to `:load-path "~/.config/emacs-org/lisp/grease.el"`, which references an old directory name (`emacs-org`) and a file path instead of a directory. Either vendor `grease.el` into `lisp/` (which is already on `load-path`) or update the path to use `user-emacs-directory`.
- [ ] **Fix `load-path` ordering for `flash-emacs` (`init.org` lines 2707 vs. 2873)**
  - `lisp/` is added to `load-path` at line 2707 (under `* Multi File Example`) before `(require 'flash-emacs)` at line 2873, which works, but `flash-emacs-jump` is referenced earlier in `org-agenda` keybindings (line 2202). Moving `(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))` to `* Core Setup & Performance` makes local packages available throughout the entire config.
- [ ] **Fix `eat` hook quoting in `use-package` (`init.org` line 2699)**
  - `:hook ('eshell-load-hook #'eat-eshell-mode)` quotes the hook name and sharp-quotes the function inside `:hook`, which expands to `'eshell-load-hook-hook` in `use-package`. It should be `:hook (eshell-load . eat-eshell-mode)`.
- [ ] **Fix `marginalia` hook syntax in `nerd-icons-completion` (`init.org` line 2860)**
  - `:hook ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup)` over-quotes both symbols in `:hook`. Use `:hook (marginalia-mode . nerd-icons-completion-marginalia-setup)`.
- [ ] **Fix typo in `org-agenda-start-with-log-mod` (`init.org` line 1778)**
  - `(org-agenda-start-with-log-mod t)` is missing the trailing `e` (`org-agenda-start-with-log-mode`).
- [ ] **Fix `evil-org` hook lambda (`init.org` line 2100)**
  - `:hook (org-mode . (lambda () evil-org-mode))` evaluates the variable `evil-org-mode` as a value instead of calling the function `(evil-org-mode)`. Simplify to `:hook (org-mode . evil-org-mode)`.

---

## 2. Duplicated Code & Redundant Declarations

- [ ] **Deduplicate Org helper functions, hooks, and timers (`* DWIM Helper Functions` vs. `* Org Mode Config`)**
  - The following functions, hooks, and timers are defined twice in `init.org` (first under `* DWIM Helper Functions` around lines 1263–1426, and again identically inside `(use-package org :config ...)` around lines 1854–2008):
    - `my/org-save-all-except` and `(run-with-idle-timer 30 t #'my/org-save-all-except)` (registers two identical 30s idle timers)
    - `my/org-indent-all-src-blocks` and `(add-hook 'before-save-hook #'my/org-indent-all-src-blocks)`
    - The anonymous `org-after-refile-insert-hook` lambda (added twice)
    - `my/org-toggle-todo-entries`, `my/org-toggle-done-entries`, `my/org-cycle-all-todo-done-entries`, `my/org-cycle-todo-entries`, `my/org-cycle-done-entries`
    - `my/org-hide-done-entries-dynamic`
  - Keep each definition in one canonical place (`* DWIM Helper Functions` or `* Org Mode Config`).
- [ ] **Deduplicate `nerd-icons` `use-package` blocks (`init.org` lines 1011 & 1023)**
  - `nerd-icons` is declared in two separate `use-package` blocks right next to each other.
- [ ] **Deduplicate `savehist-mode` and `global-auto-revert-mode` (`init.org` lines 435, 440, 447, 2848)**
  - `global-auto-revert-mode` is enabled twice in `(use-package emacs :custom ...)` (lines 435 and 447).
  - `savehist-mode` is enabled in `:custom` (line 440) and called again at top-level in the Vertico section (line 2848).
- [ ] **Consolidate TODO keyword face definitions (`org-todo-keyword-faces` and `org-modern-todo-faces`)**
  - The 13 custom TODO keyword faces (`TODO`, `NEXT`, `PLANNING`, `READY`, `ACTIVE`, `REVIEW`, `HOLD`, `ASSIGNMENT`, `BILL`, `CHORE`, `MEETING`, `DONE`, `CANCELED`) are duplicated verbatim between `org-todo-keyword-faces` (lines 1793–1806) and `org-modern-todo-faces` (lines 2296–2309). Extract them into a shared `defconst start/org-todo-faces` variable.
- [ ] **Fix corrupted TOC entry in `init.org` (`init.org` line 82)**
  - Line 82 has `- [[#flash][Flash]]:config (ws-butler-global-mode))` from an accidental paste. Let `toc-org` regenerate the Table of Contents cleanly.

---

## 3. Startup & Runtime Performance

- [ ] **Enable `lexical-binding: t` in `init.org` and `early-init.el`**
  - Both `init.org` (line 251) and `early-init.el` (line 1) set `-*- lexical-binding: nil; -*-`. Enabling `lexical-binding: t` improves closure performance, catches unused/free variables during byte/native compilation, and is required by modern Emacs packages.
- [ ] **Avoid calling `(start/install-treesit-grammars)` unconditionally on every startup (`init.org` line 2650)**
  - On every startup (when `start/enable-dev-features` is non-nil), Emacs loops over all 19 languages in `treesit-language-source-alist` and calls `treesit-language-available-p`. Keep `start/install-treesit-grammars` as an interactive command (`M-x start/install-treesit-grammars`) rather than running it eagerly during init.
- [ ] **Make `syncthingctl` after-save hook asynchronous (`init.org` lines 1655–1659)**
  - `(shell-command "syncthingctl rescan-all")` runs synchronously on every `org-mode` save, blocking the UI until `syncthingctl` returns. Replace with `(start-process "syncthing-rescan" nil "syncthingctl" "rescan-all")` so saves remain instantaneous.
- [ ] **Defer `pdf-tools-install` until a PDF is opened**
  - Once the `pdf-tools` block delimiter is fixed, ensure `(pdf-tools-install :no-query)` is only run when opening a PDF (which `:mode ("\\.pdf\\'" . pdf-view-mode)` handles as long as nothing eagerly loads `pdf-tools`).
- [ ] **Resolve startup view contention between `dashboard` and `org-agenda` (`init.org` lines 1063 vs. 2088)**
  - `dashboard` sets up `dashboard-setup-startup-hook` to render on launch, while line 2088 schedules a `0.5 sec` timer on `emacs-startup-hook` to immediately switch to `(org-agenda nil "d")`. Decide whether Emacs should launch into Dashboard, launch directly into the Daily Agenda (`initial-buffer-choice`), or only open the Agenda when no file arguments are passed on the command line.
- [ ] **Optimize `my/org-indent-all-src-blocks` on save (`init.org` line 1284)**
  - Running `org-babel-do-in-edit-buffer` on every source block in an Org file on every `before-save-hook` spawns major-mode edit buffers for each block. In large literate files like `init.org` (~80 source blocks), this adds noticeable save latency. Consider restricting it to the current block, making it an interactive command, or skipping large files like `init.org`.

---

## 4. Portability & Conventions (`AGENTS.md` Alignment)

- [ ] **Unify custom symbol prefixes to `start/`**
  - `AGENTS.md` specifies that custom functions and variables should use the `start/` prefix, but most helper functions and variables use `my/` (`my/org-return-dwim`, `my/org-cycle-dwim`, `my/relative-line-numbers`, `my/org-roam-ui-mode-custom-browser`, etc.). Gradually migrate `my/` definitions to `start/`.
- [ ] **Use `start/org-main-dir` for `org-refile-targets` (`init.org` lines 1780–1783)**
  - `org-refile-targets` uses relative filenames `("Archive.org" "Tasks.org" "Projects.org")`, which only resolve properly when the current buffer is in the same directory or when those files are already open. Use `start/org-tasks-file`, `start/org-projects-file`, and a new `start/org-archive-file` constant from the `Cross-platform paths` section.
- [ ] **Gate remaining dev-specific bindings and packages behind `start/enable-dev-features`**
  - The `"c"` (`coding`) leader key prefix (lines 786–796, binding `eglot-*` and `consult-flymake`) and `consult-project-function` (`projectile-project-root` at line 2994) are not gated by `start/enable-dev-features`, which can cause unbound function errors on Windows where `eglot` and `projectile` are disabled.
- [ ] **Make `my/org-roam-ui-mode-custom-browser` cross-platform and safe (`init.org` lines 873–881)**
  - The docstring says "open the UI in Chromium" while the command runs `shell-command` with hardcoded `"firefox --kiosk --new-window "`. Use `start-process` instead of synchronous `shell-command` (which blocks Emacs while Firefox is open!) and fall back gracefully if `firefox` is not in `PATH` or on Windows.
- [ ] **Move GUI-specific UI tweaks out of `early-init.el` or guard for non-Niri/daemon environments**
  - `early-init.el` sets `fullscreen . maximized`. Also consider moving `(menu-bar-mode -1)`, `(tool-bar-mode -1)`, and `(scroll-bar-mode -1)` into `early-init.el` (via `default-frame-alist`) so frames never flash GUI chrome before `init.el` loads.

---

## 5. Repository & Script Hygiene

- [ ] **Clean up untracked/stray runtime files in the repository root and update `.gitignore`**
  - The repository root currently contains stray files generated by Emacs at runtime: `bookmarks.md`, `custom-vars.md`, `Inbox.md`, `Inbox.org`, and `#init.org#`.
  - Remove these stray files and add `bookmarks.md`, `custom-vars.md`, and `Inbox.*` to `.gitignore` if Emacs continues generating them.
- [ ] **Remove unused Kickstart template artifacts**
  - `lisp/start-multiFileExample.el` and the `* Multi File Example` section in `init.org` (lines 2701–2721) are leftover boilerplate from Kickstart.emacs. Keep the `load-path` addition for `lisp/` (moved to Core Setup) and remove the example file and section.
- [ ] **Remove or archive dead/commented-out config sections**
  - Clean up large commented-out blocks in `init.org` such as the duplicate `start/org-babel-tangle-config` (lines 271–289), commented `org-superstar` (lines 2280–2286), `Command Log Mode (Deprecated?)` (lines 3080–3087), and the first commented `org-download` block (lines 2472–2490).
- [ ] **Update `scripts/` for safety and cross-platform reuse**
  - `scripts/fix_duplicate_ids.py` and `scripts/fix_double_dl.py` overwrite `~/org/main/Tasks.org` and `~/org/main/Inbox.org` in-place without backups or dry-run flags (`--dry-run`), and fail with `FileNotFoundError` in `fix_duplicate_ids.py` if a file does not exist. Add existence checks, atomic writes/backups, and CLI arguments.
- [ ] **Add a `Makefile` or helper script for tangling, exporting, and batch-checking**
  - Since this repo is deployed via Nix home-manager and the `after-save-hook` does not trigger outside `user-emacs-directory`, add a `Makefile` (or `scripts/build.sh`) wrapping the three commands documented in `AGENTS.md` (`tangle`, `readme`, `check`).
