;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; (defun start/org-babel-tangle-config ()
  ;; "Automatically tangle our init.org config file and refresh package-quickstart when we save it. Credit to Emacs From Scratch for this one!"
  ;; (interactive)
  ;; (when (string-equal (file-name-directory (buffer-file-name))
					  ;; (expand-file-name user-emacs-directory))
    ;; ;; Dynamic scoping to the rescue
    ;; (let ((org-confirm-babel-evaluate nil))
	  ;; (org-babel-tangle)
	  ;; (package-quickstart-refresh)
	  ;; )
    ;; ))
(defun start/org-babel-tangle-config ()
"Automatically tangle and refresh quickstart, strictly suppressing warnings."
(interactive)
(when (string-equal (file-name-directory (buffer-file-name))
		    (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil)
	;; Suppress all byte-compile and native-comp warnings temporarily
	(byte-compile-warnings nil)
	(warning-minimum-level :error)
	;; Prevent the buffer from popping up
	(display-buffer-alist '(("\\*Compile-Log\\*" (display-buffer-no-window))
				("\\*Warnings\\*" (display-buffer-no-window)))))
    (org-babel-tangle)
    ;; Use 'quietly' if your Emacs version supports it, otherwise refresh
    (package-quickstart-refresh)
    (message "Config tangled and package-quickstart refreshed!"))))
(setq native-comp-async-report-warnings-errors 'silent) ;; For Emacs 28+
(setq byte-compile-warnings '(not free-vars unresolved)) ;; Suppress common nagging warnings
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

(defun start/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
					(time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'start/display-startup-time)

(require 'use-package-ensure) ;; Load use-package-always-ensure
(setq use-package-always-ensure t) ;; Always ensures that a package is installed

(setq package-archives '(("melpa" . "https://melpa.org/packages/") ;; Sets default package repositories
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; For Eat Terminal

(setq package-quickstart t) ;; For blazingly fast startup times, this line makes startup miles faster

(use-package emacs
  :custom
  (menu-bar-mode nil)         ;; Disable the menu bar
  (scroll-bar-mode nil)       ;; Disable the scroll bar
  (tool-bar-mode nil)         ;; Disable the tool bar
  ;;(inhibit-startup-screen t)  ;; Disable welcome screen

  (delete-selection-mode t)   ;; Select text and delete it by typing.
  (electric-indent-mode nil)  ;; Turn off the weird indenting that Emacs does by default.
  (electric-pair-mode t)      ;; Turns on automatic parens pairing

  (blink-cursor-mode nil)     ;; Don't blink cursor
  (global-auto-revert-mode t) ;; Automatically reload file and show changes if the file has changed

  ;;(dired-kill-when-opening-new-dired-buffer t) ;; Dired don't create new buffer
  (recentf-mode t) ;; Enable recent file mode
  (setq history-length 25) ;; Number of commands to save in history
  (savehist-mode t) ;; Save command history
  (save-place-mode t) ;; Save position in buffer from previous session

  ;; Don't pop up UI dialogs when prompting
    (setq use-dialog-box nil)

    ;; Revert buffers when the underlying file has changed
    (global-auto-revert-mode 1)

    ;; Revert Dired and other buffers
    (setq global-auto-revert-non-file-buffers t)

 (global-visual-line-mode t)           ;; Enable truncated lines
 (display-line-numbers-type 'visual) ;; Relative line numbers
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
  (scroll-conservatively 10) ;; Smooth scrolling
(scroll-preserve-screen-position t)
  ;; (scroll-margin 15)
(scroll-margin (if (< (window-body-height) 30) 0 15))

(select-enable-clipboard t)      ; Use the system clipboard
  (select-enable-primary t)        ; Use the primary selection (middle click)
  (save-interprogram-paste-before-kill t) ; Save existing clipboard to kill ring before replacing

  (tab-width 4)

  (make-backup-files nil) ;; Stop creating ~ backup files
  (auto-save-default nil) ;; Stop creating # auto save files
  :hook
  (prog-mode . (lambda () (hs-minor-mode t))) ;; Enable folding hide/show globally
  :config
  ;; Move customization variables to a separate file and load it, avoid filling up init.el with unnecessary variables
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  :bind (
         ([escape] . keyboard-escape-quit) ;; Makes Escape quit prompts (Minibuffer Escape)
         ;; Zooming In/Out
         ("C-+" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease)
         )
  )

;; Must be set before evil loads
          ;; Basic evil customization
          (setq evil-want-keybinding nil
                evil-want-C-u-scroll t
                evil-want-C-i-jump nil
                evil-undo-system 'undo-redo)
          (use-package evil
              :init
              (evil-mode 1)
              :config
              ;; Set initial state for eat-mode
              (evil-set-initial-state 'eat-mode 'insert)



              ;; Jump to beginning of line or first non-blank character
              (defun start/jump-to-line-start ()
                "If at first non-blank char, go to beginning; else go to first non-blank."
                (interactive)
                (let ((col (current-column))
                      (first-non-blank (save-excursion
                                          (back-to-indentation)
                                          (current-column))))
                  (if (= col first-non-blank)
                      (move-beginning-of-line nil)
                    (back-to-indentation))))
              ;; H/L keybindings for motion state
              (define-key evil-motion-state-map "H" #'start/jump-to-line-start)
              (define-key evil-motion-state-map "L" #'evil-end-of-line)


          (defun evil-shift-right-keep-visual (beg end &optional count)
              "Shift right but stay in visual mode."
              (interactive "r\np")
              (evil-shift-right beg end count)
              (evil-normal-state)
              (evil-visual-restore))

              (defun evil-shift-left-keep-visual (beg end &optional count)
              "Shift left but stay in visual mode."
              (interactive "r\np")
              (evil-shift-left beg end count)
              (evil-normal-state)
              (evil-visual-restore))

          (define-key evil-visual-state-map (kbd ">") 'evil-shift-right-keep-visual)
          (define-key evil-visual-state-map (kbd "<") 'evil-shift-left-keep-visual)

          ;; Quick macro mapping
          (evil-define-key 'normal 'global (kbd "Q") (kbd "@q"))

          )
      (use-package evil-nerd-commenter
        :ensure t
        :after evil
        :config
        ;; 1. Bind 'gc' as an OPERATOR.
        ;; This automatically makes 'gcc' work for the current line
        ;; and 'gc' work with motions (like 'gcap' or 'gcG').
        (define-key evil-normal-state-map (kbd "gc") 'evilnc-comment-operator)  ;; Make it work in Visual mode too
        (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines)
      )

  (use-package evil-surround
    :ensure t
    :after evil
    :config
    (global-evil-surround-mode 1)

    ;; --- NORMAL MODE MAPPINGS ---

    ;; gsa Add surrounding (Usage: gsaw")
    (evil-define-key 'normal evil-surround-mode-map (kbd "gsa") 'evil-surround-region)

    ;; gsd Delete surrounding (Usage: gsd")
    (evil-define-key 'normal evil-surround-mode-map (kbd "gsd") 'evil-surround-delete)

    ;; gsr Replace surrounding (Usage: gsr'")
    (evil-define-key 'normal evil-surround-mode-map (kbd "gsr") 'evil-surround-change)


    ;; --- VISUAL MODE MAPPINGS ---

    ;; In Visual Mode, 'gsa' adds to the selection
    (evil-define-key 'visual evil-surround-mode-map (kbd "gsa") 'evil-surround-region)

    ;; Disable the default 's' in visual mode if it interferes
    (evil-define-key 'visual evil-surround-mode-map (kbd "s") nil))
(with-eval-after-load 'evil-surround
  (add-to-list 'evil-surround-pairs-alist '(?s . ("~" . "~"))) ; gsa s for ~code~
  (add-to-list 'evil-surround-pairs-alist '(?b . ("*" . "*"))) ; gsa b for *bold*
  (add-to-list 'evil-surround-pairs-alist '(?i . ("/" . "/")))) ; gsa i for /italics/

          ;; Evil-collection (after evil)
          (use-package evil-collection
              :after evil
              :config
              (setq evil-collection-mode-list
          	'(dired ibuffer magit corfu vertico consult info org))
              (evil-collection-init)

              ;; Reapply global H/L after evil-collection might override
              (define-key evil-motion-state-map "H" #'start/jump-to-line-start)
              (define-key evil-motion-state-map "L" #'evil-end-of-line))

(use-package general
          :config
          ;; (general-evil-setup) ;; <- evil
          ;; Set up 'C-SPC' as the leader key
          (general-create-definer start/leader-keys
            :states '(normal visual motion emacs) ;; <- evil
            :keymaps 'override
            :prefix "SPC"
            :global-prefix "C-SPC") ;; Set global leader key so we can access our keybindings from any state

          (start/leader-keys
            "SPC" '(consult-buffer :wk "Switch Buffer")
            "TAB" '(mode-line-other-buffer :wk "Previous Buffer")
            "C" '(comment-line :wk "Comment lines")
            "q" '(flymake-show-buffer-diagnostics :wk "Flymake buffer diagnostic")
            "t" '(eat :wk "Eat terminal")
        ;; "n" '(my/toggle-relative-line-numbers :wk "Toggle relative/absolute line numbers")
            "p" '(projectile-command-map :wk "Projectile")
            "s p" '(projectile-discover-projects-in-search-path :wk "Search for projects"))

          (start/leader-keys
            "f" '(:ignore t :wk "Find")
            "f c" '((lambda () (interactive) (find-file "~/.config/emacs/init.org")) :wk "Find emacs Config")
            "f r" '(consult-recent-file :wk "Find recent files")
            "f F" '(consult-fd :wk "Find files with fd")
    		"f f" '(find-file :wk "Find File")
            "f i" '(consult-imenu :wk "Find Imenu buffer locations")) ;; This one is really cool

          (start/leader-keys
            "s" '(:ignore t :wk "Search")
            "s g" '(consult-ripgrep :wk "Search with ripgrep")
            "s l" '(consult-line :wk "Search line"))

          (start/leader-keys
            "b" '(:ignore t :wk "Buffers")
            "b s" '(consult-buffer :wk "Switch buffer")
            "b k" '(kill-current-buffer :wk "Kill current buffer")
            "b i" '(ibuffer :wk "Ibuffer")
            "b n" '(next-buffer :wk "Next buffer")
            "b p" '(previous-buffer :wk "Previous buffer")
            "b r" '(revert-buffer :wk "Reload buffer"))

          (start/leader-keys
            "e" '(:ignore t :wk "Dired")
    		"e e" '(grease-here :wk "Explorer")
            "e v" '(dired :wk "Open dired")
            "e j" '(dired-jump :wk "Dired jump to current"))

          (start/leader-keys
            "o" '(:ignore t :wk "Org")
            "o a" '(org-agenda :wk "Agenda")
            "o c" '(org-capture :wk "Capture")
            "o l" '(org-store-link :wk "Store link")

            "n" '(:ignore t :wk "Notes (Roam)")
            "n f" '(org-roam-node-find :wk "Find node")
            "n i" '(org-roam-node-insert :wk "Insert node")
            "n b" '(org-roam-buffer-toggle :wk "Roam buffer")
            "n a" '(org-roam-alias-add :wk "Add Alias")
            "n r" '(org-roam-node-random :wk "Random node")
    		"n h" '(org-id-get-create :wk "Create Heading Node")
            ;; "n g" '(org-roam-ui-mode :wk "Graph UI")
    		"n u" '(my/org-roam-ui-mode-custom-browser :wk "Graph UI")
            )
    	  (start/leader-keys
            :major-modes 'org-mode
            ;;"o" '(:ignore t :wk "Org Local Leader")

            ;; Toggles
            "o t" '(org-todo :wk "Todo state")
            "o ." '(org-set-tags-command :wk "Set tags")
            "o p" '(org-priority :wk "Priority")

            ;; Insertion
            "o i" '(:ignore t :wk "Insert")
            "o i l" '(org-insert-link :wk "Insert link")
            "o i f" '(org-footnote-action :wk "Footnote")
            "o i n" '(org-add-note :wk "Add note")
            "o i p" '(org-download-clipboard :wk "Paste clipboard image")

            ;; Clocking (Time tracking)
            "o C" '(:ignore t :wk "Clock")
            "o C i" '(org-clock-in :wk "Clock in")
            "o C o" '(org-clock-out :wk "Clock out")
            "o C g" '(org-clock-goto :wk "Go to clock")

            ;; Actions
            "o e" '(org-export-dispatch :wk "Export")
            "o d" '(org-deadline :wk "Deadline")
            "o s" '(org-schedule :wk "Schedule")
            "o b" '(org-babel-tangle :wk "Tangle code")
    		"o r" '(org-refile :wk "Refile")
    		)


          (start/leader-keys
            "c" '(:ignore t :wk "Coding")
            "c e" '(eglot-reconnect :wk "Eglot Reconnect")
            "c d" '(eldoc-doc-buffer :wk "Eldoc Buffer")
            "c f" '(eglot-format :wk "Eglot Format")
            "c l" '(consult-flymake :wk "Consult Flymake")
            "c r" '(eglot-rename :wk "Eglot Rename")
            "c i" '(xref-find-definitions :wk "Find definition")
            "c v" '(:ignore t :wk "Elisp")
            "c v b" '(eval-buffer :wk "Evaluate elisp in buffer")
            "c v r" '(eval-region :wk "Evaluate elisp in region"))

          (start/leader-keys
            "g" '(:ignore t :wk "Git")
            "g s" '(magit-status :wk "Magit status"))

          (start/leader-keys
            "h" '(:ignore t :wk "Help") ;; To get more help use C-h commands (describe variable, function, etc.)
            "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
            ;;"h r" '((lambda () (interactive)
              ;;        (load-file "~/.config/emacs-org/init.el"))
                ;;    :wk "Reload Emacs config")
            "h a" '(apropos :wk "Apropos (Search all)")
            "h b" '(describe-bindings :wk "Describe bindings")
            "h c" '(describe-char :wk "Describe char at point")
            "h f" '(describe-function :wk "Describe function")
            "h k" '(describe-key :wk "Describe key")
            "h m" '(describe-mode :wk "Describe mode")
            "h v" '(describe-variable :wk "Describe variable")
            "h o" '(describe-symbol :wk "Describe symbol (DWIM)")
            "h p" '(describe-package :wk "Describe package")
            "h t" '(load-theme :wk "Load theme")
            "h r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload config"))

          (start/leader-keys
            "O" '(:ignore t :wk "Options")
            "O t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
            "O l" '(display-line-numbers-mode :wk "Toggle line numbers")
            "O n" '(my/toggle-relative-line-numbers :wk "Toggle relative/absolute line numbers")
    		)

        (start/leader-keys
            :major-modes 'pdf-view-mode
            "m" '(:ignore t :wk "PDF-View Options")
            "m t" '(pdf-view-midnight-minor-mode :wk "Toggle dark mode"))

    	  ;;  Save
    (general-define-key
    :states '(normal insert visual emacs)
    "C-s" (lambda ()
            (interactive)
            (save-buffer)
            (evil-normal-state)))

    ;; Copy/Paste
    (general-define-key
    :states 'visual
    "C-S-C" 'kill-ring-save) ; Copy selection to clipboard

    (general-define-key
    :states '(normal insert visual emacs)
    "C-S-V" 'yank)           ; Paste from clipboard

    ;; Move Windows
    (general-define-key
        :states '(normal insert visual emacs)
        "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right)
    (start/leader-keys
        "w"  '(:ignore t :wk "Windows")
        "w d" '(evil-window-delete :wk "Delete split")
        "w |" '(evil-window-vsplit :wk "Vertical split")
        "w _" '(evil-window-split  :wk "Horizontal split")
        "w =" '(balance-windows    :wk "Balance splits"))

(defun my/org-roam-ui-mode-custom-browser ()
  "Enable org-roam-ui-mode and open the UI in Chromium."
  (interactive)
  (unless org-roam-ui-mode
    (org-roam-ui-mode 1))
  (let ((browse-url-browser-function
         (lambda (url &optional _)
           (shell-command (concat "firefox --new-window " url)))))
    (org-roam-ui-open)))

          )

    ;; PDF View Keys
    (general-define-key
        :states 'motion
        :keymaps 'pdf-view-mode-map
        "j"  'pdf-view-next-line-or-next-page
        "k"  'pdf-view-previous-line-or-previous-page
        "gg" 'pdf-view-first-page
        "G"  'pdf-view-last-page
        "u"  'pdf-view-scroll-up-or-previous-page
        "d"  'pdf-view-scroll-down-or-next-page)

        (defvar my/relative-line-numbers t
      "Tracks whether relative line numbers are enabled.")

    (defun my/toggle-relative-line-numbers ()
      "Toggle between relative and absolute line numbers."
      (interactive)
      (setq my/relative-line-numbers (not my/relative-line-numbers))
      (setq display-line-numbers-type
            (if my/relative-line-numbers 'relative t))
      ;; Refresh line numbers
      (when (bound-and-true-p display-line-numbers-mode)
        (force-mode-line-update)
        (redraw-display)))


        ;; Fix general.el leader key not working instantly in messages buffer with evil mode
        ;; (use-package emacs
        ;;   :ghook ('after-init-hook
        ;;           (lambda (&rest _)
        ;;             (when-let ((messages-buffer (get-buffer "*Messages*")))
        ;;               (with-current-buffer messages-buffer
        ;;                 (evil-normalize-keymaps))))
        ;;           nil nil t)
        ;;   )

(use-package gruvbox-theme
  :config
  (setq gruvbox-bold-constructs t)
  ;;(load-theme 'gruvbox-dark-medium t)
) ;; We need to add t to trust this package
  (use-package doom-themes
    :ensure t
    :custom
    ;; Global settings (defaults)
    (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
    (doom-themes-enable-italic t) ; if nil, italics is universally disabled
    ;; for treemacs users
    (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    :config
    (load-theme 'doom-one t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (nerd-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

(add-to-list 'default-frame-alist '(alpha-background . 90)) ;; For all new frames henceforth

(set-face-attribute 'default nil
                    ;; :font "JetBrains Mono" ;; Set your favorite type of font or download JetBrains Mono
                    :height 145
                    :weight 'medium)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.

;;(add-to-list 'default-frame-alist '(font . "JetBrains Mono")) ;; Set your favorite font
(setq-default line-spacing 0.12)

(use-package doom-modeline
  :custom
  (doom-modeline-height 25) ;; Set modeline height
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package all-the-icons
:if (display-graphic-p)
:ensure t)
(use-package dashboard
:after all-the-icons ;; ensure icons functions are available
    :init
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)

    :custom
    (dashboard-startup-banner 'official)
    (dashboard-center-content t)

    ;; What shows up
    (dashboard-items
    '((agenda . 5)
        (recents . 5)
        (projects . 5)))

    ;; Custom buttons (Doom-style)
    (dashboard-navigator-buttons
    `(
        ((,(all-the-icons-octicon "calendar" :height 1.0)
        "Agenda"
        "Open org agenda"
        (lambda () (org-agenda nil "d"))))

        ((,(all-the-icons-octicon "book" :height 1.0)
        "Org files"
        "Open org directory"
        (lambda () (dired "~/org"))))

        ((,(all-the-icons-octicon "database" :height 1.0)
        "Org-roam"
        "Open org-roam buffer"
        (lambda () (org-roam-node-find))))))

    :config
    (dashboard-setup-startup-hook))

(use-package projectile
  :config
  (projectile-mode)
  :custom
  ;; (projectile-auto-discover nil) ;; Disable auto search for better startup times ;; Search with a keybind
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))) ;; . 1 means only search the first subdirectory level for projects

(use-package eglot
  :ensure nil ;; Don't install eglot because it's now built-in
  :hook ((c-mode c++-mode ;; Autostart lsp servers for a given mode
                 lua-mode) ;; Lua-mode needs to be installed
         . eglot-ensure)
  :custom
  ;; Good default
  (eglot-events-buffer-size 0) ;; No event buffers (LSP server logs)
  (eglot-autoshutdown t);; Shutdown unused servers.
  (eglot-report-progress nil) ;; Disable LSP server logs (Don't show lsp messages at the bottom, java)
  ;; Manual lsp servers
  ;;:config
  ;;(add-to-list 'eglot-server-programs
  ;;             `(lua-mode . ("PATH_TO_THE_LSP_FOLDER/bin/lua-language-server" "-lsp"))) ;; Adds our lua lsp server to eglot's server list
  )

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun start/install-treesit-grammars ()
  "Install missing treesitter grammars"
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

;; Call this function to install missing grammars
(start/install-treesit-grammars)

;; Optionally, add any additional mode remappings not covered by defaults
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (mhtml-mode . html-ts-mode)
        (javascript-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        ))

;; Or if there is no built in mode
(use-package cmake-ts-mode :ensure nil :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package go-ts-mode :ensure nil :mode "\\.go\\'")
(use-package go-mod-ts-mode :ensure nil :mode "\\.mod\\'")
(use-package rust-ts-mode :ensure nil :mode "\\.rs\\'")
(use-package tsx-ts-mode :ensure nil :mode "\\.tsx\\'")

(use-package eat
  :hook ('eshell-load-hook #'eat-eshell-mode))

(use-package org
                                :ensure nil
                    	  :hook (org-mode . org-indent-mode)
                                :custom
                                (org-edit-src-content-indentation 4) ;; Set src block automatic indent to 4 instead of 2.
                                (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links

                                ;; FOLDING
                                (org-ellipsis " ▾"
                                    org-hide-emphasis-markers t)
                                (org-cycle-separator-lines 2)

                                ;; LISTS
                                (org-list-allow-alphabetical t)
                                (org-list-indent-offset 2)
                                (org-adapt-indentation nil)

                                ;; TABLES
                                (org-table-convert-region-max-lines 10000)
                                (org-table-copy-increment t
                                    org-table-export-default-format "orgtbl-to-csv")

                                ;; STRUCTURE / FLOW
                                (org-special-ctrl-a/e t)
                                (org-special-ctrl-k t)
                                (org-yank-adjusted-subtrees t)
                                (org-M-RET-may-split-line '((default . t)))

                           	 ;; AGENDA
                           	 (org-agenda-prefix-format
                                    '((agenda . " %i %-12:c%?-12t% s")
                                    (todo   . " %i %-12:c")
                                    (tags   . " %i %-12:c")
                                    (search . " %i %-12:c")))
                                (org-agenda-span 'week)
                                (org-agenda-window-setup 'current-window)
                                (org-agenda-restore-windows-after-quit t)
                    			(org-agenda-start-with-log-mod t)
                    			(org-agenda-files
                    			 '("~/org/main/Tasks.org")
                    			 ("~/org/main/Projects.org"))
                    			(org-refile-targets
                    			 '(("Archive.org" :maxlevel . 1)
                    			   ("Tasks.org" :maxlevel . 1)
                    			   ("Projects.org" :maxlevel . 1)))
                    			(advice-add 'org-refile :after 'org-save-all-org-buffers)
                    			(org-log-done 'time)
                    			(org-log-into-drawer t)
                    			(org-todo-keywords
                    			 '((sequence "TODO(t)" "NEXT(n)" "PLANNING(p)" "REVIEW(v)" "READY(r)" "ACTIVE(a)" "HOLD(h)" "ASSIGNMENT(o)" "|" "DONE(d!)" "CANCELED(c!)")))
               				;; Note these also have to be set matching in Org-Modern
               (org-todo-keyword-faces
                       '(("TODO"     . (:foreground "#282c34" :background "#98be65" :weight bold))
                       ("NEXT"     . (:foreground "#282c34" :background "#6f8fff" :weight bold))
                       ("PLANNING" . (:foreground "#282c34" :background "#c792ea" :weight bold))
                       ("READY"    . (:foreground "#282c34" :background "#82b7ff" :weight bold))
                       ("ACTIVE"   . (:foreground "#282c34" :background "#7fdc6f" :weight bold))
                       ("REVIEW"   . (:foreground "#282c34" :background "#e0a96d" :weight bold))
                       ("HOLD"     . (:foreground "#282c34" :background "#e6d96c" :weight bold))
                       ("ASSIGNMENT"  . (:foreground "#282c34" :background "#e5404e" :weight bold))
                       ("DONE"     . (:foreground "#1f2328" :background "#304b60" :weight bold))
                       ("CANCELED" . (:foreground "#1f2328" :background "#e06c75" :weight bold))))

                                ;; (org-agenda-custom-commands
                                    ;; '(("d" "Daily agenda"
                                        ;; agenda ""
                                        ;; ((org-super-agenda-groups
                                        ;; '((:name "Today" :scheduled today)
                                            ;; (:name "Overdue" :deadline past)
                                            ;; (:name "Everything else" :anything t)))))))
                       	 ;; Custom agenda command
               (org-agenda-custom-commands
                     '(("d" "📅 Daily overview"
                        ((agenda ""
                                 ((org-agenda-span 5)   ; Span of 3 days
                                  (org-agenda-start-day "0d")
                                  (org-agenda-overriding-header "Today & Upcoming (next 5 days)")
                                  (org-super-agenda-groups
                                   '((:name "🔥 Today"
                                            :time-grid t
                                            :scheduled today
                                            :deadline today
                                            :order 1)
                                     (:name "⚠ Overdue"
                                            :deadline past
               							 :scheduled past  ; Catch items scheduled in the past too
                                            :order 2)
                                     (:name "📅 Upcoming"
                                            :scheduled future
                                            :deadline future
                                            :order 3)
                                     (:discard (:anything t))))))

                         (todo "NEXT"
                               ((org-agenda-overriding-header "🚀 Next tasks")
                                (org-super-agenda-groups
                                 '((:name "High priority"
                                          :priority "A")
                                   (:name "Normal"
                                          :anything t)))))

                         (todo "TODO"
                               ((org-agenda-overriding-header "📦 Backlog")
                                (org-super-agenda-groups
                                 '((:discard (:scheduled t)) ; drop scheduled from backlog
                                   (:anything t)))))))))                 ;;                             (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
                                :config
                                ;; Make RET context-aware like Doom
                                ;; (define-key org-mode-map (kbd "RET") #'org-return) ;; redefined below
                         (defun my/org-return-dwim ()
                           "Context-aware RET for Org (normal mode only)."
                           (interactive)
                           (cond
                           ;; Toggle checkbox
                           ((org-at-item-checkbox-p)
                             (org-toggle-checkbox))

                           ;; Follow links
                           ((and org-return-follows-link
                                 (org-in-regexp org-link-any-re))
                             (org-open-at-point))

                           ;; Tables
                           ((org-at-table-p)
                             (org-table-next-row))

                           ;; Lists
                           ((org-in-item-p)
                             (org-insert-item))

                           ;; Headings
                           ((org-at-heading-p)
                             (org-cycle))

                           ;; Fallback
                           (t
                             (org-return))))

                     (defun my/org-smart-insert-item-below ()
                    "Insert a new list item, checkbox, table row, or headline below the current line."
                    (interactive)
                      (org-back-to-heading)
                    (cond
                    ;; 1. In a Table
                    ((org-at-table-p)
                        (org-table-insert-row 'below))

                    ;; 2. On a Checkbox
                    ((org-at-item-checkbox-p)
                        (org-end-of-line)
                        (org-insert-item t)) ;; The 't' argument forces a checkbox

                    ;; 3. In a List (Ordered or Unordered)
                    ((org-in-item-p)
                        (org-end-of-line)
                        (org-insert-item))

                    ;; 4. On a Heading / TODO
                    ((org-at-heading-p)
                        (org-insert-heading-respect-content)
                        (when (org-entry-is-todo-p)
                        (org-todo 'nextset))) ;; Optional: matches TODO state of above line

                    ;; 5. Default: Just a normal newline
                    (t
                        (end-of-line)
                        (newline-and-indent)))
                    (org-update-checkbox-count t)
                    )

;;           (defun my/org-smart-insert-subheading ()
;;             "Insert a subheading after the current entry's content without splitting text."
;;             (interactive)
;;             ;; 1. Go to the start of the current heading
;;             (org-back-to-heading)
;;             ;; 2. Move to thenullgit  absolute end of that specific line
;aa3i1do; while for #n\0while b1;             (move-end-of-line 1)
;;             ;; 3. Insert heading at the end of all current sub-content
;;             (org-insert-heading-respect-content)
;;             ;; 4. Turn that new heading into a subheading
;;         (org-demote)
;; )
(defun my/org-smart-insert-sub-heading ()
  "Insert a nested item (subheading, sub-checkbox, or sub-list) below."
  (interactive)
  (cond
   ;; 1. On a Checkbox -> Insert a nested checkbox
   ((org-at-item-checkbox-p)
    (org-end-of-line)
    (org-insert-item t)nullgit
    (org-indent-item))

   ;; 2. In a List -> Insert a nested list item
   ((org-in-item-p)
    (org-end-of-line)
    (org-insert-item)
    (org-indent-item))

   ;; 3. On a Heading -> Insert a demoted heading at the end of content
   ((org-at-heading-p)
    ;; We use save-excursion to ensure we don't split the line
    (save-excursion
      (org-back-to-heading)
      (move-end-of-line 1)
      (org-insert-heading-respect-content)
      (org-demote))
    ;; Move point to the new heading
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (forward-line -1)
    (goto-char (line-end-position))
    (evil-insert-state))

   ;; 4. Default -> Normal behavior
   (t
    (end-of-line)
    (newline-and-indent)))
  (org-update-checkbox-count t))

                 (defun my/org-roam-ui-open-in-emacs ()
                   "Open org-roam-ui in an Emacs xwidget buffer."
                   (interactive)
                   (require 'xwidget)
                   (xwidget-webkit-browse-url "http://localhost:35901"))


                     )

                     (with-eval-after-load 'org
                       (define-key org-mode-map (kbd "RET") #'org-return))

                      (with-eval-after-load 'org
                      (add-hook 'emacs-startup-hook
                      	    (lambda ()
                      	    (run-at-time "0.5 sec" nil
                      			(lambda ()
                      			    (org-agenda nil "d"))))))

(defun my/org-meta-left-smart ()
  (interactive)
  (if (or (org-at-heading-p) (org-at-item-p)) (org-metaleft) (evil-shift-left (line-beginning-position) (line-end-position))))

(defun my/org-meta-right-smart ()
  (interactive)
  (if (or (org-at-heading-p) (org-at-item-p)) (org-metaright) (evil-shift-right (line-beginning-position) (line-end-position))))

(defun my/org-meta-down-smart ()
  "Move headline/item down if on one, otherwise drag the current line down."
  (interactive)
  (if (or (org-at-heading-p) (org-at-item-p))
      (org-metadown)
    (let ((col (current-column)))
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1)
      (move-to-column col))))

(defun my/org-meta-up-smart ()
  "Move headline/item up if on one, otherwise drag the current line up."
  (interactive)
  (if (or (org-at-heading-p) (org-at-item-p))
      (org-metaup)
    (let ((col (current-column)))
      (transpose-lines 1)
      (forward-line -2)
      (move-to-column col))))
    (use-package evil-org
        :ensure t
        :after org
        ;;:hook (org-mode . evil-org-mode)
        :hook (org-mode . (lambda () evil-org-mode))
        :config
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)
        (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
        ;; Set additional keybindings

        ;; Org element motions (Doom-style)
        (evil-define-key 'normal org-mode-map
            (kbd "gj") #'org-forward-element
            (kbd "gk") #'org-backward-element)
        (evil-define-key 'normal org-mode-map
            (kbd "]]") #'org-next-visible-heading
            (kbd "[[") #'org-previous-visible-heading
            (kbd "]h") #'org-forward-heading-same-level
            (kbd "[h") #'org-backward-heading-same-level)

        ;; Ensure org-mode keybindings are overridden
        ;; (evil-define-key 'normal org-mode-map
            ;; (kbd "M-h") 'org-metaleft
            ;; (kbd "M-j") 'org-metadown
            ;; (kbd "M-k") 'org-metaup
            ;; (kbd "M-l") 'org-metaright
            ;; )
        ;; (add-hook 'org-mode-hook
                    ;; (lambda ()
                    ;; (evil-define-key 'insert org-mode-map
                        ;; (kbd "M-h") 'org-metaleft
                        ;; (kbd "M-j") 'org-metadown
                        ;; (kbd "M-k") 'org-metaup
                        ;; (kbd "M-l") 'org-metaright
                        ;; )))
    	;; Smart Meta Movements (Normal & Insert states combined)
    (evil-define-key '(normal insert) org-mode-map
        (kbd "M-h") #'my/org-meta-left-smart
        (kbd "M-j") #'my/org-meta-down-smart
        (kbd "M-k") #'my/org-meta-up-smart
        (kbd "M-l") #'my/org-meta-right-smart)
    	)
    (with-eval-after-load 'evil-org
        ;; Normal mode mappings
        (evil-define-key 'normal org-mode-map (kbd "RET") #'my/org-return-dwim)
        (evil-define-key 'normal org-mode-map (kbd "C-<return>") #'my/org-smart-insert-item-below)
        (evil-define-key 'normal org-mode-map (kbd "C-RET")      #'my/org-smart-insert-item-below)
        (evil-define-key 'normal org-mode-map (kbd "C-M-j")      #'my/org-smart-insert-item-below)
        ;; Map double left click to cycle headings/logic in Normal state
        (evil-define-key 'normal org-mode-map
        (kbd "<double-mouse-1>")
        (lambda (event)
            (interactive "e")
            (mouse-set-point event) ; Move the cursor to where you double-clicked
            (my/org-return-dwim)))

            ;; Insert mode mappings
            (evil-define-key 'insert org-mode-map (kbd "C-<return>") #'my/org-smart-insert-item-below)
            (evil-define-key 'insert org-mode-map (kbd "C-RET")      #'my/org-smart-insert-item-below)
            (evil-define-key 'insert org-mode-map (kbd "C-M-j")      #'my/org-smart-insert-item-below)
            (evil-define-key 'normal org-mode-map (kbd "C-S-<return>") #'my/org-smart-insert-subheading)
            (evil-define-key 'insert org-mode-map (kbd "C-S-<return>") #'my/org-smart-insert-subheading)
            (evil-define-key 'normal org-mode-map (kbd "C-S-RET") #'my/org-smart-insert-subheading)
            (evil-define-key 'insert org-mode-map (kbd "C-S-RET") #'my/org-smart-insert-subheading)
    )

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

;; (use-package org-superstar
  ;; :after org
  ;; :hook (org-mode . org-superstar-mode))

(use-package org-modern
      :after org
      :custom
(org-modern-todo-faces
        '(("TODO"     . (:foreground "#282c34" :background "#98be65" :weight bold))
        ("NEXT"     . (:foreground "#282c34" :background "#6f8fff" :weight bold))
        ("PLANNING" . (:foreground "#282c34" :background "#c792ea" :weight bold))
        ("READY"    . (:foreground "#282c34" :background "#82b7ff" :weight bold))
        ("ACTIVE"   . (:foreground "#282c34" :background "#7fdc6f" :weight bold))
        ("REVIEW"   . (:foreground "#282c34" :background "#e0a96d" :weight bold))
        ("HOLD"     . (:foreground "#282c34" :background "#e6d96c" :weight bold))
        ("ASSIGNMENT"     . (:foreground "#282c34" :background "#e5404e" :weight bold))
        ("DONE"     . (:foreground "#1f2328" :background "#304b60" :weight bold))
        ("CANCELED" . (:foreground "#1f2328" :background "#e06c75" :weight bold))))

      :hook (org-mode . org-modern-mode)
)

    (with-eval-after-load 'org-modern
      (custom-set-variables
       '(org-modern-checkbox
         '((?X . "☑")  ; checked
           (?- . "❍")  ; intermediate
           (?\s . "☐")))))  ; unchecked

(use-package org-table-sticky-header
  :after org
  :hook (org-mode . org-table-sticky-header-mode))

(use-package org-super-agenda
:after org
:hook (org-agenda-mode . org-super-agenda-mode)

:custom
(org-super-agenda-groups
'(
    (:name "🔥 Today"
    :time-grid t
    :scheduled today
    :order 1)

    (:name "⚠ Overdue"
    :deadline past
    :order 2)

    (:name "📌 Important"
    :priority "A"
    :order 3)

    (:name "📅 Upcoming"
    :deadline future
    :order 4)

    (:name "🧾 Other"
    :anything t
    :order 99))))

(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-roam
    :custom
    (org-roam-directory (file-truename "~/org/roam"))
    (org-roam-completion-everywhere t)
    (org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))
    :bind (("C-c n f" . org-roam-node-find)
            ("C-c n i" . org-roam-node-insert)
            ("C-c n b" . org-roam-buffer-toggle))
    :config
    (org-roam-db-autosync-mode)
    (setq org-roam-capture-templates
        '(("d" "default" plain
            "%?"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}\n")
            :unnarrowed t))))

(use-package org-download
:after org
:hook ((dired-mode . org-download-enable)
	(org-mode . org-download-enable))
:config
(setq org-download-method 'directory)             ;; Save images to a directory
(setq org-download-image-dir "images")            ;; The directory name (e.g. ./images)
(setq org-download-heading-lvl nil)               ;; Don't use headings for sub-folders
(setq org-download-timestamp "%Y%m%d-%H%M%S_")    ;; Timestamp file names
(setq org-download-screenshot-method "xclip")     ;; "scrot", "gnome-screenshot", or "xclip" (Linux)
						    ;; On Mac, it uses "pngpaste" automatically if installed
)

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; (require 'start-multiFileExample)

;; (start/hello)

(use-package magit
  :defer
  :custom (magit-diff-refine-hunk (quote all)) ;; Shows inline diff
  :config (define-key transient-map (kbd "<escape>") 'transient-quit-one) ;; Make escape quit magit prompts
  )

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

(use-package corfu
      ;; Optional customizations
      :custom
      (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
      (corfu-auto t)                 ;; Enable auto completion
	  (corfu-auto-trigger ".") ;; Custom trigger characters
      (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
      (corfu-popupinfo-mode t)       ;; Enable popup information
      (corfu-popupinfo-delay 0.5)    ;; Lower popup info delay to 0.5 seconds from 2 seconds
      (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
      (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
      (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
      ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
      ;; (corfu-scroll-margin 5)        ;; Use scroll margin
      (completion-ignore-case t)
        (corfu-preselect 'prompt)      ;; Focus stays on your typing, not the first result
    	(corfu-preview-current t)      ;; Preview changes in buffer as you cycle
        (corfu-on-exact-match nil)     ;; Don't finish just because you typed the word

      ;; Emacs 30 and newer: Disable Ispell completion function.
      ;; Try `cape-dict' as an alternative.
      (text-mode-ispell-word-completion nil)

      ;; Enable indentation+completion using the TAB key.
      ;; `completion-at-point' is often bound to M-TAB.
      (tab-always-indent 'complete)

      ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
      ;; be used globally (M-/).  See also the customization variable
      ;; `global-corfu-modes' to exclude certain modes.
      :init
      (global-corfu-mode)

)

    (use-package nerd-icons-corfu
      :after corfu
      :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after corfu
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.

  ;; The functions that are added later will be the first in the list
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers
  (add-hook 'completion-at-point-functions #'cape-dict) ;; Dictionary completion
  (add-hook 'completion-at-point-functions #'cape-file) ;; Path completion
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ;; Complete elisp in Org or Markdown mode
  (add-hook 'completion-at-point-functions #'cape-keyword) ;; Keyword completion

  ;;(add-hook 'completion-at-point-functions #'cape-abbrev) ;; Complete abbreviation
  ;;(add-hook 'completion-at-point-functions #'cape-history) ;; Complete from Eshell, Comint or minibuffer history
  ;;(add-hook 'completion-at-point-functions #'cape-line) ;; Complete entire line from current buffer
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol) ;; Complete Elisp symbol
  ;;(add-hook 'completion-at-point-functions #'cape-tex) ;; Complete Unicode char from TeX command, e.g. \hbar
  ;;(add-hook 'completion-at-point-functions #'cape-sgml) ;; Complete Unicode char from SGML entity, e.g., &alpha
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345) ;; Complete Unicode char using RFC 1345 mnemonics
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(savehist-mode) ;; Enables save history mode

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  )

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure nil ;; Don't install which-key because it's now built-in
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha) ;; Same as default, except single characters are sorted alphabetically
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1) ;; Number of spaces to add to the left of each column
  (which-key-min-display-lines 6)  ;; Increase the minimum lines to display because the default is only 1
  (which-key-idle-delay 0.8)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package ws-butler
  :init (ws-butler-global-mode))

(use-package avy
    :ensure t
    :config
    (evil-define-key 'normal 'global (kbd "s") 'avy-goto-char-timer))

;;(use-package command-log-mode)

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :init
  (require 'seq) ;; Ensure seq library is loaded
  (evil-terminal-cursor-changer-activate) ; or (etcc-on)
  :custom
  (evil-motion-state-cursor 'box)  ; █
  (evil-visual-state-cursor 'box)  ; █
  (evil-normal-state-cursor 'box)  ; █
  (evil-insert-state-cursor 'bar)  ; ⎸
  (evil-emacs-state-cursor  'hbar)) ; _

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package grease
  :load-path "~/.config/emacs-org/lisp/grease.el"
    :commands (grease-open grease-toggle grease-here)
    :init
    ;; Icons (requires nerd-icons package)
    (setq grease-use-icons t)              ; Set to nil to disable icons

    ;; Sorting options
    (setq grease-sort-method 'type)        ; Default sort method
    ;; Available methods:
    ;;   'type      - Directories first, then files (default)
    ;;   'name      - Alphabetical by name
    ;;   'size      - By file size (smallest first)
    ;;   'size-desc - By file size (largest first)
    ;;   'date      - By modification date (oldest first)
    ;;   'date-desc - By modification date (newest first)
    ;;   'extension - By file extension

    (setq grease-sort-directories-first t) ; Always show dirs first (for non-type sorts)

    ;; Hidden files
    (setq grease-show-hidden nil)          ; Set to t to show dotfiles by default

    ;; Preview window
    (setq grease-preview-window-width 0.4) ; Preview takes 40% of frame width
    (setq grease-preview-writable nil)     ; Set to t to make file previews editable
    )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
