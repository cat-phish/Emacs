;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; (defun start/org-babel-tangle-config ()
;;   "Automatically tangle and refresh quickstart, strictly suppressing warnings."
;;   (interactive)
;;   (when (string-equal (file-name-directory (buffer-file-name))
;; 					  (expand-file-name user-emacs-directory))
;; 	(let ((org-confirm-babel-evaluate nil)
;; 		  ;; Suppress all byte-compile and native-comp warnings temporarily
;; 		  (byte-compile-warnings nil)
;; 		  (warning-minimum-level :error)
;; 		  ;; Prevent the buffer from popping up
;; 		  (display-buffer-alist '(("\\*Compile-Log\\*" (display-buffer-no-window))
;; 								  ("\\*Warnings\\*" (display-buffer-no-window)))))
;; 	  (org-babel-tangle)
;; 	  ;; Use 'quietly' if your Emacs version supports it, otherwise refresh
;; 	  (package-quickstart-refresh)
;; 	  (message "Config tangled and package-quickstart refreshed!"))))
;; (setq native-comp-async-report-warnings-errors 'silent) ;; For Emacs 28+
;; (setq byte-compile-warnings '(not free-vars unresolved)) ;; Suppress common nagging warnings
;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))
(setq org-export-with-broken-links t)
(defun start/org-babel-tangle-config ()
  "Automatically tangle and export to README.md, strictly suppressing warnings."
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
      ;; Tangle the config
      (org-babel-tangle)

      ;; Export to README.md (without source blocks)
      (let ((org-export-with-toc nil)  ;; Optional: disable table of contents
            (org-export-with-author nil)  ;; Optional: disable author
            (org-export-with-date nil))   ;; Optional: disable date
        (org-md-export-to-markdown))

      ;; Refresh package quickstart
      (package-quickstart-refresh)
      (message "Config tangled, README.md exported, and package-quickstart refreshed!"))))

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
  (inhibit-startup-screen t)  ;; Disable welcome screen
  (inhibit-startup-message t) ;; Disable screen that shows on first install


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
  (pixel-scroll-precision-mode 1) ;; Modern Browser like smooth scrolling (Emacs 29+)
  (scroll-conservatively 101) ;; Smooth scrolling
  (scroll-preserve-screen-position nil)
  ;; (scroll-margin 15)
  (scroll-margin (if (< (window-body-height) 30) 0 5)) ;; was set at 15, testing

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

(defconst start/is-windows (eq system-type 'windows-nt))

(defconst start/enable-dev-features (not start/is-windows)
  "When non-nil, enable programming/development tooling.")

(defconst start/org-root
  (expand-file-name (if start/is-windows "C:/Users/Jordan/org/" "~/org/")))

(defconst start/org-main-dir
  (expand-file-name "main/" start/org-root))

(defconst start/org-roam-dir
  (expand-file-name "roam/" start/org-root))

(defconst start/org-tasks-file
  (expand-file-name "Tasks.org" start/org-main-dir))

(defconst start/org-projects-file
  (expand-file-name "Projects.org" start/org-main-dir))

(defconst start/org-inbox-file
  (expand-file-name "Inbox.org" start/org-main-dir))

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

  ;; Visual state - stay in visual mode while indenting
  (define-key evil-visual-state-map (kbd ">") 'evil-shift-right-keep-visual)
  (define-key evil-visual-state-map (kbd "<") 'evil-shift-left-keep-visual)

  ;; Only bind M-h/M-l for indenting when NOT in org-mode
  (with-eval-after-load 'evil
    (evil-define-key 'visual 'global (kbd "M-l") 'evil-shift-right-keep-visual)
    (evil-define-key 'visual 'global (kbd "M-h") 'evil-shift-left-keep-visual)
    (evil-define-key 'normal 'global (kbd "M-l") 'evil-shift-right-line)
    (evil-define-key 'normal 'global (kbd "M-h") 'evil-shift-left-line))

  ;; Quick macro mapping
  (evil-define-key 'normal 'global (kbd "Q") (kbd "@q"))

  )

;; Add pipe to text objects
(defmacro define-and-bind-quoted-text-object (name key start-regexp end-regexp)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regexp ,end-regexp beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

;; Bind the pipe | to a text object
(define-and-bind-quoted-text-object "pipe" "|" "|" "|")

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

  ;; SURROUNDS
  ;; gsa Add surrounding (Usage: gsaw")
  (evil-define-key 'normal evil-surround-mode-map (kbd "gsa") 'evil-surround-region)
  ;; gsd Delete surrounding (Usage: gsd")
  (evil-define-key 'normal evil-surround-mode-map (kbd "gsd") 'evil-surround-delete)
  ;; gsr Replace surrounding (Usage: gsr'")
  (evil-define-key 'normal evil-surround-mode-map (kbd "gsr") 'evil-surround-change)
  ;; In Visual Mode, 'gsa' adds to the selection
  (evil-define-key 'visual evil-surround-mode-map (kbd "gsa") 'evil-surround-region)
  ;; Disable the default 's' in visual mode if it interferes
  (evil-define-key 'visual evil-surround-mode-map (kbd "s") nil))
(with-eval-after-load 'evil-surround
  (add-to-list 'evil-surround-pairs-alist '(?s . ("~" . "~"))) ; gsa s for ~code~
  (add-to-list 'evil-surround-pairs-alist '(?b . ("*" . "*"))) ; gsa b for *bold*
  (add-to-list 'evil-surround-pairs-alist '(?i . ("/" . "/"))) ; gsa i for /italics/
  )

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
    "q" '(flymake-show-buffer-diagnostics :wk "Flymake buffer diagnostic"))

  (when start/enable-dev-features
    (start/leader-keys
      "t" '(eat :wk "Eat terminal")
      ;; "n" '(my/toggle-relative-line-numbers :wk "Toggle relative/absolute line numbers")
      "p" '(projectile-command-map :wk "Projectile")
      "s p" '(projectile-discover-projects-in-search-path :wk "Search for projects")))

  (start/leader-keys
    "f" '(:ignore t :wk "find")
    "f c" '((lambda () (interactive) (find-file (expand-file-name "init.org" user-emacs-directory))) :wk "Find emacs Config")
    "f r" '(consult-recent-file :wk "Find recent files")
    "f F" '(consult-fd :wk "Find files with fd")
    "f f" '(find-file :wk "Find File")
    "f i" '(consult-imenu :wk "Find Imenu buffer locations")) ;; This one is really cool

  (start/leader-keys
    "s" '(:ignore t :wk "search")
    "s g" '(consult-ripgrep :wk "Search with ripgrep")
    "s l" '(consult-line :wk "Search line"))

  (start/leader-keys
    "b" '(:ignore t :wk "buffers")
    "b s" '(consult-buffer :wk "Switch buffer")
    "b d" '(kill-current-buffer :wk "Delete current buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (start/leader-keys
    "e" '(:ignore t :wk "Explorer")
    "e e" '(grease-here :wk "Explorer")
    "e v" '(dired :wk "Open dired")
    "e j" '(dired-jump :wk "Dired jump to current"))

  (start/leader-keys
    "o" '(:ignore t :wk "org")
    "o a" '(org-agenda :wk "Agenda")
    "o o" '((lambda () (interactive) (org-agenda nil "d")) :wk "Overview (Daily)")
    "o c" '(org-capture :wk "Capture")
    "o l" '(org-store-link :wk "Store link")

    "n" '(:ignore t :wk "notes (roam)")
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
    "o t" '(org-todo :wk "Todo State")
    "o ." '(org-set-tags-command :wk "Set tags")
    "o p" '(org-priority :wk "Priority")

    ;; Insertion
    "o i" '(:ignore t :wk "insert")
    "o i l" '(org-insert-link :wk "Insert link")
    "o i f" '(org-footnote-action :wk "Footnote")
    "o i n" '(org-add-note :wk "Add note")
    "o i p" '(org-download-clipboard :wk "Paste clipboard image")

    ;; Folding
    "o f" '(:ignore t :wk "fold")
    "o f d" '(my/org-cycle-done-entries :wk "Fold Done")
    "o f t" '(my/org-cycle-todo-entries :wk "Fold Todo")
    "o f a" '(my/org-cycle-all-todo-done-entries  :wk "Fold All Todo/Done")

    ;; Clocking (Time tracking)
    "o C" '(:ignore t :wk "clock")
    "o C i" '(org-clock-in :wk "Clock in")
    "o C o" '(org-clock-out :wk "Clock out")
    "o C g" '(org-clock-goto :wk "Go to clock")

    ;; Actions
    "o e" '(org-export-dispatch :wk "Export")
    "o d" '(org-deadline :wk "Deadline")
    "o s" '(org-schedule :wk "Schedule")
    "o S" '(org-sort :wk "Sort region/list")
    "o T" '(org-set-tags-command :wk "Set Tags")
    "o r" '(org-refile :wk "Refile")
    )


  (start/leader-keys
    "c" '(:ignore t :wk "coding")
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
    "g" '(:ignore t :wk "git")
    "g s" '(magit-status :wk "Magit status"))

  (start/leader-keys
    "h" '(:ignore t :wk "help") ;; To get more help use C-h commands (describe variable, function, etc.)
    "h q" '(save-buffers-kill-emacs :wk "Quit Emacs and Daemon")
    "h a" '(apropos :wk "Apropos (Search all)")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe char at point")
    "h f" '(describe-function :wk "Describe function")
    "h k" '(describe-key :wk "Describe key")
    "h m" '(describe-mode :wk "Describe mode")
    "h v" '(describe-variable :wk "Describe variable")
    "h o" '(describe-symbol :wk "Describe symbol (DWIM)")
    "h p" '(describe-package :wk "Describe package")
    "h t" '(org-babel-tangle :wk "Tangle code")
    "h T" '(load-theme :wk "Load theme")
    "h r" '((lambda () (interactive) (load-file user-init-file)) :wk "Reload config"))

  (start/leader-keys
    "O" '(:ignore t :wk "options")
    "O t" '(visual-line-mode :wk "Toggle truncated lines (wrap)")
    "O l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "O n" '(my/toggle-relative-line-numbers :wk "Toggle relative/absolute line numbers")
    )

  (start/leader-keys
    "y"   '(:ignore t :wk "yank")
    "y c" '(copy-region-as-kill :wk "Yank to clipboard")
    "y b" '( (lambda ()
               (interactive)
               (kill-new (buffer-string))
               (message "Entire buffer yanked to clipboard."))
    		 :wk "Yank entire buffer")
    "y h" '(my/org-yank-entire-subtree :wk "Yank heading + subtree")
    )

  (start/leader-keys
    :major-modes 'pdf-view-mode
    "m" '(:ignore t :wk "pdf-view options")
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
    "w"  '(:ignore t :wk "windows")
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
             (shell-command (concat "firefox --kiosk --new-window " url)))))
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

;; CUSTOM FONT SIZE BASED ON HOSTNAME
;; Apply text scaling for laptop (equivalent to Ctrl - once)
(when (string-match-p "fedora-laptop" (system-name))
  (setq text-scale-mode-step 1.1)  ; This is the default step
  (set-face-attribute 'default nil :height
                      (floor (* 145 (expt text-scale-mode-step -1)))))

(use-package doom-modeline
  :custom
  (doom-modeline-height 25) ;; Set modeline height
  (column-number-mode t)     ;; Enable column number display
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons
  :if (display-graphic-p)
  :ensure t)

(use-package dashboard
  :after nerd-icons
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)  ; Tell dashboard to use nerd-icons

  :custom
  (dashboard-startup-banner 'official)
  (dashboard-center-content t)

  ;; What shows up
  (dashboard-items
   '((agenda . 5)
     (recents . 5)
     (projects . 5)))

  ;; Custom buttons (using nerd-icons)
  (dashboard-navigator-buttons
   `(
     ((,(nerd-icons-mdicon "nf-md-calendar" :height 1.0)
       "Agenda"
       "Open org agenda"
       (lambda () (org-agenda nil "d"))))

     ((,(nerd-icons-mdicon "nf-md-book_open_variant" :height 1.0)
       "Org files"
       "Open org directory"
       (lambda () (dired start/org-root))))

     ((,(nerd-icons-mdicon "nf-md-graph" :height 1.0)
       "Org-roam"
       "Open org-roam buffer"
       (lambda () (org-roam-node-find))))))

  :config
  (dashboard-setup-startup-hook))

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

(defun my/evil-org-delete-heading-dwim (count)
  "Delete subtree if heading is folded (linewise); otherwise delete line normally."
  (interactive "p")
  (cond
   ;; CASE 1: Folded Heading -> Delete Subtree Linewise
   ((and (org-at-heading-p)
         (or (and (fboundp 'org-fold-folded-p)
                  (org-fold-folded-p))
             (outline-invisible-p (line-end-position))))
    (let ((beg (line-beginning-position))
          (end (save-excursion
                 ;; 't t' forces it to move to the start of the NEXT heading
                 (org-end-of-subtree t t)
                 (point))))
      ;; If at End of Buffer, ensure we claim the final newline so no gap remains
      (when (eobp) (setq end (point-max)))

      (evil-delete beg end 'line)))
   ;; CASE 2: Everything else -> Standard Evil Line Delete
   (t
    (evil-delete (line-beginning-position)
                 (line-beginning-position (1+ count))
                 'line
                 ?\"))))

;; (ref:insert-item-below)
(defun my/org-smart-insert-item-below ()
  "Insert a new list item, checkbox, table row, or headline below the current line."
  (interactive)
  ;; (org-back-to-heading)
  (cond
   ;; Tables
   ;; Tables: Insert row, then insert h-line below it
   ((org-at-table-p)
    (org-table-insert-hline) ; This puts a line below the row we just made
    (forward-line 1)        ; Move back up into the empty row
    (org-table-insert-row 'below)
    (org-table-align)
    (evil-insert-state))
   ;; Checkboxes
   ((org-at-item-checkbox-p)
    (org-end-of-line)
    (org-insert-item t) ;; The 't' argument forces a checkbox
    (evil-insert-state))
   ;; Lists
   ((org-in-item-p)
    (org-end-of-line)
    (org-insert-item)
    (evil-insert-state))
   ;; Headings/TODOs
   ((org-at-heading-p)
    (org-insert-heading-respect-content)
    (when (org-entry-is-todo-p)
      (org-todo 'nextset)) ;; matches TODO state of above line
    (evil-insert-state))
   ;; Default: Just a normal newline
   (t
    (end-of-line)
    (newline-and-indent)
    (evil-insert-state)))
  (org-update-checkbox-count t)
  )

;; INSERT SUBITEM BELOW
;; (ref:insert-subitem-below)
(defun my/org-smart-insert-subitem ()
  "Insert a nested item (sub-heading, sub-checkbox, or sub-list) below."
  (interactive)
  (cond
   ;; Checkbox -> Insert a nested checkbox
   ((org-at-item-checkbox-p)
    (org-end-of-line)
    (org-insert-item t)
    (org-indent-item)
    (evil-insert-state))
   ;; List -> Insert a nested list item
   ((org-in-item-p)
    (org-end-of-line)
    (org-insert-item)
    (org-indent-item)
    (evil-insert-state))
   ;; On a Heading -> Insert a demoted heading at the end of content
   ((org-at-heading-p)
    ;; Use save-excursion to ensure we don't split the line
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
   ;; Default -> Normal behavior

   (t
    (end-of-line)
    (newline-and-indent)
    (evil-insert-state)))
  (org-update-checkbox-count t))

;; (ref:insert-parent-heading)
(defun my/org-insert-parent-heading-below ()
  "Insert a new heading at the appropriate level.
        If on a heading line, go up one level and insert a sibling.
        If in content (checkbox, text, etc.), insert a sibling of current heading."
  (interactive)
  (if (org-at-heading-p)
      ;; We're ON a heading line - go up one level
      (progn
        (org-back-to-heading)
        (when (> (org-current-level) 1)  ; Only go up if not already at level 1
    	  (org-up-heading-safe))
        (org-insert-heading-respect-content)
        (evil-insert-state))
    ;; We're in content - insert sibling of current heading
    (progn
      (org-back-to-heading)
      (org-insert-heading-respect-content)
      (evil-insert-state))))

(defun my/org-yank-entire-subtree ()
  "Copy the current Org subtree (heading and all sub-contents) to the kill ring."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn
          (org-back-to-heading t)
          (org-copy-subtree)
          (message "Subtree copied to kill ring."))
      (error (message "Point is not in an Org subtree.")))))

;; (ref:auto-save)
(defun my/org-save-all-except ()
  "Save all Org buffers except for files matching specific strings."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'org-mode)
                 (buffer-file-name)
                 (buffer-modified-p)
    			 (not
    			  ;; Exempt buffer names
    			  (string-match-p "init\\.org" (buffer-file-name))
    			  )
    			 )
        (save-buffer)))))
(run-with-idle-timer 30 t #'my/org-save-all-except)

;; (ref:format-src)
(defun my/org-indent-all-src-blocks ()
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (org-babel-do-in-edit-buffer (indent-region (point-min) (point-max)))))))))
(add-hook 'before-save-hook #'my/org-indent-all-src-blocks)

;; (ref:refile-fix)
(add-hook 'org-after-refile-insert-hook
    	  (lambda ()
            (save-excursion
    		  (org-back-to-heading t)
    		  ;; Move to the parent heading to check its state
    		  (when (org-up-heading-safe)
                ;; Check if the parent heading is currently folded
                (let ((folded (save-excursion
                                (end-of-line)
                                (invisible-p (point)))))
    			  (when folded
                    ;; If it was folded, re-hide the subtree we just moved
                    ;; This fixes the 'ghost' display issue
                    (org-back-to-heading t)
                    (org-flag-subtree t)))))))

;; (ref:todo-cycle)
(defun my/org-toggle-todo-entries ()
  "Cycle visibility for all active TODO entries (non-DONE states)."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((is-todo (member (org-get-todo-state) org-not-done-keywords)))
       (when is-todo
         (if (outline-invisible-p (line-end-position))
             (outline-show-subtree)
           (outline-hide-subtree)))))
   nil 'file)
  (message "Toggled TODO entries"))
(defun my/org-toggle-done-entries ()
  "Cycle visibility for all DONE entries without hanging."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((is-done (member (org-get-todo-state) org-done-keywords)))
       (when is-done
         (if (outline-invisible-p (line-end-position))
             (outline-show-subtree)
           (outline-hide-subtree)))))
   nil 'file)
  (message "Toggled DONE entries"))
(defun my/org-cycle-all-todo-done-entries ()
  "Cycle visibility for all entries with TODO states (both TODO and DONE).
                                   If any are visible, hide all. If all are hidden, show all."
  (interactive)
  (let ((any-visible nil))
    ;; First pass: check if any TODO-state entries are visible
    (org-map-entries
     (lambda ()
       (let ((has-todo-state (org-get-todo-state)))
         (when (and has-todo-state (not (outline-invisible-p (line-end-position))))
           (setq any-visible t))))
     nil 'file)
    ;; Second pass: apply consistent action to all TODO-state entries
    (org-map-entries
     (lambda ()
       (let ((has-todo-state (org-get-todo-state)))
         (when has-todo-state
           (if any-visible
               (outline-hide-subtree)
             (outline-show-subtree)))))
     nil 'file)
    (message (if any-visible "Hidden all TODO-state entries" "Shown all TODO-state entries"))))
(defun my/org-cycle-todo-entries ()
  "Cycle visibility for all TODO entries (non-DONE states).
        If any are visible, hide all. If all are hidden, show all."
  (interactive)
  (let ((any-visible nil))
    ;; First pass: check if any TODO entries are visible
    (org-map-entries
     (lambda ()
       (let ((is-todo (member (org-get-todo-state) org-not-done-keywords)))
         (when (and is-todo (not (outline-invisible-p (line-end-position))))
           (setq any-visible t))))
     nil 'file)
    ;; Second pass: apply consistent action to all TODO entries
    (org-map-entries
     (lambda ()
       (let ((is-todo (member (org-get-todo-state) org-not-done-keywords)))
         (when is-todo
           (if any-visible
               (outline-hide-subtree)
             (outline-show-subtree)))))
     nil 'file)
    (message (if any-visible "Hidden TODO entries" "Shown TODO entries"))))
(defun my/org-cycle-done-entries ()
  "Cycle visibility for all DONE entries.
                                   If any are visible, hide all. If all are hidden, show all."
  (interactive)
  (let ((any-visible nil))
    ;; First pass: check if any DONE entries are visible
    (org-map-entries
     (lambda ()
       (let ((is-done (member (org-get-todo-state) org-done-keywords)))
         (when (and is-done (not (outline-invisible-p (line-end-position))))
           (setq any-visible t))))
     nil 'file)
    ;; Second pass: apply consistent action to all DONE entries
    (org-map-entries
     (lambda ()
       (let ((is-done (member (org-get-todo-state) org-done-keywords)))
         (when is-done
           (if any-visible
               (outline-hide-subtree)
             (outline-show-subtree)))))
     nil 'file)
    (message (if any-visible "Hidden DONE entries" "Shown DONE entries"))))

;; (ref:launch-done-collapse)
(defun my/org-hide-done-entries-dynamic ()
  "Hide all entries that are in any 'DONE' state defined in the current buffer."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-map-entries
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (member state org-done-keywords)
           (outline-hide-subtree))))
     t 'file)))

;; Source - https://stackoverflow.com/a/17492723
;; Retrieved 2026-02-15, License - CC BY-SA 3.0

;; Override org-mode's drawer hiding function to actually hide drawers properly
(with-eval-after-load 'org
  ;; 1. Core Functions using Overlays (100% safe from Org's internal parser)
  (defun my/hide-drawers-completely (beg end)
    "Use overlays to completely vanish PROPERTIES and LOGBOOK drawers by eating the PRECEDING newline."
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):" end t)
        (let* ((drawer-start (line-beginning-position))
               (ov-start (if (and (> drawer-start (point-min))
                                  (eq (char-before drawer-start) ?\n))
                             (1- drawer-start)
                           drawer-start)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
            (let ((ov-end (match-end 0)))
              (remove-overlays ov-start ov-end 'my-stealth-drawer t)
              (let ((ov (make-overlay ov-start ov-end)))
                (overlay-put ov 'display "")
                (overlay-put ov 'my-stealth-drawer t))))))))

  (defun my/show-drawers-completely (beg end)
    "Remove the stealth overlays to reveal the drawers."
    (remove-overlays beg end 'my-stealth-drawer t))

  (defun my/org-hide-drawers-subtree ()
    (save-excursion
      (org-back-to-heading t)
      (my/hide-drawers-completely (point) (save-excursion (org-end-of-subtree t) (point)))))

  (defun my/org-show-drawers-subtree ()
    (save-excursion
      (org-back-to-heading t)
      (my/show-drawers-completely (point) (save-excursion (org-end-of-subtree t) (point)))))

  (defun my/org-hide-drawers-global ()
    (my/hide-drawers-completely (point-min) (point-max)))

  (defun my/org-show-drawers-global ()
    (my/show-drawers-completely (point-min) (point-max)))

  ;; 2. Detection Logic
  (defun my/org-drawers-hidden-p ()
    (save-excursion
      (org-back-to-heading t)
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (if (re-search-forward "^[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):" end t)
            (let ((ovs (overlays-at (line-beginning-position)))
                  (is-hidden nil))
              (dolist (ov ovs)
                (when (overlay-get ov 'my-stealth-drawer)
                  (setq is-hidden t)))
              is-hidden)
          t))))

  (defun my/org-drawers-hidden-global-p ()
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):" nil t)
          (let ((ovs (overlays-at (line-beginning-position)))
                (is-hidden nil))
            (dolist (ov ovs)
              (when (overlay-get ov 'my-stealth-drawer)
                (setq is-hidden t)))
            is-hidden)
        t)))

  ;; 3. Smart TODO Expander
  (defun my/org-expand-todos-smart (&optional global)
    "Show content of TODO children, collapse DONE children.
Returns t if active TODOs were found, nil otherwise."
    (interactive)
    (let ((has-active-todos nil))
      (save-excursion
        (if global
            (progn
              (org-show-all)
              (my/org-hide-drawers-global)
              (setq has-active-todos t))
          (org-back-to-heading t)
          (org-show-subtree)
          (my/org-hide-drawers-subtree)
          (org-map-entries
           (lambda ()
             (when (member (org-get-todo-state) org-not-done-keywords)
               (setq has-active-todos t)))
           nil 'tree))

        (when has-active-todos
          (org-map-entries
           (lambda ()
             (let ((state (org-get-todo-state)))
               (when (and state (member state org-done-keywords))
                 (if (fboundp 'org-fold-subtree)
                     (org-fold-subtree t)
                   (outline-hide-subtree))
                 (setq org-map-continue-from (save-excursion (org-end-of-subtree t) (point))))))
           nil
           (if global 'file 'tree))))
      has-active-todos))

  ;; 4. Return DWIM
  (defun my/org-return-dwim ()
    (interactive)
    (cond
     ((org-at-item-checkbox-p) (org-toggle-checkbox))
     ((and org-return-follows-link (org-in-regexp org-link-any-re)) (org-open-at-point))
     ((org-at-table-p) (org-table-next-row))
     ((org-at-heading-p)
      (let ((state org-cycle-subtree-status))
        (cond
         ((eq state 'children)
          (if (my/org-expand-todos-smart)
              (progn
                (setq org-cycle-subtree-status 'smart)
                (message "TODOs Expanded (DONEs folded)"))
            (setq org-cycle-subtree-status 'subtree)
            (message "DONEs Expanded (No active TODOs)")))
         ((eq state 'smart)
          (org-show-subtree)
          (my/org-hide-drawers-subtree)
          (setq org-cycle-subtree-status 'subtree)
          (message "Subtree (drawers hidden)"))
         ((eq state 'subtree)
          (org-cycle)
          (message "Folded"))
         (t
          (org-cycle)
          (my/org-hide-drawers-subtree)))))
     (t
      (if (fboundp 'evil-next-line) (evil-next-line) (next-line)))))

  ;; 5. Cycle DWIM (TAB)
  (defun my/org-cycle-dwim ()
    (interactive)
    (if (org-at-heading-p)
        (let ((state org-cycle-subtree-status))
          (cond
           ((eq state 'children)
            (if (my/org-expand-todos-smart)
                (progn
                  (setq org-cycle-subtree-status 'smart)
                  (message "TODOs Expanded (DONEs folded)"))
              (setq org-cycle-subtree-status 'subtree)
              (message "DONEs Expanded (No active TODOs)")))
           ((eq state 'smart)
            (org-show-subtree)
            (my/org-hide-drawers-subtree)
            (setq org-cycle-subtree-status 'subtree)
            (message "SUBTREE (drawers hidden)"))
           ((and (eq state 'subtree) (my/org-drawers-hidden-p))
            (org-show-subtree)
            (my/org-show-drawers-subtree)
            (message "SUBTREE (drawers shown)"))
           (t
            (org-cycle)
            (unless (eq org-cycle-subtree-status 'children)
              (my/org-hide-drawers-subtree)))))
      (org-cycle)))

  ;; 6. Global Cycle DWIM (S-TAB)
  (defun my/org-shifttab-dwim ()
    (interactive)
    (cond
     ((eq org-cycle-global-status 'contents)
      (my/org-expand-todos-smart t)
      (setq org-cycle-global-status 'smart)
      (message "TODOs Expanded (DONEs folded)"))
     ((eq org-cycle-global-status 'smart)
      (org-show-all)
      (my/org-hide-drawers-global)
      (setq org-cycle-global-status 'all)
      (message "SHOW ALL (drawers hidden)"))
     ((and (eq org-cycle-global-status 'all)
           (my/org-drawers-hidden-global-p))
      (org-show-all)
      (my/org-show-drawers-global)
      (message "SHOW ALL (drawers shown)"))
     (t
      (org-global-cycle)
      (let ((state org-cycle-global-status))
        (cond
         ((eq state 'all)
          (my/org-hide-drawers-global)
          (message "SHOW ALL (drawers hidden)"))
         (t nil))))))

  ;; 7. Initialize file
  (add-hook 'org-mode-hook #'my/org-hide-drawers-global)

  ;; 8. Auto-Restore Folds on Undo
  (defun my/org-restore-fold-on-undo (&rest _)
    "Re-apply smart folding to the current heading after an undo (like 'u' in evil)."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading t)
          (let ((state (org-get-todo-state)))
            (cond
             ;; If DONE/CANCELED -> Fully fold the entire subtree
             ((member state org-done-keywords)
              (if (fboundp 'org-fold-subtree)
                  (org-fold-subtree t)
                (outline-hide-subtree)))

             ;; If active TODO -> Fold children, show deadline/entry, vanish drawers
             ((member state org-not-done-keywords)
              (if (fboundp 'org-fold-subtree)
                  (org-fold-subtree t)
                (outline-hide-subtree))
              (org-show-entry)
              (my/org-hide-drawers-subtree))))))))

  ;; Catch all common undo commands to trigger our restore logic
  (advice-add 'undo :after #'my/org-restore-fold-on-undo)
  (with-eval-after-load 'evil
    (advice-add 'evil-undo :after #'my/org-restore-fold-on-undo)
    (when (fboundp 'undo-tree-undo)
      (advice-add 'undo-tree-undo :after #'my/org-restore-fold-on-undo))))

(add-hook 'after-save-hook
          (lambda ()
            (when (and (derived-mode-p 'org-mode)
                       (executable-find "syncthingctl"))
              (shell-command "syncthingctl rescan-all"))))

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode)
		 (org-mode . abbrev-mode))
  :custom
  (org-edit-src-content-indentation 2) ;; Set src block automatic indent to 4 instead of 2.
  (org-return-follows-link t)   ;; Sets RETURN key in org-mode to follow links

  ;; FOLDING
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-cycle-separator-lines 2)

  ;; LISTS
  (org-list-allow-alphabetical t)
  (org-list-indent-offset 2)
  (org-adapt-indentation nil)

  ;; TABLES
  (org-table-convert-region-max-lines 10000)
  (org-table-copy-increment t)
  (org-table-export-default-format "orgtbl-to-csv")

  ;; STRUCTURE / FLOW
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-M-RET-may-split-line '((default . t)))

  ;; FORMAT SETTINGS FOR SRC BLOCKS
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-adapt-indentation nil)

  ;; TEXT EXPANSION
  ;; (ref:text-expansion)
  (define-abbrev-table 'my-org-abbrev-table '(
		  		  							  ("td" "TODO")
		  		  							  ("assg" "ASSIGNMENT")
		  		  							  ("bll" "BILL")
		  		  							  ("chr" "CHORE")
		  		  							  ("nxt" "NEXT")
		  		  							  ("pln" "PLANNING")
		  		  							  ("rvw" "REVIEW")
		  		  							  ("hld" "HOLD")
		  		  							  ("rdy" "READY")
		  		  							  ("actv" "ACTIVE")
		  		  							  ("mtng" "MEETING")
		  		  							  ("dn" "DONE")
		  		  							  ("cncld" "CANCELED")
		  		  							  ("chk" "- [ ]")
		  		  							  ("chkb" "[ ]")
		  		  							  ("chkc" "[0/0]")
		  		  							  ))
  (setq-default abbrev-table 'my-org-abbrev-table)


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
  (org-agenda-files (list start/org-tasks-file start/org-projects-file))
  (org-refile-targets
   '(("Archive.org" :maxlevel . 1)
	 ("Tasks.org" :maxlevel . 1)
	 ("Projects.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; TODO KEYWORDS
  ;; (ref:todo-keywords)
  (org-todo-keywords
   '((sequence "TODO(t)" "ASSIGNMENT(a)" "BILL(b)" "CHORE(c)" "MEETING(m)" "NEXT(n)" "PLANNING(P)" "REVIEW(V)" "HOLD(H)" "READY(R)" "ACTIVE(A)" "|" "DONE(d!)" "CANCELED(C!)")))
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
	 ("BILL"  . (:foreground "#282c34" :background "#fc830a" :weight bold))
	 ("CHORE"  . (:foreground "#282c34" :background "#e2b93d" :weight bold))
	 ("MEETING"  . (:foreground "#282c34" :background "#f52dfc" :weight bold))
	 ("DONE"     . (:foreground "#1f2328" :background "#304b60" :weight bold))
	 ("CANCELED" . (:foreground "#1f2328" :background "#e06c75" :weight bold))))

  ;; AGENDA OVERVIEW
  ;; (ref:agenda-overview)
  (org-agenda-custom-commands
   '(("d" "📅 Daily overview"
	  ((todo "NEXT"
		  	 ((org-agenda-overriding-header "🚀 NEXT TASKS")
		  	  (org-agenda-prefix-format "  %-20b %s")
		  	  (org-super-agenda-groups
		  	   '((:name "High priority"
		  		  		:priority "A")
		  		 (:name "Normal"
		  		  		:anything t)))))
	   (agenda ""
		  	   ((org-agenda-span 1)
		  		(org-agenda-start-day "0d")
		  		(org-deadline-warning-days 0)  ; Key fix: don't show future deadlines
		  		(org-agenda-overriding-header "🔥 TODAY")
		  		(org-agenda-prefix-format "  %-20b %s")
		  		(org-super-agenda-groups
		  		 '((:name "❗Overdue"
		  		          :deadline past
		  		          :scheduled past
		  		          :order 1)
		  		   (:name "⏰ Today"
		  		          :time-grid t
		  		          :scheduled today
		  		          :deadline today
		  		          :order 2)
		  		   (:discard (:anything t))))))  ; This should hide everything else
	   (agenda ""
		  	   ((org-agenda-span 7)
		  		(org-agenda-start-day "+1d")
		  		(org-agenda-start-on-weekday nil)
		  		(org-agenda-time-grid nil)
		  		(org-agenda-overriding-header "📅 UPCOMING (NEXT 7 DAYS)")
		  		(org-agenda-prefix-format "  %-20b %s")
		  		(org-super-agenda-groups nil)))
	   (todo "TODO"
		  	 ((org-agenda-overriding-header "📦 TODO BACKLOG")
		  	  (org-agenda-todo-ignore-scheduled 'all)
		  	  (org-agenda-todo-ignore-deadlines 'all)
		  	  (org-agenda-prefix-format "  %-20b %s")
		  	  (org-super-agenda-groups
		  	   '((:anything t)))))))))
  :config
  ;; AUTO-SAVE ORG MODE BUFFERS
  ;; (ref:auto-save)
  (defun my/org-save-all-except ()
	"Save all Org buffers except for files matching specific strings."
	(interactive)
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
		(when (and (derived-mode-p 'org-mode)
		  		   (buffer-file-name)
		  		   (buffer-modified-p)
		  		   ;; Exempt buffer names
		  		   (not (string-match-p "init\\.org" (buffer-file-name)))
		  		   )
		  (save-buffer)))))
  (run-with-idle-timer 30 t #'my/org-save-all-except)

  ;; AUTO-FORMAT SRC BLOCKS
  ;; (ref:format-src)
  ;; Function to indent every source block in the file
  (defun my/org-indent-all-src-blocks ()
	(when (derived-mode-p 'org-mode)
	  (save-excursion
		(goto-char (point-min))
		(while (re-search-forward org-babel-src-block-regexp nil t)
		  (let ((element (org-element-at-point)))
		  	(when (eq (org-element-type element) 'src-block)
		  	  (org-babel-do-in-edit-buffer (indent-region (point-min) (point-max)))))))))
  (add-hook 'before-save-hook #'my/org-indent-all-src-blocks)

  ;; FIX STRANGE REFILE DISPLAY ISSUES
  ;; (ref:refile-fix)
  ;; This will refile to a new heading, if it was already folded
  ;; it will remain so, otherwise will remain unfolded. Fixes
  ;; weird display issues after refiling as well.
  (add-hook 'org-after-refile-insert-hook
		  	(lambda ()
		  	  (save-excursion
		  		(org-back-to-heading t)
		  		;; Move to the parent heading to check its state
		  		(when (org-up-heading-safe)
		  		  ;; Check if the parent heading is currently folded
		  		  (let ((folded (save-excursion
		  		                  (end-of-line)
		  		                  (invisible-p (point)))))
		  		  	(when folded
		  		      ;; If it was folded, re-hide the subtree we just moved
		  		      ;; This fixes the 'ghost' display issue
		  		      (org-back-to-heading t)
		  		      (org-flag-subtree t)))))))

  ;; CYCLE FOLDING OF TODOS AND DONE
  ;; (ref:todo-cycle)
  (defun my/org-toggle-todo-entries ()
	"Cycle visibility for all active TODO entries (non-DONE states)."
	(interactive)
	(org-map-entries
	 (lambda ()
	   (let ((is-todo (member (org-get-todo-state) org-not-done-keywords)))
		 (when is-todo
		   (if (outline-invisible-p (line-end-position))
		  	   (outline-show-subtree)
		  	 (outline-hide-subtree)))))
	 nil 'file)
	(message "Toggled TODO entries"))
  (defun my/org-toggle-done-entries ()
	"Cycle visibility for all DONE entries without hanging."
	(interactive)
	(org-map-entries
	 (lambda ()
	   (let ((is-done (member (org-get-todo-state) org-done-keywords)))
		 (when is-done
		   (if (outline-invisible-p (line-end-position))
		  	   (outline-show-subtree)
		  	 (outline-hide-subtree)))))
	 nil 'file)
	(message "Toggled DONE entries"))
  (defun my/org-cycle-all-todo-done-entries ()
	"Cycle visibility for all entries with TODO states (both TODO and DONE).
		  		                                 If any are visible, hide all. If all are hidden, show all."
	(interactive)
	(let ((any-visible nil))
	  ;; First pass: check if any TODO-state entries are visible
	  (org-map-entries
	   (lambda ()
		 (let ((has-todo-state (org-get-todo-state)))
		   (when (and has-todo-state (not (outline-invisible-p (line-end-position))))
		  	 (setq any-visible t))))
	   nil 'file)
	  ;; Second pass: apply consistent action to all TODO-state entries
	  (org-map-entries
	   (lambda ()
		 (let ((has-todo-state (org-get-todo-state)))
		   (when has-todo-state
		  	 (if any-visible
		  		 (outline-hide-subtree)
		  	   (outline-show-subtree)))))
	   nil 'file)
	  (message (if any-visible "Hidden all TODO-state entries" "Shown all TODO-state entries"))))
  (defun my/org-cycle-todo-entries ()
	"Cycle visibility for all TODO entries (non-DONE states).
		  		      If any are visible, hide all. If all are hidden, show all."
	(interactive)
	(let ((any-visible nil))
	  ;; First pass: check if any TODO entries are visible
	  (org-map-entries
	   (lambda ()
		 (let ((is-todo (member (org-get-todo-state) org-not-done-keywords)))
		   (when (and is-todo (not (outline-invisible-p (line-end-position))))
		  	 (setq any-visible t))))
	   nil 'file)
	  ;; Second pass: apply consistent action to all TODO entries
	  (org-map-entries
	   (lambda ()
		 (let ((is-todo (member (org-get-todo-state) org-not-done-keywords)))
		   (when is-todo
		  	 (if any-visible
		  		 (outline-hide-subtree)
		  	   (outline-show-subtree)))))
	   nil 'file)
	  (message (if any-visible "Hidden TODO entries" "Shown TODO entries"))))
  (defun my/org-cycle-done-entries ()
	"Cycle visibility for all DONE entries.
		  		                                 If any are visible, hide all. If all are hidden, show all."
	(interactive)
	(let ((any-visible nil))
	  ;; First pass: check if any DONE entries are visible
	  (org-map-entries
	   (lambda ()
		 (let ((is-done (member (org-get-todo-state) org-done-keywords)))
		   (when (and is-done (not (outline-invisible-p (line-end-position))))
		  	 (setq any-visible t))))
	   nil 'file)
	  ;; Second pass: apply consistent action to all DONE entries
	  (org-map-entries
	   (lambda ()
		 (let ((is-done (member (org-get-todo-state) org-done-keywords)))
		   (when is-done
		  	 (if any-visible
		  		 (outline-hide-subtree)
		  	   (outline-show-subtree)))))
	   nil 'file)
	  (message (if any-visible "Hidden DONE entries" "Shown DONE entries"))))


  ;; COLLAPSE DONE ENTRIES ON LAUNCH
  ;; (ref:launch-done-collapse)
  (defun my/org-hide-done-entries-dynamic ()
	"Hide all entries that are in any 'DONE' state defined in the current buffer."
	(interactive)
	(when (derived-mode-p 'org-mode)
	  (org-map-entries
	   (lambda ()
		 (let ((state (org-get-todo-state)))
		   (when (member state org-done-keywords)
		  	 (outline-hide-subtree))))
	   t 'file)))

  ;; COLLAPSE DONE ENTRIES WHEN MARKED
  ;; (ref:mark-done-collapse)
  (defun my/org-collapse-on-done ()
	"Collapse the current heading if moved to a DONE state, with a tiny delay to catch the Logbook."
	(let ((state org-state))
	  (when (member state org-done-keywords)
		;; We use a 0-second timer to run this AFTER Org finishes writing the logbook
		(run-at-time 0 nil
		  		     (lambda (buf)
		  		       (with-current-buffer buf
		  		         (save-excursion
		  		           (org-back-to-heading t)
		  		           ;; Hide the drawer first, then the subtree
		  		           (org-cycle-hide-drawers 'all)
		  		           (outline-hide-subtree))))
		  		     (current-buffer)))))
  ;; Launch: Auto-collapse when opening a file
  (add-hook 'find-file-hook #'my/org-hide-done-entries-dynamic)
  ;; Interaction: Auto-collapse the moment you mark it DONE
  (add-hook 'org-after-todo-state-change-hook #'my/org-collapse-on-done)

  ;; AUTO COLLAPSE KEYWORD HEADINGS
  ;; (ref:collapse-keyword-headings)
  ;; Alist of heading names and optional level restrictions
  (defvar my/org-collapse-headings
	'(("#Future-Bills" .  1)   ; Only level 1
	  ("#Coffee-Roasting" . 1)
	  ("#Archive" . nil)         ; Any level
	  ("#Repeaters" . nil)          ; Any level
	  )
	"Alist of (heading-name . level).
		  		          If level is nil, collapse at any level.
		  		          If level is a number, only collapse at that level.")

  (defun my/org-hide-matching-headings ()
	"Force hide all headings that match entries in `my/org-collapse-headings'.
		  		          Respects level restrictions if specified."
	(org-map-entries
	 (lambda ()
	   (let ((heading (org-get-heading t t t t))  ; Get heading without tags, todo, etc.
		  	 (current-level (org-current-level)))
		 (dolist (entry my/org-collapse-headings)
		   (let ((target-heading (car entry))
		  		 (target-level (cdr entry)))
		  	 (when (and (string= heading target-heading)
		  		        (or (null target-level)  ; No level restriction
		  		            (= current-level target-level)))  ; Matches specific level
		  	   (outline-hide-subtree))))))
	 nil 'file))
  (add-hook 'find-file-hook
		  	(lambda ()
		  	  (when (derived-mode-p 'org-mode)
		  		(my/org-hide-matching-headings))))


  )

;; (ref:ret-map)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") #'org-return))

;; LOAD AGENDA ON LAUNCH
;; (ref:agenda-on-launch)
(with-eval-after-load 'org
  (add-hook 'emacs-startup-hook
		  	(lambda ()
		  	  (run-at-time "0.5 sec" nil
		  		  		   (lambda ()
		  		             (org-agenda nil "d"))))))

(use-package evil-org
  :ensure t
  :after org
  ;;:hook (org-mode . evil-org-mode)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))

  ;; Org element motions (Doom-style)
  (evil-define-key 'normal org-mode-map
    (kbd "gj") #'org-forward-element
    (kbd "gk") #'org-backward-element)
  (evil-define-key 'normal org-mode-map
    (kbd "]]") #'org-next-visible-heading
    (kbd "[[") #'org-previous-visible-heading
    (kbd "]h") #'org-forward-heading-same-level
    (kbd "[h") #'org-backward-heading-same-level)

  ;; Smart Meta Movements (Normal & Insert states combined)
  (evil-define-key '(normal insert) org-mode-map
    (kbd "M-h") #'my/org-meta-left-smart
    (kbd "M-j") #'my/org-meta-down-smart
    (kbd "M-k") #'my/org-meta-up-smart
    (kbd "M-l") #'my/org-meta-right-smart)

  ;; Explicitly bind TAB to org-cycle so our advice is triggered
  (evil-define-key '(normal insert) org-mode-map (kbd "TAB") #'my/org-cycle-dwim)
  (evil-define-key '(normal insert) org-mode-map (kbd "<tab>") #'my/org-cycle-dwim)
  (evil-define-key '(normal insert) org-mode-map (kbd "S-TAB") #'my/org-shifttab-dwim)
  (evil-define-key '(normal insert) org-mode-map (kbd "<backtab>") #'my/org-shifttab-dwim)
  )

(with-eval-after-load 'evil-org
  ;; DWIM RETURN KEY
  (evil-define-key 'normal org-mode-map (kbd "RET") #'my/org-return-dwim)

  ;; INSERT PARENT HEADING
  ;; Add Alt-Ret (Meta-Return) mappings (ref:parent-heading-map)
  (evil-define-key 'normal org-mode-map (kbd "M-RET") #'my/org-insert-parent-heading-below)
  (evil-define-key 'insert org-mode-map (kbd "M-RET") #'my/org-insert-parent-heading-below)
  ;; Some terminals/launchers treat M-RET as M-J, so adding this as a fallback:
  (evil-define-key 'normal org-mode-map (kbd "M-<return>") #'my/org-insert-parent-heading-below)
  (evil-define-key 'insert org-mode-map (kbd "M-<return>") #'my/org-insert-parent-heading-below)

  ;; SMART INSERT BELOW (ref:item-below-map)
  (evil-define-key 'normal org-mode-map (kbd "C-<return>") #'my/org-smart-insert-item-below)
  (evil-define-key 'normal org-mode-map (kbd "C-RET")      #'my/org-smart-insert-item-below)
  (evil-define-key 'normal org-mode-map (kbd "C-M-j")      #'my/org-smart-insert-item-below)
  (evil-define-key 'insert org-mode-map (kbd "C-<return>") #'my/org-smart-insert-item-below)
  (evil-define-key 'insert org-mode-map (kbd "C-RET")      #'my/org-smart-insert-item-below)
  (evil-define-key 'insert org-mode-map (kbd "C-M-j")      #'my/org-smart-insert-item-below)

  ;; SMART INSERT SUBITEM (ref:subitem-below-map)
  (evil-define-key 'normal org-mode-map (kbd "C-S-<return>") #'my/org-smart-insert-subitem)
  (evil-define-key 'normal org-mode-map (kbd "C-S-RET") #'my/org-smart-insert-subitem)
  (evil-define-key 'insert org-mode-map (kbd "C-S-<return>") #'my/org-smart-insert-subitem)
  (evil-define-key 'insert org-mode-map (kbd "C-S-RET") #'my/org-smart-insert-subitem)

  ;; DOUBLE CLICK TO CYCLE HEADINGS
  (evil-define-key 'normal org-mode-map
    (kbd "<double-mouse-1>")
    (lambda (event)
      (interactive "e")
      (mouse-set-point event) ; Move the cursor to where you double-clicked
      (my/org-return-dwim)))
  )

;; DELETE CONTENTS OF FOLDED HEADINGS WITH DELETE LINE
(with-eval-after-load 'evil-org
  ;; Use 'evil-define-minor-mode-key' or standard 'evil-define-key'
  ;; but avoid shadowing the base 'd' operator.

  ;; 1. Remap the 'evil-delete-line' (which is what dd usually calls)
  ;; This is the "cleanest" way to intercept 'dd' without breaking 'dw'
  (evil-define-key 'normal org-mode-map (kbd "dd") #'my/evil-org-delete-heading-dwim)

  ;; 2. Ensure 'd' is still recognized as the operator for everything else
  ;; If 'dw' still fails, add this to force the operator to stay active:
  (evil-define-key 'normal org-mode-map (kbd "d") 'evil-delete))

;; Fix Evil bindings in org-agenda (especially with org-super-agenda)
(with-eval-after-load 'org-agenda
  (require 'evil-collection-org-agenda nil t)

  ;; Force motion state
  (evil-set-initial-state 'org-agenda-mode 'motion)

  ;; Remove all org-agenda's default bindings for j/k
  (define-key org-agenda-mode-map (kbd "j") nil)
  (define-key org-agenda-mode-map (kbd "k") nil)

  ;; Now set Evil bindings
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char
    (kbd "gg") 'evil-goto-first-line
    (kbd "G") 'evil-goto-line
    (kbd "0") 'evil-beginning-of-line
    (kbd "$") 'evil-end-of-line
    (kbd "H") 'start/jump-to-line-start
    (kbd "L") 'evil-end-of-line
    (kbd "^") 'evil-first-non-blank
    (kbd "s") 'flash-emacs-jump

    ;; Quick actions
    (kbd "q") 'org-agenda-quit
    (kbd "<escape>") 'org-agenda-quit
    (kbd "RET") 'org-agenda-switch-to
    (kbd "TAB") 'org-agenda-goto
    (kbd "J") 'org-agenda-goto-date
    ))
;; Set up leader key bindings for org-agenda
(with-eval-after-load 'org-agenda
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "SPC") nil)  ; Clear SPC first if needed

  ;; Define clean which-key labels with emojis
  (which-key-add-key-based-replacements
    "SPC a" "agenda"
    "SPC aC" "clock"
    "SPC av" "view"
    "SPC af" "filter")

  (evil-define-key 'motion org-agenda-mode-map
    ;; TODO & Tags
    (kbd "SPC at") '("Change Todo" . org-agenda-todo)
    (kbd "SPC aT") '("Set Tags" . org-agenda-set-tags)
    (kbd "SPC ap") '("Set Priority" . org-agenda-priority)

    ;; Schedule & Deadline
    (kbd "SPC as") '("Schedule" . org-agenda-schedule)
    (kbd "SPC ad") '("Deadline" . org-agenda-deadline)
    (kbd "SPC aj") '("Jump to Date" . org-agenda-goto-date)
    (kbd "SPC a.") '("Goto Today" . org-agenda-goto-today)

    ;; Archive & Refile
    (kbd "SPC aa") '("Archive" . org-agenda-archive)
    (kbd "SPC aA") '("Archive Default" . org-agenda-archive-default)
    (kbd "SPC ar") '("Refile" . org-agenda-refile)
    (kbd "SPC ak") '("Kill/Delete" . org-agenda-kill)

    ;; Clock
    (kbd "SPC aCi") '("Clock In" . org-agenda-clock-in)
    (kbd "SPC aCo") '("Clock Out" . org-agenda-clock-out)
    (kbd "SPC aCc") '("Cancel Clock" . org-agenda-clock-cancel)
    (kbd "SPC aCg") '("Goto Clocked" . org-agenda-clock-goto)

    ;; View & Display
    (kbd "SPC avd") '("Day View" . org-agenda-day-view)
    (kbd "SPC avw") '("Week View" . org-agenda-week-view)
    (kbd "SPC avm") '("Month View" . org-agenda-month-view)
    (kbd "SPC avy") '("Year View" . org-agenda-year-view)
    (kbd "SPC avv") '("View Dispatch" . org-agenda-view-mode-dispatch)
    (kbd "SPC avl") '("Toggle Log" . org-agenda-log-mode)
    (kbd "SPC avf") '("Toggle Follow" . org-agenda-follow-mode)
    (kbd "SPC avg") '("Toggle Time grid" . org-agenda-toggle-time-grid)
    (kbd "SPC avD") '("Toggle Diary" . org-agenda-toggle-diary)

    ;; Filter
    (kbd "SPC aff") '("Filter" . org-agenda-filter)
    (kbd "SPC afc") '("By Category" . org-agenda-filter-by-category)
    (kbd "SPC aft") '("By Tag" . org-agenda-filter-by-tag)
    (kbd "SPC afr") '("By Regexp" . org-agenda-filter-by-regexp)
    (kbd "SPC afx") '("Clear Filters" . org-agenda-filter-remove-all)

    ;; Refresh & Misc
    (kbd "SPC aR") '("Refresh" . org-agenda-redo)
    (kbd "SPC au") '("Undo" . org-agenda-undo)
    (kbd "SPC an") '("Add Note" . org-agenda-add-note)
    (kbd "SPC ae") '("Set Effort" . org-agenda-set-effort)
    (kbd "SPC ag") '("Goto Calendar" . org-agenda-goto-calendar)
    (kbd "SPC aq") '("Quit" . org-agenda-quit)
    (kbd "SPC ax") '("Exit" . org-agenda-exit)))

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
     ("ASSIGNMENT"     . (:foreground "#282c34" :background "#e5404e" :weight bold))
     ("BILL"  . (:foreground "#282c34" :background "#fc830a" :weight bold))
     ("CHORE"  . (:foreground "#282c34" :background "#e2b93d" :weight bold))
     ("PLANNING" . (:foreground "#282c34" :background "#c792ea" :weight bold))
     ("READY"    . (:foreground "#282c34" :background "#82b7ff" :weight bold))
     ("ACTIVE"   . (:foreground "#282c34" :background "#7fdc6f" :weight bold))
     ("REVIEW"   . (:foreground "#282c34" :background "#e0a96d" :weight bold))
     ("HOLD"     . (:foreground "#282c34" :background "#e6d96c" :weight bold))
     ("MEETING"  . (:foreground "#282c34" :background "#f52dfc" :weight bold))
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

(use-package org-caldav
  :ensure t
  :config
  (setq org-caldav-url "https://cal.catphish.org"
        ;; Use the specific calendar path from Radicale
        org-caldav-calendar-id "jordan/5862c1ad-977a-1c79-4053-af22365427d0/"
        ;; New events from your phone land here
        org-caldav-inbox start/org-inbox-file
        ;; Source files to push to the server
        org-caldav-files (list start/org-tasks-file start/org-inbox-file)
        ;; Keep metadata out of your main git repo
        org-caldav-save-directory (expand-file-name "org-caldav-cache/" start/org-root)
        ;; Sync on a regular basis (optional)
        org-caldav-sync-direction 'twoway
        ;; Map TODO keywords to percentage states
        org-caldav-todo-percent-states '((0 "TODO" "ASSIGNMENT" "BILL" "CHORE" "MEETING" "NEXT" "PLANNING" "REVIEW" "HOLD" "READY" "ACTIVE")
                                         (100 "DONE" "CANCELED"))
        ;; Don't pop up a buffer showing results (silent sync)
        org-caldav-show-sync-results nil)

  ;; Setup automatic sync (runs every 1 hour when idle)
  ;; Note: This will momentarily freeze Emacs while syncing
										; (run-with-idle-timer 3600 t 'org-caldav-sync)

  ;; This ensures Emacs uses your GPG key to read the password
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq org-cycle-hide-drawers t)
  )

(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename start/org-roam-dir))
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

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  )
(with-eval-after-load 'org-roam-ui
  )

;; (use-package org-download
;;   :after org
;;   :hook ((dired-mode . org-download-enable)
;;     	 (org-mode . org-download-enable))
;;   :config
;;   (setq org-download-method 'directory)             ;; Save images to a directory
;;   (setq org-download-image-dir "./images")            ;; The directory name (e.g. ./images)
;;   (setq org-download-heading-lvl nil)               ;; Don't use headings for sub-folders
;;   (setq org-download-timestamp "%Y%m%d-%H%M%S_")    ;; Timestamp file names
;;   (add-hook 'dired-mode-hook 'org-download-enable)

;;   ;; (setq org-download-screenshot-method "xclip")     ;; "scrot", "gnome-screenshot", or "xclip" (Linux)
;;   ;; (setq org-download-screenshot-method "wl-paste -t image/png > %s")
;;   ;; On Mac, it uses "pngpaste" automatically if installed
;;   (setq org-download-screenshot-method
;; 		(if (string= (getenv "XDG_SESSION_TYPE") "wayland")
;; 			"wl-paste -t image/png > %s"
;;           "xclip -selection clipboard -t image/png -o > %s"))
;;   )
(use-package org-download
  :after org
  :hook ((dired-mode . org-download-enable)
    	 (org-mode . org-download-enable))
  :config
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")

  ;; Dynamic Clipboard Detection
  (setq org-download-screenshot-method
        (cond
         ((executable-find "dms") "dms cl paste > %s")
         ((and (string= (getenv "XDG_SESSION_TYPE") "wayland")
               (executable-find "wl-paste")) "wl-paste -t image/png > %s")
         (t "xclip -selection clipboard -t image/png -o > %s")))

  ;; Custom Directory Logic
  (defun my/org-download-method (link)
    (let* ((buffer-path (buffer-file-name))
           (buffer-name (if buffer-path (file-name-base buffer-path) "unsaved"))
           (header (or (org-get-heading t t t t) "general"))
           (clean-header (replace-regexp-in-string "[^A-Za-z0-9]" "_" header))
           (folder (concat "./images/" buffer-name "/" clean-header "/")))
      (unless (file-exists-p folder) (make-directory folder t))
      (concat folder (format-time-string org-download-timestamp) "screenshot.png")))

  (setq org-download-method 'my/org-download-method))

(use-package projectile
  :if start/enable-dev-features
  :config
  (projectile-mode)
  :custom
  ;; (projectile-auto-discover nil) ;; Disable auto search for better startup times ;; Search with a keybind
  (projectile-run-use-comint-mode t) ;; Interactive run dialog when running projects inside emacs (like giving input)
  (projectile-switch-project-action #'projectile-dired) ;; Open dired when switching to a project
  (projectile-project-search-path '("~/projects/" "~/source/" ("~/github" . 1)))) ;; . 1 means only search the first subdirectory level for projects

(use-package eglot
  :if start/enable-dev-features
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
  :if start/enable-dev-features
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'line) ;; Show errors on the current line
  (sideline-backends-right '(sideline-flymake)))

(use-package yasnippet-snippets
  :if start/enable-dev-features
  :hook (prog-mode . yas-minor-mode))

(when start/enable-dev-features
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
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
    (use-package tsx-ts-mode :ensure nil :mode "\\.tsx\\'")))

(use-package eat
  :if start/enable-dev-features
  :hook ('eshell-load-hook #'eat-eshell-mode))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
  :custom
  (corfu-cycle t)                ;; Enable cycling for single candidates
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preselect-first nil)
  :init
  (global-corfu-mode)
  :config
  ;; Disable RET for accepting completions (use normal newline instead)
  (define-key corfu-map (kbd "RET") nil)

  ;; Disable TAB for selecting completions
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil)

  ;; Disable arrow keys by binding them to normal line movement
  (keymap-set corfu-map "<down>" #'next-line)
  (keymap-set corfu-map "<up>" #'previous-line)
  (keymap-set corfu-map "M-n" #'ignore)
  (keymap-set corfu-map "M-p" #'ignore)
  sh-test
  ;; C-n/C-p for Corfu navigation
  (keymap-set corfu-map "C-n" #'corfu-next)
  (keymap-set corfu-map "C-p" #'corfu-previous)
  ;; Use TAB for cycling, default is C-n/C-p
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous)
  (define-key corfu-map (kbd "TAB") 'corfu-next)

  ;; Add a hook to show completion documentation in a popup
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
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

(require 'flash-emacs)
(setq flash-emacs-labels "asdghklqwertyuiopzxcvbnmfjASDGHKLQWERTYUIOPZXCVBNMFJ")
(evil-define-key 'normal 'global (kbd "s") 'flash-emacs-jump)
(with-eval-after-load 'flash-emacs
  (set-face-attribute 'flash-emacs-match nil
                      :background "#1dacd6"
                      :foreground "black")
  (set-face-attribute 'flash-emacs-label nil
                      :background "red"
                      :foreground "white"))

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
  (setq grease-show-hidden t)          ; Set to t to show dotfiles by default

  ;; Preview window
  (setq grease-preview-window-width 0.4) ; Preview takes 40% of frame width
  (setq grease-preview-writable nil)     ; Set to t to make file previews editable
  )

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

(use-package undo-fu-session
  :ensure t
  :custom
  (undo-fu-session-ignore-encrypted-files t)
  :config
  ;; Set the path to ~/. local/state/emacs/undo-fu-session/
  (setq undo-fu-session-directory (expand-file-name "~/.local/state/emacs/undo-fu-session/"))

  ;; Create the directory if it's missing
  (unless (file-exists-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))

  (global-undo-fu-session-mode))

;;(use-package command-log-mode)

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
  (which-key-idle-delay 0.4)       ;; Set the time delay (in seconds) for the which-key popup to appear
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)) ;; Fixes which-key window slipping out in Emacs Daemon

(use-package ws-butler
  :init (ws-butler-global-mode))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
