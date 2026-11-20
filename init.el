;;; init.el --- Personal Emacs configuration -*- lexical-binding: nil; -*-

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
  (display-line-numbers-width-start t) ;; Size the gutter for the largest line number, so switching relative/absolute doesn't shift text
  (global-display-line-numbers-mode t)  ;; Display line numbers

  (mouse-wheel-progressive-speed nil) ;; Disable progressive speed when scrolling
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

(defconst start/org-shopping-file
  (expand-file-name "Shopping.org" start/org-main-dir)
  "Shopping lists (Grocery, Bar, ...), also edited by Orgzly and Home Assistant.")

(defconst start/org-inbox-heading "Inbox"
  "Top-level heading in `start/org-tasks-file' that serves as the inbox.")

(defconst start/org-school-heading "School"
  "Top-level heading in `start/org-tasks-file' whose children are classes.")

(defconst start/org-sysmgmt-heading "System"
  "Top-level heading in `start/org-tasks-file' whose children are systems/devices.")

(defvar-local start/scroll--line-numbers-orig nil)

(defun start/scroll-absolute-line-numbers (arg)
  "`ultra-scroll-hide-functions' entry: absolute line numbers while scrolling.
Relative numbers are renumbered on every scroll step; absolute ones are cheaper.
ARG is -1 when a scroll starts pushing point and 1 once scrolling stops."
  (cond ((and (< arg 0) (memq display-line-numbers '(relative visual)))
         (setq start/scroll--line-numbers-orig display-line-numbers)
         (setq-local display-line-numbers t))
        ((and (> arg 0) start/scroll--line-numbers-orig)
         (setq-local display-line-numbers start/scroll--line-numbers-orig)
         (setq start/scroll--line-numbers-orig nil))))

(use-package ultra-scroll
  :if (not start/is-windows)
  :config
  ;; Added after load so the package's default entries (hl-line-mode) stay.
  (add-hook 'ultra-scroll-hide-functions #'start/scroll-absolute-line-numbers)
  (ultra-scroll-mode 1))

(when start/is-windows
  (pixel-scroll-precision-mode 1))

;; While scroll events are queued, don't fontify text that's about to scroll past.
(setq redisplay-skip-fontification-on-input t)

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

  ;; ;; Jump to beginning of line or first non-blank character
  (defun start/jump-to-line-start ()
    "If at first non-blank char, go to beginning; else go to first non-blank.
On an Org heading, never land on the (hidden) stars: go to the heading text,
and from there to the title after any TODO keyword and priority."
    (interactive)
    (if (and (derived-mode-p 'org-mode) (org-at-heading-p))
        (let* ((text-start (save-excursion
                             (beginning-of-line)
                             (skip-chars-forward "* \t")
                             (point)))
               (title-start (save-excursion
                              (goto-char text-start)
                              (let ((case-fold-search nil))
                                (when (and org-todo-regexp (looking-at org-todo-regexp))
                                  (goto-char (match-end 0))
                                  (skip-chars-forward " \t"))
                                (when (looking-at "\\[#.\\][ \t]*")
                                  (goto-char (match-end 0))))
                              (point))))
          (goto-char (if (= (point) text-start) title-start text-start)))
      (let ((col (current-column))
            (first-non-blank (save-excursion
                               (back-to-indentation)
                               (current-column))))
        (if (= col first-non-blank)
            (move-beginning-of-line nil)
          (back-to-indentation)))))

  ;; End of line, skipping text hidden at the end (link brackets, emphasis
  ;; markers), so the cursor sits on the last character you can see.
  (evil-define-motion start/evil-end-of-visible-line (count)
    "Like `evil-end-of-line', but stop on the last visible character."
    :type inclusive
    (evil-end-of-line count)
    (unless (evil-operator-state-p)
      (while (and (> (point) (line-beginning-position)) (invisible-p (point)))
        (backward-char))))

  (defun start/rest-of-line-invisible-p (pos)
    "Non-nil if every char from POS to the end of its line is invisible."
    (let ((eol (save-excursion (goto-char pos) (line-end-position))))
      (and (< pos eol)
           (cl-loop for i from pos below eol always (invisible-p i)))))

  (defun start/evil-append-past-hidden (orig &rest args)
    "Around `evil-append': if only hidden text follows point, append at eol.
Otherwise `a' on the last visible char lands inside the hidden text, or, if
point was already adjusted onto it, wraps to the next line."
    (if (and (not (eolp))
             (or (invisible-p (point))
                 (start/rest-of-line-invisible-p (1+ (point)))))
        (progn (end-of-line) (apply #'evil-insert args))
      (apply orig args)))
  (advice-add 'evil-append :around #'start/evil-append-past-hidden)

  ;; j/k with a count follow the visual line numbers (which skip folded text
  ;; and hidden drawers and count wrapped lines), so 10j lands on the line
  ;; labelled 10. Without a count they move by logical lines as usual.
  (evil-define-motion start/evil-next-line (count)
    "Move down COUNT lines; with a count and visual line numbers, screen lines."
    :type line
    (if (and count (eq display-line-numbers 'visual))
        (let ((line-move-visual t)) (evil-line-move count))
      (evil-next-line count)))

  (evil-define-motion start/evil-previous-line (count)
    "Move up COUNT lines; with a count and visual line numbers, screen lines."
    :type line
    (if (and count (eq display-line-numbers 'visual))
        (let ((line-move-visual t)) (evil-line-move (- count)))
      (evil-previous-line count)))

  (define-key evil-motion-state-map "j" #'start/evil-next-line)
  (define-key evil-motion-state-map "k" #'start/evil-previous-line)

  ;; H/L keybindings for motion state
  (define-key evil-motion-state-map "H" #'start/jump-to-line-start)
  (define-key evil-motion-state-map "L" #'start/evil-end-of-visible-line)

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

  ;; Custom Org-mode navigation (gh / gl / gj / gk)
  ;; Lands on heading text (skipping stars)
  (with-eval-after-load 'org
    (defun my/org-goto-heading-text (fn)
      "Execute heading navigation function FN, then place cursor at start of heading text."
      (ignore-errors (funcall fn))
      (when (org-at-heading-p)
        (beginning-of-line)
        (skip-chars-forward "* ")
        (when (looking-at org-todo-regexp)
          (goto-char (match-beginning 0)))))

    (evil-define-key 'normal org-mode-map
      (kbd "gh") (lambda () (interactive) (my/org-goto-heading-text #'org-up-element))
      (kbd "gl") (lambda () (interactive) (my/org-goto-heading-text #'org-down-element))
      (kbd "gj") (lambda () (interactive) (my/org-goto-heading-text #'org-forward-heading-same-level))
      (kbd "gk") (lambda () (interactive) (my/org-goto-heading-text #'org-backward-heading-same-level))))
  ;; Reapply global H/L after evil-collection might override
  (define-key evil-motion-state-map "H" #'start/jump-to-line-start)
  (define-key evil-motion-state-map "L" #'start/evil-end-of-visible-line)
  )

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
    "S" '(start/org-sort-by-title :wk "Sort A–Z")
    "l" '(:ignore t :wk "lists")
    "l a" '(start/org-lists-add :wk "Add items")
    "l x" '(start/org-lists-check-off :wk "Check off")
    "l o" '(start/org-lists-bar-out :wk "Bar is out of…")
    "l f" '(start/org-lists-open :wk "Open Shopping.org")
    "TAB" '(mode-line-other-buffer :wk "Previous Buffer")
    "`" '(start/org-reset-folds :wk "Reset Fold")
    "d" '(start/org-done-dwim :wk "Done (catch up repeats)")
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
    "f h" '(consult-imenu :wk "Find Header")
    "f o" '((lambda () (interactive)
              (let ((default-directory start/org-main-dir)) (call-interactively #'find-file)))
            :wk "Find file in org/main")
    "f n" '(org-roam-node-find :wk "Find roam node")
    "f t" '((lambda () (interactive) (find-file start/org-tasks-file)) :wk "Open Tasks.org")
    "f i" '(start/org-open-inbox :wk "Open Inbox"))

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
    "o v" '(start/voice-menu :wk "Voice rules")

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
    "o S" '(:ignore t :wk "sort")
    "o S S" '(start/org-sort-default :wk "Default (state→prio→sched)")
    "o S p" '(start/org-sort-by-priority :wk "Priority")
    "o S P" '(start/org-sort-by-priority-reverse :wk "Priority (rev)")
    "o S s" '(start/org-sort-by-scheduled :wk "Scheduled")
    "o S d" '(start/org-sort-by-deadline :wk "Deadline")
    "o S t" '(start/org-sort-by-state :wk "TODO state")
    "o S a" '(start/org-sort-by-title :wk "Title A–Z")
    "o S A" '(start/org-sort-by-title-reverse :wk "Title Z–A")
    "o S c" '(start/org-sort-by-created :wk "Created")
    "o S x" '(start/org-sort-custom :wk "Custom chain…")
    "o S o" '(org-sort :wk "org-sort (lists/tables)")
    "o T" '(org-set-tags-command :wk "Set Tags")
    "o r" '(org-refile :wk "Refile")
    "o R" '(start/org-refile-old-done :wk "Refile old DONE")
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
    "y C" '(my/org-yank-inside-code :wk "Yank code block to clipboard")
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
    "Enable org-roam-ui-mode and open the UI in a Firefox kiosk window.
Falls back to the default browser when firefox isn't on PATH."
    (interactive)
    (unless org-roam-ui-mode
      (org-roam-ui-mode 1))
    (let ((browse-url-browser-function
           (if (executable-find "firefox")
               (lambda (url &optional _)
                 ;; Async, so Emacs isn't blocked while Firefox is open
                 (start-process "org-roam-ui-firefox" nil
                                "firefox" "--kiosk" "--new-window" url))
             browse-url-browser-function)))
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

(with-eval-after-load 'gnus
  (face-spec-set 'gnus-group-news-low '((t :weight bold)) 'face-defface-spec))

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

(defun start/org-evil-delete-folded-subtree (args)
  "Filter-args for `evil-delete': extend a linewise delete over a folded subtree."
  (pcase-let ((`(,beg ,end ,type . ,rest) args))
    (when (and (derived-mode-p 'org-mode)
               (eq type 'line)
               (eq evil-this-operator 'evil-delete)
               beg end)
      (save-excursion
        (goto-char beg)
        (when (and (org-at-heading-p)
                   (org-fold-folded-p (line-end-position) 'outline))
          ;; `t t': through invisible text, to the start of the next heading.
          (org-end-of-subtree t t)
          (setq end (max end (point))))))
    `(,beg ,end ,type . ,rest)))
(with-eval-after-load 'evil
  (advice-add 'evil-delete :filter-args #'start/org-evil-delete-folded-subtree))

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
    (goto-char (line-end-position)) ;; logical EOL; org-end-of-line stops at visual EOL under visual-line-mode
    (org-insert-item t) ;; The 't' argument forces a checkbox
    (evil-insert-state))
   ;; Lists
   ((org-in-item-p)
    (goto-char (line-end-position))
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
    (goto-char (line-end-position))
    (org-insert-item t)
    (org-indent-item)
    (evil-insert-state))
   ;; List -> Insert a nested list item
   ((org-in-item-p)
    (goto-char (line-end-position))
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
            ;; Jump to the block's end afterwards: `org-edit-src-exit' moves
            ;; point back to the #+begin_src line when the block is folded,
            ;; which would make the search re-match the same block forever.
            (let ((end (copy-marker (org-element-end element))))
              (org-babel-do-in-edit-buffer (indent-region (point-min) (point-max)))
              (goto-char end)
              (set-marker end nil))))))))
(add-hook 'before-save-hook #'my/org-indent-all-src-blocks)

(defvar start/org--counting-checkboxes nil
  "Non-nil while `org-update-checkbox-count' is counting.")

(defun start/org--empty-checkbox-item-p (item struct)
  "Non-nil if ITEM in STRUCT is just a bullet and a checkbox, with no text or body."
  (save-excursion
    (goto-char item)
    (and (looking-at "[ \t]*\\(?:[-+*]\\|[0-9A-Za-z]+[.)]\\)[ \t]+\\(?:\\[@[^]]+\\][ \t]*\\)?\\[[- X]\\][ \t]*$")
         (<= (org-list-get-item-end item struct) (1+ (line-end-position))))))

(defun start/org--drop-empty-checkbox-items (orig item struct &rest args)
  "While counting, leave empty checkbox items out of the items Org counts.
\=`org-list-get-checkbox' is a defsubst, so it can't be advised; the item lists
the counter walks come from these functions instead."
  (let ((items (apply orig item struct args)))
    (if start/org--counting-checkboxes
        (seq-remove (lambda (i) (start/org--empty-checkbox-item-p i struct)) items)
      items)))

(defun start/org--while-counting-checkboxes (orig &rest args)
  (let ((start/org--counting-checkboxes t))
    (apply orig args)))

(with-eval-after-load 'org-list
  (dolist (fn '(org-list-get-all-items org-list-get-children org-list-get-subtree))
    (advice-add fn :around #'start/org--drop-empty-checkbox-items))
  (advice-add 'org-update-checkbox-count :around #'start/org--while-counting-checkboxes))

(defun start/org-update-all-cookies ()
  "Recount every statistics cookie in an Org buffer."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (ignore-errors (org-update-statistics-cookies 'all)))))
(add-hook 'before-save-hook #'start/org-update-all-cookies)

(defun start/org-update-entry-cookie ()
  "Recount the statistics cookie of the entry at point."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (ignore-errors (org-update-statistics-cookies nil)))))
(with-eval-after-load 'evil
  (add-hook 'evil-insert-state-exit-hook #'start/org-update-entry-cookie))

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

(defvar start/org-refile-old-done-days 3
  "Refile DONE entries whose deadline is more than this many days past.")

(defun start/org--old-done-p ()
  "Non-nil if the entry at point is done with a deadline older than the cutoff."
  (let ((dl (org-entry-get nil "DEADLINE")))
    (and (org-entry-is-done-p)
         dl
         (< (org-time-stamp-to-now dl) (- start/org-refile-old-done-days)))))

(defun start/org-refile-old-done ()
  "Refile all done entries with a deadline more than N days past to one target.
N is `start/org-refile-old-done-days'.  Acts on the active region, or else
the whole (possibly narrowed) buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (let (markers)
    (org-map-entries
     (lambda ()
       (when (start/org--old-done-p)
         (push (point-marker) markers)
         ;; Skip the subtree: it moves along with this entry.
         (setq org-map-continue-from (save-excursion (org-end-of-subtree t t)))))
     nil (when (use-region-p) 'region))
    (setq markers (nreverse markers))
    (if (null markers)
        (message "No DONE entries with a deadline over %d days past"
                 start/org-refile-old-done-days)
      (let* ((rfloc (org-refile-get-location
                     (format "Refile %d old DONE entries to" (length markers))))
             (pos (nth 3 rfloc))
             ;; Track the target with a marker: refiling entries above it
             ;; in the same file shifts its position.
             (target (when pos
                       (with-current-buffer (find-file-noselect (nth 1 rfloc))
                         (copy-marker pos))))
             (n 0))
        (unwind-protect
            (dolist (m markers)
              (when (marker-buffer m)
                (save-excursion
                  (goto-char m)
                  (org-refile nil nil (list (nth 0 rfloc) (nth 1 rfloc) (nth 2 rfloc)
                                            (and target (marker-position target))))
                  (setq n (1+ n))))
              (set-marker m nil))
          (when target (set-marker target nil)))
        (message "Refiled %d entries to %s" n (nth 0 rfloc))))))

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

(defun start/org--repeat-due-p ()
  "Non-nil if this entry has a repeating SCHEDULED/DEADLINE dated today or earlier."
  (seq-some (lambda (prop)
              (let ((ts (org-entry-get nil prop)))
                (and ts (string-match-p org-repeat-re ts)
                     (<= (org-time-stamp-to-now ts) 0))))
            '("SCHEDULED" "DEADLINE")))

(defun start/org--done-catch-up ()
  "Mark the entry at point DONE; repeat for every missed occurrence through today.
Return how many times it was marked DONE."
  (save-excursion
    (org-back-to-heading t)
    (let ((n 1))
      (org-todo 'done)
      (while (and (< n 500) (org-entry-is-todo-p) (start/org--repeat-due-p))
        ;; Org queues one log note per command and each `org-todo'
        ;; overwrites it; write it now so every occurrence is logged.
        (start/org--flush-log-note)
        (org-todo 'done)
        (setq n (1+ n)))
      n)))

(defun start/org--flush-log-note ()
  "Store a pending timestamp-only log note now instead of after the command."
  (when (and org-log-setup (memq org-log-note-how '(time state)))
    (save-window-excursion (org-add-log-note))))

(defun start/org-done-dwim ()
  "Mark DONE; for a repeating entry, also mark all missed occurrences through today.
Works in Org buffers and in the agenda."
  (interactive)
  (let ((n (if (derived-mode-p 'org-agenda-mode)
               (let ((m (or (org-get-at-bol 'org-hd-marker) (org-agenda-error)))
                     (line (line-number-at-pos)))
                 (prog1 (org-with-point-at m (start/org--done-catch-up))
                   (org-agenda-redo)
                   (goto-char (point-min))
                   (forward-line (1- line))))
             (start/org--done-catch-up))))
    (message (if (> n 1) "Marked %d occurrences DONE" "Marked DONE") n)))

;; (ref:orgzly-sort)
(require 'cl-lib)

(defvar start/org-sort-state-order
  '("ACTIVE" "NEXT" "READY" "TODO" "ASSIGNMENT" "BILL" "CHORE" "MEETING"
    "REVIEW" "PLANNING" "HOLD" nil "DONE" "CANCELED")
  "Order used by the `state' sort key.
nil marks where headings without a TODO state go. Unlisted states sort
right after nil.")

(defvar start/org-sort-default-spec '(state priority scheduled)
  "Sort chain used by `start/org-sort-default'.")

(defvar start/org-sort-history nil
  "Minibuffer history for `start/org-sort-custom'.")

(defun start/org-sort--time (time)
  "Return (MISSINGP . SECONDS) for TIME, which may be nil."
  (if time (cons nil (float-time time)) (cons t 0)))

(defvar start/org-sort-key-functions
  `((state
     . ,(lambda ()
          (let* ((order start/org-sort-state-order)
                 (state (org-get-todo-state))
                 (pos (cl-position state order :test #'equal)))
            (cons nil (or pos (1+ (or (cl-position nil order) (length order))))))))
    (priority
     . ,(lambda () (cons nil (string-to-char (org-entry-get nil "PRIORITY")))))
    (scheduled . ,(lambda () (start/org-sort--time (org-get-scheduled-time (point)))))
    (deadline  . ,(lambda () (start/org-sort--time (org-get-deadline-time (point)))))
    (title     . ,(lambda () (cons nil (downcase (or (org-get-heading t t t t) "")))))
    (created
     . ,(lambda ()
          (let* ((raw (org-entry-get nil "CREATED"))
                 (secs (and raw (ignore-errors (org-time-string-to-seconds raw)))))
            (if secs (cons nil secs) (cons t 0))))))
  "Alist of sort key symbol -> function returning (MISSINGP . VALUE) at a heading.")

(defun start/org-sort--parse (spec)
  "Turn SPEC (symbols, optionally prefixed with `.') into ((KEY . REVERSEP) ...)."
  (mapcar (lambda (item)
            (let* ((name (if (symbolp item) (symbol-name item) item))
                   (rev (string-prefix-p "." name))
                   (key (intern (if rev (substring name 1) name))))
              (unless (assq key start/org-sort-key-functions)
                (user-error "Unknown sort key: %s" key))
              (cons key rev)))
          spec))

(defun start/org-sort--less (a b)
  "Return non-nil if (MISSINGP . VALUE) A sorts before B, ignoring direction."
  (let ((va (cdr a)) (vb (cdr b)))
    (if (stringp va) (string< va vb) (< va vb))))

(defvar start/org-sort--active-keys nil
  "Parsed ((KEY . REVERSEP) ...) for the sort in progress.
A dynamic variable, so this works whatever `lexical-binding' is set to.")

(defun start/org-sort--getkey ()
  "Return the list of key values for the heading at point."
  (mapcar (lambda (k) (funcall (alist-get (car k) start/org-sort-key-functions)))
          start/org-sort--active-keys))

(defun start/org-sort--compare (ka kb)
  "Return non-nil if key list KA sorts before KB."
  (catch 'done
    (cl-loop for a in ka for b in kb for k in start/org-sort--active-keys
             do (let ((rev (cdr k)))
                  (cond
                   ;; Missing values always go last, whatever the direction.
                   ((and (car a) (not (car b))) (throw 'done nil))
                   ((and (car b) (not (car a))) (throw 'done t))
                   ((and (car a) (car b)))
                   ((start/org-sort--less a b) (throw 'done (not rev)))
                   ((start/org-sort--less b a) (throw 'done rev)))))
    nil))

(defun start/org-sort-by (spec)
  "Sort the children of the heading at point by the chained SPEC.
SPEC is a list like (state priority .scheduled); `.' reverses a key."
  (let ((start/org-sort--active-keys (start/org-sort--parse spec)))
    ;; Drawer overlays are repaired by advice on `org-sort-entries'
    ;; (see "Hide Properties Drawers").
    (org-sort-entries nil ?f #'start/org-sort--getkey #'start/org-sort--compare)
    (message "Sorted by %s"
             (mapconcat (lambda (k) (concat (if (cdr k) "." "") (symbol-name (car k))))
                        start/org-sort--active-keys " → "))))

;; Keep fold state across sorting. `org-sort-entries' expands the whole subtree
;; first; record which parts were folded (relative to each moved child block)
;; and re-fold them after the reorder.
(defun start/org-sort--range ()
  "Return (BEG END SUBTREEP) for what `org-sort-entries' will sort at point."
  (cond ((use-region-p) (list (region-beginning) (region-end) nil))
        ((org-before-first-heading-p) (list (point-min) (point-max) nil))
        (t (save-excursion
             (org-back-to-heading t)
             (list (point) (progn (org-end-of-subtree t t) (point)) t)))))

(defun start/org-sort--blocks (beg end subtreep)
  "Return ((START . END) ...) for the entries that get sorted in BEG..END.
With SUBTREEP, BEG is the parent heading, which is skipped."
  (save-excursion
    (goto-char beg)
    (when (or subtreep (not (org-at-heading-p)))
      (outline-next-heading))
    (when (and (org-at-heading-p) (< (point) end))
      (let ((level (org-outline-level)) blocks)
        (while (and (org-at-heading-p) (< (point) end))
          (let ((s (point)))
            (org-end-of-subtree t t)
            (push (cons s (min (point) end)) blocks)))
        (nreverse blocks)))))

(defun start/org-sort--fold-runs (beg end)
  "Return ((S . E) ...) for outline-folded runs between BEG and END."
  (let ((pos beg) runs)
    (while (< pos end)
      (let ((next (min end (or (org-fold-next-folding-state-change 'outline pos end) end))))
        (when (org-fold-folded-p pos 'outline)
          (push (cons pos next) runs))
        (setq pos (max next (1+ pos)))))
    (nreverse runs)))

(defun start/org-sort--block-key (blk)
  (string-trim-right (buffer-substring-no-properties (car blk) (cdr blk)) "\n+"))

(defun start/org-sort-preserve-folds (orig &rest args)
  "Around `org-sort-entries': keep collapsed headings collapsed."
  (pcase-let* ((`(,beg ,end ,subtreep) (start/org-sort--range))
               (blocks (start/org-sort--blocks beg end subtreep))
               (runs (start/org-sort--fold-runs beg end))
               ;; key -> list of fold-run lists, one per block with that text
               (per-block (make-hash-table :test #'equal))
               (absolute nil))
    ;; Folds inside one child block move with it; anything else (the
    ;; parent's own text, or a fold spanning the whole parent) stays put.
    (dolist (blk blocks)
      (let (rel)
        (dolist (run runs)
          (when (and (>= (car run) (car blk)) (<= (cdr run) (cdr blk)))
            (push (cons (- (car run) (car blk)) (- (cdr run) (car blk))) rel)))
        (push rel (gethash (start/org-sort--block-key blk) per-block))))
    (dolist (run runs)
      (unless (seq-find (lambda (b) (and (>= (car run) (car b)) (<= (cdr run) (cdr b))))
                        blocks)
        (push (cons (- (car run) beg) (- (cdr run) beg)) absolute)))
    (let ((beg-m (copy-marker beg))
          (end-m (copy-marker end t))
          ;; Undo/redo entry: restores the pre-sort folds when the sort is
          ;; undone. Its last slot (post-sort folds) is filled in below.
          (undo-entry (list 'apply #'start/org-sort--undo-folds
                            beg end runs nil)))
      (when (listp buffer-undo-list)
        (push undo-entry buffer-undo-list))
      (unwind-protect (apply orig args)
        (let ((nbeg (marker-position beg-m))
              (nend (marker-position end-m)))
          (set-marker beg-m nil)
          (set-marker end-m nil)
          (dolist (blk (start/org-sort--blocks nbeg nend subtreep))
            (let ((key (start/org-sort--block-key blk)))
              (dolist (run (pop (gethash key per-block)))
                (org-fold-region (+ (car blk) (car run))
                                 (min (cdr blk) (+ (car blk) (cdr run)))
                                 t 'outline))))
          (dolist (run absolute)
            (org-fold-region (+ nbeg (car run))
                             (min nend (+ nbeg (cdr run)))
                             t 'outline))
          (setf (nth 3 undo-entry) (max end nend)
                (nth 5 undo-entry) (start/org-sort--fold-runs nbeg nend)))))))

(defun start/org-sort--undo-folds (beg end runs other-runs)
  "Undo-list entry: fold RUNS between BEG and END after the undo finishes.
Pushes the inverse entry (with OTHER-RUNS) so redo restores those folds."
  (when (listp buffer-undo-list)
    (push (list 'apply #'start/org-sort--undo-folds beg end other-runs runs)
          buffer-undo-list))
  ;; Text is still being restored by the rest of the undo group; wait for it.
  (run-at-time 0 nil #'start/org-sort--apply-folds (current-buffer) beg end runs))

(defun start/org-sort--apply-folds (buf beg end runs)
  "In BUF, unfold BEG..END, then fold each (S . E) in RUNS."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((end (min end (point-max))))
        (when (< beg end)
          (org-fold-region beg end nil 'outline)
          (dolist (run runs)
            (when (< (car run) end)
              (org-fold-region (car run) (min (cdr run) end) t 'outline))))))))
(advice-add 'org-sort-entries :around #'start/org-sort-preserve-folds)

(defun start/org-sort-default ()
  "Sort children by `start/org-sort-default-spec'."
  (interactive) (start/org-sort-by start/org-sort-default-spec))
(defun start/org-sort-by-priority ()
  "Sort children by priority (A first)."
  (interactive) (start/org-sort-by '(priority)))
(defun start/org-sort-by-priority-reverse ()
  "Sort children by priority (lowest first)."
  (interactive) (start/org-sort-by '(.priority)))
(defun start/org-sort-by-scheduled ()
  "Sort children by scheduled date."
  (interactive) (start/org-sort-by '(scheduled)))
(defun start/org-sort-by-deadline ()
  "Sort children by deadline."
  (interactive) (start/org-sort-by '(deadline)))
(defun start/org-sort-by-state ()
  "Sort children by TODO state (see `start/org-sort-state-order')."
  (interactive) (start/org-sort-by '(state)))
;; In a plain list, title sorts sort that list Orgzly-style: open items
;; ([ ] and [-]) first, then checked ones, each group by text.
(defun start/org-sort--checklist-key ()
  "Return (CHECKEDP . TEXT) for the item at point."
  (save-excursion
    (forward-line 0)
    (if (looking-at org-list-full-item-re)
        (cons (equal (match-string 3) "[X]")
              (downcase (string-trim (buffer-substring-no-properties
                                      (match-end 0) (line-end-position)))))
      '(nil . ""))))

(defun start/org-sort--checklist-less (a b reverse)
  "Non-nil if checklist key A goes before B; REVERSE flips only the text order."
  ;; `org-sort-list' gives nil when its own item regexp fails; treat that as
  ;; an empty open item.
  (let ((a (or a '(nil . ""))) (b (or b '(nil . ""))))
    (cond ((and (car b) (not (car a))) t)
          ((and (car a) (not (car b))) nil)
          (reverse (string< (cdr b) (cdr a)))
          (t (string< (cdr a) (cdr b))))))

(defun start/org-sort--list-at-point-p ()
  "Non-nil if point is in a plain list item (not on a heading)."
  (and (not (org-at-heading-p)) (org-in-item-p)))

(defun start/org-sort-list-at-point (&optional reverse)
  "Sort the plain list at point: open items first, then checked, each A–Z.
With REVERSE, Z–A inside each group. Only the list containing point is
sorted (a sublist if point is in one); sub-items move with their parent.
Point stays on the same item."
  (let* ((item (org-in-item-p))
         (offset (- (point) item))
         (item-line (buffer-substring-no-properties
                     item (save-excursion (goto-char item) (line-end-position))))
         (struct (org-list-struct))
         (prevs (org-list-prevs-alist struct))
         (top (org-list-get-list-begin item struct prevs))
         (bottom (copy-marker (org-list-get-list-end item struct prevs))))
    (goto-char item)
    (let ((inhibit-message t))
      (org-sort-list nil ?f #'start/org-sort--checklist-key
                     (lambda (a b) (start/org-sort--checklist-less a b reverse))))
    ;; `org-sort-list' rebuilds the text; find the item again by its first line.
    (goto-char top)
    (if (search-forward item-line bottom t)
        (goto-char (min (+ (match-beginning 0) offset) (line-end-position)))
      (goto-char top))
    (set-marker bottom nil)))

(defun start/org-sort-title-and-checklist (reverse)
  "In a plain list, sort that list; otherwise sort the subheadings by title."
  (if (start/org-sort--list-at-point-p)
      (progn (start/org-sort-list-at-point reverse)
             (message "Sorted list %s" (if reverse "Z–A" "A–Z")))
    (start/org-sort-by (if reverse '(.title) '(title)))))

(defun start/org-sort-by-title ()
  "Sort A–Z: the list at point if in one, otherwise the subheadings."
  (interactive) (start/org-sort-title-and-checklist nil))
(defun start/org-sort-by-title-reverse ()
  "Sort Z–A: the list at point if in one, otherwise the subheadings."
  (interactive) (start/org-sort-title-and-checklist t))
(defun start/org-sort-by-created ()
  "Sort children by the CREATED property."
  (interactive) (start/org-sort-by '(created)))

(defun start/org-sort-custom (spec)
  "Sort children by a chain of keys read from the minibuffer.
Separate keys with commas; prefix with `.' to reverse, e.g. state,.priority,deadline."
  (interactive
   (let* ((names (mapcar (lambda (k) (symbol-name (car k))) start/org-sort-key-functions))
          (cands (append names (mapcar (lambda (n) (concat "." n)) names))))
     (list (completing-read-multiple
            "Sort by (comma-separated, .key = reverse): " cands nil t
            (car start/org-sort-history) 'start/org-sort-history))))
  (start/org-sort-by spec))

;; (ref:launch-done-collapse)
(defvar my/org-auto-collapse-keywords '("PLANNING" "HOLD" "READY" "REVIEW")
  "Additional TODO keywords to automatically collapse like DONE states.")

(defun my/org-should-collapse-state-p (state)
  "Return non-nil if STATE is a DONE state or in `my/org-auto-collapse-keywords'."
  (and state
       (or (member state org-done-keywords)
           (member state my/org-auto-collapse-keywords))))

(defun my/org-hide-done-entries-dynamic ()
  "Hide all entries that are in any 'DONE' or auto-collapse state in the current buffer."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-map-entries
     (lambda ()
       (let ((state (org-get-todo-state)))
         (when (my/org-should-collapse-state-p state)
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
              ;; Hidden with `invisible', not `display ""': Emacs pushes point
              ;; past an empty display string that starts at point, so A on a
              ;; heading with a drawer jumped to after :END:. FRONT-ADVANCE t
              ;; keeps text typed at the end of the heading outside the overlay.
              (let ((ov (make-overlay ov-start ov-end nil t nil)))
                (overlay-put ov 'invisible t)
                (overlay-put ov 'my-stealth-drawer t)
                ;; Refontify the line before the drawer so the expanded-heading
                ;; indicator moves onto this overlay (see "Expanded Heading
                ;; Indicator").
                (font-lock-flush ov-start (1+ ov-start)))))))))

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

  ;; Evil's A (`evil-append-line') and $ move with `move-end-of-line', which
  ;; skips invisible text at the end of a line. The hidden-drawer overlay
  ;; starts at a heading's newline, so that skipped past the whole drawer and
  ;; A on such a heading typed after :END:. Without a count, stay on the line.
  (defun start/evil-move-end-of-line-stay-on-line (orig &optional arg)
    "Around `evil-move-end-of-line': without a count, don't leave the line."
    (let ((eol (line-end-position)))
      (funcall orig arg)
      (when (and (or (null arg) (eq arg 1))
                 (derived-mode-p 'org-mode)
                 (> (point) eol))
        (goto-char eol))))
  (with-eval-after-load 'evil
    (advice-add 'evil-move-end-of-line :around #'start/evil-move-end-of-line-stay-on-line))

  ;; RET at the end of a heading followed by hidden text (a folded subtree or
  ;; a hidden drawer). Plain `org-return' puts the new line before the hidden
  ;; text: the fold's ▾ moves down to the new line, and a property drawer gets
  ;; split from its heading (so Org stops reading its ID). Instead the new
  ;; line goes at the start of the body, after any drawer, and a folded
  ;; heading is opened first (body plus child headings) so the line is visible.
  (defun start/org-return-past-hidden (orig &rest args)
    "Around `org-return': at the end of a heading, start a line in its body."
    (let ((eol (line-end-position)))
      (if (and (org-at-heading-p)
               (or (eolp) (start/rest-of-line-invisible-p (point)))
               (< eol (point-max))
               (invisible-p eol))
          (progn
            (when (org-fold-folded-p eol 'outline)
              (org-fold-show-entry)
              (org-fold-show-children))
            (let ((ov (seq-find (lambda (o) (overlay-get o 'my-stealth-drawer))
                                (overlays-at eol))))
              (goto-char (if ov (overlay-end ov) eol)))
            (apply orig args))
        (apply orig args))))
  ;; Outermost (depth -100): Org adds its own advice to `org-return' when a
  ;; buffer enters org-mode (`org-fold-check-before-invisible-edit-maybe'),
  ;; which would unfold the heading before this sees it folded.
  (advice-add 'org-return :around #'start/org-return-past-hidden '((depth . -100)))

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
             (let ((state (org-get-todo-state)))
               (when (and (member state org-not-done-keywords)
                          (not (my/org-should-collapse-state-p state)))
                 (setq has-active-todos t))))
           nil 'tree))

        (when has-active-todos
          (org-map-entries
           (lambda ()
             (let ((state (org-get-todo-state)))
               (when (my/org-should-collapse-state-p state)
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

  ;; Keep fold-cycling status ("SUBTREE (NO CHILDREN)", "TODOs Expanded", ...)
  ;; out of the echo area. They still go to *Messages*; prompts and errors
  ;; aren't affected.
  (defun start/without-echo-messages (orig &rest args)
    "Run ORIG with `inhibit-message', so its messages only go to *Messages*."
    (let ((inhibit-message t))
      (apply orig args)))
  (advice-add 'my/org-cycle-dwim :around #'start/without-echo-messages)
  (advice-add 'my/org-shifttab-dwim :around #'start/without-echo-messages)

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
             ;; If DONE/CANCELED or auto-collapse keyword -> Fully fold the entire subtree
             ((my/org-should-collapse-state-p state)
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

  ;; 9. Re-hide drawers after text is re-inserted (undo/redo, sorting)
  ;; Overlays don't travel with deleted/re-inserted text, so after an undo the
  ;; old stealth overlays sit at the wrong spans and the drawers show again.
  (defun my/org-rehide-drawers-in (beg end)
    "Re-hide drawers between BEG and END if they were hidden there, or elsewhere in the buffer."
    (save-excursion
      ;; Snap to whole entries so a drawer is never half inside the range.
      (let* ((beg (progn (goto-char beg)
                         (if (org-before-first-heading-p) (point-min)
                           (org-back-to-heading t) (point))))
             (end (progn (goto-char end)
                         (if (outline-next-heading) (point) (point-max))))
             ;; Only overlays that start inside the range; `remove-overlays'
             ;; would instead trim a neighbour's overlay and expose its :END:.
             (ovs (seq-filter (lambda (ov) (and (overlay-get ov 'my-stealth-drawer)
                                                (>= (overlay-start ov) beg)
                                                (< (overlay-start ov) end)))
                              (overlays-in beg end))))
        ;; Also when the undo brought back drawers that had no overlay
        ;; left (e.g. undoing a `dd' that deleted a heading with its
        ;; drawer), as long as drawers are hidden elsewhere in the buffer.
        (when (or ovs (my/org-buffer-has-hidden-drawers-p))
          (mapc #'delete-overlay ovs)
          (my/hide-drawers-completely beg end)))))

  (defun my/org-buffer-has-hidden-drawers-p ()
    "Non-nil if any drawer in the buffer is hidden by a stealth overlay."
    (seq-some (lambda (ov) (overlay-get ov 'my-stealth-drawer))
              (overlays-in (point-min) (point-max))))

  (defvar-local my/org--undo-changed nil
    "(BEG . END) span touched by the undo in progress.")

  (defun my/org--track-undo-change (beg end _len)
    (setq my/org--undo-changed
          (if my/org--undo-changed
              (cons (min beg (car my/org--undo-changed))
                    (max end (cdr my/org--undo-changed)))
            (cons beg end))))

  (defun my/org-rehide-drawers-after-undo (orig &rest args)
    "Around `primitive-undo': re-hide drawers in the span the undo touched."
    (if (not (derived-mode-p 'org-mode))
        (apply orig args)
      (setq my/org--undo-changed nil)
      (add-hook 'after-change-functions #'my/org--track-undo-change nil t)
      (unwind-protect (apply orig args)
        (remove-hook 'after-change-functions #'my/org--track-undo-change t)
        (when my/org--undo-changed
          (let ((b (copy-marker (car my/org--undo-changed)))
                (e (copy-marker (cdr my/org--undo-changed) t)))
            (setq my/org--undo-changed nil)
            ;; Wait until the whole undo command finishes, then repair.
            ;; Pass B/E as timer args: init.el uses dynamic binding, so a
            ;; lambda here would not close over them.
            (run-at-time 0 nil #'my/org--rehide-after-undo (current-buffer) b e))))))

  (defun my/org--rehide-after-undo (buf b e)
    "Timer callback: re-hide drawers between markers B and E in BUF."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (my/org-rehide-drawers-in b e)))
    (set-marker b nil)
    (set-marker e nil))
  (advice-add 'primitive-undo :around #'my/org-rehide-drawers-after-undo)

  ;; `dd' on a heading whose drawer is hidden: the heading and its drawer
  ;; look like one line, so delete them together. Evil's linewise range ends
  ;; at the heading's newline, which is where the hidden drawer's overlay
  ;; starts, so it used to delete just the heading and leave the drawer
  ;; (with its :ID:) stranded under the previous entry. Only for `d', not
  ;; `c' (retyping a heading keeps its drawer) or `y' (no duplicate IDs).
  (defun my/org-evil-delete-with-hidden-drawers (args)
    "Filter-args for `evil-delete': extend a linewise delete over hidden drawers."
    (pcase-let ((`(,beg ,end ,type . ,rest) args))
      (when (and (derived-mode-p 'org-mode)
                 (eq type 'line)
                 (eq evil-this-operator 'evil-delete)
                 end (> end (point-min)))
        (let (ov)
          (while (setq ov (seq-find (lambda (o) (and (overlay-get o 'my-stealth-drawer)
                                                     (= (overlay-start o) (1- end))))
                                    (overlays-at (1- end))))
            (setq end (min (point-max) (1+ (overlay-end ov)))))))
      `(,beg ,end ,type . ,rest)))
  (with-eval-after-load 'evil
    (advice-add 'evil-delete :filter-args #'my/org-evil-delete-with-hidden-drawers))

  ;; `cc' on such a heading: change only the heading line's text and keep
  ;; its drawer attached. A linewise change would delete the heading's
  ;; newline (the start of the hidden drawer) and reopen a line elsewhere,
  ;; losing the stars and leaving the drawer under the new line.
  (defun my/org-evil-change-keep-hidden-drawer (args)
    "Filter-args for `evil-change': on a heading line followed by a hidden
drawer, change the line's text instead of the whole line."
    (pcase-let ((`(,beg ,end ,type . ,rest) args))
      (if (and (derived-mode-p 'org-mode)
               (eq type 'line)
               end (> end beg)
               (seq-find (lambda (o) (and (overlay-get o 'my-stealth-drawer)
                                          (= (overlay-start o) (1- end))))
                         (overlays-at (1- end))))
          `(,beg ,(1- end) exclusive . ,rest)
        args)))
  (with-eval-after-load 'evil
    (advice-add 'evil-change :filter-args #'my/org-evil-change-keep-hidden-drawer))

  ;; Any other edit that breaks a hidden drawer's shape (deleting the heading
  ;; line some other way, cutting into the drawer) leaves an overlay that no
  ;; longer starts at the heading's newline: it shows as a blank line with a
  ;; stray expanded-heading " ▸". Re-hide around such overlays so they match
  ;; the text again; a drawer that's no longer whole stays visible.
  (defun my/org-stealth-drawer-intact-p (ov)
    "Non-nil if OV still covers \"\\n:DRAWER: ... :END:\" exactly."
    (let ((s (overlay-start ov)) (e (overlay-end ov)))
      (and s (< s e)
           (save-excursion
             (goto-char s)
             (looking-at (if (= s (point-min))
                             "[ \t]*:\\(?:PROPERTIES\\|LOGBOOK\\):[ \t]*$"
                           "\n[ \t]*:\\(?:PROPERTIES\\|LOGBOOK\\):[ \t]*$")))
           (save-excursion
             (goto-char e)
             (and (eolp) (progn (forward-line 0) (looking-at "[ \t]*:END:[ \t]*$")))))))

  (defun my/org-repair-stealth-drawers (beg end _len)
    "After-change: re-hide around hidden-drawer overlays the change broke."
    (let ((broken (seq-filter (lambda (ov) (and (overlay-get ov 'my-stealth-drawer)
                                                (not (my/org-stealth-drawer-intact-p ov))))
                              (overlays-in (max (point-min) (1- beg))
                                           (min (point-max) (1+ end))))))
      (dolist (ov broken)
        (let ((s (overlay-start ov)) (e (overlay-end ov)))
          (delete-overlay ov)
          ;; Hides the span again only where it still holds a whole drawer.
          (save-excursion
            (my/hide-drawers-completely
             (progn (goto-char s) (line-beginning-position))
             (progn (goto-char (min (point-max) (1+ e))) (line-end-position))))))))
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'after-change-functions #'my/org-repair-stealth-drawers nil t)))

  (defun my/org-rehide-drawers-after-sort (orig &rest args)
    "Around `org-sort-entries': re-hide drawers in the range that was sorted.
Sorting deletes and re-inserts the text, collapsing the stealth overlays."
    (let ((beg (copy-marker
                (cond ((use-region-p) (region-beginning))
                      ((org-before-first-heading-p) (point-min))
                      (t (save-excursion (org-back-to-heading t) (point))))))
          (end (copy-marker
                (cond ((use-region-p) (region-end))
                      ((org-before-first-heading-p) (point-max))
                      (t (save-excursion (org-end-of-subtree t t) (point))))
                t)))
      (unwind-protect (apply orig args)
        (my/org-rehide-drawers-in beg end)
        (set-marker beg nil)
        (set-marker end nil))))
  (advice-add 'org-sort-entries :around #'my/org-rehide-drawers-after-sort)

  ;; Catch all common undo commands to trigger our restore logic
  (advice-add 'undo :after #'my/org-restore-fold-on-undo)
  (with-eval-after-load 'evil
    (advice-add 'evil-undo :after #'my/org-restore-fold-on-undo)
    (when (fboundp 'undo-tree-undo)
      (advice-add 'undo-tree-undo :after #'my/org-restore-fold-on-undo))))

(defun start/org-revert-insert-delicately (file-name auto-save-p)
  "Insert FILE-NAME with `revert-buffer-insert-file-contents-delicately'.
Org's after-change fold fixer is skipped. When the diff times out, Emacs
falls back to delete+insert, and the fixer then signals
`beginning-of-buffer' halfway through the revert, which leaves the buffer
flagged modified and skips `after-revert-hook'. Folds are handled after
the revert instead (see \"Fold Entries Completed Outside Emacs\")."
  (org-fold-core-ignore-modifications
    (revert-buffer-insert-file-contents-delicately file-name auto-save-p)))

(defun start/org-revert-delicately ()
  "Revert Org buffers with a diff so external (synced) edits keep folds."
  (setq-local revert-buffer-insert-file-contents-function
              #'start/org-revert-insert-delicately))
(add-hook 'org-mode-hook #'start/org-revert-delicately)

(defun start/org-after-revert-refold ()
  "Re-hide stealth drawers after a revert (new text arrives without overlays)."
  (when (and (derived-mode-p 'org-mode) (fboundp 'my/org-hide-drawers-global))
    (my/org-hide-drawers-global)))
(add-hook 'after-revert-hook #'start/org-after-revert-refold)

(defvar start/org-revert-fold-dirs (list start/org-root)
  "Directories whose Org buffers keep their folds across a revert.")

(defvar-local start/org-revert--snapshot nil
  "Fold state recorded by `start/org-revert-snapshot' for the revert in progress.")
;; A manual `revert-buffer' re-runs `org-mode', which kills local variables.
(put 'start/org-revert--snapshot 'permanent-local t)

(defun start/org-revert--applies-p ()
  "Non-nil if the current buffer is an Org file under `start/org-revert-fold-dirs'."
  (and (derived-mode-p 'org-mode)
       buffer-file-name
       (seq-some (lambda (dir) (file-in-directory-p buffer-file-name dir))
                 start/org-revert-fold-dirs)))

(defun start/org-revert--headings ()
  "Return (KEY TODO BEG LEVEL TITLE) for every heading, in buffer order.
KEY is \"id:ID\", or \"path:Parent/.../Title\" when there is no ID; a #N
suffix tells repeats apart. Statistics cookies are left out of titles, since
completing a child changes them."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((seen (make-hash-table :test #'equal))
         (case-fold-search nil)
         stack out)
     (while (re-search-forward org-outline-regexp-bol nil t)
       (save-excursion
         (goto-char (match-beginning 0))
         (let* ((comp (org-heading-components))
                (level (nth 0 comp))
                (title (string-trim
                        (replace-regexp-in-string
                         "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" "" (or (nth 4 comp) ""))))
                (id (org-entry-get (point) "ID")))
           (while (and stack (>= (caar stack) level)) (pop stack))
           (let* ((base (if id (concat "id:" id)
                          (concat "path:" (string-join
                                           (reverse (cons title (mapcar #'cdr stack)))
                                           "/"))))
                  (n (puthash base (1+ (gethash base seen 0)) seen)))
             (push (list (format "%s#%d" base n) (nth 2 comp) (point) level title) out))
           (push (cons level title) stack))))
     (nreverse out))))

(defun start/org-revert--ranges (headings)
  "Return (LINE-RANGE BODY-RANGE EOL) for each of HEADINGS.
LINE-RANGE covers the newline before the heading and the heading line; it is
nil for a heading on the first line. BODY-RANGE runs from the end of the
heading line up to that newline before the next heading."
  (let ((last-end (if (eq (char-before (point-max)) ?\n)
                      (1- (point-max))
                    (point-max))))
    (cl-loop for (h . rest) on headings
             for beg = (nth 2 h)
             for eol = (save-excursion (goto-char beg) (line-end-position))
             for body-end = (if rest (1- (nth 2 (car rest))) (max eol last-end))
             collect (list (and (> beg (point-min)) (cons (1- beg) eol))
                           (cons eol body-end)
                           eol))))

(defun start/org-revert--pos-loc (pos headings)
  "Return (KEY . OFFSET) locating POS relative to its heading in HEADINGS."
  (let ((h (cl-find-if (lambda (h) (<= (nth 2 h) pos)) (reverse headings))))
    (if h (cons (car h) (- pos (nth 2 h))) (cons nil pos))))

(defun start/org-revert--loc-pos (loc headings)
  "Return the position for LOC (from `start/org-revert--pos-loc'), or nil."
  (let ((h (and (car loc) (assoc (car loc) headings))))
    (cond (h (min (+ (nth 2 h) (cdr loc)) (point-max)))
          ((null (car loc)) (min (cdr loc) (point-max))))))

(defun start/org-revert-snapshot ()
  "Record TODO keywords, fold state, point and windows before a revert."
  (setq start/org-revert--snapshot nil)
  (when (start/org-revert--applies-p)
    (save-restriction
      (widen)
      (let* ((headings (start/org-revert--headings))
             (state (make-hash-table :test #'equal)))
        (cl-loop for h in headings
                 for (line _body eol) in (start/org-revert--ranges headings)
                 do (puthash (car h)
                             (list (nth 1 h)
                                   (and line (org-fold-folded-p (nth 2 h) 'outline) t)
                                   (and (< eol (point-max))
                                        (org-fold-folded-p eol 'outline) t))
                             state))
        (cl-flet ((spot (pos) (cons (copy-marker pos)
                                    (start/org-revert--pos-loc pos headings))))
          (setq start/org-revert--snapshot
                (list :hash (buffer-hash)
                      :state state
                      :point (spot (point))
                      :windows (mapcar (lambda (w)
                                         (list w (spot (window-point w))
                                               (spot (window-start w))))
                                       (get-buffer-window-list nil nil t)))))))))

(defun start/org-revert--new-heading-folded-p (todo title beg)
  "Non-nil if a new heading should start folded, as it would on file open."
  (or (my/org-should-collapse-state-p todo)
      (string-prefix-p "#" title)
      (equal (org-entry-get beg "VISIBILITY") "folded")))

(defun start/org-revert-restore-folds ()
  "Restore the folds recorded before a revert and fold newly completed entries."
  (let ((snap start/org-revert--snapshot))
    (setq start/org-revert--snapshot nil)
    (when (and snap (start/org-revert--applies-p))
      (unwind-protect
          (unless (equal (buffer-hash) (plist-get snap :hash))
            (with-demoted-errors "Fold restore after revert: %S"
              (save-restriction
                (widen)
                (start/org-revert--apply snap))))
        ;; Release the markers recorded by the snapshot.
        (dolist (spot (cons (plist-get snap :point)
                            (mapcan #'cdr (plist-get snap :windows))))
          (set-marker (car spot) nil))))))

(defun start/org-revert--apply (snap)
  "Apply snapshot SNAP to the reverted buffer."
  (let* ((state (plist-get snap :state))
         (headings (start/org-revert--headings))
         (ranges (start/org-revert--ranges headings))
         stack folded completed)
    (when headings
      ;; Desired visibility of each range, in buffer order.
      (cl-loop
       for (key todo beg level title) in headings
       for (line body _eol) in ranges
       for old = (gethash key state)
       do (while (and stack (>= (caar stack) level)) (pop stack))
       (let* ((parent (cdar stack))
              (line-hidden (if old (nth 1 old)
                             (and parent (or (car parent) (cdr parent)))))
              (body-folded (if old (nth 2 old)
                             (start/org-revert--new-heading-folded-p todo title beg))))
         (when (and old
                    (member (car old) org-not-done-keywords)
                    (my/org-should-collapse-state-p todo))
           (push (copy-marker beg) completed))
         (push (cons level (cons line-hidden body-folded)) stack)
         (when (and line line-hidden) (push line folded))
         (when (and body-folded (< (car body) (cdr body))) (push body folded))))
      ;; Merge adjacent ranges into runs, compare with what is folded now,
      ;; and only touch the buffer if they differ.
      (let* ((region-beg (or (car (nth 0 (car ranges))) (nth 2 (car headings))))
             (region-end (cdr (nth 1 (car (last ranges)))))
             (merge (lambda (runs)
                      (let (out)
                        (dolist (r runs (nreverse out))
                          (if (and out (>= (cdar out) (car r)))
                              (setcdr (car out) (max (cdar out) (cdr r)))
                            (push (cons (car r) (cdr r)) out))))))
             (want (funcall merge (sort folded (lambda (a b) (< (car a) (car b))))))
             (have (funcall merge (start/org-sort--fold-runs region-beg region-end))))
        (unless (equal want have)
          (org-fold-region region-beg region-end nil 'outline)
          (dolist (run want)
            (org-fold-region (car run) (cdr run) t 'outline))))
      ;; Entries completed elsewhere fold the way a manual completion does.
      (dolist (m completed)
        (save-excursion
          (goto-char m)
          (my/org-collapse-heading))
        (set-marker m nil)))
    ;; Point and window positions.
    ;; A delicate revert keeps markers in place. After the delete+insert
    ;; fallback they all collapse to one spot, so use a marker only while it
    ;; is still under the heading it started in.
    (cl-flet ((pos (spot)
                (let ((m (marker-position (car spot))))
                  (if (equal (car (start/org-revert--pos-loc m headings))
                             (car (cdr spot)))
                      m
                    (or (start/org-revert--loc-pos (cdr spot) headings) m)))))
      (when-let* ((p (pos (plist-get snap :point))))
        (goto-char p))
      (pcase-dolist (`(,win ,pt ,start) (plist-get snap :windows))
        (when (and (window-live-p win) (eq (window-buffer win) (current-buffer)))
          (when-let* ((s (pos start))) (set-window-start win s t))
          (when-let* ((p (pos pt))) (set-window-point win p)))))))

(add-hook 'before-revert-hook #'start/org-revert-snapshot)
;; Run after `start/org-after-revert-refold' has re-hidden the drawers.
(add-hook 'after-revert-hook #'start/org-revert-restore-folds 90)

(add-hook 'after-save-hook
          (lambda ()
            (when (and (derived-mode-p 'org-mode)
                       (executable-find "syncthingctl"))
              (shell-command "syncthingctl rescan-all"))))

(defun my/org-yank-inside-code ()
  "Copy the contents of the inline code (~/=), inline src block, or src block at point."
  (interactive)
  (let* ((element (org-element-context))
         (type (car element))
         ;; Use org-element-property instead of plist-get to handle deferred values in Org 9.7+
         (text (when (memq type '(code verbatim src-block inline-src-block))
                 (org-element-property :value element))))
    (if text
        (progn
          (kill-new (substring-no-properties text))
          (message "Yanked inside code block!"))
      (message "Not inside an Org code element!"))))

(defconst start/org-todo-faces
  '(("TODO"       . (:foreground "#282c34" :background "#98be65" :weight bold))
    ("NEXT"       . (:foreground "#282c34" :background "#6f8fff" :weight bold))
    ("PLANNING"   . (:foreground "#282c34" :background "#c792ea" :weight bold))
    ("READY"      . (:foreground "#282c34" :background "#82b7ff" :weight bold))
    ("ACTIVE"     . (:foreground "#282c34" :background "#7fdc6f" :weight bold))
    ("REVIEW"     . (:foreground "#282c34" :background "#e0a96d" :weight bold))
    ("HOLD"       . (:foreground "#282c34" :background "#e6d96c" :weight bold))
    ("ASSIGNMENT" . (:foreground "#282c34" :background "#e5404e" :weight bold))
    ("BILL"       . (:foreground "#282c34" :background "#fc830a" :weight bold))
    ("CHORE"      . (:foreground "#282c34" :background "#e2b93d" :weight bold))
    ("MEETING"    . (:foreground "#282c34" :background "#f52dfc" :weight bold))
    ("DONE"       . (:foreground "#1f2328" :background "#304b60" :weight bold))
    ("CANCELED"   . (:foreground "#1f2328" :background "#e06c75" :weight bold)))
  "TODO keyword faces shared by `org-todo-keyword-faces' and `org-modern-todo-faces'.")

(defun start/org-refile-target-not-done-p ()
  "Refile target filter: leave out DONE/CANCELED headings."
  (not (org-entry-is-done-p)))

(defun start/org-save-after-refile (&rest _)
  "Save Org buffers after a refile, so the moved entry is written to both files."
  (org-save-all-org-buffers))
(advice-add 'org-refile :after #'start/org-save-after-refile)

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
  ;; Refile (SPC o r): the current file 3 levels deep, the main files'
  ;; top-level headings, shown as Vertico paths like Tasks.org/Todos.
  (org-refile-targets
   `((nil :maxlevel . 3)
     (,start/org-tasks-file :maxlevel . 1)
     (,start/org-projects-file :maxlevel . 1)
     (,(expand-file-name "Archive.org" start/org-main-dir) :maxlevel . 1)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-target-verify-function #'start/org-refile-target-not-done-p)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-repeat-to-state t) ;; Repeaters keep their keyword (CHORE stays CHORE, not TODO)

  ;; TODO KEYWORDS
  ;; (ref:todo-keywords)
  (org-todo-keywords
   '((sequence "TODO(t)" "ASSIGNMENT(a)" "BILL(b)" "CHORE(c)" "MEETING(m)" "NEXT(n)" "PLANNING(P)" "REVIEW(V)" "HOLD(H)" "READY(R)" "ACTIVE(A)" "|" "DONE(d!)" "CANCELED(C!)")))
  (org-todo-keyword-faces start/org-todo-faces) ;; defined above, shared with Org-Modern

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
		  		 '((:name "📚 Assignments" ; overdue + due today, above everything else
		  		          :todo "ASSIGNMENT"
		  		          :order 0)
		  		   (:name "❗Overdue"
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
  (defvar my/org-auto-collapse-keywords '("PLANNING" "HOLD" "READY" "REVIEW")
	"Additional TODO keywords to automatically collapse like DONE states.")

  (defun my/org-should-collapse-state-p (state)
	"Return non-nil if STATE is a DONE state or in `my/org-auto-collapse-keywords'."
	(and state
		 (or (member state org-done-keywords)
			 (member state my/org-auto-collapse-keywords))))

  (defun my/org-hide-done-entries-dynamic ()
	"Hide all entries that are in any 'DONE' or auto-collapse state in the current buffer."
	(interactive)
	(when (derived-mode-p 'org-mode)
	  (org-map-entries
	   (lambda ()
		 (let ((state (org-get-todo-state)))
		   (when (my/org-should-collapse-state-p state)
		  	 (outline-hide-subtree))))
	   t 'file)))

  ;; COLLAPSE DONE ENTRIES WHEN MARKED
  ;; (ref:mark-done-collapse)
  (defun my/org-collapse-on-done ()
    "Collapse the heading if it ends in a DONE or auto-collapse state.
Runs on a 0s timer so Org has finished logging and repeating first; a
repeating task is back to TODO by then and stays open."
    (when (my/org-should-collapse-state-p org-state)
      (run-at-time 0 nil #'my/org-collapse-at-marker
                   (save-excursion (org-back-to-heading t) (point-marker)))))

  (defun my/org-collapse-heading ()
    "Fold the heading at point if it is in a collapse state.
Otherwise (a repeater that went back to TODO) keep it open and only tuck
its new LOGBOOK/LAST_REPEAT drawers away. Shared by marking an entry
DONE here and by `start/org-revert-restore-folds' for external edits."
    (save-excursion
      (org-back-to-heading t)
      (if (my/org-should-collapse-state-p (org-get-todo-state))
          (progn
            ;; Hide the drawer first, then the subtree
            (org-cycle-hide-drawers 'all)
            (outline-hide-subtree))
        (my/org-hide-drawers-subtree))))

  (defun my/org-collapse-at-marker (m)
    "Fold the heading at marker M if it is still in a collapse state."
    (when (buffer-live-p (marker-buffer m))
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char m)
          (my/org-collapse-heading))))
    (set-marker m nil))
  ;; Launch: Auto-collapse when opening a file
  (add-hook 'find-file-hook #'my/org-hide-done-entries-dynamic)
  ;; Interaction: Auto-collapse the moment you mark it DONE
  (add-hook 'org-after-todo-state-change-hook #'my/org-collapse-on-done)

  ;; AUTO COLLAPSE KEYWORD HEADINGS
  ;; (ref:collapse-keyword-headings)
  ;; Alist of heading names and optional level restrictions
  ;; (defvar my/org-collapse-headings
  ;; 	'(("#Future-Bills" .  1)   ; Only level 1
  ;; 	  ("#Coffee-Roasting" . 1)
  ;; 	  ("#Archive" . nil)         ; Any level
  ;; 	  ("#Repeaters" . nil)          ; Any level
  ;; 	  )
  ;; 	"Alist of (heading-name . level).
  ;; 		  		          If level is nil, collapse at any level.
  ;; 		  		          If level is a number, only collapse at that level.")

  ;; (defun my/org-hide-matching-headings ()
  ;; 	"Force hide all headings that match entries in `my/org-collapse-headings'.
  ;; 		  		          Respects level restrictions if specified."
  ;; 	(org-map-entries
  ;; 	 (lambda ()
  ;; 	   (let ((heading (org-get-heading t t t t))  ; Get heading without tags, todo, etc.
  ;; 		  	 (current-level (org-current-level)))
  ;; 		 (dolist (entry my/org-collapse-headings)
  ;; 		   (let ((target-heading (car entry))
  ;; 		  		 (target-level (cdr entry)))
  ;; 		  	 (when (and (string= heading target-heading)
  ;; 		  		        (or (null target-level)  ; No level restriction
  ;; 		  		            (= current-level target-level)))  ; Matches specific level
  ;; 		  	   (outline-hide-subtree))))))
  ;; 	 nil 'file))
  ;; (add-hook 'find-file-hook
  ;; 		  	(lambda ()
  ;; 		  	  (when (derived-mode-p 'org-mode)
  ;; 		  		(my/org-hide-matching-headings))))
  ;; AUTO COLLAPSE HEADINGS ON OPEN
  ;; Two ways to have a heading folded when its file opens, at any level:
  ;;   #Name                folded, and hidden from capture pickers / voice
  ;;                        scripts (bookkeeping like #Repeaters, #Archive)
  ;;   :VISIBILITY: folded  (in the heading's property drawer) folded, but the
  ;;                        title stays plain, so capture and automation still
  ;;                        use it (e.g. System)
  (defun my/org-autofold-headings ()
    "Fold headings whose title starts with # or whose VISIBILITY is folded."
    (org-map-entries
     (lambda ()
       (let ((heading (org-get-heading t t t t)))  ; without tags, todo, etc.
         (when (or (and heading (string-prefix-p "#" heading))
                   (equal (org-entry-get nil "VISIBILITY") "folded"))
           (outline-hide-subtree))))
     nil 'file))

  (add-hook 'find-file-hook
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (my/org-autofold-headings))))

  ;; RESET FOLDS TO LAUNCH LAYOUT
  ;; Re-runs the same steps that fold a file when it is opened.
  (defun start/org-reset-folds ()
    "Reset folding in the current Org buffer to the layout it has on open."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (my/org-show-drawers-global)          ; drop stale stealth overlays
        (org-fold-show-all)
        (org-cycle-set-startup-visibility)    ; #+STARTUP / org-startup-folded, drawers
        (my/org-hide-drawers-global)
        (my/org-hide-done-entries-dynamic)
        (my/org-autofold-headings))
      ;; Don't leave point inside hidden text.
      (when (invisible-p (point))
        (outline-previous-visible-heading 1))
      (message "Folds reset to launch layout")))

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

(defun start/org-capture-goto-inbox ()
  "Move point to the Inbox heading in the current buffer.
Creates it above the first heading if it's missing."
  (widen)
  (let ((pos (org-find-exact-headline-in-buffer start/org-inbox-heading nil t)))
    (if pos
        (goto-char pos)
      (goto-char (point-min))
      (if (re-search-forward org-outline-regexp-bol nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max))
        (unless (bolp) (insert "\n")))
      (insert "* " start/org-inbox-heading "\n")
      (forward-line -1))))

(defun start/org-open-inbox ()
  "Open Tasks.org at its Inbox heading."
  (interactive)
  (find-file start/org-tasks-file)
  (start/org-capture-goto-inbox)
  (org-fold-show-entry)
  (org-fold-show-children)
  (recenter 0))

(defun start/org-capture--pick-child (prompt &optional parent-choice)
  "Prompt with PROMPT for a direct child of the heading at point, and go to it.
Children whose titles start with # (like #Repeaters) aren't offered. A name
that doesn't exist yet is added as a new child at the end of the subtree.
If PARENT-CHOICE is a string, it is offered first, and picking it stays on
the current heading."
  (let ((level (org-current-level))
        children)
    (save-excursion
      (when (org-goto-first-child)
        (while (progn
                 (let ((name (string-trim
                              ;; drop statistics cookies like [1/43] or [50%]
                              (replace-regexp-in-string
                               "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" ""
                               (org-get-heading t t t t)))))
                   (unless (string-prefix-p "#" name)
                     (push (cons name (point)) children)))
                 (org-get-next-sibling)))))
    (setq children (nreverse children))
    (let* ((choices (append (and parent-choice (list parent-choice))
                            (mapcar #'car children)))
           (name (string-trim (completing-read prompt choices)))
           (found (assoc name children)))
      (cond ((string-empty-p name) (user-error "Nothing picked"))
            ((equal name parent-choice))  ; stay on the parent heading
            (found (goto-char (cdr found)))
            (t (org-end-of-subtree t t)
               (unless (bolp) (insert "\n"))
               (insert (make-string (1+ level) ?*) " " name "\n")
               (forward-line -1))))))

(defun start/org-capture-goto-sysmgmt-section ()
  "Capture target: a section under the System heading, picked with completion."
  (widen)
  (let ((pos (org-find-exact-headline-in-buffer start/org-sysmgmt-heading nil t)))
    (unless pos
      (user-error "No \"%s\" heading in %s" start/org-sysmgmt-heading (buffer-name)))
    (goto-char pos)
    (start/org-capture--pick-child "System section: ")))

(defun start/org-capture-goto-class ()
  "Capture target: a class under the School heading, picked with completion."
  (widen)
  (let ((school (org-find-exact-headline-in-buffer start/org-school-heading nil t)))
    (unless school
      (user-error "No \"%s\" heading in %s" start/org-school-heading (buffer-name)))
    (goto-char school)
    (start/org-capture--pick-child "Class: ")))

(use-package org-capture
  :ensure nil
  :commands org-capture
  ;; Start typing right away in the capture popup
  :hook (org-capture-mode . evil-insert-state)
  :config
  ;; In normal state: RET saves the capture (C-c C-c), ESC aborts it (C-c C-k)
  (evil-define-minor-mode-key 'normal 'org-capture-mode
    (kbd "RET") #'org-capture-finalize
    (kbd "<return>") #'org-capture-finalize
    (kbd "<escape>") #'org-capture-kill)
  :custom
  (org-capture-templates
   '(("i" "Inbox" entry
      (file+function start/org-tasks-file start/org-capture-goto-inbox)
      "* TODO %?")
     ("s" "System" checkitem
      (file+function start/org-tasks-file start/org-capture-goto-sysmgmt-section)
      "- [ ] %?")
     ("a" "Assignment" entry
      (file+function start/org-tasks-file start/org-capture-goto-class)
      "* ASSIGNMENT %^{Assignment}\nDEADLINE: %^t"
      :immediate-finish t))))

;; Shopping lists (ref:shopping-lists)
;;   start/org-lists-add        SPC l a  add items to a list
;;   start/org-lists-check-off  SPC l x  check items off (C-u: one list)
;;   start/org-lists-bar-out    SPC l o  "the bar is out of X"
;;   start/org-lists-open       SPC l f  open Shopping.org
(require 'cl-lib)

(defvar start/org-lists-default '("Grocery" "General")
  "List used when no list, or \"shopping\"/\"grocery\", is given.")
(defvar start/org-lists-default-names '("shopping" "grocery" "groceries")
  "List names that mean `start/org-lists-default'.")
(defvar start/org-lists-bar-stock '("Bar" "Stock")
  "The bar's stock list, used by `start/org-lists-bar-out'.")
(defvar start/org-lists-fuzzy-threshold 0.88
  "Minimum similarity (0-1, from `string-distance') for a fuzzy item match.")

;;;; File access

(defun start/org-lists--call-with-file (fn)
  "Call FN in an up-to-date, widened buffer of the shopping file, then save.
Reverts first if the file changed on disk. Refuses when the buffer has unsaved
changes and the file also changed on disk, so neither side gets clobbered."
  (let* ((file start/org-shopping-file)
         (buf (or (get-file-buffer file) (find-file-noselect file))))
    (with-current-buffer buf
      (let ((stale (not (verify-visited-file-modtime buf))))
        (when (and stale (buffer-modified-p))
          (user-error "%s has unsaved changes and changed on disk; save or revert it first"
                      (buffer-name)))
        (cond (stale (revert-buffer t t t))
              ((buffer-modified-p) (save-buffer))))
      (prog1 (org-with-wide-buffer (save-excursion (funcall fn)))
        (when (buffer-modified-p)
          ;; No before-save-hook: ws-butler would trim trailing spaces on the
          ;; toggled lines, and the file should change only where we edited.
          (let ((before-save-hook nil) (inhibit-message t))
            (save-buffer)))))))

;;;; Parsing

(defconst start/org-lists--item-re
  "^\\([ \t]*\\)- \\[\\([ X]\\)\\]\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*$"
  "Checkbox item line: 1 indent, 2 box char, 3 text (nil for a blank entry).")

(defun start/org-lists--title ()
  "Title of the heading at point, without a trailing colon."
  (string-trim (replace-regexp-in-string ":\\'" "" (org-get-heading t t t t))))

(defun start/org-lists--all ()
  "Every heading in the buffer as a plist.
Keys: :path (titles from the top), :pos (heading start), :body (start of the
body after planning/drawers), :end (first child or next heading), and :items,
the heading's own top-level checkbox items, each (:box POS :checked BOOL
:text STRING-or-nil :bol POS :eol POS :indent STRING)."
  (let (entries stack)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-outline-regexp-bol nil t)
        (forward-line 0)
        (let* ((level (org-outline-level))
               (title (start/org-lists--title))
               (pos (point))
               (end (save-excursion (or (outline-next-heading) (goto-char (point-max))) (point)))
               (body (save-excursion (org-end-of-meta-data t) (min (point) end)))
               items indent)
          (while (and stack (>= (caar stack) level)) (pop stack))
          (push (cons level title) stack)
          (save-excursion
            (goto-char body)
            (while (re-search-forward start/org-lists--item-re end t)
              (let ((ind (match-string 1)))
                (unless indent (setq indent ind))
                (when (equal ind indent)
                  (push (list :box (match-beginning 2)
                              :checked (equal (match-string 2) "X")
                              :text (let ((text (match-string-no-properties 3)))
                                      (and text (not (string-empty-p text)) text))
                              :bol (match-beginning 0)
                              :eol (match-end 0)
                              :indent ind)
                        items)))))
          (push (list :path (reverse (mapcar #'cdr stack)) :pos pos :body body
                      :end end :items (nreverse items))
                entries)
          (goto-char end))))
    (nreverse entries)))

(defun start/org-lists--path-string (entry)
  (string-join (plist-get entry :path) "/"))

(defun start/org-lists--children (entry entries)
  (let ((path (plist-get entry :path)))
    (seq-filter (lambda (e) (let ((p (plist-get e :path)))
                              (and (= (length p) (1+ (length path)))
                                   (equal (butlast p) path))))
                entries)))

(defun start/org-lists--descendants (entry entries)
  (let ((path (plist-get entry :path)))
    (seq-filter (lambda (e) (let ((p (plist-get e :path)))
                              (and (> (length p) (length path))
                                   (equal (take (length path) p) path))))
                entries)))

;;;; Resolving a list

(defun start/org-lists--resolve (entry entries)
  "Resolve ENTRY to (:items ITEMS :target ENTRY-or-nil :children CHILDREN).
A heading with its own items is its own list. A container uses its General
child, or its only child; with several children it's one combined list of all
its descendants' items, with no single target (new items ask for a child)."
  (let ((children (start/org-lists--children entry entries)))
    (cond
     ((or (plist-get entry :items) (null children))
      (list :items (plist-get entry :items) :target entry))
     ((or (seq-find (lambda (c) (string= (downcase (car (last (plist-get c :path)))) "general"))
                    children)
          (and (null (cdr children)) (car children)))
      (start/org-lists--resolve
       (or (seq-find (lambda (c) (string= (downcase (car (last (plist-get c :path)))) "general"))
                     children)
           (car children))
       entries))
     (t (list :items (mapcan (lambda (e) (copy-sequence (plist-get e :items)))
                             (start/org-lists--descendants entry entries))
              :target nil :children children)))))

(defun start/org-lists--name-key (s)
  "Loose key for a list name: lower-case, no apostrophes, no trailing colon/\"list\"."
  (let* ((s (downcase (string-trim s)))
         (s (replace-regexp-in-string "['’]" "" s))
         (s (replace-regexp-in-string "[/>]" " " s))
         (s (replace-regexp-in-string ":\\'" "" s))
         (s (replace-regexp-in-string "[ \t]+list\\'" "" s)))
    (string-trim (replace-regexp-in-string "[ \t]+" " " s))))

(defun start/org-lists--find-list (name entries)
  "The entry for list NAME: a full path, a title, or \"parent title\"."
  (let ((key (start/org-lists--name-key (or name ""))))
    (if (or (string-empty-p key) (member key start/org-lists-default-names))
        (seq-find (lambda (e) (equal (plist-get e :path) start/org-lists-default)) entries)
      (or (seq-find (lambda (e) (string= (start/org-lists--name-key (start/org-lists--path-string e)) key))
                    entries)
          (seq-find (lambda (e) (string= (start/org-lists--name-key (car (last (plist-get e :path)))) key))
                    entries)
          (seq-find (lambda (e) (let ((p (plist-get e :path)))
                                  (and (cdr p)
                                       (string= (start/org-lists--name-key
                                                 (string-join (last p 2) " "))
                                                key))))
                    entries)))))

(defun start/org-lists--read-list (prompt entries)
  "Read a list name with completion; return its entry."
  (let* ((default (string-join start/org-lists-default "/"))
         (name (completing-read (format-prompt prompt default)
                                (mapcar #'start/org-lists--path-string entries)
                                nil nil nil nil default)))
    (or (start/org-lists--find-list name entries)
        (user-error "No list called \"%s\"" name))))

;;;; Matching items

(defun start/org-lists--singular (word)
  (cond ((<= (length word) 3) word)
        ((string-suffix-p "ies" word) (concat (substring word 0 -3) "y"))
        ((string-match-p "\\(?:oes\\|ses\\|xes\\|ches\\|shes\\)\\'" word) (substring word 0 -2))
        ((string-suffix-p "ss" word) word)
        ((string-suffix-p "s" word) (substring word 0 -1))
        (t word)))

(defun start/org-lists--key (text)
  "Normalized key for item TEXT (see the voice scripts' matching rules)."
  (let* ((s (downcase text))
         (s (replace-regexp-in-string "\\(?:https?://\\|www\\.\\)[^ \t]*" " " s))
         (s (replace-regexp-in-string "&" " and " s))
         (s (replace-regexp-in-string "['’]n\\b" " and" s))
         (s (replace-regexp-in-string "[.'’]" "" s))
         (s (replace-regexp-in-string "[^[:alnum:][:space:]]" " " s))
         (words (split-string s "[[:space:]]+" t)))
    (when (and (cdr words) (member (car words) '("a" "an" "the" "some" "more")))
      (setq words (cdr words)))
    (string-join (mapcar #'start/org-lists--singular words) " ")))

(defun start/org-lists--variants (text)
  "Keys TEXT matches: itself, and for \"x (y)\" also \"y x\" and \"x y\", with
y also tried cut at its first comma."
  (let ((keys (list (start/org-lists--key text))))
    (when (string-match "\\`\\(.*?\\)[ \t]*(\\([^)]*\\))" text)
      (let* ((x (match-string 1 text))
             (y (match-string 2 text))
             (ys (delete-dups (list y (car (split-string y ","))))))
        (dolist (y ys)
          (push (start/org-lists--key (concat y " " x)) keys)
          (push (start/org-lists--key (concat x " " y)) keys))))
    (delete-dups (delete "" keys))))

(defun start/org-lists--similarity (a b)
  (let ((len (max (length a) (length b))))
    (if (zerop len) 1.0 (- 1.0 (/ (float (string-distance a b)) len)))))

(defun start/org-lists--find (items text)
  "Items in ITEMS matching TEXT: every exact match, else the best fuzzy one."
  (let* ((qkeys (start/org-lists--variants text))
         (items (seq-filter (lambda (i) (plist-get i :text)) items)) ; never blanks
         (exact (seq-filter (lambda (i) (seq-intersection
                                         qkeys (start/org-lists--variants (plist-get i :text))))
                            items)))
    (or exact
        (let (best (score 0))
          (dolist (i items)
            (dolist (ik (start/org-lists--variants (plist-get i :text)))
              (dolist (qk qkeys)
                (let ((s (start/org-lists--similarity qk ik)))
                  (when (> s score) (setq score s best i))))))
          (and best (>= score start/org-lists-fuzzy-threshold) (list best))))))

;;;; Editing

(defun start/org-lists--set-checked (item checked)
  "Set ITEM's box to CHECKED by replacing the one box character."
  (save-excursion
    (goto-char (plist-get item :box))
    (delete-char 1)
    (insert (if checked "X" " "))))

(defun start/org-lists--insert-pos (entry)
  "Where a new item goes in ENTRY: below a leading blank, else before the
first item, else at the start of the body."
  (let ((first (car (plist-get entry :items))))
    (cond ((null first) (plist-get entry :body))
          ((null (plist-get first :text))
           (save-excursion (goto-char (plist-get first :eol)) (forward-line 1) (point)))
          (t (plist-get first :bol)))))

(defun start/org-lists--insert-item (pos indent text)
  "Insert \"- [ ] TEXT\" as a whole line at POS; return the position after it."
  (save-excursion
    (goto-char pos)
    (unless (bolp) (insert "\n"))
    (insert indent "- [ ] " text "\n")
    (point)))

(defun start/org-lists--pick-target (res text entries)
  "The entry a new item TEXT goes in, for resolved list RES.
A combined list asks for one of its children, repeating for nested containers."
  (while (not (plist-get res :target))
    (let* ((children (plist-get res :children))
           (name (completing-read (format "Which list for \"%s\": " text)
                                  (mapcar #'start/org-lists--path-string children)
                                  nil t))
           (child (or (seq-find (lambda (c) (string= (start/org-lists--path-string c) name))
                                children)
                      (user-error "No list chosen for \"%s\"" text))))
      (setq res (start/org-lists--resolve child entries))))
  (plist-get res :target))

(defun start/org-lists--split (input)
  (seq-remove #'string-empty-p (mapcar #'string-trim (split-string input ","))))

(defun start/org-lists--report (parts)
  "Message PARTS, an alist of (LABEL . ITEMS), skipping empty ones."
  (let ((msg (mapconcat (lambda (p) (format "%s: %s" (car p) (string-join (cdr p) ", ")))
                        (seq-filter #'cdr parts) " · ")))
    (message "%s" (if (string-empty-p msg) "Nothing changed" msg))
    msg))

;;;; Commands

(defun start/org-lists--item-texts (items)
  (delete-dups (delq nil (mapcar (lambda (i) (plist-get i :text)) items))))

(defun start/org-lists-add (list-name items)
  "Add ITEMS (a comma-separated string) to the list LIST-NAME."
  (interactive
   (start/org-lists--call-with-file
    (lambda ()
      (let* ((entries (start/org-lists--all))
             (entry (start/org-lists--read-list "Add to list" entries))
             (res (start/org-lists--resolve entry entries)))
        (list (start/org-lists--path-string entry)
              (string-join
               (completing-read-multiple
                (format "Add to %s (comma-separated): " (start/org-lists--path-string entry))
                (start/org-lists--item-texts (plist-get res :items)))
               ","))))))
  (start/org-lists--call-with-file
   (lambda ()
     (let* ((entries (start/org-lists--all))
            (entry (or (start/org-lists--find-list list-name entries)
                       (user-error "No list called \"%s\"" list-name)))
            (path (plist-get entry :path))
            added unchecked already next-pos target)
       (dolist (text (start/org-lists--split items))
         ;; Re-parse each time: earlier inserts move positions.
         (let* ((entries (start/org-lists--all))
                (entry (seq-find (lambda (e) (equal (plist-get e :path) path)) entries))
                (res (start/org-lists--resolve entry entries))
                (matches (start/org-lists--find (plist-get res :items) text)))
           (cond
            ((seq-find (lambda (i) (not (plist-get i :checked))) matches)
             (push text already))
            (matches
             (start/org-lists--set-checked (car matches) nil)
             (push (plist-get (car matches) :text) unchecked))
            (t
             (unless target
               (setq target (start/org-lists--pick-target res text entries)))
             (let* ((tentry (seq-find (lambda (e) (equal (plist-get e :path)
                                                         (plist-get target :path)))
                                      entries))
                    (pos (if next-pos (marker-position next-pos)
                           (start/org-lists--insert-pos tentry)))
                    (indent (or (plist-get (car (plist-get tentry :items)) :indent) ""))
                    (after (start/org-lists--insert-item pos indent text)))
               (setq next-pos (copy-marker after t))
               (push text added))))))
       (when next-pos (set-marker next-pos nil))
       (start/org-lists--report
        `((,(format "Added to %s" (if target (start/org-lists--path-string target)
                                    (string-join path "/")))
           . ,(nreverse added))
          ("Unchecked" . ,(nreverse unchecked))
          ("Already on the list" . ,(nreverse already))))))))

(defun start/org-lists-check-off (items &optional list-name)
  "Check off ITEMS (comma-separated) on every list where they're unchecked.
With LIST-NAME (interactively, with \\[universal-argument]), only on that list."
  (interactive
   (start/org-lists--call-with-file
    (lambda ()
      (let* ((entries (start/org-lists--all))
             (entry (and current-prefix-arg (start/org-lists--read-list "Check off on list" entries)))
             (pool (if entry (plist-get (start/org-lists--resolve entry entries) :items)
                     (mapcan (lambda (e) (copy-sequence (plist-get e :items))) entries))))
        (list (string-join
               (completing-read-multiple
                "Check off (comma-separated): "
                (start/org-lists--item-texts
                 (seq-remove (lambda (i) (plist-get i :checked)) pool)))
               ",")
              (and entry (start/org-lists--path-string entry)))))))
  (start/org-lists--call-with-file
   (lambda ()
     (let* ((entries (start/org-lists--all))
            (lists (if list-name
                       (list (plist-get (start/org-lists--resolve
                                         (or (start/org-lists--find-list list-name entries)
                                             (user-error "No list called \"%s\"" list-name))
                                         entries)
                                        :items))
                     (seq-filter #'identity (mapcar (lambda (e) (plist-get e :items)) entries))))
            checked already missing)
       (dolist (text (start/org-lists--split items))
         (let ((matches (mapcan (lambda (items) (start/org-lists--find items text)) lists)))
           (cond
            ((null matches) (push text missing))
            ((seq-every-p (lambda (i) (plist-get i :checked)) matches) (push text already))
            (t (dolist (i matches)
                 (unless (plist-get i :checked)
                   (start/org-lists--set-checked i t)
                   (plist-put i :checked t)
                   (cl-pushnew (plist-get i :text) checked :test #'equal)))))))
       (start/org-lists--report
        `(("Checked off" . ,(nreverse checked))
          ("Already checked" . ,(nreverse already))
          ("Not found" . ,(nreverse missing))))))))

(defun start/org-lists-bar-out (items)
  "The bar is out of ITEMS (comma-separated).
Unchecks each one in the bar stock list, and in the default grocery list only
if it's already there. Never adds an item to either list."
  (interactive
   (start/org-lists--call-with-file
    (lambda ()
      (let* ((entries (start/org-lists--all))
             (stock (start/org-lists--find-list (string-join start/org-lists-bar-stock " ") entries)))
        (list (string-join
               (completing-read-multiple
                "Bar is out of (comma-separated): "
                (start/org-lists--item-texts
                 (plist-get (start/org-lists--resolve stock entries) :items)))
               ","))))))
  (start/org-lists--call-with-file
   (lambda ()
     (let* ((entries (start/org-lists--all))
            (stock (or (seq-find (lambda (e) (equal (plist-get e :path) start/org-lists-bar-stock)) entries)
                       (user-error "No %s list" (string-join start/org-lists-bar-stock "/"))))
            (grocery (seq-find (lambda (e) (equal (plist-get e :path) start/org-lists-default)) entries))
            (stock-items (plist-get (start/org-lists--resolve stock entries) :items))
            (grocery-items (and grocery (plist-get (start/org-lists--resolve grocery entries) :items)))
            out on-grocery missing)
       (dolist (text (start/org-lists--split items))
         (let ((matches (start/org-lists--find stock-items text)))
           (if (null matches)
               (push text missing)
             (dolist (i matches)
               (when (plist-get i :checked)
                 (start/org-lists--set-checked i nil)
                 (plist-put i :checked nil)))
             (push (plist-get (car matches) :text) out)
             (dolist (g (start/org-lists--find grocery-items text))
               (when (plist-get g :checked)
                 (start/org-lists--set-checked g nil)
                 (plist-put g :checked nil))
               (cl-pushnew (plist-get g :text) on-grocery :test #'equal)))))
       (start/org-lists--report
        `(("Out at the bar" . ,(nreverse out))
          (,(format "Also on %s" (string-join start/org-lists-default "/")) . ,(nreverse on-grocery))
          ("Not in bar stock" . ,(nreverse missing))))))))

(defun start/org-lists-open ()
  "Open the shopping lists file."
  (interactive)
  (find-file start/org-shopping-file))

(require 'transient)

(defconst start/voice-properties
  '(("VOICE_NAME") ("VOICE_TYPE" "checklist" "heading") ("VOICE_KEYWORD")
    ("VOICE_DEADLINE" "ask" "optional" "never") ("VOICE_PICK" "child")
    ("VOICE_PLACE" "top" "bottom") ("VOICE_REACTIVATE" "t") ("VOICE_DEFAULT" "t")
    ("VOICE_LINK") ("VOICE" "off"))
  "Voice settings and their allowed values. Keys without values take free text;
VOICE_KEYWORD completes from the buffer's TODO keywords.")

(defvar start/voice-files (list start/org-tasks-file start/org-shopping-file)
  "Org files the voice engine reads. Keep in sync with `files' in .voice.toml.")

(with-eval-after-load 'org
  (dolist (p start/voice-properties)
    (when (cdr p)
      (setf (alist-get (concat (car p) "_ALL") org-global-properties nil nil #'equal)
            (string-join (cdr p) " ")))))

(defvar start/voice--marker nil "Heading the voice menu was opened on.")

(defun start/voice--title ()
  "Spoken title of the heading at point: no keyword, priority, tags, cookie or trailing colon."
  (let ((h (org-get-heading t t t t)))
    (setq h (replace-regexp-in-string "\\[[0-9]*\\(?:%\\|/[0-9]*\\)\\]" "" h))
    (string-trim (replace-regexp-in-string ":[ \t]*\\'" "" (string-trim h)))))

(defun start/voice--path ()
  "Outline path of the heading at point, joined with ›."
  (string-join (append (org-get-outline-path) (list (start/voice--title))) " › "))

(defun start/voice--checklist-p ()
  "Non-nil if the entry at point holds checkbox items."
  (save-excursion
    (org-back-to-heading t)
    (re-search-forward "^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\) \\[[ X-]\\]"
                       (save-excursion (outline-next-heading) (point)) t)))

(defun start/voice--effective (key)
  "Effective value of KEY at point as (VALUE SOURCE MARKER), or nil if unset.
SOURCE is \"here\", \"from <title>\", \"file default\" or \"engine default\"."
  (or (save-excursion
        (org-back-to-heading t)
        (let ((start (point)))
          (cl-loop do (let ((v (org-entry-get nil key)))
                        (when v
                          (cl-return (list v (if (= (point) start) "here"
                                               (concat "from " (start/voice--title)))
                                           (point-marker)))))
                   while (org-up-heading-safe))))
      (let ((v (cdr (assoc key org-keyword-properties))))
        (and v (list v "file default" nil)))
      (let ((v (pcase key
                 ("VOICE_TYPE" (if (start/voice--checklist-p) "checklist" "heading"))
                 ("VOICE_PLACE" "bottom")
                 ("VOICE_KEYWORD" "TODO")
                 ("VOICE_DEADLINE"
                  (if (equal (car (start/voice--effective "VOICE_TYPE")) "checklist")
                      "never" "optional")))))
        (and v (list v "engine default" nil)))))

(defun start/voice--excluded ()
  "Title of the heading that hides point from voice (`#' title or :VOICE: off), or nil."
  (save-excursion
    (org-back-to-heading t)
    (cl-loop do (when (or (string-prefix-p "#" (org-get-heading t t t t))
                          (equal (org-entry-get nil "VOICE") "off"))
                  (cl-return (start/voice--title)))
             while (org-up-heading-safe))))

(defun start/voice--header ()
  "Menu header for the heading the menu was opened on."
  (org-with-point-at start/voice--marker
    (let ((ex (start/voice--excluded)))
      (concat
       (propertize (start/voice--path) 'face 'transient-heading)
       (if ex (propertize (format "   [excluded by %s]" ex) 'face 'error)
         (propertize "   [reachable]" 'face 'success))
       "\n"
       (mapconcat
        (lambda (p)
          (let ((e (start/voice--effective (car p))))
            (format "  %-17s %-28s %s" (car p) (or (car e) "—")
                    (propertize (or (cadr e) "") 'face 'shadow))))
        start/voice-properties "\n")
       "\n"))))

(defun start/voice--headings ()
  "Alist of (\"File › path\" . marker) for every heading in `start/voice-files'."
  (cl-loop for f in start/voice-files
           when (file-exists-p f)
           append (with-current-buffer (find-file-noselect f)
                    (org-with-wide-buffer
                     (org-map-entries
                      (lambda ()
                        (cons (concat (file-name-base f) " › " (start/voice--path))
                              (point-marker)))
                      nil 'file)))))

(defun start/voice--pick-link ()
  "Pick a heading in the voice files and return \"id:<ID>\", creating the ID if needed."
  (let* ((cands (start/voice--headings))
         (m (cdr (assoc (completing-read "Link to heading: " cands nil t) cands))))
    (unless m (user-error "No such heading"))
    (concat "id:" (org-with-point-at m (org-id-get-create)))))

(defun start/voice--read-value (key)
  "Read a value for KEY with completion where it has allowed values."
  (let ((vals (cdr (assoc key start/voice-properties)))
        (cur (org-entry-get nil key)))
    (cond ((equal key "VOICE_LINK") (start/voice--pick-link))
          ((equal key "VOICE_KEYWORD")
           (completing-read (format "%s: " key) org-todo-keywords-1 nil t nil nil cur))
          (vals (completing-read (format "%s: " key) vals nil t nil nil cur))
          (t (read-string (format "%s: " key) cur)))))

(defun start/voice-set (key value)
  "Set voice setting KEY to VALUE on the heading at point."
  (interactive
   (let ((key (completing-read "Voice setting: " (mapcar #'car start/voice-properties) nil t)))
     (list key (start/voice--read-value key))))
  (org-set-property key value))

(defun start/voice-clear (key)
  "Remove voice setting KEY from the heading at point."
  (interactive
   (let ((keys (cl-loop for (k . _) in (org-entry-properties nil 'standard)
                        when (assoc k start/voice-properties) collect k)))
     (unless keys (user-error "No voice settings on this heading"))
     (list (completing-read "Clear: " keys nil t))))
  (org-delete-property key))

(defun start/voice-add-name (name)
  "Add NAME to the spoken aliases (VOICE_NAME) of the heading at point."
  (interactive "sAdd spoken name: ")
  (let ((cur (org-entry-get nil "VOICE_NAME")))
    (org-set-property "VOICE_NAME" (if cur (concat cur ", " name) name))))

(defun start/voice-set-link ()
  "Set VOICE_LINK on the heading at point to another heading's id: link."
  (interactive)
  (org-set-property "VOICE_LINK" (start/voice--pick-link)))

(defun start/voice-toggle-off ()
  "Toggle :VOICE: off on the heading at point."
  (interactive)
  (if (equal (org-entry-get nil "VOICE") "off")
      (org-delete-property "VOICE")
    (org-set-property "VOICE" "off")))

(defun start/voice-jump-source (key)
  "Jump to the heading that sets the effective value of KEY."
  (interactive
   (list (completing-read "Jump to source of: " (mapcar #'car start/voice-properties) nil t)))
  (let ((m (nth 2 (start/voice--effective key))))
    (unless m (user-error "%s is not set on any heading" key))
    (org-goto-marker-or-bmk m)))

(defun start/voice-follow-link ()
  "Go to the heading named by the effective VOICE_LINK."
  (interactive)
  (let ((v (car (start/voice--effective "VOICE_LINK"))))
    (unless (and v (string-prefix-p "id:" v)) (user-error "No VOICE_LINK here"))
    (org-id-goto (substring v 3))))

(defun start/voice-describe ()
  "Show the spoken names of the heading at point and any titles it shares.
TODO: replace with the engine's `describe' output once it exists, so both sides agree."
  (interactive)
  (let* ((title (start/voice--title))
         (aliases (let ((v (org-entry-get nil "VOICE_NAME")))
                    (and v (split-string v "," t "[ \t]+"))))
         (parent (save-excursion (and (org-up-heading-safe) (start/voice--title))))
         (here (save-excursion (org-back-to-heading t) (point-marker)))
         (dups (cl-loop for (name . m) in (start/voice--headings)
                        unless (and (eq (marker-buffer m) (marker-buffer here))
                                    (= m here))
                        when (string-equal-ignore-case
                              (org-with-point-at m (start/voice--title)) title)
                        collect name))
         (path (start/voice--path))
         (type (car (start/voice--effective "VOICE_TYPE")))
         (ex (start/voice--excluded)))
    (with-help-window "*Voice*"
      (princ (format "Path:   %s\n" path))
      (princ (format "Names:  %s\n"
                     (string-join (delete-dups
                                   (delq nil (append (list (downcase title)
                                                           (and parent (downcase (concat parent " " title))))
                                                     (mapcar #'downcase aliases))))
                                  ", ")))
      (princ (format "Type:   %s\n" type))
      (when ex (princ (format "Hidden: by %s\n" ex)))
      (when dups
        (princ "\nSame title elsewhere (say the parent too, or the default target wins):\n")
        (dolist (d dups) (princ (format "  %s\n" d)))))))

(transient-define-prefix start/voice--transient ()
  "Voice rules for the heading at point."
  [:description start/voice--header
   ["Edit"
    ("s" "Set setting" start/voice-set)
    ("c" "Clear setting" start/voice-clear)
    ("n" "Add spoken name" start/voice-add-name)
    ("l" "Link to heading" start/voice-set-link)
    ("o" "Toggle VOICE off" start/voice-toggle-off)]
   ["Go"
    ("j" "Jump to setting source" start/voice-jump-source)
    ("g" "Follow VOICE_LINK" start/voice-follow-link)
    ("d" "Spoken names" start/voice-describe)]])

(defun start/voice-menu ()
  "Open the voice rules menu for the heading at point."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (when (org-before-first-heading-p) (user-error "Not on a heading"))
  (setq start/voice--marker (save-excursion (org-back-to-heading t) (point-marker)))
  (start/voice--transient))

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
    (kbd "L") 'start/evil-end-of-visible-line
    (kbd "^") 'evil-first-non-blank
    (kbd "s") 'flash-emacs-jump

    ;; Quick actions
    (kbd "q") 'org-agenda-quit
    (kbd "<escape>") 'org-agenda-quit
    (kbd "RET") 'org-agenda-switch-to
    (kbd "TAB") 'org-agenda-goto
    (kbd "J") 'org-agenda-goto-date
    ))
;; general's `:major-modes' only scopes which-key labels, not bindings, so the
;; org-mode `SPC o' keys are really global. Keys shared with the agenda
;; dispatch on the current mode instead. Defined at top level so it exists
;; before the `with-eval-after-load' form below is macroexpanded.
(defmacro start/org-or-agenda (org-cmd agenda-cmd)
  "Key definition running AGENDA-CMD in org-agenda, ORG-CMD elsewhere."
  `(general-predicate-dispatch #',org-cmd
     (derived-mode-p 'org-agenda-mode) #',agenda-cmd))

;; Set up leader key bindings for org-agenda
(with-eval-after-load 'org-agenda
  (evil-define-key 'motion org-agenda-mode-map
    (kbd "SPC") nil)  ; Clear SPC first if needed

  ;; Make the org `SPC o' keys work in the agenda too, so e.g. `SPC o t d'
  ;; marks DONE in both places. See `start/org-or-agenda'.
  (start/leader-keys
    "o t" `(:def ,(start/org-or-agenda org-todo org-agenda-todo) :wk "Todo State")
    "o ." `(:def ,(start/org-or-agenda org-set-tags-command org-agenda-set-tags) :wk "Set tags")
    "o T" `(:def ,(start/org-or-agenda org-set-tags-command org-agenda-set-tags) :wk "Set Tags")
    "o p" `(:def ,(start/org-or-agenda org-priority org-agenda-priority) :wk "Priority")
    "o d" `(:def ,(start/org-or-agenda org-deadline org-agenda-deadline) :wk "Deadline")
    "o s" `(:def ,(start/org-or-agenda org-schedule org-agenda-schedule) :wk "Schedule")
    "o r" `(:def ,(start/org-or-agenda org-refile org-agenda-refile) :wk "Refile")
    "o i n" `(:def ,(start/org-or-agenda org-add-note org-agenda-add-note) :wk "Add note")
    "o C i" `(:def ,(start/org-or-agenda org-clock-in org-agenda-clock-in) :wk "Clock in")
    "o C o" `(:def ,(start/org-or-agenda org-clock-out org-agenda-clock-out) :wk "Clock out")
    "o C g" `(:def ,(start/org-or-agenda org-clock-goto org-agenda-clock-goto) :wk "Go to clock"))

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
  (org-modern-todo-faces start/org-todo-faces)

  :hook (org-mode . org-modern-mode)
  )

;; org-modern picks the folded (▶) or expanded (▼) heading star by checking
;; whether the end of the heading line is invisible. The hidden-drawer overlay
;; (see "Hide Properties Drawers") starts there, so every expanded heading
;; with a drawer showed the folded star. Count only real outline folds.
(defun start/org-modern-star-outline-folds-only (orig &rest args)
  "Around `org-modern--star': treat a heading as folded only if its subtree is."
  (cl-letf (((symbol-function 'org-invisible-p)
             (lambda (&optional pos _folding-only)
               (org-fold-folded-p (or pos (point)) 'outline))))
    (apply orig args)))
(with-eval-after-load 'org-modern
  (advice-add 'org-modern--star :around #'start/org-modern-star-outline-folds-only))

(with-eval-after-load 'org-modern
  (custom-set-variables
   '(org-modern-checkbox
     '((?X . "☑")  ; checked
       (?- . "❍")  ; intermediate
       (?\s . "☐")))))  ; unchecked

(defvar start/org-expanded-indicator " ▸"
  "Shown at the end of open headings that have visible content.")

(defun start/org--heading-has-visible-content-p ()
  "Non-nil if the heading at point has body text or children (not just a drawer)."
  (let ((level (org-current-level)))
    (save-excursion
      (org-end-of-meta-data t)
      (skip-chars-forward " \t\n")
      (and (not (eobp))
           (not (and (org-at-heading-p) (<= (org-current-level) level)))))))

(defun start/org--expanded-indicator-matcher (limit)
  "Font-lock matcher: match the newline of an open heading with content.
For a heading whose newline starts a hidden drawer, set or clear the drawer
overlay's `after-string' instead, and keep looking."
  (let (found)
    (while (and (not found) (re-search-forward org-outline-regexp-bol limit t))
      (let* ((bol (match-beginning 0))
             (eol (line-end-position))
             (show (and (< eol (point-max))
                        (not (org-fold-folded-p eol 'outline))
                        (save-excursion (goto-char bol)
                                        (start/org--heading-has-visible-content-p))))
             (drawer (seq-find (lambda (ov) (and (overlay-get ov 'my-stealth-drawer)
                                                 (= (overlay-start ov) eol)))
                               (overlays-at eol))))
        (cond
         (drawer
          (overlay-put drawer 'after-string
                       (and show (propertize start/org-expanded-indicator
                                             'face 'org-ellipsis 'cursor t))))
         ((and show (not (invisible-p eol)))
          (set-match-data (list eol (1+ eol)))
          (setq found t)))
        (goto-char eol)))
    found))

(defun start/org-expanded-indicator-setup ()
  (font-lock-add-keywords
   nil `((start/org--expanded-indicator-matcher
          (0 '(face nil display ,(concat (propertize start/org-expanded-indicator
                                                     'face 'org-ellipsis)
                                         "\n"))
             append)))
   'append)
  (add-to-list (make-local-variable 'font-lock-extra-managed-props) 'display))
(add-hook 'org-mode-hook #'start/org-expanded-indicator-setup 90)

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

;; (use-package org-caldav
;;   :ensure t
;;   :config
;;   (setq org-caldav-url "https://cal.catphish.org"
;;         ;; Use the specific calendar path from Radicale
;;         org-caldav-calendar-id "jordan/5862c1ad-977a-1c79-4053-af22365427d0/"
;;         ;; New events from your phone land here
;;         org-caldav-inbox start/org-inbox-file
;;         ;; Source files to push to the server
;;         org-caldav-files (list start/org-tasks-file start/org-inbox-file)
;;         ;; Keep metadata out of your main git repo
;;         org-caldav-save-directory (expand-file-name "org-caldav-cache/" start/org-root)
;;         ;; Sync on a regular basis (optional)
;;         org-caldav-sync-direction 'twoway
;;         ;; Don't pop up a buffer showing results (silent sync)
;;         org-caldav-show-sync-results nil)

;;   ;; --- 1. THE FINAL FIX FOR PERCENT STATES ---
;;   ;; Every keyword must be in its own dedicated list
;;   (setq org-caldav-todo-percent-states
;;         '((0 "TODO") (0 "ASSIGNMENT") (0 "BILL") (0 "CHORE")
;;           (0 "MEETING") (0 "NEXT") (0 "PLANNING") (0 "REVIEW")
;;           (0 "HOLD") (0 "READY") (0 "ACTIVE")
;;           (100 "DONE") (100 "CANCELED")))

;;   ;; --- 2. EXPORT TODOs AS CALENDAR EVENTS ---
;;   (setq org-caldav-sync-todo nil) ;; Disables VTODO task generation
;;   (setq org-icalendar-include-todo t) ;; Allows the exporter to read TODO lines
;;   (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo)) ;; Pushes deadlines to calendar grid
;;   (setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo)) ;; Pushes schedules to calendar grid

;;   ;; Setup automatic sync (runs every 1 hour when idle)
;;   ;; Note: This will momentarily freeze Emacs while syncing
;;                                         ; (run-with-idle-timer 3600 t 'org-caldav-sync)

;;   ;; This ensures Emacs uses your GPG key to read the password
;;   (setq auth-sources '("~/.authinfo.gpg"))
;;   (setq org-cycle-hide-drawers t)
;;   )

(use-package ox-icalendar
  :ensure nil ; Built-in with Org mode, no package download needed
  :config
  ;; Remove the "DL: " and "S: " prefixes from event titles
  (setq org-icalendar-deadline-summary-prefix ""
        org-icalendar-scheduled-summary-prefix "")

  ;; Where you want the local .ics file to live
  (setq org-icalendar-combined-agenda-file (expand-file-name "calendars/Org-Mode.ics" start/org-root)

        ;; Map your TODO deadlines and scheduled dates directly to calendar events (VEVENT)
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo)
        org-icalendar-use-scheduled '(event-if-todo event-if-not-todo)

        ;; Keep them as clean calendar events rather than separate task lists
        org-icalendar-include-todo nil)

  ;; Automatically regenerate the local .ics file only when saving Tasks.org
  (add-hook 'after-save-hook
            (lambda ()
              (when (equal (buffer-file-name) start/org-tasks-file)
                (org-icalendar-combine-agenda-files)))))

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

(defvar start/org-roam-db-sync-idle-seconds 120
  "Idle seconds before `start/org-roam-db-sync-quietly' runs.")

(defvar start/org-roam-db-sync-timer nil
  "Idle timer that periodically runs `org-roam-db-sync'.")

(defun start/org-roam-db-sync-quietly ()
  "Run `org-roam-db-sync' without messages; demote errors to warnings.
Loads org-roam if needed, since `:bind' defers it until first use."
  (let ((inhibit-message t)
        (message-log-max nil))
    (with-demoted-errors "org-roam periodic sync: %S"
      (when (and (require 'org-roam nil t)
                 (file-directory-p org-roam-directory))
        (org-roam-db-sync)))))

(when (timerp start/org-roam-db-sync-timer)
  (cancel-timer start/org-roam-db-sync-timer))
(setq start/org-roam-db-sync-timer
      (run-with-idle-timer start/org-roam-db-sync-idle-seconds t
                           #'start/org-roam-db-sync-quietly))

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

  ;; Only C-n/C-p select candidates. The arrow keys close the popup and move
  ;; the cursor. In insert state, evil-collection's corfu bindings (an evil
  ;; keymap for corfu-map, which wins over corfu-map itself) bind <down>/<up>,
  ;; C-j/C-k and M-j/M-k to corfu-next/previous, so override those there; nil
  ;; lets C-j/C-k/M-j/M-k fall through to their usual insert-state commands.
  (defun start/corfu-quit-and-next-line ()
    "Close the Corfu popup and move down a line."
    (interactive)
    (corfu-quit)
    (call-interactively #'next-line))
  (defun start/corfu-quit-and-previous-line ()
    "Close the Corfu popup and move up a line."
    (interactive)
    (corfu-quit)
    (call-interactively #'previous-line))
  (keymap-set corfu-map "<down>" #'start/corfu-quit-and-next-line)
  (keymap-set corfu-map "<up>" #'start/corfu-quit-and-previous-line)
  (defun start/corfu-evil-keys (&rest _)
    "Override evil-collection's insert-state candidate keys in corfu-map."
    (evil-define-key 'insert corfu-map
      (kbd "<down>") #'start/corfu-quit-and-next-line
      (kbd "<up>") #'start/corfu-quit-and-previous-line
      (kbd "C-j") nil (kbd "C-k") nil
      (kbd "M-j") nil (kbd "M-k") nil))
  ;; evil-collection sets its bindings when corfu loads, which may be after
  ;; this; reapply ours after its setup either way.
  (advice-add 'evil-collection-corfu-setup :after #'start/corfu-evil-keys)
  (with-eval-after-load 'evil (start/corfu-evil-keys))
  (keymap-set corfu-map "M-n" #'ignore)
  (keymap-set corfu-map "M-p" #'ignore)

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

(use-package evil-fringe-mark
  :after evil
  :custom
  (evil-fringe-mark-side 'right-fringe)
  (evil-fringe-mark-show-special nil) ;; hide automatic marks like . ^ < >
  :config
  (global-evil-fringe-mark-mode 1))

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

(use-package rainbow-mode
:ensure t
:hook (org-mode
        emacs-lisp-mode
        css-mode
        conf-mode))

(use-package pdf-tools
    :ensure nil ;; provided by Nix, see above
    :if (locate-library "pdf-tools")
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :config
    ;; Initialize pdf-tools
    (pdf-tools-install)

    ;; Use 'Midnight Mode' by default (Dark mode for PDFs)
    ;; You can toggle this with 'C-c C-r'
    (setq-default pdf-view-midnight-colors '("#abb2bf" . "#282c34")) ; One Dark colors

    ;; Better rendering
    (setq pdf-view-use-scaling t
            pdf-view-use-imagemagick nil)

    ;; Setup Evil bindings for PDF view
    (with-eval-after-load 'evil
        (evil-set-initial-state 'pdf-view-mode 'motion)))

(use-package super-save
:disabled t
:ensure t
:custom
(super-save-auto-save-idle t) ;; Ensure idle saving is explicitly ON
;; Save after 5 seconds of idle time
(super-save-idle-duration 5)
;; Don't save remote files (TRAMP) to avoid lag
(super-save-remote-files nil)
:config
(super-save-mode +1)
(setq super-save-silent nil))

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
  ; :init (ws-butler-global-mode))
    :config (ws-butler-global-mode))

;; Collect garbage when idle instead of mid-command.
(use-package gcmh
  :config
  (setq gcmh-idle-delay 'auto              ;; idle delay scales with how long the last GC took
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 256 1024 1024))
  (gcmh-mode 1))
;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb
