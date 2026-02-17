;;; org-caldav-sync.el --- Script to sync org-caldav in batch mode

;; Usage in Docker:
;; emacs --batch --load /path/to/org-caldav-sync.el
;;
;; Required mounts in Docker:
;; - /path/to/your/org-files  -> /org
;; - /path/to/your/emacs-conf -> /root/.emacs.d (or similar, for cache)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install org-caldav if not present
(unless (package-installed-p 'org-caldav)
  (package-refresh-contents)
  (package-install 'org-caldav))

(require 'org-caldav)

;; --- Configuration (Copied from init.org) ---

;; Update these paths to match your Docker container mounts!
(setq org-caldav-url "https://cal.catphish.org"
      org-caldav-calendar-id "jordan/7852d29b-8d80-2f1d-cb53-2e30f8db93a4/"
      
      ;; In Docker, map your org folder to /org or similar
      org-caldav-inbox "/org/Inbox.org"
      org-caldav-files '("/org/Tasks.org" "/org/Inbox.org")
      
      ;; Cache directory inside the container
      org-caldav-save-directory "/root/.emacs.d/org-caldav-cache/"
      
      org-caldav-sync-direction 'twoway
      org-caldav-todo-percent-states '((0 "TODO" "ASSIGNMENT" "BILL" "CHORE" "MEETING" "NEXT" "PLANNING" "REVIEW" "HOLD" "READY" "ACTIVE")
                                       (100 "DONE" "CANCELED"))
      
      ;; OAuth/Auth handling
      ;; If using .authinfo.gpg, you need gpg and the key in the container.
      ;; For a headless server, plain .authinfo is easier if secure, 
      ;; or set user/password variables directly if you accept the risk.
      ;; (setq org-caldav-oauth2-client-id "...")
      ;; (setq org-caldav-oauth2-client-secret "...")
      )

;; If using auth-source (recommended):
;; Ensure /root/.authinfo exists with:
;; machine cal.catphish.org login YOUR_USER password YOUR_PASSWORD port https
(setq auth-sources '("/root/.authinfo"))

;; --- Execution ---

(message "Starting Org-CalDav Sync...")
(condition-case err
    (progn
      (org-caldav-sync)
      (message "Sync completed successfully."))
  (error
   (message "Sync failed: %s" err)
   (kill-emacs 1)))
