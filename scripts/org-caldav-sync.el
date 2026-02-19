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
      
      org-caldav-calendars
      '((:calendar-id "jordan/290ea202-4add-a1fe-3fa8-1cff0f4136df/"
                      :files ("/org/Tasks.org" "/org/Inbox.org")
                      :inbox "/org/Inbox.org"
                      ;; Main Calendar: Skip specialized items (Assignment, Bill, etc)
                      :skip-conditions (regexp "^\\*+ .*\\(ASSIGNMENT\\|BILL\\|CHORE\\|MEETING\\|PLANNING\\|REVIEW\\|HOLD\\|READY\\|ACTIVE\\)"))
        (:calendar-id "jordan/332a8323-d8e5-f8f7-8295-1ae6eb82c412/"
                      :files ("/org/Tasks.org")
                      :inbox "/org/Inbox.org"
                      ;; Assignment Cal: Skip if NOT Assignment
                      :skip-conditions (notregexp "^\\*+ .*ASSIGNMENT"))
        (:calendar-id "jordan/4e3147bb-a02a-4dea-b1e1-c182ccaa2eef/"
                      :files ("/org/Tasks.org")
                      :inbox "/org/Inbox.org"
                      ;; Bill Cal: Skip if NOT Bill
                      :skip-conditions (notregexp "^\\*+ .*BILL"))
        (:calendar-id "jordan/a20ab2df-50e1-04a5-539e-70133159c660/"
                      :files ("/org/Tasks.org")
                      :inbox "/org/Inbox.org"
                      ;; Chore Cal: Skip if NOT Chore
                      :skip-conditions (notregexp "^\\*+ .*CHORE"))
        (:calendar-id "jordan/e57a627d-83a4-b64a-a0a0-974c1e4d1708/"
                      :files ("/org/Tasks.org")
                      :inbox "/org/Inbox.org"
                      ;; Meeting Cal: Skip if NOT Meeting
                      :skip-conditions (notregexp "^\\*+ .*MEETING"))
        (:calendar-id "jordan/9fbe7921-94f8-3406-cfa1-af0233228ecd/"
                      :files ("/org/Tasks.org")
                      :inbox "/org/Inbox.org"
                      ;; Planning Cal: Skip if NOT Planning/Review/etc
                      :skip-conditions (notregexp "^\\*+ .*\\(PLANNING\\|REVIEW\\|HOLD\\|READY\\|ACTIVE\\)")))

      ;; Fix the "DL: DL:" issue
      org-icalendar-deadline-summary-prefix ""
      org-icalendar-scheduled-summary-prefix ""
      
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
