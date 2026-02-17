;; this is a fix for weird window sizing when launching on
;; niri, may not be necessary on other systems
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Force the frame to be maximized on startup to fill the Niri window
(add-to-list 'default-frame-alist '(fullscreen . maximized))
