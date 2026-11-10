;;; early-init.el --- Early startup settings -*- lexical-binding: nil; -*-

;; this is a fix for weird window sizing when launching on
;; niri, may not be necessary on other systems
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Window size is left to the window manager. On niri, the window rule
;; matching app-id "^[Ee]macs$" opens it as a full-width column.
;; (Forcing `(fullscreen . maximized)' here makes the native Wayland
;; build request a true maximize, which niri shows edge-to-edge with no
;; focus ring.)
