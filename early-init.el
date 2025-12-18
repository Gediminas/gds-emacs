;;; -*- lexical-binding: t; -*-

;; Prevent flash of white
;; https://www.reddit.com/r/emacs/comments/1bfk7mj/how_to_disable_the_startup_blinding_white/
(setq default-frame-alist '(
  (background-color . "#202035")
  (foreground-color . "#999999")
  (ns-appearance . dark)
  (ns-transparent-titlebar . t)))

;; Performance 
(setq gc-cons-threshold (* 100 1000 1000))   ; 0.8mb->100mb garbage collection threshold (faster startup)
(setq read-process-output-max (* 1024 1024)) ; 64KB-->1MB
;(setq debug-on-error t)                     ; Annoying; Use when debugging to show stack trace on errors`


; https://arne.me/blog/emacs-from-scratch-part-one-foundations
; If you start Emacs now, you’ll see the GUI elements for a few milliseconds. Fix:
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; UI elements
(scroll-bar-mode -1)              ; Hide scrollbars (must be before tool-bar-mode)
(tool-bar-mode -1)                ; Hide toolbar
(menu-bar-mode -1)                ; Hide menu bar
(blink-cursor-mode -1)            ; Disable cursor blink
(setq inhibit-splash-screen t)    ; Hide emacs page (moved here to also hide it on error)

; Tab-bar
; (setq tab-bar-show 1)                 ; Hide bar if <= 1 tabs open
(setq tab-bar-show t)                 ; Show always
(setq tab-bar-close-button-show nil)  ; Hide tab close / X button
(setq tab-bar-tab-hints t)            ; Show tab numbers
(tab-bar-mode 1)                      ; Enable tab bar

;; C-c r - Reload cofig
(defun reload-init-file ()
  "Save current buffer & Reload init.el"
  (interactive)
  (save-buffer)
  (package-refresh-contents)
  ;; Unload all init-* features
  (mapc (lambda (feature)
      (when (string-match-p "^init-" (symbol-name feature))
        (unload-feature feature t))) features)
  (load-file user-init-file))

;; C-c r - Reload cofig
(defun reload-init-file ()
  "Save current buffer & Reload init.el"
  (interactive)
  (save-buffer)
  ;; Unload all init-* features
  (mapc (lambda (feature)
      (when (string-match-p "^init-" (symbol-name feature))
        (unload-feature feature t))) features)
  (load-file user-init-file))

(global-set-key (kbd "C-c r r") #'reload-init-file)
(global-set-key (kbd "C-c r e")   #'package-refresh-contents)

;; Package system setup
(setq package-archives '(
  ("melpa" . "https://melpa.org/packages/")
  ("org"   . "https://orgmode.org/elpa/")
  ("elpa"  . "https://elpa.gnu.org/packages/")))
;; (push '("melpa" . "https://melpa.org/packages/") package-archives)
