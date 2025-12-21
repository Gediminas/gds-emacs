;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Packaging

(require 'use-package)             ; Required when used with the line below 
(require 'use-package-ensure)      ; ???
(setq use-package-always-ensure t) ; Auto-install packages (no need for ":ensure t")
; (setq use-package-always-defer t)  ; always :defer t for lazy loading:

(package-initialize);              ; Might called by `use-package` but required by the line below
(unless package-archive-contents
  (package-refresh-contents))      ; Might be useful for non `use-package` installs
 

;;------------------------------------------------------------
;; Basic Settings

(use-package emacs
  :init
  ;; https://arne.me/blog/emacs-from-scratch-part-one-foundations
  ;; (setq initial-scratch-message nil)
  (setq-default indent-tabs-mode nil) ; Use spaces
  (setq-default tab-width 2)         ; tab-width for modes that use tabs (Go) 
  (defalias 'yes-or-no-p 'y-or-n-p)  ; y/n in comfirmation dialogs

  (setq next-screen-context-lines 5      ; PageUp/Down margins (default 2)
        use-short-answers t              ; y/n instead of yes/no
        sentence-end-double-space nil)   ; Sentence ends with 1 space

  (global-hl-line-mode t)                ; Highlight current line
  (column-number-mode 1)                 ; Show column in modeline
  (recentf-mode 1)                       ; track recent files:
  (savehist-mode 1)                      ; Remembers your buffer order
  (repeat-mode 1)                        ; C-x o o o...
  (winner-mode 1)                        ; Undo/redo windows
  ;(global-tab-line-mode t)
  (setq backup-directory-alist `(("." . "~/.saves"))) ;; Store backup files in ~/.saves
  (setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI

  ;(setq auto-revert-use-notify t)
  ;(global-auto-revert-mode 1)
)

;;------------------------------------------------------------
;; Modules
 
(add-to-list 'load-path (expand-file-name "mods" user-emacs-directory))

(require 'init-theme)
(require 'init-utils)
(require 'init-vertico)
(require 'init-tabs)
(require 'init-org)
(require 'init-reading)
(require 'init-programming)
(require 'init-training)
(require 'init-experiments)

(server-start)

; (use-package emacs
;   :init
;   (setq desktop-path '("~/.config/emacs/persist/"))
;   (desktop-save-mode 1)                  ; Restore session on restart
; )

;;------------------------------------------------------------
;; Custom (auto-generated)
 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
