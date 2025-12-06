;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Packaging

(require 'use-package)             ; Required when used with the line below 
(require 'use-package-ensure)      ; ???
(setq use-package-always-ensure t) ; Auto-install packages (no need for ":ensure t")

(package-initialize);              ; Might called by `use-package` but required by the line below
(unless package-archive-contents
  (package-refresh-contents))      ; Might be useful for non `use-package` installs
 

;;------------------------------------------------------------
;; Basic Settings

(setq next-screen-context-lines 5      ; PageUp/Down margins (default 2)
      use-short-answers t              ; y/n instead of yes/no
      sentence-end-double-space nil)   ; Sentence ends with 1 space

(global-display-line-numbers-mode 1)   ; Line numbers
(global-hl-line-mode t)                ; Highlight current line
(column-number-mode 1)                 ; Show column in modeline
(desktop-save-mode 1)                  ; Restore session on restart


;;------------------------------------------------------------
;; Modules
 
(add-to-list 'load-path (expand-file-name "mods" user-emacs-directory))

(require 'init-utils)
(require 'init-theme)
(require 'init-org)
(require 'init-rust)
(require 'init-training)
(require 'init-experiments)


;;------------------------------------------------------------
;; Custom (auto-generated)
 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
