;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Keybinding Help

(use-package which-key
  :config (which-key-mode))


;;------------------------------------------------------------
;; Window Management

(use-package ace-window
  :bind ("M-o" . ace-window)
  ;:bind ("M-O" . ace-delete-window)
  :custom
  (aw-scope 'frame)                        ; Limit to curr frame
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ; Use: a s d f g h j k l
  ; (aw-dispatch-always nil)
  )


;;------------------------------------------------------------
;; Visual Focus

(use-package dimmer
  :custom
  (dimmer-fraction -0.08)
  ;(dimmer-adjustment-mode :both)
  (dimmer-adjustment-mode :background)
  ;(dimmer-use-colorspace :rgb)
  (dimmer-use-colorspace :cielab)  ; Better color blending ???
  (dimmer-buffer-exclusion-regexps '("^ \\*Minibuf-[0-9]+\\*$"
                                     "^ \\*Echo Area"))
  :config
  ;; Don't dim:
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-configure-org)
  (dimmer-mode t))


;;------------------------------------------------------------
;; Restart Emacs

(use-package restart-emacs
  :bind ("C-c q" . restart-emacs))


;;------------------------------------------------------------
(provide 'init-utils)
