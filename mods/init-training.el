;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Disable arrows to force learning C-f/b/n/p/s/...

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<down>"))


;;------------------------------------------------------------
;; Disable mouse

(use-package disable-mouse
  :config
  ;; (mapc #'disable-mouse-in-keymap
  ;; (list evil-motion-state-map
  ;;       evil-normal-state-map
  ;;       evil-visual-state-map
  ;;       evil-insert-state-map))
  (global-disable-mouse-mode))


;;------------------------------------------------------------
;; SpaceFn

(global-set-key (kbd "<XF86Paste>") 'yank)


;;------------------------------------------------------------
(provide 'init-training)
