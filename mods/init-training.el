;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Always open file in read-only mode

(add-hook 'find-file-hook (lambda () (setq buffer-read-only t)))


;;------------------------------------------------------------
;; Disable arrows to force learning C-f/b/n/p/s/...

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<down>"))


;;------------------------------------------------------------
;; Disable mouse

; (use-package disable-mouse
;   :config
;   ;; (mapc #'disable-mouse-in-keymap
;   ;; (list evil-motion-state-map
;   ;;       evil-normal-state-map
;   ;;       evil-visual-state-map
;   ;;       evil-insert-state-map))
;   (global-disable-mouse-mode))


;;------------------------------------------------------------
;; SpaceFn

(global-set-key (kbd "<XF86Paste>") 'yank)


;;------------------------------------------------------------
;; Show last command

(use-package keycast
  :config
  (keycast-tab-bar-mode))

;;------------------------------------------------------------
;; Auto-update command history (lossage) buffer

(defun update-lossage-buffer ()
  "Update the \"Lossage\" buffer.
For this to work, visit the lossage buffer, and call
M-x rename-buffer Lossage RET"
  (save-excursion
    (let ((b (get-buffer "Lossage")))
      (when (buffer-live-p b)
        (with-current-buffer b
          (revert-buffer nil 'noconfirm))))))
(add-hook 'post-command-hook #'update-lossage-buffer nil 'local)


;;------------------------------------------------------------
;; Command frequency

(use-package keyfreq
  :config
  ;; (setq keyfreq-excluded-commands
  ;; 	'(self-insert-command
  ;;         forward-char
  ;;         backward-char
  ;;         previous-line
  ;;         next-line))
  (keyfreq-mode 1)		    ;
  (keyfreq-autosave-mode 1)         ;
  )


;;------------------------------------------------------------
(provide 'init-training)
