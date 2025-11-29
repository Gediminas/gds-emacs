;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Better scrolling ???

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)


;;------------------------------------------------------------
;; Try

(repeat-mode 1)   ; C-x o o o...

(recentf-mode 1)  ; track recent files:

(winner-mode 1)   ; Undo/redo windows

;;------------------------------------------------------------
;; Try


;; Vertical completion UI
(use-package vertico
  ;;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))


(savehist-mode 1)  ; remembers your buffer order:
;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))


;; Flexible matching
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring


;; Rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;; Enhanced commands with preview
(use-package consult
  :ensure t
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("C-x C-r" . consult-recent-file)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s l" . consult-line)                  ;; Alternative: rebind C-s to consult-line
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)))


;;------------------------------------------------------------
;; Try

(global-set-key (kbd "C-;") 'mode-line-other-buffer)  ; Toggle last buffer

;; https://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
;; - mapping C-; to other-window (cycling through windows)
;; - mapping C-' to other-frame (cycling through frames). I actually started using frames a lot more after adding this shortcut).
;; - throwing a buffer to the other window (in a 2-window set-up), toggling between the current buffer and last buffer in a given window, burying the other buffer (this one is great for replacing the help window that pops up (when reading man pages, apropos, etc) with whatever window was previously visible).

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


;; (use-package iflipb

;;   )

;; (global-set-key (kbd "C-'") 'iflipb-next-buffer)
;;   ;; (global-set-key
;;   ;;  (if (featurep 'xemacs) (kbd "<C-iso-left-tab>") (kbd "<C-S-iso-lefttab>"))
;;   ;;  'iflipb-previous-buffer)

(global-set-key (kbd "C-'") 'tab-bar-switch-to-tab)  ; Toggle last buffer



(global-set-key (kbd "<XF86Paste>") 'yank)


(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<down>"))


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

;; (use-package mwe)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

;(setq auto-revert-use-notify t)
;(global-auto-revert-mode 1)

(use-package keycast
  :config
  (keycast-header-line-mode))



(use-package disable-mouse
  :config
  ;; (mapc #'disable-mouse-in-keymap
  ;; (list evil-motion-state-map
  ;;       evil-normal-state-map
  ;;       evil-visual-state-map
  ;;       evil-insert-state-map))
  (global-disable-mouse-mode))



;; (orgdungeon-mode)

;;------------------------------------------------------------
(provide 'init-experiments)
