;;; -*- lexical-binding: t; -*-

;; Vertical completion UI
(use-package vertico
  :config
   (vertico-mode))


;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  (context-menu-mode t)            ;;vertico-multiform-mode adds a menu in the minibuffer to switch display modes.
  (enable-recursive-minibuffers t) ;; Support opening new minibuffers from inside existing minibuffers.

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
  :init
  (marginalia-mode))


;; Enhanced commands with preview
(use-package consult
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x b"   . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("C-x C-r" . consult-recent-file)
         ;; M-g bindings (goto-map)
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s l"   . consult-line)                  ;; Alternative: rebind C-s to consult-line
         ("M-s g"   . consult-grep)
         ("M-s r"   . consult-ripgrep))
)

;;------------------------------------------------------------
(provide 'init-vertico)
