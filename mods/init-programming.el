;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Line numbers

; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
; (add-hook 'text-mode-hook #'display-line-numbers-mode)
; u

; (global-display-line-numbers-mode 1)   ; Line numbers (conflicts with pdf-tools)
(use-package display-line-numbers
  :ensure nil  ; Built-in, no install needed
  :hook
  ((prog-mode . display-line-numbers-mode)
   (text-mode . display-line-numbers-mode)
   (conf-mode . display-line-numbers-mode))
  :config
  (setq display-line-numbers-type 'relative))  ; Relative line numbers


 
;;------------------------------------------------------------
;; Git

(use-package magit)


;;------------------------------------------------------------
;; Auto-completion

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )


;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))


;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c z" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;;------------------------------------------------------------
;; Other tools
 
(use-package yasnippet)


;;------------------------------------------------------------
;; Grammar/syntax detection

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (use-package tree-sitter)

;; (use-package tree-sitter-langs
;;   :after tree-sitter)
 

;;------------------------------------------------------------
;; LSP

(use-package eglot
  :custom
  (eglot-inlay-hints-mode nil)
  :bind
  (("C-c t h" . eglot-inlay-hints-mode)))

(use-package consult-eglot)

(use-package emacs
  :hook (rust-ts-mode . eglot-ensure)
  :hook (go-mode . eglot-ensure)
  :hook (zig-mode . eglot-ensure)
  ;; :general
  ;; (leader-keys
  ;;   "l" '(:ignore t :which-key "lsp")
  ;;   "l <escape>" '(keyboard-escape-quit :which-key t)
  ;;   "l r" '(eglot-rename :which-key "rename")
  ;;   "l a" '(eglot-code-actions :which-key "code actions"))
  )

;;------------------------------------------------------------
;; Shell

;; ; https://arne.me/blog/emacs-config-from-scratch-part-three#fnref-1
;; (use-package exec-path-from-shell
;;   :init
;;   (exec-path-from-shell-initialize))


;;------------------------------------------------------------
;; Rust

; (setq treesit-language-source-alist
;       '((rust "https://github.com/tree-sitter/tree-sitter-rust")))

; Rust-LSP
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(setq lsp-rust-analyzer-server-display-inlay-hints t)

; Rust-Build
(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t)                    ; Autoscroll while compiling
  (setq cargo-process--command-flags "--color never"))  ; Fix output on NixOS (https://github.com/kwrooijen/cargo.el/pull/46#issuecomment-390115500)

;; (use-package ron-mode)
;; (use-package cargo)
;; (use-package flycheck-rust
;; :after flycheck)


;;------------------------------------------------------------
;; Markdown

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;------------------------------------------------------------
;; Others

(use-package just-mode
  :defer t)


;;------------------------------------------------------------
(provide 'init-programming)
