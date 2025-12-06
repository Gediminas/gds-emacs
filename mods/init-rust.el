;; (use-package company-mode
;;   :init
;;   (global-company-mode))

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;; ; https://arne.me/blog/emacs-config-from-scratch-part-three#fnref-1
;; (use-package exec-path-from-shell
;;   :init
;;   (exec-path-from-shell-initialize))


(use-package emacs
  ;; :hook (zig-mode . eglot-ensure)
  :hook (rust-mode . eglot-ensure)
  ;; :hook (go-mode . eglot-ensure)
  ;; :hook (typescript-mode . eglot-ensure)
  ;; :general
  ;; (leader-keys
  ;;   "l" '(:ignore t :which-key "lsp")
  ;;   "l <escape>" '(keyboard-escape-quit :which-key t)
  ;;   "l r" '(eglot-rename :which-key "rename")
  ;;   "l a" '(eglot-code-actions :which-key "code actions"))
  )
;;--  (add-hook 'rust-mode-hook 'eglot-ensure)

(setq lsp-rust-analyzer-server-display-inlay-hints t)

(setq treesit-language-source-alist
      '((rust "https://github.com/tree-sitter/tree-sitter-rust")))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))




;;  (use-package tree-sitter)

;; (use-package tree-sitter-langs
;;   :after tree-sitter)

(use-package yasnippet)

;; (use-package ron-mode)

;; (use-package cargo)

;; ;; (use-package flycheck-rust
;;   ;; :after flycheck)

;; ;; (use-package rust-mode
;; ;;   :config
;; ;;   (add-to-list 'super-save-predicates
;; ;;                (lambda () (not (eq major-mode 'rust-mode))))

;; ;;   (defun init-rust-mode-defaults ()
;; ;;     ;; format on save
;; ;;     (setq rust-format-on-save t)

;; ;;     ;; lsp settings
;; ;;     (setq
;; ;;      ;; enable macro expansion
;; ;;      lsp-rust-analyzer-proc-macro-enable t
;; ;;      lsp-rust-analyzer-experimental-proc-attr-macros t)

;; ;;     ;; Prevent #! from chmodding rust files to be executable
;; ;;     (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ;;     ;; snippets are required for correct lsp autocompletions
;; ;;     (yas-minor-mode)

;; ;;     ;; CamelCase aware editing operations
;; ;;     ;; (subword-mode +1))

;; ;;   ;; (setq init-mode-hook 'init-rust-mode-defaults)

;; ;;   :hook ((rust-mode . cargo-minor-mode)
;; ;;          (rust-mode . lsp)
;; ;;          (rust-mode . tree-sitter-mode)
;; ;;          (rust-mode . tree-sitter-hl-mode)
;; ;;          ;; (rust-mode . (lambda () (run-hooks 'init-rust-mode-hook)))
;; ;;          ;; (flycheck-mode . flycheck-rust-setup)
;; ;; 	 )
;; ;;   )

;; (use-package rust-mode
;; :ensure t
;; :mode ("\\.rs\\'" . rust-mode)) ;;
;; (setq eglot-server-programs '((c-mode . ("clangd"))
;; (c++-mode . ("clangd"))
;; (c-ts-mode . ("clangd"))
;; (c++-ts-mode . ("clangd"))
;; (rust-mode . ("rust-analyzer"))
;; (rust-ts-mode . ("rust-analyzer"))
;; ))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'c-ts-mode-hook 'eglot-ensure)
;; (add-hook 'c++-ts-mode-hook 'eglot-ensure)
;; (add-hook 'rust-mode-hook 'eglot-ensure)
;; (add-hook 'rust-ts-mode-hook 'eglot-ensure)
;; (setq eglot-autoreconnect t)
;; (setq eglot-autoshutdown-timeout 10)


(provide 'init-rust)

