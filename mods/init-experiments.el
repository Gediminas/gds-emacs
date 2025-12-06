;;; -*- lexical-binding: t; -*-

;; Vertical completion UI
(use-package vertico
  ;;:custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

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
         ("M-s r"   . consult-ripgrep)))


;;------------------------------------------------------------
;; Try

(global-set-key (kbd "C-;") 'mode-line-other-buffer)  ; Toggle last buffer

;; https://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
;; - mapping C-; to other-window (cycling through windows)
;; - mapping C-' to other-frame (cycling through frames). I actually started using frames a lot more after adding this shortcut).
;; - throwing a buffer to the other window (in a 2-window set-up), toggling between the current buffer and last buffer in a given window, burying the other buffer (this one is great for replacing the help window that pops up (when reading man pages, apropos, etc) with whatever window was previously visible).

;;------------------------------------------------------------
;; 
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
;; 

(global-set-key (kbd "C-'") 'tabspaces-switch-or-create-workspace)  ; Toggle last buffer


;;------------------------------------------------------------
;; 

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
;; 
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

;;------------------------------------------------------------
;; 
;(setq auto-revert-use-notify t)
;(global-auto-revert-mode 1)

;;------------------------------------------------------------
;; 

(use-package keycast
  :config
  (keycast-header-line-mode))

;;------------------------------------------------------------
;; 

;; current line color
(set-cursor-color "#ffff00") 
;; (set-face-background 'hl-line "#3e4446")
(set-face-background 'hl-line "#503050")
(set-face-foreground 'hl-line nil)
(set-face-foreground 'highlight nil) ;???
(set-face-attribute 'region nil :background "#444499")

;;------------------------------------------------------------
;; 
;; 80 char limit
(use-package emacs
  :init
  (setq-default fill-column 80)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#222222"
                      :background "transparent")
  (global-display-fill-column-indicator-mode 1))




;;------------------------------------------------------------
;; 
(add-hook 'find-file-hook (lambda () (setq buffer-read-only t)))
;;------------------------------------------------------------
;; 

;; Store backup files in ~/.saves
(use-package emacs
  :config
  (setq backup-directory-alist `(("." . "~/.saves"))))

;;------------------------------------------------------------
;; 

(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI

;;------------------------------------------------------------
;; 
(use-package edit-server)


;;------------------------------------------------------------
;; 
;; (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
;; (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
;; (setq tab-bar-tab-hints t)                 ;; show tab numbers
;; (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))


;;------------------------------------------------------------
;; 

;; (use-package tabspaces
;;   :hook (after-init . tabspaces-mode)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)  ; Isolate buffers in switch-to-buffer
;;   (tab-bar-show t)  ; Show tab bar
;;   :config
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-remove-to-default t)
;;   ;; (consult-customize consult--source-buffer :hidden t :default nil)  ; Consult integration
;;   ;; (add-to-list 'consult-buffer-sources 'consult--source-project-buffer)
;;   ;; (add-to-list 'consult-buffer-sources 'consult--source-hidden-buffer :append t)
;;   )

(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it.
  ;; :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  ;; additional options
  (tabspaces-fully-resolve-paths t)  ; Resolve relative project paths to absolute
  (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*"))  ; Additional buffers to exclude
  (tab-bar-new-tab-choice "*scratch*"))


;; Filter Buffers for Consult-Buffer

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
				:predicate #'tabspaces--local-buffer-p
				:sort 'visibility
				:as #'buffer-name)))
  "Set workspace buffer list for consult-buffer.")
(add-to-list 'consult-buffer-sources 'consult--source-workspace))


(defun my-tabspaces-ibuffer-group ()
  "Group ibuffer entries by tabspace."
  (setq ibuffer-filter-groups
        (mapcar (lambda (tab)
                  (let ((tab-index (tab-bar--tab-index-by-name tab)))
                    (cons tab
                          `((predicate . (member (buffer-name)
                                                 (mapcar #'buffer-name
                                                         (tabspaces--buffer-list nil ,tab-index))))))))
                (tabspaces--list-tabspaces))))

(add-hook 'ibuffer-hook #'my-tabspaces-ibuffer-group)


 (use-package all-the-icons-completion
 :config
 (all-the-icons-completion-mode 1))

;; (use-package all-the-icons-dired
;;  :defer
;;  (all-the-icons-dired-mode 1))

;; (use-package dired-icon
;;  :config
;;  (dired-icon-mode 1))

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

(use-package consult-eglot
  ;; :custom
  )

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
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
;;

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))













;;------------------------------------------------------------
(provide 'init-experiments)
