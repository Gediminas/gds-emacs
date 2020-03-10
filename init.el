;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;;# -q ignores personal Emacs files but loads the site files.
;emacs -q --eval='(message "%s" (emacs-init-time))'
;; For macOS users:
;open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package
(require 'use-package) ; guess what this one does too ?

(use-package zenburn-theme :ensure t :config (load-theme 'zenburn t))
(use-package doom-themes :ensure t)
(use-package spacemacs-common :ensure spacemacs-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; UI
;(add-to-list 'default-frame-alist '(height . 40))
;(add-to-list 'default-frame-alist '(width . 80))
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; MacOS: Fancy titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; MacOS
(setq mac-command-modifier 'control)


;; Advanced settings
(desktop-save-mode t)     ;auto save/load session,window-size

(setq-default truncate-lines t ;do not wrap lines
 )

;; Minimize mode-line
(use-package diminish
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 
    (lambda()
      (setq mode-name "")))  
  (with-eval-after-load 'undo-tree
    (diminish 'undo-tree-mode "")) 
  (diminish 'eldoc-mode "")
  )

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode t)
)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (which-key-mode t)
  :config
  (which-key-setup-side-window-bottom)
  (setq
    which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05
    which-key-separator " "
    which-key-prefix-prefix "+")
  :diminish (which-key-mode . ""))

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq 
    ivy-use-virtual-buffers t ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’
    ivy-height 10             ; number of result lines to display
    ivy-count-format "%d/%d " ; count candidates
    ivy-initial-inputs-alist nil ; no regexp by default
    ivy-re-builders-alist     ; configure regexp engine.
        '((t   . ivy--regex-ignore-order));; allow input not in order
   ) 
  :diminish (ivy-mode . ""))

(use-package counsel
  :ensure t
  :config
  (counsel-mode t)
  :diminish (counsel-mode . "")
)

;; Ranger (test)
(use-package ranger
  :ensure t
  :commands (ranger)
  :bind (("C-x d" . deer))
  :config
  (setq ranger-cleanup-eagerly t) ; kill the buffer just after you move to another entry in the dired buffer.
  )

;(use-package avy :ensure t
;  :commands (avy-goto-word-1))

;(global-set-key (kbd "<C-tab>") 'next-buffer)


;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
