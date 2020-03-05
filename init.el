;; Package configs
(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Sane defaults
(setq delete-old-versions -1 )    ; delete excess backup versions silently
(setq version-control t )   ; use version control
(setq vc-make-backup-files t )    ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.local/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )               ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.local/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )  ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )  ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 ) ; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)  ; sentence SHOULD end with only a point.
(setq default-fill-column 80)   ; toggle wrapping text at the 80th character
(setq initial-scratch-message
 "Emacs
0123456789 0Oo 1Il jgae
C-h m / SPC h m / minor-modes") ; print a default message in the empty scratch buffer opened at startup

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
(toggle-truncate-lines t) ;do not wrap lines

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package
(require 'use-package) ; guess what this one does too ?

;; Themes
(use-package zenburn-theme :ensure t :config (load-theme 'zenburn t))
(use-package doom-themes :ensure t)
(use-package spacemacs-common :ensure spacemacs-theme)

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
  (which-key-setup-side-window-right-bottom)
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

;(global-set-key (kbd "<C-tab>") 'next-buffer)

;; General
(use-package general
  :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :keymaps '(normal)
   :prefix "SPC"
   "SPC"  'counsel-M-x
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "b"  '(:ignore t :which-key "buffer")
   "bb"  'ivy-switch-buffer
   "bd"  'kill-current-buffer
   "f"  '(:ignore t :which-key "file")
   "fd" 'deer
   "h"  '(:ignore t :which-key "help")
   "hm" 'describe-mode
   "q"  '(:ignore t :which-key "quit")
   "qq" 'save-buffers-kill-terminal
   "w"  '(:ignore t :which-key "window")
   "wd" 'evil-window-delete
   "ww" 'evil-next-window
   "w/" 'split-window-right
   "w-" 'split-window-below
   "x"  '(:ignore t :which-key "x-files")
   "xx" 'eval-buffer
   )

  (general-define-key
   :keymaps '(normal insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "/" 'swiper
   )

  ; (general-define-key
  ;  :states '(normal insert emacs)
  ;  :prefix "C-SPC"
  ;  :non-normal-prefix "C-SPC"
  ;  ;"l" '(avy-goto-line)
  ;  "a" 'align-regexp
  ;  )

  ; (general-define-key
  ;  :states '(normal motion insert emacs)
  ;  :prefix "SPC"
  ;  ;"a" 'align-regexp
  ;  ;"ar" '(ranger :which-key "call ranger")
  ;  ;"g"  '(:ignore t :which-key "Git")
  ;  ;"gs" '(magit-status :which-key "git status")
  ;  )
)

; ;; Custom keybinding
; (use-package general
;   :ensure t
;   :config (general-define-key
;   :states '(normal visual insert emacs)
;   :prefix "SPC"
;   :non-normal-prefix "M-SPC"
;   ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
;   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
;   "SPC" '(helm-M-x :which-key "M-x")
;   "pf"  '(helm-find-file :which-key "find files")
;   ;; Buffers
;   "bb"  '(helm-buffers-list :which-key "buffers list")
;   ;; Window
;   "wl"  '(windmove-right :which-key "move right")
;   "wh"  '(windmove-left :which-key "move left")
;   "wk"  '(windmove-up :which-key "move up")
;   "wj"  '(windmove-down :which-key "move bottom")
;   "w/"  '(split-window-right :which-key "split right")
;   "w-"  '(split-window-below :which-key "split bottom")
;   "wx"  '(delete-window :which-key "delete window")
;   ;; Others
;   "at"  '(ansi-term :which-key "open terminal")
;))


;(use-package avy :ensure t
;  :commands (avy-goto-word-1))
;
;(use-package general :ensure t
;  :config
;  (general-define-key "C-'" 'avy-goto-word-1)
;  )
;
;(general-define-key
; :prefix "C-c"
; ;; bind to simple key press
;  "b"	'ivy-switch-buffer  ; change buffer, chose using ivy
;  "/"   'counsel-git-grep   ; find string in git project
;  ;; bind to double key press
;  "f"   '(:ignore t :which-key "files")
;  "ff"  'counsel-find-file
;  "fr"	'counsel-recentf
;  "p"   '(:ignore t :which-key "project")
;  "pf"  '(counsel-git :which-key "find file in git dir")
;  )
;
;;(general-define-key
;  ;;; replace default keybindings
;  ;"C-s" 'swiper             ; search for string in current buffer
;  ;"M-x" 'counsel-M-x        ; replace default M-x with ivy backend
;  ;)
;
;
;

;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
