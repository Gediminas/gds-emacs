;;BOOT-TIMER

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

;;BOOT-FASTER
;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; INIT

;(require 'package)
;(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ;("gnu"   . "http://elpa.gnu.org/packages/")
                         ;("melpa" . "https://melpa.org/packages/")
			 ;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")))
;(package-initialize)
;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;;USE-PACKAGE

;(unless (package-installed-p 'use-package) ; unless it is already installed
  ;(package-refresh-contents) ; updage packages archive
  ;(package-install 'use-package)) ; and install the most recent version of use-package
;(require 'use-package) ; guess what this one does too ?
;(setq use-package-always-ensure t)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;THEME
(use-package zenburn-theme) ; :config (load-theme 'zenburn t))
(use-package doom-themes)
(use-package spacemacs-theme
  :defer t
  :config (load-theme 'spacemacs-light t))

;;CONFIG.ORG
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


;;CUSTOM.EL
;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
