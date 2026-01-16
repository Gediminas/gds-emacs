;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Theme & Appearance

;; (set-frame-font "Hack:size=12:weight=regular:antialias=true:hinting=true:hintstyle=hintfull" nil t)
;; (add-to-list 'default-frame-alist '(font . "Hack-16"))

;; (set-face-attribute 'default nil :font "Hack:size=12:weight=bold")
(set-face-attribute 'default nil :font "Hack:size=12:weight=regular")


(setq-default line-spacing 0)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ;???
        doom-themes-enable-italic t) ;???
  (load-theme 'doom-dracula t)
  (doom-themes-org-config)           ; Better org-mode faces???
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package heaven-and-hell
  :config
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes '((light . doom-one-light)
                                 (dark  . doom-dracula)))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f5>" . heaven-and-hell-load-default-theme)
         ("<f5>" . heaven-and-hell-toggle-theme)))


;;------------------------------------------------------------
;; Colors

(set-cursor-color "#ffff00")                ; cursor color

(set-face-attribute 'hl-line nil            ; current / highlight line style
  :background "#503050" 
  :box '(:line-width -1 :color "#aa55aa")
  :extend t)
; (set-face-background 'hl-line "#503050")
; (set-face-foreground 'hl-line nil)

(set-face-attribute 'region nil :background "#0000ff") ; Text selection color


;;------------------------------------------------------------
;; Pretty icons

(use-package nerd-icons)  ; M-x nerd-icons-install-fonts 

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
;; Line at 80 & 120

(setq whitespace-line-column 80)
(setq whitespace-line-column 120)
(setq whitespace-style '(face lines-tail))


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




;; Customizations

; (activities-tabs ((t nil)))

;(set-face-attribute 'tab-bar-tab nil
;                      :background "#333355"
;                      :foreground "white")

; (set-face-attribute 'tab-bar-tab-inactive nil
;                       :background nil
;                       ;; :foreground "#00BBff"
;                       :foreground "#006699"
;                       )


;; Set frame divider widths
(modify-frame-parameters (selected-frame) 
                         '((right-divider-width . 2 ) 
                           (bottom-divider-width . 2)))

;; Correct way to set face attributes
(set-face-attribute 'vertical-border nil 
                    :background "blue" 
                    :foreground "dark magenta")

;;------------------------------------------------------------
(provide 'init-theme)
