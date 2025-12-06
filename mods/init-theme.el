;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Theme & Appearance

(set-frame-font "Hack:size=15:weight=regular:antialias=true:hinting=true:hintstyle=hintfull" nil t)

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

;;------------------------------------------------------------
;; Customizations

(defun my/setup-tab-faces-light ()
  "Setup tab faces for light mode."
  ;; (set-face-attribute 'tab-bar nil
  ;;                     :background "light blue"
  ;;                     :foreground "#000000")
  ;; (set-face-attribute 'tab-bar-tab nil
  ;;                     :background "sky blue"
  ;;                     :foreground "#000000")
  )

(defun my/setup-tab-faces-dark ()
  "Setup tab faces for dark mode."

 ;; '(tab-bar ((t (:background "blue4" :foreground "#1E2029"))))
 ;; '(tab-bar-tab ((t (:background "royal blue" :foreground "#f8f8f2"))))
 ;; '(tab-bar-tab-inactive ((t (:background "blue" :foreground "dim gray"))))
 ;; '(tab-line ((t (:background "dim gray" :foreground "#1E2029"))))
 ;; '(tab-line-tab-current ((t (:background "sea green" :foreground "#f8f8f2"))))
 ;; '(tab-line-tab-inactive ((t (:background "cyan4" :foreground "gainsboro"))))

  ;; (set-face-attribute 'tab-bar nil
  ;;                     :background "blue4"
  ;;                     :foreground "#1E2029")
  (set-face-attribute 'tab-bar-tab nil
                      :background "#004477"
                      :foreground "grey")
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background nil
                      :foreground "#00BBff")
  ;; (set-face-attribute 'tab-line nil
  ;;                     :background "dim gray"
  ;;                     :foreground "#1E2029")
  (set-face-attribute 'tab-line-tab-current nil
                      :background "royal blue"
                      :foreground "white")
  (set-face-attribute 'tab-line-tab-inactive nil
                      :background nil
                      :foreground "#00BBff")
  )

;; Apply dark theme by default
(my/setup-tab-faces-dark)

;; Optional: Hook into theme changes
(defun my/apply-theme-tab-faces (theme)
  "Apply appropriate tab faces based on THEME."
  (if (memq theme '(doom-one doom-dark modus-vivendi))
      (my/setup-tab-faces-dark)
    (my/setup-tab-faces-light)))

;; Hook into theme loading
(advice-add 'load-theme :after
            (lambda (theme &rest _)
              (my/apply-theme-tab-faces theme)))

;;------------------------------------------------------------
(provide 'init-theme)
