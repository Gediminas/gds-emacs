;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Try

(global-set-key (kbd "C-;") 'mode-line-other-buffer)  ; Toggle last buffer

;; https://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
;; - mapping C-; to other-window (cycling through windows)
;; - mapping C-' to other-frame (cycling through frames). I actually started using frames a lot more after adding this shortcut).
;; - throwing a buffer to the other window (in a 2-window set-up), toggling between the current buffer and last buffer in a given window, burying the other buffer (this one is great for replacing the help window that pops up (when reading man pages, apropos, etc) with whatever window was previously visible).


;;------------------------------------------------------------
;; epub/pdf readers


(use-package ereader)

(use-package nov)


(use-package raindrop
  :defer t
  :ensure nil
  ;:ensure (raindrop :host github :repo "artawower/raindrop.el")
  )

(use-package raindrop-org
  :after raindrop
  :ensure nil
  :defer t)



; sudo apt install libjpeg-dev zlib1g zlib1g-dev
; sudo apt install zlib1g-dev libpoppler-glib-dev pkg-config autoconf automake libpng-dev
; nix: poppler
(use-package pdf-tools
  ; :mode
  ; (("\\.pdf$" . pdf-view-mode))

  :custom
  pdf-annot-activate-created-annotations t 
  pdf-view-resize-factor 1.1

  :bind
  (:map pdf-view-mode-map
    ;; normal isearch
	("C-s" . isearch-forward)
    ;; custom keys 
	("h" . pdf-annot-activate-created-annotations)
	("t" . pdf-annot-add-text-annotation)
	("D" . pdf-annot-delete))

  :hook
  ((pdf-view-mode) . (lambda () (cua-mode 0)))

  :config
  (pdf-tools-install)

  (setq-default pdf-view-display-size 'fit-page))






;;------------------------------------------------------------
(provide 'init-experiments)
