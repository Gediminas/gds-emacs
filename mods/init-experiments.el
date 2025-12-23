;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; Try

(setq large-file-warning-threshold 100000000) ; 100 MB

(global-set-key (kbd "C-;") 'mode-line-other-buffer)  ; Toggle last buffer

;; https://emacs.stackexchange.com/questions/728/how-do-i-switch-buffers-quickly
;; - mapping C-; to other-window (cycling through windows)
;; - mapping C-' to other-frame (cycling through frames). I actually started using frames a lot more after adding this shortcut).
;; - throwing a buffer to the other window (in a 2-window set-up), toggling between the current buffer and last buffer in a given window, burying the other buffer (this one is great for replacing the help window that pops up (when reading man pages, apropos, etc) with whatever window was previously visible).


;;------------------------------------------------------------
;; readers

;; Integrate with bookmarks for persistent reading positions
(use-package bookmark
  ; :ensure nil
  :config
  (setq bookmark-save-flag 1)  ; Auto-save on changes
  ; :bind
  ; (("C-c b m" . bookmark-set)    ; Set bookmark (e.g., current PDF page)
  ;  ("C-c b j" . bookmark-jump)   ; Jump to bookmark, opens in current/other tab if needed
  ;  ("C-c b l" . list-bookmarks))
  )


(use-package raindrop
  :defer t
  :ensure nil
  ;:ensure (raindrop :host github :repo "artawower/raindrop.el")
  )

(use-package raindrop-org
  :after raindrop
  :ensure nil
  :defer t)


; (use-package org-noter-pdftools)
; (use-package org-noter)
; (use-package org-noter-plus)
;
;

(use-package s)


;;------------------------------------------------------------
;; Org-Noter (Book/PDF Annotations)

; grok fix:
; (use-package org-noter-pdftools
;   :after (org-noter pdf-tools)
;   :config
;   (with-eval-after-load 'pdf-annot
;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
;   (setq org-noter-pdftools-select-after-navigation t))  ; Auto-select text for notes

; ;; Unified keybinds for annotation (same in pdf-tools/nov)
; (dolist (map (list pdf-view-mode-map))
;   (define-key map (kbd "i") 'org-noter-insert-note)  ; Insert note at point
;   (define-key map (kbd "M-i") 'org-noter-insert-precise-note)  ; Precise (highlight text first)
;   (define-key map (kbd "q") 'org-noter-kill-session))  ; Quick quit
; END grok

; kind of ok:
(use-package org-noter
  :after (pdf-tools nov)
  :config
  (setq org-noter-notes-search-path (list org-directory))  ; Notes in org dir
  (setq org-noter-auto-save-last-location t))  ; Sync position

  
; ; (use-package org-noter
; ;   :ensure t
; ;   :config
; ;   ;; Optional: Enable integration with org-roam if you use it
; ;   (setq org-noter-notes-search-path '("~/org/notes"))

; ;   ;; Recommended: Use pdf-tools for the best experience
; ;   (setq org-noter-pdftools-select-after-navigation t)
; ;   (require 'org-noter-pdftools))


; ; (use-package org-noter-pdftools
; ;   :after org-noter
; ;   :config
; ;   (with-eval-after-load 'pdf-tools
; ;     (with-eval-after-load 'org-noter
; ;       (require 'org-noter-pdftools))))


(use-package org-node)


; ;; Enhance templates for protocol (auto-grab URL/title/selection)
; (add-to-list 'org-capture-templates
;              '("p" "Protocol Clip" entry (file+headline "" "Web Captures")
;                "* %^{Title}\nSource: [[%:link][%:description]]\nCaptured: %U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?"
;                :prepend t :empty-lines 1))

; (use-package org-protocol)




; (use-package org-protocol-capture-html)
; (require 'org-protocol-capture-html)
 

; todo: https://github.com/larstvei/dot-emacs
;; display the definition of word at point
(use-package define-word
  ; :defer t
  :bind (:map custom-bindings-map ("C-c D" . define-word-at-point)))


; todo: https://github.com/larstvei/dot-emacs

(use-package move-text
  :bind (:map custom-bindings-map
              ("C-M-<down>" . move-text-down)
              ("C-M-<up>" . move-text-up)))


; (use-package org-web-tools)



;; For web: Use sparingly, convert page to org on capture
(use-package org-web-tools
  :after org
  :bind ("C-c o w" . org-web-tools-insert-web-page-as-entry))  ; Manual insert to buffer

; (use-package org-ref
;   :after calibredb
;   :config
;   (setq calibredb-ref-default-bibliography "~/books/catalog.bib")
;   (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)
;   (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename))


    
;;------------------------------------------------------------
(provide 'init-experiments)
