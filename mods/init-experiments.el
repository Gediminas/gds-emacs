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


(use-package org-noter :defer (org-noter))



; ;; Enhance templates for protocol (auto-grab URL/title/selection)
; (add-to-list 'org-capture-templates
;              '("p" "Protocol Clip" entry (file+headline "" "Web Captures")
;                "* %^{Title}\nSource: [[%:link][%:description]]\nCaptured: %U\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n%?"
;                :prepend t :empty-lines 1))

; (use-package org-protocol)



(use-package s)
(use-package elfeed-score)
(use-package org-web-tools)

; (use-package org-protocol-capture-html)
; (require 'org-protocol-capture-html)
 

;;------------------------------------------------------------
(provide 'init-experiments)
