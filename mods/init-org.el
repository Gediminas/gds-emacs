;;; init-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

; Linux Setup:
; 
; https://github.com/alphapapa/org-protocol-capture-html#org-protocol-instructions
; 
; ❱ hx ~/.local/share/applications/org-protocol.desktop
; 
; [Desktop Entry]
; Name=org-protocol
; Comment=Intercept calls from emacsclient to trigger custom actions
; Categories=Other;
; Keywords=org-protocol;
; Icon=emacs
; Type=Application
; Exec=emacsclient -- %u
; Terminal=false
; StartupWMClass=Emacs
; MimeType=x-scheme-handler/org-protocol;
; 
; ❱ update-desktop-database ~/.local/share/applications/
;
; ???:
; firefox => about:config => network.protocol-handler.expose.org-protocol => true

(use-package org
  ; :mode ("\\.org\\'" . org-mode)
  
  :custom
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (directory-files-recursively "~/org" "\\.org\\'"))
  ; (org-agenda-files (list org-directory))
  (org-hide-leading-stars t)
  (org-return-follows-link t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-log-done 'time)

  (org-capture-templates '(

    ("b" "Book Quote" entry (file+headline "" "Books")
      "* %^{Title} :book:\nQuote: #+BEGIN_QUOTE\n%?\n#+END_QUOTE\nSource: %^{Book} p.%^{Page}\n%U" :prepend t)
     ; ("t" "Todo" entry (file+headline "" "Tasks")        
     ;  "* TODO %?\n%U\n%a\n" :prepend t)
    ("t" "todo" entry (file "~/org/inbox.org")
      "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t :prepend t)


    ; ("n" "Note" entry (file+headline "~/org/notes.org" "Rand") 
    ;   "** %?" :empty-lines 0)
    ("n" "note" entry (file "~/org/inbox.org")
      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t :prepend t)

    ("r" "RSS/Elfeed Clip" entry (file+headline "~/org/inbox.org" "Feeds")
     "* %^{Title} :rss:\nSource: [[%l][Link]]\nCaptured: %U\n#+BEGIN_QUOTE\n%?\n#+END_QUOTE" :prepend t)

    ; ("m" "Note-General" entry (file+headline "" "Notes")        
    ;   "* %? :NOTE:\n%U\n%a\n" :prepend t)

    ; ("j" "Work-Log" entry (file+datetree "~/org/work-log.org")     
    ;   "* %?" :empty-lines 0)
    ("j" "Journal" entry (file+datetree "~/org/diary.org")
      "* %?\n%U\n" :clock-in t :clock-resume t)     

     ;; Web capture templates with immediate-finish
    ("p" "(web-tsel)" entry (file+headline "~/org/inbox.org" "Web")  
      "* %:description\nSource: %U, %:link\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
      :immediate-finish t :empty-lines 1)
     
    ("L" "(web-link)" entry (file+headline "~/org/inbox.org" "Web")  
      "* [[%:link][%:description]]\nCaptured On: %U"
      :immediate-finish t :empty-lines 1)

  ))
  
  :hook
  (org-mode . org-indent-mode)
  
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  
  :init
  (require 'org-protocol))

;; Auto-finalize with custom message that overrides default
(with-eval-after-load 'org-capture
  (defun my/org-capture-finalize-immediate ()
    "Automatically finalize capture if :immediate-finish is set with custom message."
    (when (and (org-capture-get :immediate-finish)
               (org-capture-get :key))
      (let* ((template-key (org-capture-get :key))
             (link (ignore-errors (plist-get org-store-link-plist :link)))
             (description (ignore-errors (plist-get org-store-link-plist :description)))
             (custom-message
              (cond
               ((string= template-key "L")
                (format "✓ Captured link: %s / %s" 
                        (or description "No title") 
                        (or link "No link")))
               ((string= template-key "p")
                (format "✓ Captured text: %s / %s" 
                        (or description "No title") 
                        (or link "No link")))
               (t "✓ Capture completed"))))
        (org-capture-finalize)
        ;; Override the default message after a tiny delay
        (run-at-time 0.1 nil (lambda () (message "%s" custom-message))))))
      
  (add-hook 'org-capture-mode-hook 'my/org-capture-finalize-immediate))

(provide 'init-org)
