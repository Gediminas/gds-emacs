;;; init-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

(use-package org
  :mode ("\\.org\\'" . org-mode)
  
  :custom
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))
  (org-hide-leading-stars t)
  (org-return-follows-link t); Enter key opens links
  (org-startup-indented t); Pretty indentation
  (org-startup-folded 'content); Start folded, outline view
  (org-log-done 'time)

  (org-capture-templates
   '(("j" "Work-Log" entry (file+datetree "~/org/work-log.org")              "* %?" :empty-lines 0)
     ("n" "Note"     entry (file+headline "~/org/notes.org" "Random Notes") "** %?" :empty-lines 0)))
  
  :hook
  (org-mode . org-indent-mode)  ; Make the indentation look nicer
  
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

(provide 'init-org)
