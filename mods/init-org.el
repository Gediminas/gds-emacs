;;; -*- lexical-binding: t; -*-

(setq org-directory "~/org")                 ; Main folder for notes
(setq org-agenda-files (list org-directory)) ; Where agenda looks for files
(setq org-default-notes-file (concat org-directory "/inbox.org")) ; default capture file
(setq org-startup-indented t)                ; Pretty indentation
(setq org-hide-leading-stars t)              ; Cleaner headings
(setq org-return-follows-link t)             ; Enter key opens links
(setq org-startup-folded 'content)           ; Start folded, outline view
(setq org-log-done 'time)                    ; Log DONE

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; *.org -> org mode
(add-hook 'org-mode-hook 'org-indent-mode)   ; Make the indentation look nicer

(setq org-capture-templates
  '(("j" "Work-Log" entry (file+datetree "~/org/work-log.org")  "* %?"  :empty-lines 0)
    ("n" "Note"     entry (file+headline "~/org/notes.org" "Random Notes") "** %?" :empty-lines 0)))

(global-set-key (kbd "C-c a") #'org-agenda)     ; Open agenda
(global-set-key (kbd "C-c c") #'org-capture)    ; Quick capture
(global-set-key (kbd "C-c l") #'org-store-link) ; Store links

(provide 'init-org)
