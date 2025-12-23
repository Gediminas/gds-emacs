;;; -*- lexical-binding: t; -*-
;;; init-reading.el --- book library and readers -*- lexical-binding: t; -*-


;;------------------------------------------------------------
;; Calibre - Book Library

(use-package calibredb
  ; :demand t  ; Load eagerly for dashboard access
  :defer t
  :config
  (setq calibredb-root-dir "~/books")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-set-layout 'list)  ; list Or 'dashboard for org-like view
  :bind
  ("C-c o l" . calibredb))


;;------------------------------------------------------------
;; PDF

(use-package pdf-tools
  :custom
  (pdf-view-resize-factor 1.1)      ; Zoom step 10% (default 25%)
  (pdf-view-display-size 'fit-page) ; Default to fit-page
  
  :init
  (pdf-loader-install)) ; Lighter than `pdf-tools-install`


;;------------------------------------------------------------
;; EPUB

; (use-package nov
;   :mode ("\\.epub\\'" . nov-mode)
;   :custom (nov-text-width 80))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  
  :custom
  (nov-text-width 80)
  
  :hook
  ; multi pages
  (nov-mode . (lambda ()
                (visual-line-mode)
                (follow-mode)))
  
  :bind
  (:map nov-mode-map
    ; multi pages
    ("C-x 3" . (lambda ()
      (interactive)
      (split-window-right)
      (other-window 1)
      (follow-mode 1))))
  )

; (use-package ereader)


;;------------------------------------------------------------
;; RSS

(use-package elfeed
  :defer t
  :config
  ;; data is stored in ~/.elfeed
  (setq elfeed-feeds '(

    ; https://linux-audit.com/resources/linux-rss-feeds/
    ("https://news.ycombinator.com/rss" HackerNews)
    ("https://lwn.net/headlines/newrss" LWN)
    ("https://www.linuxjournal.com/node/feed" LinuxJournal)
    ("https://neilzone.co.uk/index.xml" NeilsBlog)
    ("https://www.youtube.com/feeds/videos.xml?user=JtheLinuxguy" LearnLinuxTV)
    ("https://linux-audit.com/feed" LinuxAudit)
    ("https://itsfoss.com/rss" FOSS)

  ))

  ; (setq-default elfeed-search-filter "@7-days-ago +unread")
  ; (setq-default elfeed-search-title-max-width 100)
  ; (setq-default elfeed-search-title-min-width 100)
  :bind
  ("C-c o r" . elfeed))


;;------------------------------------------------------------
(provide 'init-reading)
