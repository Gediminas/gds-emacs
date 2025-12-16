;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; RSS


(use-package elfeed
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
)


;;------------------------------------------------------------
(provide 'init-reading)
