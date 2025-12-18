;;; -*- lexical-binding: t; -*-

;;------------------------------------------------------------
;; TabSpaces

(use-package tabspaces
  :hook
  (after-init . tabspaces-mode)
  :custom
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default nil) ; Kill buffers on kill&close
  (tabspaces-use-filtered-buffers-as-default t) ; remaps switch-to-buffer to tabspaces-switch-to-buffer.
  ; (tabspaces-session-project-session-store "~/.config/emacs/persist") ;; Store all project sessions in a specific directory
  ; (tabspaces-session-file "~/.config/emacs/persist/tabsession.el") ;; Store all project sessions in a specific directory
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-exclude-buffers '("*Messages*" "*Compile-Log*"))  ; Additional buffers to exclude
  (tabspaces-keymap-prefix "C-x C-z")  ; Default: C-c TAB
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
   
  ; (tabspaces-fully-resolve-paths nil)

  (tabspaces-session t) ; Save sessions automatically
  (tabspaces-session-auto-restore t)    ; Auto-restore sessions on startup and when opening projects

  ; (tab-bar-new-tab-choice "*scratch*")
  ; (tab-bar-new-tab-choice "*Messages*")

  ; :config
  ; (setq tab-bar-show t)                 ; Show always
  ; (setq tab-bar-close-button-show nil)  ; Hide tab close / X button
  ; (setq tab-bar-tab-hints t)            ; Show tab numbers

  :bind
    ; (defvar tabspaces-command-map
    ; (let ((map (make-sparse-keymap)))
    ;   (define-key map (kbd "C") 'tabspaces-clear-buffers)
    ;   (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
    ;   (define-key map (kbd "d") 'tabspaces-close-workspace)
    ;   (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
    ;   (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
    ;   (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
    ;   (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
    ;   (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
    ;   (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
    ;   (define-key map (kbd "w") 'tabspaces-show-workspaces)
    ;   (define-key map (kbd "T") 'tabspaces-toggle-echo-area-display)
    ;   map)
    ; "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
     
    ; (:map pdf-view-mode-map
    ;    ("C-z" . tabspaces-open-or-create-project-and-workspace)
    ; )


    (:map tabspaces-command-map
     ("C-z" . tabspaces-open-or-create-project-and-workspace))
    

    ; ;; For pdf-view-mode
    ; (:map pdf-view-mode-map
    ;  ("C-z" . tabspaces-open-or-create-project-and-workspace))
  
    ;; global
    ; ("C-x C-z C-z" . your-function-here)
  
  )
; (global-set-key (kbd "C-x C-z C-z") 'tabspaces-switch-or-create-workspace)  ; Toggle last buffer
(global-set-key (kbd "C-'") 'tabspaces-switch-or-create-workspace)
; (global-set-key (kbd "C-x C-z C-z") 'tabspaces-open-or-create-project-and-workspace)


 ;; Filter Buffers for Consult-Buffer

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
				:predicate #'tabspaces--local-buffer-p
				:sort 'visibility
				:as #'buffer-name)))
  "Set workspace buffer list for consult-buffer.")
(add-to-list 'consult-buffer-sources 'consult--source-workspace))


(defun my-tabspaces-ibuffer-group ()
  "Group ibuffer entries by tabspace."
  (setq ibuffer-filter-groups
        (mapcar (lambda (tab)`
                  (let ((tab-index (tab-bar--tab-index-by-name tab)))
                    (cons tab
                          `((predicate . (member (buffer-name)
                                                 (mapcar #'buffer-name
                                                         (tabspaces--buffer-list nil ,tab-index))))))))
                (tabspaces--list-tabspaces))))
(add-hook 'ibuffer-hook #'my-tabspaces-ibuffer-group)


;;------------------------------------------------------------
;; Activities

; (use-package activities
;   :config
;   (activities-mode)
;   (activities-tabs-mode)
;   ; (setq edebug-inhibit-emacs-lisp-mode-bindings t) ;; Prevent `edebug' default bindings from interfering.

;   ; :custom-face
;   ; (activities-tabs ((t nil)))
  
;   ; '(activities-tabs ((t (:inherit font-lock-function-name-face))))
;   ;  '(font-lock-function-name-face ((t (:background "dim gray"))))  

;   :bind
;   (("C-x C-a C-n" . activities-new)
;    ("C-x C-a C-d" . activities-define)
;    ("C-x C-a C-a" . activities-resume)
;    ("C-x C-a C-s" . activities-suspend)
;    ("C-x C-a C-k" . activities-kill)
;    ("C-x C-a RET" . activities-switch)
;    ("C-x C-a b" . activities-switch-buffer)
;    ("C-x C-a g" . activities-revert)
;    ("C-x C-a l" . activities-list))
;    ;("C-c C-a" . activities-resume)
;    ;("C-c C-s" . activities-suspend)
; )


;;------------------------------------------------------------
;; Bufler

;; (use-package bufler
;;   :config
;;   (bufler-mode 1)
;;   ;(bufler-tabs-mode 1)  ; tabs as workspaces
;;   :bind ("C-x C-b" . bufler))  ; list/switch

;; ;; Group by project
;; (setq bufler-groups (bufler-defgroups
;;   (group (auto-project))))


;;------------------------------------------------------------
;; One-Tab-Per-Project

; (use-package otpp
;   ; :straight t
;   :after project
;   :init
;   ;; Enable `otpp-mode` globally
;   (otpp-mode 1)
;   ;; If you want to advice the commands in `otpp-override-commands`
;   ;; to be run in the current's tab (so, current project's) root directory
;   (otpp-override-mode 1))


;;------------------------------------------------------------
;; EasySession

;; (use-package easysession
;;   :ensure t

;;   :custom
;;   (easysession-save-interval (* 10 60))  ; Save every 10 minutes

;;   ;; Display the active session name in the mode-line lighter.
;;   (easysession-save-mode-lighter-show-session-name t)

;;   ;; Optionally, the session name can be shown in the modeline info area:
;;   (easysession-mode-line-misc-info t)

  
;;   ;; Automatically load the session at startup and restore frame size and
;;   ;; position (geometry)
;;   (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
;;   ; (add-hook 'emacs-startup-hook #'easysession-load 102)

;;   ;; Automatically save the current session every `easysession-save-interval'
;;   ;; seconds (default: 10 minutes)
;;   (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
;;   )


;;------------------------------------------------------------
;; BeFrame

;; (use-package beframe
;;   :config
;;   (beframe-mode 1))


;;------------------------------------------------------------
;; Vanilla

;; https://www.rahuljuliato.com/posts/emacs-tab-bar-groups
; (use-package tab-bar
;   ; :defer t
;   :custom
;   (tab-bar-close-button-show nil)
;   (tab-bar-new-button-show nil)
;   (tab-bar-tab-hints nil)
;   (tab-bar-auto-width nil)
;   (tab-bar-separator " ")
;   ; (tab-bar-format '(
;   ;         tab-bar-format-history
;   ;         ; tab-bar-format-tabs-groups 
; 		; tab-bar-format-tabs
;   ;         tab-bar-separator
; 		; 			;tab-bar-format-add-tab
;   ;         ))
  
;   :init
;   ;;; --- OPTIONAL INTERNAL FN OVERRIDES TO DECORATE NAMES
;   ; (defun tab-bar-tab-name-format-hints (name _tab i)
; 	 ;  (if tab-bar-tab-hints (concat (format "»%s" name) "") name))

;  ;  (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
; 	; (propertize
; 	;  (concat (funcall tab-bar-tab-group-function tab))
; 	;  'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))


;   ;;; --- UTILITIES FUNCTIONS
;   (defun emacs-solo/tab-group-from-project ()
; 	"Call `tab-group` with the current project name as the group."
; 	(interactive)
; 	(when-let* ((proj (project-current))
; 				(name (file-name-nondirectory
; 					   (directory-file-name (project-root proj)))))
; 	  (tab-group (format "[%s]" name))))

;   (defun emacs-solo/tab-switch-to-group ()
;   "Prompt for a tab group and switch to its first tab.
; Uses position instead of index field."
;   (interactive)
;   (let* ((tabs (funcall tab-bar-tabs-function)))
; 	(let* ((groups (delete-dups (mapcar (lambda (tab)
; 										  (funcall tab-bar-tab-group-function tab))
; 										tabs)))
; 		   (group (completing-read "Switch to group: " groups nil t)))
; 	  (let ((i 1) (found nil))
; 		(dolist (tab tabs)
; 		  (let ((tab-group (funcall tab-bar-tab-group-function tab)))
; 			(when (and (not found)
; 					   (string= tab-group group))
; 			  (setq found t)
; 			  (tab-bar-select-tab i)))
; 		  (setq i (1+ i)))))))

;   ;;; --- EXTRA KEYBINDINGS
;   (global-set-key (kbd "C-x t P") #'emacs-solo/tab-group-from-project)
;   (global-set-key (kbd "C-x t g") #'emacs-solo/tab-switch-to-group)

;   ;;; --- TURNS ON BY DEFAULT
;   (tab-bar-mode 1))

; (custom-set-faces
;   '(tab-bar
; 	((t (:background "#232635" :foreground "#A6Accd"))))
;   '(tab-bar-tab
; 	((t (:background "#232635" :underline t))))
;   '(tab-bar-tab-inactive
; 	((t ( ;; :background "#232635" ;; uncomment to use this
; 		  ;; :box (:line-width 1 :color "#676E95")
; 		  ))))
;   '(tab-bar-tab-group-current
; 	((t (:background "#232635" :foreground "#A6Accd" :underline t))))
;   '(tab-bar-tab-group-inactive
; 	((t (:background "#232635" :foreground "#777")))))


;;------------------------------------------------------------
(provide 'init-tabs)
