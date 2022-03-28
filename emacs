;; set load paths
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path)) 
;; ensure elpa packages get loaded
(require 'package)

;;;;;;; package installation urls ;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("ELPA" . "https://tromey.com/elpa/") 
			 ("gnu" . "https://elpa.gnu.org/packages/")
                         ("MELPA" . "https://melpa.org/packages/")
			 ;("org" . "http://orgmode.org/elpa/")
                         ))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; activate installed packagesf
(package-initialize)
(add-to-list 'package-pinned-packages '(cider . "MELPA") t)

(ensure-package-installed 'ido-completing-read+ 'company
			  'smex
                          'hydra
			  'cider 'clojure-mode
                          'company
                          'lsp-mode 'lsp-ui 'lsp-treemacs
			  'rainbow-delimiters
			  'paredit
			  'clj-refactor
			  'hl-sexp
			  'ace-jump-mode
			  'ace-window
			  'expand-region
			  'neotree
			  'projectile
			  'magit 'magit-todos
			  'which-key
			  'ido
			  'ido-ubiquitous
			  'org 'german-holidays
			  'rust-mode
			  'js2-mode 'xref-js2
			  'modus-themes
                          'bm
                          'use-package
                          'quelpa 'quelpa-use-package
                          'helm-org
                          'org-ql ;; caution: org-ql MUST be installed after helm-org for org-super-links to work
                          )

(eval-when-compile
  (require 'use-package))

(use-package quelpa
:ensure t
:config (setq quelpa-upgrade-interval 7);; upgrade all packages once a week according to https://github.com/quelpa/quelpa
(add-hook #'after-init-hook #'quelpa-upgrade-all-maybe))

(use-package quelpa-use-package :ensure t)
(use-package helm-org-ql
 :config
 (setq org-ql-search-directories-files-regexp "\.org\\(_archive\\)?$"))

(use-package org-super-links
:quelpa (org-super-links :repo "toshism/org-super-links" :fetcher github :commit "0.2")
:bind (("C-c s s" . sl-link)
       ("C-c s l" . sl-store-link)
       ("C-c s C-l" . sl-insert-link)))

(use-package lsp-mode
  :commands lsp
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (zig-mode . lsp))
  :config (setq gc-cons-threshold (* 100 1024 1024)
                read-process-output-max (* 1024 1024)                
                lsp-lens-enable t
                lsp-semantic-tokens-enable nil
                lsp-signature-auto-activate nil
                lsp-enable-indentation nil 
                ;; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
                lsp-modeline-code-actions-enable nil)
  (setq lsp-zig-zls-executable "~/zig/zls/bin/zls")
  (setq lsp-file-watch-ignored-directories
   '("[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.git\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :config (setq treemacs-space-between-root-nodes nil))

;; show available key bindings when pressing any registered prefix
(which-key-mode t)

;; soft wrap long linesq
(global-visual-line-mode)
;; bind M-/ to hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; use rainbow parentheses everywhere
(require 'rainbow-delimiters)
;; highlight matching parentheses
(show-paren-mode 1)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-refs-sections-hook '(magit-insert-error-header
                              magit-insert-branch-description
                              magit-insert-local-branches
                              ;;magit-insert-remote-branches
                              )))
(use-package magit-todos
  :init
  (magit-todos-mode 1)
  :custom
  (magit-todos-require-colon nil)
  (magit-todos-keyword-suffix "" nil nil "do not use any suffixes"))

(use-package ido
  :init
  ;; use ido-mode everywhere
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10
        ido-work-directory-list '("~/" "/home/steffen/Dropbox/workspaces/current-workspace")))

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(use-package smex
  :init
  ;; smex, like ido for M-x
  (global-set-key [(meta x)] (lambda ()
                               (interactive)
                               (or (boundp 'smex-cache)
                                   (smex-initialize))
                               (global-set-key [(meta x)] 'smex)
                               (smex)))

  (global-set-key [(shift meta x)] (lambda ()
                                     (interactive)
                                     (or (boundp 'smex-cache)
                                         (smex-initialize))
                                     (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                     (smex-major-mode-commands))))
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

;; paredit everywhere
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'hl-sexp-mode)

;; hide ^M characters when editing files with windows carriage returns
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;; clojure specific configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'clojure-mode)
;; indent re-frame functions without so much leading white space
(define-clojure-indent
  (reg-sub '(1))
  (reg-event-db '(1))
  (reg-event-fx '(1)))
  
;; show fn as lambda in clojure files
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
				 (0 (progn (compose-region (match-beginning 1)
							   (match-end 1)
							   "\u03bb") nil))))))
(defun my-clojure-configuration ()
  (esk-pretty-fn)
  (remove-dos-eol)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c RET")
  (yas-minor-mode 1)
  (paredit-mode 1)
  (hl-sexp-mode 1)
  (company-mode 1))

(use-package company
  :ensure t
  :bind (("TAB" . company-indent-or-complete-common))
  :init  
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 2)
  :config (add-hook 'racer-mode-hook #'company-mode))

(global-set-key (kbd "<f5>") #'hs-toggle-hiding)
(use-package cider
  :config 
  ;; REPL history file
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  ;; nice pretty printing
  (setq cider-repl-use-pretty-printing t)
  ;; nicer font lock in REPL
  (setq cider-repl-use-clojure-font-lock t)
  ;; result prefix for the REPL
  (setq cider-repl-result-prefix ";; => ")
  ;; never ending REPL history
  (setq cider-repl-wrap-history t)
  ;; looong history
  (setq cider-repl-history-size 3000)
  (setq cider-eldoc-display-for-symbol-at-point nil) ;; disable cider documentation popups, use lsp-ui instead
  )

;; clj-refactor and dependencies
(require 'clj-refactor)

(add-hook 'clojure-mode-hook #'my-clojure-configuration)
(add-hook 'cider-mode-hook #'my-clojure-configuration)
(add-hook 'cider-repl-mode-hook #'my-clojure-configuration)
(setq nrepl-log-messages t)

(global-set-key [f8] 'other-frame)
(global-set-key [f7] 'paredit-mode)

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; CSS and Rainbow modes 
(defun all-css-modes() (css-mode) (rainbow-mode)) 
;; load css and rainbow mode for css files 
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode settings
(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.org_archive$" . org-mode))
  :init
  (setq org-indent-mode t)
  (setq org-startup-indented t)
  (setq org-image-actual-width (/ (display-pixel-width) 3)) ;; show inline images at 1/3rd of screen width
  (setq org-directory "~/org")
  (setq org-agenda-files (list org-directory))
  (setq org-mobile-directory (concat org-directory "/mobileorg"))
  (setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-include-diary t) ;; show holidays
  (setq org-hide-emphasis-markers t) ;; hide *bold* markers
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))
  (setq org-mobile-force-id-on-agenda-items nil)
  ;; show all nested sections when refiling
  (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  (setq org-outline-path-complete-in-steps nil) ; Refile in a single go
  :config
  (defun org-outlook-open (id)
    "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
    ;; 2017-03-02: following line stopped working with "org-outlook-open: ShellExecute failed: Access is denied."
    ;;(w32-shell-execute "open" (concat "outlook:" id))
    ;; fix:
    (if (boundp 'w32-shell-execute)
        (w32-shell-execute "open"
                           "C:/Program Files/Microsoft Office/root/Office16/OUTLOOK.EXE"
                           (concat "/select " "outlook:" id))
      (shell-command (concat "/mnt/c/Program\\ Files/Microsoft\\ Office/root/Office16/OUTLOOK.EXE /select outlook:" id))))
  (org-add-link-type "outlook" 'org-outlook-open)
  )

(use-package org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))


(use-package org-indent
  :after org
  :ensure nil
  :diminish ;; don't show "Ind" in the modeline
  :custom
  (org-indent-indentation-per-level 2))

(use-package org-capture
  :after org
  :bind (("\C-cc" . org-capture))
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "notes.org" "Tasks")
	   "* TODO %?\n  %i\n  %a"  :clock-in t :clock-resume t)
	  ("m" "Meeting" entry (file+headline "notes.org" "Meetings")
	   "* MEETING %? %U :MEETING:\n" :clock-in t :clock-resume t)
	  ("p" "Phone call" entry (file+headline "notes.org" "Calls")
	   "* PHONE %? %U :PHONE:\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree "tagebuch.org")
	   "* %?\nEntered on %U\n  %i\n  %a" :clock-in t :clock-resume t))))

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files '("~/org/notes.org" "~/org/tagebuch.org" "~/org/notes-urz.org"))
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5))
  ;; custom agendas for mobileorg
  (setq org-agenda-custom-commands
        '(("w" todo "TODO")
	  ("A" agenda "today"
	   ((org-agenda-ndays 1)
	    (org-agenda-overriding-header "Today")))))
  ;;;;; reviews for different time intervals, from https://stackoverflow.com/a/22440571
  ;; define "R" as the prefix key for reviewing what happened in various
  ;; time periods
  (add-to-list 'org-agenda-custom-commands
               '("R" . "Review" )
               )

  ;; Common settings for all reviews
  (setq efs/org-agenda-review-settings
        '((org-agenda-show-all-dates t)
          (org-agenda-start-with-log-mode t)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-archives-mode t)
          ;; I don't care if an entry was archived
          (org-agenda-hide-tags-regexp
           (concat org-agenda-hide-tags-regexp "\\|ARCHIVE"))))
  ;; Show the agenda with the log turn on, the clock table show and
  ;; archived entries shown.  These commands are all the same exept for
  ;; the time period.
  (add-to-list 'org-agenda-custom-commands
               `("Rw" "Week in review"
                 agenda ""
                 ;; agenda settings
                 ,(append
                   efs/org-agenda-review-settings
                   '((org-agenda-span 'week)
                     (org-agenda-start-on-weekday 0)
                     (org-agenda-overriding-header "Week in Review")))
                 ("~/org/review/week.html")))

  (add-to-list 'org-agenda-custom-commands
               `("Rd" "Day in review"
                 agenda ""
                 ;; agenda settings
                 ,(append
                   efs/org-agenda-review-settings
                   '((org-agenda-span 'day)
                     (org-agenda-overriding-header "Day in Review")))
                 ("~/org/review/day.html")))

  (add-to-list 'org-agenda-custom-commands
               `("Rm" "Month in review"
                 agenda ""
                 ;; agenda settings
                 ,(append
                   efs/org-agenda-review-settings
                   '((org-agenda-span 'month)
                     (org-agenda-start-day "01")
                     (org-read-date-prefer-future nil)
                     (org-agenda-overriding-header "Month in Review")))
                 ("~/org/review/month.html")))
  )

(use-package org-duration
  :config
  ;; work day is 8 hours
    (setq org-duration-units ;; each unit is in minutes
      '(("min" . 1)
        ("h" . 60)
        (" AT" . 480)
        ("d" . 1440)
        ("m" . 43200)
        ("y" . 525960.0)))
    ;; show clock table durations in work days (Arbeitstage)
    (setq org-duration-format '((" AT") (special . h:mm))))

(use-package holidays
  :init (require 'german-holidays)
  :config
  (setq calendar-holidays holiday-german-holidays))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable column number mode
(column-number-mode t)
(display-time)

;; GMail via gnus
;(require 'gnus)
;(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
;(setq gnus-select-method
;      '(nnimap "gmail"
;               (nnimap-address "imap.gmail.com")
;               (nnimap-server-port 993)
;               (nnimap-stream ssl)))

;; use m-tab for completion under windows instead of program switching
					;(w32-register-hot-key [M-tab])

;; ess mode for R statistics interaction
; (require 'ess-site)

;; enable winner mode - navigate window layout history with C-c <left> and C-c <right>
(winner-mode t)

;;recentf
(use-package recentf
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60)
  (global-set-key [(f12)] 'recentf-open-files))
;; ace-jump
(use-package ace-jump-mode
  :bind (("C-c j" . ace-jump-mode)
         ("C-x j" . ace-jump-mode-pop-mark))
  :init
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode              ;; the first one always map to : C-c j 
	  ace-jump-word-mode              ;; the second one always map to: C-u C-c j            
          ace-jump-line-mode))           ;; the third one always map to ：C-u C-u C-c j)
  :config
  (ace-jump-mode-enable-mark-sync)
  ;; jump back to position before last ace-mode jump with "C-x SPC"
  (autoload
    'ace-jump-mode-pop-mark
    "ace-jump-mode"
    "Ace jump back:-)"
    t)
  )
 
;; ace-window
(use-package ace-window
  :bind ("M-p" . ace-window))

;; remember places in open files
(use-package saveplace
  :init  
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  :config
  (setq-default save-place t))

(autoload 'markdown-mode "markdown-mode"
        "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; make sure we use UTF-8 as our standard encoding for text files     
(modify-coding-system-alist 'file "" 'utf-8-unix)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; expand-region for expanding/contracting selections according to nested semantic entities
(global-set-key (kbd "M-2") 'er/expand-region)
(global-set-key (kbd "M-3") 'er/contract-region)

(global-visual-line-mode)
;; bind M-/ to hippie expand
(global-set-key (kbd "M-/") #'hippie-expand)
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;; enable projectile project manager mode globally
  (projectile-mode))

;; do not create file~ everywhere
(setq make-backup-files nil)

;; helper for recompiling after at least one manual call to M-x compile
(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer) 
      (recompile))))

;; use this mode to automatically call M-x compile after each file save
(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
      (kill-local-variable 'after-save-hook)))

;; ebnf syntax highlighting, inspired by https://github.com/jeramey/ebnf-mode/blob/master/ebnf-mode.el
(define-generic-mode 'ebnf-mode
  '(("(*" . "*)"))
  '("=")
  '(("^[^ \t\n][^=]+" . font-lock-variable-name-face)
    ("['\"].*?['\"]" . font-lock-string-face)
    ("\\?.*\\?" . font-lock-negation-char-face)
    ("\\[\\|\\]\\|{\\|}\\|(\\|)\\||\\|,\\|;" . font-lock-type-face)
    ("[^ \t\n]" . font-lock-function-name-face))
  '("\\.ebnf\\'")
  `(,(lambda () (setq mode-name "EBNF")))
  "Major mode for EBNF metasyntax text highlighting.")

(provide 'ebnf-mode)
(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))
;; Rust support
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(require 'rust-mode)


;; toggle line wrap for long lines, easier when printing very wide tables
(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; javascript
(require 'xref-js2)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; javascript-mode has a clashing non-functioning keybinding to jump to definitions, unbind it 
(define-key js-mode-map (kbd "M-.") nil)
;; make sure we use xref-js2 to jump to definitions
(add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; VcXrc under Windows won't toggle keyboard layouts, so let's make it easier to insert german umlauts
(defhydra hydra-umlauts ()
  "Umlaute"
  ("'" (insert "ä") "ä")
  ("\"" (insert "Ä") "Ä")
  (";" (insert "ö") "ö")
  (":" (insert "Ö") "Ö")
  ("[" (insert "ü") "ü")
  ("{" (insert "Ü") "Ü")
  ("-" (insert "ß") "ß")
  ("E" (insert "€") "€")
  ("2" (insert "²") "²")
  ("3" (insert "³") "³"))
(global-set-key (kbd "C-;") 'hydra-umlauts/body)


(defhydra hydra-org-clock (:color blue :hint nil)
   "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit       _j_ump to current entry
        _c_ontinue   _q_uit       _d_isplay
        _o_ut        _t_imestamp  _r_eport
        _q_uery
      "
   ("i" org-clock-in)
   ("o" org-clock-out)
   ("c" org-clock-in-last)
   ("e" org-clock-modify-effort-estimate)
   ("q" org-clock-cancel)
   ("j" org-clock-goto)
   ("d" org-clock-display)
   ("r" org-clock-report)
   ("t" org-time-stamp)
   ("q" helm-org-ql)
   ("?" (org-info "Clocking commands")))
(global-set-key (kbd "C-c o") 'hydra-org-clock/body)

(global-set-key (kbd "M-S-<f6>") 'delete-indentation) ;; use M-S <f6> instead of M-S 6 (broken key on keyboard...)
(global-set-key (kbd "<f6>") (lambda () (interactive) (insert "6")))
(global-set-key (kbd "S-<f6>") (lambda () (interactive) (insert "^")))

;; bookmarks, https://github.com/joodland/bm
(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
(setq bm-restore-repository-on-load t) ;; restore on load (even before you require bm)
(setq bm-cycle-all-buffers t) ;; Allow cross-buffer 'next'
(setq bm-repository-file "~/.emacs.d/bm-repository") ;; where to store persistant files
(setq-default bm-buffer-persistence t)  ;; save bookmarks
(add-hook 'kill-buffer-hook #'bm-buffer-save)
(setq bm-marker 'bm-marker-left)
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when Emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook #'(lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))
;; Restoring bookmarks
(add-hook 'find-file-hooks   #'bm-buffer-restore)
(add-hook 'after-revert-hook #'bm-buffer-restore)


;; use Windows browser to open links in WSL
;; from https://www.reddit.com/r/bashonubuntuonwindows/comments/70i8aa/making_emacs_on_wsl_open_links_in_windows_web/
;; Determine the specific system type. 
;; Emacs variable system-type doesn't yet have a "wsl/linux" value,
;; so I'm front-ending system-type with my variable: sysTypeSpecific.
;; I'm no elisp hacker, so I'm diverging from the elisp naming convention
;; to ensure that I'm not stepping on any pre-existing variable.
(setq-default sysTypeSpecific system-type) ;; get the system-type value
(cond 
 ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
 ((eq sysTypeSpecific 'gnu/linux)  
  (when (string-match "Linux.*Microsoft.*Linux" 
                      (shell-command-to-string "uname -a"))
    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
          cmdExeArgs '("/c" "start" "") )
    (setq
     browse-url-generic-program  cmdExeBin
     browse-url-generic-args     cmdExeArgs
     browse-url-browser-function 'browse-url-generic))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   '(("Okular"
      ("okular --unique %o#src:%n%b")
      "/usr/bin/okular")))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open")))
 '(blink-cursor-mode nil)
 '(cider-eval-spinner-type 'vertical-breathing)
 '(cider-inspector-fill-frame nil)
 '(cider-lein-parameters "trampoline repl :headless")
 '(column-number-mode t)
 '(company-idle-delay 2)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("21388667ce5ee0b375e6282f0d6c6b61588da6604d343bbb19389e6a54d3d00d" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "75615f00bca2d070186d217af34b1337badbc55e6a6d6c3f6929e4c3405c8079" "1d904ba8343822dff21ffae28a348975eafeb0734034ed5fa33d78bf2519e7cb" "39b0c917e910f32f43f7849d07b36a2578370a2d101988ea91292f9087f28470" "f58379453f93eb5152f87b19322feb3ac0393f4db6f9b5c6711a8aa6d2affe6a" "8878226b9bda9a16c2639a85d86af1a4eac16e88522587afa368d745006ef476" "1d4abd3ff9d32f7740f5b8c44fc5dd3e9625e8bde84315be58a865bc087d1714" "93fcfa172aad04bd7f86323c67c661b8cfeeda044632d5e5c8d54f1a47c38e8b" "b31e969329848ec0432a23850e1db997cf16c1b85845c73996f0d582e7403b27" "88380a535b965f1172ced30e751f5abf31047f15eae17adf323ba415a9408617" "87fd15a92096797894626d25d8f8a436b90ce8d97d499a98faea972944645fbd" "e129ee166c2cd586fb0831c711fc49977a065360461ba9ac78786be822ab4338" "c0350aed6dc98abdc329906a630b4cdf8ebb147cdf2a873e2648dfc0b904b2ab" "5744f67c2f2f5bb2bfe40dd72e590c8255bbaa9441c957a7524530077bc244cc" "c727910dd591caecd19c432ecc7afbcdecca1af23cd494bb60906aa613e7666a" "65ee857bb301e7a1cbc0822aeccf0bfa1b4dfa7199a759ab7b7b0504885233b7" "405654bde08b14bb90e4f8e6f900571f7c9827708ead86b13f6949566dde2065" "ba3399d98232527210e96e5f44c78a9aeb1cb159c6cd6dfa4348f2e08215bf19" default))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ".clj-kondo" ".shadow-cljs" "target" "node_modules"))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" ".*\\.mtc.*$"))
 '(neo-window-fixed-size nil)
 '(package-check-signature nil)
 '(package-selected-packages
   '(org-ql helm-org-ql helm-org quelpa quelpa-use-package org-download lua-mode german-holidays lsp-ui zig-mode use-package lsp-java lsp-mode calfw calfw-org bm abyss-theme anti-zenburn-theme flycheck-clj-kondo xref-js2 js2-mode cider-hydra org-clock-convenience org-clock-csv markdown-mode+ htmlize magit-todos magit-org-todos ido-ubiquitous magit magit-popup markdown-preview-mode paredit which-key racer cargo rust-mode git-gutter-fringe hideshowvis ido-completing-read+ markdown-mode smex rainbow-delimiters projectile neotree hl-sexp expand-region company clj-refactor cider-eval-sexp-fu ace-window ace-jump-mode))
 '(racer-rust-src-path
   "/home/steffen/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
 '(reb-re-syntax 'string)
 '(show-paren-mode t)
 '(sp-base-key-bindings 'sp)
 '(speedbar-supported-extension-expressions
   '(".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".clj[sc]?"))
 '(tool-bar-mode nil)
 '(xref-js2-ignored-dirs '("bower_components" "node_modules" "build")))
