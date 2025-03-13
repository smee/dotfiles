;; set load paths
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path)) 
;; ensure elpa packages get loaded
(require 'package)

;;;;;;; package installation urls ;;;;;;;;;;;;;;;;;;;
(setq package-archives '(;("ELPA" . "https://tromey.com/elpa/") 
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
                          'flycheck-clj-kondo
                          'company
                          'lsp-mode 'lsp-ui 'lsp-treemacs
			  'rainbow-delimiters
			  'paredit
			  'clj-refactor
			  'ace-jump-mode
			  'ace-window
			  'expand-region
			  'neotree
			  'projectile
			  'magit 'magit-todos
			  'which-key
			  'ido
			  'ido-completing-read+
			  'german-holidays
			  'rust-mode
			  'js2-mode 'xref-js2
			  'modus-themes
                          ;;'bm
                          'use-package
                          'quelpa 'quelpa-use-package
                          'helm
                          'helm-org
                          'helm-org-rifle)

(eval-when-compile
  (require 'use-package))

(use-package quelpa
:ensure t
:config (setq quelpa-upgrade-interval 7);; upgrade all packages once a week according to https://github.com/quelpa/quelpa
)

(use-package quelpa-use-package :ensure t)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package org-super-links
:quelpa (org-super-links :repo "toshism/org-super-links" :fetcher github :branch "develop")
:bind (("C-c s s" . org-super-links-link)
       ("C-c s d" . org-super-links-delete-link)
       ("C-c s l" . org-super-links-store-link)
       ("C-c s C-l" . org-super-links-insert-link)))

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
  :config
  ;; from https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
  (defun th/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc event)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))
  (defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))
  (defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))
  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  (transient-append-suffix 'magit-dispatch "!"
  '("#" "My Magit Cmds" th/magit-aux-commands))
  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)
  :custom
  (magit-refs-sections-hook '(magit-insert-error-header
                              magit-insert-branch-description
                              magit-insert-local-branches
                              ;;magit-insert-remote-branches
                              ))
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

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

(use-package hl-sexp
  :quelpa (hl-sexp :repo "ivanp7/hl-sexp" :fetcher github :commit "3b17a19c768079cc9fe8643bb1711137de4d6b02"))

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

(use-package clojure-mode
  :ensure t  
  :config
  ;; indent re-frame functions without so much leading white space
  (define-clojure-indent
   (reg-sub '(1))
   (reg-event-db '(1))
   (reg-event-fx '(1)))
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook #'my-clojure-configuration)
  (add-hook 'cider-mode-hook #'my-clojure-configuration)
  (add-hook 'cider-repl-mode-hook #'my-clojure-configuration))



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
  (setq cider-eval-spinner-type 'vertical-breathing)
  (setq cider-inspector-fill-frame nil)
  (setq cider-lein-parameters "trampoline repl :headless")
  (setq cider-inspector-max-coll-size 1)
  )

;; clj-refactor and dependencies
(require 'clj-refactor)
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
  (setq org-agenda-files '("~/org/notes.org" "~/org/tagebuch.org"))
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
        ;;(" AT" . 480)
        ("d" . 1440)
        ("m" . 43200)
        ("y" . 525960.0)))
    ;; show clock table durations in work days (Arbeitstage)
    ;;(setq org-duration-format '((" AT") (special . h:mm)))
    )


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
 '(default ((t (:family "Rec Mono Semicasual" :foundry "ARRW" :slant normal :weight regular :height 113 :width normal)))))
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

(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file and insert a link to this file."
  (interactive)
  ;; At the very first, check if the current buffer has a file name. If not, mode needs to be
  ;; PROMPT with DEFAULT_FOLDER
  (if (null (buffer-file-name)) (setq PROMPT t))
  (setq DEFAULT_FOLDER "c:/Dropbox/home/sdienst/org/img")
  (setq DEFAULT_FOLDER_WSL "/home/sdienst/org/img")
  (setq filename (format-time-string "%Y%m%d_%H%M%S.png"))
  (suspend-frame)
  (shell-command "/mnt/c/Windows/System32/snippingtool.exe /clip")
  (shell-command
   ;; This feeds the command to the Windows Snipping Tool, via powershell
   (concat "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -command 'Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save(\"" (concat DEFAULT_FOLDER "/" filename) "\",[System.Drawing.Imaging.ImageFormat]::Png); Write-Output ''clipboard content saved as file''} else {Write-Output ''clipboard does not contain image data''}'"))
  (if (file-exists-p (concat DEFAULT_FOLDER_WSL "/" filename))
      ;; Checks if the screenshot was created
      (insert
       ;; Inserts the result in the current ORG buffer
       (concat "[[file:" (concat DEFAULT_FOLDER_WSL "/" filename) "][" filename "]]"))
      (message "No screenshot was created, aborting.")))


(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit       _j_ump to current entry
        _c_ontinue   _q_uit       _d_isplay
        _o_ut        _t_imestamp  _r_eport
        _q_uery      _s_creenshot
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("j" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("s" my-org-screenshot)
  ("t" org-time-stamp)
  ("q" helm-org-rifle)
  ("?" (org-info "Clocking commands")))
(global-set-key (kbd "C-c o") 'hydra-org-clock/body)

(global-set-key (kbd "M-S-<f6>") 'delete-indentation) ;; use M-S <f6> instead of M-S 6 (broken key on keyboard...)
(global-set-key (kbd "<f6>") (lambda () (interactive) (insert "6")))
(global-set-key (kbd "S-<f6>") (lambda () (interactive) (insert "^")))

;; bookmarks, https://github.com/joodland/bm
(use-package bm
  :quelpa (bm :repo "joodland/bm" :fetcher github :commit "9a31c61f44e6f1033ca43bd7f3eb33ffdb2ca595")
  :ensure t
  :demand t
  :bind (("<C-f2>" . bm-toggle)
	 ("<f2>" . bm-next)
	 ("<S-f2>" . bm-previous)
	 ("<left-fringe> <mouse-5>" . bm-next-mouse)
	 ("<left-fringe> <mouse-8>" . bm-previous-mouse)
	 ("<left-fringe> <mouse-9>" . bm-toggle-mouse))
  :init
  (setq bm-restore-repository-on-load t)
  :config
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
  (add-hook 'after-revert-hook #'bm-buffer-restore))


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
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" "9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2" "95167736741bef2ad3e0543ed545dada5b95fef309883253387a2b14ab67db8d" "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" "b5c3c59e2fff6877030996eadaa085a5645cc7597f8876e982eadc923f597aca" "f5661fd54b1e60a4ae373850447efc4158c23b1c7c9d65aa1295a606278da0f8" "fc608d4c9f476ad1da7f07f7d19cc392ec0fb61f77f7236f2b6b42ae95801a62" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" "3e2039156049bd0661317137a3761d4c2ff43e8a2aa423f6db0c0e8df0197492" "4320a92406c5015e8cba1e581a88f058765f7400cf5d885a3aa9b7b9fc448fa7" "f4157511d5d4a31766a01ce6aeef7329a39afbfa61f6f6a96a29bb97dc9e00b1" "7887cf8b470098657395502e16809523b629249060d61607c2225d2ef2ad59f5" "e46fd158e0a01987e24e266a9dfb2d5a5202656aa1028d53ea814621a53c7899" "e2337309361eef29e91656c5e511a6cb8c54ce26231e11424a8873ea88d9093e" "11873c4fbf465b956889adfa9182495db3bf214d9a70c0f858f07f6cc91cbd47" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "53585ce64a33d02c31284cd7c2a624f379d232b27c4c56c6d822eff5d3ba7625" "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71" "21388667ce5ee0b375e6282f0d6c6b61588da6604d343bbb19389e6a54d3d00d" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "75615f00bca2d070186d217af34b1337badbc55e6a6d6c3f6929e4c3405c8079" "1d904ba8343822dff21ffae28a348975eafeb0734034ed5fa33d78bf2519e7cb" "39b0c917e910f32f43f7849d07b36a2578370a2d101988ea91292f9087f28470" "f58379453f93eb5152f87b19322feb3ac0393f4db6f9b5c6711a8aa6d2affe6a" "8878226b9bda9a16c2639a85d86af1a4eac16e88522587afa368d745006ef476" "1d4abd3ff9d32f7740f5b8c44fc5dd3e9625e8bde84315be58a865bc087d1714" "93fcfa172aad04bd7f86323c67c661b8cfeeda044632d5e5c8d54f1a47c38e8b" "b31e969329848ec0432a23850e1db997cf16c1b85845c73996f0d582e7403b27" "88380a535b965f1172ced30e751f5abf31047f15eae17adf323ba415a9408617" "87fd15a92096797894626d25d8f8a436b90ce8d97d499a98faea972944645fbd" "e129ee166c2cd586fb0831c711fc49977a065360461ba9ac78786be822ab4338" "c0350aed6dc98abdc329906a630b4cdf8ebb147cdf2a873e2648dfc0b904b2ab" "5744f67c2f2f5bb2bfe40dd72e590c8255bbaa9441c957a7524530077bc244cc" "c727910dd591caecd19c432ecc7afbcdecca1af23cd494bb60906aa613e7666a" "65ee857bb301e7a1cbc0822aeccf0bfa1b4dfa7199a759ab7b7b0504885233b7" "405654bde08b14bb90e4f8e6f900571f7c9827708ead86b13f6949566dde2065" "ba3399d98232527210e96e5f44c78a9aeb1cb159c6cd6dfa4348f2e08215bf19" default))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" ".clj-kondo" ".shadow-cljs" "target" "node_modules"))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" ".*\\.mtc.*$"))
 '(neo-window-fixed-size nil)
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %F")
             :image-converter
             ("dvipng -D %D -T tight -o %O %F")
             :transparent-image-converter
             ("dvipng -D %D -T tight -bg Transparent -o %O %F"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %F -quality 100 %O"))))
 '(package-check-signature nil)
 '(package-selected-packages
   '(modus-themes projectile lsp-treemacs lsp-ui lsp-mode org-sidebar vundo with-editor helm-org-rifle magit-section popup request treemacs company-racer ripgrep helm-org quelpa quelpa-use-package org-download lua-mode german-holidays zig-mode use-package lsp-java calfw calfw-org bm abyss-theme anti-zenburn-theme flycheck-clj-kondo xref-js2 js2-mode cider-hydra org-clock-convenience org-clock-csv markdown-mode+ htmlize magit-todos magit-org-todos ido-ubiquitous magit-popup markdown-preview-mode paredit which-key racer cargo rust-mode git-gutter-fringe hideshowvis ido-completing-read+ markdown-mode smex rainbow-delimiters neotree hl-sexp expand-region company clj-refactor cider-eval-sexp-fu ace-window ace-jump-mode))
 '(quelpa-update-melpa-p nil)
 '(racer-rust-src-path
   "/home/steffen/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
 '(reb-re-syntax 'string)
 '(safe-local-variable-values
   '((buffer-file-coding-system . utf-8-unix)
     (org-export-html-style-include-scripts)
     (org-export-html-style-include-default)))
 '(sp-base-key-bindings 'sp)
 '(speedbar-supported-extension-expressions
   '(".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".clj[sc]?"))
 '(tool-bar-mode nil)
 '(xref-js2-ignored-dirs '("bower_components" "node_modules" "build")))
