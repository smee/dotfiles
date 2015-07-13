(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(org-agenda-files
   (quote
    ("~/org/gtd.org" "~/org/notes.org" "~/foo.org" "~/org/tagebuch.org")))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; set load paths
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path)) 
;; ensure elpa packages get loaded
(require 'package)  
;; activate installed packages
(package-initialize) 
;;;;;;; package installation urls ;;;;;;;;;;;;;;;;;;;
(setq package-archives '(;("ELPA" . "http://tromey.com/elpa/") 
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("MELPA" . "http://melpa.org/packages/")
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
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'ido-ubiquitous 'smex 'cider 'rainbow-delimiters 'smartparens 'cider 'cider-eval-sexp-fu)   

;; activate installed packages
(package-initialize) 
(add-to-list 'package-pinned-packages '(cider . "MELPA") t)

;; soft wrap long linesq
(global-visual-line-mode)
;; bind M-/ to hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
  ;;;;
  ;;;; cygwin support
  ;;;;

;; use rainbow parentheses everywhere
(require 'rainbow-delimiters)
;; highlight matching parentheses
(show-paren-mode 1)
;; paredit mode for emacs lisp and clojure
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode #'smartparens-strict-mode)

;; use ido-mode everywhere
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-work-directory-list '("~/" "d:/dropbox/workspaces/android-workspace"))

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
                                   (smex-major-mode-commands)))
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)
;;;; clojure specific configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cider-eval-sexp-fu)
;; show fn as lambda in clojure files
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
				 (0 (progn (compose-region (match-beginning 1)
							   (match-end 1)
							   "\u03bb") nil))))))
(defun my-clojure-configuration ()
  (smartparens-strict-mode t)
  (esk-pretty-fn)
  (turn-on-eldoc-mode))
(add-hook 'clojure-mode-hook 'my-clojure-configuration)

(add-hook 'cider-mode-hook 'my-clojure-configuration)
(setq nrepl-log-messages t)

(global-set-key [f8] 'other-frame)
(global-set-key [f7] 'smartparens-mode)
(global-set-key [f9] 'cider-jack-in)


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
(setq org-directory "~/org")
(setq org-mobile-directory (concat org-directory "/mobileorg"))
(setq org-agenda-files '("~/org/notes.org" "~/org/tagebuch.org"))
(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "notes.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "tagebuch.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))    
(setq org-mobile-force-id-on-agenda-items nil)
;; custom agendas for mobileorg
(setq org-agenda-custom-commands
      '(("w" todo "TODO")
	("h" agenda "" ((org-agenda-show-all-dates nil)))
	("W" agenda "" ((org-agenda-ndays 21)
			(org-agenda-show-all-dates nil)))
	("A" agenda ""
	 ((org-agenda-ndays 1)
	  (org-agenda-overriding-header "Today")))))

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
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(f12)] 'recentf-open-files)
;; ace-jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-SPC") 'ace-jump-mode)
;; remember places in open files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(autoload 'markdown-mode "markdown-mode"
        "Major mode for editing Markdown files" t)
     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; make sure we use UTF-8 as our standard encoding for text files     
(modify-coding-system-alist 'file "" 'utf-8-unix)

;; hide ^M characters when editing files with windows carriage returns
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'clojure-mode 'remove-dos-eol)
