(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-2)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(tool-bar-mode nil)
 '(org-agenda-files (quote ("~/org/gtd.org" "~/org/notes.org" "~/foo.org" "~/org/tagebuch.org")) t))
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
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize) 
;;;;;;; package installation urls ;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
;; soft wrap long lines
(global-visual-line-mode)
;; bind M-/ to hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
  ;;;;
  ;;;; cygwin support
  ;;;;
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "d:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
  	     (file-readable-p cygwin-root))
    
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name) 
    (setq explicit-shell-file-name shell-file-name)
    (setq explicit-shell-args '("--login" "-i"))
    
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    ; Add Cygwin Info pages
    (add-to-list 'Info-default-directory-list
       (concat cygwin-root "/usr/share/info"))))

;; enable slime to translate cygwin<->windows paths
(when (string-match "cygwin$"  system-configuration)                                                                                           
  (setq slime-to-lisp-filename-function (lambda (filename)                                                                                     
                                          (replace-regexp-in-string                                                                            
                                           "\n" "" (shell-command-to-string                                                                    
                                                    (format "cygpath.exe --windows %s" filename))))                                            
        lisp-to-slime-filename-function (lambda (filename)                                                                                     
                                          (replace-regexp-in-string                                                                            
                                           "\n" "" (shell-command-to-string                                                                    
                                                    (format "cygpath.exe --unix %s filename"))))))                                             
                                                                                                             


;; use rainbow parentheses everywhere
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
;; highlight matching parentheses
(show-paren-mode 1)
;; paredit mode for emacs lisp and clojure
(require 'paredit)
(add-hook 'emacs-lisp-mode 'enable-paredit-mode)


;; syntax highlight clojure in slime repl
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))
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
;; colors for magit diffs
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))
;;;; clojure specific configurations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show fn as lambda in clojure files
(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
				 (0 (progn (compose-region (match-beginning 1)
							   (match-end 1)
							   "\u03bb") nil))))))
(defun my-clojure-configuration ()
  (highlight-parentheses-mode t)
  (paredit-mode t)
  (esk-pretty-fn)
  (turn-on-eldoc-mode))

(add-hook 'clojure-mode-hook 'my-clojure-configuration)
(add-hook 'clojurescript-mode-hook 'my-clojure-configuration)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")
(setq nrepl-popup-stacktraces nil)

(global-set-key [f8] 'other-frame)
(global-set-key [f7] 'paredit-mode)
(global-set-key [f9] 'cider-jack-in)

;;;;;;;;;;; auto complete everywhere ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Auto-Complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;; ac-nrepl (Auto-complete for the nREPL)
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

;; Poping-up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; CSS and Rainbow modes 
(defun all-css-modes() (css-mode) (rainbow-mode)) 
;; load css and rainbow mode for css files 
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes)) 

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
;; enable reveal.js export for org-mode
;; see https://github.com/yjwen/org-reveal/ for details
;(require 'ox-reveal)

;; Enable column number mode
(column-number-mode t)
(display-time)

;; GMail via gnus
(require 'gnus)
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; use m-tab for completion under windows instead of program switching
					;(w32-register-hot-key [M-tab])

;; use bash completions in shell

(autoload 'bash-completion-dynamic-complete "bash-completion" "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
	  'bash-completion-dynamic-complete)

;; ess mode for R statistics interaction
(require 'ess-site)

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
;; enable eldoc in nrepl
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
;; remember places in open files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; haskell
(setq haskell-program-name "D:/haskell/bin/ghci.exe")

(autoload 'markdown-mode "markdown-mode"
        "Major mode for editing Markdown files" t)
     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
