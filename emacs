(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Okular"
      ("okular --unique %o#src:%n%b")
      "/usr/bin/okular"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(cider-eval-spinner-type (quote vertical-breathing))
 '(cider-lein-parameters "trampoline repl :headless")
 '(custom-enabled-themes (quote (tango-dark)))
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(magit-todos-require-colon nil)
 '(neo-hidden-regexp-list
   (quote
    ("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" ".*\\.mtc.*$")))
 '(org-agenda-files
   (quote
    ("~/org/gtd.org" "~/org/notes.org" "~/foo.org" "~/org/tagebuch.org")))
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (magit-todos magit-org-todos ido-ubiquitous magit magit-popup markdown-preview-mode org paredit which-key helm racer cargo rust-mode git-gutter-fringe hideshowvis ido-completing-read+ markdown-mode smex rainbow-delimiters projectile neotree hl-sexp expand-region company clj-refactor cider-eval-sexp-fu ace-window ace-jump-mode)))
 '(racer-rust-src-path
   "/home/steffen/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
 '(reb-re-syntax (quote string))
 '(sp-base-key-bindings (quote sp))
 '(speedbar-supported-extension-expressions
   (quote
    (".org" ".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".js" ".f\\(90\\|77\\|or\\)?" ".ad[abs]" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" ".ma?k" "[Mm]akefile\\(\\.in\\)?" ".clj[sc]?")))
 '(tool-bar-mode nil))

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

;; activate installed packagesf
(package-initialize) 
(add-to-list 'package-pinned-packages '(cider . "MELPA") t)

(ensure-package-installed 'ido-completing-read+
			  'smex
			  'cider
			  'rainbow-delimiters
			  'paredit
			  'cider
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
			  'org)
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

(require 'magit-todos)
(magit-todos-mode 1)

;; use ido-mode everywhere
(ido-mode t)
(ido-everywhere t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-work-directory-list '("~/" "/home/steffen/Dropbox/workspaces/current-workspace"))

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
  (turn-on-eldoc-mode)
  (remove-dos-eol)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c RET")
  (yas-minor-mode 1)
  (paredit-mode 1)
  (hl-sexp-mode 1)
  (company-mode 1))

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(global-set-key (kbd "<f5>") #'hs-toggle-hiding)
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

;; enable figwheel CLJS repl in active NREPL connection
(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))
(global-set-key (kbd "C-c C-f") 'cider-figwheel-repl)
;; clj-refactor and dependencies
(require 'clj-refactor)

(add-hook 'clojure-mode-hook #'my-clojure-configuration)
(add-hook 'cider-mode-hook #'my-clojure-configuration)
(add-hook 'cider-repl-mode-hook #'my-clojure-configuration)
(setq nrepl-log-messages t)

(global-set-key [f8] 'other-frame)
(global-set-key [f7] 'paredit-mode)
(global-set-key [f9] 'cider-jack-in)
(global-set-key [f10] 'cider-figwheel-repl)


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
	 "* TODO %?\n  %i\n  %a"  :clock-in t :clock-resume t)
	("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree "tagebuch.org")
	 "* %?\nEntered on %U\n  %i\n  %a" :clock-in t :clock-resume t)))
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
(setq ace-jump-mode-submode-list
      '(ace-jump-char-mode              ;; the first one always map to : C-c SPC 
	ace-jump-word-mode              ;; the second one always map to: C-u C-c SPC            
        ace-jump-line-mode))           ;; the third one always map to ：C-u C-u C-c SPC)
(define-key global-map (kbd "C-SPC") 'ace-jump-mode)
;; ace-window
(require 'ace-window)
(define-key global-map (kbd "M-p") 'ace-window)

;; remember places in open files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

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
;; enable projectile project manager mode globally
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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

;; Rust support
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
