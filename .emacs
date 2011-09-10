(server-start)

;; ----------
;; load-paths
;; ----------
(add-to-list 'load-path "~/.elisp/")
(add-to-list 'load-path "~/.elisp/theme")
(add-to-list 'load-path "~/.elisp/slime")
(add-to-list 'load-path "~/dev/ublog.el")
(add-to-list 'load-path "~/.elisp/org-mode")
(add-to-list 'load-path "~/.elisp/haskell-mode")
(add-to-list 'load-path "~/.elisp/org-mode-contrib")
(add-to-list 'load-path "~/.elisp/mingus")

;; ---------
;; Autoloads
;; ---------
(require 'whitespace)
(require 'filladapt)
(require 'tramp)
(require 'slime)
(require 'magit)
(require 'magit-bisect)
(require 'magit-simple-keys)
(require 'git-commit)
(require 'rebase-mode)
(require 'paredit)
(require 'color-theme)
(require 'color-theme-hober2)
(require 'clojure-mode)
(require 'org-install)
(require 'quack)
(require 'inf-haskell)
(require 'haskell-ghci)
(require 'haskell-indent)
(require 'haskell-doc)
(require 'php-mode)
(require 'cscope)
(require 'csharp-mode)
(require 'ess)
(require 'rcirc-controls)
(require 'windmove)
(require 'framemove)
(require 'winner)

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))

;; ----------------------
;; General Customizations
;; ----------------------
(setq-default inhibit-startup-message t
	      font-lock-maximum-decoration t
	      visible-bell t
	      require-final-newline t
	      resize-minibuffer-frame t
	      column-number-mode t
	      display-battery-mode t
	      transient-mark-mode t
	      next-line-add-newlines nil
	      blink-matching-paren t
	      quack-pretty-lambda-p t
	      blink-matching-delay .25
	      vc-follow-symlinks t
	      indent-tabs-mode t
	      tab-width 8
	      c-basic-offset 8
	      edebug-trace t
	      fill-adapt-mode t
	      winner-mode t)

(set-face-attribute 'default nil :height 85)
(global-font-lock-mode 1)
(color-theme-hober2)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Remove toolbar, menubar, scrollbar and tooltips
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-scroll-bar-mode 'nil)

;; Set the default browser to Conkeror
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; General mode loading
(show-paren-mode t)
(savehist-mode t)
(ido-mode t)
(rcirc-track-minor-mode t)

;; Unbind C-z. I don't want suspend
(when window-system
  (global-unset-key "\C-z"))

;; ----------------------
;; Final newline handling
;; ----------------------
(setq require-final-newline t)
(setq next-line-extends-end-of-buffer nil)
(setq next-line-add-newlines nil)

;; -------------------
;; Everything in UTF-8
;; -------------------
(prefer-coding-system                   'utf-8)
(set-language-environment               'utf-8)
(set-default-coding-systems             'utf-8)
(setq file-name-coding-system           'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-write           'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-terminal-coding-system             'utf-8)
(set-clipboard-coding-system            'utf-8)
(set-selection-coding-system            'utf-8)
(setq default-process-coding-system     '(utf-8 . utf-8))
(add-to-list 'auto-coding-alist         '("." . utf-8))

;; ------------------
;; Custom Keybindings
;; ------------------
(global-set-key [(meta \])] 'forward-paragraph)
(global-set-key [(meta \[)] 'backward-paragraph)
(global-set-key "\C-\M-w" 'kill-ring-save-whole-line)
(global-set-key [C-M-backspace] #'(lambda () (interactive) (zap-to-char -1 32)))
(global-set-key "\C-z" 'jump-to-char)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xv" 'magit-status)
(global-set-key (kbd "<f5>") 'th-save-frame-configuration)
(global-set-key (kbd "<f6>") 'th-jump-to-register)

;; ---------------------
;; Style and indentation
;; ---------------------
(defmacro define-new-c-style (name derived-from style-alists tabs-p path-list)
  `(progn
     (add-hook 'c-mode-common-hook
	       (lambda ()
		 (c-add-style ,name
			      '(,derived-from (c-offsets-alist
					       ,style-alists)))))
     (add-hook 'c-mode-hook
	       (lambda ()
		 (let ((filename (buffer-file-name)))
		   (when (and filename
			      (delq nil
				    (mapcar (lambda (path)
					      (string-match (expand-file-name path)
							    filename))
					    ',path-list)))
		     (setq indent-tabs-mode ,tabs-p)
		     (c-set-style ,name)))))))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists with tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;; Syntax for define-new-c-style:
;; <style name> <derived from> <style alist> <tabs-p> <list of paths to apply to>

(define-new-c-style "linux-tabs-only" "linux" (arglist-cont-nonempty
					       c-lineup-gcc-asm-reg
					       c-lineup-arglist-tabs-only) t
					       ("~/"))

(define-new-c-style "subversion" "gnu" (inextern-lang 0) nil ("~/svn"))

;; --------------------------
;; Autofill and Adaptive fill
;; --------------------------
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)
(add-hook 'c-mode-common-hook 'turn-on-filladapt-mode)

;; ---
;; ido
;; ---
(setq 
 ido-ignore-buffers                 ; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
 ido-case-fold  t                   ; be case-insensitive
 ido-use-filename-at-point nil      ; don't use filename at point (annoying)
 ido-use-url-at-point nil           ; don't use url at point (annoying)
 ido-enable-flex-matching t         ; be flexible
 ido-max-prospects 6                ; don't spam my minibuffer
 ido-confirm-unique-completion nil  ; don't wait for RET with unique completion
 ido-max-directory-size 100000)

;; -----
;; Dired
;; -----
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "<return>")
	      'dired-find-alternate-file) ; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))))

(put 'dired-find-alternate-file 'disabled nil)

;; -----
;; magit
;; -----
(setq magit-commit-all-when-nothing-staged nil
      magit-revert-item-confirm t
      magit-process-connection-type nil
      process-connection-type nil)

;; -----
;; rcirc
;; -----

;; General settings
(setq rcirc-server-alist '(("irc.freenode.net" :nick "artagnon" :full-name "Ramkumar Ramachandra")))

(defun irc (oftc-p)
  (interactive "P")
  (if oftc-p
      (rcirc-connect "irc.oftc.net" "6667" "artagnon" "artagnon" "Ramkumar Ramachandra"
		     nil nil)
      (rcirc-connect "irc.freenode.net" "6667" "artagnon" "artagnon" "Ramkumar Ramachandra"
		     nil nil)))

(defun gtalk ()
  (interactive)
  (rcirc-connect "fran" "6667" "artagnon"))

;; Logging
(setq rcirc-log-flag "t"
      rcirc-log-directory "~/.emacs.d/rcirc-log")

(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-all-mode-buffers 'rcirc-mode))

(add-hook 'window-configuration-change-hook
          '(lambda ()
	    (setq rcirc-fill-column (- (window-width) 10))))

;; ---
;; ZNC
;; ---
(defun rcirc-detach-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
	       (eq (process-status (rcirc-buffer-process)) 'open))
      (with-rcirc-server-buffer
	(setq rcirc-buffer-alist
	      (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
	  (rcirc-send-string (rcirc-buffer-process)
			     (concat "DETACH " rcirc-target))))
    (setq rcirc-target nil)
    (kill-buffer buffer)))

(define-key rcirc-mode-map [(control c) (control d)] 'rcirc-detach-buffer)

;; ----------
;; Mode hooks
;; ----------
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(defalias 'perl-mode 'cperl-mode)

;; Paredit
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp scheme inferior-lisp))

;; ------
;; Scheme
;; ------
(setq scheme-program-name "mzscheme")

;; ---------------------
;; SLIME for Common Lisp
;; ---------------------
(setq inferior-lisp-program "sbcl"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///home/artagnon/ebooks/HyperSpec/")
(slime-setup)

;; -----
;; Tramp
;; -----
(setq recentf-auto-cleanup 'never
      tramp-default-method "ssh")

;; ---------
;; diff-mode
;; ---------
(define-key diff-mode-map [(meta q)] 'fill-paragraph)

;; ---------
;; mail-mode
;; ---------
(setq user-mail-address "artagnon@gmail.com"
      user-full-name "Ramkumar Ramachandra")

(add-hook 'mail-mode-hook
	  (lambda ()
	    (define-key mail-mode-map [(control c) (control c)]
	      (lambda ()
		(interactive)
		(save-buffer)
		(server-edit)))))

(add-hook 'mail-mode-hook
	  (lambda ()
	    (define-key mail-mode-map [(control c) (control k)]
	      (lambda ()
		(interactive)
		(revert-buffer t t nil)
		(server-edit)))))
;; --------
;; org-mode
;; --------
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-ct" 'org-todo)
(setq org-fast-tag-selection-include-todo t
      org-log-done 'note
      org-hide-leading-stars t
      org-agenda-files '("~/notes/diary"))

;; let windmove work in org-mode
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; org-remember
(org-remember-insinuate)
(setq org-default-notes-file "~/.notes")
(define-key global-map "\C-cr" 'org-remember)

;; org-mode and LaTeX Beamer

;; allow for export=>beamer by placing
;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
	     '("beamer"
	       "\\documentclass[8pt]{beamer}
\\beamertemplateballitem
\\usepackage{hyperref}
\\usepackage{color}
\\usepackage{listings}
\\usepackage{natbib}
\\usepackage{upquote}
\\usepackage{amsfonts}
\\lstset{frame=single, basicstyle=\\ttfamily\\small, upquote=false, columns=fixed, breaklines=true, keywordstyle=\\color{blue}\\bfseries, commentstyle=\\color{red}, numbers=left, xleftmargin=2em}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}"
		"\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}")))

;; ----------
;; LaTeX mode
;; ----------
(add-hook 'latex-mode-hook (lambda () (define-key tex-mode-map "\C-c\C-c" 'tex-compile-dvi)))
(defun tex-compile-dvi ()
  (interactive)
  (shell-command (concat "pdflatex -output-format dvi " (tex-main-file) "&")))

;; ------------------------
;; Useful utility functions
;; ------------------------
(defun revert-all-buffers ()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (if (string-match "\\*" (buffer-name buffer)) 
          (progn
            (setq list (cdr list))
            (setq buffer (car list)))
          (progn
            (set-buffer buffer)
            (revert-buffer t t t)
            (setq list (cdr list))
            (setq buffer (car list))))))
  (message "Refreshing open files"))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
            (progn (rename-file name new-name 1)
                   (rename-buffer new-name)
                   (set-visited-file-name new-name)
                   (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
        (progn (copy-file filename newname 1)
               (delete-file filename)
               (set-visited-file-name newname)
               (set-buffer-modified-p nil)
	       t))))

(defun reformat-hard-wrap (beg end)
  (interactive "r")
  (shell-command-on-region beg end "fmt -w2000" nil t))

(defmacro replace-in-file (from-string to-string)
  `(progn
     (goto-char (point-min))
     (while (search-forward ,from-string nil t)
       (replace-match ,to-string nil t))))

(defun cleanup-fancy-quotes ()
  (interactive)
  (progn
    (replace-in-file "’" "'")
    (replace-in-file "“" "\"")
    (replace-in-file "”" "\"")
    (replace-in-file "" "")))
