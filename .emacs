(server-start)

;; ----------
;; load-paths
;; ----------
(add-to-list 'load-path "~/.elisp/")
(add-to-list 'load-path "~/.elisp/theme")
(add-to-list 'load-path "~/.elisp/slime")
(add-to-list 'load-path "~/dev/ublog.el") ;; µblog.el development
(add-to-list 'load-path "~/.elisp/org-mode")
(add-to-list 'load-path "~/.elisp/haskell-mode")
(add-to-list 'load-path "~/.elisp/org-mode-contrib")

;; ---------
;; Autoloads
;; ---------
(require 'dtrt-indent)
(require 'filladapt)
(require 'tramp)
(require 'slime)
(require 'magit)
(require 'paredit)
(require 'color-theme)
(require 'color-theme-subdued)
(require 'clojure-mode)
(require 'saveplace)
(require 'ido)
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

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; ----------------------
;; General Customizations
;; ----------------------
(setq inhibit-startup-message t
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
      indent-tabs-mode nil
      tab-width 2)
(set-face-attribute 'default nil :height 95)
(global-font-lock-mode 1)
(color-theme-subdued)
(setq-default saveplace t
	      fill-adapt-mode t
	      dtrt-indent-mode t)
(setq edebug-trace t)
(defalias 'yes-or-no-p 'y-or-n-p)

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
(prefer-coding-system 'utf-8)
(set-language-environment "utf-8")
(set-default-coding-systems             'utf-8)
(setq file-name-coding-system           'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-write           'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-terminal-coding-system             'utf-8)
(set-clipboard-coding-system            'utf-8)
(set-selection-coding-system            'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(add-to-list 'auto-coding-alist '("." . utf-8))

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
(define-key magit-mode-map "\C-uc" #'(lambda ()
				       "magit-commit-amend"
				       (interactive)
				       (magit-log-edit-toggle-amending) (magit-log-edit)))

;; --------------------------
;; Autofill and Adaptive fill
;; --------------------------
(add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)
(add-hook 'c-mode-hook 'turn-on-filladapt-mode)

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
   ido-confirm-unique-completion nil) ; don't wait for RET with unique completion
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
;; rcirc
;; -----

;; General settings
(setf rcirc-server-alist '(("irc.freenode.net" :nick "artagnon" :full-name "Ramkumar Ramachandra")))

(defun irc ()
  (interactive)
  (rcirc-connect "irc.freenode.net" "6667" "artagnon"))

(defun gtalk ()
  (interactive)
  (rcirc-connect "kytes" "6667" "artagnon"))

;; Logging
(setf rcirc-log-flag "t"
      rcirc-log-directory "~/.emacs.d/rcirc-log")

;; Reconnect on demand
;; Derived from code on
;; <http://www.emacswiki.org/cgi-bin/wiki/rcircReconnect>
;; but more general in that the process does not need to be alive.
(defun-rcirc-command reconnect (arg)
  "Reconnect the server process."
  (interactive "i")
  (if (buffer-live-p rcirc-server-buffer)
      (with-current-buffer rcirc-server-buffer
	(let ((reconnect-buffer (current-buffer))
	      (server (or rcirc-server rcirc-default-server))
	      (port (if (boundp 'rcirc-port) rcirc-port rcirc-default-port))
	      (nick (or rcirc-nick rcirc-default-nick))
	      channels)
	  (dolist (buf (buffer-list))
	    (with-current-buffer buf
	      (when (equal reconnect-buffer rcirc-server-buffer)
		(remove-hook 'change-major-mode-hook
			     'rcirc-change-major-mode-hook)
		(and (boundp 'rcirc-initial-target)
		     (rcirc-channel-p rcirc-initial-target)
		     (setq channels (cons rcirc-initial-target channels)))
		  )))
	  (if process (delete-process process))
	  (rcirc-connect server port nick
			 nil
			 nil
			 channels)))))

;; Tab completion
(defun rcirc-complete-nick ()
  "Cycle through nick completions from list of nicks in channel."
  (interactive)
  (if (eq last-command this-command)
      (setq rcirc-nick-completions
            (append (cdr rcirc-nick-completions)
                    (list (car rcirc-nick-completions))))
    (setq rcirc-nick-completion-start-offset
          (- (save-excursion
               (if (re-search-backward " " rcirc-prompt-end-marker t)
                   (1+ (point))
                 rcirc-prompt-end-marker))
             rcirc-prompt-end-marker))
    (setq rcirc-nick-completions
          (let ((completion-ignore-case t))
            (all-completions
	     (buffer-substring
	      (+ rcirc-prompt-end-marker
		 rcirc-nick-completion-start-offset)
	      (point))
	     (append (rcirc-channel-nicks (rcirc-buffer-process)
					  rcirc-target)
		     (rcirc-commands))))))
  (let ((completion (car rcirc-nick-completions)))
    (when completion
      (rcirc-put-nick-channel (rcirc-buffer-process) completion rcirc-target)
      (delete-region (+ rcirc-prompt-end-marker
			rcirc-nick-completion-start-offset)
		     (point))
      (insert (concat completion
                      (if (= (+ rcirc-prompt-end-marker
                                rcirc-nick-completion-start-offset)
                             rcirc-prompt-end-marker)
                          (if (eq (aref completion 0) ?/) " " ": ")))))))

(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-all-mode-buffers 'rcirc-mode))

(add-hook 'window-configuration-change-hook
          '(lambda ()
             (setq rcirc-fill-column (- (window-width) 10))))

(defun rcirc-commands ()
  "Return a list of defined IRC commands.
If a command called rcirc-cmd-foo exists, the IRC command /FOO
will be part of the list returned."
  (let ((commands))
    (mapatoms (lambda (sym)
		(let ((name (symbol-name sym)))
		  (when (and (commandp sym)
			     (string= (substring name 0 (min (length name) 10))
				      "rcirc-cmd-"))
		    (setq commands (cons (concat"/" (upcase (substring name 10)))
					 commands))))))
    commands))

;; ----------
;; Mode hooks
;; ----------
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
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
(setq inferior-lisp-program "sbcl")
(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///home/artagnon/ebooks/HyperSpec/")
(slime-setup)

;; -----
;; Tramp
;; -----
(setq recentf-auto-cleanup 'never)
(setq tramp-default-method "ssh")
(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

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

(defun kill-ring-save-whole-line (&optional interactive)
  (interactive "p")
  (save-excursion
    (let ((beg (progn (beginning-of-line) (point)))
	  (end (progn (end-of-line) (point))))
	(kill-new (filter-buffer-substring beg end)))
    (if (interactive-p)
	(let* ((killed-text (current-kill 0))
	       (message-len (min (length killed-text) 40)))
	  (message "Saved text from \"%s\""
		   (substring killed-text 0 message-len))))))

(defun jump-to-char (arg char)
  "Jump to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncJump to char: ")
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (search-forward (char-to-string char) nil nil arg))

;; Reformat hard-wrapped regions
(defun reformat-hard-wrap (beg end)
   (interactive "r")
   (shell-command-on-region beg end "fmt -w2000" nil t))
