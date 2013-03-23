(server-start)

;; ----------
;; load-paths
;; ----------
(add-to-list 'load-path "~/.elisp/")
(add-to-list 'load-path "~/.elisp/theme")
(add-to-list 'load-path "~/.elisp/haskell-mode")
(add-to-list 'load-path "~/.elisp/magit")
(add-to-list 'load-path "~/.elisp/magit/contrib")
(add-to-list 'load-path "~/.elisp/git-commit-mode")
(add-to-list 'load-path "~/.elisp/clojure-mode")
(add-to-list 'load-path "~/.elisp/org-mode/lisp")
(add-to-list 'load-path "~/.elisp/yasnippet")
(add-to-list 'load-path "~/.elisp/auto-complete/lib/popup")
(add-to-list 'load-path "~/.elisp/auto-complete")
(add-to-list 'load-path "~/.elisp/ace-jump-mode")
(add-to-list 'load-path "~/.elisp/smex")
(add-to-list 'load-path "~/.elisp/scala-mode2")

(add-to-list 'custom-theme-load-path "~/.elisp/eltbus-theme")

;; ---------
;; Autoloads
;; ---------
(require 'whitespace)
(require 'filladapt)
(require 'tramp)
(require 'magit)
(require 'magit-bisect)
(require 'magit-simple-keys)
(require 'git-commit)
(require 'rebase-mode)
(require 'paredit)
(require 'clojure-mode)
(require 'inf-haskell)
(require 'haskell-ghci)
(require 'haskell-indent)
(require 'haskell-doc)
(require 'php-mode)
(require 'cscope)
(require 'csharp-mode)
(require 'rcirc-controls)
(require 'windmove)
(require 'framemove)
(require 'winner)
(require 'uniquify)
(require 'nnmairix)
(require 'edit-server)
(require 'kivy-mode)
(require 'cython-mode)
(require 'mirah-mode)
(require 'yasnippet)
(require 'auto-complete-config)
(require 'slim-mode)
(require 'revive)
(require 'ace-jump-mode)
(require 'smex)
(require 'scala-mode2)

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))
(add-to-list 'auto-mode-alist '("\\.mirah$" . mirah-mode))

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
	      winner-mode t
	      uniquify-buffer-name-style 'forward)

(set-face-attribute 'default nil :height 85)
(global-font-lock-mode 1)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
(load-theme 'eltbus t)

;; Remove toolbar, menubar, scrollbar and tooltips
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-scroll-bar-mode 'nil)

;; Set the default browser to Chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; General mode loading
(show-paren-mode t)
(savehist-mode t)
(ido-mode t)
(rcirc-track-minor-mode t)
(edit-server-start)

;; Unbind C-z. I don't want suspend
(when window-system
  (global-unset-key (kbd "C-z")))

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
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "C-M-DEL") #'(lambda () (interactive) (zap-to-char -1 32)))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-r") #'(lambda () (interactive) (revert-buffer nil t)))

;; ------
;; c-mode
;; ------
(add-hook 'c-mode-common-hook 'turn-on-filladapt-mode)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

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
 ido-default-file-method 'selected-window   ; open files in selected window
 ido-default-buffer-method 'selected-window ; open buffers in selected window
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
(global-set-key (kbd "C-x v") 'magit-status)
(setq magit-commit-all-when-nothing-staged nil
      magit-revert-item-confirm t
      magit-process-connection-type nil
      process-connection-type nil)

(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'git-commit-mode-hook 'flyspell-mode)

;; -----
;; rcirc
;; -----

;; General settings
(setq rcirc-server-alist
      '(("irc.freenode.net"
	 :port 6667
	 :nick "artagnon"
	 :full-name "Ramkumar Ramachandra")))

(defun gtalk ()
  (interactive)
  (rcirc-connect "localhost" "6667" "artagnon"))

;; Wrap long lines according to the width of the window
(add-hook 'window-configuration-change-hook
          '(lambda ()
	    (setq rcirc-fill-column (- (window-width) 2))))

(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-all-mode-buffers 'rcirc-mode))

;; ----
;; gnus
;; ----

(setq gnus-directory "~/.gnus"
      gnus-cache-directory "~/.gnus/cache"
      gnus-cache-active-file "~/.gnus/cache/active"
      gnus-message-directory "~/.gnus/mail"
      gnus-use-cache t
      gnus-cachable-groups "^nnimap"
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

;; agent
(setq gnus-agent-directory "~/.gnus/agent"
      gnus-agent t
      gnus-agent-cache t
      gnus-agent-consider-all-articles t
      gnus-agent-queue-mail t)

;; smtpmail for sending mail
(setq starttls-use-gnutls t
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      stmpmail-debug-info t
      smtpmail-debug-verb t)

;; Display tweaks
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ;; topics in groups

(defvar *ram-mails* "artagnon@gmail\\.com")
(setq gnus-extra-headers '(To Cc)
      nnmail-extra-headers gnus-extra-headers)

(defun gnus-user-format-function-j (headers)
  (let ((to (gnus-extra-header 'To headers)))
    (if (string-match *ram-mails* to)
        (if (string-match "," to) "›" "»")
        (if (or (string-match *ram-mails*
                              (gnus-extra-header 'Cc headers))
                (string-match *ram-mails*
                              (gnus-extra-header 'BCc headers)))
            "~"
            " "))))

(setq gnus-summary-line-format "%U%uj%z %(%[%15&user-date;%]  %-15,15f  %B%s%)\n"
      ;;gnus-summary-line-format "%U%R %~(pad-right 2)t%* %uj %B%~(max-right 30)~(pad-right 30)n  %~(max-right 90)~(pad-right 90)s %-135=%&user-date;\n"
      gnus-user-date-format-alist '((t . "%a %k:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")

(setq gnus-user-date-format-alist
      '(((* 1 3600) . "Minutes ago")
	((* 2 3600) . "An hour ago")
	((* 3 3600) . "2 hours ago")
	((* 4 3600) . "3 hours ago")
	((* 5 3600) . "4 hours ago")
	((* 6 3600) . "5 hours ago")
	((gnus-seconds-today) . "Today")
	((+ 86400 (gnus-seconds-today)) . "Yesterday")
	((* 3 86400) . "2 days ago")
	((* 4 86400) . "3 days ago")
	((* 6 86400) . "%A")
	((* 8 86400) . "A week ago")
	((gnus-seconds-month) . "This month, %e")
	((gnus-seconds-year) . "%b %e")
	(t . "%b %e, %y")))

;; Subscriptions
(setq gnus-default-subscribed-newsgroups t
      gnus-select-method '(nntp "news.gmane.org")
      gnus-secondary-select-methods
      '((nnimap "dovecot"
	 (nnimap-stream shell)
	 (imap-shell-program "/usr/lib/dovecot/imap 2>/dev/null")
	 (nnimap-need-unselect-to-notice-new-mail nil))))

; (nnimap "gmail"
;	 (nnimap-address "imap.gmail.com")
;	 (nnimap-server-port 993)
;	 (nnimap-stream ssl))

(setq gnus-parameters
      '(("nnimap\\+dovecot:INBOX"
	 (display . all)
         (expiry-target . delete)
         (expiry-wait . immediate))))

;; Bugfix: thread expire
(defun gnus-summary-kill-thread (&optional unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is zero, mark thread as expired.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((articles (gnus-summary-articles-in-thread))
	(hide (or (null unmark) (= unmark 0))))
    (save-excursion
      ;; Expand the thread.
      (gnus-summary-show-thread)
      ;; Mark all the articles.
      (while articles
	(gnus-summary-goto-subject (car articles))
	(cond ((null unmark)
	       (gnus-summary-mark-article-as-read gnus-killed-mark))
	      ((> unmark 0)
	       (gnus-summary-mark-article-as-unread gnus-unread-mark))
	      ((= unmark 0)
	       (gnus-summary-mark-article nil gnus-expirable-mark))
	      (t
	       (gnus-summary-mark-article-as-unread gnus-ticked-mark)))
	(setq articles (cdr articles))))
    ;; Hide killed subtrees when hide is true.
    (and hide
	 gnus-thread-hide-killed
	 (gnus-summary-hide-thread))
    ;; If hide is t, go to next unread subject.
    (when hide
      ;; Go to next unread subject.
      (gnus-summary-next-subject 1 t)))
  (gnus-set-mode-line 'summary))

;; Scoring
(add-hook 'message-sent-hoook 'gnus-score-followup-article)
(add-hook 'message-sent-hoook 'gnus-score-followup-thread)
(setq gnus-use-scoring t)

;; Speed hacks
(setq gc-cons-threshold 3500000
      gnus-use-correct-string-widths nil
      gnus-asynchronous t
      gnus-use-header-prefetch t)

;; Mairix
(define-key gnus-summary-mode-map
    (kbd "$ /") 'nnmairix-search)

;; -------------
;; flyspell-mode
;; -------------
(global-set-key "\C-cf" 'flyspell-check-previous-highlighted-word)

;; ---------
;; text-mode
;; ---------
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; ---------------
;; emacs-list-mode
;; ---------------
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))

;; ------------
;; haskell-mode
;; ------------
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; ---------
;; perl-mode
;; ---------
(defalias 'perl-mode 'cperl-mode)

;; -------
;; Paredit
;; -------
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp scheme inferior-lisp))

;; -----
;; Tramp
;; -----
(setq recentf-auto-cleanup 'never
      tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist
	     (quote (("^(?!.*kytes).*$" "\\`root\\'" "/ssh:%h:"))))

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
;; -------
;; sh-mode
;; -------
(setq sh-indentation 8
      sh-basic-offset 8)

;; I don't use sh-repeat
(add-hook 'sh-mode-hook
	  (lambda ()
	    (define-key sh-mode-map
	      (kbd "C-c C-r") #'(lambda () (interactive) (revert-buffer nil t)))))

;; --------
;; org-mode
;; --------
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c t") 'org-todo)
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
(setq org-default-notes-file "~/.notes")
(define-key global-map (kbd "C-c r") 'org-remember)

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
\\usepackage{alltt}
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
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; ---------
;; yasnippet
;; ---------
(setq yas-snippet-dirs
      '("~/.elisp/yasnippet/snippets" "~/.elisp/snippets"))
(yas-global-mode 1)

;; -------------
;; auto-complete
;; -------------
(add-to-list 'ac-dictionary-directories "~/.elisp/auto-complete/dict")
(ac-config-default)

;; ------
;; revive
;; ------
(setq revive:configuration-file "~/.elisp/.state.el")
(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "R" 'resume)
(define-key ctl-x-map "K" 'wipe)

;; ---
;; man
;; ---
(setq Man-notify-method 'aggressive)

;; -------------
;; ace-jump-mode
;; -------------
(global-set-key (kbd "C-c j") 'ace-jump-mode)

;; ----
;; smex
;; ----
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

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
