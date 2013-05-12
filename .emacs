(server-start)

;; ----------
;; load-paths
;; ----------
(add-to-list 'load-path "~/.elisp/")
(add-to-list 'load-path "~/.elisp/theme")
(add-to-list 'load-path "~/.elisp/haskell-mode")
(add-to-list 'load-path "~/.elisp/magit")
(add-to-list 'load-path "~/.elisp/magit/contrib")
(add-to-list 'load-path "~/.elisp/git-modes")
(add-to-list 'load-path "~/.elisp/clojure-mode")
(add-to-list 'load-path "~/.elisp/org-mode/lisp")
(add-to-list 'load-path "~/.elisp/yasnippet")
(add-to-list 'load-path "~/.elisp/auto-complete/lib/popup")
(add-to-list 'load-path "~/.elisp/auto-complete")
(add-to-list 'load-path "~/.elisp/ace-jump-mode")
(add-to-list 'load-path "~/.elisp/smex")
(add-to-list 'load-path "~/.elisp/scala-mode2")

(add-to-list 'custom-theme-load-path "~/.elisp/zenburn-emacs")

;; ---------
;; Autoloads
;; ---------
(require 'whitespace)
(require 'filladapt)
(require 'tramp)
(require 'magit)
(require 'magit-bisect)
(require 'magit-simple-keys)
(require 'rebase-mode)
(require 'git-commit-mode)
(require 'gitignore-mode)
(require 'gitconfig-mode)
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
(require 'ace-jump-mode)
(require 'smex)
(require 'scala-mode2)
(require 'recentf)
(require 'saveplace)

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
	      uniquify-buffer-name-style 'forward
	      save-place t)

(set-face-attribute 'default nil :height 85)
(global-font-lock-mode 1)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
(load-theme 'zenburn t)

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
(electric-indent-mode 1)
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
(global-set-key (kbd "C-c C-r") #'(lambda () (interactive) (revert-buffer nil t)))
(global-set-key (kbd "C-S-n") #'(lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") #'(lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "M-j") #'(lambda () (interactive) (join-line -1)))

;; ------
;; c-mode
;; ------
(add-hook 'c-mode-common-hook 'turn-on-filladapt-mode)

(defmacro define-new-c-style (name derived-from style-alists tabs-p match-path)
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
			      (string-match (expand-file-name ,match-path) filename))
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
;; <style name> <derived from> <style alist> <tabs-p> <path to apply to>

(define-new-c-style "linux-tabs-only" "linux" (arglist-cont-nonempty
					       c-lineup-gcc-asm-reg
					       c-lineup-arglist-tabs-only) t
					       "~")

(define-key c-mode-map [(meta j)] #'(lambda () (interactive) (join-line -1)))

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

(global-set-key (kbd "C-x l") 'magit-log-simple)
(global-set-key (kbd "C-x h") #'(lambda () (interactive)
				  (magit-show-commit
				   (magit-rev-parse "HEAD") nil t t)))

;; git-modes
(setq git-commit-confirm-commit nil)

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

;; -------------
;; flyspell-mode
;; -------------
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(global-set-key (kbd "C-c f") 'flyspell-check-previous-highlighted-word)

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
(define-key diff-mode-map [(meta backspace)] 'backward-kill-word)

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
(defmacro define-new-sh-style (indentation basic-offset match-path)
  `(add-hook 'sh-mode-hook
	     (lambda ()
	       (let ((filename (buffer-file-name)))
		 (when (and filename
			    (string-match (expand-file-name ,match-path) filename))
		   (setq sh-indetnation ,indentation)
		   (setq sh-basic-offset ,basic-offset))))))

(define-new-sh-style 2 2 "~/src/zsh")
(define-new-sh-style 8 8 "~")

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

;; ---
;; man
;; ---
(setq Man-notify-method 'aggressive)

;; -------------
;; ace-jump-mode
;; -------------
(global-set-key (kbd "M-h") 'ace-jump-mode)

;; -------
;; recentf
;; -------
(setq recentf-max-saved-items 200
      recentf-max-menu-items 50)
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file
	 (ido-completing-read "Choose recent file: "
			      (mapcar #'abbreviate-file-name recentf-list) nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

;; ----
;; smex
;; ----
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ------------------------
;; Useful utility functions
;; ------------------------
(defun rename-file-and-buffer ()
  "Renames the current buffer and the file it's visiting"
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
	  (progn (rename-file name new-name 1)
		 (rename-buffer new-name)
		 (set-visited-file-name new-name)
		 (set-buffer-modified-p nil)))))))

(defun delete-file-and-buffer ()
  "Removes file connected to current buffer and kills buffer"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-file-and-buffer)

(defun smart-kill-whole-line (&optional arg)
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

;; [(control shift backspace)]
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)

(defun smart-open-line ()
  "Shortcut for C-e RET"
  (interactive)
  (move-end-of-line nil)
  (newline))

(global-set-key [(shift return)] 'smart-open-line)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)
