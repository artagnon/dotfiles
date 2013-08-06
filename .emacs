(server-start)

;; ----------
;; load-paths
;; ----------
(add-to-list 'load-path "~/.elisp/")
(add-to-list 'load-path "~/.elisp/haskell-mode")
(add-to-list 'load-path "~/.elisp/magit")
(add-to-list 'load-path "~/.elisp/magit/contrib")
(add-to-list 'load-path "~/.elisp/git-modes")
(add-to-list 'load-path "~/.elisp/clojure-mode")
(add-to-list 'load-path "~/.elisp/org-mode/lisp")
(add-to-list 'load-path "~/.elisp/auto-complete/lib/popup")
(add-to-list 'load-path "~/.elisp/auto-complete")
(add-to-list 'load-path "~/.elisp/ace-jump-mode")
(add-to-list 'load-path "~/.elisp/smex")
(add-to-list 'load-path "~/.elisp/scala-mode2")
(add-to-list 'load-path "~/.elisp/rust-mode")
(add-to-list 'load-path "~/.elisp/llvm-mode")
(add-to-list 'load-path "~/.elisp/dash")
(add-to-list 'load-path "~/.elisp/s")
(add-to-list 'load-path "~/.elisp/projectile")
(add-to-list 'load-path "~/.elisp/flx")
(add-to-list 'load-path "~/.elisp/go-mode")
(add-to-list 'load-path "~/.elisp/lua-mode")

(add-to-list 'custom-theme-load-path "~/.elisp/zenburn-emacs")

;; ---------
;; Autoloads
;; ---------
(require 'whitespace)
(require 'filladapt)
(require 'tramp)
(require 'magit)
(require 'magit-bisect)
(require 'magit-blame)
(require 'magit-simple-keys)
(require 'rebase-mode)
(require 'git-commit-mode)
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'paredit)
(require 'clojure-mode)
(require 'cscope)
(require 'windmove)
(require 'framemove)
(require 'winner)
(require 'uniquify)
(require 'nnmairix)
(require 'edit-server)
(require 'cython-mode)
(require 'auto-complete-config)
(require 'slim-mode)
(require 'ace-jump-mode)
(require 'smex)
(require 'scala-mode2)
(require 'recentf)
(require 'saveplace)
(require 'rust-mode)
(require 'haskell-mode-autoloads)
(require 'kconfig-mode)
(require 'llvm-mode)
(require 'tablegen-mode)
(require 'dash)
(require 's)
(require 'projectile)
(require 'flx-ido)
(require 'go-mode)
(require 'lua-mode)

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("mutt-.*-" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

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
(electric-indent-mode 1)
(edit-server-start)

;; Unbind C-z. I don't want suspend
(when window-system
  (global-unset-key [(control z)]))

;; y-or-n-p please
(defalias 'yes-or-no-p 'y-or-n-p)

;; only stop-the-world gc every 20M
(setq gc-cons-threshold 20000000)

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
(global-set-key [(control c) (control r)] #'(lambda () (interactive) (revert-buffer nil t)))
(global-set-key [(control shift n)] #'(lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key [(control shift p)] #'(lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key [(meta j)] #'(lambda () (interactive) (join-line -1)))

;; ------
;; c-mode
;; ------
(add-hook 'c-mode-common-hook 'turn-on-filladapt-mode)

(defmacro define-new-c-style (name derived-from style-alist match-path)
  `(progn
     (c-add-style ,name
		  '(,derived-from ,@style-alist))
     (add-hook 'c-mode-hook
	       (lambda ()
		 (let ((filename (buffer-file-name)))
		   (when (and filename
			      (string-match (expand-file-name ,match-path) filename))
		     (c-set-style ,name)))))
     (add-hook 'c++-mode-hook
	       (lambda ()
		 (let ((filename (buffer-file-name)))
		   (when (and filename
			      (string-match (expand-file-name ,match-path) filename))
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
;; <style-name> <derived-from> <style-alist> <match-path>

(define-new-c-style "llvm" "gnu" ((fill-column . 80)
				  (c++-indent-level . 2)
				  (c-offsets-alist . ((innamespace 0))))
  "~/src/llvm")

(define-new-c-style "emacs" "gnu" nil "~/src/emacs")

(define-new-c-style "linux-tabs-only" "linux" ((indent-tabs-mode . t)
					       (c-offsets-alist
						(arglist-cont-nonempty
						 c-lineup-gcc-asm-reg
						 c-lineup-arglist-tabs-only)))
  "~")

(define-key c-mode-map [(meta j)] #'(lambda () (interactive) (join-line -1)))

;; ---
;; ido
;; ---
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(setq
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" "^\*Ido")
 ido-case-fold t
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window
 ido-max-directory-size nil)

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
(setq vc-handled-backends '(Git))
(global-set-key [(control x) (v)] 'magit-status)
(setq magit-commit-all-when-nothing-staged t
      magit-revert-item-confirm t
      magit-process-connection-type nil
      process-connection-type nil)

(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)

(global-set-key [(control x) (h)] #'(lambda () (interactive)
				  (magit-show-commit
				   (magit-rev-parse "HEAD") nil t t)))

;; git-modes
(setq git-commit-confirm-commit nil)

;; -------------
;; flyspell-mode
;; -------------
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(global-set-key [(control c) (f)] 'flyspell-check-previous-highlighted-word)

;; ----------
;; projectile
;; ----------
(projectile-global-mode)

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

(defmacro define-new-pl-style (offset match-path)
  `(add-hook 'cperl-mode-hook
	     (lambda ()
	       (let ((filename (buffer-file-name)))
		 (when (and filename
			    (string-match (expand-file-name ,match-path) filename))
		   (setq cperl-indent-level ,offset)
		   (setq cperl-brace-offset 0)
		   (setq cperl-continued-brace-offset ,(- offset))
		   (setq cperl-label-offset ,(- offset))
		   (setq cperl-continued-statement-offset ,offset)
		   (setq cperl-merge-trailing-else nil))))))

(define-new-pl-style 4 "~/src/linux")
(define-new-pl-style 8 "~/src/git")

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
(define-key diff-mode-map [(control c) (control r)] #'(lambda () (interactive) (revert-buffer nil t)))

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

;; -----------------
;; shell-script-mode
;; -----------------
(defmacro define-new-sh-style (indentation basic-offset match-path)
  `(add-hook 'sh-mode-hook
	     (lambda ()
	       (let ((filename (buffer-file-name)))
		 (when (and filename
			    (string-match (expand-file-name ,match-path) filename))
		   (setq sh-indetnation ,indentation)
		   (setq sh-basic-offset ,basic-offset)
		   (setq sh-indent-for-case-label 0)
		   (setq sh-indent-for-case-alt '+))))))

(define-new-sh-style 8 8 "~")

;; I don't use sh-repeat
(add-hook 'sh-mode-hook
	  (lambda ()
	    (define-key sh-mode-map
	      [(control c) (control r)] #'(lambda () (interactive) (revert-buffer nil t)))))


;; -----------
;; python-mode
;; -----------
;; I use C-c > for py-shift-region-right
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key py-mode-map
	      [(control c) (control r)] #'(lambda () (interactive) (revert-buffer nil t)))))

;; --------
;; org-mode
;; --------
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
(global-set-key [(meta h)] 'ace-jump-word-mode)

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

(global-set-key [(control c) (r)] 'recentf-ido-find-file)

;; ----
;; smex
;; ----
(smex-initialize)
(global-set-key [remap execute-extended-command] 'smex)

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

(global-set-key [(control x) (control k)] 'delete-file-and-buffer)

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
