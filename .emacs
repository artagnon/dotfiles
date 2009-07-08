(server-start)

;; ----------
;; load-paths
;; ----------
(add-to-list 'load-path "~/.elisp/")  ;; generic
(add-to-list 'load-path "~/.elisp/org-mode")  ;; org-mode
;;(add-to-list 'load-path "~/.elisp/swank-clojure")  ;; swank-clojure
(add-to-list 'load-path "~/.elisp/slime")  ;; slime
(add-to-list 'load-path "~/dev/twitel") ;; twitel development

;; ---------
;; Autoloads
;; ---------
(require 'tramp)
(require 'ido)
(require 'slime)
(require 'magit)
(require 'paredit)
(require 'socks)
(require 'color-theme)
(require 'anything-config)
(require 'basic-edit-toolkit)
(require 'doremi)
(require 'doremi-cmd)
(require 'doremi-frm)
(require 'stumpwm-mode)
(require 'traverselisp)
(require 'clojure-mode)
(require 'saveplace)
(require 'moz)
(require 'javascript-mode "javascript")
(require 'icicles)
(require 'org-install)
(require 'quack)
;;(require 'swank-clojure)
;;(custom-set-variables '(swank-clojure-jar-path "~/.clojure/clojure.jar"))
;;(require 'swank-clojure-autoload)

;; ----------------
;; auto-mode-alists
;; ----------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; ----------------------
;; General Customisations
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
      blink-matching-delay .25)
(global-font-lock-mode 1)
(color-theme-dark-laptop)
(setq-default saveplace t)
(setq edebug-trace t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove toolbar, menubar, scrollbar and tooltips
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-scroll-bar-mode 'nil)

;; Set the default browser to Conkeror
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/local/bin/conkeror")

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
(global-set-key [(control meta y)] #'(lambda ()
				       "yank-pop-inverse"
				       (interactive)
				       (yank-pop -1)))
(global-set-key [(meta \])] 'forward-paragraph)
(global-set-key [(meta \[)] 'backward-paragraph)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-xv" 'magit-status)
(global-set-key (kbd "<f5>") 'th-save-frame-configuration)
(global-set-key (kbd "<f6>") 'th-jump-to-register)
(define-key magit-mode-map "\C-uc" #'(lambda ()
				       "magit-commit-amend"
				       (interactive)
				       (magit-log-edit-toggle-amending) (magit-log-edit)))

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

;; ---
;; ERC
;; ---
(setq socks-override-functions 1)
(setq socks-noproxy '("localhost"))  ;; do not conect via socks proxy for localhost (to use bitlbee)
(setq socks-server '("Default server" "127.0.0.1" 9050 5))  ;; configure socks proxy v5 (Tor)
(setq erc-server-connect-function 'socks-open-network-stream)  ;; connect via socks proxy
(setq erc-modules '(autoaway autojoin button completion fill irccontrols match menu netsplit noncommands readonly ring scrolltobottom services stamp track))
(setq erc-log-channels-directory "~/.erc/logs/")  ;; erc logging
(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)
(setq erc-auto-discard-away t) ;; discard autoaway when active on ERC
(setq erc-autoaway-idle-seconds 600) ;; autoaway idle time is 10 mins
(setq erc-autoaway-message "[Autoaway] Wandered off") ;; autoaway message
(setq erc-auto-set-away t) ;; autoaway when ERC is idle, not emacs itself

;; Show relevant notifications only
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
				"324" "328" "329" "332" "333" "353" "477"))

;; Change header line face if disconnected
(defface erc-header-line-disconnected
    '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
	  (t 'erc-header-line-disconnected))))

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

;; -----
;; rcirc
;; -----

;; General settings
(setf rcirc-server-alist '(("irc.freenode.net" :nick "artagnon" :full-name "Ramkumar R" :channels (""))))

;; Logging
(add-hook 'rcirc-print-hooks 'rcirc-write-log)
(defcustom rcirc-log-directory "~/.rcirc-logs"
  "Where logs should be kept.  If nil, logs aren't kept.")
(defun rcirc-write-log (process sender response target text)
  (when rcirc-log-directory
    (with-temp-buffer
      ;; Sometimes TARGET is a buffer :-(
      (when (bufferp target)
	(setq target (with-current-buffer buffer rcirc-target)))

      (unless
	  (or (null target)              ;; Don't log if target is null
	      (string= "JOIN" response)  ;; Don't log JOINs
	      (string= "MODE" response)  ;; Don't log MODEs
	      (string= "QUIT" response)) ;; Don't log QUITs
	(insert (format "@%-10s" process))
	(insert (format-time-string "[%d-%m-%y|%H:%M] "))
	(insert (format "%-16s " (rcirc-user-nick sender)))
	(unless (string= response "PRIVMSG")
	  (insert "/" (downcase response) " "))
	(insert text "\n")

	;; Append the line to the appropriate logfile.
	(let ((coding-system-for-write 'no-conversion))
	  (write-region (point-min) (point-max)
			(concat rcirc-log-directory "/" (downcase target))
			t 'quietly))))))

;; Autoaway
(setf rcirc-auto-away-server-regexps nil
      rcirc-auto-away-after 3600
      rcirc-auto-away-reason "Wandered off")

(defun rcirc-auto-away ()
  (message "rcirc-auto-away")
  (rcirc-auto-away-1 rcirc-auto-away-reason)
  (add-hook 'post-command-hook 'rcirc-auto-unaway))

(defun rcirc-auto-away-1 (reason)
  (let ((regexp (mapconcat (lambda (x) (concat "\\(" x "\\)")) 
			   rcirc-auto-away-server-regexps "\\|")))
    (dolist (process (rcirc-process-list))
      (when (string-match regexp (process-name process))
	(rcirc-send-string process "AWAY [Autoaway] Wandered off")))))

(defun rcirc-auto-unaway ()
  (remove-hook 'post-command-hook 'rcirc-auto-unaway)
  (rcirc-auto-away-1 ""))

(run-with-idle-timer rcirc-auto-away-after t 'rcirc-auto-away)
;;(cancel-function-timers 'rcirc-auto-away)

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

;; Hide IRC channels from iswitchb
;; (add-to-list 'iswitchb-buffer-ignore
;; 	     (lambda (name)
;; 	       (catch 'rcirc
;; 		 (mapc (lambda (process)
;; 			 (when (string= name (buffer-name (process-buffer process)))
;; 			   (throw 'rcirc t))
;; 			 (mapc (lambda (buffer)
;; 				 (when (string= name (buffer-name buffer))
;; 				   (throw 'rcirc t)))
;; 			       (with-rcirc-process-buffer process
;; 				 (mapcar 'cdr rcirc-buffer-alist))))
;; 		       (rcirc-process-list))
;; 		 nil)))

;; ----------
;; Mode hooks
;; ----------
(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode t)))
(add-hook 'javascript-mode-hook (lambda () (moz-minor-mode t)))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Paredit
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp scheme inferior-lisp))

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
      org-hide-leading-stars t)

;; org-remember
(org-remember-insinuate)
(setq org-default-notes-file "~/.notes")
(define-key global-map "\C-cr" 'org-remember)

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

;; Automagically step up priviliges for editing root files

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around my-find-file activate)
  "Open FILENAME using Tramp’s sudo method if it’s read-only and not owned by current user."
   (let* ((my-filename (ad-get-arg 0))
	  (file-owner-uid (nth 2 (file-attributes my-filename))))

     (if (not (file-writable-p my-filename))
	 (if (and (not (= file-owner-uid (user-uid)))
		  (y-or-n-p (concat "File " my-filename " is read-only. Open it as root? ")))
	     (progn
	       (ad-set-arg 0 (concat "/sudo::" my-filename))
	       ad-do-it
	       (rename-buffer
		(format "%s:%s"
			 (file-remote-p (buffer-file-name) 'method)
			 (buffer-name))))

	     (if (and (= file-owner-uid (user-uid))
		      (y-or-n-p (concat "File " my-filename " is read-only. Make buffer writable? ")))
		 (progn
		   ad-do-it
		   (toggle-read-only -1))))
	 ad-do-it)))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; Conviniently save and restore frame configurations

(defvar th-frame-config-register ?°
  "The register which is used for storing and restoring frame
configurations by `th-save-frame-configuration' and
`th-jump-to-register'.")

(defun th-save-frame-configuration (arg)
  "Stores the current frame configuration in register
`th-frame-config-register'. If a prefix argument is given, you
can choose which register to use."
  (interactive "P")
  (let ((register (if arg
                      (read-char "Which register? ")
                    th-frame-config-register)))
    (frame-configuration-to-register register)
    (message "Frame configuration saved in register '%c'."
             register)))

(defun th-jump-to-register (arg)
  "Jumps to register `th-frame-config-register'. If a prefix
argument is given, you can choose which register to jump to."
  (interactive "P")
  (let ((register (if arg
                      (read-char "Which register? ")
                    th-frame-config-register)))
    (jump-to-register register)
    (message "Jumped to register '%c'."
             register)))
