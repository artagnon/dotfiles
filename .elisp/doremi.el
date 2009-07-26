;;; doremi.el --- Do Re Mi: Incremental change using arrow keys or mouse wheel.
;;
;; Filename: doremi.el
;; Description: Incremental change using arrow keys or mouse wheel.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2009, Drew Adams, all rights reserved.
;; Created: Thu Sep 02 08:21:37 2004
;; Version: 21.1
;; Last-Updated: Sat Dec 27 10:09:33 2008 (-0800)
;;           By: dradams
;;     Update #: 1381
;; URL: http://www.emacswiki.org/cgi-bin/wiki/doremi.el
;; Keywords: keys, cycle, repeat, higher-order
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `mwheel', `ring', `ring+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Do Re Mi: Incremental change using arrow keys or mouse wheel.
;;
;; When you invoke Do Re Mi commands, you can then press and hold an
;; up/down arrow key, or turn the mouse wheel, to run up and down the
;; scale: do, re, mi,...
;;
;; Use the up/down arrow keys or the mouse wheel to:
;;
;;  - Change nearly any parameter incrementally (dynamically).
;;
;;  - Repeat an action.
;;
;;  - Cycle through a set of values, without changing anything (for
;;    example, to choose an item).  In this use, think of choosing
;;    from a menu.  This is similar to using a minibuffer history.
;;    The input choices can take the form of any Emacs-Lisp sequence
;;    (list, array, string, vector) - this sequence is converted to a
;;    circular structure (ring).
;;
;;  - Do just about anything: call a different function for each
;;    arrow.
;;
;; This works with numerical parameters that can be incremented and
;; decremented, and it works with parameters that can take on one of a
;; number of values.  In fact, it is even more general than that: you
;; can use it to associate nearly any function or pair of functions
;; with the arrow keys and the mouse wheel.
;;
;; By default, the up and down arrow keys are used, but any other keys
;; may be used instead.  Mouse wheel movements are recognized for
;; Emacs 20 and Emacs 21 (using library `mwheel.el').  `mouse-2'
;; presses are ignored, so that they won't interfere with rotating the
;; wheel.
;;
;; See the doc string for function `doremi' for more information.
;;
;; Code defining a few example commands is included here (but
;; commented out), so you can see how to use this.  For more examples
;; of using function `doremi', see files `doremi-frm.el' and
;; `doremi-cmd.el'.
;;
;; This library uses library `ring+.el', which provides extensions to
;; the standard library `ring.el' to let you manipulate circular
;; structures.
;;
;;  Commands defined here:
;;
;;    `doremi'.
;;
;;  Non-interactive functions defined here:
;;
;;    `doremi-limit', doremi-wrap.
;;
;;  User options (variables) defined here:
;;
;;    `doremi-boost-down-key', `doremi-boost-up-key',
;;    `doremi-down-key', `doremi-up-key'.
;;
;;  Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;    (require 'doremi)
;;
;;  See also these related libraries that make use of `doremi':
;;
;;    `doremi-frm.el' - Incrementally adjust frame properties.
;;    `doremi-cmd.el' - Other Do Re Mi commands.
;;    `doremi-mac.el' - Macro to define Do Re Mi commands and
;;                      automatically add them to Do Re Mi menu.
;;
;;  This has been tested on GNU Emacs 20, 21, and 22 on MS Windows.
;;
;;
;; TO DO?:
;;
;;   - Replace `boost-*' keys by test for modifiers (as for wheel).
;;   - Combine with customize.  That is, have customize buffers use
;;     Do Re Mi commands to defined numeric or enumeration values.
;;   - Provide buttons (menu items) in menus that act like up & down
;;     arrows.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/12/31 dadams
;;      doremi: Use doremi-set-new-value instead of wrapping input functions.
;;              Commented out setting redisplay-dont-pause to t.
;;      Added: doremi-set-new-value.
;;      doremi-boost-scale-factor: Clarified doc string.
;;      property -> parameter in all doc strings (RMS).
;; 2007/10/21 dadams
;;      doremi: Don't let switch-frame events quit the loop.
;;      Added: doremi-limit, doremi-wrap.
;; 2006/01/07 dadams
;;      Added :link.
;; 2005/07/25 dadams
;;     Added :prefix to defgroup.
;; 2005/01/16 dadams
;;     doremi: Bind redisplay-dont-pause to `t' for Emacs 21.
;;             Use error-message-string to format error string.
;; 2005/01/01 dadams
;;     defvar -> defcustom.  Added (defgroup doremi).
;; 2004/11/28 dadams
;;     doremi: Allowed for GROWTH-FN to be a function, not just a flag.
;;             Added initial value to initial prompt.
;;             Corrected addition of last event to unread-command-events.
;;             Improved error messages.
;; 2004/09/26 dadams
;;     Renamed do-re-mi* to doremi*.
;;     Prefixed everything here with doremi-.
;;     Changed convert-sequence-to-ring to ring-convert-sequence-to-ring.
;; 2004/09/24 dadams
;;     Added use of mouse wheel.  Changed key sequences to events.
;;     Change prompt to add Meta info only for non-enumeration.
;;     Suppress keystroke echoing.
;; 2004/09/19 dadams
;;     Moved doremi-buffers to doremi-cmd.el.
;;     Commented-out commands test-*.
;; 2004/09/11 dadams
;;     Moved to doremi-frm.el: adjust-*, cycle-frame-configs,
;;           grow-font, move-frame-*, and apply of push-frame-config.
;; 2004/09/07 dadams
;;     Added: cycle-frame-configs.
;;     Apply push-frame-config to frame commands here.
;; 2004/09/06 dadams
;;     Added boost-*.  Added error treatment to move-frame-*.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ring+) ;; ring-convert-sequence-to-ring, ring-insert+extend,
                 ;; ring-member, ring-next, ring-previous
(require 'mwheel nil t) ; (no error if not found): mwheel-event-button

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

;; In Emacs 20, because `mwheel.el' is not loaded, byte-compiling
;; would give the following error messages, which can be ignored:
;;
;; While compiling doremi:
;;   ** reference to free variable mouse-wheel-down-event
;;   ** reference to free variable mouse-wheel-up-event
;; While compiling the end of the data:
;;   ** the function mwheel-event-button is not known to be defined.
;;
;; This eliminates (only) the first of these two byte-compiler messages:
(defvar mouse-wheel-down-event)
(defvar mouse-wheel-up-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User Options (Variables)

(defgroup doremi nil
  "Do Re Mi: Incremental change using arrow keys or mouse wheel.
Define commands to perform repetitive or incremental operations."
  :prefix "doremi-" :group 'convenience
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
doremi.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/doremi.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Doremi")
  :link '(emacs-commentary-link :tag "Commentary" "doremi")
  )

(defcustom doremi-up-key 'up
  "*Key (event) associated with one direction of adjusting by `doremi'.
The other direction is associated with `doremi-down-key'."
  :type 'symbol :group 'doremi)

(defcustom doremi-down-key 'down
  "*Key (event) associated with one direction of adjusting by `doremi'.
The other direction is associated with `doremi-up-key'."
  :type 'symbol :group 'doremi)

(defcustom doremi-boost-up-key 'M-up
  "*Same as `doremi-up-key', but increments faster
by a factor of `doremi-boost-scale-factor'."
  :type 'symbol :group 'doremi)

(defcustom doremi-boost-down-key 'M-down
  "*Same as `doremi-down-key', but increments faster
by a factor of `doremi-boost-scale-factor'."
  :type 'symbol :group 'doremi)

(defcustom doremi-boost-scale-factor 10
  "*Factor to boost incremental change of numerical properties.
Using `doremi-boost-up-key' or `doremi-boost-down-key', instead of
`doremi-up-key' or `doremi-down-key' means the increment is this many
times larger.  Using a modifier key with the mouse wheel has the same
effect as using `doremi-boost-up-key' or `doremi-boost-down-key'."
  :type 'integer :group 'doremi)
 
;;; Commands (Interactive Functions)

;;;###autoload
(defun doremi (setter-fn init-val incr &optional growth-fn enum allow-new-p)
  "Use arrow keys and/or mouse wheel to adjust some parameter.

Variables `doremi-up-key' and `doremi-down-key' are variables that you
can assign to any key sequences.  You can use these keys or the mouse
wheel to dynamically adjust any parameter.  The keys can be held down
for continual adjustment.

Example parameters include background color and font size, but a
parameter can be anything that is adjustable in any of these ways:
 * A numerical parameter that can be incremented or decremented, such
   as frame height.
 * A parameter that can take on one of several values (an enumerated
   choice), such as a frame background color.
 * A parameter that has an associated function to change its value
   incrementally.

SETTER-FN is a function that adjusts the parameter.  Two forms:
 1) It takes a value as argument and sets the parameter to this value.
 2) It is a \"growth\" function, which takes an increment as argument
    and incrementally adjusts the value of the parameter.

 Note that \"growth\" function really means, here, that the function
 takes an increment as argument and does the incrementation (or
 whatever) itself.  It is contrasted with an absolute SETTER-FN that
 just uses a value that is incremented by `doremi'.  The difference is
 which function does the incrementing, SETTER-FN or `doremi'.

 In case #1, the new parameter value _must_ be returned by SETTER-FN.
 In case #2, the new parameter value should be returned by SETTER-FN,
             so that it can be echoed to the user.

INIT-VAL is the initial value for adjustment.  In the case of an
 incremental growth function (case #2), this is ignored.

INCR is an adjustment increment.
 For an absolute SETTER-FN (#1), this is applied to INIT-VAL before
     calling the function.  If ENUM is non-nil, then INCR is ignored.
 For an incremental growth function, this is passed to the function.

If GROWTH-FN is non-nil, then SETTER-FN is an incremental growth
  function (case #2), and it is called with INCR as its only argument.
If GROWTH-FN is a function, then it is used as an alternative growth
  function.  In this case, SETTER-FN is called for `doremi-up-key' and
  GROWTH-FN is called for `doremi-down-key' (mouse wheel is similar).

ENUM is a choice-enumeration sequence (list, array, string...).
  If ENUM is non-nil, then it is converted to a ring (circular
  structure), and `doremi-up-key' and `doremi-down-key' set the
  parameter to `ring-next' and `ring-previous' values, respectively.

If ENUM is non-nil, then ALLOW-NEW-P defines what happens if INIT-VAL
is not a member of ENUM.  If ALLOW-NEW-P is nil, then an error is
raised.  If non-nil, then INIT-VAL is added (to the ring created from
ENUM).  If the symbol `extend', then if the ring is full it is
extended to include INIT-VAL; other non-nil values cause the oldest
item in a full ring to be dropped.

For numerical parameters (not enumerated choices), there are actually
two levels of incrementation.  For faster incrementation, you can use
`doremi-boost-up-key' and `doremi-boost-down-key', or you can use any
modifier(s) (Shift, Meta, Control...) with the mouse wheel.
Incrementation is then `doremi-boost-scale-factor' times faster.

For examples of using `doremi', see the source code of libraries
`doremi.el', `doremi-frm.el', and `doremi-cmd.el'."
  (setq incr (or incr 1))
  (let ((new-incr incr))
        ;; $$$$ (redisplay-dont-pause t))       ; To give continual feedback.
    ;; Convert sequence of values (list, array, vector, string) to a ring.
    (when (and enum (sequencep enum)) (setq enum (ring-convert-sequence-to-ring enum)))

    ;; Loop.  Prompt, read event, and act on arrow-key or mouse-wheel event.
    (let ((prompt (format "Use %s, %s, or mouse wheel to adjust value"
                          doremi-up-key doremi-down-key))
          (echo-keystrokes 0)           ; Suppress keystroke echoing.
          (keys (list doremi-up-key doremi-down-key
                      doremi-boost-up-key doremi-boost-down-key))
          evnt save-prompt)
      (unless enum (setq prompt (concat prompt " (modifier key: faster)")))
      (setq prompt (format (concat prompt ".  Value now: %s") init-val))
      (setq save-prompt prompt)
      (while (progn (setq evnt    (read-event prompt)
                          prompt  nil)
                    (or (member evnt keys)
                        (and (consp evnt)
                             (member (event-basic-type (car evnt))
                                     '(switch-frame mouse-wheel mouse-2 wheel-up wheel-down)))))
        ;; Set up the proper increment value.
        (cond ((equal doremi-up-key evnt) (setq new-incr incr)) ; +
              ((equal doremi-down-key evnt) (setq new-incr (- incr))) ; -
              ((equal doremi-boost-up-key evnt)
               (setq new-incr (* doremi-boost-scale-factor incr))) ; ++
              ((equal doremi-boost-down-key evnt)
               (setq new-incr (* doremi-boost-scale-factor (- incr)))) ; --

              ;; Emacs 20 mouse wheel.
              ((and (consp evnt) (equal 'mouse-wheel (event-basic-type (car evnt))))
               (setq new-incr (if (< 0 (nth 2 evnt)) incr (- incr)))
               (when (event-modifiers evnt) ; Boost it
                 (setq new-incr (* doremi-boost-scale-factor new-incr))))

              ;; Emacs 21 mouse wheel: `mwheel.el'
              ;; Free vars here: `mouse-wheel-down-event', `mouse-wheel-up-event'.
              ;; Those vars and function `mwheel-event-button' are defined in `mwheel.el'.
              ((and (consp evnt)
                    (member (event-basic-type (car evnt)) '(wheel-up wheel-down)))
               (let ((button (mwheel-event-button evnt)))
                 (cond ((eq button mouse-wheel-down-event) (setq new-incr incr))
                       ((eq button mouse-wheel-up-event) (setq new-incr (- incr)))
                       (t (error "`doremi' - Bad binding in mwheel-scroll"))))
               (when (event-modifiers evnt) ; Boost it
                 (setq new-incr (* doremi-boost-scale-factor new-incr)))))
        (if (and (consp evnt) (memq (event-basic-type (car evnt)) '(mouse-2 switch-frame)))
            (message save-prompt) ;; Just skip mouse-2 event (ignore while using wheel).

          ;; Adjust setting and update INIT-VAL.  Four cases are treated separately:
          ;; 1) ENUM non-nil: use `ring-next' and `ring-previous'.
          ;; 2) SETTER-FN and GROWTH-FN are both "growth" functions: call one of them.
          ;; 3) SETTER-FN is a "growth" function: call it on the INCR arg.
          ;; 4) otherwise (absolute fn): increment INIT-VAL, then call SETTER-FN on it.
          (condition-case failure
              (setq init-val
                    (cond ( ;; 1) Ring of values (enumeration list).  Use `ring-next''...
                           (ring-p enum)
                           ;; If INIT-VAL is not already in the ring, add it.
                           ;; Extend the ring size if ALLOW-NEW-P is `extend'.
                           (when (and allow-new-p (not (ring-member enum init-val)))
                             (ring-insert+extend enum init-val
                                                 (eq 'extend allow-new-p)))
                           (when (< (ring-length enum) 2)
                             (error "`doremi' - Need at least two alternatives: %s" enum))
                           (let* ((vec (cdr (cdr enum)))
                                  (veclen (length vec)))
                             (if (natnump new-incr)
                                 (doremi-set-new-value setter-fn (ring-next enum init-val))
                               (doremi-set-new-value setter-fn (ring-previous enum init-val)))))

                          ;; 2) Two incremental growth functions.  Call one on INCR only.
                          ((functionp growth-fn)
                           (if (natnump new-incr)
                               (doremi-set-new-value setter-fn new-incr)
                             (doremi-set-new-value growth-fn (- new-incr))))

                          ;; 3) Single incremental growth function.  Call it on INCR only.
                          (growth-fn (doremi-set-new-value setter-fn new-incr))

                          ;; 4) Otherwise.  Increment value.  Call setter function on new value.
                          ((and (numberp new-incr) (numberp init-val))
                           (doremi-set-new-value setter-fn (+ init-val new-incr)))
                          (t (error "`doremi' - Bad argument.  INIT-VAL: %s, INCR: %s"
                                    init-val new-incr))))
            (error (error "%s" (error-message-string failure))))))
      (message nil)
      (setq unread-command-events (cons evnt unread-command-events)))))

(defun doremi-set-new-value (setter-fn newval)
  "Apply SETTER-FN to NEWVAL, and return NEWVAL.  Display progress message."
  (prog1 (setq newval (funcall setter-fn newval))
    (message "Use %s, %s, or mouse wheel again.  New value: %s"
             doremi-up-key doremi-down-key newval)))
 
;;; Non-Interactive Functions

(defun doremi-limit (value min max)
  "Limit VALUE to MIN or MAX limit if either is overshot.
MIN or MAX = nil means no such limit."
  (cond ((and max (> value max)) max)
        ((and min (< value min)) min)
        (t value)))

(defun doremi-wrap (value min max)
  "Wrap VALUE around if it overshoots MIN or MAX."
  (cond ((> value max) min)
        ((< value min) max)
        (t value)))
 
;;; Example Commands.  Uncomment these and try them to get the idea.
;;
;; See also the commands in `doremi-cmd.el' and `doremi-frm.el' for
;; more examples.


;; Uses an enumeration list, (buffer-list).
;; (defun doremi-buffers ()
;;   "Successively cycle among existing buffers."
;;   (interactive)
;;   (doremi (lambda (newval) (switch-to-buffer newval 'norecord) newval)
;;            (current-buffer)
;;            nil                         ; ignored
;;            nil                         ; ignored
;;            (buffer-list)))

;; Test command that uses an enumeration list.
;; This command changes nothing.  It just echoes successive values.
;; (defun test-list ()
;;   (interactive)
;;   (doremi (lambda (newval) newval) 'c 1 nil '(a b c d e f g)))

;; Test command that uses an enumeration list.
;; In this test, the init-val is not a member of the enumeration list.
;; An error is raised.
;; This command changes nothing.  It just echoes successive values.
;; (defun test-list-prohibit-nonmember ()
;;   (interactive)
;;   (doremi (lambda (newval) newval) 'c 1 nil '(a b)))

;; Test command that uses an enumeration list.
;; In this test, the init-val is not a member of the enumeration list.
;; Because of non-nil 6th arg ALLOW-NEW-P, the initial value 'c is added
;; to the enumeration.
;; This command changes nothing.  It just echoes successive values.
;; (defun test-list-allow-nonmember ()
;;   (interactive)
;;   (doremi (lambda (newval) newval) 'c 1 nil '(a b) t))

;; Test command that uses an enumeration list.
;; In this test, the init-val is not a member of the enumeration list.
;; Because 6th arg ALLOW-NEW-P is 'extend, the enumeration is enlarged
;; to include the initial value 'c.
;; This command changes nothing.  It just echoes successive values.
;; (defun test-list-allow-nonmember+extend ()
;;   (interactive)
;;   (doremi (lambda (newval) newval) 'c 1 nil '(a b) 'extend))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doremi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doremi.el ends here
