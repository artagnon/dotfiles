;;; doremi-frm.el --- Incrementally adjust face attributes and frame parameters.
;;
;; Filename: doremi-frm.el
;; Description: Incrementally adjust face attributes and frame parameters.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2009, Drew Adams, all rights reserved.
;; Created: Sat Sep 11 10:40:32 2004
;; Version: 22.0
;; Last-Updated: Sat Dec 27 10:09:33 2008 (-0800)
;;           By: dradams
;;     Update #: 2151
;; URL: http://www.emacswiki.org/cgi-bin/wiki/doremi-frm.el
;; Keywords: frames, extensions, convenience, keys, repeat, cycle
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `doremi', `eyedropper', `faces', `faces+',
;;   `frame-cmds', `frame-fns', `hexrgb', `misc-fns', `mwheel',
;;   `ring', `ring+', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Do Re Mi commands to incrementally adjust face attributes and
;;    frame parameters using arrow keys or mouse wheel.
;;
;;  When you invoke Do Re Mi commands, you can then press and hold an
;;  up/down arrow key, or rotate the mouse wheel, to change face
;;  attributes or frame parameters.  For more info, see file
;;  `doremi.el' and the doc-string for function `doremi' in
;;  particular.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change face and frame
;;    parameters.  You can save any changes you have made, by using
;;    Customize.  To visit a Customize buffer of all unsaved changes
;;    you have made, use command `customize-customized'.
;;
;;    Frame parameter changes, such as background color, can be saved
;;    for future use by all frames or all frames of a certain kind.
;;    For that, you must change the frame parameters of the
;;    correponding frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame.  Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames.  These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'.  The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined in library
;;    `frame-cmds.el'.
;;
;;    Example: Suppose you change the background color of a frame and
;;    want to make that the default background color for new frames in
;;    the future.  You will need to update the value of variable
;;    `default-frame-alist' to use the `background-color' parameter
;;    setting of the changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'.  Those commands are
;;    defined in library `frame-cmds.el'.
;;
;;
;;  User options defined here:
;;
;;    `doremi-frame-config-ring-size',
;;    `doremi-move-frame-wrap-within-display-flag',
;;    `doremi-push-frame-config-for-cmds-flag',
;;    `doremi-wrap-color-flag'.
;;
;;
;;  Commands defined here:
;;
;;    `doremi-all-faces-bg', `doremi-all-faces-fg',
;;    `doremi-all-frames-bg', `doremi-all-frames-fg', `doremi-bg',
;;    `doremi-bg-blue', `doremi-bg-brightness',
;;    `doremi-bg-color-name', `doremi-bg-cyan', `doremi-bg-green',
;;    `doremi-bg-hue', `doremi-bg-magenta', `doremi-bg-purity',
;;    `doremi-bg-red', `doremi-bg-saturation', `doremi-bg-value',
;;    `doremi-bg-yellow', `doremi-face-bg', `doremi-face-fg',
;;    `doremi-fg', `doremi-fg-blue', `doremi-fg-cyan',
;;    `doremi-fg-green', `doremi-fg-hue', `doremi-fg-magenta',
;;    `doremi-fg-red', `doremi-fg-saturation', `doremi-fg-value',
;;    `doremi-fg-yellow', `doremi-font', `doremi-font-size',
;;    `doremi-frame-configs', `doremi-frame-height',
;;    `doremi-frame-horizontally', `doremi-frame-vertically',
;;    `doremi-frame-width', `doremi-increment-background-color',
;;    `doremi-increment-color-component',
;;    `doremi-increment-face-bg-color',
;;    `doremi-increment-face-fg-color',
;;    `doremi-increment-foreground-color',
;;    `doremi-set-background-color', `doremi-set-foreground-color',
;;    `doremi-toggle-wrap-color', `doremi-undo-last-face-change',
;;    `doremi-undo-last-frame-color-change',
;;    `toggle-doremi-wrap-color'.
;;
;;
;;  Non-interactive functions defined here:
;;
;;    `doremi-face-bg-1', `doremi-face-fg-1', `doremi-face-set',
;;    `doremi-frame-config-wo-parameters',
;;    `doremi-frame-new-position', `doremi-increment-color',
;;    `doremi-increment-face-color-read-args', `doremi-number-arg',
;;    `doremi-push-current-frame-config',
;;    `doremi-push-frame-config-for-command',
;;    `doremi-wrap-or-limit-color-component'.
;;
;;
;;  Internal variables defined here:
;;
;;    `doremi-frame-config-ring', `doremi-last-frame-color',
;;    `doremi-last-face-value'.
;;
;;
;;  See also these related Do Re Mi libraries:
;;
;;    `doremi-mac.el' - Macro to define Do Re Mi commands and
;;                      automatically add them to a Do Re Mi menu.
;;    `doremi-cmd.el' - Do Re Mi commands not dealing with frames.
;;
;;  See also these files for other frame commands:
;;
;;     `autofit-frame.el' - Automatically fit each frame to its
;;                          selected window.  Uses `fit-frame.el'.
;;
;;     `fit-frame.el'     - 1) Fit a frame to its selected window.
;;                          2) Incrementally resize a frame.
;;
;;     `frame-cmds.el'    - Various frame and window commands.
;;
;;     `thumb-frm.el'     - Shrink frames to a thumbnail size and
;;                          restore them again.
;;
;;     `zoom-frm.el'      - Zoom a frame, so that its font becomes
;;                          larger or smaller.
;;
;;
;;  Put this in your init file (`~/.emacs'): (require 'doremi-frm)
;;
;;  Suggested key bindings:
;;
;;   (defalias 'doremi-prefix (make-sparse-keymap))
;;   (defvar doremi-map (symbol-function 'doremi-prefix)
;;     "Keymap for Do Re Mi commands.")
;;   (define-key global-map "\C-xt" 'doremi-prefix)
;;   (define-key doremi-map "a" 'doremi-all-faces-fg) ; "All"
;;   (define-key doremi-map "c" 'doremi-bg) ; "Color"
;;   (define-key doremi-map "f" 'doremi-face-fg) ; Face"
;;   (define-key doremi-map "h" 'doremi-frame-height)
;;   (define-key doremi-map "t" 'doremi-font) ; "Typeface"
;;   (define-key doremi-map "u" 'doremi-frame-configs) ; "Undo"
;;   (define-key doremi-map "x" 'doremi-frame-horizontally)
;;   (define-key doremi-map "y" 'doremi-frame-vertically)
;;   (define-key doremi-map "z" 'doremi-font-size)) ; "Zoom"
;;
;;  Customize the menu.  Uncomment this to try it out.
;;
;;   (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
;;   (define-key global-map [menu-bar doremi]
;;     (cons "Do Re Mi" menu-bar-doremi-menu))
;;   (define-key menu-bar-doremi-menu [doremi-frame-configs]
;;     '(menu-item "Frame Configurations" . doremi-frame-configs
;;       :help "Cycle among frame configurations recorded: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-font]
;;     '(menu-item "Font" . doremi-font
;;       :help "Successively cycle among fonts, choosing by name: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-font-size]
;;     '(menu-item "Font Size (Zoom)" . doremi-font-size
;;       :help "Change font size for frame incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-undo-last-face-change]
;;     '(menu-item "Undo Face Color Change" doremi-undo-last-face-change
;;       :enable (facep 'doremi-last-face)
;;       :help "Undo the last face color change by Do Re Mi"))
;;   (define-key menu-bar-doremi-menu [doremi-face-bg]
;;     '(menu-item "Face Background..." . doremi-face-bg
;;       :help "Change background color of a face incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-all-faces-fg]
;;     '(menu-item "All Faces - Foreground..." . doremi-all-faces-fg
;;       :help "Change foreground color of all faces incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-face-fg]
;;     '(menu-item "Face Foreground..." . doremi-face-fg
;;       :help "Change foreground color of a face incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-all-frames-bg]
;;     '(menu-item "All Frame Backgrounds..." . doremi-all-frames-bg
;;       :help "Change background of all frames incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-undo-last-frame-color-change]
;;     '(menu-item "Undo Frame Color Change" doremi-undo-last-frame-color-change
;;       :enable doremi-last-frame-color
;;       :help "Undo the last frame color change by `doremi-fg' or `doremi-bg'"))
;;   (define-key menu-bar-doremi-menu [doremi-bg]
;;     '(menu-item "Frame Background..." . doremi-bg
;;       :help "Change frame background color incrementally: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-frame-vertically]
;;     '(menu-item "Move Frame" doremi-frame-vertically
;;       :help "Move frame incrementally: `up'/`down'/`left'/`right'"))
;;   (define-key menu-bar-doremi-menu [doremi-frame-height]
;;     '(menu-item "Frame Size" doremi-frame-height
;;       :help "Resize frame incrementally: `up'/`down'/`left'/`right'"))
;;
;;
;;  TO DO?
;;
;;    1. Factor out more common stuff between foreground and background.
;;    2. Make it easy to turn on and off doremi-push-frame-config stuff.
;;    3. Integrate more with Customize.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2008/01/03 dadams
;;     doremi(-face)-(bg|fg), doremi-all-frames-(bg|fg):
;;       Scale increment arg by 256 for RGB.
;;     doremi-increment-color(-component): Do not scale INCREMENT arg by 256 for RGB.
;;     doremi-increment-color: Limit INCREMENT from -100 to 100 for HSV only.
;; 2007/12/31 dadams
;;     doremi-last-face-value: Use copy-face instead of internal-find-face.
;; 2007/12/30 dadams
;;     doremi-all-(faces|frames)-(bg|fg): Bound doremi-wrap-color-flag to nil.
;; 2007/11/01 dadams
;;     doremi-frame-(horizontally|vertically): Don't use doremi-number-arg.
;;     Lowercased all :groups.
;; 2007/10/26 dadams
;;     doremi-last-frame-color: Initial value is nil now.
;;     doremi-undo-last-frame-color-change: Added error message if no last frame color.
;; 2007/10/21 dadams
;;     Renamed: doremi-wrap-or-limit to doremi-wrap-or-limit-color-component.
;;              Redefined it using doremi-limit and doremi-wrap.
;; 2007/10/08 dadams
;;     Use lowercase for defgroup group.
;; 2007/09/30 dadams
;;     doremi-face-set: (setq attrs (cdr attrs)) -> (setq attrs (cddr attrs)).
;; 2006/06/23 dadams
;;     picked-(back|fore)ground -> eyedrop-picked-(back|fore)ground
;;     Require eyedropper.el or palette.el, depending on the Emacs version.
;;     doremi(-face)-(bg|fg):
;;       Added pickup-p arg. Use picked color if pickup-p arg or C-u (not <0).
;;     doremi-(bg|fg)-*: Call doremi-(bg|fg) with pickup-p arg.
;;     Bug fix, doremi(-face)-(bg|fg):
;;       Only pick up picked bg or fg on first call (interactive).
;;       Tolerate no load of pick-up code.
;; 2006/06/06 dadams
;;     Use hexrgb-defined-colors(-alist) instead of x-defined-colors.
;; 2006/05/30 dadams
;;     doremi-increment-color-component: Use hexrgb-color-name-to-hex.
;;     Removed: doremi-color-name-to-RGB.
;; 2006/01/07 dadams
;;      Added :link.
;; 2005/12/26: dadams
;;     Updated group and parent groups.
;; 2005/12/13 dadams
;;     doremi-increment-face-(b|f)g-color:
;;       Bug fix: Only update doremi-last-face(-value) when interactive.
;; 2005/08/09 dadams
;;     Added: doremi-wrap-color-flag, doremi-wrap-or-limit, doremi-toggle-wrap-color,
;;            toggle-doremi-wrap-color.
;;     doremi-increment-color: Take doremi-wrap-color-flag into account.
;;                             Use doremi-wrap-or-limit.
;; 2005/08/02 dadams
;;     Added: doremi-all-faces-(b|f)g, doremi-all-frames-(b|f)g,
;;            doremi-set-(back|fore)ground-color.
;;     doremi-(b|f)g, doremi-increment-(back|fore)ground-color,
;;       doremi-undo-last-frame-color-change, doremi-increment-color: Added frame arg.
;;     doremi-increment-color: Updated doc string.  Lower bound of increment is -100.
;;     doremi-increment-face-(b|f)g-color: Use nil frame arg to doremi-increment-color.
;;                                         Save face arg as last face.
;;     doremi-increment-(back|fore)ground-color: Use doremi-number-arg.
;;     doremi-(b|f)g, doremi-increment-(back|fore)ground-color:
;;       Use doremi-set-(back|fore)ground-color instead of set-(back|fore)ground-color.
;;     doremi-color-name-to-RGB: Use facemenu-read-color, instead of completing-read.
;;     doremi-last-face-value, doremi-last-frame-color: Better default values.
;;     doremi-undo-last-face-change: Error if no last face.
;;     Only require strings.el if read-number is not fboundp.
;; 2005/07/31 dadams
;;     Added: doremi-color-name-to-RGB, doremi-number-arg.
;;     doremi-frame-(horizontally|vertically), doremi-(bg|fg),
;;       doremi-increment-face-color-read-args, doremi-increment-color-component:
;;         Use doremi-number-arg.
;;     doremi-increment-color-component: Made into a command.
;;     doremi-face-(fg|bg): Use doremi-increment-face-color-read-args.
;; 2005/07/29 dadams
;;     Added: doremi-increment-color-component.
;; 2005/07/25 dadams
;;     Added: :prefix to defgroup.
;; 2005/07/17 dadams
;;     doremi-increment-color: Limit increment to 100 max.
;;     Mention in doc strings that increment max is 100.
;; 2005/07/02 dadams
;;     Added: doremi-fg*, doremi-increment-foreground-color,
;;            doremi-undo-last-frame-color-change, doremi-last-frame-color.
;; 2005/07/01 dadams
;;     doremi-face-[fb]g: Added treatment of negative prefix arg (use picked color).
;;     doremi-face-[fb]g-1: Use increment arg, already normalized by caller.
;;     doremi-undo-last-face-change: Use doremi-last-face directly.
;; 2005/06/30 dadams
;;     doremi-face-[fb]g:
;;       Also display sample of face before changes.
;;       Save face before changes, for doremi-undo-last-face-change.
;;       Error if face arg doesn't name a face.
;;     Added: doremi-last-face-value, doremi-undo-last-face-change.
;;     Removed: doremi-face-(fore|back)ground (to faces+.el as face-(fore|back)ground-20+.
;;     Hard require faces+.el.
;; 2005/06/28 dadams
;;     doremi-face-[fb]g: Pop up a sample.
;;     Added: doremi-face-[fb]g-1.
;; 2005/06/26 dadams
;;     doremi-increment-color: Fixed bug when face was a symbol, not a variable - use
;;       apply instead of eval funcall.
;; 2005/06/24 dadams
;;     doremi-face-[bf]g:
;;       1) No longer convert face to internal-get-face form.
;;       2) Use face, instead of face-name.
;;       3) No longer use doremi-face-set.  Use set-face-attribute or modify-face.
;;     doremi-increment-face-[bf]g-color: ensure face via facep, not internal-get-face.
;;     doremi-face-set: No longer use face-spec-set.  This should be OK for Emacs 22,
;;       but it is not used, for now.
;; 2005/05/29 dadams
;;     Renamed: doremi-frame-move-wrap-within-display ->
;;                doremi-move-frame-wrap-within-display-flag.
;; 2005/01/25 dadams
;;     doremi-face-bg, doremi-face-fg (bug fix):
;;       1) Use internal-get-face, not facemenu-get-face.
;;       2) Use face-name for face arg to doremi-face-set.
;;     doremi-increment-face-fg-color (and -bg-) (bug fix): Use internal-get-face.
;;     doremi-frame-move-wrap-within-display: defvar -> defcustom.
;; 2005/01/18 dadams
;;     Added Note on saving changes.
;; 2005/01/16 dadams
;;     1. Added: doremi-face-set, doremi-face-foreground, doremi-face-background.
;;     2. doremi-face-fg, doremi-face-bg, doremi-increment-face-fg-color,
;;        doremi-increment-face-bg-color: Use doremi-face-set and doremi-face-foreground
;;                                        or doremi-face-background.
;; 2005/01/15 dadams
;;     doremi-increment-color and functions that call it: default is hue.
;;     Added: doremi-bg-cyan, doremi-bg-magenta, doremi-bg-yellow.
;; 2005/01/09 dadams
;;     Renamed: doremi-bg-rgb to doremi-bg, doremi-increment-face-bg-hex to
;;       doremi-increment-face-bg-color, doremi-increment-face-fg-hex to
;;       doremi-increment-face-fg-color, doremi-face-bg-rgb to doremi-face-bg,
;;       doremi-face-fg-rgb to doremi-face-fg, doremi-increment-background-hex to
;;       doremi-increment-background-color.
;;     Treat HSV now too: doremi-bg, doremi-increment-background-color, doremi-face-fg,
;;       doremi-increment-face-fg-color, doremi-face-bg,
;;       doremi-increment-face-bg-color, doremi-bg-value.
;;     Added: doremi-bg-hue, doremi-bg-saturation, doremi-bg-value (HSV version),
;;       doremi-bg-brightness, doremi-bg-purity, doremi-push-frame-config-for-cmds-flag,
;;       doremi-increment-color, doremi-increment-face-color-read-args.
;;     doremi-increment-background-color, doremi-increment-face-bg-color,
;;       doremi-increment-face-fg-color: Factored out common parts to create
;;       doremi-increment-color and doremi-increment-face-color-read-args.
;;     Fixed to use characters, not symbols: doremi-bg-red, doremi-bg-green,
;;       doremi-bg-blue, doremi-bg-hue, doremi-bg-saturation, doremi-bg-value,
;;       doremi-bg-brightness, doremi-bg-purity.
;;     Do not do doremi-push-frame-config-for-command by default
;;       (doremi-push-frame-config-for-cmds-flag is nil).
;; 2005/01/08 dadams
;;     Moved doremi-grow-font to frame-cmds.el, and renamed it to enlarge-font.
;; 2005/01/07 dadams
;;     doremi-grow-font: Treat error when new size is too small.
;; 2005/01/01 dadams
;;     defvar -> defcustom.  Added (defgroup doremi-frm).
;; 2004/12/28 dadams
;;     doremi-bg-rgb:
;;       You can now chain from changing one parameter to another.
;;       Color parameter (r,g,b,v) is now character type, not symbol type.
;;       Changed arg order.
;;     doremi-increment-background-hex:
;;       COMPONENT is a character, not symbol.
;;       Changed arg order.
;;     Added: doremi-face-bg-rgb, doremi-face-fg-rgb, doremi-increment-face-bg-hex,
;;            doremi-increment-face-fg-hex.
;; 2004/11/28 dadams
;;     Rewrote doremi-frame-horizontally and doremi-frame-vertically to:
;;       1) move frame off the display
;;       2) wrap frame around display
;;     Added: doremi-frame-new-position, doremi-frame-move-wrap-within-display.
;;     Require frame-fns.el[c].  Hard require ring+.el[c].
;; 2004/10/17 dadams
;;     doremi-grow-font: Fixed for Emacs 21: set point size and width to "*"
;; 2004/10/11 dadams
;;     doremi-frame-(horizontally|vertically):
;;       1. If start off screen, move frame back on screen (no error).
;;       2. Use modify-frame-parameters, not set-frame-position, bc unchanging
;;          value could be a cons.
;;       3. Chain each off of the other, so can use all four arrows.
;; 2004/09/26 dadams
;;     Renamed do-re-mi* to doremi*.
;;     Prefixed everything here with doremi-.
;;     Removed "adjust", "cycle", and "move" from names.
;; 2004/09/23 dadams
;;     doremi-grow-font: Removed font-info stuff (unused).
;;     doremi-frame-width, doremi-frame-horizontally:
;;            Changed key sequences to events.
;; 2004/09/21 dadams
;;     doremi-push-frame-config-for-command: Message only if interactive-p.
;; 2004/09/20 dadams
;;     Added: doremi-bg-blue, doremi-bg-brightness,
;;            doremi-bg-green, doremi-bg-red,
;;            doremi-bg-rgb, doremi-increment-background-hex.
;;     Renamed doremi-adjust-bg-color to doremi-bg-color-name.
;;     Changed suggested binding C-xtc to doremi-bg-rgb.
;;     Apply doremi-push-frame-config-for-command to new commands.
;; 2004/09/19 dadams
;;     Corrected interactive spec for doremi-font-size
;; 2004/09/17 dadams
;;     Added non-nil allow-new-p to doremi-adjust-bg-color
;; 2004/09/11 dadams
;;     Created this from stuff in doremi.el and frame-cmds.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(require 'doremi) ;; doremi, doremi-limit, doremi-wrap
(require 'hexrgb) ;; hexrgb-color-name-to-hex, hexrgb-color-values-to-hex,
                  ;; hexrgb-defined-colors, hexrgb-defined-colors-alist,
                  ;; hexrgb-increment-blue, hexrgb-increment-green, hexrgb-increment-red,
                  ;; hexrgb-hsv-to-rgb, hexrgb-rgb-to-hsv
(require 'ring+)  ;; ring-insert, ring-member, ring-next
(require 'frame-fns) ;; frame-geom-spec-cons, frame-geom-value-cons
(require 'faces+) ;; face-background-20+, face-foreground-20+, Emacs 20: read-face-name
(if (fboundp 'defvaralias) ;; Emacs 22
    (require 'palette nil t) ;; eyedrop-picked-background, eyedrop-picked-foreground
  (require 'eyedropper nil t)) ;; eyedrop-picked-background, eyedrop-picked-foreground
(require 'frame-cmds nil t) ;; (no error if not found):
                            ;; frame-configuration-to-register, enlarge-font
                            ;; jump-to-frame-config-register
(unless (fboundp 'read-number)
  (require 'strings nil t)) ;; (no error if not found): read-number (std in Emacs 22)

(eval-when-compile (require 'cl)) ;; case
                           ;; (plus, for Emacs<21: pop; for Emacs <20: cadr, when, unless)

;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User Options (Variables)

(defgroup doremi-frame-commands nil
  "Commands to incrementally adjust face attributes and frame parameters."
  :prefix "doremi-" :group 'doremi :group 'frames :group 'faces
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
doremi-frm.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/doremi-frm.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Doremi")
  :link '(emacs-commentary-link :tag "Commentary" "doremi-frm")
  )

(defcustom doremi-frame-config-ring-size 20
  "*Maximum number of stored frame configurations."
  :type 'integer :group 'Doremi-Frame-Commands)

(defcustom doremi-push-frame-config-for-cmds-flag nil
  "*Non-nil means commands that change frame config save it first.
This is done by advising all commands that change frame configuration
when library `doremi-frm.el' is loaded."
  :type 'boolean :group 'Doremi-Frame-Commands)

(defcustom doremi-move-frame-wrap-within-display-flag t
  "*Non-nil means wrap frame movements within the display.
Commands `doremi-frame-horizontally' and `doremi-frame-vertically'
then move the frame back onto the display when it moves off of it.
If nil, you can move the frame as far off the display as you like."
  :type 'boolean :group 'doremi-frame-commands)

(defcustom doremi-wrap-color-flag t
  "*Non-nil means wrap color changes around past the max and min.
For example, if non-nil, a current color value has FFFF as the red
component, and the red component is incremented by 1, then the result
has a red component of 0000.  If nil, the same example yields FFFF,
because the red component is already at its maximum."
  :type 'boolean :group 'doremi-frame-commands)
 
;;; Internal Variables

(defvar doremi-frame-config-ring (make-ring doremi-frame-config-ring-size)
  "Frame configuration ring.")

;; An Emacs 22 bug doesn't let us add t as a third arg here for `copy-face'.
(defvar doremi-last-face-value (cons 'doremi-last-face
                                     (copy-face 'default 'doremi-last-face))
  "Previous value of the last face changed by Do Re Mi.
That is, changed by `doremi-face-*' or `doremi-undo-last-face-change',
but not by `doremi-all-faces-*'.

A `consp' with the face name as `car' and the face value as `cdr'.
The face named `doremi-last-face' is a copy of the face before the
change.

Command `doremi-undo-last-face-change' swaps the `cdr' with the
current value of the face named by the `car', so it toggles between
the last two values of the face.")

(defvar doremi-last-frame-color nil
  "Previous value of last frame color changed by Do Re Mi.
That is, changed by `doremi-fg' or `doremi-bg' (or
`doremi-undo-last-frame-color-change'), but not by
`doremi-all-frames-fg' or `doremi-all-frames-bg'.

A `consp' with `foreground-color' or `background-color' as `car' and
the color as `cdr'.

Command `doremi-undo-last-frame-color-change' swaps this with the
current color, so it toggles between the last two values.")
 
;;; Miscellaneous Do Re Mi Frame Commands

;; This command uses an incremental growth function, `enlarge-font',
;; which is defined in `frame-cmds.el'.
;;;###autoload
(defun doremi-font-size (&optional increment frame)
  "Change font size for FRAME by INCREMENT.
Interactively, INCREMENT is given by the prefix argument.
Optional FRAME parameter defaults to current frame."
  (interactive "p")
  (doremi (lambda (inc) (enlarge-font inc frame)
            (cdr (assq 'font (frame-parameters frame))))
          (cdr (assq 'font (frame-parameters frame)))
          increment
          t))


;; You can replace the enumeration list (x-list-fonts "*") with a list
;; of fonts you have.  A short list is easier to work with, but you
;; can use long lists like these too:
;;   (x-list-fonts "*")
;;   (append w32-fixed-font-alist (list (generate-fontset-menu)))
;;
;; For example, you can use a short list like this:
;;
;; ("-*-Garamond-normal-i-*-*-*-*-96-96-p-*-iso8859-2"
;;  "-*-*-normal-r-*-*-15-112-96-96-c-*-fontset-iso8859_1_15"
;;  "-*-Arial-bold-i-*-*-*-*-96-96-p-*-iso8859-1"
;;  "-*-Century Gothic-bold-i-*-*-*-*-96-96-p-*-iso8859-5"
;;  "-*-Microsoft Sans Serif-normal-r-*-*-*-*-96-96-p-*-iso8859-4")
;;
;;;###autoload
(defun doremi-font ()
  "Successively cycle among fonts, choosing by name.
Operates on the current frame. Cycled font list is (x-list-fonts \"*\")."
  (interactive)
  (doremi (lambda (newval) (set-frame-font newval) newval)
          (frame-parameter (selected-frame) 'font)
          nil                           ; ignored
          nil                           ; ignored
          (x-list-fonts "*")
          'extend))


;; This command uses an enumeration list, `hexrgb-defined-colors'. See also `doremi-bg'.
;;;###autoload
(defun doremi-bg-color-name ()
  "Successively cycle among background colors, choosing by name.
Operates on the current frame."
  (interactive)
  (let ((fr (selected-frame)))
    (doremi (lambda (newval) (set-background-color newval) newval)
            (frame-parameter fr 'background-color)
            nil                         ; ignored
            nil                         ; ignored
            hexrgb-defined-colors
            t)                          ; Add current color, if not in list.
    (frame-update-face-colors fr)))     ; Update the way faces display with new bg.


;; This command uses an absolute setting function.  It rebinds `doremi-up-key'
;; and `doremi-down-key' so they more are intuitive for width.
;;;###autoload
(defun doremi-frame-width (&optional increment frame)
  "Change width of current frame incrementally.
Width of frame FRAME is increased in increments of amount INCREMENT."
  (interactive "p")
  (let ((doremi-up-key 'left)           ; More intuitive keys for width.
        (doremi-boost-up-key 'M-left)
        (doremi-down-key 'right)
        (doremi-boost-down-key 'M-right))
    (doremi (lambda (new-val) (set-frame-width frame new-val) new-val)
            (frame-width frame)
            (- increment)))             ; Reverse, so arrows correspond.
  (when (member (car unread-command-events)
                (list doremi-up-key doremi-down-key
                      doremi-boost-up-key doremi-boost-down-key))
    (doremi-frame-height increment frame)))

;; This command uses an absolute setting function.
;;;###autoload
(defun doremi-frame-height (&optional increment frame)
  "Change height of current frame incrementally.
Height of frame FRAME is increased in increments of amount INCREMENT."
  (interactive "p")
  (doremi (lambda (new-val) (set-frame-height frame new-val) new-val)
          (frame-height frame)
          (- increment))                ; Reverse, so arrows correspond.
  (when (member (car unread-command-events) '(left right M-left M-right))
    (doremi-frame-width increment frame)))

;; ;; This does the same thing as `doremi-frame-height'.
;; ;; Example command that uses an incrementing function, `enlarge-frame',
;; ;; defined in `frame-cmds.el'.
;; (defun doremi-frame-height-bis (&optional increment frame)
;;   "Change frame height incrementally."
;;   (interactive "p")
;;   (doremi (lambda (inc) (enlarge-frame inc frame) (frame-height frame))
;;             (frame-height frame)
;;             (- increment)               ; Reverse, so arrows correspond.
;;             t))

;; Move frame left/right incrementally.
;; This command uses an incremental growth function.
;; Rebinds `doremi-up-key' and `doremi-down-key': more intuitive for horizontal.
;; Uses default increment value of 10.
;;;###autoload
(defun doremi-frame-horizontally (&optional increment frame)
  "Move frame left/right incrementally.
Prefix arg is the INCREMENT to move (default value interactively: 10).
FRAME defaults to the selected frame.

Variable `doremi-move-frame-wrap-within-display-flag' controls whether
or not you can move the frame completely off the display. The default
behavior (value `t') is to wrap frame movement around the display."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 10)))
  (setq increment (or increment 10))    ; 1 is too small
  (setq frame (or frame (selected-frame)))
  (let ((doremi-up-key 'left)           ; More intuitive keys for width.
        (doremi-boost-up-key 'M-left)
        (doremi-down-key 'right)
        (doremi-boost-down-key 'M-right))
    (doremi
     (lambda (incr)
       (modify-frame-parameters
        frame
        (list (list 'left '+ (doremi-frame-new-position frame 'left incr))))
       (frame-geom-spec-cons (assq 'left (frame-parameters frame)) frame))
     (frame-geom-spec-cons (assq 'left (frame-parameters frame)) frame)
     (- increment)                      ; Reverse, so arrows correspond.
     t))
  (when (member (car unread-command-events)
                (list doremi-up-key doremi-down-key
                      doremi-boost-up-key doremi-boost-down-key))
    (doremi-frame-vertically increment frame)))



;; Move frame up/down incrementally.
;; This command uses an incremental growth function.
;; Uses default increment value of 10.
;;;###autoload
(defun doremi-frame-vertically (&optional increment frame)
  "Move frame up/down incrementally.
Prefix arg is the INCREMENT to move (default value interactively: 10).
FRAME defaults to the selected frame.

Variable `doremi-move-frame-wrap-within-display-flag' controls whether or
not you can move the frame completely off the display. The default
behavior (value `t') is to wrap frame movement around the display."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 10)))
  (setq increment (or increment 10))    ; 1 is too small
  (setq frame (or frame (selected-frame)))
  (doremi (lambda (incr)
            (modify-frame-parameters
             frame
             (list (list 'top '+ (doremi-frame-new-position frame 'top incr))))
            (frame-geom-spec-cons (assq 'top (frame-parameters frame)) frame))
          (frame-geom-spec-cons (assq 'top (frame-parameters frame)) frame)
          (- increment)               ; Reverse, so arrows correspond.
          t)
  (when (member (car unread-command-events) '(left right M-left M-right))
    (doremi-frame-horizontally increment frame)))

(defun doremi-frame-new-position (frame type incr)
  "Return the new TYPE position of FRAME, incremented by INCR.
TYPE is `left' or `top'.
INCR is the increment to use when changing the position."
  (let ((new-pos
         (+ incr (cadr (frame-geom-value-cons
                        type (cdr (assq type (frame-parameters frame)))))))
        (display-dimension
         (if (eq 'left type) (x-display-pixel-width) (x-display-pixel-height)))
        (frame-dimension
         (if (eq 'left type) (frame-pixel-width frame) (frame-pixel-height frame))))
    (if (not doremi-move-frame-wrap-within-display-flag)
        new-pos
      (when (< new-pos (- frame-dimension)) (setq new-pos display-dimension))
      (when (> new-pos display-dimension) (setq new-pos (- frame-dimension)))
      new-pos)))

;;;###autoload
(defun doremi-push-current-frame-config ()
  "Push the current frame configuration to `doremi-frame-config-ring'
after removing frame parameters `buffer-list' and `minibuffer'."
  (let ((curr-conf (doremi-frame-config-wo-parameters (current-frame-configuration)
                                                      '(buffer-list minibuffer))))
    (unless (ring-member doremi-frame-config-ring curr-conf)
      (ring-insert doremi-frame-config-ring curr-conf))))


;;;###autoload
(defun doremi-frame-config-wo-parameters (frame-config params-to-remove)
  "A copy of FRAME-CONFIG, but without frame parameters PARAMS-TO-REMOVE."
  (cons 'frame-configuration
        (mapcar (lambda (fr+parms)
                  (let ((parms (copy-sequence (nth 1 fr+parms))))
                    (list (car fr+parms) ; frame
                          (delete-if (lambda (parm) (memq (car parm) params-to-remove))
                                     parms)
                          (nth 2 fr+parms)))) ; window config
                (cdr frame-config))))   ; frames alist


;; NOTE: Frame parameters `buffer-list' and `minibuffer' are ignored
;;       when determining if two frame configurations are equal here.
;;;###autoload
(defun doremi-push-frame-config-for-command (command)
  "Advise COMMAND to save frame configuration.
You can restore previous frame configurations with \\[doremi-frame-configs]."
  (when (featurep 'ring+)
    (eval
     `(defadvice ,command (around doremi-push-frame-config-for-command activate)
        "Saves frame configuration. You can restore previous frame configuration \
with \\[doremi-frame-configs]."
        (doremi-push-current-frame-config)
        (when (fboundp 'frame-configuration-to-register) ; Defined in `frame-cmds.el'
          (frame-configuration-to-register frame-config-register))
        ad-do-it                        ; COMMAND code is executed here.
        (when (interactive-p)
          (message
           (substitute-command-keys
            (if (fboundp 'jump-to-frame-config-register) ; Defined in `frame-cmds.el'
                (format "Use `\\[jump-to-frame-config-register]' (`C-x r j %c') or \
`\\[doremi-frame-configs]' to restore frames as before (undo)." frame-config-register)
              "Use `\\[doremi-frame-configs]' to restore frames as before (undo)."))))))))


;; Undo (rotate) frame configuration changes made by the
;; frame-changing commands defined here (see mapcar, at end of file).
;;
;; Note:
;;;###autoload
(defun doremi-frame-configs ()
  "Cycle among frame configurations recorded in `doremi-frame-config-ring'."
  (interactive)
  (when (featurep 'ring+)
    (doremi (lambda (newval)            ; Cycle among previous frame configs.
              (set-frame-configuration (ring-next doremi-frame-config-ring newval))
              newval)
            (doremi-frame-config-wo-parameters (current-frame-configuration)
                                               '(buffer-list minibuffer))
            nil
            nil
            doremi-frame-config-ring
            t)))
 
;;; Background Frame Color Commands

;;;###autoload
(defun doremi-bg-red (&optional increment)
  "Change frame background red value incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?r increment))

;;;###autoload
(defun doremi-bg-green (&optional increment)
  "Change frame background green value incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?g increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-blue (&optional increment)
  "Change frame background blue value incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?b increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-cyan (&optional increment)
  "Change frame background cyan value incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?r (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-magenta (&optional increment)
  "Change frame background green value incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?g (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-yellow (&optional increment)
  "Change frame background blue value incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?b (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-hue (&optional increment)
  "Change frame background hue incrementally.  See `doremi-bg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-bg ?h increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-saturation (&optional increment)
  "Change frame background color saturation incrementally.
Prefix arg is the INCREMENT to change.  See `doremi-bg'."
  (interactive "p")
  (doremi-bg ?s increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-bg-value (&optional increment)
  "Change frame background brightness (HSV \"value\") incrementally.
Prefix arg is the INCREMENT to change.  See `doremi-bg'."
  (interactive "p")
  (doremi-bg ?v increment nil (consp current-prefix-arg)))

(defalias 'doremi-bg-brightness 'doremi-bg-value)
(defalias 'doremi-bg-purity 'doremi-bg-saturation)

;;;###autoload
(defun doremi-bg (component &optional increment frame pickup-p)
  "Change FRAME's background color incrementally.
Optional arg FRAME defaults to the selected frame.

You are prompted for the color COMPONENT to increment/decrement (a
character):
  `r' - red
  `g' - green
  `b' - blue
  `h' - hue (basic color)
  `s' - saturation (purity)
  `v' - value (brightness)
The default is `h' (hue).

Tip: To increment or decrement the cyan, magenta, or yellow component,
     just decrement or increment the red, green, or blue component,
     respectively.  CMY is just the opposite direction from RGB.

You can change at any time to increment/decrement a different color
component (r, g, b, h, s, or v).  For example, you can type `r' and
use the arrow keys or mouse wheel to change the red component, then
type `b' and use the arrows or wheel to change the blue component, and
so on, all in the same call to `doremi-bg'.

The initial color value is converted to a hexadecimal RGB (red, green,
blue) string that starts with \"#\".  The initial value is the current
background color of the selected frame.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively, PICKUP-P
is non-nil), then the frame background is first set to the value of
`eyedrop-picked-background'.  This happens only if library
`eyedropper.el' or `palette.el' is loaded.  This lets you pick up a
background color from somewhere, using `eyedrop-pick-background-at-*',
and then use that as the initial value for `doremi-bg'.

Colors can be expressed in Emacs as color names or hex RGB strings.
Depending on your operating system, the RGB components for a given
Emacs color can have different numbers of hex digits.  For example, on
one system RGB component values might vary from 000 to FFF; on another
system they might vary from 0000 to FFFF.  Incrementing or
decrementing a given color's RGB spec makes it roll over when the
limit (say 000 or FFF) is reached.

Prefix arg is the INCREMENT to change; the default value is 1.  Use a
prefix argument to supply a different INCREMENT.  For RGB, INCREMENT
is actually multiplied by 256.  If you need finer control than this,
use command `doremi-increment-background-color' to refine the color.

As for all Do Re Mi incrementation commands, use `doremi-boost-up-key'
and `doremi-boost-down-key' for faster incrementation.  The change is
`doremi-boost-scale-factor' times faster than for `doremi-up-key' and
`doremi-down-key'."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (setq increment (or increment 1))
  (setq doremi-last-frame-color (assq 'background-color (frame-parameters frame)))
  (when (and (or pickup-p (and (interactive-p) (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-background) eyedrop-picked-background)
    (doremi-set-background-color eyedrop-picked-background frame))
  (doremi (lambda (inc)
            (doremi-increment-background-color component inc frame)
            (cdr (assq 'background-color (frame-parameters frame))))
          (cdr (assq 'background-color (frame-parameters frame)))
          (if (memq component '(?r ?g ?b)) (* 256 increment) increment)
          t)
  (let ((next-component (pop unread-command-events)))
    (frame-update-face-colors frame)    ; Update the way faces display
    (when (member next-component '(?r ?g ?b ?h ?s ?v))
      (doremi-bg next-component increment frame))))

;;;###autoload
(defun doremi-all-frames-bg (component increment)
  "Change background color of all frames incrementally.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-bg'.  This command behaves similarly, but it is
the background color of all frames that is changed, not one frame.

For RGB, INCREMENT is actually multiplied by 256.  If you need finer
control than this, use command `doremi-increment-background-color' to
refine the color.

Option `doremi-wrap-color-flag' is bound to nil during this command."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (let ((doremi-wrap-color-flag nil))
    (doremi (lambda (inc)
              (dolist (frame (frame-list))
                (doremi-increment-background-color component inc frame))
              "- N/A -")                ; Irrelevant
            "- N/A -"                   ; Irrelevant
            (if (memq component '(?r ?g ?b)) (* 256 increment) increment)
            t)
    (let ((next-component (pop unread-command-events)))
      (when (member next-component '(?r ?g ?b ?h ?s ?v))
        (doremi-all-frames-bg next-component increment)))))

;;;###autoload
(defun doremi-increment-background-color (component increment &optional frame)
  "Change frame background color by INCREMENT of color COMPONENT.
You are prompted for the color COMPONENT to increment/decrement.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
INCREMENT is given by the prefix argument.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg'."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (doremi-increment-color component increment
                          (cdr (assq 'background-color (frame-parameters frame)))
                          'doremi-set-background-color (or frame (selected-frame)))
  (cdr (assq 'background-color (frame-parameters frame)))) ; Return new value.
 
;;; Foreground Frame Color Commands

;;;###autoload
(defun doremi-fg-red (&optional increment)
  "Change frame foreground red value incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?r increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-green (&optional increment)
  "Change frame foreground green value incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?g increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-blue (&optional increment)
  "Change frame foreground blue value incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?b increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-cyan (&optional increment)
  "Change frame foreground cyan value incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?r (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-magenta (&optional increment)
  "Change frame foreground green value incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?g (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-yellow (&optional increment)
  "Change frame foreground blue value incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?b (- increment) nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-hue (&optional increment)
  "Change frame foreground hue incrementally.  See `doremi-fg'.
Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?h increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-saturation (&optional increment)
  "Change frame foreground color saturation incrementally.
See `doremi-fg'.  Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?s increment nil (consp current-prefix-arg)))

;;;###autoload
(defun doremi-fg-value (&optional increment)
  "Change frame foreground brightness (HSV \"value\") incrementally.
See `doremi-fg'.  Prefix arg is the INCREMENT to change."
  (interactive "p")
  (doremi-fg ?v increment nil (consp current-prefix-arg)))

(defalias 'doremi-fg-brightness 'doremi-fg-value)
(defalias 'doremi-fg-purity 'doremi-fg-saturation)

;;;###autoload
(defun doremi-fg (component &optional increment frame pickup-p)
  "Change FRAME's foreground color incrementally.
Optional arg FRAME defaults to the selected frame.

You are prompted for the color COMPONENT to increment/decrement:
Prefix arg is the INCREMENT to change; the default value is 1.

For RGB, INCREMENT is actually multiplied by 256.  If you need finer
control than this, use command `doremi-increment-foreground-color' to
refine the color.

See `doremi-bg'; `doremi-fg' is the same, with \"foreground\"
substituted for \"background\"."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (setq increment (or increment 1))
  (setq doremi-last-frame-color (assq 'foreground-color (frame-parameters frame)))
  (when (and (or pickup-p (and (interactive-p) (or (consp current-prefix-arg))))
             (boundp 'eyedrop-picked-foreground) eyedrop-picked-foreground)
    (doremi-set-foreground-color eyedrop-picked-foreground frame))
  (doremi (lambda (inc)
            (doremi-increment-foreground-color component inc frame)
            (cdr (assq 'foreground-color (frame-parameters frame))))
          (cdr (assq 'foreground-color (frame-parameters frame)))
          (if (memq component '(?r ?g ?b)) (* 256 increment) increment)
          t)
  (let ((next-component (pop unread-command-events)))
    (frame-update-face-colors frame)    ; Update the way faces display
    (when (member next-component '(?r ?g ?b ?h ?s ?v))
      (doremi-fg next-component increment frame))))

;;;###autoload
(defun doremi-all-frames-fg (component increment)
  "Change foreground color of all frames incrementally.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-fg'.  This command behaves similarly, but it is
the foreground color of all frames that is changed, not one frame.

For RGB, INCREMENT is actually multiplied by 256.  If you need finer
control than this, use command `doremi-increment-foreground-color' to
refine the color.

Option `doremi-wrap-color-flag' is bound to nil during this command."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (let ((doremi-wrap-color-flag nil))
    (doremi (lambda (inc)
              (dolist (frame (frame-list))
                (doremi-increment-foreground-color component inc frame))
              "- N/A -")                ; Irrelevant
            "- N/A -"                   ; Irrelevant
            (if (memq component '(?r ?g ?b)) (* 256 increment) increment)
            t)
    (let ((next-component (pop unread-command-events)))
      (when (member next-component '(?r ?g ?b ?h ?s ?v))
        (doremi-all-frames-fg next-component increment)))))

;;;###autoload
(defun doremi-increment-foreground-color (component increment &optional frame)
  "Change foreground color of FRAME by INCREMENT of color COMPONENT.
You are prompted for the color COMPONENT to increment/decrement.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
INCREMENT is given by the prefix argument.
Optional arg FRAME defaults to the selected frame.  See `doremi-bg'."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (doremi-increment-color component increment
                          (cdr (assq 'foreground-color (frame-parameters frame)))
                          'doremi-set-foreground-color (or frame (selected-frame)))
  (cdr (assq 'foreground-color (frame-parameters frame)))) ; Return new value.

;;;###autoload
(defun doremi-undo-last-frame-color-change (&optional frame)
  "Restore last frame color changed by `doremi-fg' or `doremi-bg'.
This acts as a toggle between the last two values.
Optional arg FRAME defaults to the selected frame.
  The last frame-color change must have been to FRAME, or the result
  will likely not be what you expect.
Note: This does not undo changes made by `doremi-all-faces-fg' or
`doremi-all-faces-bg'"
  (interactive)
  (unless doremi-last-frame-color (error "No undo - no last frame color."))
  (let* ((temp (assq (car doremi-last-frame-color) (frame-parameters frame))))
    (modify-frame-parameters (or frame (selected-frame)) `(,doremi-last-frame-color))
    (setq doremi-last-frame-color temp)))
 
;;; Face and Color Commands

;;;###autoload
(defun doremi-undo-last-face-change ()
  "Return last face changed by `doremi-face-*' to its previous value.
This acts as a toggle between the last two values of the face.
Note: This does not undo changes made by `doremi-all-frames-fg' or
`doremi-all-frames-bg'."
  (interactive)
  (unless (facep 'doremi-last-face) (error "No undo - no last face."))
  (let ((face (car doremi-last-face-value)))
    (copy-face face 'doremi-temp-face)  ; Save current value face.
    (copy-face 'doremi-last-face face)  ; Restore previous value.
    (setq doremi-last-face-value        ; Be able to get back the changed value.
          (cons face (copy-face 'doremi-temp-face 'doremi-last-face)))))

;;;###autoload
(defun doremi-face-bg (face component &optional increment pickup-p)
  "Change background color of FACE incrementally.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-bg'.  This command behaves the same, except that
it is the background color of FACE that is changed, not the frame
background color.

For RGB, INCREMENT is actually multiplied by 256. If you need finer
control than this, use command `doremi-increment-face-bg-color' to
refine the color.

If `eyedrop-picked-background' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively, PICKUP-P
is non-nil), then the face background is first set to the value of
`eyedrop-picked-background'.  This happens only if library
`eyedropper.el' or `palette.el' is loaded.  This lets you pick up a
background color from somewhere, using `eyedrop-pick-background-at-*',
and then use that as the initial value for `doremi-face-bg'."
  (interactive (doremi-increment-face-color-read-args))
  (unless (facep face)
    (error "Command `doremi-face-bg': FACE arg is not a face name: %s" face))
  (let* ((special-display-regexps nil)
         (after-make-frame-functions nil)
         (sample-text (format "\n    Sample text in face `%s'\n" face))
         (pop-up-frame-alist
           (append '((name . "*Face Sample*") (height . 5) (auto-raise . t) (minibuffer)
                     (tool-bar-lines . 0) (menu-bar-lines . 0) (vertical-scroll-bars))
                   `((width ,@ (+ 4 (length sample-text))))
                   (frame-parameters))))
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value (cons face 'doremi-last-face))
    (when (and (or pickup-p (and (interactive-p) (or (consp current-prefix-arg))))
               (boundp 'eyedrop-picked-background) eyedrop-picked-background)
      (set-face-background face eyedrop-picked-background))
    (with-temp-buffer (get-buffer-create "*Face Sample*")
      (pop-to-buffer "*Face Sample*")
      (insert sample-text)
      (goto-char 2)
      (put-text-property 6 (progn (goto-char (point-min)) (forward-line 2) (point))
                         'face face)
      (save-excursion (insert (format "    Previous value of `%s'" face)))
      (put-text-property (point) (save-excursion (forward-line 1) (point))
                         'face 'doremi-last-face)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (doremi-face-bg-1 face component
                        (if (memq component '(?r ?g ?b)) (* 256 increment) increment))
      (if (one-window-p t) (delete-frame) (delete-window))))
  (when (get-buffer "*Face Sample*") (kill-buffer "*Face Sample*"))
  (let ((new-background  (face-background-20+ face nil 'default)))
    (if (fboundp 'set-face-attribute)
        (set-face-attribute face nil ':background new-background)
      (modify-face face nil new-background nil nil nil nil))
    (put face 'customized-face (list (list 't (list ':background new-background)))))
  (put face 'face-modified nil)
  (message (substitute-command-keys
            "Use `\\[doremi-undo-last-face-change]' to return to previous face \
value. Use `\\[customize-face]' to revisit changes.")))

;;;###autoload
(defun doremi-all-faces-bg (component increment)
  "Change background color of all faces incrementally, for all frames.
Option `doremi-wrap-color-flag' is bound to nil during this command.
See `doremi-all-faces-fg' - this is the same, with \"background\"
substituted for \"foreground\"."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (let ((doremi-wrap-color-flag nil))
    (doremi (lambda (inc)
              (dolist (face (face-list))
                (doremi-increment-face-bg-color face component inc))
              "- N/A -")                ; Irrelevant
            "- N/A -"                   ; Irrelevant
            increment
            t)
    (let ((next-component (pop unread-command-events)))
      (when (member next-component '(?r ?g ?b ?h ?s ?v))
        (doremi-all-faces-bg next-component increment)))))

;;;###autoload
(defun doremi-increment-face-bg-color (face component increment)
  "Change background color of FACE by INCREMENT of COMPONENT.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
INCREMENT is given by the prefix argument."
  (interactive (doremi-increment-face-color-read-args))
  (unless (facep face)
    (error "Command `doremi-increment-face-bg-color': FACE arg is not a face: %s" face))
  (when (interactive-p)
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value (cons face 'doremi-last-face)))
  (doremi-increment-color component increment
                          (or (face-background-20+ face nil 'default)
                              (cdr (assq 'background-color (frame-parameters))))
                          'set-face-background nil face)
  (face-background-20+ face nil 'default)) ; Return new value.

;;;###autoload
(defun doremi-face-fg (face component &optional increment pickup-p)
  "Change foreground color of FACE incrementally.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-bg'.  This command behaves the same, except that
it is the foreground color of FACE that is changed, not the frame
background color.

For RGB, INCREMENT is actually multiplied by 256.  If you need finer
control than this, use command `doremi-increment-face-fg-color' to
refine the color.

If `eyedrop-picked-foreground' is non-nil and you use plain `C-u'
instead of a numeric prefix argument (or, non-interactively,
PICKUP-P is non-nil), then the face foreground is first set to
the value of `eyedrop-picked-foreground'.  This happens only if
library `eyedropper.el' or `palette.el' is loaded.  This lets you
pick up a foreground color from somewhere, using
`eyedrop-pick-foreground-at-*', and then use that as the initial
value for `doremi-face-fg'."
  (interactive (doremi-increment-face-color-read-args))
  (unless (facep face)
    (error "Command `doremi-face-fg': FACE arg is not a face name: %s" face))
  (let* ((special-display-regexps nil)
         (after-make-frame-functions nil)
         (sample-text (format "\n    Sample text in face `%s'\n" face))
         (pop-up-frame-alist
           (append '((name . "*Face Sample*") (height . 5) (auto-raise . t) (minibuffer)
                     (tool-bar-lines . 0) (menu-bar-lines . 0) (vertical-scroll-bars))
                   `((width ,@ (+ 4 (length sample-text))))
                   (frame-parameters))))
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value (cons face 'doremi-last-face))
    (when (and (or pickup-p (and (interactive-p) (or (consp current-prefix-arg))))
               (boundp 'eyedrop-picked-foreground) eyedrop-picked-foreground)
      (set-face-foreground face eyedrop-picked-foreground))
    (with-temp-buffer (get-buffer-create "*Face Sample*")
      (pop-to-buffer "*Face Sample*")
      (insert sample-text)
      (goto-char 2)
      (put-text-property 6 (progn (goto-char (point-min)) (forward-line 2) (point))
                         'face face)
      (save-excursion (insert (format "    Previous value of `%s'" face)))
      (put-text-property (point) (save-excursion (forward-line 1) (point))
                         'face 'doremi-last-face)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (doremi-face-fg-1 face component
                        (if (memq component '(?r ?g ?b)) (* 256 increment) increment))
      (if (one-window-p t) (delete-frame) (delete-window))))
  (when (get-buffer "*Face Sample*") (kill-buffer "*Face Sample*"))
  (let ((new-foreground  (face-foreground-20+ face nil 'default)))
    (if (fboundp 'set-face-attribute)
        (set-face-attribute face nil ':foreground new-foreground)
      (modify-face face new-foreground nil nil nil nil nil))
    (put face 'customized-face (list (list 't (list ':foreground new-foreground)))))
  (put face 'face-modified nil)
  (message (substitute-command-keys
            "Use `\\[doremi-undo-last-face-change]' to return to previous face \
value. Use `\\[customize-face]' to revisit changes.")))

;;;###autoload
(defun doremi-all-faces-fg (component increment)
  "Change foreground color of all faces incrementally, for all frames.
You are prompted for the color COMPONENT to increment.
Prefix arg is the INCREMENT to change; the default value is 1.

See command `doremi-face-fg'.  This command behaves similarly, but it
is the foreground color of all faces that is changed, not one face.

For RGB, INCREMENT is actually multiplied by 256.  If you need finer
control than this, use command `doremi-increment-face-fg-color' to
refine the color.

Option `doremi-wrap-color-flag' is bound to nil during this command."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         (doremi-number-arg)))
  (let ((doremi-wrap-color-flag nil))
    (doremi (lambda (inc)
              (dolist (face (face-list))
                (doremi-increment-face-fg-color face component inc))
              "- N/A -")                ; Irrelevant
            "- N/A -"                   ; Irrelevant
            increment
            t)
    (let ((next-component (pop unread-command-events)))
      (when (member next-component '(?r ?g ?b ?h ?s ?v))
        (doremi-all-faces-fg next-component increment)))))

;;;###autoload
(defun doremi-increment-face-fg-color (face component increment)
  "Change foreground color of FACE by INCREMENT of COMPONENT.
The color is changed on all frames.
You are prompted for the FACE and the color COMPONENT to increment.
COMPONENT and INCREMENT are as for `doremi-increment-color'.
INCREMENT is given by the prefix argument."
  (interactive (doremi-increment-face-color-read-args))
  (unless (facep face)
    (error "Command `doremi-increment-face-fg-color': FACE arg is not a face: %s" face))
  (when (interactive-p)
    (copy-face face 'doremi-last-face)
    (setq doremi-last-face-value (cons face 'doremi-last-face)))
  (doremi-increment-color component increment
                          (or (face-foreground-20+ face nil 'default)
                              (cdr (assq 'foreground-color (frame-parameters))))
                          'set-face-foreground nil face)
  (face-foreground-20+ face nil 'default)) ; Return new value.

;;;###autoload
(defun doremi-increment-color-component (component color increment)
  "Increase COMPONENT (RGB or HSV) of COLOR by INCREMENT.
Returns a hexadecimal RGB code (a string) for the new color, of the
form #RRRRGGGGBBBB (RRRR: red, GGGG: green, BBBB: blue).

COMPONENT is the color component to increment (a character):
  `r' - red
  `g' - green
  `b' - blue
  `h' - hue (basic color)
  `s' - saturation (purity)
  `v' - value (brightness)
  The default is `h' (hue).
COLOR is a string representing a color.  It can be a color name or a
  hexadecimal RGB string of the form #RRRRGGGGBBBB.
INCREMENT is the increment to increase the value component of COLOR."
  (interactive
   (list (read-char-exclusive
          "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
         ;; Cannot use `facemenu-read-color' here, because we allow "#...".
         (completing-read "Color (name or #rrrrggggbbbb): " hexrgb-defined-colors-alist)
         (doremi-number-arg)))      
  (setq color (hexrgb-color-name-to-hex color))
  (let ((hlen (/ (1- (length color)) 3)) ; length of one hex color, R, G, or B
        result)
    (setq result
          (case component
            (?r (hexrgb-increment-red color hlen increment))
            (?g (hexrgb-increment-green color hlen increment))
            (?b (hexrgb-increment-blue color hlen increment))
            (otherwise
             ;; Convert RGB to HSV.  Convert range 0-65535 to range 0.0-1.0.
             (let* ((rgb (x-color-values color))
                    (red   (/ (float (nth 0 rgb)) 65535.0))
                    (green (/ (float (nth 1 rgb)) 65535.0))
                    (blue  (/ (float (nth 2 rgb)) 65535.0))
                    (hsv (hexrgb-rgb-to-hsv red green blue))
                    (hue        (nth 0 hsv))
                    (saturation (nth 1 hsv))
                    (value      (nth 2 hsv)))
               (case component
                 (?h (setq hue (+ hue (/ increment 100.0)))
                     (when (> hue 1.0) (setq hue (1- hue))))
                 (?v (setq value (+ value (/ increment 100.0)))
                     (when (> value 1.0) (setq value (1- value))))
                 (?s (setq saturation (+ saturation (/ increment 100.0)))
                     (when (> saturation 1.0) (setq saturation (1- saturation))))
                 ;; Default input COMPONENT is hue.
                 (otherwise (setq hue (+ hue (/ increment 100.0)))
                            (when (> hue 1.0) (setq hue (1- hue)))))
               (hexrgb-color-values-to-hex
                (mapcar (lambda (x) (floor (* x 65535.0)))
                        (hexrgb-hsv-to-rgb hue saturation value)))))))
    (when (interactive-p) (message result))
    result))
 
;;; Helper Functions for Face and Color Commands

(defun doremi-increment-face-color-read-args ()
  "Read arguments for functions `doremi*-face-*'.
That is, for functions `doremi-face-bg', `doremi-face-fg',
`doremi-increment-face-bg-color', and
`doremi-increment-face-fg-color'."
  ;; Args read are 1) face to change, 2) color component to change, 3) increment
  (list (if (< emacs-major-version 21)
            (read-face-name "Face to change: ")
          (read-face-name "Face to change"))
        (read-char-exclusive
         "Adjust red, green, blue, hue, saturation, or value? [rgbhsv]: ")
        (doremi-number-arg)))

(defun doremi-number-arg ()
  "Get a number argument: use prefix arg, if present, or prompt for it.
If `read-number' is defined, and no prefix arg, then use 1."
  (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
      (and (fboundp 'read-number) (read-number "Increment: " 1))
      1))

(defun doremi-face-bg-1 (face component increment)
  "Helper function for `doremi-face-bg'.
See it for FACE, COMPONENT, and INCREMENT."
  (doremi (lambda (inc)
            (doremi-increment-face-bg-color face component inc)
            (or (face-background-20+ face nil 'default) ; Frame bg as default.
                (cdr (assq 'background-color (frame-parameters)))))
          (or (face-background-20+ face nil 'default) ; Frame bg as default.
              (cdr (assq 'background-color (frame-parameters))))
          increment
          t)
  (let ((next-component (pop unread-command-events)))
    (when (member next-component '(?r ?g ?b ?h ?s ?v))
      (doremi-face-bg-1 face next-component increment))))

(defun doremi-face-fg-1 (face component increment)
  "Helper function for `doremi-face-fg'.
See it for FACE, COMPONENT, and INCREMENT."
  (doremi (lambda (inc)
            (doremi-increment-face-fg-color face component inc)
            (or (face-foreground-20+ face nil 'default) ; Frame fg as default.
                (cdr (assq 'foreground-color (frame-parameters)))))
          (or (face-foreground-20+ face nil 'default) ; Frame fg as default.
              (cdr (assq 'foreground-color (frame-parameters))))
          increment
          t)
  (let ((next-component (pop unread-command-events)))
    (when (member next-component '(?r ?g ?b ?h ?s ?v))
      (doremi-face-fg-1 face next-component increment))))

(defun doremi-increment-color (component increment color set-fn
                               &optional frame &rest args)
  "Increment COLOR by INCREMENT of COMPONENT and use for FRAME.
COMPONENT is as for `doremi-bg' (which see).
For HSV components, INCREMENT is limited here to range -100 to 100.
COLOR is the color to change.
SET-FN is the function used to set the new FRAME parameter value.
  It can set the background or foreground color.
  It must accept FRAME as its final argument.
Optional arg FRAME does *not* default to the selected frame, because
  for some SET-FNs, such as `set-face-foreground', nil = all frames.
ARGS are additional arguments for SET-FN, which appear before the
  color in the calling sequence.  For example, if SET-FN is
  `set-face-foreground', ARGS can be a list containing the face whose
  foreground is to be set."
  (unless (string-match "#" color)      ; Convert color name to #hhh...
    (setq color (hexrgb-color-values-to-hex (x-color-values color))))
  (let ((hlen (/ (1- (length color)) 3))) ; length of one hex color, R, G, or B
    (case component
      (?r (apply set-fn
                 (append args
                         (list (hexrgb-increment-red color hlen increment
                                                     doremi-wrap-color-flag))
                         (list frame))))
      (?g (apply set-fn
                 (append args
                         (list (hexrgb-increment-green color hlen increment
                                                       doremi-wrap-color-flag))
                         (list frame))))
      (?b (apply set-fn
                 (append args
                         (list (hexrgb-increment-blue color hlen increment
                                                      doremi-wrap-color-flag))
                         (list frame))))
      (otherwise
       (when (> increment 100)  (setq increment 100))
       (when (< increment -100) (setq increment -100))
       ;; Convert RGB to HSV.  Convert range 0-65535 to range 0.0-1.0.
       (let* ((rgb (x-color-values color))
              (red   (/ (float (nth 0 rgb)) 65535.0))
              (green (/ (float (nth 1 rgb)) 65535.0))
              (blue  (/ (float (nth 2 rgb)) 65535.0))
              (hsv (hexrgb-rgb-to-hsv red green blue))
              (hue        (nth 0 hsv))
              (saturation (nth 1 hsv))
              (value      (nth 2 hsv)))
         (case component
           (?h (setq hue (doremi-wrap-or-limit-color-component
                          (+ hue (/ increment 100.0)))))
           (?v (setq value (doremi-wrap-or-limit-color-component
                            (+ value (/ increment 100.0)))))
           (?s (setq saturation (doremi-wrap-or-limit-color-component
                                 (+ saturation (/ increment 100.0)))))
           ;; Default input COMPONENT is hue.
           (otherwise (setq hue (doremi-wrap-or-limit-color-component
                                 (+ hue (/ increment 100.0))))))
         (apply set-fn
                (append args
                        (list (hexrgb-color-values-to-hex
                               (mapcar (lambda (x) (floor (* x 65535.0)))
                                       (hexrgb-hsv-to-rgb hue saturation value))))
                        (list frame))))))))

(defun doremi-wrap-or-limit-color-component (component)
  "Limit color COMPONENT between 0.0 and 1.0.
Wrap around if `doremi-wrap-color-flag'."
  (if doremi-wrap-color-flag
      (doremi-wrap component  0.0  1.0)
    (doremi-limit component  0.0  1.0)))

(defalias 'toggle-doremi-wrap-color 'doremi-toggle-wrap-color)

(defun doremi-toggle-wrap-color ()
  "Toggle value of `doremi-wrap-color-flag'."
  (interactive)
  (setq doremi-wrap-color-flag (not doremi-wrap-color-flag)))

;; A function like this should be available as part of the Customize
;; code, but there is none.  This is OK for Emacs 22, but won't work for Emacs 20.
;; We don't bother to use this now.
(defun doremi-face-set (face spec)
  "Tell Customize that FACE has been set to value SPEC.
SPEC is as for `defface'."
  (let ((attrs (face-spec-choose spec)))
    (while attrs
      (let ((attribute (car attrs))
            (value (cadr attrs)))
        (when attribute (set-face-attribute face nil attribute value)))
      (setq attrs (cddr attrs))))
  (put face 'customized-face spec)
  (message (substitute-command-keys
            "Use `\\[customize-face]' to revisit changes.")))

;; A function like this should be available in Emacs.
(defun doremi-set-background-color (color-name &optional frame)
  "Set the background color of the FRAME to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current background color, use `frame-parameters'.
This is the same as `set-background-color', except that this accepts a
FRAME parameter."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (or frame (selected-frame))
                           (list (cons 'background-color color-name)))
  (frame-update-face-colors (or frame (selected-frame))))

;; A function like this should be available in Emacs.
(defun doremi-set-foreground-color (color-name &optional frame)
  "Set the foreground color of the FRAME to COLOR-NAME.
When called interactively, prompt for the name of the color to use.
To get the frame's current foreground color, use `frame-parameters'.
This is the same as `set-foreground-color', except that this accepts a
FRAME parameter."
  (interactive (list (facemenu-read-color)))
  (modify-frame-parameters (or frame (selected-frame))
                           (list (cons 'foreground-color color-name)))
  (frame-update-face-colors (or frame (selected-frame))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Apply `doremi-push-frame-config-for-command' to all commands that change
;; frame configuration.  Only do this if `doremi.el' is loaded, so
;; can use `doremi-frame-configs'.
(when (and doremi-push-frame-config-for-cmds-flag (featurep 'doremi))
  (mapcar 'doremi-push-frame-config-for-command
          '(doremi-bg doremi-bg-blue doremi-bg-brightness doremi-bg-color-name
                      doremi-bg-cyan doremi-bg-green doremi-bg-hue doremi-bg-magenta
                      doremi-bg-red doremi-bg-saturation doremi-bg-value doremi-bg-yellow
                      doremi-fg doremi-fg-blue doremi-fg-brightness doremi-fg-color-name
                      doremi-fg-cyan doremi-fg-green doremi-fg-hue doremi-fg-magenta
                      doremi-fg-red doremi-fg-saturation doremi-fg-value doremi-fg-yellow
                      doremi-face-bg doremi-face-fg doremi-font doremi-font-size
                      doremi-frame-height doremi-frame-width
                      doremi-increment-background-color doremi-increment-face-bg-color
                      doremi-increment-face-fg-color enlarge-font
                      doremi-frame-horizontally doremi-frame-vertically
                      doremi-undo-last-face-change doremi-undo-last-frame-color-change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doremi-frm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doremi-frm.el ends here
