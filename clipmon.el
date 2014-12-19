;;; clipmon.el --- Clipboard monitor - automatically paste clipboard changes.
;;; About:

;; Copyright (C) 2014 Brian Burns
;;
;; Author: Brian Burns <bburns.km@gmail.com>
;; URL: https://github.com/bburns/clipmon
;;
;; Version: 0.1.20141130
;; Created: 2014-02-21
;; Package-Requires: ((s "0.0.1"))
;; Keywords: convenience
;; License: GPLv3


;;; Commentary:

;; Clipmon
;; =======
;;
;; Clipmon is a clipboard monitor - it detects changes to the clipboard and
;; pastes the contents into the current location.
;;
;; This makes it easier to take notes from a webpage, for example, by just
;; copying text you wish to save. This works well when paired with an autocopy
;; feature or plugin for the browser, e.g. AutoCopy 2 for Firefox.
;;
;; You can continue to use the Emacs kill-ring with yank and pull as usual,
;; since clipmon only looks at the system clipboard, as used by other
;; applications.
;;
;;
;; Usage
;; -----
;; Make a keybinding like the following to turn clipmon on and off: 
;;
;;   (global-set-key (kbd "<M-f2>") 'clipmon-toggle)
;;
;; Turn it on, then go to another application, e.g. a browser, and copy some
;; text to the clipboard. Clipmon should detect it after a second or two, and
;; make a sound - if you switch back to Emacs, it should have pasted the text
;; into your buffer. 
;;
;;
;; Options
;; -------
;;
;; Once started, Clipmon checks the clipboard for changes every
;; `clipmon-interval' seconds. If no change is detected after `clipmon-timeout'
;; minutes, the monitor will turn itself off automatically.
;;
;; The cursor color can be set with `clipmon-cursor-color' - eg "red", or nil
;; for no change. 
;;
;; A sound is played on each change, and on starting and stopping clipmon. The
;; sound can be set with `clipmon-sound' - this can be a filename (.wav or .au),
;; t for the default Emacs beep/flash, or nil for no sound.
;;
;; When selecting text to copy, it's sometimes difficult to avoid grabbing a
;; leading space - to remove these from the text before pasting, set
;; `clipmon-trim-string' to t (on by default).
;;
;; To filter the text more set `clipmon-remove-regexp' - it will remove any
;; matching text before pasting. By default it is set up to to remove
;; Wikipedia-style references, e.g. "[3]".
;;
;; You can also have newlines appended to the text - specify the number to add
;; with `clipmon-newlines'. The default is 2, giving a blank line between each
;; clip.
;;
;;
;; See all options here: (customize-group 'clipmon)
;;
;;
;; Todo
;; ----
;; - bug - try to start with empty kill ring - gives error on calling
;;   current-kill
;; - test with -Q
;; - package.el
;; - preserve echo message - often gets wiped out
;; - bug - lost timer
;;   when put laptop to sleep with it on, on resuming,
;;   it seemed to lose track of the timer, and couldn't turn it off without
;;   calling (cancel-function-timers 'clipmon--tick)
;;
;;
;; License
;; -------
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 's) ; string library


;;;; Public settings
; -----------------------------------------------------------------------------

(defgroup clipmon nil
  "Clipboard monitor - automatically paste clipboard changes."
  :group 'convenience
  :group 'killing
  :version "24.4"
  )

; ----

(defcustom clipmon-cursor-color "red"
  "Color to set cursor when clipmon is on. Set to nil for no change."
  :group 'clipmon
  :type 'color
  )

(defcustom clipmon-sound
  (concat (file-name-directory (or load-file-name (buffer-file-name))) "ding.wav")
  "Sound to play when pasting text - can be path to a sound file,
non-nil for the default Emacs beep, or nil for none."
  :group 'clipmon
  :type '(radio (string :tag "Audio file") (boolean :tag "Default beep"))
  )

(defcustom clipmon-interval 2
  "Interval for checking clipboard, in seconds."
  :group 'clipmon
  :type 'integer
  )

(defcustom clipmon-timeout 5
  "Stop the timer if no clipboard activity after this many minutes.
Set to nil for no timeout."
  :group 'clipmon
  :type 'integer
  )

(defcustom clipmon-trim-string t
  "Remove leading whitespace from string before pasting if non-nil.
Often it's hard to select text without grabbing a leading space,
so this will remove it for you."
  :group 'clipmon
  :type 'boolean
  )

(defcustom clipmon-remove-regexp
  "\\[[0-9]+\\]\\|\\[citation needed\\]\\|\\[by whom?\\]"
  "Regexp to match text to remove before pasting,
e.g. Wikipedia-style references - [3], [12]."
  :group 'clipmon
  :type 'regexp
  )

(defcustom clipmon-newlines 2
  "Number of newlines to append to clipboard contents before pasting."
  :group 'clipmon
  :type 'integer
  )



;;;; Public functions
; -----------------------------------------------------------------------------

;;;###autoload
(defun clipmon-toggle ()
  "Turn clipmon on and off."
  (interactive)
  (if clipmon--timer (clipmon-stop) (clipmon-start)))


(defun clipmon-start ()
  "Start the clipboard monitor timer, change cursor color, play a sound."
  (interactive)
  (let ((clipmon-keys (get-function-keys 'clipmon-toggle))) ; eg "<M-f2>, C-0"
    (if clipmon--timer
        (message "Clipboard monitor already running. Stop with %s." clipmon-keys)
      ; initialize
      (setq clipmon--previous-contents (clipboard-contents))
      (setq clipmon--timeout-start (current-time))
      (setq clipmon--timer (run-at-time nil clipmon-interval 'clipmon--tick))
      ; change cursor color
      (when clipmon-cursor-color
        (setq clipmon--cursor-color-original (face-background 'cursor))
        (set-face-background 'cursor clipmon-cursor-color)
        )
      (message
       "Clipboard monitor started with timer interval %d seconds. Stop with %s."
       clipmon-interval clipmon-keys)
      (clipmon--play-sound)
      )))


(defun clipmon-stop ()
  "Stop the clipboard monitor timer."
  (interactive)
  (cancel-timer clipmon--timer)
  (setq clipmon--timer nil)
  (if clipmon--cursor-color-original
      (set-face-background 'cursor clipmon--cursor-color-original))
  (message "Clipboard monitor stopped.")
  (clipmon--play-sound)
  )



;;;; Private variables
; -----------------------------------------------------------------------------

(defvar clipmon--timer nil "Timer handle for clipboard monitor.")
(defvar clipmon--timeout-start nil "Time that timeout timer was started.")
(defvar clipmon--previous-contents nil "Last contents of the clipboard.")
(defvar clipmon--cursor-color-original nil "Original cursor color.")



;;;; Private functions
; -----------------------------------------------------------------------------

(defun clipmon--tick ()
  "Check the contents of the clipboard - if it has changed, paste the contents."
  (let ((s (clipboard-contents))) ; s may actually be nil here
    (if (and s (not (string-equal s clipmon--previous-contents))) ; if changed
        (clipmon--paste s)
        ; otherwise stop monitor if it's been idle a while
        (if clipmon-timeout
            (let ((idletime (seconds-since clipmon--timeout-start)))
              (when (> idletime (* 60 clipmon-timeout))
                (clipmon-stop)
                (message
                 "Clipboard monitor stopped after %d minutes of inactivity."
                 clipmon-timeout)
                )))
        )))


(defun clipmon--paste (s)
  "Insert the string S at the current location, play sound, update state."
  (setq clipmon--previous-contents s)
  (if clipmon-trim-string (setq s (s-trim-left s)))
  (if clipmon-remove-regexp
      (setq s (replace-regexp-in-string clipmon-remove-regexp "" s)))
  (insert s)
  (dotimes (i clipmon-newlines) (insert "\n"))
  (if clipmon-sound (clipmon--play-sound))
  (setq clipmon--timeout-start (current-time)))


(defun clipmon--play-sound ()
  "Play a sound file, the default beep (or screen flash), or nothing."
  (if clipmon-sound
      (if (stringp clipmon-sound) (play-sound-file clipmon-sound) (beep))))



;;;; Library functions
; -----------------------------------------------------------------------------

(defun clipboard-contents ()
  "Get contents of system clipboard, as opposed to Emacs's kill ring.
Returns a string, or nil."
  (x-get-selection-value))


(defun get-function-keys (function)
  "Get list of keys bound to a function, as a string.
e.g. (get-function-keys 'ibuffer) => 'C-x C-b, <menu-bar>...'"
  (mapconcat 'key-description (where-is-internal function) ", "))


(defun seconds-since (time)
  "Return number of seconds elapsed since the given time.
TIME should be in Emacs time format (see current-time).
Valid for up to 2**16 seconds = 65536 secs = 18hrs."
  (cadr (time-subtract (current-time) time)))


;;;; Provide

(provide 'clipmon)

;;; clipmon.el ends here
