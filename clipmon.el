;;; clipmon.el --- Clipboard monitor - automatically pastes clipboard changes.
;;; About:
;;
;; Copyright (c) 2014-2015 Brian Burns
;;
;; Author: Brian Burns <bburns.km@gmail.com>
;; Homepage: https://github.com/bburns/clipmon
;;
;; Version: 20150102
;; Keywords: convenience
;; License: GPLv3
;;
;; This file is NOT part of GNU Emacs.
;;
;;
;;; Commentary:
;;
;;;; Description
;; ----------------------------------------------------------------------------
;;
;; Clipmon is a clipboard monitor - it watches the system clipboard and pastes
;; any changes into the current location in Emacs.
;;
;; This makes it easier to take notes from a webpage, for example - just copy
;; the text you want to save and it will be pasted into Emacs. You can still use
;; the Emacs kill-ring with yank and pull as usual, since clipmon only looks at
;; the system clipboard.
;;
;; This works best when paired with an autocopy feature or plugin for the
;; browser, e.g. AutoCopy 2 for Firefox [1] - then you can just select text to
;; copy it to the clipboard.
;;
;; [1] https://addons.mozilla.org/en-US/firefox/addon/autocopy-2/
;;
;;
;;;; Usage
;; ----------------------------------------------------------------------------
;;
;; Make a key-binding like the following to turn clipmon on and off:
;;
;;     (global-set-key (kbd "<M-f2>") 'clipmon-toggle)
;;
;; Then turn it on and go to another application, like a browser, and copy some
;; text to the clipboard - clipmon should detect it after a second or two, and
;; make a sound. If you switch back to Emacs, it should have pasted the text
;; into your buffer.
;;
;;
;;;; Options
;; ----------------------------------------------------------------------------
;;
;; Once started, clipmon checks the clipboard for changes every
;; `clipmon-interval' seconds (default 2). If no change is detected after
;; `clipmon-timeout' minutes (default 5), clipmon will turn itself off
;; automatically.
;;
;; The cursor color can be set with `clipmon-cursor-color' - eg "red", or nil
;; for no change.
;;
;; A sound can be played on each change, and on starting and stopping clipmon.
;; The sound can be set with `clipmon-sound' - this can be t for the included
;; ding.wav file, a path to a sound file (.wav or .au), or nil for no sound.
;;
;; When selecting text to copy, it's sometimes difficult to avoid grabbing a
;; leading space - to remove these from the text, set `clipmon-trim-string' to t
;; (on by default).
;;
;; To filter the text some more set `clipmon-remove-regexp' - it will remove any
;; matching text before pasting. By default it is set to remove Wikipedia-style
;; references, e.g. "[3]".
;;
;; You can specify strings to add to the start and end of the text, with
;; `clipmon-prefix' and `clipmon-suffix'. By default the suffix is set to two
;; newlines, which will leave a blank line in between entries.
;;
;; For more customization, set `clipmon-transform-function' to a function that
;; takes the clipboard text and returns a modified version - e.g. to make the
;; text lowercase before pasting,
;;    (setq clipmon-transform-function (lambda (s) (downcase s)))
;;
;; See all options here: (customize-group 'clipmon)
;;
;;
;;;; License
;; ----------------------------------------------------------------------------
;;
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
;;
;;
;;; Code:
;;
;;;; Public settings
;; ----------------------------------------------------------------------------

(defgroup clipmon nil
  "Clipboard monitor - automatically paste clipboard changes."
  :group 'convenience
  :group 'killing
  :version "24.4")


(defcustom clipmon-cursor-color "red"
  "Color to set cursor when clipmon is on. Set to nil for no change."
  :group 'clipmon
  :type 'color)

(defcustom clipmon-sound t
  "Path to sound file to play on paste, t for included file, or nil.
Use t for the included sound file (see
`clipmon--included-sound-file'), nil for no sound, or path to an
audio file - Emacs can play .wav or .au files."
  ; Note: can't use `ding' here because it doesn't make a sound when Emacs
  ; doesn't have focus.
  :group 'clipmon
  :type '(radio
          (string :tag "Audio file (.wav or .au)")
          (boolean :tag "Included sound file")))

(defcustom clipmon-interval 2
  "Interval for checking clipboard, in seconds."
  :group 'clipmon
  :type 'integer)

(defcustom clipmon-timeout 5
  "Stop the timer if no clipboard activity after this many minutes.
Set to nil for no timeout."
  :group 'clipmon
  :type 'integer)


;; transforms on text - these are performed in this order

(defcustom clipmon-trim-string t
  "Remove leading whitespace from string before pasting if non-nil.
Often it's hard to select text without grabbing a leading space,
so this will remove it."
  :group 'clipmon
  :type 'boolean)

(defcustom clipmon-remove-regexp
  "\\[[0-9]+\\]\\|\\[citation needed\\]\\|\\[by whom?\\]"
  "Any text matching this regexp will be removed before pasting.
e.g. Wikipedia-style references - [3], [12]."
  :group 'clipmon
  :type 'regexp)

(defcustom clipmon-prefix ""
  "String to add to start of clipboard contents before pasting."
  :group 'clipmon
  :type 'string)

(defcustom clipmon-suffix "\n\n"
  "String to append to clipboard contents before pasting.
Default is two newlines, which leaves a blank line in between pastes."
  :group 'clipmon
  :type 'string)

(defcustom clipmon-transform-function nil
  "Function to perform additional transformations on the clipboard text.
Receives one argument, the clipboard text - should return the changed text.
E.g. to make the text lowercase before pasting,
    (setq clipmon-transform-function (lambda (s) (downcase s)))"
  :group 'clipmon
  :type 'function)


;;;; Initialize
;; ----------------------------------------------------------------------------

; add to Options menu
(define-key-after global-map [menu-bar options clipmon]
  '(menu-item "Clipboard monitor" clipmon-toggle
              :help "Automatically paste changes from the clipboard."
              :button (:toggle . clipmon))
  'blink-cursor-mode) ; add after this item


;;;; Private variables
;; ----------------------------------------------------------------------------

(defvar clipmon nil "Clipmon status: t if monitoring clipboard.")
(defvar clipmon--timer nil "Timer handle for clipboard monitor.")
(defvar clipmon--timeout-start nil "Time that timeout timer was started.")
(defvar clipmon--previous-contents nil "Last contents of the clipboard.")
(defvar clipmon--cursor-color-original nil "Original cursor color.")

(defconst clipmon--folder
  (file-name-directory load-file-name)
  "Path to clipmon install folder.")

(defconst clipmon--included-sound-file
  (expand-file-name "ding.wav" clipmon--folder)
  "Path to included audio file.")



;;;; Public functions
;; ----------------------------------------------------------------------------

;;;###autoload
(defun clipmon-toggle ()
  "Turn clipmon (clipboard monitor) on and off."
  (interactive)
  (if clipmon (clipmon-stop) (clipmon-start)))


(defun clipmon-start ()
  "Start the clipboard timer, change cursor color, and play a sound."
  (interactive)
  (if clipmon
      (message "Clipboard monitor already running.")
    ; initialize
    (setq clipmon--previous-contents (clipboard-contents))
    (setq clipmon--timeout-start (current-time))
    (setq clipmon--timer
          (run-at-time nil clipmon-interval 'clipmon--check-clipboard))
    ; change cursor color
    (when clipmon-cursor-color
      (setq clipmon--cursor-color-original (face-background 'cursor))
      (set-face-background 'cursor clipmon-cursor-color))
    (message
     "Clipboard monitor started with timer interval %d seconds. Stop with %s."
     clipmon-interval
     (substitute-command-keys "\\[clipmon-toggle]")) ; eg "<M-f2>"
    (clipmon--play-sound)
    (setq clipmon t)))


(defun clipmon-stop ()
  "Stop the clipboard timer, restore cursor, and play a sound."
  (interactive)
  (if (null clipmon)
      (message "Clipboard monitor already stopped.")
    (cancel-timer clipmon--timer)
    (setq clipmon--timer nil)
    (if clipmon--cursor-color-original
        (set-face-background 'cursor clipmon--cursor-color-original))
    (message "Clipboard monitor stopped.")
    (clipmon--play-sound)
    (setq clipmon nil)))



;;;; Private functions
;; ----------------------------------------------------------------------------

(defun clipmon--check-clipboard ()
  "Check the clipboard and insert contents if changed.
Otherwise stop clipmon if it's been idle a while."
  (let ((s (clipboard-contents))) ; s may actually be nil here
    (if (and s (not (string-equal s clipmon--previous-contents))) ; if changed
        (clipmon--on-clipboard-change s)
        ; otherwise stop monitor if it's been idle a while
        (if clipmon-timeout
            (let ((idletime (seconds-since clipmon--timeout-start)))
              (when (> idletime (* 60 clipmon-timeout))
                (clipmon-stop)
                (message
                 "Clipboard monitor stopped after %d minutes of inactivity."
                 clipmon-timeout)
                ))))))


(defun clipmon--on-clipboard-change (s)
  "Clipboard changed - transform text, insert it, play sound, update state."
  (setq clipmon--previous-contents s) ; save contents
  (setq s (clipmon--transform-text s))
  (insert s)
  (clipmon--play-sound)
  (setq clipmon--timeout-start (current-time))) ; restart timeout timer


(defun clipmon--transform-text (s)
  "Apply transformations to clipboard text."
  (if clipmon-trim-string (setq s (trim-left s)))
  (if clipmon-remove-regexp
      (setq s (replace-regexp-in-string clipmon-remove-regexp "" s)))
  (if clipmon-prefix (setq s (concat clipmon-prefix s)))
  (if clipmon-suffix (setq s (concat s clipmon-suffix)))
  (if clipmon-transform-function (setq s (funcall clipmon-transform-function s)))
  s)


(defun clipmon--play-sound ()
  "Play a user-specified sound file, the included sound file, or nothing."
  (if clipmon-sound
      (if (stringp clipmon-sound)
          (play-sound-file clipmon-sound)
          (play-sound-file clipmon--included-sound-file))))



;;;; Library functions
;; ----------------------------------------------------------------------------

(defalias 'clipboard-contents 'x-get-selection-value
  "Get contents of system clipboard, as opposed to Emacs's kill ring.
Returns a string, or nil.")


(defun trim-left (s)
  "Remove any leading spaces from s."
  (replace-regexp-in-string  "^[ \t]+"  ""  s))


(defun seconds-since (time)
  "Return number of seconds elapsed since the given time.
Time should be in Emacs time format (see `current-time').
Valid for up to 2**16 seconds = 65536 secs = 18hrs."
  (cadr (time-subtract (current-time) time)))


;;;; Provide

(provide 'clipmon)

;;; clipmon.el ends here
