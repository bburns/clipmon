;;; clipmon.el --- Clipboard monitor - automatically paste clipboard changes
;;
;; Copyright (c) 2015 Brian Burns
;;
;; Author: Brian Burns <bburns.km@gmail.com>
;; URL: https://github.com/bburns/clipmon
;; Keywords: convenience
;; Version: 20150114
;;
;; This package is NOT part of GNU Emacs.
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
;;; Commentary:
;;
;;;; Description
;; ----------------------------------------------------------------------------
;;
;; Clipmon is a clipboard monitor - it watches the system clipboard and pastes
;; any changes into the current location in Emacs.
;;
;; You can use it for taking notes from a webpage, for example - just copy the
;; text you want to save and it will be pasted into Emacs. Typically you turn it
;; on when you need to copy a lot of text from elsewhere, then turn it off when
;; you're done.
;;
;; It also helps to have an autocopy feature or addon for the browser, e.g.
;; AutoCopy 2 for Firefox [1] - then you can just select text to copy it to the
;; clipboard.
;;
;; [1] https://addons.mozilla.org/en-US/firefox/addon/autocopy-2/
;;
;;
;;;; Installation
;; ----------------------------------------------------------------------------
;;
;; It's simplest to use the package manager:
;;
;;     M-x package-install <return> clipmon <return>
;;
;; It will then be ready to use, and will also be loaded automatically the next
;; time you start Emacs.
;;
;;
;;;; Usage
;; ----------------------------------------------------------------------------
;;
;; Add something like this to your .emacs file to turn clipmon on and off:
;;
;;     (global-set-key (kbd "<M-f2>") 'clipmon-toggle)
;;
;; Then try it out - turn it on, go to another application and copy some text to
;; the clipboard - clipmon should detect it after a second or two and make a
;; sound. If you switch back to Emacs, there should be some new text in your
;; buffer.
;;
;; You can still yank and pull text in Emacs as usual while clipmon is on, since
;; it only looks at the system clipboard.
;;
;; If no change is detected after `clipmon-timeout' minutes, clipmon will turn
;; itself off automatically.
;;
;;
;;;; Options
;; ----------------------------------------------------------------------------
;;
;; There are various options you can set with customize:
;;
;;     (customize-group 'clipmon)
;;
;; or set them in your .emacs file - these are the default values:
;;
;;     (setq clipmon-cursor-color "red")  ; color for cursor when clipmon is on
;;     (setq clipmon-sound t)             ; t for included beep, or path or nil
;;     (setq clipmon-interval 2)          ; time interval to check clipboard (secs)
;;     (setq clipmon-timeout 5)           ; stop if no activity after n minutes
;;
;; transforms on the clipboard text are performed in this order:
;;
;;     (setq clipmon-trim-string t)          ; remove leading whitespace
;;     (setq clipmon-remove-regexp           ; remove text matching this regexp
;;           "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]")
;;     (setq clipmon-prefix "")              ; add to start of text
;;     (setq clipmon-suffix "\n\n")          ; add to end of text
;;     (setq clipmon-transform-function nil) ; additional transform function
;;
;;
;;;; Sound File
;; ----------------------------------------------------------------------------
;;
;; The sound file was created with Audacity [http://audacity.sourceforge.net/].
;; It's not too loud so hopefully it doesn't get annoying when you're taking a
;; lot of notes...
;;
;;
;;; Code:
;;
;;;; Public settings
;; ----------------------------------------------------------------------------

(defgroup clipmon nil
  "Clipboard monitor - automatically pastes clipboard changes."
  :group 'convenience
  :group 'killing
  :package-version '(clipmon . "20150114"))


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
  "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]"
  "Any text matching this regexp will be removed before pasting.
e.g. Wikipedia-style references with 1-3 digits - [3], [115]."
  :group 'clipmon
  :type 'regexp)

(defcustom clipmon-prefix ""
  "String to add to start of clipboard contents before pasting."
  :group 'clipmon
  :type 'string)

(defcustom clipmon-suffix "\n\n"
  "String to add to end of clipboard contents before pasting.
Default is two newlines, which leaves a blank line between clips.
To add a newline, type C-q C-j."
  :group 'clipmon
  :type 'string)

(defcustom clipmon-transform-function nil
  "Function to perform additional transformations on the clipboard text.
Receives one argument, the clipboard text - should return the changed text.
E.g. to make the text lowercase before pasting,
    (setq clipmon-transform-function (lambda (s) (downcase s)))"
  :group 'clipmon
  :type 'function
  :risky t)


;;;; Initialize
;; ----------------------------------------------------------------------------

; add item to Options menu
(define-key-after global-map [menu-bar options clipmon] ; path to new item
  '(menu-item "Clipboard monitor (paste changes)"
              clipmon-toggle ; function to call on click
              :help "Automatically paste changes from the system clipboard." ; tooltip
              :button (:toggle . clipmon--on)) ; show checkmark on/off
  'blink-cursor-mode) ; add after this item


;;;; Private variables
;; ----------------------------------------------------------------------------

(defvar clipmon--on nil "Clipmon status: t if monitoring clipboard.")
(defvar clipmon--timer nil "Timer handle for clipboard monitor.")
(defvar clipmon--timeout-start nil "Time that timeout timer was started.")
(defvar clipmon--previous-contents nil "Last contents of the clipboard.")
(defvar clipmon--cursor-color-original nil "Original cursor color.")

(defconst clipmon--folder
  (file-name-directory load-file-name)
  "Path to clipmon install folder.")

(defconst clipmon--included-sound-file
  (expand-file-name "clipmon.wav" clipmon--folder)
  "Path to included audio file.")



;;;; Public functions
;; ----------------------------------------------------------------------------

;;;###autoload
(defun clipmon-toggle ()
  "Turn clipmon (clipboard monitor) on and off."
  (interactive)
  (if clipmon--on (clipmon-stop) (clipmon-start)))


(defun clipmon-start ()
  "Start the clipboard timer, change cursor color, and play a sound."
  (interactive)
  (if clipmon--on
      (message "Clipboard monitor already running.")
    ; initialize
    (setq clipmon--previous-contents (clipmon--clipboard-contents))
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
    (setq clipmon--on t)))


(defun clipmon-stop ()
  "Stop the clipboard timer, restore cursor, and play a sound."
  (interactive)
  (if (null clipmon--on)
      (message "Clipboard monitor already stopped.")
    (cancel-timer clipmon--timer)
    (setq clipmon--timer nil)
    (if clipmon--cursor-color-original
        (set-face-background 'cursor clipmon--cursor-color-original))
    (message "Clipboard monitor stopped.")
    (clipmon--play-sound)
    (setq clipmon--on nil)))



;;;; Private functions
;; ----------------------------------------------------------------------------

(defun clipmon--check-clipboard ()
  "Check the clipboard and insert contents if changed.
Otherwise stop clipmon if it's been idle a while."
  (let ((s (clipmon--clipboard-contents))) ; s may actually be nil here
    (if (and s (not (string-equal s clipmon--previous-contents))) ; if changed
        (clipmon--on-clipboard-change s)
        ; otherwise stop monitor if it's been idle a while
        (if clipmon-timeout
            (let ((idletime (clipmon--seconds-since clipmon--timeout-start)))
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
  (if clipmon-trim-string (setq s (clipmon--trim-left s)))
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

(defalias 'clipmon--clipboard-contents 'x-get-selection-value
  "Get contents of system clipboard (as opposed to Emacs's kill ring).
Returns a string, or nil.")


(defun clipmon--trim-left (s)
  "Remove leading spaces from string."
  (replace-regexp-in-string  "^[ \t]+"  ""  s))


(defun clipmon--seconds-since (time)
  "Return number of seconds elapsed since the given time.
Time should be in Emacs time format (see `current-time').
Valid for up to 2**16 seconds = 65536 secs = 18hrs."
  (cadr (time-subtract (current-time) time)))


;;;; Footer

(provide 'clipmon)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; clipmon.el ends here
