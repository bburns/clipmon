;;; clipmon.el --- Clipboard monitor - automatically paste clipboard changes.
;;; About:

;; Copyright (C) 2014 Brian Burns
;; 
;; Author: Brian Burns <bburns.km@gmail.com>
;; URL: https://github.com/bburns/clipmon
;; Version: 0.1.20141130
;; 
;; Keywords: clipboard, paste, autopaste
;; Package-Requires: ((s "0.0.1"))
;; License: MIT
;; Created: 2014-02-21


;;; Commentary:

;; Description
;; 
;; Automatically pastes contents of clipboard if change detected after
;; a certain time interval.
;; 
;; Useful for taking notes from the web. Best when paired with an autocopy
;; feature or plugin for the browser, so can just select text to copy it to the clipboard.
;; e.g. AutoCopy 2 for Firefox [1]
;;
;;
;; Usage
;;
;; Start the monitor with `clipmon-toggle' - it will check the clipboard every
;; `clipmon-interval' seconds and paste any new contents at the current
;; location. The cursor changes color to indicate the clipboard is being monitored.
;; 
;; If no change is detected after `clipmon-timeout' seconds, the
;; monitor will turn itself off, or you can call `clipmon-toggle' again to turn it off
;; manually.
;; 
;;
;; Keybindings
;; 
;; You can bind `clipmon-toggle' to a key, eg `M-f2', and use this to
;; start/stop clipmon. Add something like this to your .emacs file:
;;     (global-set-key (kbd "<M-f2>") 'clipmon-toggle)
;;
;;
;; Sound
;;
;; File: click.wav by Mike Koenig, from SoundBible.com [2]
;; License: Creative Commons Attribution 3.0 [3]
;;
;;
;; Links
;;
;; [1] https://addons.mozilla.org/en-US/firefox/addon/autocopy-2/
;; [2] http://soundbible.com/783-Click.html
;; [3] https://creativecommons.org/licenses/by/3.0/us/



;;; Todo:

;> only use external clipboard, not emacs one. so can cut/rearrange text while it's running.
;> preserve echo message? often gets wiped out

;> test with -Q
;> requirements, package load
;> make custom group
 
;> bug - try to start with empty kill ring - gives error on calling current-kill

;> bug - lost timer
; when put laptop to sleep with it on, on resuming,
; it seemed to lose track of the timer, and couldn't turn it off without
; calling (cancel-function-timers 'clipmon--tick)


;;; Code:
;;;; Library functions

(require 's) ; string library


; one problem is this seems to remove the contents after calling it, so it changes to nil,
; which makes clipmon add a blank line. wth?
; so... if it's nil just bail?
(defun clipboard-contents ()
  "Get contents of system clipboard, as opposed to Emacs's kill ring.
Returns a string, or nil."
  ; (or (x-get-selection-value) ""))
  (x-get-selection-value))

(defun function-get-keys (function)
  "Get list of keys bound to a function, as a string.
e.g. (function-get-keys 'ibuffer) => 'C-x C-b, <menu-bar>...'"
  (mapconcat 'key-description (where-is-internal function) ", "))

; test
; (function-get-keys 'where-is)
; (function-get-keys 'ibuffer)
; (function-get-keys 'undo)


(defun load-file-directory ()
  "Get directory of this file, as it is being loaded."
  ; used to get path to included sound file
  ; load-file-name has full path of current file
  (file-name-directory load-file-name))

; test - load-file-name is normally set by emacs during file load
; (let ((load-file-name "c:/foo/")) (load-file-directory))
; (let ((load-file-name "c:/foo/")) (concat (load-file-directory) "ting.wav"))


(defun seconds-since (time)
  "Return number of seconds elapsed since the given TIME.
Valid for up to 2**16 seconds = 65536 secs = 18hrs."
  (cadr (time-subtract (current-time) time)))

; (setq sometime (current-time))
; (21648 2846 877000 0)
; (seconds-since sometime)
; 166


;;;; Public settings

(defgroup clipmon nil
  "Options for clipmon package - clipboard monitor."
  :group 'convenience
  :group 'killing
  )

(defcustom clipmon-interval 2
  "Interval for checking clipboard, in seconds."
  :group 'clipmon
  :type 'integer
  )

(defcustom clipmon-timeout 5
  "Stop the timer if no clipboard activity after this many minutes. Set to nil for no timeout."
  :group 'clipmon
  :type 'integer
  )

(defcustom clipmon-trim-string t
  "Remove leading whitespace from string before pasting."
  :group 'clipmon
  :type 'boolean
  )

(defcustom clipmon-remove-regexp "\\[[0-9]+\\]\\|\\[citation needed\\]\\|\\[by whom?\\]"
  "Regexp to match text to remove before pasting, eg Wikipedia-style references - [3], [12]."
  :group 'clipmon
  :type 'regexp
  )

; test
; (setq clipmon-remove-regexp "\\[([0-9]+\\|citation needed)\\]")
; (setq clipmon-remove-regexp "\\[[0-9]+\\]")
; (setq clipmon-remove-regexp "\\[citation needed\\]")
; (setq clipmon-remove-regexp "\\[[0-9]+\\]\\|\\[citation needed\\]")
; (replace-regexp-in-string clipmon-remove-regexp "" "Page [37][citation needed]foo.")

; (replace-regexp-in-string  "[0-9]+" ""  "Page[37][foo]1932.")
; (replace-regexp-in-string  "\\[[0-9]+\\]" ""  "Page[37][foo]1932.")
; (replace-regexp-in-string  "\\[foo\\]" ""  "Page[37][foo]1932.")
; (replace-regexp-in-string  "foo\\|37" ""  "Page[37][foo]1932.")
; (replace-regexp-in-string "\\[[0-9]+\\]" "" "[3] foo [[bar]] [zork] [] [14.0] quirp[37][38][39]. changed[40][41]")


(defcustom clipmon-newlines 2
  "Number of newlines to append after pasting clipboard contents."
  :group 'clipmon
  :type 'integer
  )

(defcustom clipmon-sound (concat (load-file-directory) "click.wav")
  "Sound to play when pasting text - t for default beep, nil for none, or path to sound file."
  :group 'clipmon
  :type 'file
  )
; test
; (unbind 'clipmon-sound)
; (setq clipmon-sound nil)
; (setq clipmon-sound t)
; (setq clipmon-sound (concat (file-directory) "ting.wav"))
; (setq clipmon-sound (concat (file-directory) "ding.wav"))
; (setq clipmon-sound (concat (file-directory) "click.wav"))


(defcustom clipmon-cursor-color "red"
  "Color to set cursor when clipmon is on. Set to nil for no change."
  :group 'clipmon
  :type 'color
  )



;;;; Private variables

(defvar clipmon--timer nil "Timer handle for clipboard monitor.")
(defvar clipmon--timeout-start nil "Time that timeout timer was started.")
(defvar clipmon--previous-contents nil "Last contents of the clipboard.")
(defvar clipmon--cursor-color-original nil "Original cursor color.")


;;;; Public functions

(defun clipmon-toggle ()
  "Turn clipmon on and off."
  (interactive)
  (if clipmon--timer (clipmon-stop) (clipmon-start)))


(defun clipmon-start ()
  "Start the clipboard monitor timer, and check the clipboard contents each interval."
  (interactive)
  (let ((clipmon-keys (function-get-keys 'clipmon-toggle))) ; eg "<M-f2>, C-0"
    (if clipmon--timer (message "Clipboard monitor already running. Stop with %s." clipmon-keys)
      (setq clipmon--previous-contents (clipboard-contents))
      (setq clipmon--timeout-start (current-time))
      (setq clipmon--timer (run-at-time nil clipmon-interval 'clipmon--tick))
      (when clipmon-cursor-color
        (setq clipmon--cursor-color-original (face-background 'cursor))
        (set-face-background 'cursor clipmon-cursor-color)
        )
      (message "Clipboard monitor started with timer interval %d seconds. Stop with %s." clipmon-interval clipmon-keys)
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

; test
; (clipmon-start)
; timer-list
; (clipmon-stop)
; (cancel-function-timers 'clipmon--tick)



;;;; ------------------------------------------------------------
;;;; Private functions

(defun clipmon--tick ()
  "Check the contents of the clipboard - if it has changed, paste the contents."
  (let ((s (clipboard-contents)))
    (if (and s (not (string-equal s clipmon--previous-contents)))
        (clipmon--paste s)
        ; no change in clipboard - if timeout is set, stop monitor if it's been idle a while
        (if clipmon-timeout
            ; (let ((idletime (- (time) clipmon--timeout-start)))
            (let ((idletime (seconds-since clipmon--timeout-start)))
              (when (> idletime (* 60 clipmon-timeout))
                (clipmon-stop)
                (message "Clipboard monitor stopped after %d minutes of inactivity." clipmon-timeout)
                )))
        )))


(defun clipmon--paste (s)
  "Insert the string S at the current location, play sound, and update the state."
  (setq clipmon--previous-contents s)
  (if clipmon-trim-string (setq s (s-trim-left s)))
  ; (if clipmon-remove-wikipedia-references (setq s (replace-regexp-in-string "\\[[0-9]+\\]" "" s)))
  (if clipmon-remove-regexp (setq s (replace-regexp-in-string clipmon-remove-regexp "" s)))
  (insert s)
  (dotimes (i clipmon-newlines) (insert "\n"))
  (if clipmon-sound (clipmon--play-sound))
  (setq clipmon--timeout-start (current-time)))


(defun clipmon--play-sound ()
  "Play a sound file, the default beep, or nothing."
  (if clipmon-sound
      (if (stringp clipmon-sound) (play-sound-file clipmon-sound) (beep))))


;;;; Provide

(provide 'clipmon)

;;; clipmon.el ends here
