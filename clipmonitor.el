;;; clipmonitor (clipboard monitor)
;
; Description:
; Automatically paste contents of clipboard if change detected - 
; makes it easier to take notes from web pages.
;
; Usage: 
; Call clipmonitor-start to start timer
; It will check the clipboard every clipmonitor-interval seconds 
; If clipboard has changed, paste the contents 
; If no change detected after clipmonitor-timeout seconds, turn off the timer 
; Or call clipmonitor-stop manually to turn it off 
;
; Site: https://github.com/bburns/clipmonitor
; Author: brian burns <bburns.km@gmail.com>
; Date: 2014-02-21


; todo:
; convert to a minor mode
; only use external clipboard, not emacs one. so can cut/rearrange text while it's running.
 

; name: clipboard monitor, clipm, clipmonitor, autocopy, autoclip, autopaste?
; prefix: clipm, clipmon, cmon, clipmonitor?





;;; User settings

(defcustom clipmonitor-interval 2
  "Interval for checking clipboard, in seconds.")

(defcustom clipmonitor-timeout 5
  "Stop the timer if no clipboard activity after this many minutes. Set to nil for no timeout.")

(defcustom clipmonitor-newlines 1
  "Number of newlines to append after pasting clipboard contents.")

(defcustom clipmonitor-sound t
  "Sound to play when pasting text - t for default beep, nil for none, or path to sound file.")

(defcustom clipmonitor-trim-string t
  "Remove leading whitespace from string before pasting.")


;;; Private variables

(defvar clipmonitor-timer nil "Timer handle for clipboard monitor.")
(defvar clipmonitor-timeout-start nil "Time that timeout timer was started.")
(defvar clipmonitor-previous-contents nil "Last contents of the clipboard.")


;;; Keybindings

; (setq clipmonitor-key "<f12>")
(setq clipmonitor-key "<M-f2>")

(global-set-key (kbd clipmonitor-key) (lambda () (interactive)
                                (if clipmonitor-timer (clipmonitor-stop) (clipmonitor-start))))


;;; Public functions

(defun clipmonitor-start () (interactive)
  "Start the clipboard monitor timer, and check the clipboard contents each interval."
  (if clipmonitor-timer (message "Clipboard monitor already running. Stop with %s." clipmonitor-key)
    (setq clipmonitor-previous-contents (clipboard-contents))
    (setq clipmonitor-timeout-start (time))
    (setq clipmonitor-timer (run-at-time nil clipmonitor-interval 'clipmonitor-tick))
    (message "Clipboard monitor started with timer interval %d seconds. Stop with %s." clipmonitor-interval clipmonitor-key)
    (clipmonitor-play-sound)
    ))

(defun clipmonitor-stop () (interactive)
  "Stop the clipboard monitor timer."
  (cancel-timer clipmonitor-timer)
  (setq clipmonitor-timer nil)
  (message "Clipboard monitor stopped.")
  )


;;; Private functions

(defun clipmonitor-tick ()
  "Check the contents of the clipboard - if they've changed, paste the contents."
  (let ((current-contents (clipboard-contents)))
    (if (not (string= current-contents clipmonitor-previous-contents))
        (progn
          (if clipmonitor-trim-string
              (insert (s-trim-left current-contents))
            (insert current-contents))
          (dotimes (i clipmonitor-newlines) (insert "\n"))
          (if clipmonitor-sound (clipmonitor-play-sound))
          (setq clipmonitor-previous-contents current-contents)
          (setq clipmonitor-timeout-start (time)))
        ; no change in clipboard - stop monitor if it's been idle a while
        (if clipmonitor-timeout
            (let ((idletime (- (time) clipmonitor-timeout-start)))
              (when (> idletime (* 60 clipmonitor-timeout))
                (clipmonitor-stop)
                (message "Clipboard monitor stopped after %d minutes of inactivity." clipmonitor-timeout)
                (beep)
                )))
        )))


(defun clipmonitor-play-sound ()
  "Play a sound - if clipmonitor-sound is t, play the default beep, otherwise
if it's a string, play the sound file at the path."
  ; (if clipmonitor-sound
      ; (if (stringp clipmonitor-sound) (play-sound-file clipmonitor-sound)) (beep)))
  (cond
   ((eq clipmonitor-sound t) (beep))
   ((stringp clipmonitor-sound) (play-sound-file clipmonitor-sound)))) ;. catch error


;;; Library

(defun clipboard-contents (&optional arg)
  "Return the current or previous clipboard contents.
With nil or 0 argument, return the most recent item.
With numeric argument, return that item.
With :all, return all clipboard contents in a list."
  (cond
   ((null arg) (current-kill 0))
   ((integerp arg) (current-kill arg))
   ((eq :all arg) kill-ring)
   (t nil)))

; test
; (clipboard-contents)
; (clipboard-contents 0)
; (clipboard-contents 9)
; (clipboard-contents :all)
; (clipboard-contents t)
; (clipboard-contents "hi")


; from https://github.com/magnars/s.el
(defun s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))



;;; Testing

; (setq clipmonitor-timeout 5)
; timer-list
; (cancel-function-timers 'clipmonitor-tick)


;;; Provide

(provide 'clipmonitor)

; eof
