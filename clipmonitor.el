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

; Todo:
; convert to a minor mode
; name: clipboard monitor, clipm, clipmonitor, autocopy, autoclip, autopaste?
; prefix: clipm, clipmon, cmon, clipmonitor?


; (clipmonitor-start)
; (clipmonitor-stop)


;;; User settings

(defcustom clipmonitor-interval 2 "Interval for checking clipboard, in seconds.")
(defcustom clipmonitor-timeout 30 "Stop the timer if no clipboard activity after this many seconds.")


;;; Private variables

(defvar clipmonitor-timer nil "Timer handle for clipboard monitor.")
(defvar clipmonitor-timeout-start nil "Time that timeout timer was started.")
(defvar clipmonitor-previous-contents nil "Last contents of the clipboard.")


;;; Keybindings

; (global-set-key (kbd "<f12>") 'clipmonitor-start)
; (global-set-key (kbd "<f12>") 'clipmonitor-stop)


;;; Public functions

(defun clipmonitor-start () (interactive)
  "Start the clipboard monitor timer, and check the clipboard contents each interval."
  (setq clipmonitor-previous-contents (clipboard-contents))
  (setq clipmonitor-timer (run-at-time nil clipmonitor-interval 'clipmonitor-tick))
  (message "Clipboard monitor started with timer interval %d seconds)." clipmonitor-interval)
  )

(defun clipmonitor-stop () (interactive)
  "Stop the clipboard monitor timer."
  (cancel-timer clipmonitor-timer)
  (message "Clipboard monitor stopped.")
  )


;;; Private functions

(defun clipmonitor-tick ()
  "Check the contents of the clipboard - if they've changed, paste the contents."
  (let ((current-contents (clipboard-contents)))
     ; if no change in clipboard, stop monitor if it's been idle a while
    (if (string= current-contents clipmonitor-previous-contents)
        (progn
          (let ((idletime (- (time) clipmonitor-timeout-start)))
            (when (> idletime clipmonitor-timeout)
              (clipmonitor-stop)
              (message "Clipboard monitor stopped after %d seconds of inactivity." clipmonitor-timeout)
              (beep)
              )))
      ; else clipboard contents changed
      (insert current-contents "\n")
      (setq clipmonitor-previous-contents current-contents)
      (setq clipmonitor-timeout-start (time))
      )))


;;; Library

(defun clipboard-contents (&optional arg)
  "Return the current or previous clipboard contents.
With nil or 0 argument, return the most recent item.
With numeric argument, return that item.
With :all, return all clipboard contents in a list."
  ; (if (null arg) (nth 0 kill-ring) (if (eq t arg) kill-ring (nth arg kill-ring))))
  ; (if (null arg) (nth 0 kill-ring) (if (integerp arg) (nth arg kill-ring) kill-ring)))
  (cond
   ; ((null arg) (nth 0 kill-ring))
   ; ((integerp arg) (nth arg kill-ring))
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



(provide 'clipmonitor)

; eof
