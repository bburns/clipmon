;;; clipmon.el --- Clipboard monitor - mirror system clipboard to kill ring and autoinsert
;;
;; Copyright (c) 2015 Brian Burns
;;
;; Author: Brian Burns <bburns.km@gmail.com>
;; URL: https://github.com/bburns/clipmon
;; Keywords: convenience
;; Version: 20150206
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
;; Clipmon is a clipboard monitor - it watches the system clipboard and can
;; automatically insert any new text into the current location in Emacs.
;;
;; It also adds changes to the system clipboard to the kill ring, making Emacs
;; into a cyclic clipboard manager for text - you can then use a package like
;; browse-kill-ring or helm-ring to view and manage your clipboard history.
;;
;; You can use it for taking notes from a webpage, for example - just copy the
;; text you want to save and it will be added to Emacs. It helps to have an
;; autocopy feature or addon for the browser, e.g. AutoCopy 2 for Firefox - then
;; you can just select text to add it to Emacs.
;;
;; Here's a diagram - text flows from the top to the bottom:
;;
;;                  +---------------------+
;;                  |   Other programs    |+
;;                  +---------------------+|
;;                   +---------------------+
;;                           /
;;                     +-----------+
;;                     |  System   |
;;                     | clipboard |
;;                     +-----------+
;;     OS                /
;;     ---------------------------------------------------
;;     Emacs           /
;;                    /
;;           +--------------+      +---------------+
;;           | clipmon-mode |......|  autoinsert   |
;;           +--------------+      +---------------+
;;                   |                     .
;;             +-----------+               .
;;             | Emacs     ++              .
;;             | kill ring ++       +--------------+
;;             +-----------+|+      |  transforms  |
;;              +-----------+|      +--------------+
;;               +-----------+             .
;;                      |                  .
;;                      | yank             . autoinsert
;;                 +--------------------------+
;;                 |      Emacs buffer        |
;;                 +--------------------------+
;;
;;
;; The solid line is turned on and off with `clipmon-mode', while the dotted
;; line is turned on and off with `clipmon-autoinsert-toggle', usually bound to a key.
;; There are also various transformations you can perform on the text, e.g.
;; adding newlines to the end.
;;
;; Emacs's kill-ring is like the system clipboard but with multiple items in it.
;; If you copy a bunch of things in another program, Emacs normally only knows
;; about the last one copied, but with clipmon mode on, it will monitor the
;; system clipboard and add any new text it sees to the kill ring.
;;
;;
;;;; Installation
;; ----------------------------------------------------------------------------
;;
;; It's simplest to use the package manager:
;;
;;     M-: (package-install 'clipmon)
;;
;; It will then be ready to use, and will also be available the next time you
;; start Emacs.
;;
;;
;;;; Usage
;; ----------------------------------------------------------------------------
;;
;; Add this to your .emacs file:
;;
;;     ;; monitor the system clipboard and add any changes to the kill ring
;;     (clipmon-mode 1)
;;
;;     ;; hit this when you want to insert changes at the current location
;;     (global-set-key (kbd "<M-f2>") 'clipmon-autoinsert-toggle)
;;
;; Try it out - turn on autoinsert with your keybinding (or select the item in
;; the Options menu), go to another application and copy some text to the
;; clipboard - clipmon should detect it after a second or two and make a beep.
;; If you switch back to Emacs, the text should be there in your buffer. You can
;; turn off autoinsert with the same keybinding.
;;
;; If no change is detected after a certain number of minutes, autoinsert will
;; turn itself off automatically with a beep. This was to prevent the author
;; from forgetting that autoinsert is on and accidentally adding text to his
;; buffer.
;;
;; Note that you can still yank and pull text in Emacs as usual while autoinsert
;; is on, since it only monitors the system clipboard.
;;
;; Also, if you happen to copy the same text to the clipboard twice,
;; clipmon won't know about the second time, as it is only able to detect
;; changes. And if you copy text faster than the timer interval is set it may
;; miss some changes.
;;
;;
;;;; Using as a clipboard manager
;; ----------------------------------------------------------------------------
;;
;; To try it out, make sure clipmon-mode is on (also accessible from the Options
;; menu) and autoinsert is off, then copy a few pieces of text from another
;; program (slower than the default timer interval of 2 seconds though). Switch
;; back to Emacs, and note that you can yank any of the text back with C-y, M-y,
;; M-y...
;;
;; You can use the package browse-kill-ring to manage the kill ring - call
;; `browse-kill-ring' to see the contents of the kill ring, insert from it,
;; delete items, etc. Helm also has a package called helm-ring, with the
;; function `helm-show-kill-ring'.
;;
;; You can also persist the kill ring between sessions if you'd like (though
;; note that this might involve writing sensitive information like passwords to
;; the disk - you could always delete such text from the kill ring though with
;; `browse-kill-ring-delete').
;;
;; To persist the kill ring, add this to your .emacs file:
;;
;;     (clipmon-persist)
;;     (setq savehist-autosave-interval (* 5 60)) ; save every 5 minutes (default)
;;
;; This will use Emacs's savehist library to save the kill ring, both at the end
;; of the session and at set intervals. Note though that savehist also saves
;; various other settings by default, including the minibuffer history - see
;; `savehist-mode' for more details.
;;
;;
;; The kill ring has a fixed number of entries which you can set, depending on
;; how you use the kill-ring, or how much history you want to save:
;;
;;     (setq kill-ring-max 500) ; default is 60 in Emacs 24.4
;;
;; To see how much space the kill-ring is taking up, call this function:
;;
;;     (clipmon-kill-ring-total)
;;     => 29670 characters
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
;;     (setq clipmon-timer-interval 2)       ; check system clipboard every n secs
;;     (setq clipmon-autoinsert-sound t)     ; t for included beep, or path or nil
;;     (setq clipmon-autoinsert-color "red") ; color of cursor when autoinsert is on
;;     (setq clipmon-autoinsert-timeout 5)   ; stop autoinsert after n mins inactivity
;;
;; before inserting the text, transformations are performed on it in this order:
;;
;;     (setq clipmon-transform-trim t)        ; remove leading whitespace
;;     (setq clipmon-transform-remove         ; remove text matching this regexp
;;           "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]")
;;     (setq clipmon-transform-prefix "")     ; add to start of text
;;     (setq clipmon-transform-suffix "\n\n") ; add to end of text
;;     (setq clipmon-transform-function nil)  ; additional transform function
;;
;;
;;;; Todo
;; ----------------------------------------------------------------------------
;;
;; - Prefix with C-u to set a target point, then allow cut/copy/pasting from
;;   within Emacs, eg to take notes from another buffer, or move text elsewhere.
;;
;;
;;;; History
;; ----------------------------------------------------------------------------
;;
;; 20150206 refactored to handle kill ring better
;;   clipmon-mode now just adds changes to the kill-ring.
;;   clipmon-autoinsert-toggle added to toggle automatic inserting of text.
;;   changed several setting names - all with aliases to old names.
;;   clipmon-action removed - no longer need to call kill-new or insert with it.
;; 20150131 added clipmon-action, to accommodate adding to kill-ring
;; 20150120 initial release
;;
;;
;;;; Sound File
;; ----------------------------------------------------------------------------
;;
;; The sound file was created with Audacity [http://audacity.sourceforge.net/].
;; It's a bit on the quiet side so hopefully it doesn't get annoying when you're
;; taking a lot of notes...
;;
;;
;;;; Feedback
;; ----------------------------------------------------------------------------
;;
;; Feedback is always welcome - for feature requests or bug reports, see the
;; Github issues page [https://github.com/bburns/clipmon/issues]. Pull requests
;; are welcome also.
;;
;; Thanks to tuhdo for suggesting using clipmon as a clipboard manager, and
;; Steve Purcell for initial feedback.
;;
;;
;;; Code:

;;;; Renamings
;; ----------------------------------------------------------------------------

;; just renaming some things here - must come before defcustoms.

(eval-when-compile ; because it's a macro
  (defalias 'clipmon--rename 'define-obsolete-variable-alias))

;; rename old to new
(clipmon--rename 'clipmon-interval      'clipmon-timer-interval     "20150206")
(clipmon--rename 'clipmon-cursor-color  'clipmon-autoinsert-color   "20150206")
(clipmon--rename 'clipmon-sound         'clipmon-autoinsert-sound   "20150206")
(clipmon--rename 'clipmon-timeout       'clipmon-autoinsert-timeout "20150206")
(clipmon--rename 'clipmon-trim          'clipmon-transform-trim     "20150206")
(clipmon--rename 'clipmon-remove-regexp 'clipmon-transform-remove   "20150206")
(clipmon--rename 'clipmon-prefix        'clipmon-transform-prefix   "20150206")
(clipmon--rename 'clipmon-suffix        'clipmon-transform-suffix   "20150206")

;; FIXME no way to mark as obsolete/removed?
(clipmon--rename 'clipmon-action        'clipmon-action-obsolete    "20150206")


;;;; Public settings
;; ----------------------------------------------------------------------------

(defgroup clipmon nil
  "Clipboard monitor - add clipboard contents to kill ring and automatically insert."
  :group 'convenience
  :group 'killing)

(defcustom clipmon-timer-interval 2
  "Interval for checking system clipboard for changes, in seconds."
  :group 'clipmon
  :type 'integer)

(defcustom clipmon-autoinsert-color "red"
  "Color to set cursor when clipmon autoinsert is on. Set to nil for no change."
  :group 'clipmon
  :type 'color)

(defcustom clipmon-autoinsert-sound t
  "Path to sound file to play on autoinsert, t for included file, or nil.
Use t for the included sound file (see
`clipmon--included-sound-file'), nil for no sound, or path to an
audio file - Emacs can play .wav or .au files."
  ; Note: can't use `ding' here because it doesn't make a sound when Emacs
  ; doesn't have focus.
  :group 'clipmon
  :type '(radio
          (string :tag "Audio file (.wav or .au)")
          (boolean :tag "Included sound file")))

(defcustom clipmon-autoinsert-timeout 5
  "Stop autoinsert if no system clipboard activity after this many minutes.
Set to nil for no timeout."
  :group 'clipmon
  :type 'integer)


;; transforms on text - these are performed in this order

(defcustom clipmon-transform-trim t
  "If non-nil, remove leading whitespace from string before autoinserting.
Often it's hard to select text without grabbing a leading space,
so this will remove it."
  :group 'clipmon
  :type 'boolean)

(defcustom clipmon-transform-remove
  "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]"
  "Any text matching this regexp will be removed before autoinserting.
e.g. Wikipedia-style references with 1-3 digits - [3], [115]."
  :group 'clipmon
  :type 'regexp)

(defcustom clipmon-transform-prefix ""
  "String to add to start of clipboard contents before autoinserting."
  :group 'clipmon
  :type 'string)

(defcustom clipmon-transform-suffix "\n\n"
  "String to add to end of clipboard contents before autoinserting.
Default is two newlines, which leaves a blank line between clips.
(To add a newline in the customize interface, type C-q C-j)."
  :group 'clipmon
  :type 'string)

(defcustom clipmon-transform-function nil
  "Function to perform additional transformations on text before autoinserting.
Receives one argument, the clipboard text - should return the changed text.
E.g. to make the text lowercase before pasting,
    (setq clipmon-transform-function (lambda (s) (downcase s)))"
  :group 'clipmon
  :type 'function
  :risky t)


;;;; Initialize
;; ----------------------------------------------------------------------------

; add items to Options menu
;;;###autoload
(define-key-after global-map [menu-bar options clipmon-separator] ; path to new item
  '(menu-item "---")
  'highlight-paren-mode) ; add after this

;;;###autoload
(define-key-after global-map [menu-bar options clipmon-killring] ; path to new item
  '(menu-item "Clipboard monitor (add to kill ring)"
              clipmon-mode ; function to call on click
              :help "Add changes to the system clipboard to Emacs's kill ring."
              :button (:toggle . clipmon-mode)) ; show checkmark on/off
  'clipmon-separator) ; add after this

;;;###autoload
(define-key-after global-map [menu-bar options clipmon-autoinsert] ; path to new item
  '(menu-item "Clipboard monitor (autoinsert)"
              clipmon-autoinsert-toggle ; function to call on click
              :help "Automatically insert changes from the system clipboard."
              :button (:toggle . clipmon--autoinsert)) ; show checkmark on/off
  'clipmon-killring) ; add after this


;;;; Private variables
;; ----------------------------------------------------------------------------

(defvar clipmon--timer nil "Timer handle for clipboard monitor.")
(defvar clipmon--autoinsert nil "t if autoinsert is on.")
(defvar clipmon--autoinsert-timeout-start nil "Time that timeout timer was started.")
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
(define-minor-mode clipmon-mode
  "Turn clipboard monitor on/off - watch system clipboard, add to kill ring.
Call `clipmon-autoinsert-toggle' to turn autoinsert on/off."
  :global t
  :lighter ""
  ; value of clipmon-mode is toggled before this implicitly
  (if clipmon-mode (clipmon-start) (clipmon-stop)))


(defun clipmon-start ()
  "Start clipboard monitor - watch system clipboard, add changes to kill ring."
  (interactive)
  (setq clipmon-mode t) ; in case called outside of clipmon-mode fn
  (if clipmon--timer
      (error "Clipboard monitor already running.")
    (setq clipmon--previous-contents (clipmon--clipboard-contents))
    (setq clipmon--timer
          (run-at-time nil clipmon-timer-interval 'clipmon--check-clipboard))
    (message "Clipboard monitor started - watching system clipboard, adding changes to kill ring.")
    ))


(defun clipmon-stop (&optional msg)
  "Stop clipboard monitor and autoinsert modes."
  (interactive)
  (setq clipmon-mode nil) ; in case called outside of clipmon-mode fn
  (if (null clipmon--timer)
      (error "Clipboard monitor already stopped.")
    (cancel-timer clipmon--timer)
    (setq clipmon--timer nil)
    (if clipmon--autoinsert (clipmon-autoinsert-stop)) ; turn off autoinsert also
    (message "Clipboard monitor stopped.")
    ))


;;;###autoload
(defun clipmon-autoinsert-toggle ()
  "Turn autoinsert on/off - watch system clipboard and insert changes.
Will change cursor color and play a sound. Text will be
transformed before insertion according to various settings - see
`clipmon--transform-text'."
  (interactive)
  ;; note: a minor mode would toggle the value here rather than in the fns
  (if clipmon--autoinsert (clipmon-autoinsert-stop) (clipmon-autoinsert-start)))


(defun clipmon-autoinsert-start ()
  "Turn on autoinsert - change cursor color, play sound, insert changes."
  (interactive)
  (if (null clipmon--timer) (clipmon-start)) ; make sure clipmon is on
  (if clipmon--autoinsert
      (error "Clipboard monitor autoinsert already on.")
    (setq clipmon--autoinsert-timeout-start (current-time))
    (when clipmon-autoinsert-color
      (setq clipmon--cursor-color-original (face-background 'cursor))
      (set-face-background 'cursor clipmon-autoinsert-color))
    (message
     "Clipboard monitor autoinsert started with timer interval %g seconds. Stop with %s."
     clipmon-timer-interval
     (substitute-command-keys "\\[clipmon-autoinsert-toggle]")) ; eg "<M-f2>"
    (clipmon--play-sound)
    (setq clipmon--autoinsert t)
    ))


(defun clipmon-autoinsert-stop (&optional msg)
  "Turn off autoinsert - restore cursor color and play sound."
  (interactive)
  (if (null clipmon--autoinsert)
      (error "Clipboard monitor autoinsert already off.")
    (if clipmon--cursor-color-original
        (set-face-background 'cursor clipmon--cursor-color-original))
    (message (or msg "Clipboard monitor autoinsert stopped."))
    (clipmon--play-sound)
    (setq clipmon--autoinsert nil)
    ))


;;;###autoload
(defun clipmon-persist ()
  "Persist the kill ring to disk using Emacs's savehist library.
Will save the kill ring at the end of the session and at various
intervals as specified by `savehist-autosave-interval'. Note that
savehist also includes various other Emacs settings by default,
including the minibuffer history - see `savehist-mode' for more
details."
  ; (require 'savehist)
  (defvar savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (savehist-mode 1))


;;;; Private functions
;; ----------------------------------------------------------------------------

(defun clipmon--check-clipboard ()
  "Check the clipboard and call `clipmon--on-clipboard-change' if changed.
Otherwise check autoinsert idle timer and stop if it's been idle a while."
  (let ((s (clipmon--clipboard-contents))) ; s may actually be nil here
    (if (and s (not (string-equal s clipmon--previous-contents))) ; if changed
        (clipmon--on-clipboard-change s)
      ;; otherwise stop autoinsert if clipboard has been idle a while
      (if (and clipmon--autoinsert clipmon-autoinsert-timeout)
          (let ((idle-seconds (clipmon--seconds-since clipmon--autoinsert-timeout-start)))
            (when (>= idle-seconds (* 60 clipmon-autoinsert-timeout))
              (clipmon-autoinsert-stop (format
                  "Clipboard monitor autoinsert stopped after %g minutes of inactivity."
                  clipmon-autoinsert-timeout))
              ))))))


(defun clipmon--on-clipboard-change (s)
  "Clipboard changed - add text to kill ring, and optionally insert it."
  (setq clipmon--previous-contents s) ; save contents
  (kill-new s) ; add to kill ring
  (when clipmon--autoinsert
    (setq s (clipmon--autoinsert-transform-text s))
    (insert s)
    (undo-boundary)
    (clipmon--play-sound)
    (setq clipmon--autoinsert-timeout-start (current-time)))) ; reset timeout


(defun clipmon--autoinsert-transform-text (s)
  "Apply autoinsert transformations to clipboard text S."
  (if clipmon-transform-trim (setq s (clipmon--trim-left s)))
  (if clipmon-transform-remove
      (setq s (replace-regexp-in-string clipmon-transform-remove "" s)))
  (if clipmon-transform-prefix (setq s (concat clipmon-transform-prefix s)))
  (if clipmon-transform-suffix (setq s (concat s clipmon-transform-suffix)))
  (if clipmon-transform-function (setq s (funcall clipmon-transform-function s)))
  s)


(defun clipmon--play-sound ()
  "Play a user-specified sound file, the included sound file, or nothing."
  (if clipmon-autoinsert-sound
      (if (stringp clipmon-autoinsert-sound)
          (play-sound-file clipmon-autoinsert-sound)
          (play-sound-file clipmon--included-sound-file))))



;;;; Library functions
;; ----------------------------------------------------------------------------

(defun clipmon-kill-ring-total ()
  "Get total size of kill ring, in characters."
  (let ((sum 0))
    (mapc (lambda (s) (setq sum (+ sum (length s)))) kill-ring) sum))


(defun clipmon--clipboard-contents ()
  "Get current contents of system clipboard, as opposed to Emacs's kill ring.
Returns a string, or nil."
  ; when the OS is first started x-get-selection-value will throw (error "No
  ; selection is available")
  (ignore-errors (x-get-selection-value)))


(defun clipmon--trim-left (s)
  "Remove leading spaces from string S."
  (replace-regexp-in-string  "^[ \t]+"  ""  s))


(defun clipmon--seconds-since (time)
  "Return number of seconds elapsed since the given time, including milliseconds.
TIME should be in Emacs time format (see `current-time').
Valid for up to 2**16 seconds = 65536 secs ~ 18hrs."
  (let* ((elapsed (time-subtract (current-time) time))
         (seconds (nth 1 elapsed))
         (microseconds (nth 2 elapsed)) ; accurate to milliseconds on my system, anyway
         (total (+ seconds (/ microseconds 1.0e6))))
    total))


;;;; Footer
;; ----------------------------------------------------------------------------

(provide 'clipmon)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; clipmon.el ends here
