
Clipmon
=======

Clipmon will automatically paste the contents of the clipboard when a change is detected - 
making it easier to take notes from web pages or other sources, etc.

It can play a sound when text is pasted, and will turn off after a set period of inactivity.


Usage: 

Call `(clipmon-toggle)` to start the timer - it will check the clipboard every `clipmon-interval` seconds.

If the clipboard has changed, it will paste the contents at the current location and add `clipmon-newlines` newlines. 

Call `(clipmon-toggle)` turn it off, or if no change is detected after `clipmon-timeout` seconds, it will turn off the timer automatically.

You can also start and stop the timer with `(clipmon-start)` and `(clipmon-start)`.



Example key binding:

(global-set-key (kbd "<M-f2>") 'clipmon-toggle)

(global-set-key (kbd "<M-f1>") 'clipmon-toggle)




Defaults:

```
(defcustom clipmon-interval 2
  "Interval for checking clipboard, in seconds.")

(defcustom clipmon-timeout 5
  "Stop the timer if no clipboard activity after this many minutes. Set to nil for no timeout.")

(defcustom clipmon-newlines 2
  "Number of newlines to append after pasting clipboard contents.")

; (defcustom clipmon-sound t
; (defcustom clipmon-sound "ting.wav"
; hmm. on init, this gets called from init.el, and so fil-dir is set to its directory. 
(defcustom clipmon-sound (concat (file-directory-loading) "ting.wav")
  "Sound to play when pasting text - t for default beep, nil for none, or path to sound file.")

(defcustom clipmon-trim-string t
  "Remove leading whitespace from string before pasting.")

(defcustom clipmon-key "<M-f2>"
  "Key to toggle clipmon on and off.")
```


Home: https://github.com/bburns/clipmon  
License: MIT. NO WARRANTY.  



