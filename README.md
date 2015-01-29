
clipmon.el [![Travis build status](https://secure.travis-ci.org/bburns/clipmon.png?branch=master)](http://travis-ci.org/bburns/clipmon) [![melpa.org](http://melpa.org/packages/clipmon-badge.svg)](http://melpa.org/#/clipmon)
----------------------------------------------------------------------------


Description
----------------------------------------------------------------------------

Clipmon is a clipboard monitor - it watches the system clipboard and inserts
any changes into the current location in Emacs. It can also add them to the
kill-ring, which can then be browsed with a package like browse-kill-ring.

You can use it for taking notes from a webpage, for example - just copy the
text you want to save and it will be pasted into Emacs. Typically you turn it
on when you need to copy a lot of text from elsewhere, then turn it off when
you're done.

It also helps to have an autocopy feature or addon for the browser, e.g.
AutoCopy 2 for Firefox
[https://addons.mozilla.org/en-US/firefox/addon/autocopy-2/] - then you can
just select text to copy it to the clipboard.


Installation
----------------------------------------------------------------------------

It's simplest to use the package manager:

    M-x package-install <return> clipmon <return>

It will then be ready to use, and will also be loaded automatically the next
time you start Emacs.


Usage
----------------------------------------------------------------------------

Add something like this to your .emacs file to turn clipmon on and off easily:

    (global-set-key (kbd "<M-f2>") 'clipmon-mode)

Then try it out - turn it on, go to another application and copy some text to
the clipboard - clipmon should detect it after a second or two and make a
sound. If you switch back to Emacs, there should be some new text in your
buffer.

You can still yank and pull text in Emacs as usual while clipmon is on, since
it only looks at the system clipboard.

If no change is detected after `clipmon-timeout` minutes, clipmon will turn
itself off automatically.


Options
----------------------------------------------------------------------------

There are various options you can set with customize:

    (customize-group 'clipmon)

or set them in your .emacs file - these are the default values:

    (setq clipmon-cursor-color "red")  ; color for cursor when clipmon is on
    (setq clipmon-sound t)             ; t for included beep, or path or nil
    (setq clipmon-interval 2)          ; time interval to check clipboard (secs)
    (setq clipmon-timeout 5)           ; stop if no activity after n minutes
    (setq clipmon-action 'insert)      ; action to take with clipboard text

transforms on the clipboard text are performed in this order:

    (setq clipmon-trim-string t)          ; remove leading whitespace
    (setq clipmon-remove-regexp           ; remove text matching this regexp
          "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]")
    (setq clipmon-prefix "")              ; add to start of text
    (setq clipmon-suffix "\n\n")          ; add to end of text
    (setq clipmon-transform-function nil) ; additional transform function


Hooks

As with all minor modes, hooks for calling the mode function and turning the
mode on and off are available (the on and off hooks are not bound by
default though):

    clipmon-mode-hook
    clipmon-on-hook
    clipmon-off-hook


Sound File
----------------------------------------------------------------------------

The sound file was created with Audacity [http://audacity.sourceforge.net/].
It's a bit on the quiet side so hopefully it doesn't get annoying when you're
taking a lot of notes...


Todo
----------------------------------------------------------------------------

- Prefix with C-u to set a target point, then allow pasting from within Emacs.
- Put sound, messages on start, detect, stop hooks instead of hardcoded.


Feedback
----------------------------------------------------------------------------

Feedback is always welcome - for feature requests or bug reports, see the
Github issues page [https://github.com/bburns/clipmon/issues], or feel free
to open a pull request.



----

Author: Brian Burns  
URL: https://github.com/bburns/clipmon  
Version: 20150120  

This file was generated from commentary in clipmon.el.

----

