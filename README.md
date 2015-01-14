
## clipmon.el [![Build Status](https://secure.travis-ci.org/bburns/clipmon.png?branch=master)](http://travis-ci.org/bburns/clipmon)
----


Description
----------------------------------------------------------------------------

Clipmon is a clipboard monitor - it watches the system clipboard and pastes
any changes into the current location in Emacs.

You can use it for taking notes from a webpage, for example - just copy the
text you want to save and it will be pasted into Emacs. Typically you just
turn it on when you need to copy a lot of text from elsewhere, then turn it
off when you're done.


Installation
----------------------------------------------------------------------------

It's simplest to use the package manager:

    M-x package-install <return> clipmon <return>

It will then be ready to use, and will also be loaded automatically the next
time you start Emacs.


Usage
----------------------------------------------------------------------------

Add something like this to your .emacs file to turn clipmon on and off. You
can also evaluate it now by hitting C-x C-e after the expression to set the
key:

    (global-set-key (kbd "<M-f2>") 'clipmon-toggle)

To try it out, turn it on, then go to another application and copy some text
to the clipboard - clipmon should detect it after a second or two and make a
sound. If you switch back to Emacs, it should have pasted the text into your
buffer.

You can still yank and pull text in Emacs as usual while clipmon is on, since
it only looks at the system clipboard.

It's also helpful to have an autocopy feature or addon for the browser, e.g.
AutoCopy 2 for Firefox [1] - then you can just select text to copy it to the
clipboard.

If no change is detected after `clipmon-timeout` minutes, clipmon will turn
itself off automatically, with a beep.


[1] https://addons.mozilla.org/en-US/firefox/addon/autocopy-2/


Options
----------------------------------------------------------------------------

There are various options you can set with customize (hit C-x C-e after this):
    (customize-group 'clipmon)

or set them in your .emacs file - these are the default values:
    (setq clipmon-cursor-color "red")  ; color for cursor when clipmon is on
    (setq clipmon-sound t)             ; t for included beep, or path or nil
    (setq clipmon-interval 2)          ; time interval to check clipboard (secs)
    (setq clipmon-timeout 5)           ; stop if no activity after n minutes

Transforms on the clipboard text are performed in this order:
    (setq clipmon-trim-string t)          ; remove leading whitespace
    (setq clipmon-remove-regexp           ; remove text matching this regexp
          "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]")
    (setq clipmon-prefix "")              ; add to start of text
    (setq clipmon-suffix "\n\n")          ; add to end of text
    (setq clipmon-transform-function nil) ; additional transform function

For the most flexibility, set `clipmon-transform-function` to a function that
takes the clipboard text and returns a modified version - e.g. to make the
text lowercase before pasting,

   (setq clipmon-transform-function (lambda (s) (downcase s)))


Sound File
----------------------------------------------------------------------------

The sound file was created with Audacity [http://audacity.sourceforge.net/].
It's not too loud so hopefully it doesn't get annoying when you're taking a
lot of notes...


License
----------------------------------------------------------------------------

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.



----

Author: Brian Burns <bburns.km@gmail.com>  
URL: https://github.com/bburns/clipmon  
Version: 20150114  

This file was generated from commentary in clipmon.el.

----

