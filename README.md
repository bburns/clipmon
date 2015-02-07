
clipmon.el [![Travis build status](https://secure.travis-ci.org/bburns/clipmon.png?branch=master)](http://travis-ci.org/bburns/clipmon) [![melpa.org](http://melpa.org/packages/clipmon-badge.svg)](http://melpa.org/#/clipmon)
----------------------------------------------------------------------------


Description
----------------------------------------------------------------------------

Clipmon is a clipboard monitor - it watches the system clipboard and can
automatically insert any new text into the current location in Emacs.

It also adds changes to the system clipboard to the kill ring, making Emacs
into a cyclic clipboard manager for text - you can then use a package like
browse-kill-ring or helm-ring to view and manage your clipboard history.

You can use it for taking notes from a webpage, for example - just copy the
text you want to save and it will be added to Emacs. It helps to have an
autocopy feature or addon for the browser, e.g. AutoCopy 2 for Firefox - then
you can just select text to add it to Emacs.

Here's a diagram - text flows from the top to the bottom:

                 +---------------------+
                 |   Other programs    |+
                 +---------------------+|
                  +---------------------+
                          /
                    +-----------+
                    |  System   |
                    | clipboard |
                    +-----------+
    OS                /
    ---------------------------------------------------
    Emacs           /
                   /
          +--------------+      +---------------+
          | clipmon-mode |......|  autoinsert   |
          +--------------+      +---------------+
                  |                     .
            +-----------+               .
            | Emacs     ++              .
            | kill ring ++       +--------------+
            +-----------+|+      |  transforms  |
             +-----------+|      +--------------+
              +-----------+             .
                     |                  .
                     | yank             . autoinsert
                +--------------------------+
                |      Emacs buffer        |
                +--------------------------+


The solid line is turned on and off with `clipmon-mode`, while the dotted
line is turned on and off with `clipmon-autoinsert-toggle`, usually bound to a key.
There are also various transformations you can perform on the text, e.g.
adding newlines to the end.

Emacs's kill-ring is like the system clipboard but with multiple items in it.
If you copy a bunch of things in another program, Emacs normally only knows
about the last one copied, but with clipmon mode on, it will monitor the
system clipboard and add any new text it sees to the kill ring.


Installation
----------------------------------------------------------------------------

It's simplest to use the package manager:

    M-: (package-install 'clipmon)

It will then be ready to use, and will also be available the next time you
start Emacs.


Usage
----------------------------------------------------------------------------

Add this to your .emacs file:

    ;; monitor the system clipboard and add any changes to the kill ring
    (clipmon-mode 1)

    ;; hit this when you want to insert changes at the current location
    (global-set-key (kbd "<M-f2>") 'clipmon-autoinsert-toggle)

Try it out - turn on autoinsert with your keybinding (or select the item in
the Options menu), go to another application and copy some text to the
clipboard - clipmon should detect it after a second or two and make a beep.
If you switch back to Emacs, the text should be there in your buffer. You can
turn off autoinsert with the same keybinding.

If no change is detected after a certain number of minutes, autoinsert will
turn itself off automatically with a beep. This was to prevent the author
from forgetting that autoinsert is on and accidentally adding text to his
buffer.

Note that you can still yank and pull text in Emacs as usual while autoinsert
is on, since it only monitors the system clipboard.

Also, if you happen to copy the same text to the clipboard twice,
clipmon won't know about the second time, as it is only able to detect
changes. And if you copy text faster than the timer interval is set it may
miss some changes.


Using as a clipboard manager
----------------------------------------------------------------------------

To try it out, make sure clipmon-mode is on (also accessible from the Options
menu) and autoinsert is off, then copy a few pieces of text from another
program (slower than the default timer interval of 2 seconds though). Switch
back to Emacs, and note that you can yank any of the text back with C-y, M-y,
M-y...

You can use the package browse-kill-ring to manage the kill ring - call
`browse-kill-ring` to see the contents of the kill ring, insert from it,
delete items, etc. Helm also has a package called helm-ring, with the
function `helm-show-kill-ring`.

You can also persist the kill ring between sessions if you'd like (though
note that this might involve writing sensitive information like passwords to
the disk - you could always delete such text from the kill ring though with
`browse-kill-ring-delete`).

To persist the kill ring, add this to your .emacs file:

    (clipmon-persist)
    (setq savehist-autosave-interval (* 5 60)) ; save every 5 minutes (default)

This will use Emacs's savehist library to save the kill ring, both at the end
of the session and at set intervals. Note though that savehist also saves
various other settings by default, including the minibuffer history - see
`savehist-mode` for more details.


The kill ring has a fixed number of entries which you can set, depending on
how you use the kill-ring, or how much history you want to save:

    (setq kill-ring-max 500) ; default is 60 in Emacs 24.4

To see how much space the kill-ring is taking up, call this function:

    (clipmon-kill-ring-total)
    => 29670 characters


Options
----------------------------------------------------------------------------

There are various options you can set with customize:

    (customize-group 'clipmon)

or set them in your .emacs file - these are the default values:

    (setq clipmon-timer-interval 2)       ; check system clipboard every n secs
    (setq clipmon-autoinsert-sound t)     ; t for included beep, or path or nil
    (setq clipmon-autoinsert-color "red") ; color of cursor when autoinsert is on
    (setq clipmon-autoinsert-timeout 5)   ; stop autoinsert after n mins inactivity

before inserting the text, transformations are performed on it in this order:

    (setq clipmon-transform-trim t)        ; remove leading whitespace
    (setq clipmon-transform-remove         ; remove text matching this regexp
          "\\[[0-9][0-9]?[0-9]?\\]\\|\\[citation needed\\]\\|\\[by whom?\\]")
    (setq clipmon-transform-prefix "")     ; add to start of text
    (setq clipmon-transform-suffix "\n\n") ; add to end of text
    (setq clipmon-transform-function nil)  ; additional transform function


Todo
----------------------------------------------------------------------------

- Prefix with C-u to set a target point, then allow cut/copy/pasting from
  within Emacs, eg to take notes from another buffer, or move text elsewhere.


History
----------------------------------------------------------------------------

20150206 refactored to handle kill ring better
  clipmon-mode now just adds changes to the kill-ring.
  clipmon-autoinsert-toggle added to toggle automatic inserting of text.
  changed several setting names - all with aliases to old names.
  clipmon-action removed - no longer need to call kill-new or insert with it.
20150131 added clipmon-action, to accommodate adding to kill-ring
20150120 initial release


Sound File
----------------------------------------------------------------------------

The sound file was created with Audacity [http://audacity.sourceforge.net/].
It's a bit on the quiet side so hopefully it doesn't get annoying when you're
taking a lot of notes...


Feedback
----------------------------------------------------------------------------

Feedback is always welcome - for feature requests or bug reports, see the
Github issues page [https://github.com/bburns/clipmon/issues]. Pull requests
are welcome also.

Thanks to tuhdo for suggesting using clipmon as a clipboard manager, and
Steve Purcell for initial feedback.


----

Author: Brian Burns  
URL: https://github.com/bburns/clipmon  
Version: 20150206  

This file was generated from commentary in clipmon.el - do not edit!

----

