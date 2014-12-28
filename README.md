
clipmon.el
==========


Description
----------------------------------------------------------------------------

Clipmon is a clipboard monitor - it watches the system clipboard and pastes
any changes into the current location in Emacs.

This makes it easier to take notes from a webpage, for example - just copy
the text you want to save and it will be pasted into Emacs. You can still use
the Emacs kill-ring with yank and pull as usual, since clipmon only looks at
the system clipboard.

This works best when paired with an autocopy feature or plugin for the
browser, e.g. AutoCopy 2 for Firefox [1] - then you can just select text to
copy it to the clipboard.

[1] https://addons.mozilla.org/en-US/firefox/addon/autocopy-2/


Usage
----------------------------------------------------------------------------

Make a key-binding like the following to turn clipmon on and off:

    (global-set-key (kbd "<M-f2>") 'clipmon-toggle)

Then turn it on and go to another application, like a browser, and copy some
text to the clipboard - clipmon should detect it after a second or two, and
make a sound. If you switch back to Emacs, it should have pasted the text
into your buffer.


Options
----------------------------------------------------------------------------

Once started, clipmon checks the clipboard for changes every
`clipmon-interval` seconds (default 2). If no change is detected after
`clipmon-timeout` minutes (default 5), clipmon will turn itself off
automatically.

The cursor color can be set with `clipmon-cursor-color` - eg "red", or nil
for no change.

A sound can be played on each change, and on starting and stopping clipmon.
The sound can be set with `clipmon-sound` - this can be t for the included
ding.wav file, a path to a sound file (.wav or .au), or nil for no sound.

When selecting text to copy, it's sometimes difficult to avoid grabbing a
leading space - to remove these from the text, set `clipmon-trim-string` to t
(on by default).

To filter the text some more set `clipmon-remove-regexp` - it will remove any
matching text before pasting. By default it is set to remove Wikipedia-style
references, e.g. "[3]".

You can specify strings to add to the start and end of the text, with
`clipmon-prefix` and `clipmon-suffix`. By default the suffix is set to two
newlines, which will leave a blank line in between entries.

For more customization, set `clipmon-transform-function` to a function that
takes the clipboard text and returns a modified version - e.g. to make the
text lowercase before pasting,
   (setq clipmon-transform-function (lambda (s) (downcase s)))

See all options here: (customize-group 'clipmon)


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
Homepage: https://github.com/bburns/clipmon  
Version: 0.1.20141227  

----
