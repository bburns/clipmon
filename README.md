
Clipmon
=======

Clipmon is a clipboard monitor - it detects changes to the clipboard and
pastes the contents into the current location.

This makes it easier to take notes from a webpage, for example, by just
copying text you wish to save. This works well when paired with an autocopy
feature or plugin for the browser, e.g. AutoCopy 2 for Firefox.

You can continue to use the Emacs kill-ring with yank and pull as usual,
since clipmon only looks at the system clipboard, as used by other
applications.


Usage
-----
Make a keybinding like the following to turn clipmon on and off: 

    (global-set-key (kbd "<M-f2>") 'clipmon-toggle)

Turn it on, then go to another application, e.g. a browser, and copy some
text to the clipboard. Clipmon should detect it after a second or two, and
make a sound - if you switch back to Emacs, it should have pasted the text
into your buffer. 


Options
-------

Once started, Clipmon checks the clipboard for changes every
`clipmon-interval` seconds. If no change is detected after `clipmon-timeout`
minutes, the monitor will turn itself off automatically.

The cursor color can be set with `clipmon-cursor-color` - eg "red", or nil
for no change. 

A sound is played on each change, and on starting and stopping clipmon. The
sound can be set with `clipmon-sound` - this can be a filename (.wav or .au),
t for the default Emacs beep/flash, or nil for no sound.

When selecting text to copy, it's sometimes difficult to avoid grabbing a
leading space - to remove these from the text before pasting, set
`clipmon-trim-string` to t (on by default).

To filter the text more set `clipmon-remove-regexp` - it will remove any
matching text before pasting. By default it is set up to to remove
Wikipedia-style references, e.g. "[3]".

You can also have newlines appended to the text - specify the number to add
with `clipmon-newlines`. The default is 2, giving a blank line between each
clip.


See all options here: `(customize-group 'clipmon)`


Todo
----
- bug - try to start with empty kill ring - gives error on calling
  current-kill
- test with -Q
- package.el
- preserve echo message - often gets wiped out
- bug - lost timer
  when put laptop to sleep with it on, on resuming,
  it seemed to lose track of the timer, and couldn't turn it off without
  calling (cancel-function-timers 'clipmon--tick)


License
-------
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




-------

Home: https://github.com/bburns/clipmon  

