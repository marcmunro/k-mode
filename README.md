# Emacs K Mode

K-mode attempts to make emacs less surprising and more usable.  It aims
to make advanced functionality easily accessible and make the whole user
experience more pleasant.

It has been written primarily for Marc's way of looking at the world and
so may not work for you.  If you'd like to fork it for a wider audience,
please do so.  If no the other hand you'd like to assist in or help
direct its development, please get in touch: <marc@bloodnok.com>

License
=======
[GPL V3](https://www.google.com)

Current Status
==============

It is very much still in development.  It doesn't even have a version
number.  It will advance as and when Marc feels like it, or when other
developers join in and push things along.

What is Lacking?
================

<a name="lacking">
- version numbering;
- testing and bug fixing (please report bugs to me.  If I can reproduce
  them, I'll fix them);
- customisations (ie defcustom stuff);
- updates of next-error to match those in k-compile;
- handling of grep and friends;
- projects based on other SCMs (ie other than git);
- kill-ring tweaks (see k-cua below);
- name shortening for long names in k-tabbar;
- buffer-placement improvements (see k-frame below);
- a redo feature;
- the whole K key handling thing which is barely mentioned in k.el;
- emacsclient stuff;
- hippie-expand;
- ispell;
- other cool things (outline/org mode, etc);
- a vision (beyond making emacs usage more productive and less
  surprising);

What Works (and is maybe good)?
===============================

k-cua
-----

This takes the standard, friendly-ish useful and powerful cua-mode and
extends it a little.

- All deletions now go to the kill-ring, even if done by backspace or by
  highlighting and replacing text.

  There should probably be a lower limit on kills composed entirely of
  backspaces, and we should probably remove trailing whitespace from
  kills, but that's for the future (see what is lacking above).

- The insert key is mapped to yank, and shift-insert to yank-pop.  Use
  ctrl-insert to toggle overwrite mode.

- Kill-line is made a bit smarter and faster (though this has nothing to
  do with cua).

k-word
------

Word movement is different in vi and emacs.  Sometimes vi makes more
sense.  K-word attempts to find the best of all worlds.  Read the
comments in k-word.

k-frame
-------

Buffer and Window/Frame Association

There are a number of aspects to this:
1) The poisition of each buffer on each window, along with any active
   region, is recorded so that if that buffer is revisited after
   visiting some other buffer, point, mark and the screen position will
   be retained.  Standard emacs behaviour is unhelpful if you view the
   same buffer in multiple windows: switching to another buffer and back
   again, will bring the buffer back with point at the same place it is
   in one of the other windows.

2) Active regions are made sane.  In standard emacs, if you set the mark
   in one window and switch to another showing the same buffer, the new
   window will now have as its active region the region between mark in
   the first window and point in the new one.  This is unhelpful and
   very surprising.  K-mode fixes this.  It just works the way it
   should. 

3) Primary selection is made sane.  If you cut and paste using just the
   mouse (as $DEITY intended), in standard emacs the primary selection
   gets very confused when you switch between windows showing the same
   buffer.  Try it and tell me it doesn't suck.  K-mode fixes this.  You
   are welcome.

4) Frames that have only displayed the currently displayed buffer are
   automatically deleted when that buffer is killed.  This is simply to
   make it easier to keep mental track of what buffers are in play.
   
5) Buffers that are unmodified and have only been displayed on
   the current frame are killed when the frame is deleted.  There is a
   variable you can use to disable this if it seems too draconian but
   try it - it's safer and more natural than you might think.

Control of Buffer Placement

Marc really dislikes emacs' tendency to split windows for help,
compilation, debug, etc buffers.  The buffer-placement mechanism
eliminates much window-splitting.

Rather than split a window for a, possibly new, buffer, buffer-placement
allows the user to choose where the buffer is placed.  It tries placing
the buffer in a suitable window and if that is not suitable the user can
move it to the next most suitable one.  The re-place-buffer operation is
on F5 and only active after an initial buffer-placement.  Initial
buffer-placement happens anywhere (more or less) that a buffer would
normally have been split.  A "placed" buffer is identified by its
frame's background colour quickly flashing blue as it is selected.
Successive re-place operations (F5) will cause whatever frame the
buffer was placed on to be returned to its previous state, and the next
option to be tried, again with a blue flash.  You can repeatedly cycle
through these options for as long as you like.

The choices of window that are made, in order or preference, are:

- any windows currently showing the buffer
  (yay, compilation will no longer open a new window on every frame
  where you run it);
- a new frame;
- all other active frames (starting with the next one reading from left
  to right, top to bottom, and then cycling back to the top-leftmost
  frame);
- the current frame, split as would have happened originally;
- the current frame, unsplit.

This makes more sense once seen.  Try it and if you don't like it, let
me know why.

Frame/Window Navigation

F5 and shift-F5 are defined as navigation keys that switch between
windows, switching frames and moving the mouse pointer as needed. 

k-project
---------

This allows buffers to be identified as belonging to a spcific project.
This is an enabling technology for k-compile and k-tabbar.

k-tabbar
--------

This is a bit like the standard tabbar but way better:

- each window has its own independent tabbar
- buffers belong to multiple buffer-groups
- only buffers for the selected buffer-group are shown in a tabbar;
- the buffer-list highlights the selected buffer;
- buffers which have not appeared in the frame are greyed-out;
- the ordering of the buffer list in each tabbar is such that the most
  recently selected appear first (modulo avoiding re-ordering for
  aethetic reasons);

A buffer-list is simply a collection of buffers that meet a particular
criterion:

- a project buffer list contains buffers associated with a project;
- a dired bufefr list contains buffers for files in the directory of a
  dired buffer;
- a mode buffer list contains buffers with a specific major mode;
- a frame buffer list contains only those buffers that have appeared on
  the frame in question;
- the *emacs* buffer list, for special emacs buffers like *Messages*,
- etc.

To see all available buffer-groups, you click on the leftmost
(buffer-group entry) in the buffer-list display.  To switch-back to the
buffer-list display, you click on the buffer-group you are interested
in.

It makes more sense when you try it.

k-compile
---------

Combines with k-project to associate compilations and compilation
buffers with projects.  This allows multiple compilation buffers to be
open simultaneously and even for multiple compilations to be
simultaneously executing (one for each project).  It also places the
compilation buffers into the project buffer-group to which they apply.

k-search
--------

Integrates all of emacs search-modes, including the incremental ones.
Its pretty cool but needs some documentation.
