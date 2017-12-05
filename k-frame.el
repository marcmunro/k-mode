;;; k-frame.el --- Frame/window to buffer association stuff for k-mode
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; Frame movement/management stuff
;; This has 3 main components:
;; 1) the stuff that allows us to switch frames (and windows)
;; 2) the stuff that modifies emacs default handling of frames and windows
;;    in order to improve the user experience.  Specifically:
;;    - recall start, point and mark for each buffer on a per window basis
;; 3) the stuff that associates frames and buffers so that deleting
;;    a frame kills all buffers that have not been displayed on any other
;;    frame, and killing a buffer that is the only buffer that has been
;;    displayed on a frame results in the frame being deleted.
;;
;; TODO:
;; - Allow delete-frame to fail if kill-buffer fails
;; - allow for region to be highlighted in multiple windows of same frame??
;;   (maybe this should be another k module??

(require 'k-core)

;;;
;;; BASIC FRAME/WINDOW STATE STUFF
;;; (for returning start and point to previous states when switching
;;; between buffers)

;;; We record our state in the following parameters:
;;; k-frame::buffer-dalist
;;; - (frame parameter) a dalist, keyed by buffer and symbol of various
;;;   window settings:
;;;   point  The value of point from when the buffer was last being visited
;;;   start  The window start value from ditto
;;;   eob    Whether point was at the end of the buffer from ditto
;;;   mark   The mark position at the time focus was last lost
;;;   mark-active Whether the mark was active when focus was last lost
;;;

(window-parameter (selected-window) 'k-frame::buffer-dalist)
(frame-parameter (selected-frame) 'buffer-list)

(defun k-frame::window-state-properties nil
  "Record of a window's main state (buffer, start and point), after
  the last command was run.")

(defun k-frame::record-state (window)
  "Record the current window and its start, point and buffer."
  (setplist 'k-frame::window-state-properties
	    `(start ,(window-start window)
		    point ,(window-point window)
		    buffer ,(window-buffer window)
		    window ,window)))

(defun k-frame::window-state-dalist (window)
  "Return the dalist of window-state-properties for WINDOW"
  (or (window-parameter window 'k-frame::buffer-dalist)
      (list nil)))

(defun k-frame::get-marker (dalist buffer symbol)
  "Get a marker from DALIST for the entry keyed by BUFFER and SYMBOL."
  (let ((entry (k::dalist-assq dalist buffer symbol)))
    (or (cdr entry)
	(setcdr entry (make-marker)))))

(defun k-frame::save-window-positions (window)
  "Save the currently recorded window state from
  `k-frame::window-state-properties' into the window-parameter
  `k-frame::buffer-dalist'."
  (let* ((buffer (get 'k-frame::window-state-properties 'buffer))
	 (dalist (k-frame::window-state-dalist window))
	 (start (k-frame::get-marker dalist buffer 'start))
	 (point (k-frame::get-marker dalist buffer 'point))
	 (eob-entry (k::dalist-assq dalist buffer 'eob)))
    (and (buffer-live-p buffer)
	 (with-current-buffer buffer
	   ;;(message "SAVING WINDOW STATE FOR %s ON %s (%s)"
	       ;;buffer window
	       ;;(get 'k-frame::window-state-properties 'point))
	   (set-marker point (get 'k-frame::window-state-properties 'point))
	   (set-marker start (get 'k-frame::window-state-properties 'start))
	   (setcdr eob-entry
		   (eq (get 'k-frame::window-state-properties 'point)
		       (point-max)))
	   (set-window-parameter window 'k-frame::buffer-dalist dalist)))))

(defun k-frame::save-window-state (window)
  "Does `k-frame::record-state' and `k-frame::save-window-positions' for
  WINDOW."
  (k-frame::record-state window)
  (k-frame::save-window-positions window))
  
(defun k-frame::restore-window-positions (window)
  "Restore the currently recorded window state from
  `k-frame::window-state-properties' into the window-parameter
  `k-frame::buffer-dalist'."
  (let* ((buffer (window-buffer window))
	 (dalist (k-frame::window-state-dalist window))
	 (eob-entry (k::dalist-assq dalist buffer 'eob)))
    (and buffer
	 (with-current-buffer buffer 
	   ;;(message "RESTORING WINDOW STATE FOR %s ON %s (%s)"
	   ;;buffer window (k-frame::get-marker dalist buffer 'point))
	   (if (cdr eob-entry)
	       (goto-char (point-max))
	     (let ((start (k-frame::get-marker dalist buffer 'start))
		   (point (k-frame::get-marker dalist buffer 'point)))
	       (when (marker-position start)
		 (set-window-start window start)
		 (goto-char point))))))))

(defun k-frame::last-noted-window ()
  "Return the last window that we were known to be visiting."
  (get 'k-frame::window-state-properties 'window))

(defun k-frame::last-noted-buffer ()
  "Return the last window that we were known to be visiting."
  (get 'k-frame::window-state-properties 'buffer))

(defun k-frame::window-changed-p (window)
  "Return t if WINDOW is not the same as last noted."
  (not (eq window (k-frame::last-noted-window))))

(defun k-frame::window-buffer-changed-p (window)
  "Return t if the buffer associated with WINDOW is different since last
  noted, and we are looking at the same window."
  (if (k-frame::window-changed-p window)
      nil
    (not (eq (window-buffer window)
	     (get 'k-frame::window-state-properties 'buffer)))))

(defun k-frame::handle-window-buffer-change (window)
  "Save the state for the buffer previously associated with WINDOW, and
  reset any previous state for the newly selected buffer."
  (k-frame::save-window-positions window)
  (get 'k-frame::window-state-properties 'window)
  (k-frame::restore-window-positions window))
    
;;;
;;; BUFFER STUFF
;;;


(defvar k-frame::significant-buffers nil
  "List of buffers that we have visited and are therefore deemed
  significant.")

(defun k-frame::record-significant-buffer (buffer)
  "Record that BUFFER has been visited and is thus significant."
  (unless (memq buffer k-frame::significant-buffers)
    (setq k-frame::significant-buffers
	  (cons buffer k-frame::significant-buffers)))
  (let* ((frame (selected-frame))
	 (blist (frame-parameter frame 'buffer-list)))
    (unless (memq buffer blist)
      ;; Ensure that the buffer is deemed to have been visited by the
      ;; frame.  WHY IS THIS NOT ALREADY THE CASE?
      (set-frame-parameter frame 'buffer-list
			   (cons buffer blist)))))

(defvar k-frame::last-significant-killed-buffer nil
  "Records the last significant buffer that was killed.  This is used so
  that code that tests for significant buffers as part of the
  kill-buffer process continue to get a useful result.")

(defun k-frame::significant-buffer-p (buf)
  "Return t if BUF is considered significant.  This is all buffers
  except those whose names begin with space and have no associated
  file.  If an otherwise insignificant buffer becomes displayed in a
  window, it becomes significant and is recorded in
  `k-frame::buffer-details'."
  (or (buffer-file-name buf)
      (memq buf k-frame::significant-buffers)
      (eq buf k-frame::last-significant-killed-buffer)
      (not (string-match-p "^ \\*" (buffer-name buf)))))

(defun k-frame::drop-buffer (buffer)
  "Drop all records of BUFFER from k-frame's state."
  (setq k-frame::significant-buffers
	(delq buffer k-frame::significant-buffers)
	k-frame::last-significant-killed-buffer buffer)
  (walk-windows
   (lambda (window)
     (set-window-parameter
      window 'k-frame::buffer-dalist
      (assq-delete-all
       buffer (window-parameter window 'k-frame::buffer-dalist))))
   nil t))


;;;
;;; FRAME SWITCHING STUFF
;;;

(defvar k-frame::wmctrl-exists 'undefined
  "Boolean identifying whether we have wmctrl available.")

(defun k-frame::check-wmctrl ()
  "Identify whether wmctrl is available and if not and this is the first
  time we have checked, give the user some feedback."
  (let ((result
	 (catch 'k-shell-error
	   (let ((shell-stdout
		  (k::shell-command "wmctrl -v")))
	     shell-stdout))))
    (setq
     k-frame::wmctrl-exists
     (not
      (when (consp result)
	;; No wmctrl found.
	(if (eq k-frame::wmctrl-exists 'undefined)
	    ;; This is our first time of checking.  Give the user some
	    ;; useful feedback.
	    (message "Cannot execute wmctrl (%s)\n%s"
		     (cdr result)
		     "k-frame functionality will be diminished."))
	t)))))

(defun k-frame::deiconify-frame (frame)
  "De-iconify the emacs frame FRAME, using wmctrl." 
  (if k-frame::wmctrl-exists
      (k::shell-command
       (concat "wmctrl -i -R "
	       (frame-parameter frame 'outer-window-id)))
    (message
     "Emacs is unable to de-iconify frames.  Please install wmctrl")))

(defun k-frame::switch-to-desktop (desktop)
  "Switch to DESKTOP using wmctrl.  Do nothing if desktop is nil." 
  (and desktop
       (if k-frame::wmctrl-exists
	   (k::shell-command (concat "wmctrl -s "
				     (prin1-to-string desktop)))
	 (message
	  "Emacs is unable to switch desktops.  Please install wmctrl"))))

(defun k-frame::window-workspaces ()
  "Return an alist of window-ids (from the window manager) to their
  workspaces, using wmctrl if it exists." 
  (if k-frame::wmctrl-exists
      (let* ((k-frame::suspend-buffer-list-handling t)
	     (winstr (k::shell-command "wmctrl -l")))
	(mapcar
	 (lambda (line)
	   (let* ((parts (split-string line " +"))
		  (window (string-to-number
			   (substring (car parts) 2 nil) 16)))
	     (cons window (string-to-number (cadr parts)))))
	 (split-string winstr "\n")))))

(setq k-frame::wmctrl-exists nil)
(defvar k-frame::last-deiconified-frame nil
  "The last frame that was de-iconified.")

(defun k-frame::frame-coords (frame &optional window-workspaces)
  "Return the coordinates of frame FRAME as a list in the form:
  display-id workspace-id x-coord y-coord visible.  If wmctrl is not
  available workspace-ids will be nil."
  (list (frame-parameter frame 'display)	
	(string-to-number (frame-parameter frame 'outer-window-id))
	(cdr (assoc (string-to-number (frame-parameter frame 'outer-window-id))
		    (or window-workspaces
			(k-frame::window-workspaces))))
	(frame-parameter frame 'left)	
	(frame-parameter frame 'top)
	(and (not (eq frame k-frame::last-deiconified-frame))
	     (frame-parameter frame 'visibility))))

(defun k-frame::frames-for-current-display ()
  "Return a list of all frames that are on by the same display as that
  of the selected-frame."
  (let ((display (frame-parameter (selected-frame) 'display)))
    (delete
     nil
     (mapcar
      (lambda (frame)
	(and (string= display (frame-parameter frame 'display)) frame))
      (frame-list)))))

(defun k-frame::all-frame-coords (&optional window-workspaces)
  "Return an alist of all frame coordinates."
    (mapcar (lambda (frame)
	      (cons frame (k-frame::frame-coords frame window-workspaces)))
	    (k-frame::frames-for-current-display)))

(defun k-frame::visible-p (visibility)
  "Predicate identifying whether the provided VISIBILITY frame parameter
  represents a visible frame or not."
  (and visibility (not (eq visibility 'icon))))

(defun k-frame::viscmp (v1 v2)
  "Like C's strcmp for visibility frame parameters."
  (if (k-frame::visible-p v1)
      (if (k-frame::visible-p v2)
	  0
	-1)
    (if (k-frame::visible-p v2)
	1
      0)))

(defun k-frame::cmp (list1 list2 elem)
  "Compare the elements from LIST1 and LIST2 at the ELEMth positions
  using strcmp semantics.  The elements may be strings integers or
  visibility frame parameters."
  ;; TODO: eliminate use of eval below as it is very bad practice
  (let* ((icon 'icon)  ;; Allows eval on 'icon to return 'icon
	 (e1 (eval (nth elem list1)))	;; We use eval as some parameters 
	                                ;; are integer expressions
	 (e2 (eval (nth elem list2)))
	 (res (cond
	       ((stringp e1) (k::strcmp e1 e2))
	       ((integerp e1) (- e1 (or e2 0))) ;; (Interpret nil as zero)
	       ((integerp e2) (- 0 e2))
	       ((not (or e1 e2)) 0)
	       (t (k-frame::viscmp e1 e2)))))
    res))

(defun k-frame::frame-sort-fn (f1 f2)
  "Sort frames by workspace, and then left to right, top to bottom, with
invisible frames shown last in each workspace.

F1 and F2 are alists of frame coords, keyed by frame."
  (let ((fields '(3 6 4 5))
	(res))
    (while fields
      (setq res (k-frame::cmp f1 f2 (car fields)))
      (setq fields (if (= res 0) (cdr fields) nil)))
    (< res 0)))

(defun k-frame::sort-frame-alist (frames)
  "Takes FRAMES, an alist of frame-coords, and sorts it."
  (sort frames 'k-frame::frame-sort-fn))

(defun k-frame::frame-alist-match (frame alist)
  ;; TODO: doc string
  (if alist
      (if (eq frame (caar alist))
	  alist
	(k-frame::frame-alist-match frame (cdr alist)))))

(defun k-frame::find-next-frame-coords (frame all-frames same-desktop)
  "Identify the next frame after FRAME from ALL-FRAMES.  Direction is
  left to right, top to bottom.  If SAME-DESKTOP, limit the frame
  selection to frames on the current desktop." 
  (let (frame-desktop
	found)
    (or 
     (catch 'found
       (mapc
	(lambda (frame-coords)
	  (if found ;; ie if we are after FRAME in the ALL-FRAMES list
	      (if same-desktop
		  (if (equal (nth 3 frame-coords) frame-desktop)
		      (throw 'found frame-coords))
		(throw 'found frame-coords))
	    (if (eq frame (car frame-coords))
		(setq frame-desktop (nth 3 frame-coords)
		      found t))))
	all-frames)
       nil) ;; ensure our `catch' form returns nil if 'found was not thrown
     (catch 'found
       ;; We only reach this point if we were unable to find a suitable 
       ;; frame after FRAME in the mapc above.  Here we look again for
       ;; frames preceding (or including) FRAME.
       (mapc
	(lambda (frame-coords)
	  (if same-desktop
	      (if (equal (nth 3 frame-coords) frame-desktop)
		  (throw 'found frame-coords))
	    (throw 'found frame-coords)))
	all-frames)))))

(defun k-frame::next-frame-coords (frame &optional same-desktop)
  "Identify the next frame after FRAME.  Direction is left to right, top
  to bottom.  If optional SAME-DESKTOP, limit the frame selection to
  frames on the current desktop."
  (k-frame::find-next-frame-coords
   frame (k-frame::sort-frame-alist (k-frame::all-frame-coords))
   same-desktop))

(defun k-frame::prev-frame-coords (frame &optional same-desktop)
  "Identify the previous frame before FRAME.  Direction is left to
  right, top  to bottom.  If optional SAME-DESKTOP, limit the frame
  selection to frames on the current desktop."
  (k-frame::find-next-frame-coords
   frame (reverse (k-frame::sort-frame-alist (k-frame::all-frame-coords)))
   same-desktop))
       
(defun k-frame::switch-to-frame (frame-coords)
  "Switch input focus to the frame identified by FRAME-COORDS."
  ;; TODO: Maybe move this down to below k-frame::record-window-selections??
  ;; ## CHECK THE FOLLOWING COMMENTED OUT CALL
  ;;(k-frame::record-window-selections (selected-window))
  (let ((pos (cdr (mouse-position)))
	(frame (car frame-coords))
	(cur-frame (selected-frame)))
    (when (eq cur-frame k-frame::last-deiconified-frame)
      (iconify-frame)
      (setq  k-frame::last-deiconified-frame nil))
    (when (not (eq (nth 6 frame-coords) t))
      ;; We are switching to an iconified, or non-visible frame.
      ;; Start be ensuring we are on the right workspace.
      (k-frame::switch-to-desktop (nth 3 frame-coords)))
    (select-frame-set-input-focus frame)
    (if (eq (cdr (assq 'visibility (frame-parameters))) t)
	(setq k-frame::last-deiconified-frame nil)
      (k-frame::deiconify-frame frame)
      (setq k-frame::last-deiconified-frame frame))
    (set-mouse-position frame (or (car pos) 1) (or (cdr pos) 1))))

(defun k-frame::next-frame (arg)
  "Switch to the next frame for the current display.  With ARG
  switch to a frame on the same desktop."
  (interactive "P")
  (k-frame::switch-to-frame
   (k-frame::next-frame-coords (selected-frame) arg)))

(defun k-frame::prev-frame (arg)
  "Switch to the previous frame for the current display.  With ARG 
  switch to a frame on the same desktop."
  (interactive "P")
  (k-frame::switch-to-frame
   (k-frame::prev-frame-coords (selected-frame) arg)))

(defun k-frame::windows-from-tree (tree)
  "Return a simple list of windows within a window tree, including the
  minibuffer only if it is active."
  (mapcar
   (lambda (entry)
     (if (listp entry)
	 (apply #'append (k-frame::windows-from-tree (cddr entry)))
       (if (window-minibuffer-p entry)
	   (if (minibuffer-window-active-p entry)
	       (list entry))
	 (list entry))))
   tree))
			    
(defun k-frame::frame-windows (&optional frame)
  "Return a list of active windows for the selected frame in a
  reasonably sensible order."
  (apply #'append nil (k-frame::windows-from-tree (window-tree frame))))

(defun k-frame::get-next-window (window &optional same-desktop)
  "Identify the window that follows WINDOW for the current display.
  Windows may be in different frames.   With SAME-DESKTOP choose only
  frames on the same desktop." 
  (let* ((frame (window-frame window))
	 (windows (k-frame::frame-windows frame))
	 found)
    (or
     (catch 'found
       (mapc
	(lambda (this-window)
	  (if found
	      (throw 'found this-window)
	    (setq found (eq window this-window))))
	windows)
       nil)
     ;; No more windows on this frame.  Get the first window on the next
     ;; frame.
     (let ((coords (k-frame::next-frame-coords frame same-desktop)))
       (car (k-frame::frame-windows (car coords)))))))

(defun k-frame::all-windows (window &optional same-desktop)
  "Return a list of all windows in display order starting at the window
  following WINDOW.  If SAME-DESKTOP is non-nil, limit to frames on the
  current desktop."
  (let* ((this (k-frame::get-next-window window same-desktop))
	 (windows (cons this nil)))
    (while
	(progn
	  (setq this (k-frame::get-next-window this same-desktop))
	  (setq windows (cons this windows))
	  (not (eq this window))))
    (nreverse windows)))

(defun k-frame::next-window (&optional arg)
  "Switch to the next window for the current display.  This may switch
  frames.   With ARG switch only to frames on the same desktop."
  (interactive "P")
  (let ((windows (k-frame::frame-windows))
	(this (selected-window))
	found)
    (select-window
     (or
      (catch 'found
	(mapc
	 (lambda (window)
	   (if found
	       (throw 'found window)
	     (setq found (eq window this))))
	 windows)
	nil) ;; Failed to find a next window, so result is nil
      (progn
	;; Get here if there was no next-window to which to switch.
	(k-frame::next-frame arg)
	;; Return first window of selected-frame
	(car (k-frame::frame-windows)))))))

(defun k-frame::next-window-or-placement (&optional arg)
  "If we have a temporarily placed buffer (see place-buffer), then
  re-place it, else do the normal next-window operation."
  (interactive "P")
  (let ((buffer (k-frame::temporarily-placed-buffer)))
    (if buffer
	(k-frame::place-buffer buffer)
      (k-frame::next-window arg))))

(defun k-frame::prev-window (&optional arg)
  "Switch to the previous window for the current display.  This may switch
  frames.   With ARG switch only to frames on the same desktop."
  (interactive "P")
  (let ((windows (k-frame::frame-windows))
	(this (selected-window))
	found)
    (select-window
     (or
      (catch 'found
	(mapc
	 (lambda (window)
	   (if (eq window this)
	       (throw 'found found)
	     (setq found window)))
	 windows)
	nil) ;; Failed to find a next window, so result is nil
      (progn
	;; Get here if there was no next-window to which to switch.
	(k-frame::prev-frame arg)
	;; Return last window of selected-frame
	(car (last (k-frame::frame-windows))))))))

;;;
;;; BUFFER PLACEMENT STUFF
;;; Replaces normal split-window functionality with a smart and
;;; selectable buffer placement mechanism.
;;;

(defvar k-frame::allow-split-window nil
  "Used to indicate whether split-buffer is allowed to proceed
  uninterupted, rather than leading to a selectable buffer placement.
  Set this to t in a local let binding to allow normal split-window
  operation.")

(defvar k-frame::window-to-split nil
  "Details about an intercepted attempt to split a window.  This is a
  list of (WINDOW WINDOW-CONFIGURATION SPLIT-WINDOW-ARGS).")

(defvar k-frame::place-buffer-args nil
  "Saved set of args to `k-frame::place-buffer.'")

(defvar k-frame::temporarily-placed-buffer nil
  "The last buffer that was temporarily placed by
  `k-frame::place-buffer'.  This buffer is available to be replaced iff 
  `last-command' is `k-frame::place-buffer'.")

(defun k-frame::temporarily-placed-buffer ()
  "If there is a temporarily placed buffer that can be placed elsewhere,
  return the buffer, else nil."
  (and (eq last-command 'k-frame::place-buffer)
       k-frame::temporarily-placed-buffer))

(defvar k-frame::restoration-data nil
  "Data for restoring our display to its state prior to the last call to
  `k-frame::place-buffer-in-window'.  This is a list in the form:
    (UNDO-FUNCTION ARGS)")

(defun k-frame::window-recalls-buffer-p (window buffer)
  "T if WINDOW has previously visited and recorded info on BUFFER."
  (and (memq buffer (frame-parameter
		     (window-frame window) 'buffer-list))))

(defun k-frame::capture-window-configuration (window buffer)
  "Capture data about WINDOW so that it can later be perfectly restored."
  (message "CAPTURE %s %s" window buffer)
  (setq k-frame::restoration-data
	(list
	 'k-frame::restore-window-configuration
	 window
	 (current-window-configuration (window-frame window))
	 (unless (k-frame::window-recalls-buffer-p window buffer)
	   buffer)))
  (with-selected-window window
    (walk-windows 'k-frame::save-window-state)))

(defun k-frame::forget-buffer-visit (window buffer)
  "Update WINDOW and all associated variables to make it appear that it
  has not visited BUFFER."
  (let ((frame (window-frame window)))
    (set-frame-parameter
     frame 'buffer-list
     (delq buffer (frame-parameter frame 'buffer-list)))
    (set-window-parameter
     window 'k-frame::buffer-dalist
     (assq-delete-all
      buffer (window-parameter window 'k-frame::buffer-dalist)))))

(defun k-frame::restore-window-configuration (window config forget-buffer)
  "Undo a `k-frame::place-buffer' operation by restoring a saved window
  configuration.  If FORGET-BUFFER, then make the window forget that
  forget-buffer has been visited."
  (when forget-buffer
    (k-frame::forget-buffer-visit window forget-buffer))
  (with-selected-window window
    (set-window-configuration config)
    (walk-windows 'k-frame::restore-window-positions)))

(defvar k-frame::frame-flash-color "PowderBlue"
  "Colour to use when wishing to get a frame noticed.")

(defun k-frame::flash-frame ()
  "Make the current frame stand out by flashing the background color."
  (unwind-protect
      (let ((bcolor (frame-parameter (selected-frame)
				     'background-color))
	    res)
	(set-background-color k-frame::frame-flash-color)
	(redisplay)
	(sleep-for 0.4)  ;; sit-for does not always work in this case!
	(set-background-color bcolor))))

(defun k-frame::place-buffer-in-window (buffer window)
  "Place BUFFER in WINDOW, setting state so that the placement can be
  undone by a call to `k-frame::undo-last-placement'."
  (k-frame::capture-window-configuration window buffer)
  (with-selected-window window
    (switch-to-buffer buffer)
    (k-frame::flash-frame)))

(defun k-frame::place-buffer-in-split-window (buffer window &rest args)
  "Place BUFFER in the window resulting from splitting WINDOW, setting
  state so that the placement can be undone by a call to
  `k-frame::undo-last-placement'."
  (k-frame::capture-window-configuration window buffer)
  (let ((k-frame::allow-split-window t))
    (with-selected-window window
      (with-selected-window (apply 'split-window args)
	(switch-to-buffer buffer)
	(k-frame::flash-frame)))))

(defun k-frame::drop-frame (frame)
  "Drop FRAME, which was used for a temporary buffer placement."
  (let ((k-frame::auto-kill-buffers-conditionally nil))
    (delete-frame frame)))

(defun k-frame::place-buffer-in-new-frame (buffer)
  "Place BUFFER in its own frame so that the placement can be undone by
  a call to `k-frame::undo-last-placement'."
  (with-current-buffer buffer
    (let* ((frame (new-frame))
	   (cur-window (selected-window))
	   (window (car (window-list frame))))
      (setq k-frame::restoration-data
	    (list
	     'k-frame::drop-frame frame))
      (with-selected-window window
	(k-frame::flash-frame)))))
      
(selected-window)
(defun k-frame::undo-last-placement ()
  "Undo the last action of `k-frame::place-buffer'. "
  (apply (car k-frame::restoration-data)
	 (cdr k-frame::restoration-data)))

(defun k-frame::place-buffer (buffer &rest args)
  "Attempt to find a place to display BUFFER.  If ARGS are provided we
  are dealing with a new buffer.  Repeated calls to this function will
  exchange the last attempt for a new one.   ARGS provide a number of
  options which may be tried.  Each entry in ARGS may be either a window
  or a list containing a label (this may be used in order to
  re-prioritise the list prior to ths function being called), a function
  and the function's arguments.  Each arg from args will be tried on 
  successive calls.  If arg is a function and returns nil, the next arg
  will be tried.  When all args have been exhausted, we start again from
  the beginning." 
  (setq this-command 'k-frame::place-buffer)
  (if args
      (setq k-frame::temporarily-placed-buffer buffer
	    k-frame::place-buffer-args args
	    k-frame::restoration-data nil)
    (setq k-frame::place-buffer-args
	  (append (cdr k-frame::place-buffer-args)
		  (cons (car k-frame::place-buffer-args) nil)))
    (k-frame::undo-last-placement))
  (let ((head (car k-frame::place-buffer-args)))
    (cond
     ((windowp head)
      (k-frame::place-buffer-in-window buffer head))
     (t
      (apply (cadr head) (car (cddr head))))))
  (car k-frame::place-buffer-args))

(defun k-frame::prioritize-placements (placements buffer)
  "Take an ordered list of PLACEMENTS, and prioritise them as follows:
     any entries for windows showing BUFFER;
     any entry for a new frame;
     remaining window entries, except for the last;
     any split-window entry;
     the last window entry."
  (let* ((buffer-windows 
	  (get-buffer-window-list buffer nil t))
	 (best
	  (delq nil
		(mapcar
		 (lambda (elem)
		   (and (memq elem buffer-windows) elem))
		 placements)))
	 (new-frame-entries 
	  (delq nil
		(mapcar
		 (lambda (entry)
		   (and (consp entry) (eq (car entry) 'new-frame) entry))
		 placements)))
	 (split-window-entries
	  (delq nil
		(mapcar
		 (lambda (entry)
		   (and (consp entry) (eq (car entry) 'split-window) entry))
		 placements)))
	 (remaining placements)
	 last)
    ;; Remove our matched elements from remaining
    (mapc
     (lambda (elem)
       (setq remaining (delq elem remaining)))
     (append best new-frame-entries split-window-entries))
    (setq last (last remaining))
    (append best new-frame-entries
	    (delq (car last) remaining) split-window-entries last)))

(defun k-frame::make-split-window-entry (buffer window args)
  "Create an entry for a split-window command for placement in a list of
options for buffer-placement."
  (list 'split-window
	'k-frame::place-buffer-in-split-window
	(append (list buffer window) args)))

(defun k-frame::make-frame-entry (buffer)
  "Create an entry for a split-window command for placement in a list of
options for buffer-placement."
  (list 'new-frame
	'k-frame::place-buffer-in-new-frame
	(list buffer)))

(defvar k-frame::result-of-split nil
  "Record of the window returned by splitting off a small window in
  `k-frame::split-window' below.  The small window is used to minimise
  the amount of display redrawing that is needed when subverting the
  normal split-window mechanism.")

(defun k-frame::handle-window-split ()
  "Called from post-command hook function to complete the action started
  by a call to split-window.  We defer this stage so that whatever
  command caused the window to be split has the opportunity to switch to
  a new buffer in that window.  What this function does is attempt to
  find a more suitable location to display the buffer."
  (let* ((k-frame::allow-split-window t)
	 (orig-window (car k-frame::window-to-split))
	 (buffer (window-buffer k-frame::result-of-split))
	 window-list split-window-entry new-frame-entry)
    ;; Restore the window configuration from before our split-window
    ;; call.
    (set-window-configuration (nth 1 k-frame::window-to-split))
    ;; Create an entry, for our list of buffer placement options, that
    ;; performs the split-window as originally requested.
    (setq split-window-entry
	  (k-frame::make-split-window-entry
	   buffer orig-window (nth 2 k-frame::window-to-split)))
    ;; Create an entry, for our list of buffer placement options, that
    ;; places buffer on a new-frame
    (setq new-frame-entry (k-frame::make-frame-entry buffer))
    ;; Create the full list of buffer placement options for
    ;; k-frame::place-buffer.
    (setq
     k-frame::window-to-split nil
     window-list (k-frame::prioritize-placements
		  (append (k-frame::all-windows (selected-window) t)
			  (list split-window-entry
				new-frame-entry))
		  buffer))
    ;; Place buffer at the first entry from our list of possible
    ;; placements. 
    (apply 'k-frame::place-buffer buffer window-list)))

(defun k-frame::split-window (orig-fun &rest args)
  "Advice function to override normal window splitting mechanism so that
  whatever buffer appears in the new window can be placed more sanely in:
  an existing frame if possible; a new frame if desirable; or in a newly
  split window if necessary."
  (if (or k-frame::allow-split-window
	  (minibuffer-window-active-p (selected-window)))
      (apply orig-fun args)
    (let ((window (selected-window))
	  split-window-keep-point)
      (setq k-frame::window-to-split
	    (list window
		  (current-window-configuration (window-frame window))
		  args))
      (k-frame::save-window-positions window)
      (let ((k-frame::allow-split-window t))
	;; Create a very small window as it will have minimal resdisplay
	;; requirements.  This will initially be used instead of the
	;; normal-sized window that split-window would normally return.
	;; The contents of this window will be examined through our
	;; pos-command hook function, which will provide an alternative,
	;; initial, placement for that buffer, and allow the user to
	;; choose other, possibly more suitable, alternative placements
	;; through `k-frame::next-window-or-placment'.
	(setq k-frame::result-of-split (split-window window -2))))))

(defun k-frame::split-window-command (orig-fun &rest args)
  "Advice function for split-window commands to cancel our override of
  normal split-window functionality for explicitly called commands."
  (let ((k-frame::allow-split-window t))
    (apply orig-fun args)))

;; The following advice is safe when k-mode is inactive so let's just
;; make it unconditional.
(advice-add 'split-window-vertically :around #'k-frame::split-window-command)
(advice-add 'split-window-horizontally :around #'k-frame::split-window-command)


;;;
;;; MARK AND SELECTION HANDLING
;;;

(defun k-frame::record-window-mark (window)
  "Save the mark for WINDOW.  This allows different marks to be active
  for the same buffer in different windows, which should make switching
  windows with active marks much less confusing.  Note that we store a
  separate boolean `mark-active' flag so that we can retain our saved
  marker (rather than having to recycle and garbage-collect it)  This is
  an optimisation to minimise the number of active marks for a buffer."
  (let ((buffer (window-buffer window)))
    (if buffer
	(with-current-buffer buffer
	  (let* ((dalist (k-frame::window-state-dalist window))
		 (mark (k-frame::get-marker dalist buffer 'mark))
		 (mark-active-entry
		  (k::dalist-assq dalist buffer 'mark-active)))
	(if mark-active
	    (progn
	      (setcdr mark-active-entry t)
	      (set-marker mark (mark)))
	  (setcdr mark-active-entry nil)))))))

(defun k-frame::restore-window-mark (window)
  "TODO: doc string.
  Note that result is t if WINDOW has a buffer."
  (let ((buffer (window-buffer window)))
    (if buffer
	(with-current-buffer buffer
	  (let* ((dalist (k-frame::window-state-dalist window))
		 (mark (k-frame::get-marker dalist buffer 'mark))
		 (mark-is-active
		  (k::dalist-get dalist buffer 'mark-active)))
	    (if mark-is-active
		(set-mark mark)
	      (and mark-active (deactivate-mark)))
	    t)))))

(defvar k-frame::last-primary-selection nil
  "A record of the last primary selection from the last focused window.
  This is used to ensure that the primary selection is not modified by
  emacs in mysterious ways when changing frames.")

(defun k-frame::leave-window (window)
  "Deal with leaving window WINDOW."
  (setq k-frame::last-primary-selection (gui-get-selection))
  (k-frame::record-window-mark window)
  (let ((last-buffer (k-frame::last-noted-buffer)))
    (and last-buffer (buffer-live-p last-buffer)
	 (with-current-buffer last-buffer
	   (and mark-active
		(deactivate-mark))))))
  
(defun k-frame::arrive-at-window (window)
  "Deal with arriving at window WINDOW."
  (when (k-frame::restore-window-mark window)
    (gui-set-selection 'PRIMARY k-frame::last-primary-selection)
    (setq k-frame::last-noted-buffer (window-buffer window))))

;;;
;;; HOOKS INTO EMACS
;;;

(defun k-frame::pre-command ()
  "Pre-command hook function for recording basic window state."
  (let ((window (selected-window)))
    (unless (window-minibuffer-p window)
      (k-frame::record-state window))))

(defvar k-frame::buffer-list-updated nil
  "Records whether the bufer-list has been changed since last we
checked.")

(defun k-frame::update-buffer-list ()
  "Record the fact that the buffer-list has been updated."
  (setq k-frame::buffer-list-updated t))
  
(defvar k-frame::window-to-delete nil
  "Set this to cause a window to be deleted in the post-command hook.")

(defun k-frame::post-command ()
  "Post-command hook function for recording basic window state."
  (let ((window (selected-window)))
    (unless (window-minibuffer-p window)
      (when k-frame::window-to-split
	(k-frame::handle-window-split))
      (when k-frame::window-to-delete
	(delete-window k-frame::window-to-delete)
	(setq k-frame::window-to-delete nil))
      (when (k-frame::window-changed-p window)
	(and (k-frame::last-noted-window)
	     (k-frame::leave-window (k-frame::last-noted-window)))
	(k-frame::arrive-at-window window))
      (when (and k-frame::buffer-list-updated
		 (k-frame::window-buffer-changed-p window))
	(k-frame::handle-window-buffer-change window))
      (when k-frame::buffer-list-updated
	;; Ensure all currently visited buffers are recorded as significant
	(walk-windows
	 (lambda (window)
	   (k-frame::record-significant-buffer (window-buffer window))))
	(setq k-frame::buffer-list-updated nil)))))

(defun k-frame::has-other-significant-buffer-p (frame buffer)
  "T if FRAME has a significant buffer other than BUFFER."
  (catch 'found
    (mapc
     (lambda (buf)
       (and (not (eq buf buffer))
	    (k-frame::significant-buffer-p buf)
	    (throw 'found t)))
     (frame-parameter frame 'buffer-list))
    nil))

(defvar k-frame::auto-delete-frames t
  "Allow k-frame to automatically delete frames when they no longer
  contain significant buffers.")

(defvar k-frame::auto-close-windows t
  "Allow k-frame to automatically close windows when killing a buffer.")

(defun k-frame::kill-buffer ()
  "Hook function called when a buffer is to be killed, responsible for
  clearing state associated with the buffer and redisplaying any affected
  tabbars."
  (let ((k-frame::auto-kill-buffers-conditionally nil)
	(window (selected-window))
	(buffer (current-buffer)))
    (with-current-buffer buffer
      ;; We use the with-current-buffer form to ensure that we don't
      ;; change current-buffer as we go through this.
      (and (k-frame::significant-buffer-p buffer)
	   (progn
	     (k-frame::drop-buffer buffer)
	     (message "BUFFER %s DROPPED IN K-FRAME" buffer)))
      (when k-frame::auto-delete-frames 
	(mapc
	 (lambda (frame)
	   (unless (k-frame::has-other-significant-buffer-p frame buffer)
	     (delete-frame frame)))
       (frame-list)))
      (when (and k-frame::auto-close-windows
		 (eq buffer (window-buffer window))
		 (> (length (k-frame::frame-windows)) 1))
	;; Cannot delete-window directly here as that will change the
	;; selected-window and thereby the current-buffer, leading
	;; to mayhem and sadness.
	(setq k-frame::window-to-delete window)))))

(defun k-frame::buffer-appears-on-other-frame-p (buffer frame)
  "T if BUFFER appears in the buffer-list of any frame other than
  FRAME."
  (catch 'found
    (mapc
     (lambda (frame)
       (when (memq buffer (frame-parameter frame 'buffer-list))
	 (throw 'found t)))
     (delq frame (frame-list)))
    nil))

(defvar k-frame::auto-kill-buffers-conditionally t
  "Allow k-frame to automatically kill buffers that are associated with
  no remaining frames.  For modified buffers visiting files, the chance
  to save the buffer will be offered.  If the offer is declined, the
  buffer will not be killed.")

(defun k-frame::significant-buffers (frame)
  "Return the list of significant buffers associated with FRAME."
  (delq nil
	(mapcar
	 (lambda (buffer)
	   (and (k-frame::significant-buffer-p buffer) buffer))
	 (frame-parameter frame 'buffer-list))))

(defun k-frame::check-buffer-killable (buffer)
  "Return t if BUFFER may be killed."
  ;; TODO: HANDLE KILLING OF PROCESS BUFFERS?
  (cond
   ((not (buffer-modified-p buffer))  t)
   ((k::is-compilation-buffer-p buffer) t)
   ((k::emacs-buffer-p buffer)  nil)
   (t (if (or buffer-offer-save
	      (buffer-file-name buffer))
	  (if (yes-or-no-p
	       (format "Save modified buffer (%s)? "
		       (buffer-name (current-buffer))))
	      (progn
		(save-buffer)
		t))))))

(defun k-frame::killable-buffers (buffers)
  "Return the set of buffers from BUFFERS that are killable."
  (delq nil
	(mapcar
	 (lambda (buffer)
	   (with-current-buffer buffer
	     (and (k-frame::check-buffer-killable buffer) buffer)))
	 buffers)))

(defun k-frame::delete-frame (frame)
  "Possibly kill unmodified, non-special buffers that are associated
only with FRAME." 
  (if k-frame::auto-kill-buffers-conditionally
      (let ((k-frame::auto-delete-frames nil)
	    (buffers
	     (delq nil
		   (mapcar
		    (lambda (buffer)
		      (unless (k-frame::buffer-appears-on-other-frame-p
			       buffer frame)
			buffer))
		    (k-frame::significant-buffers frame)))))
	;; buffers is the list of signicant buffers for frame that are
	;; associated with no other frames.
	(mapc
	 (lambda (buffer)
	   (kill-buffer buffer))
	 (k-frame::killable-buffers buffers)))))

(defun k-frame::handle-switch-frame (orig-fun &rest args)
  "Advice function for `handle-switch-frame'.  This checks whether we are
  currently handling a temporary buffer placement, and if so, updates
  this-command so that we can continue to do so.  We need this when we
  quickly switch from one buffer placement to another as switch-frame
  events can be raised which cause last-command to be updated."
  (if (eq last-command 'k-frame::place-buffer)
      (setq this-command 'k-frame::place-buffer)))

;;;
;;; ACTIVATION
;;;

(defun k-frame::reset-state ()
  "Reset k-frame's stored state."
  (setq k-frame::significant-buffers nil)
  (walk-windows
   (lambda (window)
     (set-window-parameter window 'k-frame::buffer-dalist nil)
     (k-frame::record-state window))
   nil t)
  (setq k-frame::last-noted-buffer (current-buffer)))

(defun k-frame::activate ()
  "Activate the k-frame stuff."
  (k-frame::reset-state)
  (k-frame::check-wmctrl)
  (advice-add 'split-window :around #'k-frame::split-window)
  (advice-add 'handle-switch-frame :before #'k-frame::handle-switch-frame)
  (add-hook 'buffer-list-update-hook #'k-frame::update-buffer-list)
  (add-hook 'kill-buffer-hook #'k-frame::kill-buffer)
  (add-hook 'delete-frame-functions #'k-frame::delete-frame)
  (add-hook 'pre-command-hook #'k-frame::pre-command)
  (add-hook 'post-command-hook #'k-frame::post-command))

(defun k-frame::deactivate ()
  "Deactivate the k-frame stuff."
  (advice-remove 'split-window #'k-frame::split-window)
  (advice-remove 'handle-switch-frame #'k-frame::handle-switch-frame)
  (remove-hook 'buffer-list-update-hook #'k-frame::update-buffer-list)
  (remove-hook 'kill-buffer-hook #'k-frame::kill-buffer)
  (remove-hook 'delete-frame-functions #'k-frame::delete-frame)
  (remove-hook 'pre-command-hook #'k-frame::pre-command)
  (remove-hook 'post-command-hook #'k-frame::post-command))

(add-hook 'k-mode-activation-hook #'k-frame::activate)
(add-hook 'k-mode-deactivation-hook #'k-frame::deactivate)


;;;
;;; KEY BINDINGS
;;; 

(define-key k-mode-map [f5] 'k-frame::next-window-or-placement)
(define-key k-mode-map [S-f5] 'k-frame::prev-window)

(provide 'k-frame)

(when nil
  (setq frame (selected-frame))
  (setq buffers
	(delq nil
	      (mapcar
	       (lambda (buffer)
		 (unless (k-frame::buffer-appears-on-other-frame-p
			  buffer frame)
		   buffer))
	       (k-frame::significant-buffers frame))))

  (k-frame::killable-buffers buffers)
  (setq comp (car buffers))
  (k::emacs-buffer-p comp)
  (k::emacs-buffer-p (get-buffer "*Messages*"))
  (k-frame::check-buffer-killable comp)
  (message "WIBBLE")
  (message "WUBBLE")
  )
