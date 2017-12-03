;;; k-tabbar.el --- tabbar stuff for k-mode (dead cool)
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; TODO:
;;   Descriptive header
;;
;; - handle long names 
;; - handle frame resizing
;; - Implement Ctrl/Alt/Shift versions of click events
;; - Update and complete mouseover help text

;; BUGS:

(require 'k-core)
(require 'k-frame)  ;; For identfication of significant buffers

;;;
;;; CACHE HANDLING
;;;

;; The following values are cached, in window-parameters, to reduce the
;; amount of calculation performed each time a tabbar is displayed:
;;
;; - k-tabbar::tabbar-line
;;   TODO: describe this (see k7-tabbar.el)
;; - k-tabbar::tabbar-elems
;;   TODO: describe this
;; - k-tabbar::tabbar-scroll
;;   TODO: describe this
;; - k-tabbar::buffer-list
;;   TODO: describe this
;; - k-tabbar::path
;;   Record the full file-name path for a buffer.
;;

(defun k-tabbar::window-cache-get (window symbol &rest args)
  "Get value, for WINDOW, of SYMBOL from cache.  If not found calculate
  the value using the cache-function of the same name as symbol and
  cache its value."
  (or (window-parameter window symbol)
      (set-window-parameter window symbol
                            (apply symbol window args))))

(defun k-tabbar::cache-get (symbol &rest args)
  "Get value of SYMBOL from cache for current window.
  If not found calculate the value using the cache-function of the same
  name as symbol and cache its value."
  (apply 'k-tabbar::window-cache-get (selected-window) symbol args))
(defun k-tabbar::clear-cache (symbol &optional window)
  "Clear cached value of SYMBOL.  If WINDOW is provided, do it only for
  window, otherwise clear it for all windows."
  (if window
      (set-window-parameter window symbol nil)
    (walk-windows
     (lambda (window)
       (k-tabbar::clear-cache symbol window))
     nil t)))

(defun k-tabbar::clear-window-cache (window &rest symbols)
  "Clear WINDOWs cached values for SYMBOLS."
  (mapcar
   (lambda (symbol) (k-tabbar::clear-cache symbol window))
   symbols))

(defun k-tabbar::reset-tabbar-cache (window &optional clear-buffer-list)
  "Clear the fundamental tabbar cache values for WINDOW, causing a
  full recalculation of that window's tabbar.  With optional
  CLEAR-BUFFER-LIST also clear the buffer-list cache which will cause
  re-reordering of the tabbar."
  (k-tabbar::clear-window-cache
   window
   'k-tabbar::tabbar-line 'k-tabbar::tabbar-elems
   'k-tabbar::tabbar-scroll)
  (when clear-buffer-list
    (k-tabbar::clear-window-cache window 'k-tabbar::buffer-list)))

(defun k-tabbar::clear-buffer-group-display-caches ()
  "Clear the caches for windows displaying buffer-groups."
  ;; TODO: Check if this is used
  (walk-windows
   (lambda (window)
     (when (window-parameter window 'k-tabbar::show-buffer-group
                             )
       ;; Force selection of another buffer-group
       (set-window-parameter window 'k-tabbar::buffer-group nil)
       (k-tabbar::reset-tabbar-cache window)))
   nil t))

(defun k-tabbar::clear-all-windows-cache (&rest symbols)
  "Clear all windows cached values for SYMBOLS."
  (walk-windows
     (lambda (window)
       (apply 'k-tabbar::clear-window-cache window symbols))
     nil t))

(defun k-tabbar::all-cache-symbols ()
  "Return the list of symbols representing all cached window values."
  '(k-tabbar::tabbar-line
    k-tabbar::tabbar-elems
    k-tabbar::show-buffer-group
    k-tabbar::buffer-group
    k-tabbar::tabbar-scroll
    k-tabbar::show-buffer-group
    k-tabbar::buffer-list))

;;;
;;; TABBAR-STATE MANIPULATION
;;;

;; k-tabbar maintains the following state:
;; - k-tabbar::buffer-groups
;;   An alist of buffer-group keys to a list of buffers belonging to that
;;   buffer-group
;;
;; Also, the following window-parameters are used, in addition to the
;; cache values described elsewhere:
;; - k-tabbar::show-buffer-group
;;   Boolean describing whether the tabbar is showing a list of
;;   buffer-groups rather than a buffer-list.
;; - k-tabbar::buffer-group
;;   The currently selected buffer-group key for the window.
;; - k-tabbar::window-buffer-groups
;;   An alist of buffer-groups associated with the window, with the most
;;   recently used buffers appearing first in the cdr of each alist entry.
;; - k-tabbar::buffer
;;   Currently (last known) selected buffer for the window.  Used to
;;   determine when windows show different buffers.
;;
;;

(defvar k-tabbar::buffer-groups nil
  "An alist of buffer-groups mapped to the list of buffers they contain.")

(defvar k-tabbar::last-killed-buffer nil
  "Records the last buffer known to have been handled by
  `k-tabbar::kill-buffer'.  This is used to prevent
  `k-tabbar::update-buffer-list' from adding the buffer back into
  our state between the time we remove it and the time it is actually
  killed.")

(defun k-tabbar::actual-buffer-group-key (pkey)
  "Return the actual key for a buffer group that looks like the
  potential key PKEY.  This allows `eq' to be used for comparing keys
  rather than the slower `equal'."
  (or (car (assoc pkey k-tabbar::buffer-groups))
      pkey))

(defun k-tabbar::frame-buffer-group-key (frame)
  "Return the key for the frame buffer-group for FRAME."
  (k-tabbar::actual-buffer-group-key (cons 'frame frame)))

(defun k-tabbar::mode-buffer-group-key (buf)
  "Return the key for the mode buffer-group for BUF."
  (k-tabbar::actual-buffer-group-key
   (cons 'mode (with-current-buffer buf major-mode))))

(defun k-tabbar::project-buffer-group-key (path)
  "Return the key for the mode buffer-group for PATH."
  ;; TODO: Handle shortened and dynamic project names
  ;;       Integrate with k-project.el
  (k-tabbar::actual-buffer-group-key (cons 'project path)))

(defun k-tabbar::dired-master-buffer-p (buf)
  "T if BUF is a dired-mode buffer."
  (and buf (eq 'dired-mode (with-current-buffer buf major-mode))))

(defun k-tabbar::dired-directory (bgkey)
  "Return the dired directory for the buffer group given by bgkey.
  Return nothing if the master dired buffer has been killed (and not yet
  removed by housekeeping)."
  (and (buffer-live-p (cdr bgkey))
       (with-current-buffer (cdr bgkey) list-buffers-directory)))

(defun k-tabbar::is-dired-buffer-group-p (bgkey)
  "Predicate to determine whether BGKEY is for a dired
buffer-group."
  (eq (car bgkey) 'dired))

(defun k-tabbar::dired-bg-for-dir (dir)
  "Return the dired buffer-group for directory DIR."
  (let ((bg k-tabbar::buffer-groups)
        this
        result)
    (while bg
      (setq this (caar bg))
      (if (and (k-tabbar::is-dired-buffer-group-p this)
               (string= (k-tabbar::dired-directory this) dir))
          (setq result this
                bg nil)
        (setq bg (cdr bg))))
    result))

(defun k-tabbar::dired-buffer-group-key (buf)
  (if (k-tabbar::dired-master-buffer-p buf)
      (k-tabbar::actual-buffer-group-key
       (cons 'dired buf))
    (let ((fname (buffer-file-name buf)))
      (and fname (k-tabbar::dired-bg-for-dir
                  (file-name-directory fname))))))

(defun k-tabbar::buffer-in-bgroup-p (buf bgkey)
  "Predicate identifying whether BUF is a member of the buffer-group
  given by BGKEY."
  (let ((bg (assq bgkey k-tabbar::buffer-groups)))
    (and bg (memq buf (cdr bg)) t)))

(defun k-tabbar::drop-buffer-from-group (buffer bgkey)
  "Low level function to drop BUFFER from the buffer-group given by
  BGKEY, modifying caches appropriately."
  (when (k-tabbar::buffer-in-bgroup-p buffer bgkey)
    (let ((bgroup (delq buffer (assq bgkey k-tabbar::buffer-groups)))
          dropped)
      (if (or (and (k-tabbar::is-dired-buffer-group-p bgkey)
                   (k-tabbar::dired-master-buffer-p buffer))
              (not (cdr bgroup)))
          ;; If we are dropping the master buffer of a dired
          ;; buffer-group, or the last buffer of a buffer-group, we
          ;; drop the buffer-group.
          (setq k-tabbar::buffer-groups
              (assq-delete-all bgkey k-tabbar::buffer-groups)
              dropped t))
      (walk-windows
       (lambda (window)
         (if (window-parameter window 'k-tabbar::show-buffer-group)
             (if dropped
                 ;; We need to update the buffer-group list display
                 (k-tabbar::reset-tabbar-cache window t))
           ;; Not showing the buffer-group list
           (when (eq bgkey
                     (window-parameter window 'k-tabbar::buffer-group))
             ;; We are showing the buffer-group from which buffer has been
             ;; dropped, so we will have to refresh it.
             (if (eq buffer (window-buffer window))
                 ;; The window's buffer is the buffer we have removed
                 ;; from the window's selected buffer-group.  We will
                 ;; have to clear the buffer-group so that a suitable
                 ;; alternative can be selected.
                 (k-tabbar::clear-window-cache
                  window 'k-tabbar::buffer-group)
               ;; Remove buffer from the buffer-list cache for the window
               (set-window-parameter
                window 'k-tabbar::buffer-list
                (delq buffer
                      (window-parameter window 'k-tabbar::buffer-list))))
             ;; Clear the window cache so that the display will be
             ;; updated.
             (k-tabbar::reset-tabbar-cache window t)))
	 ;; Also need to remove buffer entries from
	 ;; `k-tabbar::window-buffer-groups'
	 (let* ((wbg (window-parameter
		     window  'k-tabbar::window-buffer-groups))
		(entry (assq bgkey wbg)))
	   (if (memq buffer entry)
	       (or (setcdr entry (delq buffer (cdr entry)))
		   ;; Looks like entry is now empty
		   (set-window-parameter
		    window 'k-tabbar::window-buffer-groups
		    (assq-delete-all bgkey wbg))))))
       nil t))))

(defun k-tabbar::drop-buffer (buffer)
  "Drop BUFFER from all buffer-groups, updating our caches as needed."
  (mapc
   (lambda (buffer-group)
     (when (memq buffer (cdr buffer-group))
       (k-tabbar::drop-buffer-from-group
        buffer (k-tabbar::actual-buffer-group-key (car buffer-group)))))
   (copy-tree k-tabbar::buffer-groups)))


(defun k-tabbar::project-name (bgkey)
  "Return the project name for the project group given byu BGKEY."
  (if (fboundp #'k-project::project-name)
      (k-project::project-name (cdr bgkey))
    (cdr bgkey)))

(defun k-tabbar::record-project-buffer (buf)
  "If BUF is visiting a file in a project directory, record it as a
  project buffer, returning t if it was not previously known.  If buf is
  a compilation buffer for a project, we don't need to add a
  mode-buffer, so we throw `k-tabbar::stop'." 
  (let ((path (k::project-path buf)))
    (when path
      (when (fboundp #'k-project::add-project-path)
	(k-project::add-project-path path))
      (k-tabbar::record-bgroup-buffer 
       (k-tabbar::project-buffer-group-key path)
       buf)
      (if (eq (with-current-buffer buf major-mode)
	      'compilation-mode)
	  (throw 'k-tabbar::stop nil)))))

(defun k-tabbar::record-mode-buffer (buf)
  "Record BUF as a mode buffer."
  (with-current-buffer buf
    (k-tabbar::record-bgroup-buffer
     (k-tabbar::mode-buffer-group-key buf) buf)))
    
(defconst k-tabbar::emacs-buffer-group-key '(emacs)
  "The key for the emacs buffer-group.")
  
(defun k-tabbar::record-emacs-buffer (buf)
  "Record BUF as an emacs special buffer (if it is so), and preventing
   it from being added to any further categories." 
  (when (k::emacs-buffer-p buf)
    (k-tabbar::record-bgroup-buffer
     k-tabbar::emacs-buffer-group-key buf)
    (throw 'k-tabbar::stop nil)))

(defun k-tabbar::add-buf-to-bgroup (buf bgroup)
  "Add BUF to buffer-group BGROUP."
  (setcdr bgroup
	  (cons buf (cdr bgroup))))

(defun k-tabbar::cache-new-buffer-in-group (buf bgkey)
  "Add BUF to the buffer list for any windows showing bufer group BGKEY, 
  and clear their tabbar display caches."
  (walk-windows
   (lambda (window)
     (when (eq bgkey (window-parameter window 'k-tabbar::buffer-group))
       (set-window-parameter
	window 'k-tabbar::buffer-list
	(append (window-parameter window 'k-tabbar::buffer-list)
		(list buf)))
       ;; Force redisplay of the buffer-list.
       (k-tabbar::reset-tabbar-cache window)))
   nil t))

(defun k-tabbar::record-bgroup-buffer (bgkey buf)
  "Record an association between the buffer-group identfied by BGKEY and
  BUF, clearing caches as appropriate."
  (if bgkey
      (let ((bgroup (assq bgkey k-tabbar::buffer-groups)))
	(if bgroup
	    (unless (k-tabbar::buffer-in-bgroup-p buf bgkey)
	      (k-tabbar::add-buf-to-bgroup buf bgroup)
	      (k-tabbar::cache-new-buffer-in-group buf (car bgroup)))
	  (setq k-tabbar::buffer-groups
		(cons (list bgkey buf)
		      k-tabbar::buffer-groups))
	  (k-tabbar::clear-buffer-group-display-caches)))))

(defun k-tabbar::record-frame-buffer (buf frame)
  "Record an association between BUF and FRAME."
  (k-tabbar::record-bgroup-buffer
   (k-tabbar::frame-buffer-group-key frame)
   buf))

(defun k-tabbar::is-frame-buffer-p (buf frame)
  "Predicate returning t if BUF is in the frame buffer-group for FRAME."
  (k-tabbar::buffer-in-bgroup-p
   buf (k-tabbar::frame-buffer-group-key frame)))

(defun k-tabbar::record-buffers-for-dir (path bgkey)
  "Record any buffer showing a file in PATH to the buffer-group given by
  BGKEY."
  (mapc
   (lambda (buf)
     (let ((filename (buffer-file-name buf)))
       (and filename
	    (string= path (file-name-directory filename))
	    (k-tabbar::record-bgroup-buffer bgkey buf))))
   (buffer-list)))
	
(defun k-tabbar::record-dired-buffer (buf)
  "Record BUF as a dired buffer.  If buf is a master dired buffer, check
  all other buffers for inclusion into the new dired group.  Stop any
  further checking of categories by throwing `k-tabbar::stop'." 
  (let ((bgkey (k-tabbar::dired-buffer-group-key buf)))
    (if (k-tabbar::dired-master-buffer-p buf)
	(let ((path (with-current-buffer (cdr bgkey)
		      list-buffers-directory)))
	  (unless (assq bgkey k-tabbar::buffer-groups)
	    (k-tabbar::record-bgroup-buffer bgkey buf))
	    (walk-windows
	     (lambda (window)
	       (if (eq buf (window-buffer window))
		   (unless (window-parameter
			    window 'k-tabbar::show-buffer-group)
		     (set-window-parameter
		      window 'k-tabbar::buffer-group bgkey))))
	     nil t)
	  (k-tabbar::record-buffers-for-dir path bgkey)
	  (throw 'k-tabbar::stop nil))
      ;; Not a master buffer
      (when bgkey
	(k-tabbar::record-bgroup-buffer bgkey buf)))))
	
(defvar k-tabbar::buffer-group-fns
  '((frame . k-tabbar::record-frame-buffer)
    (project . k-tabbar::record-project-buffer)
    (dired . k-tabbar::record-dired-buffer)
    (emacs . k-tabbar::record-emacs-buffer)
    (mode . k-tabbar::record-mode-buffer))
  "An alist of buffer-group functions keyed by category.  Customise this
  to add categories.  To disable one of the standard categories, set its
  function to nil.  Note that these functions are processed in the order
  they appear in the alist, and each function can prevent further
  functions being tried by throwing `k-tabbar::stop'") 

(defun k-tabbar::record-buffer (buf &optional frame)
  "Record the existance of BUF, associating it with the appropriate
  buffer-groups, etc.  With optional FRAME, associate buf with its frame
  buffer-group."
  (if frame
      (funcall (cdr (assq 'frame k-tabbar::buffer-group-fns))
	       buf frame)
    (when (k-frame::significant-buffer-p buf)
      (unless (eq buf k-tabbar::last-killed-buffer)
	(catch 'k-tabbar::stop
	  (mapc
	   (lambda (fn-entry)
	     (unless (eq 'frame (car fn-entry))
	       ;; Frame is handled as a special case aove
	       (and (cdr fn-entry)
		    (funcall (cdr fn-entry) buf))))
	   k-tabbar::buffer-group-fns))))))

(defun k-tabbar::non-frame-buffer-groups ()
  "Return the buffer-group keys for all non-frame buffer-groups."
  (remq nil
	(mapcar
	 (lambda (bg) ;; return non-frame-entries
	   (unless (eq (caar bg) 'frame) (car bg)))
	 k-tabbar::buffer-groups)))

(defun k-tabbar::position (buf buffer-list)
  "Return integer position of BUF in BUFFER-LIST, or nil if not found.
  This is a helper function for sorting buffers with `k-tabbar::buffer<'."
  (let ((posn 0) (result))
    (while buffer-list
      (if (eq buf (car buffer-list))
	  (setq buffer-list nil
		result posn)
	(setq posn (1+ posn)
	 buffer-list (cdr buffer-list))))
    result))

(defun k-tabbar::buffer< (buf1 buf2)
  "Predicate for sorting buffers.  This sorts BUF1 and BUF2 according to
their relative positions in `k-tabbar::frame-buffer-list'.  If neither
buffer appears in the list, they are sorted alphabetically."
  (let ((pos1 (k-tabbar::position buf1 k-tabbar::frame-buffer-list))
	(pos2 (k-tabbar::position buf2 k-tabbar::frame-buffer-list)))
    (if pos1
	(if pos2
	    (< pos1 pos2)
	  t)
      (if pos2
	  nil
	(string< (buffer-name buf1) (buffer-name buf2))))))

(defun k-tabbar::buffer-group-frame-posn (buffer-group buffer-list)
  "Return the lowest position of all buffers in BUFFER-GROUP, from
  BUFFER-LIST." 
  (let ((buffers (cdr buffer-group))
	pos
	result)
    (while buffers
      (if (setq pos (k-tabbar::position (car buffers) buffer-list))
	  (if (or (not result) (< pos result))
	      (setq result pos)))
      (setq buffers (cdr buffers)))
    result))

(defvar k-tabbar::this-buffer nil
  "Variable to be redefined in local scope for use in
  `k-tabbar::buffer-group-type-priority' for sorting dired master
  buffers first.")

(defun k-tabbar::buffer-group-type-priority (bgkey)
  "Return an integer describing the relative priority, for
  sorting purposes, of the buffer-group type (the car of a
  buffer-group key) given by BGTYPE."
  (let ((bgtype (car bgkey)))
    (cond
     ((eq bgtype 'project) 2)
     ((eq bgtype 'dired)
      (if (k-tabbar::dired-master-buffer-p k-tabbar::this-buffer) 1 3))
     ((eq bgtype 'mode) 4)
     ((eq bgtype 'emacs) 5)
     (5))))

(defun k-tabbar::buffer-group-key< (bgkey1 bgkey2)
  "Predicate for sorting buffer-groups.  This sorts BGKEY1 and BGKEY2
 according to the relative positions of their earliest buffer in
`k-tabbar::frame-buffer-list'.  If neither buffer-group has buffers in
the list, or their positions are equal, they are sorted by type: dired,
project, mode, emacs, and finally aphabetically by name/path."
  (let ((pos1 (k-tabbar::buffer-group-frame-posn
	       (assq bgkey1 k-tabbar::buffer-groups)
	       k-tabbar::frame-buffer-list))
	(pos2 (k-tabbar::buffer-group-frame-posn
	       (assq bgkey2 k-tabbar::buffer-groups)
	       k-tabbar::frame-buffer-list)))
    (if (not (eq pos1 pos2))
	(if (and pos1 pos2)
	    (< pos1 pos2) ;; pos1 and pos2 exist and are different
	  (if pos1
	      t
	    nil))
      ;; pos1 and pos2 are the same
      (if (eq (car bgkey1) (car bgkey2))
	  ;; both bgs are of the same type
	  (string< (format "%s" (cdr bgkey1)) (format "%s" (cdr bgkey2)))
	;; Different types, so compare their relative priorities
	(< (k-tabbar::buffer-group-type-priority bgkey1)
	   (k-tabbar::buffer-group-type-priority bgkey2))))))


(defun k-tabbar::buffer-group (window)
  "Cache-get function to return a suitable default buffer-group key for
  WINDOW.  If the window's  current-buffer is a dired master buffer,
  then return the dired buffer-group, otherwise the first entry from a
  sorted-list of buffer-groups, ignoring frame buffer-groups unless
  there is no alternative."
  (let* ((k-tabbar::frame-buffer-list
	  (frame-parameter (window-frame window) 'buffer-list))
	 (k-tabbar::this-buffer (window-buffer window)) ;; used by sort
	 (sorted (sort (k-tabbar::non-frame-buffer-groups)
		       #'k-tabbar::buffer-group-key<))
	 elem
	 result)
    (while (setq elem (car sorted))
      (if (k-tabbar::buffer-in-bgroup-p k-tabbar::this-buffer elem)
	  (setq result elem
		sorted nil)
	(setq sorted (cdr sorted))))
    (if result
	result
      (k-tabbar::frame-buffer-group-key (window-frame window)))))

(defun k-tabbar::buffer-group-name (bgkey)
  "Return a suitable buffer-group name for BGKEY."
  ;; TODO: maybe handle name shortening
  (let ((type (car bgkey)))
    (concat "["
            (cond
             ((eq type 'project)
	      (concat "Proj:" (k-tabbar::project-name bgkey)))
             ((eq type 'frame) "Frame")
             ((eq type 'mode)
              (concat "Mode:"
                      (replace-regexp-in-string
                       "-mode$" "" (symbol-name (cdr bgkey)))))
             ((eq type 'dired) (concat "Dired:" (buffer-name (cdr bgkey))))
             ((eq type 'emacs) "*emacs*")
             (t "?????"))
            "]")))

(defun k-tabbar::buffer-group-description (bgkey)
  "Return a suitable buffer-group name for BGKEY."
  ;; TODO: handle project name shortening when it is implemented
  (let ((type (car bgkey)))
    (concat
     (cond
      ((eq type 'project) "Project buffer group")
      ((eq type 'frame) "Frame buffer group")
      ((eq type 'mode) "Mode buffer group")
      ((eq type 'dired) "Dired buffer group")
      ((eq type 'emacs) "Buffer group"))
     " for "
     (cond
      ((eq type 'project) (concat "project in " (cdr bgkey)))
      ((eq type 'frame) "this frame")
      ((eq type 'mode) (symbol-name (cdr bgkey)))
      ((eq type 'dired) (concat "directory "
				(k-tabbar::dired-directory bgkey)))
      ((eq type 'emacs) "special emacs buffers")))))
     
(defun k-tabbar::buffer-group-help (buffer-group)
  "Provide the help text for buffer-group buttons.  Specifically
  describe BUFFER-GROUP"
  (format
   (concat
    "Buffer group %s\n (%s)\n\n"
    "mouse-1: select this buffer group and switch to buffer list display.")
   (k-tabbar::buffer-group-name buffer-group)
   (k-tabbar::buffer-group-description buffer-group)))
      
(defun k-tabbar::buffer-list-buffer-group-help (buffer-group)
  "Provide the help text for buffer-group buttons.  Specifically
  describe BUFFER_GROUP"
  (format
   (concat
    "Buffer group %s\n (%s)\n"
    "All buffers shown here belong to this buffer group.\n\n"
    "mouse-1: switch to a display of all buffer groups.")
   (k-tabbar::buffer-group-name buffer-group)
   (k-tabbar::buffer-group-description buffer-group)))
      
(defun k-tabbar::update-frame-buffer-group (window)
  "Check that WINDOW's buffer is recorded for its frame buffer-group."
  (let ((buf (window-buffer window))
	(frame (selected-frame)))
    (unless (k-tabbar::is-frame-buffer-p buf frame)
      (k-tabbar::new-window-buffer))))

(defun k-tabbar::record-recent-window-buffer (window)
  "Record for WINDOW that its buffer was recently visited in the context
  of the current buffer-group for the window."
  (setq k-tabbar::last-window window)
  (let* ((bgkey (window-parameter window 'k-tabbar::buffer-group))
	 (wbg (window-parameter window 'k-tabbar::window-buffer-groups))
	 (buffer (window-buffer window))
	 entry)
    (when (and bgkey (k-tabbar::buffer-in-bgroup-p buffer bgkey))
      (if wbg
	  (progn
	    (setq entry (assq bgkey wbg))
	    (if entry
		(unless (eq (nth 1 entry) buffer)
		  ;; buffer is not the first element in our list
		  (setcdr entry (cons buffer (delq buffer (cdr entry)))))
	      ;; This is a new buffer-group for the alist
	      (setcdr wbg (cons (cons bgkey (list buffer)) (cdr wbg)))))
	(set-window-parameter
	 window 'k-tabbar::window-buffer-groups
	 (list (cons bgkey (list buffer))))))))

;;
;; TABBAR KEYMAPS
;;

(defvar k-tabbar::null-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line mouse-1] 'ignore)
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line mouse-2] 'ignore)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-3] 'ignore)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-5] 'ignore)
    keymap)
  "No-action keymap for separators, etc.")

(defvar k-tabbar::select-buffer-groups-keymap 
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line down-mouse-1]
      'k-tabbar::switch-to-buffer-groups)
    (define-key keymap [header-line mouse-2] 'ignore)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-3] 'ignore)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-4]
      'k-tabbar::switch-to-buffer-groups)
    (define-key keymap [header-line mouse-5]
      'k-tabbar::switch-to-buffer-groups)
    keymap)
  "The keymap for the select buffer groups button.")

(defvar k-tabbar::buffer-group-entry-keymap 
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line down-mouse-1]
      'k-tabbar::act-on-buffer-group-entry)
    (define-key keymap [header-line mouse-2] 'ignore)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-3] 'ignore)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-5] 'ignore)
    keymap)
  "The keymap for a buffer-group selection button.")

(defvar k-tabbar::buffer-entry-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line down-mouse-1]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line C-mouse-1]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line M-mouse-1]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line down-mouse-2]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line C-mouse-2]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line M-mouse-2]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line down-mouse-3]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line C-mouse-3]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line M-mouse-3]
      'k-tabbar::act-on-buffer-entry)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-5] 'ignore)
    keymap)
  "The keymap for buffer selection buttons.")

(defvar k-tabbar::scroll-button-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line down-mouse-1]
      'k-tabbar::tabbar-handle-scroll-event)
    (define-key keymap [header-line mouse-2] 'ignore)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-3] 'ignore)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-4] 'ignore)
    (define-key keymap [header-line mouse-5] 'ignore)
    keymap)
  "The keymap for the tabbar scroll buttons.")

;;
;; TABBAR INTERACTION
;;

(defun k-tabbar::switch-to-buffer-groups ()
  "Make the current tabbar show the available buffer-groups
  rather than the current buffer list."
  (interactive)
  (let ((window (selected-window)))
    (set-window-parameter window 'k-tabbar::show-buffer-group t)
    (k-tabbar::reset-tabbar-cache window t)
    (force-window-update window)))

(defun k-tabbar::best-buffer (window bgkey)
  "Return the most suitable buffer for displaying on WINDOW from the
  buffer-group given by BGKEY."
  (let ((wbg (window-parameter window 'k-tabbar::window-buffer-groups))
	entry
	result)
    (setq
     result
     (nth 1 (or (assq bgkey wbg)
		(assq bgkey k-tabbar::buffer-groups)
		(window-parameter window 'k-tabbar::buffer-list))))
    result))


(defun k-tabbar::switch-to-buffer-list (window bgkey)
  "Make the current tabbar for WINDOW show the buffer list for the
  buffer-group given by BGKEY along with a suitable buffer."
  (set-window-parameter window 'k-tabbar::show-buffer-group nil)
  (set-window-parameter window 'k-tabbar::buffer-group bgkey)
  (unless (k-tabbar::buffer-in-bgroup-p
	   (window-buffer window) bgkey)
    ;; Current buffer for window is not a member of the new
    ;; buffer-group, so find a candidate buffer to replace it with.
    (set-window-buffer window (k-tabbar::best-buffer window bgkey)))
  (k-tabbar::reset-tabbar-cache window t)
  (force-window-update window))

(defun k-tabbar::buffer-group-from-event (event)
  "Return the buffer-group key for the buffer-group button that was
  clicked in EVENT."
  (let* ((event-object (nth 4 (cadr event)))
	 (string (car event-object))
	 (pos (cdr event-object)))
    (get-text-property pos 'buffer-group string)))
    
(defun k-tabbar::act-on-buffer-group-entry (event)
  "Act on EVENT, which will be a key-click from a buffer-group button,
  by selecting the appropriate buffer-group and switching to the
  buffer-list display."
  (interactive "@e")
  (let ((window (caar (cdr event)))
	(buffer-group (k-tabbar::buffer-group-from-event event)))
    (k-tabbar::switch-to-buffer-list window buffer-group)))

(defun k-tabbar::buffer-from-event (event)
  "Return the buffer for the buffer-list button that was clicked in EVENT."
  (let* ((event-object (nth 4 (cadr event)))
	 (string (car event-object))
	 (pos (cdr event-object)))
    (get-text-property pos 'buffer string)))

(defun k-tabbar::switch-to-buffer (window buffer)
  "Set WINDOW to display BUFFER, returning to any previously saved
  state and (re)setting the appropriate window parameters."
  (set-window-parameter window 'k-tabbar::tabbar-elems nil)
  (set-window-parameter window 'k-tabbar::tabbar-line nil)
  (set-window-buffer window buffer))

(defvar k-tabbar::frame-flash-color "PowderBlue"
  "Colour to use when wishing to get a frame noticed.")

(defun k-tabbar::visit-buffer-new-frame (buffer)
  "Like it says.  TODO: better comment"
  (with-current-buffer buffer
    (make-frame)))

(defvar k-tabbar::last-buffer-entry-action nil
  "TODO: doc string")

(defvar k-tabbar::alternate-frames nil
  "TODO: doc string")

(defun k-tabbar::alternate-frames-for-buffer (buffer)
  "Build a list, in a sensible order, of alternate frames for displaying 
buffer."
  IMPLEMENT THIS)

(defun k-tabbar::visit-buffer-alternate-frame (buffer)
  "Like it says.  TODO: better comment"
  (if (and (eq last-command 'k-tabbar::act-on-buffer-entry)
	   (eq k-tabbar::last-buffer-entry-action 'alternate-buffer))
      (progn
	;; Same command as last: try a different alternate frame
	(message "SOMETHING DIFFERENT"))
    (message "SOMETHING NEW: %s" k-tabbar::last-buffer-entry-action)))

(defun k-tabbar::act-on-buffer-entry (event)
  "Act on EVENT, which will be a key-click from a buffer button."
  ;; TODO: Enhance the doc string when the function gets enhanced
  (interactive "@e")
  (let ((type (car event))
	(window (caar (cdr event)))
	(buffer (k-tabbar::buffer-from-event event))
	action)
    (cond
     ((eq type 'down-mouse-1)
      (k-tabbar::switch-to-buffer window buffer))
     ((or (eq type 'down-mouse-2)
	  (eq type 'C-mouse-1)
	  (eq type 'C-mouse-2)
	  (eq type 'M-mouse-2))
      (k-tabbar::visit-buffer-new-frame buffer))
     (nil
      (switch-to-buffer-other-frame buffer)
      (let ((bcolor (frame-parameter (selected-frame) 'background-color)))
	(set-background-color k-tabbar::frame-flash-color)
	(sit-for 0.4)
	(set-background-color bcolor)))
     ((or (eq type 'down-mouse-3)
	  (eq type 'M-mouse-1)
	  (eq type 'C-mouse-3)
	  (eq type 'M-mouse-3))
      (setq action 'alternate-buffer)
      (k-tabbar::visit-buffer-alternate-frame buffer))
     (t
      (message "Do not know how to handle %s here." type)))
    (setq k-tabbar::last-buffer-entry-action action)))

(defun k-tabbar::tabbar-handle-scroll-event (event)
  (interactive "@e")
  (let* ((window (caar (cdr event)))
	 (event-object (nth 4 (cadr event)))
	 (string (car event-object))
	 (direction (get-text-property 0 'direction string)))
    (if (eq direction '>)
	(set-window-parameter
	 window 'k-tabbar::tabbar-scroll
	 (1+ (or (window-parameter window 'k-tabbar::tabbar-scroll) 0)))
      (when (eq direction '<)
	(set-window-parameter
	 window 'k-tabbar::tabbar-scroll
	 (1- (window-parameter window 'k-tabbar::tabbar-scroll)))))
    
    ;; Force a redraw
    (set-window-parameter window 'k-tabbar::tabbar-line nil)
    (force-window-update window)))




;;
;; TABBAR HEADER LINE FORMATTING
;;

(defun k-tabbar::button (text is-bg is-selected is-on-frame help keymap
			      &rest properties)
  "Create a propertized string for a tabbar button.
  TEXT is the text of the button, IS-BG identifies whether the
  button is for a buffer-group (rather than a buffer).
  IS-SELECTED describes whether the button is for the selected
  buffer or buffer-group.  IS-ON-FRAME, applies only to buttons
  for buffers and identifies whether the buffer has been
  displayed on the current frame.  HELP is the help text for the
  button, displayed on mouseover."

  (let* ((fcolor (if is-selected "blue"
		   (if (or is-bg is-on-frame) "gray20" "gray50")))
	 (mfcolor (if is-selected "blue" "DarkViolet"))
	 (bcolor (if is-bg "PaleGoldenrod" "gray80"))
	 (height 0.8)
	 (fg-face (list
		      :inherit 'variable-pitch
		      :height height
		      :background bcolor
		      :foreground fcolor
		      :box '(:color "white" :style released-button))))
    (apply
     #'propertize
     (concat " " text " ")
     'pointer 'hand
     'help-echo help
     'local-map keymap
     'font-lock-face fg-face
     'face fg-face
     'mouse-face (list
		  :inherit 'variable-pitch
		  :height height
		  :background bcolor
		  :foreground mfcolor
		  :box '(:color "white" :style pressed-button))
     'element-type 'element
     properties)))

(defun k-tabbar::scroll-help (direction)
  ;; TODO: implement this
  "SCROLL")

(defun k-tabbar::scroll-button (direction &optional is-bg)
  "Return a scroll-button for DIRECTION (which will should be '< or
  '>).  The optional IS-BG argument identifies whether this button is
  for a buffer-group display which determines the background color." 
  (let ((string
	 (if (eq direction '<) "<" ">")) ;;"«" "»"))
	(fcolor "blue")
	(mfcolor "DarkViolet")
	(bcolor (if is-bg "PaleGoldenrod" "gray80")))
    (propertize
     string
     'mouse-face (list
		  :inherit 'variable-pitch
		  :height 0.8
		  :weight 'bold
		  :background bcolor
		  :foreground mfcolor
		  :underline t
		  :box '(:color "white" :style pressed-button))
    'font-lock-face (list :inherit 'variable-pitch
			   :height 0.8
			   :weight 'bold
			   :background bcolor
			   :foreground fcolor
			   :box '(:color "white" :style released-button))
     'face (list :inherit 'variable-pitch
		 :height 0.8
		 :weight 'bold
		 :background bcolor
		 :foreground fcolor
		 :box '(:color "white" :style released-button))
     'direction direction
     'pointer 'hand
     'help-echo (k-tabbar::scroll-help direction)
     'local-map k-tabbar::scroll-button-keymap
     'element-type 'scroll-button)))

(defun k-tabbar::tabbar-left-buffer-group (window)
  "Return the button for the tabbar's leftmost buffer-group for WINDOW."
  (let ((display-buffer-group
	 (window-parameter window 'k-tabbar::show-buffer-group))
	(buffer-group (k-tabbar::window-cache-get
		       window 'k-tabbar::buffer-group)))
    (k-tabbar::button
     (k-tabbar::buffer-group-name buffer-group)
     t display-buffer-group nil
     (if display-buffer-group
	 (k-tabbar::buffer-group-help buffer-group)
       (k-tabbar::buffer-list-buffer-group-help buffer-group))
     (if display-buffer-group
	 k-tabbar::buffer-group-entry-keymap
       k-tabbar::select-buffer-groups-keymap)
     'buffer-group buffer-group)))

(defconst k-tabbar::buffer-group-separator
  (propertize " "
	      'local-map k-tabbar::null-keymap
	      'element-type 'separator
	      'font-lock-face (list :inherit 'variable-pitch
				    :height 1
				    :background "PaleGoldenrod")
	      'face (list :inherit 'variable-pitch
			  :height 1
			  :background "PaleGoldenrod"))
  "Single-width separator element for buffer-group displays")

(defconst k-tabbar::buffer-list-separator
  (propertize " "
	      'local-map k-tabbar::null-keymap
	      'element-type 'separator
	      'font-lock-face (list :inherit 'variable-pitch
				    :height 1
				    :background "gray80")
	      'face (list :inherit 'variable-pitch
			  :height 1
			  :background "gray80"))
  "Single-width separator element for buffer-list displays")

(defun k-tabbar::separator (is-bg &optional width)
  "Return a separator element for a header-line."
  (if width
      (let ((bcolor (if is-bg "PaleGoldenrod" "gray80")))
	(propertize (make-string width (string-to-char " "))
		    'local-map k-tabbar::null-keymap
		    'element-type 'separator
		    'font-lock-face (list :inherit 'variable-pitch
					  :height 1
					  :background bcolor)
		    'face (list :inherit 'variable-pitch
				:height 1
				:background bcolor)))
    (if is-bg
	k-tabbar::buffer-group-separator
      k-tabbar::buffer-list-separator)))

(defun k-tabbar::element-to-wrap (tabbar-line)
  "Determine whether TABBAR-LINE needs to wrap, and if so, at what
  element position."
  (with-temp-buffer
    (let ((truncate-partial-width-windows)
	  (inhibit-modification-hooks t)
	  deactivate-mark ;; Prevent deactivation of the mark!
	  (n 0)
	  (truncate-lines nil)
	  (buffer-undo-list t))
      (catch 'wrap
	(mapc
	 (lambda (elem)
	   (insert elem)
	   (unless (eq 'separator (get-text-property 0 'element-type elem))
	     (goto-char 1)
	     (and (> (vertical-motion 1) 0)
		  (throw 'wrap n))
	     (setq n (+ n 2))))
	 tabbar-line)
	nil))))

(defvar k-tabbar::extra-spaces-for-header-line 8
  "TODO: Customise this.  Extra spaces that can be added to header-line
  to pack scroll-bars to a suitable rightmost position.  This overcomes
  some oddities in wrapping in a temp buffer that seem to be related to
  temp buffers being narrower than a header line.")

(defun k-tabbar::spare-space (tabbar-line)
  "Return a count of how many separator spaces can be added to
  TABBAR-LINE  before the line width will be exceeded."
  (with-temp-buffer
    (let ((truncate-partial-width-windows)
	  (inhibit-modification-hooks t)
	  deactivate-mark ;; Prevent deactivation of the mark!
	  (n 0)
	  (truncate-lines nil)
	  (buffer-undo-list t))
	(mapc
	 (lambda (elem)
	   (insert elem))
	 tabbar-line)
	(while
	    (progn
	      (goto-char 1)
	      (= (vertical-motion 1) 0))
	  (goto-char (point-max))
	  (insert k-tabbar::buffer-group-separator)
	  (setq n (1+ n)))
	(+ k-tabbar::extra-spaces-for-header-line n -1))))

(defun k-tabbar::tabbar-add-scroll (window elems)
  (let ((scroll-pos (k-tabbar::element-to-wrap elems))
	(is-bg (window-parameter window 'k-tabbar::show-buffer-group))
	(result (copy-sequence elems))
	spaces tail)
    (when scroll-pos
      (setq tail (nthcdr scroll-pos result))
      (setcdr tail nil)
      (setcar tail (k-tabbar::scroll-button '> is-bg))
      (setq spaces (k-tabbar::spare-space result))
      (setcdr tail (cons (car tail) nil))
      (setcar tail (k-tabbar::separator is-bg spaces)))
    result))

(defun k-tabbar::sort-dired-primary-first (blist)
  "BLIST is a sorted list of buffers for dired mode.  Find the master
  buffer entry from the list and move it to the front."
  (let ((this blist)
	master)
    (while this
      (if (k-tabbar::dired-master-buffer-p (car this))
	  (setq master (car this)
		this nil
		blist (delq master (copy-tree blist)))
	(setq this (cdr this))))
    (cons master blist)))
    
(defun k-tabbar::buffer-list (window)
  "Cache-get function for determining the buffer-list for WINDOW.
  This returns a list of buffers for the current buffer-group,
  that is sorted appropriately for the current frame."
  (let* ((buffer-group (k-tabbar::window-cache-get
			window 'k-tabbar::buffer-group))
	 (blist (copy-tree (cdr (assq buffer-group
				       k-tabbar::buffer-groups))))
	 (k-tabbar::frame-buffer-list
	  (frame-parameter (window-frame window) 'buffer-list)))
    (setq blist (sort blist #'k-tabbar::buffer<))
    (if (k-tabbar::is-dired-buffer-group-p buffer-group)
	;; Find the master buffer and move it to the start of the list
	(k-tabbar::sort-dired-primary-first blist)
      blist)))
  
(defun k-tabbar::help-for-buffer-entry (buf is-selected on-frame)
  "Return the help text for a buffer-list entry button.  This button is
  for BUF which.  IS-SELECTED will be t if buf is the current buffer.
  ON-FRAME will be t if buf has been shown on the current frame."
  (let ((name (buffer-name buf)))
    (format
     (concat 
      "Select buffer %s%s\n\n"
      "mouse-1: select %s %s\n"
      "Ctrl-mouse-1: select %s on this frame\n"
      "Alt-mouse-1: select %s on another frame"
      )
     name
     (if is-selected " (the current buffer)" "")
     name
     (if on-frame "on this frame" "on another frame")
     name name)))
   
(defun k-tabbar::tabbar-buffer-list (window)
  "Build the tabbar buffer-list for WINDOW.
  Note that this *must* be a single-depth (ie unnested list) in order
  for scrolling to work."
  ;; TODO: make help text for mouse-1 deal with buffers that have no
  ;; frame associated with them.  Ditto for k-tabbar::act-on-buffer-entry
  (let ((buffer-list
	 (k-tabbar::window-cache-get window 'k-tabbar::buffer-list))
	(frame-buffers
	 (frame-parameter (window-frame window) 'buffer-list))
	(selected-buffer (window-buffer window)))
    (apply
     #'append
     (mapcar
      (lambda (buf)
	(let ((on-frame (member buf frame-buffers)))
	  (list
	   (k-tabbar::separator nil)
	   (k-tabbar::button
	    (buffer-name buf)
	    nil (eq buf selected-buffer) on-frame
	    (k-tabbar::help-for-buffer-entry
	     buf (eq buf selected-buffer) on-frame)
	    k-tabbar::buffer-entry-keymap
	    'on-frame on-frame
	    'buffer buf))))
      buffer-list))
    ))

(defun k-tabbar::buffer-group-keys (window)
  "Return a sorted list of buffer-group keys for WINDOW.
  The list will be sorted as follows:
  - The active buffer-group for the frame
  - The frame buffer-group 
  - All other (non-frame) buffer-groups sorted according to the rules
    defined in `k-tabbar::buffer-group-key<' (above)"
  (let* ((k-tabbar::frame-buffer-list
	  (frame-parameter (window-frame window) 'buffer-list))
	 (this-bg (k-tabbar::window-cache-get
		  window 'k-tabbar::buffer-group))
	 (frame-bg (k-tabbar::frame-buffer-group-key
		    (window-frame window)))
	 (other-groups
	  (remq this-bg (k-tabbar::non-frame-buffer-groups))))
    (append
     (list this-bg)
     (unless (eq this-bg frame-bg) (list frame-bg))
     (sort other-groups #'k-tabbar::buffer-group-key<)
     )))

(defun k-tabbar::tabbar-buffer-groups (window)
  "Return a list of propertized, displayable strings for all buffer-groups
  for WINDOW (with the execption of the current buffer-group which is
  handled separately).
  Note that this *must* be a single-depth (ie unnested list) in order for
  scrolling to work."
  (let ((buffer-group-keys (k-tabbar::buffer-group-keys window)))
    (cdr
     (apply
      #'append
      (mapcar
       (lambda (bgkey)
	 (list (k-tabbar::separator t)
	       (k-tabbar::button
		(k-tabbar::buffer-group-name bgkey)
		t nil nil
		(k-tabbar::buffer-group-help bgkey)
		k-tabbar::buffer-group-entry-keymap
		'buffer-group bgkey)))
       (cdr buffer-group-keys))))))

(defun k-tabbar::tabbar-elems (window)
  "Generate a tabbar-line for WINDOW.  This is normally called only if
  the window cache entry of the same name is null. 
  Tabbar-elems is a full-width (non-scrolled) displayable version of the
  header-line.  Note that this returns an unnested list which is what
  our scroll handling mechanism needs."
  (cons
   (k-tabbar::tabbar-left-buffer-group window)
   (if (window-parameter window 'k-tabbar::show-buffer-group)
       (k-tabbar::tabbar-buffer-groups window)
     (k-tabbar::tabbar-buffer-list window))))

(defun k-tabbar::tabbar-line (window)
  "Generate a tabbar-line for WINDOW.  This is normally called only if
the window cache entry of the same name is null.
The tabbar-line takes tabbar-elems and applies scrolling to it."
  (let ((elems (k-tabbar::window-cache-get
		window 'k-tabbar::tabbar-elems))
	(scroll-amount
	 (window-parameter window 'k-tabbar::tabbar-scroll)))
    
    (k-tabbar::tabbar-add-scroll
     window
     (if (and scroll-amount (> scroll-amount 0))
	 (append
	  (list (car elems)
		(cadr elems)
		(k-tabbar::scroll-button '< nil))
	  (nthcdr (1+ (* 2 scroll-amount)) elems))
       elems))))

(defun k-tabbar::generate-tabbar-line ()
  "Generate a header-line entry for the current window, using cached
  data as much as possible."
  (k-tabbar::update-frame-buffer-group (selected-window))
  (k-tabbar::cache-get 'k-tabbar::tabbar-line))

(defconst k-tabbar::header-line-format
  '(:eval (k-tabbar::generate-tabbar-line)))


;;;
;;; HOOKS INTO EMACS
;;;

(defconst k-tabbar::suspend-buffer-list-handling nil
  "Variable to be overridden in local let bindings that will prevent
  updates of the buffer-list from being processed by
  `k-tabbar::update-buffer-list' (below).")

(defun k-tabbar::new-window-buffer ()
  "Called from k-frame when a new buffer has been visited in a window."
  ;; TODO: relocate this
  (let* ((window (selected-window))
	 (buffer (window-buffer window))
	 (k-tabbar::suspend-buffer-list-handling t)
	 (buffer-group-key
	  (window-parameter window 'k-tabbar::buffer-group))
	 (buffer-group
	  (cdr (assq buffer-group-key k-tabbar::buffer-groups)))
	 (buffer-in-buffer-group (member buffer buffer-group)))
    (unless (eq k-tabbar::last-killed-buffer buffer)
      (k-tabbar::record-buffer buffer)
      (k-tabbar::record-buffer buffer (window-frame window))
      (set-window-parameter
       window 'k-tabbar::path
       (k::buffer-path buffer))
      (if buffer-in-buffer-group
	  (k-tabbar::reset-tabbar-cache window nil)
	;; Force reselection of buffer-group
	(k-tabbar::reset-tabbar-cache window t)
	(k-tabbar::clear-window-cache window 'k-tabbar::buffer-group))
      (force-window-update window))))

(defun k-tabbar::update-buffer-list ()
  "Hook function called whenever the buffer-list is updated.  This
  provides the safest place to record the association between windows
  and buffers."
  (unless k-tabbar::suspend-buffer-list-handling
    (let ((k-tabbar::suspend-buffer-list-handling t))
      (unless (or (eq (current-buffer) k-tabbar::last-killed-buffer)
		  (minibufferp (current-buffer)))
	(let* ((window (selected-window))
	       (buffer (window-buffer window)))
	  (unless (eq buffer (window-parameter window 'k-tabbar::buffer))
	    (k-tabbar::new-window-buffer)
	    (k-tabbar::record-recent-window-buffer window)
	    (set-window-parameter window 'k-tabbar::buffer buffer)))))))

(defun k-tabbar::kill-buffer ()
  "Hook function called before the current buffer is killed.
  Responsible for removing all references to the buffer from our state."
  (let ((buffer (current-buffer)))
    (when (k-frame::significant-buffer-p buffer)
      ;; Prevent `k-tabbar::update-buffer-list from re-adding buffer
      ;; back into our state before buffer is actually killed.
      (setq k-tabbar::last-killed-buffer buffer)
      (k-tabbar::drop-buffer buffer)
      (force-window-update))))

(defvar k-tabbar::deleting-frame nil
  "TODO: doc string")

(defun k-tabbar::delete-frame (frame)
  "Hook function to handle the deletion of FRAME by removing it from our
  state."
  (unless k-tabbar::deleting-frame
    (let ((k-tabbar::deleting-frame t))
      (setq k-tabbar::buffer-groups
	    (assq-delete-all 
	     (k-tabbar::frame-buffer-group-key frame)
	     k-tabbar::buffer-groups)))))

(defvar k-tabbar::mode-bg-before-change nil
  "Record of the current-buffer's mode buffer-group-key made before the
  buffer's major mode is changed.")

(defun k-tabbar::before-mode-change ()
  "Handle a change in current buffer's major mode.  This is called prior
  to the mode change, so the old mode is still in effect."
  (let ((buffer (current-buffer)))
    (when (k-frame::significant-buffer-p buffer)
      (setq k-tabbar::mode-bg-before-change 
	    (k-tabbar::mode-buffer-group-key buffer))
      (add-hook 'post-command-hook #'k-tabbar::after-mode-change))))

(defun k-tabbar::after-mode-change ()
  "Complete the change to the current buffer's major mode.  This is
  called after the mode change, so the new mode will be in effect."
  (remove-hook 'post-command-hook #'k-tabbar::after-mode-change)
  (let ((buffer (current-buffer))
	window-list)
    (walk-windows
     ;; Record list of windows for which we will have to change the
     ;; buffer-group being shown, to the new mode buffer-group.
     (lambda (window)
       (when (and (eq buffer (window-buffer window))
		  (eq k-tabbar::mode-bg-before-change
		      (window-parameter window 'k-tabbar::buffer-group))
		  (not (window-parameter window 'k-tabbar::show-buffer-group)))
	 ;; This window is showing the buffer and the old mode
	 ;; buffer-group: we will have to set it to the new mode
	 ;; buffer-group.
	 (set-window-parameter
	  window 'k-tabbar::buffer-group 'pending)
	 (setq window-list (cons window window-list)))))
    (k-tabbar::drop-buffer-from-group
     buffer k-tabbar::mode-bg-before-change)
    (k-tabbar::record-buffer buffer)
    (let ((new-mode-bg (k-tabbar::mode-buffer-group-key buffer)))
      (mapc
       (lambda (window)
	 (set-window-parameter
	  window 'k-tabbar::buffer-group new-mode-bg)
	 (k-tabbar::reset-tabbar-cache window t))
       window-list)))
  (force-window-update))

(defun k-tabbar::after-save ()
  "Hook function called when a buffer is saved.  This is used to
  determine whether the file-name, directory or project, has changed."
  (let* ((buffer (current-buffer))
	 (new-path (k::buffer-path buffer)))
    (unless (string= new-path 
		     (window-parameter (selected-window) 'k-tabbar::path))
      ;; Path has changed.  This will affect the buffer's inclusion in
      ;; project and dired buffer-groups.  The simplest way to deal with
      ;; this is to drop all record of the buffer, re-record it and
      ;; redisplay everything.
      (k-tabbar::drop-buffer buffer)
      (k-tabbar::record-buffer buffer)
      (k-tabbar::record-buffer buffer (selected-frame))
      (set-window-parameter
       (selected-window) 'k-tabbar::path
       (k::buffer-path buffer)))))
  

;;
;; MODE SWITCHING AND GENERAL SETUP
;;

(defun k-tabbar::reset-state ()
  "Reset all state and cache values."
  (setq k-tabbar::buffer-groups nil
	k-tabbar::last-killed-buffer nil)
  (mapc
   #'k-tabbar::record-buffer
   (buffer-list))
  (walk-windows
   (lambda (window)
     (apply 'k-tabbar::clear-window-cache window
	    (k-tabbar::all-cache-symbols))
     (mapc
      (lambda (buf)
	(if (k-frame::significant-buffer-p buf)
	    (k-tabbar::record-buffer buf (window-frame window))))
      (frame-parameter (window-frame window) 'buffer-list))
     (k-tabbar::record-recent-window-buffer window))
   nil t)
  (force-window-update))
   
(defvar k-tabbar::old-header-line-format nil
  "Backup of header-line-format from before activation.")

(defvar k-tabbar::old-tabbar-mode nil
  "Backup of tabbar-mode from before activation.")

(defun k-tabbar::activate ()
  "Activate k-tabbar."
  (k-tabbar::reset-state)
  (unless k-tabbar::old-header-line-format
    (setq k-tabbar::old-header-line-format header-line-format
	  k-tabbar::old-tabbar-mode tabbar-mode))
  (if tabbar-mode
      (tabbar-mode -1))
  (setq-default header-line-format k-tabbar::header-line-format)
  (add-hook 'buffer-list-update-hook #'k-tabbar::update-buffer-list)
  (add-hook 'after-save-hook #'k-tabbar::after-save)
  (add-hook 'kill-buffer-hook #'k-tabbar::kill-buffer)
  (add-hook 'delete-frame-functions #'k-tabbar::delete-frame)
  (add-hook 'change-major-mode-hook #'k-tabbar::before-mode-change)
  (force-window-update))

(defun k-tabbar::deactivate ()
  "Deactivate k-tabbar."
  ;; TODO: Restore header-line-format
  (remove-hook 'buffer-list-update-hook #'k-tabbar::update-buffer-list)
  (remove-hook 'after-save-hook #'k-tabbar::after-save)
  (remove-hook 'kill-buffer-hook #'k-tabbar::kill-buffer)
  (remove-hook 'delete-frame-functions #'k-tabbar::delete-frame)
  (remove-hook 'change-major-mode-hook #'k-tabbar::before-mode-change)
  (setq-default header-line-format k-tabbar::old-header-line-format)
  (if k-tabbar::old-tabbar-mode
      (tabbar-mode nil))
  (setq k-tabbar::old-header-line-format nil
	k-tabbar::old-tabbar-mode nil)
  (force-window-update))

(add-hook 'k-mode-activation-hook #'k-tabbar::activate)
(add-hook 'k-mode-deactivation-hook #'k-tabbar::deactivate)

(provide 'k-tabbar)



(when nil
  ;; BUILDING/TESTING THIS

(defun k-tabbar::format-buffer (buf)
  "Return a string that shows buf, in an easily read format."
   (format "        %s" buf))

(defun k-tabbar::format-buffer-group (bgroup)
  "Return a string that shows BGROUP, in an easily read format."
  ;; TODO: Add formatted buffer-group name
  (concat 
   (format "  %s:\n" (car bgroup))
   (mapconcat 'k-tabbar::format-buffer (cdr bgroup) "\n")))
  
(defun k-tabbar::format-buffer-groups ()
  "Return a string that shows the current tabbar state, in an easily
  read format."
  (mapconcat 'k-tabbar::format-buffer-group
	     k-tabbar::buffer-groups "\n"))



  (k-tabbar::generate-tabbar-line)
  (k-tabbar::reset-state)
  (k-tabbar::format-buffer-groups)
  (k-tabbar::activate)
  (k-tabbar::deactivate)
  (setq debug-on-error t)
  )
