;;; k-core.el --- Core functionality used by many K-mode parts
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; 
;;
;; Specifically, this file includes:
;; - Functionality for extended minibuffer handling allowing prompts,
;;   histories and completion lists to be modified even as text is
;;   entered.
;;
;; - Pre and Post command handling with hooks to handle multiple
;;   k-mode components.
;;
;; - The core mechanisms for switching k-mode on and off, including
;;   the creation and enablement of specific k-mode keymaps.

;;
;; EXTENDED MINIBUFFER HANDLING SUPPORT STUFF
;;


(defconst k::managing-minibuffer nil
  "Dynamically scoped variable identifying whether k-mode is
managing our minibuffer history.  If it is,
`k::record-modified-history' will be in use.")

(defconst k::minibuffer-continue nil
  "Overide this variable in dynamic scope.  It will be set to t when a
minibuffer interaction has been interupted.  This allows minibuffer
prompts, completions, histories, etc to be switched seamlessly while
retaining the entered text (which will be provdided as the default on
the next call.")

(defconst k::minibuffer-zeroth-element nil
  "Should be locally overridden in a dynamic scope.  Used to
temporarily store the zeroth history element (the new one being
created but not yet recorded in history) between minibuffer reads when
the minibuffer read will be continued.")

(defconst k::minibuffer-active-history nil
  "Dynamically scoped variable containing the currently active
minibuffer history.  This is used by `k::modify-minibuffer-history' to
record changes to history elements so that they can be retained
between calls to `read-from-minibuffer', etc.")

(defconst k::minibuffer-elem 0
  "Dynamically scoped variable identifying our position in the history
list of a minibuffer-read.  This is used to retain our position
between calls to `read-from-minibuffer', etc.")

(defconst k::managing-minibuffer nil
  "Dynamically scoped variable identifying whether k-mode is
managing our minibuffer history.  If it is,
`k::record-modified-history' will be in use.")

(defun k::modify-minibuffer-history ()
  "Update the current history element to reflect the current
minibuffer contents.  This allows us to temporarily modify each
history element so that when switching between them, modifications
are not lost."
  (let ((minibuffer-str
	 (buffer-substring (minibuffer-prompt-end) (point-max))))
    (if (> k::minibuffer-elem 0)
	(setcar (nthcdr (1- k::minibuffer-elem) k::minibuffer-active-history)
		minibuffer-str)
    ;; Current element is zero
    (setq k::minibuffer-zeroth-element minibuffer-str))))

(defconst k::upper-exit nil
  "The upper value of history-element in `goto-history-element' that will
cause reading to be stopped for continuation.")

(defconst k::lower-exit nil
  "The lower value of history-element in `goto-history-element' that will
cause reading to be stopped for continuation.")

(defun k::continue-minibuffer-entry ()
  "Cause the current minibuffer-read to exit, but with `k::continue'
set so that a new minibuffer-read can commence with the same string."
  (setq k::minibuffer-continue t)
  (setq k::minibuffer-point (- (point) (minibuffer-prompt-end)))
  (exit-minibuffer))

(defun k::record-modified-history (orig-fun nabs)
  "Advice function for goto-history-element.
This updates `k::minibuffer-active-history',
`k::minibuffer-zeroth-element', and `k::minibuffer-elem' whenever
we switch between history elements during a minibuffer-read.  This
allows us more complete control of our minibuffer history allowing it
to be modified and retained within a group of related minibuffer
operations (eg when switching search-modes part way through entering
search strings)."
  (if k::managing-minibuffer
    (if (and k::upper-exit (>= nabs k::upper-exit))
	(k::continue-minibuffer-entry)
      (if (and k::lower-exit (<= nabs k::lower-exit))
	  (k::continue-minibuffer-entry)
	(k::modify-minibuffer-history)
	(if (and (= nabs 0)
		 (not minibuffer-text-before-history)
		 k::minibuffer-zeroth-element)
	    (setq minibuffer-text-before-history k::minibuffer-zeroth-element))
	(setq k::minibuffer-elem
	      (min (max nabs 0) (length k::minibuffer-active-history)))
	)
      (apply orig-fun (list nabs)))
    ;; Not k::managing-minibuffer
    (apply orig-fun (list nabs))))

(defconst k::minibuffer-point nil
  "Used to provide a starting position for point within
completing-read.  This allows our position within the entered string
to be maintained even when we manipulate said string.  Primarily
used when switching between file and buffer selection in
`k-buffer::select-file-or-buffer'.")

(defun k::completing-read-adv-fn (orig-fun prompt collection &optional
					   predicate require-match
					   initial-input hist def
					   inherit-input-method)
  "Advice function for completing read.  This allows a cursor position
to be set for the current input.  Used in conjunction with
`k::minibuffer-point'."
;;  (message "CR3 - args: %s"
;;	   (prin1-to-string
;;	     (list prompt collection predicate require-match
;;		   initial-input hist def inherit-input-method)))
  (if k::minibuffer-point
      (if (consp initial-input)
	  (if (consp k::minibuffer-point)
	      ;; k::minbuffer-point is relative to initial-input
	      ;; rather than absolute.
	      (setcdr initial-input (+ (cdr initial-input)
				       (cdr k::minibuffer-point)))
	    (setcdr initial-input k::minibuffer-point))
	(setq initial-input (cons initial-input
				  k::minibuffer-point))))
  (apply orig-fun (list prompt collection predicate
			require-match initial-input hist def
			inherit-input-method)))

(defconst k::minibuffer-highlight nil
  "Used to cause the minibuffer string to be highlighted as a
transient region.  This allows the initial input for `completing-read'
to be easily dismissed by simply typing something new.")

(defun k::minibuffer-setup ()
  "Cause the minibuffer string to be the highlighted, selected region."
  (if k::minibuffer-highlight
      (set-mark (minibuffer-prompt-end))))

(defun k::de-highlight (orig-fun args)
  (when k::minibuffer-highlight
    (deactivate-mark)
    (setq k::minibuffer-highlight nil))
  (funcall orig-fun args))




;;
;; KEYBINDING HELPER FUNCTIONS
;;

(defun k::forward-line (&optional n)
  "As the standard forward-line function but leave point at the end of
the last line, rather the start of the next."
  (if (and (> n 0) (looking-at "$"))
      (forward-char 1))
  (forward-line n)
  (if (> n 0)
      (forward-char -1)))

(defun k::get-keybindings (keymap &rest keys)
  "Return a list of keybindings (as cons-cells of the form 
  (KEY . BINDING) from KEYMAP for KEYS."
  (mapcar
   (lambda (key)
     (cons key (lookup-key keymap key)))
   keys))

(defun k::set-keybindings (keymap bindings)
  "Set keybings within KEYMAP to BINDINGS which is a list of cons-cells
  of the form (KEY . BINDING) as returned by `k::get-keybindings'."
  (mapc
   (lambda (binding)
     (define-key keymap (car binding) (cdr binding)))
   bindings))

(k::get-keybindings isearch-mode-map [C-down] [C-up])

;;
;; ALIST OF ALISTS FUNCTIONS
;;

(defun k::alist-assq (alist key)
  "Return the cons cell from ALIST keyed by KEY.  If there is no such
  entry, create an entry for it before returning a cons cell of the form 
  (keyY . nil).  The cdr of the cons cell can then be simply set to
  update ALIST."
  (let (entry)
    (or (assq key alist)
	(prog1
	    (setq entry (cons key nil))
	  (if (car alist)
	      (setcdr alist (cons entry (cdr alist)))
	    (setcar alist entry))))))
	  
(defun k::dalist-assq (dalist key1 key2)
  "Return the cons cell from DALIST (an alist of alists) keyed by KEY1
  and KEY2.  If there is no such entry, create an entry for it before
  returning a cons cell of the form (key2 . nil).  The cdr of the cons
  cell can then be simply set to update DALIST."
  (k::alist-assq (k::alist-assq dalist key1) key2))
    
(defun k::dalist-get (dalist key1 key2)
  "Return the value from DALIST, keyed by KEY1, KEY2."
  (cdr (k::dalist-assq dalist key1 key2))) 
    
(defun k::dalist-delq (dalist key1 key2)
  ;; TODO: make KEY2 optional
  "Remove from DALIST the cons cell keyed by KEY1, KEY2."
  (let ((alist-entry (assq key1 dalist)))
    (when alist-entry
      (unless (setcdr alist-entry (assq-delete-all
				   key2 (cdr alist-entry)))
	;; Our deletion has resulted in an empty alist entry, so we
	;; should delete the parent entry entirely
	(if (eq (caar dalist) key1)
	    (progn
	      ;; We need to delete the first entry in dalist, so replace
	      ;; it with the second.
	      (setcar dalist (cadr dalist))
	      (setcdr dalist (cddr dalist)))
	  ;; delete key1 from the 2nd entry
	  (setcdr dalist (assq-delete-all key1 (cdr dalist))))))))
	
	
;;
;; PATH AND VCS DIR STUFF
;;

(defun k::is-compilation-buffer-p (buf)
  "T if BUF is a compilation buffer." 
  (with-current-buffer buf
    (and (boundp 'compilation-arguments) compilation-arguments t)))
  

(defun k::is-process-buffer-p (buf)
  "T if BUF is a process buffer (or compilation buffer)." 
  (or (k::is-compilation-buffer-p buf)
      (get-buffer-process buf)))
    
(defun k::buffer-path (buf)
  "Return the full directory path for BUF."
  (and buf
       (or (buffer-file-name buf)
	   (with-current-buffer buf
	     (or (and list-buffers-directory
		      (directory-file-name list-buffers-directory))
		 (and (k::is-process-buffer-p buf)
		      (replace-regexp-in-string
		       "/$" ""
		       (expand-file-name default-directory))))))))

(defun k::is-git-dir (path)
  "T if PATH is the root directory of a git project."
  (file-exists-p (concat path "/.git")))

(defvar k::vcs-dir-functions
  '(k::is-git-dir)
  "List of functions for finding project directories.  Each such
  function takes a path argument and returns a non-nil value iff it
  represents the root directory of a project.")

(defun k::is-vcs-dir (path)
  "Return PATH if it is the root directory of a project as determined by 
trying each function in `k::project-dir-functions'."
  (catch 'found
    (mapc
     (lambda (fn)
       (and (funcall fn path) (throw 'found path)))
     k::vcs-dir-functions)
    nil))

(defun k::vcs-dir-for-path (path)
  "Search upwards from the current directory to determine whether we are
  in a project managed by some form of VCS."
  (when path
    (unless (string= "/" path)
      (or (k::is-vcs-dir path)
	  (k::vcs-dir-for-path
	   (directory-file-name (file-name-directory path)))))))

(defun k::project-path (&optional buf)
  "Return the project path for BUF if it contains a file in a project
  directory. "
  (let ((path (k::buffer-path (or buf (current-buffer)))))
    (and path (k::vcs-dir-for-path path))))
   





;;
;; K-MODE MISCELANY
;;

(defun k::emacs-buffer-p (buf)
  "T if BUF appears to be a special emacs buffer (*scratch*, *Messages*,
  etc)."
  (and (not (buffer-file-name buf))
       (not (k::is-process-buffer-p buf))
       (string-match-p "^\\*.*\\*$" (buffer-name buf)) t))

(defun k::shell-command (command)
  "Run shell COMMAND in an inferior shell, returning a string containing
stdout."
  (with-temp-buffer
    (call-process-region
     (point) (point) shell-file-name nil t nil
     shell-command-switch command)
    (buffer-string)))

(require 'subr-x)

(defun k::shell-command (command)
  "Run shell COMMAND in an inferior shell, returning a string containing
stdout.  TODO: Error handling."
  (with-temp-buffer
    (let ((result (call-process
		   shell-file-name nil (current-buffer) nil
		   shell-command-switch command)))
      (if (= result 0)
	  (string-trim-right (buffer-string))
	(throw 'k-shell-error (cons result (buffer-string)))))))
	
(defvar k::norecord-buffer nil
  "Bind this locally and set it to the value of any buffer that should
  not be recorded (by k-tabbar, k-frame, etc)
  NOT YET IMPLEMENTED IN K-FRAME - THERE HAS BEEN NO NEED SO FAR.")

(defvar k::in-emacsclient nil
  "Boolean identifying whether we are setting up an emacsclient
  buffer.")


;;
;; K-MODE MODE SWITCHING, SETTINGS SAVE AND RESTORE, AND KEYMAP HANDLING
;;

(defvar k-mode-map (make-sparse-keymap)
  "Main keymap for K-mode.")

(defvar k-mode-activation-hook nil
  "Functions to be run when k-mode is activated.")

(defvar k-mode-deactivation-hook nil
  "Functions to be run when k-mode is turned off.")

(defvar k::k-mode-active nil
  "Records whether k-mode is active or not.  This is set after `k-mode'
itself, allowing us to tell whether k-mode was active prior to any
currently in progress activation.")

(define-minor-mode k-mode
  "Toggle k mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."
  :lighter " K"  ;; Initial value of mode line
  :global t
  ;;:keymap
  (if k-mode
      (progn
	(message "Activating K mode")
	(advice-add 'goto-history-element
		    :around #'k::record-modified-history)
	(advice-add 'completing-read :around #'k::completing-read-adv-fn)
	(advice-add 'left-char :around #'k::de-highlight)
	(advice-add 'previous-line-or-history-element
		    :around #'k::de-highlight)
	(advice-add 'next-line-or-history-element
		    :around #'k::de-highlight)
	(advice-add 'move-beginning-of-line
		    :around #'k::de-highlight)
	(advice-add 'move-end-of-line
		    :around #'k::de-highlight)
	(add-hook 'minibuffer-setup-hook 'k::minibuffer-setup)
	(run-hooks 'k-mode-activation-hook)
	(setq k::k-mode-active t))
    (message "DEACTIVATING K MODE")
    (advice-remove 'goto-history-element #'k::record-modified-history)
    (advice-remove 'completing-read #'k::completing-read-adv-fn)
    (advice-remove 'previous-line-or-history-element #'k::de-highlight)
    (advice-remove 'next-line-or-history-element #'k::de-highlight)
    (advice-remove 'move-beginning-of-line #'k::de-highlight)
    (advice-remove 'move-end-of-line#'k::de-highlight)
    (remove-hook 'minibuffer-setup-hook 'k::minibuffer-setup)
    (run-hooks 'k-mode-deactivation-hook)
    (setq k::k-mode-active nil)))



(provide 'k-core)

