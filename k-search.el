;;; k-search.el --- Search functionality for K-mode
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html


;; Combined and simplified search stuff for k-mode.
;; This allows all search modes to be accessed from as few keys as possible.

;; TODO:
;; - make DEL as first command in isearch clear the current search string
;;   (or do something else appropriate for starting the isearch)
;; - have ESC switch into k-esc mode
;; - make next/prev match consistent between isearch and other search modes
;; - recursive-edits?
;; - search/replace operations
;; - implement list-matching-lines as a search mode?


(require 'k-core)

(defvar k-search::search-history nil
  "The history of all k-searches, sans duplicates.")

(defvar k-search::search-modes
  '((re "Re-search%d[%c]: "
	"Regex search %D (%C)"
	re-search-forward re-search-backward nil)
    (str "Str-search%d[%c]: "
	 "String search %D (%C)"
	 search-forward search-backward nil)
    (word "Word-search%d[%c]: "
	  "Word search %D (%C)"
	  word-search-forward word-search-backward nil)
    (isearch-re "Re-Isearch%d[%c]: "
		"Regexp isearch %D (%C)"
		isearch-forward-regexp isearch-backward-regexp t)
    (isearch "Isearch%d[%c]: "
	     "Isearch %D (%C)"
	     isearch-forward isearch-backward t))
  "Alist of search-modes to search mode details.  Each detail is in
the form: (prompt description forward-search-fn backward-search-fn
is-incremental), where prompt and description are strings that will
be formatted by k-search::format-searchstr.")

(defvar k-search::search-mode
  (caar k-search::search-modes)
  "The current search-mode.")

(defvar k-search::search-is-forward t
  "Whether search direction is forwards.")

(defun k-search::search-case ()
  "Return short descriptive string for whether search is case-sensitive."
  (if case-fold-search "nc" "cd"))

(defun k-search::search-Case ()
  "Return long descriptive string for whether search is case-sensitive."
  (if case-fold-search "case insensitive" "case sensitive"))

(defun k-search::search-direction ()
  "Return short string for search direction."
  (if k-search::search-is-forward ">" "<"))

(defun k-search::search-Direction ()
  "Return long string for search direction."
  (if k-search::search-is-forward "forwards" "backwards"))

(defun k-search::format-searchstr (str)
  "Format a search prompt or description string.  TODO: add
description of how."
  (let ((parts (split-string str "%")))
    (apply 'concat
	   (car parts)
	   (mapcar
	    (lambda (part)
	      (pcase (elt part 0)
		(?c (concat (k-search::search-case)
			    (substring part 1)))
		(?C (concat (k-search::search-Case)
			    (substring part 1)))
		(?d (concat (k-search::search-direction)
			    (substring part 1)))
		(?D (concat (k-search::search-Direction)
			    (substring part 1)))
		(- part)))
	    (cdr parts)))))

(defun k-search::search-prompt ()
  "Return the prompt for the current search mode."
  (let ((prompt-str
	 (cadr (assoc k-search::search-mode k-search::search-modes))))
    (k-search::format-searchstr prompt-str)))

(defun k-search::search-description ()
  "Return the description for the current search mode."
  (let ((prompt-str
	 (cadr (cdr (assoc k-search::search-mode k-search::search-modes)))))
    (k-search::format-searchstr prompt-str)))

(defun k-search::search-fn ()
  "Return the search function for the current search mode."
  (nth (if k-search::search-is-forward 3 4)
       (assoc k-search::search-mode k-search::search-modes)))

(defconst k-search::cur-buffer nil
  "Override this in a dynamic scope in order to track the current buffer
before entering a minibuffer.  It is defined using defconst so that this
documentation string can be attached to it.")

(defconst k-search::is-incremental nil
  "Should be locally overridden in a dynamic scope when performing an
  isearch.")

(defun k-search::next-search-mode (mode)
  "Return the search mode from k-search::search-modes that follows MODE."
  (let ((cur k-search::search-modes))
    (while (and cur
		(not (eq (caar cur) mode)))
      (setq cur (cdr cur)))
    (caar (or (cdr cur) k-search::search-modes))))

(defun k-search::continue-search ()
  "Leave the current k-search search so that we may continue with a
new search-mode."
  (setq k::minibuffer-continue t)
  (if k-search::is-incremental
      (isearch-exit)
    (exit-minibuffer)))

;; TODO: make doc strings for k-search::search-point and friends

(defun k-search::handle-point-at-mode-switch (cur new)
  "When switching modes, ensure that point is handled sanely."
  (if (k-search::search-is-incremental-p cur)
      (with-selected-window k-search::cur-window
	(goto-char k-search::point))))
  
(defun k-search::minibuffer-next-mode ()
  "Switch to the next search mode, temporarily exitting the minibuffer."
  (interactive)
  (let ((new (k-search::next-search-mode k-search::search-mode)))
    ;;(k-search::handle-point-at-mode-switch k-search::search-mode new)
    (setq k-search::search-mode new)
    (k-search::continue-search)))
  
(defun k-search::minibuffer-prev-mode ()
  "Switch to the previous search mode, temporarily exitting the minibuffer."
  (interactive)
  (let* ((k-search::search-modes (reverse k-search::search-modes))
	 (new (k-search::next-search-mode k-search::search-mode)))
    ;;(k-search::handle-point-at-mode-switch k-search::search-mode new)
    (setq k-search::search-mode new)
    (k-search::continue-search)))

(defun k-search::toggle-case-fold ()
  "Toggle the case-fold-search value that is appropriate to the
current search mode, keeping isearch-case-fold-search and
case-fold-search in step."
  (interactive)
  (with-current-buffer (window-buffer k-search::cur-window)
    (setq isearch-case-fold-search
	  (setq case-fold-search (not case-fold-search))))
  (k-search::continue-search))
  
(defun k-search::toggle-search-direction ()
  "Toggle the direction of k searches."
  (interactive)
  (setq k-search::search-is-forward (not k-search::search-is-forward))
  (k-search::continue-search))

(defconst k-search::basic-search-modes
  '(re-search-forward re-search-backward
    search-forward search-backward
    word-search-forward word-search-backward)
  "The set of \"basic\" search modes.  These are search-modes for which
the search function takes a string parameter for which to search, unlike
the incremental search modes.")

(defun k-search::search-is-incremental-p (&optional mode)
  "Whether the search function is incremental, and therefore handles
its own search string."
  
  (nth 5 (assoc (or mode k-search::search-mode) k-search::search-modes)))

(defun k-search::read-one-search-str (search-str elem)
  "Read a search string using the minibuffer.  This allows the search-type
to be modified even as the search string is entered.
Returns a tuple of the form (continue search-str cur-history-elem)
  k::minibuffer-continue is t if the reading of input was interupted
    by changing some aspect of the search-mode
  search-str is the string returned.
  k::minibuffer-elem identifies which history element is being returned."
  (let ((k::minibuffer-continue)
	(k::managing-minibuffer t)
	(k::minibuffer-elem elem))
    (setq search-str (read-from-minibuffer
		      (k-search::search-prompt) search-str
		      k-search::minibuffer-keymap nil
		      (cons 'k::minibuffer-active-history
			    k::minibuffer-elem)))
    (list k::minibuffer-continue search-str k::minibuffer-elem)))

(defun k-search::search-with-wraparound (search-fn string)
  "Perform a non-incremental, interactive, search which wraps around
the end, or start depending on direction, of the buffer."
  (let ((start (point))
	(failed-already)
	(result)
	(history-delete-duplicates t))
    ;; Add history here so that it gets added even if the search fails
    (add-to-history 'k-search::search-history string)
    (while
	(progn
	  (condition-case err
	      (progn
		(setq result
		      (funcall search-fn string))
		nil)
	    (search-failed
	     ;; We didn't find what we were looking for - we may want
	     ;; to wrap-around.
	     (if failed-already
		 (progn
		   ;; We already tried wrapping the search around the end, so
		   ;; return to our starting position, and re-raise the error.
		   (goto-char start)
		   (signal (car err) (cdr err))) ;; re-raise original error
	       ;; Wrap the search around the end of the buffer and try
	       ;; again.
	       (if k-search::search-is-forward
		   (progn
		     (message "Search hit BOTTOM, continuing at TOP")
		     (goto-char (point-min)))
		 (message "Search hit TOP, continuing at BOTTOM")
		 (goto-char (point-max)))
	       (setq failed-already t))))))
    result))


(defconst ksearch::no-push-state nil
  "Dynamically override this and set it to t to prevent the next call to
isearch-push-state from doing anything.")

(defun k-search::isearch-push-state-fn (orig-fun)
  "Advice function for isearch-push-state.  This allows us to prevent
isearch-push-state from doing anything (because we would otherwise get
duplicate states pushed due to the way that isearch-mode is implemented)."
  (if ksearch::no-push-state
      (setq ksearch::no-push-state nil)
    (funcall orig-fun)))

(advice-add 'isearch-push-state :around #'k-search::isearch-push-state-fn)


(defun k-search::set-isearch-string (str)
  "Set the isearch-string from str as though it had been entered manually."
  (mapc
   (lambda (x)
     (isearch-yank-string (string x)))
   str)
  (if (string= "" str)
      ;; This, or something like it, is needed to make empty strings
      ;; appear with a prompt.
      (isearch-process-search-string "" "")))
 

(defun k-search::isearch-hook-fn ()
  "Run on entry to isearch-mode, this sets up an initial search string
from our search history."
  (when k-search::is-incremental  ;; Do only if we are called from k-search
    (isearch-push-state) ;; This has to happen before we can run
                         ;; isearch-yank-string
    (k-search::set-isearch-string k-search::cur-string)
    ;; Prevent the next call to  isearch-push-state() from doing anything.
    ;; This makes up for the isearch-push-state() call we had to make at
    ;; the start of this fn. 
    (setq ksearch::no-push-state t)))

(add-hook 'isearch-mode-hook 'k-search::isearch-hook-fn)

(defun k-search::do-incremental-search (string elem)
  "Run an incremental search, with the search string initialised from
our parameters."
  (when (> (length k::minibuffer-active-history) (length k-search::search-history))
    ;; An extra history element has been added.  We must remove it.
    ;; TODO: prevent it from being added in the first place rather then
    ;; resorting to this hack.  CHECK WHETHER THIS HAS NOW BEEN DONE
    ;; - A SIMPLE MESSAGE FROM HERE SHOULD DO IT
    (setq k::minibuffer-active-history (cdr k::minibuffer-active-history)))
  (let* ((k-search::cur-elem elem)
	 (k-search::cur-string string)
	 (k-search::is-incremental t)
	 (res (funcall (k-search::search-fn))))
    (if res
	(add-to-history 'k-search::search-history isearch-string)
      (k-search::isearch-save-history-elem isearch-string k-search::cur-elem))
   (list k::minibuffer-continue isearch-string k-search::cur-elem res)
))
  

(defun k-search::isearch-replace-search-str (str)
  ;; TODO: comment this (is it in use)
  (setq isearch-cmds (last isearch-cmds))
  (isearch--set-state (car isearch-cmds))
  (k-search::set-isearch-string str))

(defun k-search::isearch-save-history-elem (string elem)
  "Save STRING as the ELEMth search history element.  If elem is zero,
save it in k::minibuffer-zeroth-element"
  (if (= 0 elem)
      (setq k::minibuffer-zeroth-element string)
    (setcar (nthcdr (1- elem) k::minibuffer-active-history) string)))
  
(defun k-search::isearch-get-history-elem (elem)
  "Extract the ELEMth (starting at 1) entry from our search history."
  (if (= 0 k-search::cur-elem)
      (or k::minibuffer-zeroth-element "")
    (or (nth (1- k-search::cur-elem) k::minibuffer-active-history)
	"")))

(defun k-search::isearch-history-next ()
  "Modify the isearch search string to be the next string from our
history."
  (interactive)
  (if (<= k-search::cur-elem 0)
      (minibuffer-message "End of history; no default available")
    (k-search::isearch-save-history-elem isearch-string k-search::cur-elem)
    (setq k-search::cur-elem (1- k-search::cur-elem))
    (k-search::isearch-replace-search-str
     (k-search::isearch-get-history-elem k-search::cur-elem))))

(defun k-search::isearch-history-prev ()
  "Modify the isearch search string to be the previous string from our
history."
  (interactive)
  (if (>= k-search::cur-elem (length k::minibuffer-active-history))
      (minibuffer-message "Beginning of history; no preceding item")
    (k-search::isearch-save-history-elem isearch-string k-search::cur-elem)
    (setq k-search::cur-elem (1+ k-search::cur-elem))
    (k-search::isearch-replace-search-str
     (k-search::isearch-get-history-elem k-search::cur-elem))))
  
;;

(defun k-search::execute-search (string elem &optional highlight)
  "Execute the appropriate search function for k-search.  If not doing
  an incremental search and highlight is non-nil, then highlight the
  matched string."
  (let ((k::minibuffer-continue)
	(search-fn (k-search::search-fn))
	(res))
    (cond
     ((memq search-fn k-search::basic-search-modes)
      (list nil string elem
	    (prog1
		(k-search::search-with-wraparound search-fn string)
	      (and highlight
		   (isearch-highlight (match-beginning 0) (match-end 0)))
	      )))
     ((k-search::search-is-incremental-p)
      (k-search::do-incremental-search string elem))
      ;;(setq res (k-search::do-incremental-search string elem))
      ;;(list k::minibuffer-continue string elem res))
     (t (signal :error (format "UNHANDLED SEARCH TYPE: %s" search-fn))))))

(defun k-search::do-one-search (string elem &optional exit-if-no-continue)
  "Attempt a k-search, returning a tuple of (CONTINUE SEARCH-STR ELEM RESULT),
where result is the result of any search function that was called, and
CONTINUE, if true, causes another go around.  This is intended to be
called from a loop."
  (let ((res))
    (when (not (k-search::search-is-incremental-p))
      (setq res (k-search::read-one-search-str string elem))
      (unless (car res)
	;; Update string and elem from the result of read-one-search-str
	(setq string (cadr res))
	(setq elem (nth 2 res))))
    (if (car res)
	;; Return res so that we can go round again
	res
      ;; Execute the search, returning the appropriate tuple
      (unless exit-if-no-continue
	(k-search::execute-search string elem)))))

(defun k-search::fixup-history (elem string limit)
  "Each call to do-one-search may result in the history being extended
but not otherwise modified.  This de-extends the history (ensuring the
length remains within LIMIT), and updates it at ELEM with STRING."
;;  (message "FIXUP ELEM: %s, STRING: \"%s\", LIMIT: %s, HIST: %s"
;;	   elem string limit
;;	   k::minibuffer-active-history)
  (if (> (length k::minibuffer-active-history) limit)
      (setq k::minibuffer-active-history (cdr k::minibuffer-active-history)))
  (if (= elem 0)
      (setq k::minibuffer-zeroth-element string)
    (setcar (nthcdr (1- elem) k::minibuffer-active-history) string))
;;  (message "FIXUP(3) HIST: %s, ZEROTH: %s"
;;	   k::minibuffer-active-history
;;	   k::minibuffer-zeroth-element)
  )

(defun k-search::start ()
  "Maybe add (point) to the mark-ring."
  (with-selected-window k-search::cur-window
    (setq k-search::start-point (point))
    (unless (or (and transient-mark-mode mark-active)
		(and (mark) (= (mark) (point))))
      (push-mark k-search::start-point))))

(defun k-search::quit()
  "Quit from a k-search.  This performs a non-local exit after moving
point back to our starting point."
  (interactive)
  (with-selected-window k-search::cur-window
    (let ((mark (pop-mark))) ;; unset the mark we may have set on entry
      (if (and mark (not (eq mark k-search::start-point)))
	  ;; Looks like we didn't set the mark on entry to this
	  ;; k-search.  In that case we shouldn't have popped the
	  ;; mark, so re-push it.
	  (push-mark mark)))
    (goto-char k-search::start-point))
  (isearch-dehighlight)
  (throw 'k-search::exit t))

(defvar k-search::start-point nil
  "Provides the value of point at which k-search::search was invoked.
  This allows quit from k-search to return to the starting point.
  Note that this variable is be redefined locally in
  k-search::search.  This definition exists solely to provide this
  documentation string.")

(defvar k-search::perform-search nil
  "When k-search::do-one-search completes with k:minibuffer-continue
  set to t, this variable tells k-search::search to execute a single
  search based upon the current string and element.  The value is set
  by k-search::search-forward and k-search::search-backward.
  Note that this variable is be redefined locally in
  k-search::search.  This definition exists solely to provide this
  documentation string.")

(defvar k-search::point nil
  "This records the value of point, each time through the main loop in
  k-search::search.  It is used in order to return point when
  switching search mode from an incremental search. 
  Note that this variable is be redefined locally in
  k-search::search.  This definition exists solely to provide this
  documentation string.")

(defun k-search::search (&optional string)
  "Do the k-search thing."
  (interactive)
  (if (window-minibuffer-p)
      ;; Because k-mode is a minor-mode, its key bindings take
      ;; precedence over k-search::minibuffer-keymap.  Since we are in
      ;; the minibuffer we don't want to run k-search::search, so we
      ;; will instead invoke the appropriate function from
      ;; k-search::minibuffer-keymap.
      (call-interactively 
       (lookup-key k-search::minibuffer-keymap (this-command-keys)))
    (catch 'k-search::exit
      (let* ((res)
	     (cur-search-mode)
	     (k-search::start-point)
	     (k-search::point)
	     (k-search::perform-search)
	     (elem (if string 0 (if k-search::search-history 1 0)))
	     (k::minibuffer-active-history (copy-tree k-search::search-history))
	     (k::minibuffer-zeroth-element)
	     (k::minibuffer-highlight t)
	     (k-search::cur-window (selected-window))
	     (exit-to-last-string)
	     (string (or string
			 (car k::minibuffer-active-history)))
	     (limit (length k-search::search-history)))
	(k-search::start)

	(while
	    (progn
	      (setq cur-search-mode k-search::search-mode
		    k-search::point (point)
		    res (k-search::do-one-search string elem
						 exit-to-last-string)
		    elem (nth 2 res)
		    string (cadr res))

	      ;; TODO: clean this code - maybe move/rewrite fixup-history
	      (if (or (car res)
		      (not exit-to-last-string))
		  (k-search::fixup-history elem string limit))
	  
	      (when (car res) ;; ie, while continuing
		(if k-search::perform-search
		    (progn
		      (k-search::execute-search string elem t)
		      (setq exit-to-last-string t))
		  (k-search::handle-point-at-mode-switch
		   cur-search-mode k-search::search-mode)
		  ;; provide a, temporary, descriptive message of the new
		  ;; selected search mode
		  (minibuffer-message
		   (format "%s: %s" (k-search::search-description)
			   (cadr res)))))
	      (car res)))
	(isearch-dehighlight)
	(nth 3 res)))))

(defun k-search::de-propertise (str)
  "Remove font etc, from str."
  (set-text-properties 0 (length str) nil str)
  str)

(defun k-search::clear-highlighted-region ()
  (if (and transient-mark-mode mark-active)
      (delete-region (point) (mark))))
  
(defun k-search::yank-word (arg)
  "Get the next word from k-search::cur-buffer, advancing point, and add it 
to the search string."
  (interactive "p")
  (k-search::clear-highlighted-region)
  (insert
   (with-selected-window k-search::cur-window
     (let ((start))
       (setq start (point))
       (dotimes (n arg
	 (k-word::next-word-end)))
       (k-search::de-propertise (buffer-substring start (point)))))))

(defun k-search::yank-line (arg)
  "Get the next line from k-search::cur-buffer, advancing point, and add it 
to the search string."
  (interactive "p")
  (k-search::clear-highlighted-region)
  (insert
   (with-selected-window k-search::cur-window
     (let ((start))
       (setq start (point))
       (k::forward-line arg)
       (k-search::de-propertise (buffer-substring start (point)))))))

(defun k-search::search-in-search (string)
  (unless string
    (setq string (car k::minibuffer-active-history)))
)  

(defun k-search::search-forward ()
  "Cause k-search::search to peform a forwards search."
  (interactive)
  (setq k-search::search-is-forward t
	k-search::perform-search t)
  (k-search::continue-search))
  
(defun k-search::search-backward ()
  "Cause k-search::search to peform a backwards search."
  (interactive)
  (setq k-search::search-is-forward nil
	k-search::perform-search t)
  (k-search::continue-search))


(defvar k-search::minibuffer-keymap
  (copy-keymap minibuffer-local-map)
  "The minibuffer keymap for k-search functions.")

;; This prevents emacs from allowing the cursor into a prompt string
;; when reading from the minibuffer.
(customize-set-variable
 'minibuffer-prompt-properties
 (quote (read-only t cursor-intangible t face minibuffer-prompt)))

(define-key k-mode-map (vector ?\C-/) 'k-search::search-forward)
(define-key k-mode-map [C-?] 'k-search::search-backward)
(define-key k-mode-map "\C-f" 'k-search::search)

(lookup-key k-search::minibuffer-keymap "\C-f")

(define-key k-search::minibuffer-keymap
  [C-down] 'k-search::minibuffer-next-mode)
(define-key k-search::minibuffer-keymap
  [\C-up] 'k-search::minibuffer-prev-mode)
(define-key k-search::minibuffer-keymap
  "\C-w" 'k-search::yank-word)
(define-key k-search::minibuffer-keymap
  "\C-k" 'k-search::yank-line)
(define-key k-search::minibuffer-keymap
  "\C-u" 'universal-argument)
(define-key k-search::minibuffer-keymap
  "\M-c" 'k-search::toggle-case-fold)
(define-key k-search::minibuffer-keymap
  "\M-d" 'k-search::toggle-search-direction)
(define-key k-search::minibuffer-keymap
  "\C-f" 'k-search::search-forward)
(define-key k-search::minibuffer-keymap
  (kbd "C-n") 'k-search::search-forward)
(define-key k-search::minibuffer-keymap
  (kbd "C-p") 'k-search::search-backward)
(define-key k-search::minibuffer-keymap
  (kbd "C-g") 'k-search::quit)


(defvar k-search::pre-k-search-bindings
  (k::get-keybindings
   isearch-mode-map
   [C-down] [C-up] [down] [up] (kbd "C-f") (kbd "C-n") (kbd "C-p"))
  "Record of original keybindings in isearch-map that will be modfied in
  k-mode, and to which we can restore when exiting k-mode.") 
  
(defun k-search::activate ()
  "Set up key-bindings for k-search."
  (k::set-keybindings 
   isearch-mode-map
   `(([C-down] . k-search::minibuffer-next-mode)
     ([C-up] . k-search::minibuffer-prev-mode)
     ([down] . k-search::isearch-history-next)
     ([up] . k-search::isearch-history-prev)
     (,(kbd "C-f") . isearch-repeat-forward)
     (,(kbd "C-n") . isearch-repeat-forward)
     (,(kbd "C-p") . isearch-repeat-backward))))

(defun k-search::deactivate ()
  "Undo key-bindings for k-search."
  (k::set-keybindings isearch-mode-map k-search::pre-k-search-bindings))

(add-hook 'k-mode-activation-hook 'k-search::activate)
(add-hook 'k-mode-deactivation-hook 'k-search::deactivate)



(provide 'k-search)
