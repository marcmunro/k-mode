;;; k-word.el --- Word movement stuff for K-mode
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
;; Word handling stuff for Marc's K mode
;; This makes word movement and manipulation more natural and
;; predictable.
;;
;; By default when moving forward by words, emacs places point at the
;; end of a word.  In other editors, like vi, all movement is based
;; upon the start of words.  This is much more predictable
;; particularly when moving by, or deleting multiple words.  However, 
;; the emacs approach does have the advantage of allowing the user to
;; easily get to either end of a word, by moving back and forwards.
;; K-word takes the more predictable approach of editors like vi, but
;; still gives the advantage of being able to easily navigate to the
;; end of words.  It also deals with camelCase, treating an upper-case
;; letter following a lower-case one as the start of a word.
;;
;; Normal word navigation will take you to the start of each word in
;; turn, but changing direction will allow you to reach the end of a
;; word.  This means that by alternate use of `k-word::forward' and
;; `k-word::backward' you can go from the beginning to the end of a word,
;; or from the beginning to the end of a non-word sequence between words.
;; Also, at the last word of a line word movement forward and, by
;; extension, kill-word will navigate to the end of the word, rather than
;; the start of the next, which will be on the next line.  


(defun k-word::next-word-start-pos ()
  "Return the buffer position of the start of the next word."
  (save-excursion
    (re-search-forward "[^[:word:]]+" (point-max) t)
    (point)))

(defun k-word::next-word-end-pos ()
  "Return the buffer position of the next word end."
  (save-excursion
    (right-word)
    (point)))

(defun k-word::prev-word-start-pos ()
  "Return the buffer position of the preceding word start."
  (save-excursion
    (left-word)
    (point)))

(defun k-word::next-lower (limit)
  "Move point to next lower case character before LIMIT.  Return point if
found, else nil."
  (let ((case-fold-search))
    (if (re-search-forward "[[:lower:]]" limit t)
	(progn
	  (forward-char -1) ;; point will be after the match, so move it back
	  (point)))))

(defun k-word::next-upper (limit)
  "Move point to next upper case character before LIMIT.  Return point if
found, else nil."
  (let ((case-fold-search))
   (if (re-search-forward "[[:upper:]]" limit t)
	(progn
	  (forward-char -1) ;; point will be after the match, so move it back
	  (point)))))

(defun k-word::next-camel-word-start (limit)
  "Move point to next camelCase word start before LIMIT, returning
point if found, else nil."
  (and (k-word::next-lower limit)
       (k-word::next-upper limit)))


(defun k-word::prev-lower (limit)
  "Move point to previous lower case character after LIMIT.  Return point if
found, else nil."
  (let ((case-fold-search))
    (if (re-search-backward "[[:lower:]]" limit t)
	(point))))
  
(defun k-word::prev-upper (limit)
  "Move pint to previous upper case character after LIMIT.  Return point if
found, else nil."
  (let ((case-fold-search))
    (if (re-search-backward "[[:upper:]]" limit t)
	(point))))
  
(defun k-word::prev-camel-word-start (limit)
  "Move point to prev camelCase word start after LIMIT, returning
point if found, else nil." 
  (and (k-word::prev-upper (min (point) (1+ limit)))
       (if (k-word::prev-lower limit)
	   (progn
	     (forward-char 1)
	     (point))
	 (goto-char limit))))

(defun k-word::after-non-word-p ()
  "Is there a non-word character directly before point?"
  (save-excursion
    (forward-char -1)
    (looking-at "[^[:word:]]")))

(defun k::at-last-word-of-line ()
  "Figure out whether going forward a word will change lines.  Return t,
if so."
  (< (point)
     (save-excursion (k-word::one-word-forward) (beginning-of-line) (point))))
  
(defun k-word::goto-end-word-p (arg)
  "Predicate to determine whether word movement should go to the end
of a word rather than the usual case of the beginning.  If ARG
is provided, we will not."
  (and (not arg)
       (or (and (or (eq this-command 'k-word::backward)
		    (eq this-command 'k-word::backward-kill-word))
		(eq last-command 'k-word::forward)
		(k-word::after-non-word-p))
	   (and (or (eq this-command 'k-word::forward)
		    (eq this-command 'k-word::kill-word))
		(looking-at "[[:word:]]")
		(or (eq last-command 'k-word::backward)
		    (k::at-last-word-of-line))))))

(defun k-word::next-word-end ()
  "Move point to the next word end, taking camelCase into account."
  (let ((p))
    (setq p (k-word::next-word-end-pos))
    (if (not (k-word::next-camel-word-start p))
	(goto-char p))))
  
(defun k-word::one-word-forward ()
  "Move point to the start of the next word, taking camelCase into account."
  (let ((p))
    (setq p (k-word::next-word-start-pos))
    (if (not (k-word::next-camel-word-start p))
	(goto-char p))))
  
(defun k-word::forward (arg)
  "Command to move point a number of words forward, taking camelCase
into account.

With argument ARG, do it that many times." 
  (interactive "^P")
  (if (k-word::goto-end-word-p arg)
      (k-word::next-word-end)
    (let ((narg (prefix-numeric-value arg)))
      (if (< narg 0)
	  (k-word::backward (- narg))
	(while (> narg 0)
	  (setq narg (1- narg))  
	  (k-word::one-word-forward))))))

(defun k-word::one-word-back ()
  "Move point to the previous start of word, taking camelCase into account."
  (let ((p))
    (setq p (k-word::prev-word-start-pos))
    (if (not (k-word::prev-camel-word-start p))
	(goto-char p))))

(defun k-word::backward (arg)
  "Command to move point a number of words backward, taking camelCase
into account.

With argument ARG, do it that many times." 
  (interactive "^P")
  (let ((narg (prefix-numeric-value arg))
	(goto-end (k-word::goto-end-word-p arg)))
    (if (< narg 0)
	(k-word::forward (- narg))
      (while (> narg 0)
	(setq narg (1- narg))
	(k-word::one-word-back)))
    (if goto-end (k-word::next-word-end))))

(defun k-word::backward-kill-word (arg)
  "Command to delete the word before point.

With argument ARG, do it that many times." 
  (interactive "P")
  (kill-region (point) (progn
			 (k-word::backward arg)
			 (point))))

(defun k-word::kill-word (arg)
  "Command to delete the word after point.

With argument ARG, do it that many times." 
  (interactive "P")
  (kill-region (point) (progn
			 (k-word::forward arg)
			 (point))))


;; Replace current mappings of of word movement and word kill
;; functions, with their k-word equivalents.
;;
(define-key k-mode-map [remap right-word] 'k-word::forward)
(define-key k-mode-map [remap forward-word] 'k-word::forward)
(define-key k-mode-map [remap left-word] 'k-word::backward)
(define-key k-mode-map [remap backward-word] 'k-word::backward)
(define-key k-mode-map [remap kill-word] 'k-word::kill-word)
(define-key k-mode-map
  [remap backward-kill-word] 'k-word::backward-kill-word)


(provide 'k-word)
