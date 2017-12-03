;;; k-cua.el --- cua-mode hacks for K-mode
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; Cua-mode with some modifications.  Specifically:
;;
;; - All deletions based on regions or the DEL, Backspace keys, etc,
;;   copy into the kill-ring.  This is a long way from normal or
;;   standard behaviour but is much saner.  Everything you kill goes
;;   into the kill ring, no matter whether big or small, and kills are
;;   appended in the normal way.  This means if you delete a word and
;;   then the next three characters, it all becomes one kill.
;; - The insert key is used as yank.  S-insert as yank-pop, C-insert
;;   switches to overwrite mode.  This just seems like a better use for
;;   a dedicated keyboard key than toggling overwrite mode.
;; - Kill line is smarter than normal: if at the start of a line, it
;;   kills the contents *and* the line.  This reduces the number of
;;   times you need to press the kill-line key.

(require 'k-core)
(require 'cua-base)



(defun k::kill-line (orig-fun arg)
  "kill-line advice function that will kill entire lines when no arg is
provided and we are at the start of a line, otherwise does the normal
thing."
  (if (and (null arg)
	   (eq (point) (line-beginning-position)))
      (setq arg '(1)))
  (apply orig-fun arg))

(defun k::cua-delete-region (orig-fun)
  "Make delete-region do a kill so that mouse kills are in the kill-ring."
  (cua-cut-region nil))

(defun k::delete-to-kill-ring (orig-fun arg &optional killflag)
  "Advice function for delete-<direction>ward-char to cause all
  deletions to be sent to the kill-ring.
  TODO: Make this smarter so that we only record kills of more than N
  characters. "
  (setq killflag '(t))
  (apply orig-fun arg killflag))


(defvar k-cua::was-cua-mode nil
  "Whether cua-mode was active before we started k-mode.")

(defvar k-cua::was-transient-mark-mode nil
  "Whether transient-mark-mode was active before we started k-mode.")

(defun k-cua::activate ()
  "Activate the cua-mode part of k-mode."
  (unless k::k-mode-active
    (setq k-cua::was-cua-mode cua-mode
	  k-cua::was-transient-mark-mode transient-mark-mode))
  (cua-mode t)
  (transient-mark-mode t)
  (advice-add 'kill-line :around #'k::kill-line)
  (advice-add 'cua-delete-region :around #'k::cua-delete-region)
  (advice-add 'delete-forward-char :around #'k::delete-to-kill-ring)
  (advice-add 'delete-backard-char :around #'k::delete-to-kill-ring)
  (advice-add 'backward-delete-char-untabify :around #'k::delete-to-kill-ring)
  )

(defun k-cua::deactivate ()
  "Activate the cua-mode part of k-mode."
  (cua-mode (or k-cua::was-cua-mode -1))
  (transient-mark-mode (or k-cua::was-transient-mark-mode -1))
  (advice-remove 'kill-line #'k::kill-line)
  (advice-remove 'cua-delete-region #'k::cua-delete-region)
  (advice-remove 'delete-forward-char #'k::delete-to-kill-ring)
  (advice-remove 'delete-backard-char #'k::delete-to-kill-ring)
  (advice-remove 'backward-delete-char-untabify #'k::delete-to-kill-ring)
  )

(add-hook 'k-mode-activation-hook 'k-cua::activate)
(add-hook 'k-mode-deactivation-hook 'k-cua::deactivate)


(define-key k-mode-map [insert] 'cua-paste)
(define-key k-mode-map [S-insert] 'cua-paste-pop)
(define-key k-mode-map [C-insert] 'overwrite-mode)


(provide 'k-cua)
