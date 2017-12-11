;;; k-compile.el --- Compilation handling for k-mode
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; Compilation commands for k-mode
;; This gives each project its own independent compilation buffer, and
;; asks to save only those files associated with the project.
;; Each compilation buffer has its own history.

;;
;; TODO:
;; - make next-error work on a by-project basis.
;; - grep, etc?


(require 'k-project)

(defun k-compile::name-compilation-buffer (&optional mode)
  "Set the name of a compilation buffer based on the project in
which it belongs, defaulting to *compilation*, if we are not in a known
project."
  (let ((project (k-project::buffer-project)))
    (if project
	(format "*compile %s*" project)
      "*compilation*")))

(defvar k-compile::compilation-history nil
  "An alist, keyed by project path, for compilation histories.  

This gives each project its own compilation history.")

(defun k-compile::compile (orig-fun arg)
  "Ask to save only files within the project of the current buffer, and
provide a different compilation command history for each project."
  (interactive "P")
  (let ((compile-history compile-history)
	(project (k-project::buffer-project)))
    
    (setq compile-command (or (car compile-history)
			      k::default-compilation-command))
    (call-interactively orig-fun t)
    (when k::project
      (k::record-project-buffer
       (k::project-path-for-buffer)
       (get-buffer (k::name-compilation-buffer))
       'k::rename-compilation-buffer))
    (k::save-to-k-alist 'compile-history k::project)))

(defun k-compile::compilation-history-entry (path)
  "Return the alist entry for PATH from
  `k-compile::compilation-history', creating it if needed."
  (or (assoc path k-compile::compilation-history)
      (let ((entry (cons path nil)))
	(setq k-compile::compilation-history
	      (cons entry k-compile::compilation-history))
	entry)))

(defvar k-compile::old-history nil
  "Used to store pervious value of compile-history so that it can be
  kept independent of project-based compilation histories")

(defvar k-compile::old-command nil
  "Used to store pervious value of compile-command so that it can be
  kept independent of project-based commands.")

(defun k-compile::compile (orig-fun arg &rest other-args)
  "Ask to save only files within the project of the current buffer, and
provide a different compilation command history for each project."
  (interactive
   ;; If you need to understand this, take a look at nadvice.el in the
   ;; emacs distribution.  There are comments there that help.
   (lambda (spec)
     (setq k-compile::old-history compile-history
	   k-compile::old-command compile-command)
     (let ((project (k-project::buffer-project))
	   (path (k::project-path))
	   history-entry)
       (if project
	   (setq history-entry (k-compile::compilation-history-entry path)
		 compile-history (cdr history-entry)
		 compile-command (or (car compile-history)
				     compile-command))))
     (advice-eval-interactive-spec spec)))
  (let ((project (k-project::buffer-project))
	(path (k::project-path)))
    (apply orig-fun arg other-args)
    (when project
      (setcdr (k-compile::compilation-history-entry path) compile-history)
      (setq compile-history k-compile::old-history
	    compile-command k-compile::old-command))))


;;;
;;; ACTIVATION
;;;


(defvar k-compile::orig-buffer-name-function nil
  "Original (before k-compile was activated) value for
  `compilation-buffer-name-function'.")

(defun k-compile::activate ()
  "Activate the k-compile stuff."
  (unless k-compile::orig-buffer-name-function
    (setq
     k-compile::orig-buffer-name-function compilation-buffer-name-function))
  (setq
   compilation-buffer-name-function #'k-compile::name-compilation-buffer)
  (advice-add 'compile :around #'k-compile::compile)
  )

(defun k-compile::deactivate ()
  "Deactivate the k-compile stuff."
  (setq
   compilation-buffer-name-function k-compile::orig-buffer-name-function
   k-compile::orig-buffer-name-function nil)
  (advice-remove 'compile #'k-compile::compile)
  )

(add-hook 'k-mode-activation-hook #'k-compile::activate)
(add-hook 'k-mode-deactivation-hook #'k-compile::deactivate)

(require 'compile)

;; Some error handling stuff for flutter
(add-to-list 'compilation-error-regexp-alist 'flutter)
(add-to-list
 'compilation-error-regexp-alist-alist
 '(flutter "^[^•]*•[^•]* at \\(.*\\):\\([0-9]*\\):\\([0-9]*\\) •" 1 2 3))


(provide 'k-compile)


