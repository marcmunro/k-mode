;;; k.el --- The K minor mode
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; K is Marc's mode that attempts to:
;;  - eliminate much surprising or unwanted emacs behaviour;
;;  - provide easily used advanced functionality to improve productivity;
;;  - provide unsurprising and consistent key-bindings;
;;  - make powerful emacs features easily accessible;
;;


;; BUGS:
;;
;; TODO:
;; - bring in remaining k-mode functionality (k-project, k-tabbar, etc)
;; - make k-cua smarter about appending small kills (see
;;   `k::delete-to-kill-ring') 
;; - implement the following:
;;
;; k-esc-keys:
;;  . - redo
;;  - - goto previous mark
;;  + - goto next mark
;;  n - next-match
;;  p - prev-match
;;  : - enter vi-like editing mode
;;    "r " - read file
;;    "w " - write file
;;    :    - eval command
;;         - anything else is assumed to be interactive command
;;  x - M-x
;;  / - enter a search string
;;  ? - enter a search string for backwards search
;;  0-9* - universal argument
;;  d - delete prefix
;;      w - delete words
;;      d - delete lines
;;      $ - delete to end of line
;;  c - change prefix (as delete but escapes k-esc to allow for insert)
;;  i - exit k-esc
;;  a - Ditto
;;  I - goto beginning of line and escape
;;  A - goto end of line and escape
;;  e - execute kbd macro
;;    - anything else is handled as an escape from k-esc and then
;;      interpreted normally
;; 

(require 'k-core)
(require 'k-cua)
(require 'k-word)
(require 'k-frame)
(require 'k-project)
(require 'k-tabbar)

(require 'k-compile)
(require 'k-search)
(provide 'k)

;; A few random-ish definitions which make sense with k mode:

(setq debugger-bury-or-kill 'kill)


(k-mode)

