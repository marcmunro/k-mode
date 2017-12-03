;;; k-project.el --- Project identification and handling for k-mode
;;
;; Copyright (C) 2017 Marc Munro
;;
;; This file is not part of GNU Emacs
;;
;; Compatibility: GNU Emacs 25.x
;;
;; License: GPL V3
;;   https://www.gnu.org/licenses/gpl-3.0.html

;; K-project
;; Stuff for associating buffers with projects
;;
;; A buffer belongs to a project iff it represents a file or directory
;; within a project directory.   The project directory for a given
;; directory path, is returned by `k::vcs-dir-for-path'.
;;
;; TODO:
;; - Make the save-some-buffers stuff, near eof, work
;; - Refactor to avoid loops??
;; 

(defvar k-project::projects-alist nil
  "Alist of project paths to names of the form:
    (PATH . NAME)

  Where:
    NAME is the display name for the project.  This must be unique among
    all projects, and will be based upon the basename of the project
    path.  If a duplicate name would be generated by the addition of a new
    project, a uniquify process will be run which will possibly modify
    NAME.")


(defun k-project::get-existing-project-path (path)
  "Return the alist entry from k::project-paths that matches PATH, or
  nil if there is none."
  (assoc path k-project::projects-alist))

(defun k-project::matching-project-basenames (name)
  "Return an alist of the form ((path . name)) of
  `k-project::projects-alist' entries for which their path basenames
  match NAME."
  (delq nil
	(mapcar
	 (lambda (entry)
	   (if (string= name (file-name-nondirectory (car entry)))
	       (cons (car entry) (cadr entry))))
	 k-project::projects-alist)))

(defun k-project::project-path-old (path)
  "Search up the directory tree looking for a .git folder.
  TODO: You may want to provide other means to define projects."
  ;; DEPRECATED left for historical reasons - may become important when
  ;; re-integrating k::compile
  (cond
   ((not path) "process")
   ((string= path "/") nil)
   ((string= "/" (substring path -1))
    (k-project::project-path (substring path 0 -1)))
   ((file-exists-p (concat path "/.git")) path)
   (t (k-project::project-path
       (directory-file-name (file-name-directory path))))))

(defun k-project::common-items (list1 list2)
  "Return a list of the initial elements in LIST1 that match LIST2."
  (if (string= (car list1) (car list2))
      (cons (car list1)
	    (and (cdr list1)
		 (k-project::common-items (cdr list1) (cdr list2))))))

(defun k-project::common-starts (list)
  "Return a list containing the common elements of LIST, a list of list
  of elements.  Eg if list is '((a b c) (a b d)), the result will be '(a
  b)."
  (if (cdr list)
      (k-project::common-items (car list)
			       (k-project::common-starts (cdr list)))
    (car list)))

(defun k-project::common-path-suffix (list)
  "Get the common path-suffix from a LIST of paths."
  (k-project::common-starts
   (mapcar (lambda (path) (reverse (cdr (split-string path "/")))) list)))
  
(defun k-project::common-path-prefix (list)
  "Get the common path-prefix from a LIST of paths."
  (k-project::common-starts
   (mapcar (lambda (path) (cdr (split-string path "/"))) list)))

(defun k-project::project-basenames (list)
  "From a LIST of project directory names return the list of distinct
  project basenames."
  (delete-dups
   (mapcar
    (lambda (dirname)
      (file-name-nondirectory dirname))
    list)))

(defun k-project::matching-projects-for-basename (alist basename)
  "Return a new alist containing the entries from ALIST that match
  BASENAME."
  (delq nil
	(mapcar
	 (lambda (entry)
	   (if (string= basename (file-name-nondirectory (car entry)))
	       entry))
	 alist)))

(defun k-project::shared-suffix-elements (path paths)
  "Return the number of suffix elements from path that are shared with
  other path entries."
  (apply 'max
	 (mapcar
	  (lambda (this-path)
	    (if (eq path this-path)
		0
	      (length (k-project::common-starts
		       (list (reverse path) (reverse this-path))))))
	  paths)))

(defun k-project::attempt-prefix-removal (paths)
  "For each path in PATHS (a list of list of directory parts), attempt
  to remove as much of the prefix as possible, and return the modified
  list.  The definition of as much as possible is:
  - at least one entry must be left
  - whatever is left must not match the suffix of any other entry"
  (mapcar
   (lambda (path)
     (let ((target-len
	    (1+ (k-project::shared-suffix-elements path paths))))
       (nthcdr (- (length path) target-len) path)))
   paths))

(defun k-project::truncate-homedir-entry (paths alist)
  "For each path in PATHS that is a reference to our home directory,
  remove that home directory entry."
  (let* ((homedir (getenv "HOME"))
	 (len (length homedir))
	 (parts (cdr (split-string homedir "/"))))
    (cl-mapcar
     (lambda (entry1 entry2)
       (when (eq t (compare-strings homedir 0 len (car entry2) 0 len))
	 ;; This entry is for the user's home directory.  Remove any
	 ;; parts of homedir that have not already been removed.
	 ;; Note that this is a simplistic approach and may fail to
	 ;; deal with pathalogical cases.
	 
	 (dolist (part parts)
	   (if (string= part (car entry1))
	       (setq entry1 (cdr entry1)))))
       (or entry1 '("")))
     paths alist)))

(defun k-project::shorten-intermediate-suffices (paths)
  "For each path in PATHS (a list of list of directory parts), replace
  the cdr of the list with a '...' entry."
  (mapcar
   (lambda (path)
     (if (cdr path)
	 (list (car path) "...")
       path))
   paths))

(defun k-project::replace-suffices (paths suffices)
  "For each path in PATHS (a list of list of directory parts), replace
  append a contracted version of SUFFICES (which is actually in reverse
  order)."
  (let ((suffix
	 (if (> (length suffices) 1)
	     (list "..." (car suffices))
	   (list (car suffices)))))
    (mapcar
     (lambda (path)
       (append path suffix))
     paths)))

(defun k-project::join-path-parts (paths)
  "Concatenate the components of each path in PATHS and eliminate any
  superfluous elipses, ie those at the start of the directory path, or
  that are duplicated."
  (mapcar
   (lambda (path)
     (replace-regexp-in-string
      "\\(\\(.../\\)\\|^/\\).../" "\\2"
      (mapconcat (lambda (x) x) path "/"))
     )
   paths))

(defun k-project::uniquify-matching-projects (alist name)
  "Ensure uniqueness of names in ALIST that have basename NAME.
  See `k-project::uniquify-project-names' for a description of
  uniquification."
  (let ((matches (k-project::matching-projects-for-basename alist name)))
    (if (= 1 (length matches))
	;; There is a single match, so make the project name the basename.
	(setcdr (car matches) name))
    (let* ((paths
	    (mapcar
	     (lambda (match)
	       (reverse (split-string (car match) "/")))
	     matches))
	   ;; Note that paths contains reversed lists of dir elements
	   (suffices (k-project::common-starts paths))
	   (length (length suffices)))
      ;; 1) Remove common suffices from paths
      (setq paths (mapcar (lambda (rpath) (nthcdr length rpath)) paths))
      ;; 2) Remove common prefixes from paths
      (setq paths (mapcar 'reverse paths))
      (setq length (length (k-project::common-starts paths)))
      (setq paths (mapcar (lambda (path) (nthcdr length path)) paths))
      ;; 3) Remove as much of other prefixes as possible
      (setq paths (k-project::attempt-prefix-removal paths))
      ;; 4) Remove homedirs
      (setq paths (k-project::truncate-homedir-entry paths alist))
      ;; 5) Replace remaining suffices with "..."
      (setq paths (k-project::shorten-intermediate-suffices paths))
      ;; 6) Replace original suffices
      (setq paths (k-project::replace-suffices paths suffices))
      ;; 7) Concatenate parts and eliminate superfluous elipses
      (setq paths (k-project::join-path-parts paths))
      ;; Update the name part of each alist
      (cl-mapcar
       (lambda (match-entry path)
	 (let ((alist-entry (assoc (car match-entry) alist)))
	   (setcdr alist-entry path)))
       matches paths))))

(defun k-project::uniquify-project-names (alist)
  "Ensure that each entry in `k-project::projects-alist' has a unique
  name that is as short but also as meaningful as possible.
  The problem arises where multiple directories contain similarly named
  projects. 

  The rules for uniquification are as follows:
  1) Remove any common suffices (and retain them)
  2) Remove any common prefices
  3) Remove as much of each remaining prefix as possible while ensuring
     uniqueness
  4) Truncate references to our home directory
  5) Replace suffices of remaining values with /... to indicate removed
     directories 
  6) Add back abbreviated (with /...) versions of suffices removed in
     step 1
  7) Eliminate superflous /... sequences.

  Eg using: /root/proj/xyz/skit /home/karen/proj/xyz/skit 
            /home/marc/proj/xyz/skit /home/marc/xyz/skit
  1) ==> /root/proj /home/karen/proj /home/marc/proj /home/marc
  2) ==> root/proj home/karen/proj home/marc/proj home/marc
  3) ==> root/proj karen/proj marc/proj marc
  4) ==> root/proj karen/proj proj <empty string>
  4) ==> root/... karen/... proj <empty string>
  6) ==> root/.../.../skit karen/.../.../skit proj/.../skit .../skit
  7) ==> root/.../skit karen/.../skit proj/.../skit skit"
  (let ((basenames (k-project::project-basenames (mapcar 'car alist))))
    (mapc
     (lambda (name)
       (k-project::uniquify-matching-projects alist name))
     basenames)))

(defun k-project::add-project-path (path)
  "Add a new project PATH to `k-project::projects-alist', ensuring that
its name is unique, returning the new alist entry."
  (unless (assoc path k-project::projects-alist)
    (setq k-project::projects-alist
	  (cons (cons path "ZZZ") k-project::projects-alist))
    (k-project::uniquify-project-names k-project::projects-alist)))

(defun k-project::project-name (path)
  "Search up the directory tree looking for a vcs folder."
  (let ((ppath (k::vcs-dir-for-path path))
	(pentry))
    (when ppath
      (setq pentry (or (k-project::get-existing-project-path ppath)
		       (k-project::add-project-path ppath)))
      (cdr pentry))))

(defun k-project::buffer-project (&optional buffer)
  "Return the project for BUFFER (or `current-buffer')."
  (k-project::project-name (k::project-path buffer)))
  
(defun k-project::project-buffer-p ()
  "Identify whether the current buffer belongs to the project defined
in `k::project'."
  (string= k::project (k-project::buffer-project))) 

(defvar k-project::save-project-buffers-only nil
  "Whether `k-project::save-some-buffers' should attempt to save only
  project buffers.  This should be overridden in local let bindings.")

(defun k-project::save-some-buffers (orig-fun &optional arg predicate)
  "Save modified buffers only for files in the project of
`current-buffer'."
  (message "SAVE SOME BUFFERS")
  (let ((k::project (k-project::buffer-project)))
    (if k::project
	(setq predicate
	      (lambda ()
		(string= k::project (k-project::buffer-project)))))
    (apply orig-fun arg (list predicate))))

;;;
;;; ACTIVATION
;;;

(defun k-project::activate ()
  "Activate k-project stuff."
  (advice-add 'save-some-buffers :around #'k-project::save-some-buffers))
  
(defun k-project::deactivate ()
  "Deactivate k-project stuff."
  (advice-remove 'save-some-buffers #'k-project::save-some-buffers))
  
(add-hook 'k-mode-activation-hook #'k-project::activate)
(add-hook 'k-mode-deactivation-hook #'k-project::deactivate)

(provide 'k-project)

