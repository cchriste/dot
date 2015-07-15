;;;
;;; File:         walk-files.el
;;; Author:       Craig R. Steury
;;; Created:      July 27, 1995
;;; 
;;; Description:  Take a stroll through some files doing something or nothing -
;;;               its your choice!
;;;
;;;               (walk-files function file-list) -
;;;                     Performs 'function on each file in file-list.
;;;                     
;;;               (walk-files-in-file function file) -
;;;                     Calls walk-files with filenames found in file.
;;;
;;;               (walk-continue) -
;;;                     Continues on last file with last specified function.
;;;                     Useful when an error occurs. 
;;;                     
;;;                                                    
;;;                                                 
;;; RCS:  $Id: walk-files.el,v 1.6 1995/08/14 22:26:07 csteury Exp $
;;;               
;;;  (c) Copyright 1995, Parametric Technology Corporation, all rights reserved.
;;;

(require 'cl)
(require 'lisp-util)
(provide 'walk-files)

(defvar *walk-files* nil)
(defvar *current-file* nil)
(defvar *walk-func* nil)
(defvar *modified-files-buffer* "*Modified Files*")
(defvar *pathname-type-exclusion-list* nil "Pathname types to exclude from consideration.")
(defvar *pathname-type-inclusion-list* nil "Pathname types to include (exclude all others).")


;; Example - Assume /tmp/www.lisp has some valid filenames in it.
(defun wt1 ()
  (interactive "")
  (walk-files-in-file #'(lambda ()
			  (pop-to-buffer (current-buffer))
			  (cond ((y-or-n-p "Next "))
				(t
				 (kill-buffer (current-buffer))
				 (keyboard-quit))))
		      "/tmp/www.lisp"))

;;;
;;; Assumes a file consisting of filenames such as might be generated from 'find' or
;;; 'what-system', etc.  This function builds a list from these files and calls the
;;; function #'walk-files below.
;;; 
(defun walk-files-in-file (func file &optional reset-modified-file-buffer-p)
  "Visit all files specified in file, performing 'func on each in turn."
  (unless (file-exists-p file)
    (message (format "%s does not exist. " file)))
  (when reset-modified-file-buffer-p
    (get-buffer-create *modified-files-buffer*)
    (set-buffer *modified-files-buffer*)
    (erase-buffer))
  (condition-case err
      (save-excursion
	(walk-files func (build-file-list-from-file file)))
    (error
     (message "")
     (princ (format "Error in walk-files-in-file:  %s  " err))
     t)))

;;;
;;; Walk files performing some function.
;;; 
(defun walk-files (func file-list)
  "Visit files in 'file-list performing 'func on each in turn."
  (setq *walk-files* file-list
	*walk-func* func)
  (while (walk-next-file)
    (walk-do-file)))

;;; Continue the walk where you left off ...
(defun walk-continue ()
  "Continue the walk where you left off ..."
  (walk-do-file)
  (walk-files *walk-func* *walk-files*))

(defun walk-next-file ()
  (when (setq *current-file* (pop *walk-files*))
    (let* ((name  (substitute-in-file-name *current-file*))
	   (ftype (pathname-type name)))
      (cond ((and *pathname-type-inclusion-list*
		  ftype
		  (member ftype *pathname-type-inclusion-list*))
	     (find-file name)
	     (auto-save-mode -1)
	     t)
	    ((and *pathname-type-exclusion-list*
		  ftype
		  (not (member ftype *pathname-type-exclusion-list*)))
	     (find-file name)
	     (auto-save-mode -1)
	     t)
	    ((and (null *pathname-type-exclusion-list*)
		  (null *pathname-type-inclusion-list*))
	     (find-file name)
	     (auto-save-mode -1)
	     t)
	    (t
	     (walk-next-file))))))


(defun walk-do-file ()
  (let ((file (substitute-in-file-name *current-file*)))
    (cond ((not (file-exists-p file))
	   (message (format "%s does not exist!  " file))
	   (sleep-for 1))
	  (t
	   (find-file file)
	   (auto-save-mode -1)
	   (message (buffer-name))
	   (funcall *walk-func*)
	   (when (buffer-modified-p)
	     (let ((buffer (current-buffer)))
	       (save-buffer)
	       (pop-to-buffer (get-buffer-create *modified-files-buffer*))
	       (goto-char (point-max))
	       (when (symbolp *walk-func*)
		 (insert (format "%S " *walk-func*)))
	       (insert file)
	       (newline)
	       (set-buffer buffer)))
	   (kill-buffer (current-buffer))))))
