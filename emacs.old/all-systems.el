;;;
;;; File:         all-systems.el
;;; Author:       Dan Harlin/Craig R. Steury
;;; Created:      August 14, 1995
;;; 
;;; Description:  Walk across all files in all systems, performing function.
;;;               The top-level functions #'ss-do-all-files uses the perl-utility
;;;               'what-system to generate all the files in the CDRS what-system
;;;               database.  It then uses the package 'walk-files to walk through
;;;               these files performing some function on each file.
;;;               
;;; RCS:  $Id: all-systems.el,v 1.3 1998/08/06 19:08:47 csteury Exp $
;;;               
;;;  (c) Copyright 1995, Parametric Technology Corporation, all rights reserved.
;;;

(require 'walk-files)

(defvar *all-systems* nil)
(defvar *current-system* nil)
(defvar *system-files* nil)

;;; Example
(defun ss-tst ()
  (interactive "")
  (ss-do-all-files
   #'(lambda ()
       (pop-to-buffer (current-buffer))
       (cond ((y-or-n-p "Next "))
	     (t
	      (kill-buffer (current-buffer))
	      (keyboard-quit))))))

;;; Top-level routine.
(defun ss-do-all-files (func)
  "Walk through files in what-system database performing 'func on each in turn."
  (ss-init)
  (ss-do-all-systems func))


(defun ss-do-all-systems (func)
  (while (ss-next-system)
    (walk-files func *system-files*)))

(defun ss-next-system ()
  (cond ((setq *current-system* (pop *all-systems*))
	 (save-excursion
	   (ss-do-call-process "what-system" "*What Files*" "-s" *current-system*)
	   (set-buffer (get-buffer-create "*What Files*"))
	   (goto-char (point-min))
	   ;; FIXME build-file-list-from-buffer no longer exists!
	   (setq *system-files* (build-file-list-from-buffer "*What Files*"))
	   (kill-buffer (get-buffer "*What Files*")))
	 t)
	(t nil)))

(defun ss-init ()
  (save-excursion
    (ss-do-call-process "what-system" "*What Systems*" "-n")
    (set-buffer (get-buffer "*What Systems*"))
    (goto-char (point-min))
    (insert "(")
    (goto-char (point-max))
    (insert ")")
    (goto-char (point-min))
    (setq *all-systems*
	  (mapcar '(lambda (a)
		      (format "%S" a))
		  (read (set-marker (make-marker) (point) (current-buffer))))))
  (kill-buffer (get-buffer "*What Systems*")))


(defun ss-do-call-process (program buffer &rest args)
  (let ((status nil))
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (erase-buffer)
      (setq status (apply 'call-process program nil buffer nil args))
      (set-buffer-modified-p nil))
    status))		

(defun ss-continue ()
  "Continue walk through files in what-system database performing previously specified function."
  (when *walk-func*
    (walk-continue)))
