;;;
;;; File:         patch.el
;;; Author:       Craig R. Steury
;;; Created:      January  8, 1993
;;; Description:  GNU lisp code to facilitate "patching" an inferior
;;;               Common Lisp image using the ilisp interface.
;;;               
;;;               
;;;  (c) Copyright 1993, Evans & Sutherland, all rights reserved.
;;;

(require 'lisp-util)

;;;----------------------------------------------------------------
;;; patch-region
;;; 
;;; patch-region are used by the other patching functions.
;;; 
(defun patch-region (&optional beg end type)
  "Extract region and create a lisp patch file.  Note that its necessary to have a lisp running for this to work."
  (interactive)
  (when (buffer-modified-p)
    (error "This buffer is modifed.  Please save your changes and repatch."))
  
  (save-excursion
    (let* ((end (if end end (region-end)))
	   (beg (if beg beg (region-beginning)))
	   (type (if type type "region"))
	   (process (get-process (or (and (boundp 'fi:common-lisp-buffer-name)
					  fi:common-lisp-buffer-name)
				     "lisp"))) ; punt
	   (tmp-dir (if (eq window-system 'x)
		      "/tmp"
		      "c:/tmp"))
	   (name (file-name-nondirectory (buffer-file-name))))
      (unless process
	(error "No lisp running"))
      
      (let* ((filename (format "%s/%s" tmp-dir name))
	     (packaget (get-package-for-lisp))
	     (system (find-system (buffer-file-name))))
	(message (format "%s" system))
	(cond ((null system)
	       (error "%s is not a member of any system!" (buffer-file-name)))
	      (t
	       (message (format "Patching %s ..." type))
	       ;; Find the name of the thing we're compiling ....
	       ;; First insert the in-package form.
	       (save-excursion
		 (set-buffer (get-buffer-create "*Lisp-utils temp*"))
		 (delete-region (point-min) (point-max))
		 (insert packaget)
		 (write-region (point-min) (point-max) filename nil 'nomessage))
	       ;; Now write the form to a temporary file and compile it.
	       (write-region beg end filename t 'nomessage)
	       (message
		(send-form-to-lisp-process-and-go
		 (format "(idp-env::gnu-patch-form \"%s\" \"%s\" \"%s\" \"%s\")"
			 system
			 type
			 filename
			 (buffer-file-name))))))))))



;;;----------------------------------------------------------------
;;; add-patch
;;; 
;;; 
;;; 
(defun add-patch ()
  "Extract the form under the cursor and create a lisp patch file.  Note that its necessary to have a lisp running for this to work."
  (interactive)
  (save-excursion
    (let* ((end (progn (end-of-defun) (point)))
	   (beg (progn (beginning-of-defun) (point))))
      ;; Find the name of the thing we're compiling ....
      (re-search-forward "[ ]")
      (if (looking-at "(")
	(forward-char 1))
      (let* ((name-start (point))
	     (name-end (forward-sexp))
	     (name (region-to-string name-start name-end)))
	(patch-region beg end name)))))


;;;----------------------------------------------------------------
;;; patch-file
;;; 
;;; 
;;; 
(defun patch-file ()
  "Make current file  a lisp patch file.  Note that its necessary to have a lisp running for this to work."
  (interactive)
  (patch-region (point-min) (point-max) "buffer"))

;;;----------------------------------------------------------------
;;; patch-buffer
;;; 
;;; 
;;; 
(defun patch-buffer ()
  (interactive)
  (patch-region (point-min) (point-max) "buffer"))
