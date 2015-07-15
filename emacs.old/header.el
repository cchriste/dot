;;;
;;; File:         header.el
;;; Author:       Craig R. Steury
;;; Created:      December 16, 1992
;;; Description:  make-header builds a header for various types of
;;;               files based on the editing-mode of the file.
;;;               
;;;  (c) Copyright 1992, Evans & Sutherland, all rights reserved.
;;;

(require 'lisp-util)

(defvar *author* "Boris Yeltsin")
(defvar *copyright* "all rights reserved.")

;;;----------------------------------------------------------------
;;; make-header
;;; 
(defun make-header ()
  "Insert a mode-dependent file header at the top of the current buffer"
  (interactive)
  (beginning-of-buffer)
  (insert-header)
  (beginning-of-buffer)
  ;; Need to make sure there is a file-name associated with the buffer.
  (cond
    ((null (buffer-file-name))
     (beep)
     (beep)
     (write-file nil)))
  (search-forward "File:")
  (end-of-line)
  (insert-string (file-name-nondirectory (buffer-file-name)))
  (search-forward "Author:")
  (end-of-line)
  (insert-string *author*)
  (search-forward "Created:")
  (end-of-line)
  (insert-date-string t)
  (beginning-of-buffer)
  (search-forward "(c) Copyright")
  (end-of-line)
  (insert-copyright-notice)
  (beginning-of-buffer)
  (search-forward "Description:")
  (end-of-line)
  )

;;;----------------------------------------------------------------
;;; insert-header
;;; 
(defun insert-header ()
  (cond ((or (eq major-mode 'lisp-mode)
	     (eq major-mode 'fi:common-lisp-mode))
	 (insert-lisp-header))
	((or (eq major-mode 'emacs-lisp-mode)
	     (eq major-mode 'fi:emacs-lisp-mode))
	 (insert-lisp-header))
	((eq major-mode 'perl-mode)
	 (insert-perl-header))
	((eq major-mode 'c-mode)
	 (insert-C-header))
	((eq major-mode 'c++-mode)
	 (insert-C-header))
	(t
	 (insert-cs-shell-header))))

;;;----------------------------------------------------------------
;;; insert-date-string
;;; 
(defun insert-copyright-notice ()
  (let* ((time-string (current-time-string))
	 (time-string-length (length time-string)))
	(insert-string (substring time-string
				  (- time-string-length 4)
				  time-string-length) ", ")
	(insert-string *copyright*)))
  
(defun insert-date-string (&optional fullname)
  "Insert a date string of the form dd-mon-yr into the buffer at the point"
  (let* ((time-string (current-time-string))
	 (time-string-length (length time-string)))
    (insert-string (if fullname
		     (month-fullname (substring time-string 4 7))
		     (substring time-string 4 7)) " ")    ;; Put in the Month.
    (insert-string (substring time-string 8 10) ", ")   ;; Put in the Day.
    (insert-string (substring time-string
			      (- time-string-length 4) ;; Put in the Year.
			      time-string-length))))

;;;----------------------------------------------------------------
;;; insert-perl-header
;;; 
(defun insert-perl-header ()
  (insert-before-markers
   "#!/usr/dsg/bin/perl\n"
   "#\n")
  (insert-cs-shell-header))

;;;----------------------------------------------------------------
;;; insert-lisp-header
;;; 
(defun insert-lisp-header ()
  (insert-before-markers
   ";;;\n"
   ";;; File:\t\n"
   ";;; Author:\t\n"
   ";;; Created:\t\n"
   (if (or (eq major-mode 'lisp-mode)
	   (eq major-mode 'fi:common-lisp-mode))
     ";;; Package:\tCDRS\n"
     "")
   ";;; \n"
   ";;; Description: \n"
   ";;;               \n"
   ";;; (c) Copyright \n"
   ";;;\n\n"))

;;;----------------------------------------------------------------
;;; insert-C-header
;;; 
(defun insert-C-header ()
  (insert-before-markers
   "/*\n"
   " * File:        \n"
   " * Author:      \n"
   " * Created:     \n"
   " * \n"
   " * Description: \n"
   " *              \n"
   " * (c) Copyright \n"
   " */\n\n"))

;;;----------------------------------------------------------------
;;; insert-cs-shell-header
;;; 
(defun insert-cs-shell-header ()
  (insert-before-markers
   "#\n"
   "# File:        \n"
   "# Author:      \n"
   "# Created:     \n"
   "# \n"
   "# Description: \n"
   "#              \n"
   "# (c) Copyright \n"
   "#\n\n"))


;;;----------------------------------------------------------------
;;; make-function-header
;;; 
;;; 
;;; 
(defun make-function-header (name)
"Insert a standard function header comment before point.  Leave the cursor 
in the comment body."
  (interactive (list (grab-c++-function-name)))
  ;; Find a blank line
  (while (not (looking-at "^\\s-*$"))
    (backward-char 1))

  ;; Replace "extra" white-space with just a newline so header looks correct.
  (replace-match "\n")
  (forward-char 1)

  (if (or (equal major-mode 'lisp-mode)
	  (equal major-mode 'fi:common-lisp-mode)
	  (equal major-mode 'emacs-lisp-mode)
	  (equal major-mode 'fi:emacs-lisp-mode))
    (insert-string
     ";;;----------------------------------------------------------------\n"
     ";;; " name "\n"
     ";;; \n"
     ";;; \n"
     ";;; \n")
    (insert-string
     "//----------------------------------------------------------------\n"
     "// " name "\n"
     "// \n"
     "// \n"))
;;     (insert-string
;;      "/*----------------------------------------------------------------\n"
;;      " * " name "\n"
;;      " * \n"
;;      " * \n"
;;      " */\n"))
  (forward-line -2)
  (end-of-line))

;;;----------------------------------------------------------------
;;; grab-c++-function-name
;;; 
(defun grab-c++-function-name ()
"Prompt for a C++ function name.  Assuming the cursor is on or in a C++
function name grab the name under the cursor and offer that word as the
default.  This is not all that easy since C++ function names can contain
spaces and plenty of punctuation, so we just do our best." 
  (save-excursion
    (let* ((id "[-a-zA-Z0-9_~!%^&*+=|:<>,/]")
	   (start (progn (while (looking-at id)
			   (backward-char 1))
			 (forward-char 1)
			 (point)))
	   (word (progn (while (looking-at id)
			  (forward-char 1))
			(buffer-substring start (point))))
	   (fun (read-string (format "Function name (default %s): " word))))
      (if (equal fun "")
	  word
	fun))))

;;;----------------------------------------------------------------
;;; insert-include-guard
;;; 
;;; Insert
;;;	#ifndef HEADER_FILE_HXX_
;;;	#define HEADER_FILE_HXX_
;;;	#endif /* HEADER_FILE_HXX_ */
;;; 
(defun insert-include-guard ()
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name)))
	(here (point)))
    (setq name (concat (upcase name) "_"))
    (save-restriction
      (insert "#ifndef " name "\n"
	      "#define " name "\n\n"
	      "#endif /* " name " */\n")
      (narrow-to-region here (point))
      (beginning-of-buffer)
      (while (search-forward "." nil t)
	(replace-match "_" nil t)))
    (forward-line -1)
    ))

;;;----------------------------------------------------------------
;;; make-new-header
;;; 
;;; Call make-header and insert-include-guard on the current buffer.
;;;
(defun make-new-header ()
  (interactive)
  (make-header)
  (end-of-buffer)
  (insert-include-guard)
  (forward-line -7)
  (end-of-line))

;;;----------------------------------------------------------------
;;; make-new-class
;;; 
;;; Insert a basic class declaration at point.  Prompt for the class
;;; name.
;;; 
(defun make-new-class (class-name)
  (interactive "sClass name: ")
  (insert
   "/*----------------------------------------------------------------\n"
   " * " class-name "\n"
   " * \n"
   " * \n"
   " */\n"
   "class " class-name "\n"
   "{\n"
   "public:\n"
   "  " class-name "();\n"
   "  ~" class-name "();\n"
   "\n"
   "private:\n"
   "  // Cannot be called.\n"
   "  " class-name "(const " class-name " &);\n"
   "  " class-name " & operator=(const " class-name " &);\n"
   "};\n"
  )
  (forward-line -13)
  (end-of-line))
