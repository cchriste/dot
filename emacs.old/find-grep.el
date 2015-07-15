;;;
;;; File:	find-grep.el
;;; Author:	Craig R. Steury
;;; Created:	July 31, 1998
;;; 
;;; Description: find-grep uses 'find' to generate the list of files, which are
;;;              piped through xargs and grep to locate a string.  Its big
;;;              advantage over tag-search is that it can be run asynchronously.
;;;              
;;;              Note:  xargs only works on NT for cygnus version b19 and
;;;              later.
;;;
;;;              
;;;              fgrep-cdrs is syntactic sugar for find-grep.  Sets path to look under
;;;		  $WS/lisp/tk and $WS/lisp/cdrs.  Also looks for files of
;;;		  extension lisp.
;;;
;;;               Also note that the output of this search can be stored in
;;;               a file of extension 'grep' and re-used.
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;

(require 'compile)

;;;----------------------------------------------------------------
;;; find-grep-command
;;; 
;;; 
;;; 
(defvar find-grep-command
 "find $WORK_SRC -name '*.[hcC]*' -print | xargs grep -n ")

;;;----------------------------------------------------------------
;;; find-grep-history
;;; 
;;; 
;;; 
(defvar find-grep-history (list find-grep-command))
      

;;;----------------------------------------------------------------
;;; reset-find-grep
;;; 
;;; 
;;; 
(defun reset-find-grep()
  (interactive )
  (setq find-grep-history (list find-grep-command)))


;;;----------------------------------------------------------------
;;; find-grep
;;; 
;;; 
;;; 
(defun find-grep (command-args)
  "Finds relevant files and then runs grep on those files with ARGS"
  (interactive
   (list (read-from-minibuffer "Run find & grep (like this): "
			       (or (and find-grep-history
					(cons (car find-grep-history) 0))
				   (cons find-grep-command 0))
			       nil
			       nil
			       'find-grep-history)))

  (require 'compile)
  (compile-internal (concat command-args " /dev/null")
		    "No more grep hits" "grep"
		    ;; Give it a simpler regexp to match.
		    nil grep-regexp-alist))

;;;----------------------------------------------------------------
;;; cs-grep-mode
;;; 
;;; Sets current-buffer to have grep-mode.  Its assumed that the file
;;; is meaningful to grep mode!
;;; 
(defun cs-grep-mode ()
  (require 'compile)
  (compilation-mode)
  (setq mode-name "grep"))

;;;----------------------------------------------------------------
;;; fgrep-cdrs-command
;;; 
;;; 
;;; 
(defvar fgrep-cdrs-command
 "find $WORK_SRC/lisp/tk $WORK_SRC/lisp/cdrs -name '*.lisp' -print | xargs grep -n ")

;;;----------------------------------------------------------------
;;; fgrep-cdrs-history
;;;
;;; 
(defvar fgrep-cdrs-history (list fgrep-cdrs-command))
      

;;;----------------------------------------------------------------
;;; reset-fgrep-cdrs
;;; 
;;; 
;;; 
(defun reset-fgrep-cdrs()
  (interactive )
  (setq fgrep-cdrs-history (list fgrep-cdrs-command)))

;;;----------------------------------------------------------------
;;; fgrep-cdrs
;;; 
;;; 
;;; 
(defun fgrep-cdrs (command-args)
  "Finds relevant files and then runs grep on those files with ARGS"
  (interactive
   (list (read-from-minibuffer "Run find & grep (like this): "
			       (or (and fgrep-cdrs-history
					(cons (car fgrep-cdrs-history) 0))
				   (cons fgrep-cdrs-command 0))
			       nil
			       nil
			       'fgrep-cdrs-history)))

  (require 'compile)
  (compile-internal (concat command-args " /dev/null")
		    "No more grep hits" "grep"
		    ;; Give it a simpler regexp to match.
		    nil grep-regexp-alist))
