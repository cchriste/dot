;;;
;;; File:	lisp-init.el
;;; Author:	Craig R. Steury
;;; Created:	August 04, 1998
;;; 
;;; Description: Initialize emacs-lisp/inferior-lisp interface.
;;;              This file contains code common to both the 'ilisp and
;;;              franz emacs interfaces.  It also loads one of the 2
;;;              interfaces.
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;

(require 'cl)

;;;----------------------------------------------------------------
;;; *emacs-lisp-interface*
;;; 
;;; Set default emacs-lisp interface.  This can be overridden by setting
;;; this variable in .emacs.
;;; 
(defvar *emacs-lisp-interface*
    (if (equal window-system 'x)
      'ilisp
      'franz))


;;; Load generic patching-interface.
(load-library "patch")		      


;;;----------------------------------------------------------------
;;; *defsystem-database-initialized*
;;; 
;;; The first time we try to query lisp for the "system" a lisp file lives
;;; in this will be nil and we'll print a message to the mini-buffer indicating
;;; that things may take a while.
;;; 
(defvar *defsystem-database-initialized* nil)

;;;----------------------------------------------------------------
;;; find-system
;;; 
;;; Get filename's system from inferior lisp -process.
;;; 
(defun find-system (filename)
  (interactive "sEnter a filename: ")
  (unless *defsystem-database-initialized*
    (message "Computing defsystem file database. This will take a few moments.")
    (sleep-for 1)
    (setq *defsystem-database-initialized* t))

  (send-form-to-lisp-process (format "(idp-env:lookup-cached-system-name \"%s\")"
				     filename)))

;;;
;;; Load appropriate emacs/lisp interface library.
;;; 
(if (or (member window-system '(win32 w32))
	(not (equal *emacs-lisp-interface* 'ilisp)))
  (load-library "franz-init")
  (load-library "ilisp-init"))
