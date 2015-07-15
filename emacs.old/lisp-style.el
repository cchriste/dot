;;;
;;; File:	lisp-style.el
;;; Author:	Craig R. Steury
;;; Created:	August 06, 1998
;;; 
;;; Description: Setup lisp indentation for emacs-lisp and common-lisp code.
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;

(require 'cl)
(load-library "cl-indent")

;;;----------------------------------------------------------------
;;; *customized-indent-forms*
;;; 
;;; Keep a list of forms we've customized indentation for.  The
;;; reason for this is that Franz has added their own emacs modes
;;; for both common-lisp and emacs mode.  Rather than copy the
;;; code below for the new mode, we look up the values in the franz
;;; mode and set them from a single location.
;;; 
(defvar *customized-indent-forms* nil)

;;;----------------------------------------------------------------
;;; set-lisp-indentation
;;; 
;;; This is an attempt to setup lisp indentation in one place.
;;; Stash the indentation values away in *customized-indent-forms*
;;; so that we can retrieve them when setting up franz-indentation.
;;; 
(defun set-lisp-indentation (form indentation)
  (let ((found-p (assoc form *customized-indent-forms*)))
    (if found-p
      (setf (cdr found-p) indentation)
      (setq *customized-indent-forms*
	(cons (cons form indentation) *customized-indent-forms*)))
    
    (put form 'common-lisp-indent-function indentation)))
  

;;;----------------------------------------------------------------
;;; ptc-lisp-indent
;;; 
;;; 
(defun ptc-lisp-indent ()
  (interactive)
  (setq lisp-indent-function 'common-lisp-indent-function)
  
  (set-lisp-indentation 'if 1)
  (set-lisp-indentation 'loop 0)
  (set-lisp-indentation 'defgeneric 2)
  ;;(set-lisp-indentation 'defmethod 0)
  (set-lisp-indentation 'with-slots 2)
  (set-lisp-indentation 'with-cancel-box-down 2)
  (set-lisp-indentation 'with-accessors 2)
  (set-lisp-indentation 'with-nd-vectors 2)
  (set-lisp-indentation 'with-static-fixnum-arrays 2)
  (set-lisp-indentation 'with-big-vector 2)
  (set-lisp-indentation 'with-big-vectors-of-pts 2)
  (set-lisp-indentation 'with-big-vectors-of-pts-2 2)
  (set-lisp-indentation 'with-boxed-nr-dmatrices 2)
  (set-lisp-indentation 'with-boxed-nr-dvectors 2)
  (set-lisp-indentation 'handler-case
			'((&whole 6 &rest 1) &rest (&whole 4 (&whole 4 &rest 1) &body)))

  (set-lisp-indentation 'handler-bind
			'((&whole 6 &rest 1) &rest (&whole 4 (&whole 4 &rest 1) &body)))

  (set-lisp-indentation 'restart-case
			'((&whole 6 &rest 1) &rest (&whole 5 (&whole 4 &rest 1) &body)))
  )

;;; Do this once so we're sure this gets executed!
(ptc-lisp-indent)

;;;
;;; Use cl-indent to set our indentation for emacs-lisp and common-lisp.
;;; 
(add-hook 'lisp-mode-hook  'ptc-lisp-indent)	
(add-hook 'emacs-lisp-mode-hook  'ptc-lisp-indent) 
