;;;
;;; File:	group-init.el
;;; Author:	Craig R. Steury
;;; Created:	December 31, 1992
;;;
;;; Description:  CDRS customization file for GNU emacs.  Note that this file
;;;               is deliberately kept small since it may be loaded uncompiled.
;;;               
;;; (c) Copyright 1995, Parametric Technology Corporation, all rights reserved.
;;;


;;;----------------------------------------------------------------
;;; *slc-emacs-libraries-root*
;;; 
;;; This is the root-directory location for SLC emacs libraries.  This is
;;; expected to be a list of directories in order of search preference.
;;; If this is not specified it will be set to:
;;;  ($WORK_SRC/cm/emacs $SYS_SRC/cm/emacs)
;;;
;;; If it is set by a developer, then is is assumed that it is a directory
;;; or list of directories and it will *overrides* the default locations.
;;; 
(defvar *slc-emacs-libraries-root* nil)

(defvar *default-library-root* (if (equal window-system 'x)
				 "/sst/trunk-src/cm/emacs"
				 "t:/trunk-src/cm/emacs"))

;; 
;; Setup load-path to be *slc-emacs-libraries-root* if it has been set in the
;; developers environment.  This effectively overrides the default settings of
;; $WORK_BIN/cm/emacs and $SYS_BIN/cm/emacs.  
;;
(cond (*slc-emacs-libraries-root*
       (if (atom *slc-emacs-libraries-root*)
	 (setq *slc-emacs-libraries-root* (list *slc-emacs-libraries-root*))))
      ((and (getenv "WORK_BIN")
	    (getenv "SYS_BIN")
	    (getenv "WORK_SRC")
	    (getenv "SYS_SRC"))
       (setq *slc-emacs-libraries-root*
	     (list (substitute-in-file-name "$WORK_BIN/cm/emacs")
		   (substitute-in-file-name "$SYS_BIN/cm/emacs")
		   (substitute-in-file-name "$WORK_SRC/cm/emacs")
		   (substitute-in-file-name "$SYS_SRC/cm/emacs"))))

      ;; Ok, just hardwire it to the system-area
      (t
       (setq *slc-emacs-libraries-root* (list *default-library-root*))))
       


(setq load-path (append *slc-emacs-libraries-root* load-path))

;;; Load compiled emacs libraries.
(load-library "group-init-other")
