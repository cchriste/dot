;;;
;;; File:	load-path.el
;;; Author:	Craig R. Steury
;;; Created:	July 31, 1998
;;; 
;;; Description: Load-path used in compiling emacs-lisp code
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;

;;;
;;; Use the current-directory to pick up emacs-libraries.  We also need the
;;; ilisp subdirectory for those files requiring ilisp.
;;; 
;;(setq load-path (cons "d:/src/r20/cm/emacs"  load-path))
(let ((wb (and (getenv "WORK_SRC")
	       (substitute-in-file-name "$WORK_SRC/cm/emacs")))
      (sb (and (getenv "SYS_SRC")
	       (substitute-in-file-name "$SYS_SRC/cm/emacs"))))
  (cond ((and wb
	      sb
	      (file-exists-p wb)
	      (file-exists-p sb))
	 (setq load-path (append (list wb
				       sb
				       (concat wb "/ilisp")
				       (concat sb "/ilisp"))
				 load-path)))
	((and sb
	      wb
	      (file-exists-p sb)
	      (not (file-exists-p wb)))
	 (setq load-path (append (list sb (concat sb "/ilisp"))
				 load-path)))
	((and sb
	      wb
	      (not (file-exists-p sb))
	      (file-exists-p wb))
	 (setq load-path (append (list wb (concat sb "/ilisp"))
				 load-path)))))
