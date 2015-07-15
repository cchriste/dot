;;;
;;; File:	group-init-other.el
;;; Author:	Craig R. Steury
;;; Created:	August 04, 1998
;;; 
;;; Description: 
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;

(require 'cl)

;;; We override the built-in version of this function so
;;; get rid of it.
(fmakunbound 'run-lisp)

;;;
;;; Setup emacs style indentation, etc.
;;; 
(load-library "c++-style")
(load-library "lisp-style")
(load-library "perl-style")

;;(setq require-final-newline t)
(setq cvs-inhibit-copyright-message t)

(defun emacs-version-19-p ()
  (= (string-to-int emacs-version) 19))

;;;
;;; Loading/autoloading of GNU libraries...
;;; 
(autoload 'make-header "header"  "Mode dependent header generator" t)
(autoload 'make-function-header "header"  "Function header generator" t)
(autoload 'fgrep-cdrs "find-grep" "Search CDRS source files for string." t) 
(autoload 'find-grep "find-grep" "Pipe find into grep using xargs." t)
(autoload 'cs-grep-mode "find-grep" "Put file with ext .grep in grep mode." t)
(autoload 'make-new-header "header"  "Insert header and include guard" t)
(autoload 'make-new-class "header"  "Insert a C++ class declaration" t)

(autoload 'cvs-update "pcl-cvs" "Run a 'cvs update' from emacs." t)
(autoload 'cvs-examine "pcl-cvs" "Run a 'cvs update' from emacs." t)
(autoload 'moccur "moccur" "Do an occur across multiple buffers." t)

(autoload 'run-lisp "lisp-init" "Run inferior-lisp process." t)
(autoload 'fi:common-lisp "lisp-init" "Run inferior-lisp process." t)
(autoload 'mouse-scroll "fsf-mouse" "Added mouse functionality." t)
(autoload 'rmime-format "rmime" "" nil)

;; Put files of extension ".grep" in cs-grep-mode.
(setq auto-mode-alist
      (append '(("\\.grep"  . cs-grep-mode))
	      auto-mode-alist))

;;;
;;; Load NT specific initialization.
;;; 
(when (member window-system '(win32 w32))
  (load-library "nt-init")) 


