;;;
;;; File:	nt-init.el
;;; Author:	Robert Mecklenburg
;;; Created:	June 23, 1997
;;;
;;; Description:  NT customization file for GNU emacs.
;;;               
;;; (c) Copyright 1997, Parametric Technology Corporation, all rights reserved.
;;;


;;; pcl-cvs, gnats, various utilites.
(setq load-path (append load-path (list "c:/usr/emacs/site-lisp")))

;;;----------------------------------------------------------------
;;; Set the standard subshell.  Before loading this file you should
;;; execute:
;;;
;;; (setq shell-file-name
;;;      "c:/usr/gnuwin32/H-i386-cygwin32/bin/bash.exe")
;;;
;;; To set the full path to your favorite subshell.  The rest of these
;;; variables set argument handling, pruning of ^M, and set the default
;;; file completion character to / instead of \
;;;
(if (equal window-system 'win32)
  (setq win32-quote-process-args t)
  (setq w32-quote-process-args t))
(setq shell-command-switch "-c")
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
(add-hook 'shell-mode-hook
               '(lambda () (setq comint-completion-addsuffix t))
               t)

;;;----------------------------------------------------------------
;;; strip-control-ms
;;;
;;; Remove ^Ms from the entire buffer.
;;;
(defun strip-control-ms ()
  "Delete all ^Ms."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (and (not (eobp))
		(re-search-forward "\C-m" nil t))
      (delete-region (match-beginning 0) (match-end 0)))))

;;;----------------------------------------------------------------
;;; clean-nt-makes
;;;
;;; Strip most warnings from the makes.  These are typically found in
;;; render.  Don't use this to clean dsg makes!
;;;
(defun clean-nt-makes ()
  (interactive)
  (strip-control-ms)
  (kill-all-string "assuming extern returning int")
  (kill-all-string ", possible loss of data")
  (kill-all-string "no return value")
  (kill-all-string "unreferenced local variable")
  (kill-all-string "signed/unsigned mismatch")
  )

(add-hook 'compilation-filter-hook 'strip-control-ms)

;;;----------------------------------------------------------------
;;; Set up mail sending
;;;
;;; In your own .emacs set:
;;; 
;;; (setq user-full-name "Robert Mecklenburg")
;;; (setq user-mail-address "mecklen@ptc.com")
;;; (setq smtpmail-default-smtp-server "phldev1.ptc.com")
;;;
(setq smtpmail-local-domain nil)
(setq send-mail-function 'smtpmail-send-it)
(load-library "smtpmail")

;;;----------------------------------------------------------------
;;; Set up ange ftp mode
;;;
;;; This configures the special ange-ftp mode.  In your own .emacs set:
;;; 
;;; (setq ange-ftp-ftp-program-name "c:/usr/local/bin/ftp.exe")
;;;
(cond ((getenv "TEMP")
       (setq ange-ftp-tmp-name-template 
	     (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
       (setq ange-ftp-gateway-tmp-name-template 
	     (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))))


;;;----------------------------------------------------------------
;;; Set the default font
;;;
;;; This sets the default font for all new frames.  The code assumes you
;;; have set two variables:
;;;
;;; (setq primary-nt-font "-*-6x13-normal-r-*-*-13-97-*-*-c-*-*-ansi-")
;;; (setq secondary-nt-font "-*-Lucida Console-normal-r-*-*-12-90-*-*-c-*-*-ansi-")
;;;
;;; If the primary-nt-font exists it becomes the default font,
;;; otherwise the secondary becomes the default font.
;;;
(if (boundp 'primary-nt-font)
  (if (x-list-fonts primary-nt-font)
    (progn
      (set-default-font primary-nt-font)
      (setq default-frame-alist
	    (list (cons 'font primary-nt-font))))
    (progn
      (set-default-font secondary-nt-font)
      (setq default-frame-alist
	    (list (cons 'font secondary-nt-font))))))


(cond ((not (and (boundp 'cvs-program)
		 cvs-program
		 (file-exists-p cvs-program)))
       (setq cvs-program "c:/usr/bin/cvs.exe")))
