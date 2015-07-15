;;;
;;; File:         fi-patch.el
;;; Author:       Danny R. Harlin
;;; Created:      February 22, 1995
;;; 
;;; Description:  GNU lisp code to facilitate "patching".
;;;               
;;; RCS: $Id: fi-patch.el,v 1.5 1998/08/06 19:08:51 csteury Exp $
;;;               
;;;  (c) Copyright 1995, Evans & Sutherland, all rights reserved.
;;;


(require 'cl)
(require 'lisp-util)

(provide 'fi-patch)

;;; These functions are used by the other patching functions.

(defvar *patching-system* nil)
(defvar *patching-in-progress* nil)


(defun patching-in-progress-p ()
  (and *patching-in-progress*
       (buffer-file-name)
       (string= "lisp" (pathname-type (buffer-file-name)))))

(defun start-patching-p ()
  (and (not *patching-in-progress*)
       (buffer-file-name)
       (string= "lisp" (pathname-type (buffer-file-name)))))
       
(defun fi-patch-start ()
  (interactive)
  (if (buffer-modified-p)
    (error "This buffer is modifed.  Please save your changes and repatch.")
    (if (null (setq *patching-system* (lookup-cached-system-name (buffer-file-name))))
	(error "%s is not a member of any system!" (buffer-file-name))
      (setq *patching-in-progress* t)
      (save-excursion
	(set-buffer (get-buffer-create "Patch-Region"))
	(erase-buffer))))
  t)

(defun fi-patch-append-region (&optional beg end)
  (interactive)
  (if (buffer-modified-p)
    (error "This buffer is modifed.  Please save your changes and repatch.")
    (let ((end (if end end (region-end)))
	     (beg (if beg beg (region-beginning)))
	     (lisp-package (upcase fi:package))
	   (cbuffer (current-buffer)))
      (save-excursion
	(set-buffer (get-buffer-create "Patch-Region"))
	(goto-char (point-max))
	(insert "(in-package \"") (insert lisp-package) (insert "\")")
	(newline 2)
	(insert-buffer-substring cbuffer beg end)
	(newline 2))))
  t)

(defun fi-patch-finish ()
  (interactive)
  (let ((filename (format "/usr/tmp/%s" (file-name-nondirectory (buffer-file-name)))))
    (save-excursion
      (set-buffer (get-buffer-create "Patch-Region"))
      (write-region (point-min) (point-max) filename nil 'nomessage))
    (let ((fi::lisp-case-mode ':unknown))
      (message "Compiling and creating patch ...")
      (send-form-to-lisp-process (format "(idp-env::gnu-patch-form \"%s\" \"%s\" \"%s\" \"%s\")"
					 *patching-system*
					 "Bogus"
					 filename
					 (buffer-file-name)))			 
      (message "Compiling and creating patch ... Done")))
  (setq *patching-in-progress* nil)
  t)

(defun fi-patch-abort ()
  (interactive)
  (setq *patching-in-progress* nil)
  (save-excursion
    (set-buffer (get-buffer-create "Patch-Region"))
    (erase-buffer))
  (message "Patch aborted ... Done"))

;;;
;;; These functions use the above functions to patch in various ways.
;;;

(defun fi-patch-region (&optional beg end)
  "Extract region and append to patch file."
  (interactive)
  (fi-patch-append-region beg end))

(defun fi-patch-function ()
  "Extract function and append to patch file."
  (interactive)
  (let* ((end (save-excursion (end-of-defun) (point)))
	  (start (save-excursion
		   (fi:beginning-of-defun)
		   (point))))
    (fi-patch-append-region start end)))

(defun fi-patch-buffer-changed-definitions ()
  "Extract buffer's changed definitions and append to patch file."
  (interactive)
  (cond ((fi::check-buffer-for-changes-p 'last-compile-or-eval)
	 (let ((kill-ring nil))
	   (fi::do-buffer-changed-definitions ':copy 'last-compile-or-eval)
	   (save-excursion
	     (sleep-for 3)
	     (set-buffer (get-buffer-create "Patch-Region"))
	     (if kill-ring (yank))))))
  t)


;;;
;;; Load all patches.
;;;

(defun fi-patch-load-all ()
  (interactive)
  (let ((fi::lisp-case-mode ':unknown))
    (message "Loading all patches ...")
    (message
     (send-form-to-lisp-process "(if (idp-env::load-system-patches)
                       \"Loading all patches ... Done\")"))))
  
;;;
;;; Show all patches.
;;;

(defun fi-patch-show-all ()
  (interactive)
  (let ((fi::lisp-case-mode ':unknown))
    (message "Show all patches ...")
    (send-form-to-lisp-process "(idp-env::show-system-patches)")
    (message "Show all patches ... Done")))
  
;;;
;;; The stuff below handles the Platform options.
;;;

(defvar *platforms* (list (cons ':NT t)
			  (cons ':SUN t)
			  (cons ':SGI t)
			  (cons ':HP t)
			  (cons ':IBM t)))

(defun toggle-platform (platform)
  (interactive)
  (setf (cdr (assoc platform *platforms*))
    (not (cdr (assoc platform *platforms*)))))

(defun platform-p (platform)
  (interactive)
  (cdr (assoc platform *platforms*)))

;;;
;;; Menu Bar menus
;;;

(and (intern-soft "emacs-minor-version")
     (not (boundp 'emacs-minor-version))
     (setq emacs-minor-version 10))

(if (>= emacs-minor-version 11)
    (defconst dsg-patching-menu
	'("DSGPatch"
	  ["Start Patch" fi-patch-start (start-patching-p)]
	  ["Finish Patch" fi-patch-finish (patching-in-progress-p)]
	  ["Abort Patch" fi-patch-abort (patching-in-progress-p)]
	  "----"
	  ["Add Region" fi-patch-region (patching-in-progress-p)]
	  ["Add Function" fi-patch-function (patching-in-progress-p)]
	  ["Add Changed Definitions" fi-patch-buffer-changed-definitions
	   (patching-in-progress-p)]
	  "----"
	  ["Load All Patches" fi-patch-load-all t]
	  ["Show All Patches" fi-patch-show-all t]
	  "----"
	  ("Platforms"
	   ["NT" (toggle-platform ':SUN)
	    :style toggle
	    :selected (platform-p ':NT)]
	   ["Sun" (toggle-platform ':SUN)
	    :style toggle
	    :selected (platform-p ':SUN)]
	   ["SGI" (toggle-platform ':SGI)
	    :style toggle
	    :selected (platform-p ':SGI)]
	   ["HP" (toggle-platform ':HP)
	    :style toggle
	    :selected (platform-p ':HP)]
	   ["IBM" (toggle-platform ':IBM)
	    :style toggle
	    :selected (platform-p ':IBM)]
	   ["ESV" (toggle-platform ':ESV)
	    :style toggle
	    :selected (platform-p ':ESV)])
	  ))
  (defconst dsg-patching-menu
      '("DSGPatch"
	["Start Patch" fi-patch-start (start-patching-p)]
	["Finish Patch" fi-patch-finish (patching-in-progress-p)]
	["Abort Patch" fi-patch-abort (patching-in-progress-p)]
	"----"
	["Add Region" fi-patch-region (patching-in-progress-p)]
	["Add Function" fi-patch-function (patching-in-progress-p)]
	["Add Changed Definitions" fi-patch-buffer-changed-definitions
	 (patching-in-progress-p)]
	"----"
	["Load All Patches" fi-patch-load-all t]
	["Show All Patches" fi-patch-show-all t]
	)))

(defun dsg-install-menubar (menu-bar)
  (set-menubar (delete (assoc (car menu-bar) current-menubar)
		       (copy-sequence current-menubar)))
  (add-menu nil (car menu-bar) (cdr menu-bar)))

(dsg-install-menubar dsg-patching-menu)
