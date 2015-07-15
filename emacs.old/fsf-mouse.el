;;;
;;; File:         fsf-mouse.el
;;; Author:       Craig R. Steury
;;; Created:      July  2, 1996
;;; 
;;; Description:  Use mouse to describe lisp objects and/or extract them
;;;               (ie bring them to bottom of *lisp* buffer).
;;;               mouse-scroll uses 3rd mouse button to scroll up/down
;;;               depending on whether the line clicked-on is already at
;;;               the top of the window.
;;;               
;;;               
;;;  (c) Copyright 1996, Parametric Technology Corporation, all rights reserved.
;;;

(require 'cl)
(require 'lisp-util)


;;;----------------------------------------------------------------
;;; mouse-extract-lisp-object
;;; 
;;; "Grab" object.
;;; 
(defun mouse-extract-lisp-object (arg)
  (interactive "e")
  (let ((cnt 100)
	start-pos
	end-pos
	lisp-string
	object-found)
    (save-excursion
      (while (and (not (zerop cnt))
		  (not (looking-at "\#")))
	(backward-char 1)
	(decf cnt))
      (when (looking-at "\#")
	(setq start-pos (point))
	(forward-char 1)
	(when (looking-at "\<")
	  (setq object-found t) 
	  ;; Search for a package-marker
	  (setq end-pos (progn (re-search-forward ">" (point-max) t) (point)))
	  (setq lisp-string (region-to-string start-pos end-pos)))))

    (cond (object-found
	   (goto-char (point-max))
	   (send-form-to-lisp-process-and-go
	    (concat "(let ((obj (object-from-print-string "
		    "\""
		    lisp-string
		    "\""
		    ")))  obj)"))
	   (message lisp-string))
	  (t
	   (message "Object not located")))))

;;;----------------------------------------------------------------
;;; mouse-describe-lisp-object
;;; 
;;; Grab object and describe it.
;;; 
(defun mouse-describe-lisp-object (arg)
  (interactive "e")
  (let ((cnt 100)
	start-pos
	end-pos
	lisp-string
	object-found)
    (save-excursion
      (while (and (not (zerop cnt))
		  (not (looking-at "\#")))
	(backward-char 1)
	(decf cnt))
      (when (looking-at "\#")
	(setq start-pos (point))
	(forward-char 1)
	(when (looking-at "\<")
	  (setq object-found t)
	  ;; Search for a package-marker
	  (setq end-pos (progn (re-search-forward ">" (point-max) t) (point)))
	  (setq lisp-string (region-to-string start-pos end-pos)))))

    (cond (object-found
	   (goto-char (point-max))
	   (send-form-to-lisp-process-and-go
	    (concat "(let ((obj (object-from-print-string "
		    "\""
		    lisp-string
		    "\""
		    "))) (progn (typecase obj (array (if (< (array-total-size obj) 100)(print (listarray obj))(describe obj)))(t (describe obj))) obj))"))
	   
	   (message lisp-string))
	  (t
	   (message "Object not located")))))


;;;----------------------------------------------------------------
;;; mouse-scroll
;;; 
;;; Use the third-mouse to bring a line to the top of the screen,
;;; scroll-down the top-line, or indent.
;;; 
(defun mouse-scroll (event)
  (interactive "e")
  (dans-mouse-in-window event))

;;;----------------------------------------------------------------
;;; dans-mouse-in-window
;;; 
;;; 
;;; 
(defun dans-mouse-in-window (event)
  (mouse-set-point event)
  (let ((cp (point)))
    (track-mouse event)
    (mouse-set-point event)
    (let ((np (point)))
      (if (= cp np)
        (mouse-scrollup)
        (indent-region cp np nil)))))

;;;----------------------------------------------------------------
;;; find-mouse-line
;;; 
;;; 
;;; 
(defun find-mouse-line()
  (let* ((mp (cdr (mouse-position)))
         (x  (car mp))
         (y  (cdr mp)))
    y))


;;;----------------------------------------------------------------
;;; find-mouse-column
;;; 
;;; 
;;; 
(defun find-mouse-column()
  (let* ((mp (cdr (mouse-position)))
         (x  (car mp))
         (y  (cdr mp)))
    x))

;;;----------------------------------------------------------------
;;; mouse-scrollup
;;; 
;;; 
;;; 
(defun mouse-scrollup ()
  (save-excursion
  (beginning-of-line)
    (if (= (window-start) (window-point))
      (recenter)
      (recenter 0))))

