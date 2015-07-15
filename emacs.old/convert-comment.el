;;;
;;; File:	convert-comment.el
;;; Author:	Robert Mecklenburg
;;; Created:	October 18, 1999
;;; 
;;; Description: Convert a // comment into a /* */ comment.
;;;               
;;; (c) Copyright 1999, Parametric Technology Corporation, all rights reserved.
;;;

;;;----------------------------------------------------------------
;;; line-is-slash-comment
;;; 
(defun line-is-slash-comment ()
  "Return t if the current line is a // comment."
  (save-excursion
   (beginning-of-line)
   (looking-at "[ ]*//")))

;;;----------------------------------------------------------------
;;; find-comment-start
;;; 
(defun find-comment-start ()
  "If the current line is a // comment, search backwards for the
beginning of the comment.  If the current line is not a // comment,
search forward for the next // comment.  Leave point a the beginning
of the comment line."
  (if (line-is-slash-comment)
    (progn
      (forward-line -1)
      (while (line-is-slash-comment)
	     (forward-line -1))
      (forward-line 1)
      (beginning-of-line)
      (point))
    (progn
      (forward-line 1)
      (while (not (line-is-slash-comment))
	     (forward-line 1)))
    (beginning-of-line)
    (point)))

;;;----------------------------------------------------------------
;;; find-comment-end
;;; 
(defun find-comment-end ()
  "Return the character position of the line following the current comment."
  (save-excursion
   (while (line-is-slash-comment)
	  (forward-line 1))
   (beginning-of-line)
   (point)))

;;;----------------------------------------------------------------
;;; convert-comment
;;; 
(defun convert-comment ()
  "Convert // style c++ comments into /* */ comments.  If point is
within a // comment, convert the current comment.  If point is not
within a // comment, search forward for the next // comment and
convert it."
  (interactive)
  (let ((start (find-comment-start))
	(end (find-comment-end)))
    (insert "/*\n")
    (while (search-forward "//" end t)
	   (replace-match " *" nil t))
    (forward-line 1)
    (beginning-of-line)
    (insert "*/\n")
    (c-indent-region start (point))))
