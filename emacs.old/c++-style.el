;;;
;;; File:	c++-style.el
;;; Author:	Robert Mecklenburg
;;; Created:	April 10, 1997
;;; 
;;; Description: Set the default C++ indentation style.
;;;               
;;; (c) Copyright 1997, Parametric Technology Corporation, all rights reserved.
;;;

;;; Set the standard indentation style.
(add-hook 'c-mode-common-hook
	  '(lambda ()
	    (c-set-style "gnu")
	    (c-set-offset 'substatement-open 0)
	    (c-set-offset 'brace-list-open 0)
	    (c-set-offset 'innamespace 0)
			;(c-set-offset 'arglist-cont-nonempty '(c-lineup-gcc-asm-reg +))
      ;(c-set-offset 'arglist-intro '+)
	    (setq c-hanging-comment-ender-p nil)))

