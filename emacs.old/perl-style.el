;;;
;;; File:	perl-style.el
;;; Author:	Craig R. Steury
;;; Created:	August 07, 1998
;;; 
;;; Description: 
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;


;;; Set perl-mode variables
(add-hook 'perl-mode-hook
	  `(lambda ()
	    (setq perl-tab-to-comment t)
	    (setq perl-indent-level 2)
	    (setq perl-continued-statement-offset 2)
	    (setq perl-continued-brace-offset -2)
	    (setq perl-brace-offset 0)
	    (setq perl-brace-imaginary-offset 0)
	    (setq perl-label-offset -2)))
