;;;
;;; File:	franz-init.el
;;; Author:	Craig R. Steury
;;; Created:	August 04, 1998
;;; 
;;; Description: 
;;;               
;;; (c) Copyright 1998, Parametric Technology Corporation, all rights reserved.
;;;

(require 'lisp-util)

;;;----------------------------------------------------------------
;;; franz-root
;;; 
;;; This is the root directory of the Franz Allegro lisp distribution.
;;; Set this in your .emacs file if you want something different than
;;; this.
;;; 
(defvar franz-root (if (member window-system '(win32 w32))
		     "C:/Program Files/acl432"
		     "/cdrs/lisp/franz/$MACHINE/latest/home/emacs/fi"))
  

;;; Add support for Franz emacs interface
;;;
;;; In your own .emacs set:
;;;
;;; (setq franz-root "C:/Program Files/acl432")
;;;
;;; NT-version.
;;; 
;;; You probably also want the following if you build your own lisp.
;;; By default we point to $SYS_BIN/bin to pick up cdrs.dxl
;;;
;;; This is for emacs 19.
;;; (setq fi:common-lisp-image-name
;;;       (cons "c:/Progra~1/acl432/lisp.exe"
;;;              (substitute-in-file-name "$CDRS/bin/cdrs.dxl")))
;;;
;;; In emacs 20 there are now two variables:
;;; (setq fi:common-lisp-image-name "c:/Progra~1/acl432/lisp.exe")
;;; (setq fi:common-lisp-image-file (substitute-in-file-name "$CDRS/bin/cdrs.dxl"))
;;;              
;;; Unix version
;;; (setq fi:common-lisp-image-name "$CDRS/bin/cdrs")
;;; 
(cond ((and franz-root
	    (file-exists-p (substitute-in-file-name franz-root))
	    (member window-system '(win32 w32)))
       (setq load-path
	     (cons (concat franz-root "/eli") load-path))
       (unless (and (boundp 'fi:common-lisp-directory)
		    fi:common-lisp-directory)
	 (setq fi:common-lisp-directory
	   (cond ((and (getenv "WORK_SRC")
		       (file-exists-p (substitute-in-file-name "$WORK_SRC")))
		  (substitute-in-file-name "$WORK_SRC/"))
		 ((and (getenv "SYS_SRC")
		       (file-exists-p (substitute-in-file-name "$SYS_SRC")))
		  (substitute-in-file-name "$SYS_SRC/"))
		 (t
		  "c:/tmp"))))

       ;; Pick up system version of lisp. 
       (unless (or (and (boundp 'fi:common-lisp-image-name)
			fi:common-lisp-image-name)
		   (not (getenv "SYS_BIN")))
	 (setq fi:common-lisp-image-name
	       (cons "c:/Progra~1/acl432/lisp.exe"
		     (substitute-in-file-name "$SYS_BIN/bin/cdrs.dxl")))))
	 
      ((and franz-root (equal window-system 'x))
       (unless (or (and (boundp 'fi:common-lisp-image-name)
			fi:common-lisp-image-name)
		   (not (getenv "CDRS")))
	 (setq fi:common-lisp-image-name
	   (substitute-in-file-name "$CDRS/bin/cdrs")))
       (message "Franz-emacs interface not fully debugged for unix ...")
       (sleep-for 2)
       (setq load-path (cons (substitute-in-file-name franz-root) load-path)))
			
      (franz-root
       (message "Check your .emacs setting of franz-root, it doesn't seem to exist.")
       (sleep-for 3)))


;;; This needs to come before fi-site-init.el
(setq fi:find-tag-lock nil)

;;; Allow developers to override this one.
(unless (and (boundp 'fi:check-unbalanced-parentheses-when-saving)
	     fi:check-unbalanced-parentheses-when-saving)
  (setq fi:check-unbalanced-parentheses-when-saving nil))


;;; Load Franz defined emacs-libraries.
(load-library "fi-site-init")



;;; Set default inferior-lisp values.
(setq	fi:common-lisp-buffer-name "*lisp*"
	fi:common-lisp-image-arguments  nil
	fi:common-lisp-host            nil)



;;;----------------------------------------------------------------
;;; run-lisp
;;; 
;;; Provides a stream-lined interface to Franz-emacs inferior-lisp
;;; interface.  Two things I didn't like about their fi:common-lisp
;;; command were:
;;;  1. Answering too many questions I don't really care about.
;;;  2. It remembering the answers to questions I *do* care about
;;;     and want to change the answers to!
;;;
;;;     
(defun run-lisp (&optional args)
  (interactive "sEnter lisp name: ")
  (setq fi:common-lisp-image-arguments  nil
	fi:common-lisp-first-time t)
	
  (when args
    (let* ((nargs (make-list-from-string args))
	   (name (car nargs))
	   (command-line-args (if (cdr nargs)
				(setq fi:common-lisp-image-arguments
				  `("--" ,@(cdr nargs))))))

      ;; Assume if name is supplied that it needs "dxl" appended
      ;; plus a directory component pre-pended.  Get the directory
      ;; component from fi:common-lisp-image-name 
      (cond ((member window-system '(win32 w32))
	     (setq name (concat (file-name-directory
				 (if (= emacs-major-version 19)
				   (cdr fi:common-lisp-image-name)
				   fi:common-lisp-image-file))
				name
				".dxl"))
	     (cond ((file-exists-p (substitute-in-file-name name))
		    (fi:common-lisp fi:common-lisp-buffer-name
				    fi:common-lisp-directory
				    (if (= emacs-major-version 19)
				      (car fi:common-lisp-image-name)
				      fi:common-lisp-image-name)
				    command-line-args
				    fi:common-lisp-host
				    name))
		    (t (error (format "%s does not exist!" name)))))
	     
	    ((equal window-system 'x)
	     (cond ((file-exists-p (substitute-in-file-name fi:common-lisp-image-name))
		    (fi:common-lisp fi:common-lisp-buffer-name
				    fi:common-lisp-directory
				    fi:common-lisp-image-name
				    command-line-args
				    fi:common-lisp-host))
		   (t  (error (format "%s does not exist!" fi:common-lisp-image-name)))))
	    (t
	     (error "Unknown window-system"))))))

	
;;;----------------------------------------------------------------
;;; ptc-lisp-hooks
;;; 
;;; Fix up lisp-mode indentation.  In particular get rid of screwy
;;; emacs-lisp indentation of 'if' forms.  This was setup in the
;;; library lisp-style.  This functions copies the values defined
;;; there into the indent-hooks defined for the Franz emacs-modes.  
;;;
;;; Also get rid of annoying lisp-mode auto-fill.
;;; 
(defun ptc-lisp-hooks ()
  (interactive)
  (auto-fill-mode -1)
  (setq lisp-indent-function 'fi:lisp-indent-hook)

  (dolist (i *customized-indent-forms*)
    (put (car i) 'fi:lisp-indent-hook (cdr i)))

  ;; Override these mappings - they're much better.
  (setf (symbol-function 'fi:comment-region) 'comment-region)
  )


(defun update-menubar ()
  (require 'fi-patch))

(add-hook 'fi:emacs-lisp-mode-hook 'ptc-lisp-hooks)
(add-hook 'fi:common-lisp-mode-hook 'ptc-lisp-hooks)
(add-hook 'fi:common-lisp-mode-hook 'update-menubar)

;;;----------------------------------------------------------------
;;; find-unbalanced-parens
;;; 
;;; More convenient paren balancing check
;;; 
(defun find-unbalanced-parens ()
  (interactive)
  (fi:find-unbalanced-parenthesis))


;;;----------------------------------------------------------------
;;; send-form-to-lisp-process
;;; 
;;; Evaluate form in inferior-lisp process.
;;; 
(defun send-form-to-lisp-process (form &optional and-go)
  (let ((fi::lisp-case-mode ':unknown))
    (fi:eval-in-lisp form)))


;;;----------------------------------------------------------------
;;; send-form-to-lisp-process-and-go
;;; 
;;; Currently a no-op in Franz -
;;; 
(defun send-form-to-lisp-process-and-go (form)
  (send-form-to-lisp-process form))

;;; This doesn't work for various reasons
;;(define-key fi:lisp-listener-mode-map   [M-S-mouse-1] 'x-mouse-extract-object)





