;;;
;;; File:         ilisp-init.el
;;; Author:       Craig R. Steury
;;; Created:      December 30, 1992
;;; Description:  init-files using the ilisp-package and other CDRS
;;;               extensions.
;;;               
;;;               
;;;  (c) Copyright 1992, Evans & Sutherland, all rights reserved.
;;;

;;;
;;; Setup path to pickup ilisp libraries.
;;; 
(let ((wb (and (getenv "WORK_BIN")
	       (substitute-in-file-name "$WORK_BIN/cm/emacs/ilisp")))
      (sb (and (getenv "SYS_BIN")
	       (substitute-in-file-name "$SYS_BIN/cm/emacs/ilisp")))
      (ws (and (getenv "WORK_SRC")
	       (substitute-in-file-name "$WORK_SRC/cm/emacs/ilisp")))
      (ss (and (getenv "SYS_SRC")
	       (substitute-in-file-name "$SYS_SRC/cm/emacs/ilisp"))))
  (cond ((and wb
	      sb
	      (file-exists-p wb)
	      (file-exists-p sb))
	 (setq load-path (append load-path (list wb sb ws ss))))
	((and sb
	      wb
	      (file-exists-p sb)
	      (not (file-exists-p wb)))
	 (setq load-path (append load-path (list sb ss))))
	((and sb
	      wb
	      (not (file-exists-p sb))
	      (file-exists-p wb))
	 (setq load-path (append load-path (list wb))))

	;; Hardwire system-area again
	(t
	 (setq load-path (append load-path (list (concat *default-library-root* "/ilisp")))))))



(require 'cl)
(require 'lisp-util)
(require 'ilisp)

(cond ((string-match "XEmacs" (emacs-version))
       (load-library "xemacs-mouse"))
      (t
       (load-library "fsf-mouse")
       (define-key ilisp-mode-map       [M-S-mouse-1] 'mouse-extract-lisp-object)
       (define-key ilisp-mode-map       [M-S-mouse-2] 'mouse-describe-lisp-object)))



;;;; Autoloads ...
;;; comments 
(define-key lisp-mode-map        "\C-z;" 'comment-region-lisp)
(define-key emacs-lisp-mode-map  "\C-z;" 'comment-region-lisp)


(setq ilisp-site-hook
  '(lambda ()
	(setq lisp-wait-p nil)
	(setq lisp-no-popper t)))

;;;----------------------------------------------------------------
;;; run-lisp
;;; 
;;; 
;;; 
(defun run-lisp (&optional args)
  "Run an inferior Lisp process with command-line arguments."
  (interactive "sEnter lisp name: ")
  (let* ((start-inferior-lisp t)
	 (nargs (make-list-from-string args))
	 (name (car nargs)))
    (cond ((and (getenv "SYS_BIN")
		(file-exists-p (substitute-in-file-name "$SYS_BIN/lisp/tk/low")))
	   (setq ilisp-lisp-binary-support-dir (substitute-in-file-name "$SYS_BIN/lisp/tk/low")))
	  ((and (getenv "WORK_BIN")
		(file-exists-p (substitute-in-file-name "$WORK_BIN/lisp/tk/low")))
	   (setq ilisp-lisp-binary-support-dir (substitute-in-file-name "$WORK_BIN/lisp/tk/low")))
	  (t
	   (if (not (or (getenv "SYS_BIN")(getenv "WORK_BIN")))
	     (message
	      (format "$SYS_BIN or $WORK_BIN are not defined.  %S not started."
		      name))
	     (message
	      (format "Neither $SYS_BIN/lisp/tk/low nor $WORK_BIN/lisp/tk/low exist.  %S not started."
		      name)))
	   (sleep-for 3)
	   (setq start-inferior-lisp nil)))
    (when start-inferior-lisp
      (let* ((dialect-name   (intern (format "setup-%s" name)))
	     (dialect-exists-p (fboundp dialect-name)))
	(unless dialect-exists-p
	  (eval `(defdialect ,(intern name) "Allegro Common LISP" allegro)))
      (setq ilisp-program args)
      (ilisp-start-dialect "lisp" args dialect-name)))))


(defun inferior-lisp-running-p ()
  (and ilisp-buffer
       (get-buffer ilisp-buffer)
       (get-buffer-process (ilisp-buffer))
       (eq (process-status (ilisp-process)) 'run)))

;;; Add these for using tags instead of inferior lisp for searches.
;;(define-key ilisp-mode-map "\C-\M-." 'edit-definitions-lisp)
;;(define-key ilisp-mode-map "\C-\M-," 'next-definition-lisp)

(define-key lisp-mode-map     "\M-." 'find-tag)
(define-key lisp-mode-map     "\M-," 'tags-loop-continue)
(define-key ilisp-mode-map     "\M-." 'find-tag)
(define-key ilisp-mode-map     "\M-," 'tags-loop-continue)
(define-key ilisp-mode-map     "\C-z\C-b" 'compile-buffer-and-go-lisp)

;;;
;; START OF Set Package based on current buffer
;;;

(defvar *ilisp-interrupt-to-change-package* nil
  "When t interrupt inferior-lisp to change its current package")

(defvar *ilisp-update-lisp-package* t
  "When t update inferior-lisp with current buffer's package")

(defun change-package ()
  (and *ilisp-update-lisp-package*
       (or (lisp-waiting-at-prompt-p)
	   *ilisp-interrupt-to-change-package*)
       (inferior-lisp-running-p)
       (ilisp-initialized)
       (lisp-buffer-package)
       (string-equal ilisp-status " :ready")
       (set-package-lisp (lisp-buffer-package))))


;;;----------------------------------------------------------------
;;; find-package-for-lisp
;;; 
;;; 
;;; 
(defun find-package-for-lisp ()
  (let ((package (if (lisp-waiting-at-prompt-p)
		   (if lisp-buffer-package
		     lisp-buffer-package
		     (if (lisp-buffer-package)
		       (lisp-buffer-package)))
		   (buffer-current-package t))))
    (if package
      (upcase package))))

  

(defun Buffer-menu-this-window ()
  "Select this line's buffer in this window. Sets a lisp buffer's package."
  (interactive)
  (switch-to-buffer (Buffer-menu-buffer t))
  (change-package))

;;;
;; END OF Set Package based on current buffer
;;;

(defun compile-buffer-and-go-lisp ()
  "Compile the current buffer and switch to the current ILISP buffer."
  (interactive)
  (compile-region-lisp (point-min) (point-max) t))



;;;----------------------------------------------------------------
;;; send-form-to-lisp-process
;;; 
;;; Evaluate form in inferior-lisp process.
;;; 
(defun send-form-to-lisp-process (form &optional and-go)
  (ilisp-send form "Sending form to common-lisp ..." 'eval and-go nil))

;;;----------------------------------------------------------------
;;; send-form-to-lisp-process-and-go
;;; 
;;; Same thing as above function except go to *lisp* buffer and leave
;;; the result there.
;;; 
(defun send-form-to-lisp-process-and-go (form)
  (send-form-to-lisp-process form t))
