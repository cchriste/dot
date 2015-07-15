;;;
;;; File:         lisp-util.el
;;; Author:       Craig R. Steury
;;; Created:      December 30, 1992
;;; Description:  Utility functions for GNU.  Some of these probably belong
;;;               in a more common-lisp specific file.
;;;               
;;; RCS: $Id: lisp-util.el,v 1.3 1998/09/22 15:59:43 csteury Exp $
;;;               
;;;  (c) Copyright 1992, Evans & Sutherland, all rights reserved.
;;;

(require 'cl)
(provide 'lisp-util)

(defvar *process-packet-size* 80)
(defvar *What-System-Buffer* "*what-system*")

;;; Handy-dandy string-splitter borrowed from Franz gnu emacs interface.  Basic idea
;;; is to break strings into chunks so GNU doesn't choke.  

(defun send-string-split (process string &optional nl-cr)
  "Send string to process in small pieces using send-string."
  (interactive "sSend (to process): \nsSend to process in pieces (string): ")
  (setq string (concat string "\n"))
  (let ((size (length string))
	(filtered-string
	 (if nl-cr
	   (substitute-chars-in-string '((?\n . ?\r)) string)
	   string))
	(start 0))
    (while (and (> size 0)
		(condition-case nil
		    (progn
		      (process-send-string
		       process
		       (substring filtered-string
				  start
				  (+ start
				     (min size
					  *process-packet-size*))))
		      t)
		  (error
		   (message "Error writing to subprocess.")
		   nil)))
      (setq size (- size *process-packet-size*))
      (setq start (+ start *process-packet-size*)))))

(defun substitute-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		 (if (setq pair (assq char char-assoc-list))
		     (char-to-string (cdr pair))
		   (char-to-string char)))
	       string
	       nil)))

;;; Substitute substrings within a string ...
(defun substitute-pattern-in-string (string pat1 pat2)
  "Substitute occurrances of substring pat1 with substring pat2 in string"
  (if (string-match pat1 string)
    (concat pat2 (substring string (match-end 0)))
    string))


(defun string-substitute (a b string)
  "Substitute all occurances of character 'b with character 'a"
  (let ((len (length string))
	(x 0))
    (while (< x len)
      (if (char-equal (aref string x) b)
	(setf (aref string x) a))
      (incf x))
    string))


(defun get-package-for-lisp ()
  (let ((pack (or (and (boundp 'fi:package)
		       fi:package)
		  (buffer-current-package))))
    (if pack
      (format "(in-package '%s)\n" pack)
      "(in-package (package-name *package*))\n")))

;;;----------------------------------------------------------------
;;; buffer-package
;;; 
;;; 
;;; 
(defvar buffer-package nil)


;;;----------------------------------------------------------------
;;; normalize-package-name
;;; 
;;; 
;;; 
(defun normalize-package-name (package &optional nickname-p)
  "Given a name string PACKAGE, return its full name.
   But return its nickname if optional NICKNAME-P is set."
  (and package
       (string-match "\"" package)
       (setq package nil))
  (and package
       (string-match "[^\n ]+" package)
       (setq package (substring package (match-beginning 0) (match-end 0))))
  package)
 

;;;----------------------------------------------------------------
;;; buffer-current-package
;;; 
;;; This was the routine contained in buf-pack.el.  Its advantage is that it doesn't
;;; require a running lisp to get a lisp-file's package.
;;; 
(defun buffer-current-package (&optional no-errorp)
  "Return the package for this buffer.  The package name is a string.  If there is none, return NIL.
   This caches the package, so calling this more than once is cheap."

  ;; return buffer-package if already computed
  (cond ((and buffer-package (not (eq 'not-yet-computed buffer-package)))
	 buffer-package)		
	(t
	 (let (in-package-line)
	   ;; go search for the first in-package in the current buffer
	   (save-excursion
	     (setq buffer-package
		   (progn
		     (goto-char (point-min))
		     (if (null (re-search-forward "^.*(in-package *[':\"]" nil t))
		       (if no-errorp nil (error "The current buffer must have an IN-PACKAGE."))
		       ;; now grab the next sexp and that is the current package
		       (buffer-substring (point) (progn (forward-sexp 1) (point))))))
	     (when buffer-package
	       (setq in-package-line
		     (progn
		       (beginning-of-line)
		       (buffer-substring (point) (progn (forward-sexp 1) (point)))))
	       ;; normalize the package name
	       (setq buffer-package
		     (or (normalize-package-name buffer-package t)
			 buffer-package))
	       (setq mode-line-buffer-identification
		     (new-package-name-for-mode-line (car mode-line-buffer-identification)
						     (upcase buffer-package))))))
	 buffer-package)))

;;;----------------------------------------------------------------
;;; new-package-name-for-mode-line
;;; 
;;; 
;;; 
(defun new-package-name-for-mode-line (string package)
  (list (concat
         (let ((start (string-match "  Package: " string)))
           (if start
             (substring string 0 start)
             string))
         "  Package: "
         package)))

;;;----------------------------------------------------------------
;;; region-to-string
;;; 
;;; 
;;; 
(defun region-to-string (&optional start stop)
  "Convert a region to a string."
  ;; in case one or both of the args was defaulted
  (when (not start)
    (setq start (point))
    (setq stop (mark)))
  (if (not stop)
    (setq stop (if (= start (point)) (mark) (point))))
  (buffer-substring (min start stop) (max start stop)))

;;; This will allow execution of batch emacs commands by effectively
;;; disabling the prompt-checking stuff.
(defvar *batch-mode* nil)

;;;----------------------------------------------------------------
;;; lisp-waiting-at-prompt-p
;;; 
;;; 
;;; 
(defun lisp-waiting-at-prompt-p ()
  (if (not (and (get-buffer "*lisp*")
		(eq 'run (process-status "lisp"))))
    nil
    (if *batch-mode* t
	(save-excursion
	  (set-buffer "*lisp*")
	  (let ((answer t)
		(current-location (point)))
	    (goto-char (point-max))
	    ;; Look for the first non-blank character from the end of buffer.
	    (if (not (re-search-backward "[^\b\n\t]" (- (point) 250) t)) 
	      (setq answer nil)
	      (progn
		(backward-char 1)
		(if (looking-at ">")
		  (setq answer t)
		  (progn
		    (beginning-of-line)
		    (if (search-forward "> " (+ (point) 10) t)
		      (setq answer t)
		      (setq answer nil))))))
	    (goto-char current-location)
	    answer)))))


;;;----------------------------------------------------------------
;;; make-list-from-string
;;; 
;;;  Turn a string with spaces into a list of strings delineated by spaces.
;;;   Given a string such as "a b c" returns ("a" "b" "c")
;;; 
(defun make-list-from-string (string)
  "Break up a string into a list.  Break at white-space"
  (let (acc sacc (index 0) (len (length string)))
    (while (< index len)
      (while (and (< index len)
		  (or (char-equal ? (aref string index))
		      (char-equal ?\t (aref string index))
		      (char-equal ?\n (aref string index))))
	(setq index (1+ index)))
      (setq sacc "")
      (while (and (< index len)
		  (not (char-equal ? (aref string index)))
		  (not (char-equal ?\t (aref string index)))
		  (not (char-equal ?\n (aref string index))))
	(setq sacc (concat sacc (char-to-string (aref string index))))
	(setq index (1+ index)))
      (if (not (string-equal "" sacc))
	(setq acc (cons sacc acc))))
    (reverse acc)))


;;;----------------------------------------------------------------
;;; break-string-at-newlines-and-listify
;;; 
;;; 
;;; 
(defun break-string-at-newlines-and-listify (string)
  "Break up a string into a list.  Break at newlines only. "
  (let (acc sacc (index 0) (len (length string)))
    (while (< index len)
      (while (and (< index len)
		  (or (char-equal ?\n (aref string index))))
	(setq index (1+ index)))
      (setq sacc "")
      (while (and (< index len)
		  (not (char-equal ?\n (aref string index))))
	(setq sacc (concat sacc (char-to-string (aref string index))))
	(setq index (1+ index)))
      (if (not (string-equal "" sacc))
	(setq acc (cons sacc acc))))
    (reverse acc)))


;;;----------------------------------------------------------------
;;; split-string-between-spaces
;;; 
;;; Given a long string, split it up into convenient lines by inserting newlines.
;;; 
(defun split-string-between-spaces (string)
  (let ((cnt 0)
	(lcnt nil)
	(acc nil)
	(start nil)
	(new nil)
	(len (length string))
	(line-length 75))
    (if (string-equal string "")
      string
      (while (< cnt len)
	;; Strip leading blanks.
	(while (and (< cnt len)
		    (char-equal ? (aref string cnt)))
	  (setq cnt (1+ cnt)))
	(setq start cnt)
	(setq lcnt 0)
	(while (and (< lcnt line-length)
		    (< cnt len))
	  (setq cnt (1+ cnt))
	  (setq lcnt (1+ lcnt)))
	(if (or (= cnt len)
		(char-equal ? (aref string cnt)))
	  (setq acc (cons (substring string start cnt) acc))
	  (progn
	    (while (and (< cnt len)
			(not (char-equal ? (aref string cnt))))
	      (setq cnt (1+ cnt)))
	    (setq acc (cons (substring string start cnt) acc)))))
      (if (= (length acc) 1)
	(car acc)
	;; Ok we have multiple lines, so stick in new lines but escape them
	;; for Unix.
	(progn
	  (dolist (l acc)
	    (setq new (concat l "\n" new)))
	  new)))))


(defun remove-craig  (what ls &optional test)
  "Removes item from list using specified test."
  (if (null test)
    (setq test 'eq))
  (remove-aux what ls test))


(defun remove-aux (what ls test)
  (cond ((null ls) nil)
	((funcall test what (car ls))
	 (remove-aux what (cdr ls) test))
	(t (cons (car ls)
		 (remove-aux what (cdr ls) test)))))

(defun remove-string (what ls)
  (remove-craig what ls 'string-equal))


;;; deletes "" from a list.
(defun remove-empty-strings (ls)
  (remove-string "" ls))

(defun string-nil-to-nil (ls)
  (cond ((null ls) nil)
	((string-equal "NIL" (car ls))
	 (cons nil
	       (string-nil-to-nil (cdr ls))))
	(t (cons (car ls)
		 (string-nil-to-nil (cdr ls))))))


(defmacro with-input-from-buffer (buffer &rest body)
  (let ((cb (make-symbol "cb"))
	(result (make-symbol "result")))
    (` (let (((, cb) (current-buffer))
	     (, result))
	 (unwind-protect
	      (save-window-excursion
		(set-buffer (, buffer))
		(setq (, result) (progn (,@ body)))
		(, result))
	   (set-buffer (, cb)))))))


(defun get-clean-buffer (name)
  (let ((cb (current-buffer)))
    (set-buffer (get-buffer-create name))
    (erase-buffer)
    (set-buffer-modified-p nil)
    (set-buffer cb)
    name))

(defun read-file-name-from-buffer (buffer)
  (let (filename)
    (with-input-from-buffer buffer
		      (setq filename (read-line-or-lines-from-user (format " Complete Pathname\n\n")
								   "Choose a file - Should only be one!! "
								   "No full pathname found."))
		      (when filename
			(substitute-in-file-name filename))
		      filename)))

(defun read-system-name-from-buffer (buffer)
  (let (output)
    (with-input-from-buffer buffer
      (setq output (read-line-or-lines-from-user (format " Systems this file belongs to.\n\n")
						 "What system is this file in?  "
						 nil))
      (if (string-equal output "")
	nil
	output))))

(defun read-lock-from-buffer (buffer)
  (with-input-from-buffer buffer
    (read-line-or-lines-from-user (format " Which lock?\n\n")
				  "Enter lock number:  "
				  nil)))

(defun read-line-or-lines-from-user (&optional title prompt error-string)
  (let* ((items (remove-string "NIL"
			       (make-list-from-string (region-to-string (point-min) (point-max)))))
	 (len (length items))
	 (buf "*Tmp*")
	 (response nil))
    (cond ((zerop len) nil)
	  ((= 1 len)
	   (setq response (format "%s" (car items))))
	  (t
	   (with-output-to-temp-buffer buf
	     (princ title)
	     (dolist (s items)
	       (princ (format "    %s\n" s))))
	   (setq response (read-from-minibuffer prompt))
	   (kill-buffer buf)))
    response))



(defun find-defsystem-path (sys)
  (let (path)
    (when sys
      (run-proc "what-system" *What-System-Buffer* nil "-p" sys)
      (setq path (read-file-name-from-buffer *What-System-Buffer*)))
    path))



(defun remove-whitespace (str)
  (if (and (not (zerop (length str)))			    ;C.S. zero-length strings from somewhere?
	   (or (= ?  (aref str 0))
	       (= ?\n (aref str 0))
	       (= ?\t (aref str 0))))
    (remove-whitespace (substring str 1))
    str))


;;; Expand ALL environment variables in string.

(defun full-substitute-in-file-name (name)
  (let (spair)
    (catch 'new-name
      (while (setq spair (substitute-ev-in-string name))
	(if (cdr spair)
	  (setq name (car spair))
	  (throw 'new-name (car spair)))))))

;;; Help function for full-substitute-ev-in-string.
;;; Returns a pair consisting of (string . <environment-variable-found-p>)
;;; The latter is used to terminating continuing ev expansion.

(defun substitute-ev-in-string (name)		
  (let ((match (string-match "\\$[a-zA-Z][a-zA-Z1-9_-]*" name)))
    (if (not match)
      (cons name nil)
      (let* ((mb (match-beginning 0))
	     (me (match-end 0))
	     (ev (getenv (substring name (1+ mb) me))))
	(if (not ev)
	  (cons name nil)
	  (let ((bstring (substring name 0 mb))
		(estring (substring name me)))
	    (cons (concat bstring ev estring) t)))))))


;;;----------------------------------------------------------------
;;; pathname-name
;;; 
;;; 
;;; 
(defun pathname-name (name)
  (let* ((name-part (file-name-nondirectory name))
	 (dot       (string-match "\\\." name-part)))
    (if (and dot (not (zerop dot)))
      (substring name-part 0 dot)
      name-part)))


;;;----------------------------------------------------------------
;;; pathname-type
;;; 
;;; 
;;; 
(defun pathname-type (name)
  (let* ((name-part (file-name-nondirectory name))
	 (dot       (string-match "\\\." name-part)))
    (if (and dot (not (zerop dot)))
      (substring name-part (1+ dot)))))


;;; Convenience function.  Infile of nil means(input from /dev/null),  Otherwise
;;; get input from the specified file.  Its up to the user to make sure the
;;; contents of this file are meaningful.  Also get rid of empty strings as
;;; arguments since this wrecks havoc with 'call-proc.

(defun run-proc (name buffer infile &rest args)
  (setq args (remove-empty-strings args))
  (let ((cb (current-buffer)))
    (set-buffer (get-clean-buffer buffer))
    (apply 'call-process name infile buffer nil args)
    (set-buffer-modified-p nil)
    (set-buffer cb)
    name))


;;;
;;; Given a file of file-names (some possibly commented-out with # or ;)
;;; build a list of double-quoted filenames of the uncommented names.
;;;
(defun build-file-list-from-file (file)
  (let ((wb (get-buffer-create "*Walk Files Buffer*"))
	(tb (get-buffer-create "Tmp Buffer"))
	buf-max
	file-list)
    (set-buffer wb)
    (erase-buffer)
    (set-buffer tb)
    (erase-buffer)
    (insert-file file)
    (setq buf-max (point-max))
    (goto-char (point-min))
    
    (while (/= (point) buf-max)
      (unless (looking-at "^[ ]*[;#]+.*")
	(let ((beg (progn (beginning-of-line)(point)))
	      (end (progn (end-of-line)(point))))
	  (set-buffer wb)
	  (insert "\"")
	  (insert-buffer-substring tb beg end)
	  (insert "\"")
	  (next-line 1)))
      (set-buffer tb)
      (next-line 1)
      (beginning-of-line))
    (set-buffer wb)
    (goto-char (point-max))
    (insert ")")
    (goto-char (point-min))
    (insert "(")
    (backward-char 1)
    (setq file-list (read (get-buffer wb)))
    (kill-buffer wb)
    (kill-buffer tb)
    file-list))


(defconst calendar-month-abbrev-list
  '(("Jan" . "1") ("Feb" . "2")  ("Mar" . "3")  ("Apr" . "4")
    ("May" . "5") ("Jun" . "6")  ("Jul" .  "7")  ("Aug" . "8")
    ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))


(defconst calendar-month-fullname-list
  '(("Jan" . "January") ("Feb" . "February")  ("Mar" . "March")  ("Apr" . "April")
    ("May" . "May") ("Jun" . "June")  ("Jul" .  "July")  ("Aug" . "August")
    ("Sep" . "September") ("Oct" . "October") ("Nov" . "November") ("Dec" . "December")))

(defun month-fullname (abbrev)
  (cdr (assoc abbrev calendar-month-fullname-list)))

(defun time-string ()
  (interactive)
  (let* ((date (current-time-string))
	 (garbage
          (string-match
           "^\\([A-Z][a-z]*\\) *\\([A-Z][a-z]*\\) *\\([0-9]*\\)  *\\([0-9]*:[0-9]*:[0-9]*\\) \\([0-9]*\\)$"
           date))
	 (month (substring date (match-beginning 2) (match-end 2)))
         (day
          (substring date (match-beginning 3) (match-end 3)))
	 (time (string-substitute ?- ?: (substring date (match-beginning 4)(match-end 4))))
         (year
	  (substring date (match-beginning 5) (match-end 5))))
    (concat month "-" day "-" year "-" time)))

