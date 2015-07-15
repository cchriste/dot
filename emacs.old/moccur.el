;;; <ctc> Note: this functionality is now replaced by multi-occur[-in-matching-buffers].  It's a little more irritating if you want to search all buffers, but... whatever.

;; -*- Mode: Emacs-Lisp -*-
;; File:	        moccur.el
;; Description: 	multi-buffer occur mode
;; Author:		Markus Freericks <mfx@cs.tu-berlin.de>
;; Last Modified:	01-Aug-1991
;; Version:		1.0
;;

;; ========== Standard Disclaimer ==========
;; This file is not part of the GNU Emacs distribution (yet).

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.  Refer to the GNU Emacs General Public License
;; for full details. You should consider this code to be covered under
;; the terms of the GPL.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights
;; and responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.

;; Motivation
;; moccur is a major mode modelled after the 'Occur' mode of the
;; standard distribution. It is quite nice to use when you need to
;; work with a lot of buffers. 
;;
;; Incompatibilites to Occur mode: 
;; a) it browses through *all* buffers that have a file name
;; associated with them; those may or may not include the current
;; buffer. Especially, while standard occur works 
;; on 'all lines following point', Moccur does not.
;; b) there is no support for the 'NLINE' argument.
;;
;; Usage:
;; moccur <regexp> shows all occurences of <regexp> in all buffers
;; currently existing that refer to files.
;; the occurences are displayed in a buffer running in Moccur mode;
;; C-c C-c gets you to the occurence

(provide 'occur)

(defvar *moccur-buffer-name-exclusion-list* '("TAGS" "*scratch*"))

(defun moccur-excluded-buffer-p (buffer-name excluded-buffer-names)
  (cond ((null excluded-buffer-names) nil)
	((string-equal buffer-name (car excluded-buffer-names)) t)
	(t (moccur-excluded-buffer-p buffer-name (cdr excluded-buffer-names)))))

(defun moccur-filter-buffers (buffer-list)
  (cond ((null buffer-list) nil)
	((moccur-excluded-buffer-p (buffer-name (car buffer-list))
				   *moccur-buffer-name-exclusion-list*)
	 (moccur-filter-buffers (cdr buffer-list)))
	(t (cons (car buffer-list)
		 (moccur-filter-buffers (cdr buffer-list))))))
  

;;;----------------------------------------------------------------
;;; moccur
;;; 
;;; If a prefix arg is supplied only look at buffers which correspond to
;;; files.
;;; 
(defun moccur (regexp &optional file-buffers-only-p)
  "Show all lines of all buffers containing a match for REGEXP.
The lines are shown in a buffer named *Moccur*.
It serves as a menu to find any of the moccurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive "sList lines matching regexp: \nP")
  (if (get-buffer "*Moccur*")					      ; there ought to be just one of these
    (kill-buffer (get-buffer "*Moccur*")))
  (let*  ((buffers (moccur-filter-buffers (buffer-list)))
	  (occbuf (generate-new-buffer "*Moccur*"))
	  (matches 0)
	  (firstmatch t))
    (set-buffer occbuf)
    (insert "Lines matching " regexp "\n")
								      ; (moccur-mode)
    (while buffers
      (if (and file-buffers-only-p
	       (not (buffer-file-name (car buffers))))
	(setq buffers (cdr buffers))
	(let ((currbuf (car buffers)))
	  (setq buffers (cdr buffers))
	  (set-buffer currbuf)
	  (goto-char (point-min))
	  (setq firstmatch t)
	  (while (re-search-forward regexp nil t)
	    (setq matches (+ matches 1))
	    (let* ((linenum (count-lines (point-min)(point)))
		   (tag (format "\n%3d " linenum)))
	      (save-excursion
		(set-buffer occbuf)
		(if firstmatch
		  (progn
		    (insert "Buffer:\t" (buffer-name currbuf) 
			    "\nFile:\t" (if (buffer-file-name currbuf)
					 (buffer-file-name currbuf)
					 "Non-file Buffer")
			    "\n")
		    (setq firstmatch nil)))
		(insert tag))
	      (forward-word -1)					      ; needed if match goes to eoline
	      (beginning-of-line)
	      (let ((beg (point)))
		(end-of-line)
		(append-to-buffer occbuf beg (point)))
	      (forward-line nil)))
	  (save-excursion
	    (set-buffer occbuf)
	    (if (not firstmatch)(insert "\n\n"))))))
      
    (if (> matches 0)
      (save-excursion      
	(set-buffer occbuf)
	(moccur-mode)
	(goto-char (point-min))
	(pop-to-buffer occbuf)      
	(message "%d matches" matches)
	t)
      (progn
	(message "no matches")
	nil))))

(defvar moccur-mode-map ())

(if moccur-mode-map
    ()
    (setq moccur-mode-map (make-sparse-keymap))
    (define-key moccur-mode-map "\C-c\C-c" 'moccur-mode-goto-occurrence))

(defun moccur-mode ()
  "Major mode for output from \\[moccur].
Move point to one of the occurrences in this buffer,
then use \\[moccur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrenc was found in.
\\{occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map moccur-mode-map)
  (setq major-mode 'moccur-mode)
  (setq mode-name "Moccur"))

(defun moccur-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
    (if (not (eq major-mode 'moccur-mode))
        (error "This is no moccur buffer")
	(let ((beg nil)
	      (line nil)
	      (lineno nil)
	      (dstbuf nil))
	  (save-excursion
	    (beginning-of-line 1)
	    (setq beg (point))
            (end-of-line 1)
            (setq line (buffer-substring beg (point)))
            (if (string-match "^[0-9]* " line)
                (progn
                  (setq lineno (car (read-from-string line)))
                  (if (re-search-backward "^Buffer:")
                      (progn
                        (search-forward "\t")
                        (setq beg (point))
                        (search-forward "\n")
                        (setq line (buffer-substring beg (- (point) 1)))
                        (setq dstbuf (get-buffer line))
			(if (not dstbuf)
			    (message "buffer: <%s> doesn't exist anymore" line)))
                      (error "what did you do with the header?!")))
                (error "this is no occurence line!")))
	  (if (and lineno dstbuf)
	      (progn
		(message "selectung <%s> line %d" line lineno)
		(pop-to-buffer dstbuf)
		(goto-line lineno))))))
