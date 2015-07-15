;;; clearcase.el --- 

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Cameron Christensen <cameronc@microsoft.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

;;;
;; Clearcase stuff
;;;

;; load clearcase setup code
;; (add-to-list 'load-path (substitute-in-file-name "$HOME/emacs"))
(add-to-list 'load-path "t:/r24src/cm/emacs")
(load "clearcase")

; define helper functions for checking out the current version of a file:
(defvar proc-stdout-txt "")

(defun proc-stdout (proc txt)
    (message (concat "txt: " txt))
       (setq proc-stdout-txt (concat proc-stdout-txt txt)))

(defun clearcase-get-file-cur-version (file)
    (let ((proc (start-process "ff_cleartool" nil "cleartool" "ls" file)))
      (set-process-filter proc 'proc-stdout)
      (accept-process-output proc); get the output
      (process-send-eof "ff_cleartool")
      (delete-process "ff_cleartool")
      )
    (let ((re0 "\\(@@\\)")
	  (re1 "\\( \\)")
	  (start 0)
	  (end   0))
      
      (if (string-match re0 proc-stdout-txt)
	  (setq start (match-end 0)))
      (if (string-match re1 proc-stdout-txt)
	  (setq end   (match-beginning 0)))
      (substring proc-stdout-txt start end)))

(defun clearcase-checkout-file-current-version (file)
  (let ((cur-version (clearcase-get-file-cur-version file)))
    (message cur-version)
    (call-process "cleartool" nil nil nil "co" "-nc"
		  clearcase-checkout-switches "-version"
		  (concat (concat file "@@") cur-version))
    (clearcase-sync-from-disk file t)))

(defun clearcase-checkout-current-buffer-current-version ()
  "Checkout current version (based on the timestamp) of the file"
  (interactive)
  (clearcase-checkout-file-current-version buffer-file-name))

; A shortcut for checking out the current buffers current version
; of file:
; CTRL-c-c
;;(global-set-key "\C-cc" 'clearcase-checkout-current-buffer-current-version)

(defun clearcase-fprop-latest-version (file)
  "For FILE, return its \"latest-version\" ClearCase property."
  (let ((re "\\(.*/\\).*")
	(str (clearcase-fprop-version file)))
    (if (string-match re str)
      (concat (substring str (match-beginning 0) (match-end 1))
              "LATEST"))))

(defun clearcase-file-is-in-mvfs-p (filename)
  "Return whether existing FILE, resides in an MVFS filesystem."
  (let ((truename (file-truename filename)))
    (message (concat "truename: " truename))
    (or
     ;; case 1: its prefix matches an "always VOB" prefix like /vobs/...
     ;;
     ;; nyi: problem here: we return true for "/vobs/nonexistent/"
     ;;
     (numberp (string-match clearcase-always-mvfs-regexp truename))

     ;; case 2: it has a prefix which is a known VOB-root
     ;;
     (clearcase-file-matches-vob-root truename clearcase-known-vob-root-cache)

     ;; case 3: it has an ancestor dir which is a newly met VOB-root
     ;;
     (clearcase-file-vob-root truename))))

(defun clearcase-ediff-file-with-version (truename other-version)
  (let ((other-vxpath
	 (clearcase-vxpath-cons-vxpath (clearcase-vxpath-element-part truename) other-version)))
    (message (concat "truename: " truename))
    (message (concat "other version: " other-version))
    (message (concat "other vxpath: " other-vxpath))
    (if (not (clearcase-file-is-in-mvfs-p truename))
	(message "file is NOT in mvfs-p. :~("))
    (if (clearcase-file-is-in-mvfs-p truename)
	(message "file is in mvfs-p!")
      (ediff-files other-vxpath truename)
      (ediff-buffers (clearcase-vxpath-get-version-in-buffer other-vxpath)
                     (find-buffer-visiting truename)))))

(defun clearcase-ediff-latest-current-buffer ()
  "Use Ediff to compare a version in the current buffer against its
predecessor."
  (interactive)
  (clearcase-ediff-file-with-version buffer-file-name
                                     (clearcase-fprop-latest-version
				      buffer-file-name)))

(global-set-key "\C-cd" 'clearcase-ediff-latest-current-buffer)

; A shortcut for checking out/in the current buffer:
; CTRL-c-o
(global-set-key "\C-co" 'clearcase-toggle-read-only)

; A shortcut for diffing the current buffer against predecessor
; CTRL-c-d
(global-set-key "\C-cp" 'clearcase-ediff-pred-current-buffer)

; FreeForm specific environment options:
(setq-default clearcase-suppress-confirm t)
(setq-default clearcase-suppress-checkout-comments t)
(setq-default clearcase-checkout-switches "-unreserved")

;;;
;; End Clearcase stuff
;;;


(provide 'clearcase)
;;; clearcase.el ends here
