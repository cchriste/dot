;;; font-lock+.el --- Enhancements to standard library `font-lock.el'.
;; 
;; Filename: font-lock+.el
;; Description: Enhancements to standard library `font-lock.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2007-2009, Drew Adams, all rights reserved.
;; Created: Sun Mar 25 15:21:07 2007
;; Version: 22.0
;; Last-Updated: Sat Aug  1 15:26:45 2009 (-0700)
;;           By: dradams
;;     Update #: 73
;; URL: http://www.emacswiki.org/cgi-bin/wiki/font-lock+.el
;; Keywords: languages, faces, highlighting
;; Compatibility: GNU Emacs: 22.x, 23.x
;; 
;; Features that might be required by this library:
;;
;;   `font-lock', `syntax'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Enhancements to standard library `font-lock.el'.
;;
;;  This library tells font lock to ignore any text that has the text
;;  property `font-lock-ignore'.  This means, in particular, that font
;;  lock will not erase or otherwise interfere with highlighting that
;;  you apply using library `highlight.el'.
;;
;;  Load this library after standard library `font-lock.el' (which
;;  should be preloaded).  Put this in your Emacs init file (~/.emacs):
;;
;;    (require 'font-lock+)
;;
;;  
;;  CAVEAT: Be aware that using this library will **slow down**
;;          font-locking.  In particular, unfontifying a large buffer
;;          (e.g. turning off `font-lock-mode') will take noticeably
;;          longer.  Give it a try, and see if the slowdown annoys
;;          you.  It doesn't annoy me, but people are different, and
;;          your use of Emacs might be very different from mine.
;;
;;
;;  Non-interactive functions defined here:
;;
;;    `put-text-property-unless-ignore'.
;;
;;
;;  ***** NOTE: The following functions defined in `font-lock.el'
;;              have been REDEFINED HERE:
;;
;;    `font-lock-append-text-property', `font-lock-apply-highlight',
;;    `font-lock-apply-syntactic-highlight',
;;    `font-lock-default-unfontify-region',
;;    `font-lock-fillin-text-property',
;;    `font-lock-fontify-anchored-keywords',
;;    `font-lock-fontify-keywords-region',
;;    `font-lock-fontify-syntactically-region',
;;    `font-lock-prepend-text-property'.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2007/03/25 dadams
;;     Created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun put-text-property-unless-ignore (start end property value &optional object)
  "`put-text-property', but ignore text with property `font-lock-ignore'."
  (let ((here (min start end))
        (end1 (max start end)))
    (while (< here end1)
      (unless (get-text-property here 'font-lock-ignore object)
        (put-text-property here (1+ here) property value object))
      (setq here (1+ here)))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Don't unfontify any text that has property `font-lock-ignore'.
;;
(defun font-lock-default-unfontify-region (beg end)
  "Unfontify from BEG to END, unless text with property `font-lock-ignore'."
  (let ((here (min beg end))
        (end1 (max beg end)))
    (while (< here end1)
      (unless (get-text-property here 'font-lock-ignore)
        (remove-list-of-text-properties
         here (1+ here) (append font-lock-extra-managed-props
                                (if font-lock-syntactic-keywords
                                    '(syntax-table face font-lock-multiline)
                                  '(face font-lock-multiline)))))
      (setq here (1+ here)))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Use `put-text-property-unless-ignore' instead of `put-text-property'.
;;
(defun font-lock-prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      (put-text-property-unless-ignore start next prop
                                       (append val (if (listp prev) prev (list prev)))
                                       object)
      (setq start next))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Use `put-text-property-unless-ignore' instead of `put-text-property'.
;;
(defun font-lock-append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      (put-text-property-unless-ignore start next prop
                                       (append (if (listp prev) prev (list prev)) val)
                                       object)
      (setq start next))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Use `put-text-property-unless-ignore' instead of `put-text-property'.
;;
(defun font-lock-fillin-text-property (start end prop value &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end prop nil object)) next)
    (while start
      (setq next (next-single-property-change start prop object end))
      (put-text-property-unless-ignore start next prop value object)
      (setq start (text-property-any next end prop nil object)))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Use `put-text-property-unless-ignore' instead of `put-text-property'.
;;
(defun font-lock-apply-syntactic-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT,
see `font-lock-syntactic-keywords'."
  (let* ((match (nth 0 highlight))
         (start (match-beginning match)) (end (match-end match))
         (value (nth 1 highlight))
         (override (nth 2 highlight)))
    (if (not start)
        ;; No match but we might not signal an error.
        (or (nth 3 highlight)
            (error "No match %d in highlight %S" match highlight))
      (when (and (consp value) (not (numberp (car value))))
        (setq value (eval value)))
      (when (stringp value) (setq value (string-to-syntax value)))
      ;; Flush the syntax-cache.  I believe this is not necessary for
      ;; font-lock's use of syntax-ppss, but I'm not 100% sure and it can
      ;; still be necessary for other users of syntax-ppss anyway.
      (syntax-ppss-after-change-function start)
      (cond
        ((not override)
         ;; Cannot override existing fontification.
         (or (text-property-not-all start end 'syntax-table nil)
             (put-text-property-unless-ignore start end 'syntax-table value)))
        ((eq override t)
         ;; Override existing fontification.
         (put-text-property-unless-ignore start end 'syntax-table value))
        ((eq override 'keep)
         ;; Keep existing fontification.
         (font-lock-fillin-text-property start end 'syntax-table value))))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Use `put-text-property-unless-ignore' instead of `put-text-property'.
;;
(defun font-lock-fontify-syntactically-region (start end &optional loudly ppss)
  "Put proper face on each string and comment between START and END.
START should be at the beginning of a line."
  (let ((comment-end-regexp
         (or font-lock-comment-end-skip
             (regexp-quote
              (replace-regexp-in-string "^ *" "" comment-end))))
        state face beg)
    (if loudly (message "Fontifying %s... (syntactically...)" (buffer-name)))
    (goto-char start)
    ;;
    ;; Find the `start' state.
    (setq state (or ppss (syntax-ppss start)))
    ;;
    ;; Find each interesting place between here and `end'.
    (while
        (progn
          (when (or (nth 3 state) (nth 4 state))
            (setq face (funcall font-lock-syntactic-face-function state))
            (setq beg (max (nth 8 state) start))
            (setq state (parse-partial-sexp (point) end nil nil state
                                            'syntax-table))
            (when face (put-text-property-unless-ignore beg (point) 'face face))
            (when (and (eq face 'font-lock-comment-face)
                       (or font-lock-comment-start-skip
                           comment-start-skip))
              ;; Find the comment delimiters
              ;; and use font-lock-comment-delimiter-face for them.
              (save-excursion
                (goto-char beg)
                (if (looking-at (or font-lock-comment-start-skip
                                    comment-start-skip))
                    (put-text-property-unless-ignore beg (match-end 0) 'face
                                                     font-lock-comment-delimiter-face)))
              (if (looking-back comment-end-regexp (point-at-bol) t)
                  (put-text-property-unless-ignore (match-beginning 0) (point) 'face
                                                   font-lock-comment-delimiter-face))))
          (< (point) end))
      (setq state (parse-partial-sexp (point) end nil nil state
                                      'syntax-table)))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; 1. Use `put-text-property-unless-ignore' instead of `put-text-property'.
;; 2. Use `defun' instead of `defsubst', since we don't want to reproduce whole library.
;;
(defun font-lock-apply-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT, see `font-lock-keywords'."
  (let* ((match (nth 0 highlight))
         (start (match-beginning match)) (end (match-end match))
         (override (nth 2 highlight)))
    (if (not start)
        ;; No match but we might not signal an error.
        (or (nth 3 highlight)
            (error "No match %d in highlight %S" match highlight))
      (let ((val (eval (nth 1 highlight))))
        (when (eq (car-safe val) 'face)
          (add-text-properties start end (cddr val))
          (setq val (cadr val)))
        (cond
          ((not (or val (eq override t)))
           ;; If `val' is nil, don't do anything.  It is important to do it
           ;; explicitly, because when adding nil via things like
           ;; font-lock-append-text-property, the property is actually
           ;; changed from <face> to (<face>) which is undesirable.  --Stef
           nil)
          ((not override)
           ;; Cannot override existing fontification.
           (or (text-property-not-all start end 'face nil)
               (put-text-property-unless-ignore start end 'face val)))
          ((eq override t)
           ;; Override existing fontification.
           (put-text-property-unless-ignore start end 'face val))
          ((eq override 'prepend)
           ;; Prepend to existing fontification.
           (font-lock-prepend-text-property start end 'face val))
          ((eq override 'append)
           ;; Append to existing fontification.
           (font-lock-append-text-property start end 'face val))
          ((eq override 'keep)
           ;; Keep existing fontification.
           (font-lock-fillin-text-property start end 'face val)))))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; 1. Use `put-text-property-unless-ignore' instead of `put-text-property'.
;; 2. Use `defun' instead of `defsubst', since we don't want to reproduce whole library.
;;
(defun font-lock-fontify-anchored-keywords (keywords limit)
  "Fontify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `font-lock-keywords',
LIMIT can be modified by the value of its PRE-MATCH-FORM."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights
        (lead-start (match-beginning 0))
        ;; Evaluate PRE-MATCH-FORM.
        (pre-match-value (eval (nth 1 keywords))))
    ;; Set LIMIT to value of PRE-MATCH-FORM or the end of line.
    (if (not (and (numberp pre-match-value) (> pre-match-value (point))))
        (setq limit (line-end-position))
      (setq limit pre-match-value)
      (when (and font-lock-multiline (>= limit (line-beginning-position 2)))
        ;; this is a multiline anchored match
        ;; (setq font-lock-multiline t)
        (put-text-property-unless-ignore (if (= limit (line-beginning-position 2))
                                             (1- limit)
                                           (min lead-start (point)))
                                         limit
                                         'font-lock-multiline t)))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (and (< (point) limit)
                  (if (stringp matcher)
                      (re-search-forward matcher limit t)
                    (funcall matcher limit)))
        ;; Apply each highlight to this instance of `matcher'.
        (setq highlights lowdarks)
        (while highlights
          (font-lock-apply-highlight (car highlights))
          (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))


;; REPLACES ORIGINAL in `font-lock.el'.
;;
;; Use `put-text-property-unless-ignore' instead of `put-text-property'.
;;
(defun font-lock-fontify-keywords-region (start end &optional loudly)
  "Fontify according to `font-lock-keywords' between START and END.
START should be at the beginning of a line.
LOUDLY, if non-nil, allows progress-meter bar."
  (unless (eq (car font-lock-keywords) t)
    (setq font-lock-keywords
          (font-lock-compile-keywords font-lock-keywords)))
  (let ((case-fold-search font-lock-keywords-case-fold-search)
        (keywords (cddr font-lock-keywords))
        (bufname (buffer-name)) (count 0)
        (pos (make-marker))
        keyword matcher highlights)
    ;;
    ;; Fontify each item in `font-lock-keywords' from `start' to `end'.
    (while keywords
      (if loudly (message "Fontifying %s... (regexps..%s)" bufname
                          (make-string (incf count) ?.)))
      ;;
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (and (< (point) end)
                  (if (stringp matcher)
                      (re-search-forward matcher end t)
                    (funcall matcher end))
                  ;; Beware empty string matches since they will
                  ;; loop indefinitely.
                  (or (> (point) (match-beginning 0))
                      (progn (forward-char 1) t)))
        (when (and font-lock-multiline
                   (>= (point)
                       (save-excursion (goto-char (match-beginning 0))
                                       (forward-line 1) (point))))
          ;; this is a multiline regexp match
          ;; (setq font-lock-multiline t)
          (put-text-property-unless-ignore (if (= (point)
                                                  (save-excursion
                                                    (goto-char (match-beginning 0))
                                                    (forward-line 1) (point)))
                                               (1- (point))
                                             (match-beginning 0))
                                           (point)
                                           'font-lock-multiline t))
        ;; Apply each highlight to this instance of `matcher', which may be
        ;; specific highlights or more keywords anchored to `matcher'.
        (setq highlights (cdr keyword))
        (while highlights
          (if (numberp (car (car highlights)))
              (font-lock-apply-highlight (car highlights))
            (set-marker pos (point))
            (font-lock-fontify-anchored-keywords (car highlights) end)
            ;; Ensure forward progress.  `pos' is a marker because anchored
            ;; keyword may add/delete text (this happens e.g. in grep.el).
            (if (< (point) pos) (goto-char pos)))
          (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))
    (set-marker pos nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'font-lock+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock+.el ends here