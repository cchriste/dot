;; grab-mail - get mail from other buffer, insert into current buffer,
;; indented and prefixed with "> "

(defun grab ()
  (interactive)
  (other-window 1)
  (beginning-of-buffer)
  (forward-paragraph 1)
  (end-of-buffer)
  (copy-region-as-kill (mark) (point))
  (other-window 1)
  (yank)
  (exchange-point-and-mark)
  (replace-regexp "^" "> ")
  (exchange-point-and-mark)
  (end-of-buffer)
  (next-line 1))
