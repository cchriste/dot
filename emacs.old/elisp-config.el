(require 'mic-paren)

(add-hook 'emacs-lisp-mode-hook 'paren-activate)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (mapcar
;;              (lambda (file)
;;                (setq file (expand-file-name file))
;;                (when (string= file (buffer-file-name))
;;                  (save-excursion (byte-compile-file file))))
;;              '("~/.emacs" "~/*.el"))))


(defun byte-compile-dest-file (file)
	(let ((just-file (file-name-nondirectory file))
				(basename (file-name-sans-extension file)))
		(when (string= (file-name-sans-extension just-file) "")
			(setq basename file))
		(concat basename ".elc")))

;;; When we close emacs, byte-compile all the elisp files in
;;; our site-lisp directory.
(add-hook 'kill-emacs-hook
					(lambda ()
						(mapcar
						 (lambda (file)
							 (setq file (expand-file-name file))
							 (when (file-newer-than-file-p file
																						 (byte-compile-dest-file file))
								 (save-excursion (byte-compile-file file))))
						 (append (directory-files "~/.emacs.d/site-lisp/" t ".*\.el$" t) '("~/.emacs.d/site-lisp/.emacs")))))

