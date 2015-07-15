(add-to-list 'load-path "~/Software/slime")

(cond
 ((string-equal system-name "zaurus.chicago")
	;; ZAURUS
	(setq inferior-lisp-program "/usr/local/bin/clisp"))

 (running-on-linux
	;; bean

	;(setq inferior-lisp-program "/usr/bin/clisp")
	(setq inferior-lisp-program "/usr/bin/sbcl")

	;; TODO: Use w3m browser for hyperspec

	(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec-7.0/HyperSpec/"))

 (running-on-windows
	;; chobi
	(setq inferior-lisp-program "/usr/bin/clisp")))

(require 'mic-paren)

(require 'slime)
(slime-setup)
(setq slime-startup-animation nil)

(global-set-key "\C-cs" 'slime-selector)

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'paren-activate)

;(define-key slime-mode-map (kbd "[") 'insert-parentheses)
;(define-key slime-mode-map (kbd "]") 'move-past-close-and-reindent)
(define-key slime-mode-map (kbd "[") (lambda () (interactive) (insert "(")))
(define-key slime-mode-map (kbd "]") (lambda () (interactive) (insert ")")))
(define-key slime-mode-map (kbd "(") (lambda () (interactive) (insert "[")))
(define-key slime-mode-map (kbd ")") (lambda () (interactive) (insert "]")))

;; (define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
;; (define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
;; (define-key slime-mode-map (kbd "C-b") 'backward-sexp)
;; (define-key slime-mode-map (kbd "C-M-b") 'backward-char)
;; (define-key slime-mode-map (kbd "C-f") 'forward-sexp)
;; (define-key slime-mode-map (kbd "C-M-f") 'forward-char)
