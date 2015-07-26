;; Welcome to Hell
;; .emacs
;; Cameron Christensen
;; Modified for many seasons, multiple copies abound. 
;; Abandon all hope ye who enter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *author* "Cameron Christensen")
(defvar *copyright* "SCI Institute, all rights reserved.")
	
;; If this emacs is being run as the EDITOR, don't make backup files
;; so the opened file will be edited in place (required for crontab
;; and possibly others).
(if (getenv "__emacs_as_EDITOR__")
  (setq make-backup-files nil))

;; Add my emacs libs to load path
(setq load-path (cons (substitute-in-file-name "$HOME/emacs") load-path))
(setq load-path (cons (substitute-in-file-name "$HOME/emacs/cc-mode") load-path))
(setq load-path (cons (substitute-in-file-name "$HOME/code/contrib/emacswikicode") load-path))

;;; Win32-specific customizations
(if (eq window-system 'w32)
  ;;; Set shell proxy to cygwin bash
  (progn
    (setq shell-file-name "bash.exe") ;; This is needed for 'shell-command
    (setenv "SHELL" "bash.exe")))       ;; This is needed for 'shell

(require 'cc-mode)
(load-library "find-other-file")
(load-library "c++-style")
(load-library "lisp-style")
(load-library "perl-style")
(autoload 'make-header "header"  "Mode dependent header generator" t)
(autoload 'make-function-header "header"  "Function header generator" t)
(autoload 'make-new-header "header"  "Insert header and include guard" t)
(autoload 'make-new-class "header"  "Insert a C++ class declaration" t)
(autoload 'find-grep "find-grep" "Pipe find into grep using xargs." t)

(load-library "facemenu+")
(and (eq window-system 'mac)
     (progn
      (setq palette-font "-apple-monaco-medium-r-normal--5-120-72-72-m-120-iso10646-1")
      ;(set-default-font "-apple-lucida grande ce-bold-r-normal--0-0-0-0-m-0-mac-centraleurroman")))
      (set-default-font "-apple-monaco-medium-r-normal--11-110-72-72-m-110-iso10646-1")))

;; Transparent Remote Access Multiple Protocol
;; TODO: This is great, but for some reason conflict with svn mode. Temporarily disabled. <ctc>
;; (add-to-list 'load-path "~/emacs/tramp/lisp/")
;; (add-to-list 'Info-default-directory-list "~/emacs/tramp/info/")
;; (require 'tramp)
;; (setq tramp-default-method "scp")

;; make .h files load c++-mode by default.
(add-to-list 'auto-mode-alist '("[.]h$" . c++-mode))

;;;cmake mode
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; csharp mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Twitter!
(load-library "twit")
(setq twit-show-user-images t)

;;; Future home of org-mode! TODO

;;;
;; [from PetTomato blog]

;; actionscript
(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
;; Activate actionscript-mode for any files ending in .as
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.pde$" . java-mode))
;; Load our actionscript-mode extensions. [these don't all work]
;;(eval-after-load "actionscript-mode" '(load "as-config"))

;; Show current function in modeline
(setq which-func-modes '(actionscript-mode python-mode emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode makefile-mode sh-mode fortran-mode java-mode))
(which-function-mode t)

;; [/PetTomato]
;;;

;;;
;; Cameron's personal touch.

;; Display ansi codes in shell.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Scroll less aggressively.
(setq scroll-up-aggressively 0.2)
(setq scroll-down-aggressively 0.2)

;;; color-theme
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-hober)))

;; Custom global colors
(set-background-color "#001322")
(set-foreground-color "AntiqueWhite")
(set-cursor-color "gold")

;; make dabbrev-expand work better with html.
;; NOTE: This doesn't work, but it gets us closer I suppose.
(setq dabbrev-abbrev-skip-leading-regexp "[^ ]*[<>=*]")

;;; Remove ^Ms from the entire buffer.
(defun strip-control-ms ()
  "Delete all ^Ms."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (and (not (eobp))
		(re-search-forward "\C-m" nil t))
      (delete-region (match-beginning 0) (match-end 0)))))

;;; Insert current date (from http://www.emacswiki.org/emacs/InsertDate)
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y.%m.%d.%H%M")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%A, %B %d, %Y"))))
      (insert (format-time-string format))))
(defun insert-standard-date ()
    "Inserts standard date time string." 
    (interactive)
    (insert (format-time-string "%c")))

;;; Revert the current buffer without asking first.
(defun fast-revert-buffer ()
"Revert the buffer from the original file without asking."
  (interactive)
  (revert-buffer t t))

;;; Assign shortcuts
(global-set-key "\C-cr" 'fast-revert-buffer)
(global-set-key "\C-ct" 'toggle-truncate-lines)
(global-set-key "\C-x\C-_" 'call-last-kbd-macro)
(global-set-key "\C-cd" 'ediff-buffers)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key (kbd "C-c l") 'insert-date)
(global-set-key "\C-ca" 'find-other-source-file)
(global-set-key "\C-c\M-." 'tags-search)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key (kbd "C-S-p") '(lambda() (interactive) (scroll-down 1)))
(global-set-key (kbd "C-S-n") '(lambda() (interactive) (scroll-up 1)))
(global-set-key "\C-c\C-f" 'make-function-header)
(global-set-key "\C-c\C-m" 'compile)

;; <TODO>
;; (elisp-set-key "\C-c\C-c" 'comment-region)

;;; Display line numbers even for large files.
(setq line-number-display-limit 1000000)


;;;; (Cameron learns emacs...)

;; (if (string= (system-name) "PBJ")
;;   (message "this system is called PBR!")
;;   (message "this system has the name: %s"
;; 	    (system-name))
;;   (message "oh, bother"))


;; <TODO> Write a wrapper to call this with .* buffers and whatever
;;        word is currently under the point.
(global-set-key "\C-x\M-/" 'multi-occur-in-matching-buffers)

;;; set style based on which tree is being editing.
(defun maybe-Iris-offset ()
  (if (string-match "code\\(/\\|\\\\\\)iris" buffer-file-name)
    (progn
      (setq c-basic-offset 2)
      (setq c-require-final-newline 'nil)
      (setq fill-column 105))))
(add-hook 'c-mode-hook 'maybe-Iris-offset)
(add-hook 'c++-mode-hook 'maybe-Iris-offset)

;; [/Cameron's customizations]
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This makes "M-x compile" smarter by trying to guess what the compilation
;;;; command should be for the C, C++, and Fortran language modes.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; By requiring `compile' at this point, we help to ensure that the global
;; value of compile-command is set properly.  If `compile' is autoloaded when
;; the current buffer has a buffer-local copy of compile-command, then the
;; global value doesn't get set properly.

(require 'compile)

;; This gives the form of the default compilation command for C++, C, and
;; Fortran programs.  Specifying the "-lm" option for C and C++  eliminates a
;; lot of potential confusion.

(defvar compile-guess-command-table
  '((c-mode       . "gcc -Wall -g %s -o %s -lm"); Doesn't work for ".h" files.
    (c++-mode     . "g++ -g %s -o %s -lm")	; Doesn't work for ".h" files.
    (fortran-mode . "f77 -C %s -o %s")
    (actionscript-mode . "mxmlc -debug=true -strict=true -use-network=false -optimize=true %s")
    )
  "*Association list of major modes to compilation command descriptions, used
by the function `compile-guess-command'.  For each major mode, the compilation
command may be described by either:

  + A string, which is used as a format string.  The format string must accept
    two arguments: the simple (non-directory) name of the file to be compiled,
    and the name of the program to be produced.

  + A function.  In this case, the function is called with the two arguments
    described above and must return the compilation command.")

;; This code guesses the right compilation command when Emacs is asked
;; to compile the contents of a buffer.  It bases this guess upon the
;; filename extension of the file in the buffer.

(defun compile-guess-command ()

  (let ((command-for-mode (cdr (assq major-mode
				     compile-guess-command-table))))
    (if (and command-for-mode
	     (stringp buffer-file-name))
	(let* ((file-name (file-name-nondirectory buffer-file-name))
	       (file-name-sans-suffix (if (and (string-match "\\.[^.]*\\'"
							     file-name)
					       (> (match-beginning 0) 0))
					  (substring file-name
						     0 (match-beginning 0))
					nil)))
	  (if file-name-sans-suffix
	      (progn
		(make-local-variable 'compile-command)
		(setq compile-command
		      (if (stringp command-for-mode)
			  ;; Optimize the common case.
			  (format command-for-mode
				  file-name file-name-sans-suffix)
			(funcall command-for-mode
				 file-name file-name-sans-suffix)))
		compile-command)
	    nil))
      nil)))

;; Add the appropriate mode hooks.
(add-hook 'c-mode-hook       (function compile-guess-command))
(add-hook 'c++-mode-hook     (function compile-guess-command))
(add-hook 'fortran-mode-hook (function compile-guess-command))
(add-hook 'actionscript-mode-hook (function compile-guess-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
; '(dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
 '(display-time-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mail-host-address "\"sci.utah.edu\"")
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; I think the desktop restoration calls need to be at the end of this file.
;; Save emacs desktop and reload it on startup.
(desktop-load-default)
;; if desired... save some variables:
;;	(setq desktop-locals-to-save (cons 'foobar desktop-locals-to-save))
;;	(setq desktop-globals-to-save (cons 'foobar desktop-globals-to-save))
(desktop-read)

(put 'scroll-left 'disabled nil)

;;;;
;;; Default frame size as a percentage <TODO>.

;; Set the size and location of the first frame.
(setq initial-frame-alist '((width . 121)
                            (top . 5)
                            (left . 785)
                            (height . 103)))

;; Set defaults for subsequent frames.
(setq default-frame-alist (append '((width . 121)
                                    (top . 5)
                                    (left . 50)
                                    (height . 72))
                                  default-frame-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
